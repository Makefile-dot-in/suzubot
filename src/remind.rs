use std::fmt;

use chrono::{DateTime, Utc};
use crate::comp_util::{ListElementModel, self};
use crate::linkable::Linkable;
use crate::utils::ParsedDatetime;
use crate::{PoiseContext, fmttime_discord, truncate};
use crate::errors::Error;
use crate::{pg, pgtyp, utils::vec_to_u64, errors::Contextualizable};
use pgtyp::ToSql;
use crate::{ser, errors::Result};
use ser::Mentionable;

#[derive(Debug)]
pub enum RemindContext {
    DeserializingReminder,
    GettingTotalReminders,
    GettingActiveReminders,
    RemindingOf {
        id: i32,
    },
    ParsingTime,
    AddingReminder,
    DeterminingLatency,
}

impl fmt::Display for RemindContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RemindContext::*;
        match self {
            DeserializingReminder => write!(f, "deserializing reminder object"),
            GettingTotalReminders => write!(f, "getting total reminders"),
            GettingActiveReminders => write!(f, "getting active reminders"),
            RemindingOf { id } => write!(f, "reminding (id = {id})"),
            ParsingTime => write!(f, "parsing time"),
            AddingReminder => write!(f, "adding reminder"),
            DeterminingLatency => write!(f, "determining gateway latency")
        }
    }
}


#[derive(Debug)]
pub enum RemindError {
    ReminderNotFound {
        id: i32,
    }
}

impl fmt::Display for RemindError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RemindError::*;
        match self {
            ReminderNotFound { id } => write!(f, "{id}: reminder not found")
        }
    }
}



struct Reminder {
    id: i32,
    guild_id: Option<ser::GuildId>,
    channel_id: ser::ChannelId,
    creator_id: ser::UserId,
    creation_time: DateTime<Utc>,
    target_time: DateTime<Utc>,
    description: String,
}

#[derive(Debug, Clone, Copy)]
enum ReminderFilter {
    GuildId([u8; 8]),
    ChannelId([u8; 8]),
    CreatorId([u8; 8]),
    CreationTimeBefore(DateTime<Utc>),
    CreationTimeAfter(DateTime<Utc>),
    TargetTimeBefore(DateTime<Utc>),
    TargetTimeAfter(DateTime<Utc>),
}

impl From<ser::GuildId> for ReminderFilter {
    fn from(gid: ser::GuildId) -> Self {
        Self::GuildId(gid.as_u64().to_be_bytes())
    }
}


impl From<ser::ChannelId> for ReminderFilter {
    fn from(cid: ser::ChannelId) -> Self {
        Self::ChannelId(cid.as_u64().to_be_bytes())
    }
}

impl From<ser::UserId> for ReminderFilter {
    fn from(uid: ser::UserId) -> Self {
        Self::CreatorId(uid.as_u64().to_be_bytes())
    }
}

impl ReminderFilter {
    fn to_sql_expr(self, idx: u16) -> String {
        use ReminderFilter::*;
        match self {
            GuildId(_) => format!("server_id = ${idx}"),
            ChannelId(_) => format!("channel_id = ${idx}"),
            CreatorId(_) => format!("creator_id = ${idx}"),
            CreationTimeBefore(_) => format!("creation_time < ${idx}"),
            CreationTimeAfter(_) => format!("creation_time > ${idx}"),
            TargetTimeBefore(_) => format!("target_time < ${idx}"),
            TargetTimeAfter(_) => format!("target_time > ${idx}"),
        }
    }

    fn to_arg(&self) -> &(dyn ToSql + Sync) {
        use ReminderFilter::*;
        match self {
            GuildId(val) => val,
            ChannelId(val) => val,
            CreatorId(val) => val,
            CreationTimeBefore(val) => val,
            CreationTimeAfter(val) => val,
            TargetTimeBefore(val) => val,
            TargetTimeAfter(val) => val,
        }
    }
}

impl Reminder {
    const ADD_REMINDER: &'static str = "
        INSERT INTO reminders (server_id, channel_id, creator_id, creation_time, target_time, description)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id;";

    fn list_reminders_sql(whereclauses: &str, idx: u16) -> String {
        format!("SELECT id, server_id, channel_id, creator_id, creation_time, target_time, description, count(*) OVER() as total
                FROM reminders
                WHERE TRUE {whereclauses}
                ORDER BY id DESC
                LIMIT 10
                OFFSET ${idx}")
    }

    const LIST_REMINDERS_NEXT_IDX: u16 = 1;

    const ACTIVE_REMINDERS: &'static str = "
        DELETE FROM reminders
        WHERE target_time <= NOW()
        RETURNING id, server_id, channel_id, creator_id, creation_time, target_time, description;";

    const SINGLE_REMINDER_BY_ID: &'static str = "
        SELECT id, server_id, channel_id, creator_id, creation_time, target_time, description FROM reminders
        WHERE id = $1;";


    /// inserts self into the database using the databse pool
    /// and returns the ID of the newly created reminder.
    pub async fn add_reminder(&self, data: &crate::Data) -> Result<i32> {
        let mut conn = data.dbconn.get().await?;
        let trans = conn.transaction().await?;
        let row = trans.query_one(Self::ADD_REMINDER, &[
            &self.guild_id.map(|g| g.0.to_be_bytes()),
            &self.channel_id.0.to_be_bytes(),
            &self.creator_id.0.to_be_bytes(),
            &self.creation_time,
            &self.target_time,
            &self.description,
        ]).await?;
        trans.commit().await?;
        Ok(row.try_get(0)?)
    }

    fn from_row(r: pg::Row) -> Result<Self> {
        Ok(Self {
            id: r.try_get("id")?,
            guild_id: r
                .try_get::<_, Option<_>>("server_id")?
                .map(vec_to_u64)
                .transpose()?
                .map(ser::GuildId),
            channel_id: ser::ChannelId(vec_to_u64(r.try_get("channel_id")?)?),
            creator_id: ser::UserId(vec_to_u64(r.try_get("creator_id")?)?),
            creation_time: r.try_get("creation_time")?,
            target_time: r.try_get("target_time")?,
            description: r.try_get("description")?
        })
    }

    pub async fn list_reminders(
        data: &crate::Data,
        filters: &[ReminderFilter],
        offset: i64
    ) -> Result<(Vec<Self>, i64)> {
        let conn = data.dbconn.get().await?;
        let mut params: Vec<&(dyn ToSql + Sync)> = Vec::new();
        let mut whereclauses = String::new();
        let mut paramidx = Self::LIST_REMINDERS_NEXT_IDX;

        for filter in filters {
            whereclauses.push_str(&format!(" AND {expr}", expr = filter.to_sql_expr(paramidx)));
            params.push(filter.to_arg());
            paramidx += 1;
        }

        params.push(&offset);
        let query = Self::list_reminders_sql(&whereclauses, paramidx);
        log::debug!("reminders running query: ({query:?}, {params:?})");
        let rows = conn.query(&query, &*params).await?;

        let total = rows
            .first()
            .map(|row| row.try_get::<_, i64>("total"))
            .transpose()
            .map_err(Error::from)
            .contextualize(RemindContext::GettingTotalReminders)?
            .unwrap_or(0);


        let reminders = rows
            .into_iter()
            .map(Self::from_row)
            .collect::<Result<Vec<_>>>()
            .contextualize(RemindContext::DeserializingReminder)?;


        Ok((reminders, total))
    }

    pub async fn pagination_gen(
        data: &crate::Data,
        filters: &[ReminderFilter],
        pagenum: u32
    ) -> Result<(Vec<Reminder>, u32, u32)> {
        let (reminders, total) = Self::list_reminders(data, filters, i64::from(pagenum) * 10).await?;
        let total_pages: u32 = total.div_ceil(10).try_into().unwrap_or(u32::MAX);
        let pagenum = pagenum.clamp(0, total_pages);
        Ok((reminders, total_pages, pagenum))
    }


    pub async fn active_reminders_prepare(conn: &pg::Client) -> Result<pg::Statement> {
        Ok(conn.prepare(Self::ACTIVE_REMINDERS).await?)
    }

    pub async fn active_reminders(
        trans: &pg::Transaction<'_>,
        stmt: &pg::Statement
    ) -> Result<Vec<Self>> {
        let rows = trans.query(stmt, &[]).await?;

        let reminders = rows
            .into_iter()
            .map(Self::from_row)
            .collect::<Result<Vec<_>>>()
            .contextualize(RemindContext::DeserializingReminder)?;
        Ok(reminders)
    }

    pub async fn remind(&self, ctx: impl AsRef<ser::Http>) -> Result<()> {
        self.channel_id.send_message(ctx, |m| {
            m.content(format!("{user_mention} {ts_fmt}: {description}",
                              user_mention = self.creator_id.mention(),
                              ts_fmt = self.creation_time.format("<t:%s:R>"),
                              description = self.description))
                .allowed_mentions(|a| {
                    a.empty_parse().users([self.creator_id])
                })
        }).await?;
        Ok(())
    }

    pub async fn single_reminder_by_id(
        data: &crate::Data,
        id: i32
    ) -> Result<Self> {
        let conn = data.dbconn.get().await?;
        let row = conn.query_opt(Self::SINGLE_REMINDER_BY_ID, &[&id]).await?
            .ok_or(RemindError::ReminderNotFound { id })?;
        Ok(Reminder::from_row(row)?)
    }
}

impl ListElementModel for Reminder {
    fn pagination_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{id}] {user} in {server} in {channel} at {target_ts}, set at {creation_ts}: {description}",
               id = self.id,
               user = self.creator_id.mention(),
               server = self.guild_id.map(|g| g.link(()).to_string()).unwrap_or_else(|| "DMs".to_owned()),
               channel = self.channel_id.mention(),
               target_ts = fmttime_discord(self.target_time.into()),
               creation_ts = fmttime_discord(self.creation_time.into()),
               description = truncate(self.description.to_owned(), 30))
    }
}

pub mod service {
    use crate::errors::{withctx_error_logged, Result, Contextualizable, Error, LogError};
    use crate::utils::get_latency;
    use crate::remind::RemindContext;
    use crate::pg;
    use super::Reminder;

    use std::sync::Arc;
    use std::time::Duration as StdDuration;
    use poise::serenity_prelude as ser;
    use tokio::sync::Mutex;

    async fn run_reminders(
        ctx: &ser::Context,
        shardmgr: &Arc<Mutex<ser::ShardManager>>,
        conn: &mut pg::Client,
        stmt: &pg::Statement
    ) -> Result<()> {
        let trans = conn.transaction().await?;
        let queue = Reminder::active_reminders(&trans, &stmt)
            .await
            .contextualize(RemindContext::GettingActiveReminders)?;

        let latency_opt = get_latency(shardmgr, ser::ShardId(ctx.shard_id))
            .await
            .map_err(Error::from)
            .contextualize(RemindContext::DeterminingLatency)?;

        match latency_opt {
            Some(latency) if latency < StdDuration::from_secs(120) => {},
            Some(latency) => {
                log::warn!("Gateway latency is {latency_s} s, skipping an iteration",
                           latency_s = latency.as_secs());
                return Ok(())
            },
            None => {
                log::warn!("Gateway latency cannot be determined, skipping an iteration");
                return Ok(())
            },
        }

        for reminder in queue {
            reminder
                .remind(&ctx)
                .await
                .contextualize(RemindContext::RemindingOf { id: reminder.id })
                .logerr();
        }
        trans.commit().await?;
        Ok(())
    }

    // calls run_reminder repeatedly with a 60 minute interval.
    async fn run_reminders_repeatedly(
        ctx: &ser::Context,
        shardmgr: &Arc<Mutex<ser::ShardManager>>,
        conn: &mut pg::Client,
        stmt: &pg::Statement
    ) {
        loop {
            tokio::time::sleep(StdDuration::from_secs(60)).await;
            if let Err(e) = run_reminders(ctx, shardmgr, conn, stmt).await {
                if conn.is_closed() {
                    // if the connection is cloned we probably can't process any event
                    log::error!("Error: {e}. Detected database closure, returning...");
                    return;
                }

                log::error!("Error: {e}");
            }
        }
    }
    pub async fn service(
        ctx: ser::Context,
        _user: Arc<ser::CurrentUser>,
        shardmgr: Arc<Mutex<ser::ShardManager>>,
        data: crate::Data
    ) -> ! {
        loop {
            log::info!("Starting remind service...");
            let res: Result<_> = try {
                let conn = data.dbconn.dedicated_connection().await?;
                let stmt = Reminder::active_reminders_prepare(&conn).await?;
                (conn, stmt)
            };

            log::info!("Connection to database successful");

            let (mut conn, stmt) = match res {
                Ok(x) => x,
                Err(e) => {
                    log::error!("Failed to initialize reminder service: {err}",
                                err = withctx_error_logged(&e));
                    log::info!("Trying again in 1 minute...");
                    tokio::time::sleep(StdDuration::from_secs(60)).await;
                    continue;
                }
            };

            run_reminders_repeatedly(&ctx, &shardmgr, &mut conn, &stmt).await;
            log::info!("Attempting to restart remind service...");
            tokio::time::sleep(StdDuration::from_secs(60)).await;
        }
    }
}

#[poise::command(slash_command)]
/// Sets a reminder.
pub async fn remind(
    ctx: PoiseContext<'_>,
    #[max_length = 50]
    #[description = "Time to remind at"]
    when: ParsedDatetime,
    #[description = "Description to remind with"]
    #[max_length = 300]
    description: String
) -> Result<()> {
    let now = Utc::now();
    if now > *when {
        ctx.say(format!("Error: the time to remind at must be in the future (you specified <t:{ts}>, which is <t:{ts}:R>)",
                        ts = when.timestamp())).await?;
        return Ok(());
    }


    let reminder = Reminder {
        id: -1, // no ID yet (until assigned by DB)
        guild_id: ctx.guild_id(),
        channel_id: ctx.channel_id(),
        creator_id: ctx.author().id,
        creation_time: *ctx.created_at(),
        target_time: when.into(),
        description: description.clone()
    };

    let id = reminder
        .add_reminder(ctx.data())
        .await
        .contextualize(RemindContext::AddingReminder)?;

    ctx.say(format!("Reminder set for {ts} (ID: {id}): {description}.",
                    ts = fmttime_discord((*when).into()))).await?;

    Ok(())
}



pub mod admin {
    use super::{ReminderFilter, Reminder};

    use crate::{ser, PoiseContext, errors::Result, comp_util, utils::ParsedDatetime};

    #[poise::command(slash_command, subcommands("list"))]
    pub async fn reminders(_ctx: PoiseContext<'_>) -> Result<()> { Ok(()) }

    #[poise::command(slash_command)]
    /// Lists all reminders in the current server.
    async fn list(
        ctx: PoiseContext<'_>,
        #[description = "Only show reminders from this channel"]
        channel: Option<ser::ChannelId>,
        #[description = "Only show reminders from this user"]
        user: Option<ser::UserId>,
        #[description = "Only show reminders created before this time"]
        created_before: Option<ParsedDatetime>,
        #[description = "Only show reminders created after this time"]
        created_after: Option<ParsedDatetime>,
        #[description = "Only show reminders that will fire before this time"]
        target_before: Option<ParsedDatetime>,
        #[description = "Only show reminders that will fire after this time"]
        target_after: Option<ParsedDatetime>
    ) -> Result<()> {
        comp_util::paginate(
            ctx,
            ctx.author().id,
            "Reminders",
            |pagenum| async move {
                let filters_opt = [
                    Some(ctx.guild_id().expect("command should only be runnable in guilds").into()),
                    channel.map(Into::into),
                    user.map(Into::into),
                    created_before.as_deref().copied().map(ReminderFilter::CreationTimeBefore),
                    created_after.as_deref().copied().map(ReminderFilter::CreationTimeAfter),
                    target_before.as_deref().copied().map(ReminderFilter::TargetTimeBefore),
                    target_after.as_deref().copied().map(ReminderFilter::TargetTimeAfter),
                ];
                let filters = filters_opt
                    .into_iter()
                    .filter_map(|x| x)
                    .collect::<Vec<_>>();
                Reminder::pagination_gen(ctx.data(), &filters, pagenum).await
            }
        ).await
    }
}

#[poise::command(slash_command, subcommands("list", "info"))]
/// Reminder related commands.
pub async fn reminders(_ctx: PoiseContext<'_>) -> Result<()> { Ok(()) }

#[poise::command(slash_command)]
/// Lists all reminders you've set.
async fn list(
    ctx: PoiseContext<'_>,
    #[description = "Only show reminders from this channel"]
    channel: Option<ser::ChannelId>,
    #[description = "Only show reminders created before this time"]
    created_before: Option<ParsedDatetime>,
    #[description = "Only show reminders created after this time"]
    created_after: Option<ParsedDatetime>,
    #[description = "Only show reminders that will fire before this time"]
    target_before: Option<ParsedDatetime>,
    #[description = "Only show reminders that will fire after this time"]
    target_after: Option<ParsedDatetime>
) -> Result<()> {
    comp_util::paginate(
        ctx,
        ctx.author().id,
        "Reminders",
        |pagenum| async move {
            let filters_opt = [
                Some(ctx.author().id.into()),
                ctx.guild_id().map(Into::into),
                channel.map(Into::into),
                created_before.as_deref().copied().map(ReminderFilter::CreationTimeBefore),
                created_after.as_deref().copied().map(ReminderFilter::CreationTimeAfter),
                target_before.as_deref().copied().map(ReminderFilter::TargetTimeBefore),
                target_after.as_deref().copied().map(ReminderFilter::TargetTimeAfter)
            ];
            let filters = filters_opt
                .into_iter()
                .filter_map(|x| x)
                .collect::<Vec<_>>();
            Reminder::pagination_gen(ctx.data(), &filters, pagenum).await
        }
    ).await
}


#[poise::command(slash_command)]
/// View information about a specific reminder.
pub async fn info(
    ctx: PoiseContext<'_>,
    #[description = "ID of the reminder"]
    #[min = 0]
    id: i32
) -> Result<()> {
    let reminder = Reminder::single_reminder_by_id(ctx.data(), id).await?;
    if reminder.creator_id != ctx.author().id && !ctx.guild_id().zip(reminder.guild_id).is_some_and(|(g1, g2)| g1 == g2) {
        ctx.say("You can only view reminders created by you or reminders created in this server.").await?;
        return Ok(())
    }
    ctx.send(|m| {
       m.embed(|e| {
           e.title(format!("⏰ Reminder #{id}", id = reminder.id))
            .color(ser::Color::RED);
           if let Some(guild_id) = reminder.guild_id {
               e.field("Server", guild_id.link(()), true);
           }
           e.field("User", reminder.creator_id.mention(), true)
            .field("Channel", reminder.channel_id.mention(), true)
            .field("Creation time", fmttime_discord(reminder.creation_time.into()), true)
            .field("Reminder time", fmttime_discord(reminder.target_time.into()), true)
            .field("Description", reminder.description, true)
       }).allowed_mentions(|a| a.empty_parse())
    }).await?;
    Ok(())
}

