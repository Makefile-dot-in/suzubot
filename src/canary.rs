use crate::pgtyp::{FromSql, ToSql};
use crate::errors::Result;
use crate::pg;
use crate::utils::vec_to_u64;
use poise::{serenity_prelude as ser, Event, FrameworkContext, ChoiceParameter};
use ser::Mentionable;
use crate::{PoiseContext, SuzuError};

#[derive(Clone, Copy, PartialEq, Eq, Debug, ToSql, FromSql)]
#[postgres(name = "canarytrigger")]
enum CanaryTrigger {
    Channel,
    Role,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, ToSql, FromSql, ChoiceParameter)]
#[postgres(name = "canaryaction")]
enum CanaryAction {
    Kick,
    Ban,
}

const GET_TRIGGER_ACTION: &'static str = "
   SELECT DISTINCT ON (trigger_id)
       trigger_id, trigger_action
   FROM canaries
   WHERE guild_id = $1
     AND trigger_id = ANY ($2)
     AND trigger_type = $3
   ORDER BY trigger_id, trigger_action DESC
";

async fn get_trigger_action(
    data: &crate::Data,
    guild: ser::GuildId,
    trigger_ids: &[ser::GenericId],
    trigger_type: CanaryTrigger
) -> Result<Option<(ser::GenericId, CanaryAction)>> {
    let conn = data.dbconn
        .get()
        .await?;

    let trigger_ids: Vec<_> = trigger_ids.iter().map(|id| id.0.to_be_bytes()).collect();
    let Some(row): Option<pg::Row> = conn
        .query_opt(GET_TRIGGER_ACTION, &[
            &guild.0.to_be_bytes(),
            &trigger_ids,
            &trigger_type,
        ])
        .await?
    else {
        return Ok(None);
    };
    let channelid = vec_to_u64(row.try_get(0)?)?;
    Ok(Some((ser::GenericId(channelid), row.try_get(1)?)))
}


const SET_TRIGGER_ACTION: &'static str = "
   INSERT INTO canaries
     (guild_id, trigger_id, trigger_type, trigger_action)
   VALUES ($1, $2, $3, $4)
   ON CONFLICT ON CONSTRAINT canaries_pkey
   DO UPDATE SET trigger_action = EXCLUDED.trigger_action
";

const DEL_TRIGGER_ACTION: &'static str = "
   DELETE FROM canaries
   WHERE guild_id = $1
     AND trigger_id = $2
     AND trigger_type = $3
";

async fn set_trigger_action(
    data: &crate::Data,
    guild: ser::GuildId,
    trigger_id: ser::GenericId,
    trigger_type: CanaryTrigger,
    trigger_action: Option<CanaryAction>
) -> Result<()> {
    let mut conn = data.dbconn
        .get()
        .await?;

    let trans = conn.transaction().await?;
    match trigger_action {
        Some(trigger_action) => {
            trans
                .execute(SET_TRIGGER_ACTION,
                         &[
                             &guild.0.to_be_bytes(),
                             &trigger_id.0.to_be_bytes(),
                             &trigger_type,
                             &trigger_action,
                         ])
                .await?;
        },
        None => {
            trans
                .execute(DEL_TRIGGER_ACTION,
                     &[
                         &guild.0.to_be_bytes(),
                         &trigger_id.0.to_be_bytes(),
                         &trigger_type
                     ])
                .await?;
        }
    }

    trans.commit().await?;
    Ok(())
}



async fn execute_action(
    ctx: &ser::Context,
    data: &crate::Data,
    user: &ser::Member,
    guild: ser::GuildId,
    action: CanaryAction,
    trigger: ser::GenericId,
    trigger_type: CanaryTrigger,
) -> Result<bool> {
    if user
        .permissions(ctx)?
        .intersects(ser::Permissions::MANAGE_GUILD
                    | ser::Permissions::MANAGE_MESSAGES) {
        return Ok(false);
    }

    let mention = match trigger_type {
        CanaryTrigger::Role => ser::RoleId(trigger.0).mention(),
        CanaryTrigger::Channel => ser::ChannelId(trigger.0).mention(),
    };

    let reason = format!("automated ban based on breaking canary: {mention}");
    
    match action {
        CanaryAction::Kick => {
            guild.kick_with_reason(ctx, user, &reason).await?;
        }
        CanaryAction::Ban => {
            let _ = crate::ban::dm_user(ctx, user.into(), guild, data, "automated ban").await;
            let reason = format!("automated ban based on breaking canary: {mention}");
            guild.ban_with_reason(ctx, user, 1, &reason).await?;
        }
    }
    Ok(true)
}

pub async fn evhandler<'a>(
    ctx: &'a ser::Context,
    evt: &'a Event<'a>,
    _fwctx: FrameworkContext<'a, crate::Data, SuzuError>,
    data: &crate::Data,
) -> Result<()> {
    use Event::*;
    match evt {
        Message { new_message } => {
            let Some(guild_id) = new_message.guild_id else {
                return Ok(())
            };

            let Some((id, action)) = get_trigger_action(
                data,
                guild_id,
                &[ser::GenericId(new_message.channel_id.0)],
                CanaryTrigger::Channel,
            ).await? else {
                return Ok(())
            };

            let member = guild_id
                .member(ctx, new_message.author.id)
                .await?;

            let action_taken = execute_action(
                ctx,
                data,
                &member,
                guild_id,
                action,
                id,
                CanaryTrigger::Channel,
            ).await?;

            if action_taken {
                new_message.delete(ctx).await?;
            }
            Ok(())
        }
        GuildMemberAddition { new_member: member, .. }
        | GuildMemberUpdate { new: member, .. } => {
            let ids: Vec<_> = member.roles.iter().map(|rid| ser::GenericId(rid.0)).collect();
            let Some((id, action)) = get_trigger_action(
                data,
                member.guild_id,
                &ids,
                CanaryTrigger::Role,
            ).await? else {
                return Ok(())
            };

            execute_action(
                ctx,
                data,
                member,
                member.guild_id,
                action,
                id,
                CanaryTrigger::Role,
            ).await?;
            Ok(())
        }
        _ => Ok(()),
    }
}

#[poise::command(slash_command, subcommands("role", "channel"))]
pub async fn canary(_ctx: PoiseContext<'_>) -> Result<()> {
    Ok(())
}

#[poise::command(
    slash_command,
    guild_only = true,
    default_member_permissions = "MANAGE_GUILD"
)]
async fn role(
    ctx: PoiseContext<'_>,
    #[description = "The role that should trigger the action"]
    role_id: ser::RoleId,
    #[description = "The action to be taken when a user takes up this role"]
    trigger_action: Option<CanaryAction>,
) -> Result<()> {
    set_trigger_action(
        ctx.data(),
        ctx.guild_id().unwrap(),
        ser::GenericId(role_id.0),
        CanaryTrigger::Role,
        trigger_action
    ).await?;
    ctx.send(|e| e.content("Settings updated")).await?;
    Ok(())
}

#[poise::command(
    slash_command,
    guild_only = true,
    default_member_permissions = "MANAGE_GUILD"
)]
async fn channel(
    ctx: PoiseContext<'_>,
    #[description = "The channel that should trigger this action"]
    channel_id: ser::ChannelId,
    #[description = "The action o be taken when a user posts in this channel"]
    trigger_action: Option<CanaryAction>,
) -> Result<()> {
    set_trigger_action(
        ctx.data(),
        ctx.guild_id().unwrap(),
        ser::GenericId(channel_id.0),
        CanaryTrigger::Channel,
        trigger_action
    ).await?;
    ctx.send(|e| e.content("Settings updated")).await?;
    Ok(())
}
