use std::borrow::Cow;

use crate::comp_util::ask_yn;
use crate::errors::Contextualizable as _;
use crate::errors::LogError;
use crate::errors::Result;
use crate::log::log_modmail;
use crate::log::LogErrorContext;
use crate::log::LogType;
use crate::ser;
use crate::utils::vec_to_u64;
use crate::PoiseContext;
use crate::SuzuError;
use poise::dispatch::FrameworkContext;
use poise::Event;
use postgres_types::ToSql;
use ser::Interaction;
use ser::Mentionable;
use tokio_postgres::GenericClient;
use tokio_postgres::Transaction;

#[derive(Clone, Copy)]
struct Modmail {
    ticket_num: i32,
    modmailch: ser::ChannelId,
    modmailmsg: ser::MessageId,
    staff_role: ser::RoleId,
}

impl Modmail {
    const LOOKUP_COMMAND: &str =
        "SELECT ticket_num, modmailch, modmailmsg, staff_role FROM modmail WHERE guild_id = $1";
    pub async fn lookup<C: GenericClient>(client: &C, gid: ser::GuildId) -> Result<Option<Self>> {
        let row = client
            .query_opt(Self::LOOKUP_COMMAND, &[&u64::from(gid).to_be_bytes()])
            .await?;
        let row = match row {
            Some(r) => Some({
                Self {
                    ticket_num: r.try_get(0)?,
                    modmailch: ser::ChannelId(vec_to_u64(r.try_get(1)?)?),
                    modmailmsg: ser::MessageId(vec_to_u64(r.try_get(2)?)?),
                    staff_role: ser::RoleId(vec_to_u64(r.try_get(3)?)?),
                }
            }),
            None => None,
        };
        Ok(row)
    }

    const UPDATE_COMMAND: &str = "INSERT INTO modmail (guild_id, ticket_num, modmailch, modmailmsg, staff_role) VALUES ($1, $2, $3, $4, $5)
                                  ON CONFLICT (guild_id) DO UPDATE SET ticket_num = $2, modmailch = $3, modmailmsg = $4, staff_role = $5";
    pub async fn update(&self, gid: ser::GuildId, trans: &Transaction<'_>) -> Result<()> {
        let Self {
            ticket_num,
            modmailch,
            modmailmsg,
            staff_role,
        } = self;
        trans
            .execute(
                Self::UPDATE_COMMAND,
                &[
                    &gid.0.to_be_bytes() as &(dyn ToSql + Sync),
                    ticket_num,
                    &modmailch.0.to_be_bytes(),
                    &modmailmsg.0.to_be_bytes(),
                    &staff_role.0.to_be_bytes(),
                ],
            )
            .await?;
        Ok(())
    }
}

#[poise::command(slash_command, guild_only = true)]
pub async fn modmail(
    ctx: PoiseContext<'_>,
    #[description = "Channel to post the message in"] modmailch: ser::ChannelId,
    #[description = "Ticket number to count from"]
    #[min = 0]
    ticket_num: Option<i32>,
    #[description = "Staff role"] staff_role: Option<ser::RoleId>,
    #[description = "Button name"] button_name: Option<String>,
) -> Result<()> {
    let mut conn = ctx.data().dbconn.get().await?;
    let trans = conn.transaction().await?;

    let msg = modmailch
        .send_message(ctx, |r| {
            r.components(|c| {
                c.create_action_row(|r| {
                    r.create_button(|b| {
                        b.emoji('📩')
                            .label(button_name.as_deref().unwrap_or("Message the admins"))
                            .custom_id("start_modmail")
                    })
                })
            })
        })
        .await?;

    let gid = ctx.guild_id().unwrap();
    let settings = match Modmail::lookup(&trans, gid).await? {
        Some(m) => {
            if let Err(e) = m.modmailch.delete_message(ctx, m.modmailmsg).await {
                ctx.say(format!("Failed to delete previous modmail message: {e}"))
                    .await
                    .logerr();
            }
            Modmail {
                ticket_num: ticket_num.unwrap_or(m.ticket_num),
                modmailch,
                modmailmsg: msg.id,
                staff_role: staff_role.unwrap_or(m.staff_role),
            }
        }
        None => Modmail {
            ticket_num: ticket_num.unwrap_or(0),
            modmailch,
            modmailmsg: msg.id,
            staff_role: match staff_role {
                Some(x) => x,
                None => {
                    ctx.say("Missing argument: staff_role").await?;
                    return Ok(());
                }
            },
        },
    };

    settings.update(gid, &trans).await?;
    trans.commit().await?;
    ctx.say("Successfully updated!").await?;
    Ok(())
}

pub async fn modmail_process_events<'a>(
    ctx: &'a ser::Context,
    evt: &'a Event<'a>,
    _fwctx: FrameworkContext<'a, crate::Data, SuzuError>,
    data: &crate::Data,
) -> Result<()> {
    use Event::*;
    match evt {
        InteractionCreate {
            interaction: Interaction::MessageComponent(compinter),
        } if compinter.data.custom_id.as_str() == "start_modmail" => {
            start_modmail(compinter, data, ctx).await?;
        }
        InteractionCreate {
            interaction: Interaction::MessageComponent(compinter),
        } if compinter.data.custom_id.as_str() == "close_modmail" => {
            close_modmail(compinter, data, ctx).await?;
        }
        _ => {}
    }
    Ok(())
}

async fn start_modmail(
    compinter: &ser::MessageComponentInteraction,
    data: &crate::Data,
    ctx: &ser::Context,
) -> Result<()> {
    let Some(gid) = compinter.guild_id else {
        return Ok(());
    };
    let mut conn = data.dbconn.get().await?;
    let trans = conn.transaction().await?;
    let Some(mut settings) = Modmail::lookup(&trans, gid).await? else {
        return Ok(());
    };
    if settings.modmailmsg != compinter.message.id {
        return Ok(());
    };
    let confirmation = ask_yn(ctx, &compinter.user, async |ar| {
        compinter
            .create_interaction_response(ctx, |r| {
                r.kind(ser::InteractionResponseType::ChannelMessageWithSource)
                    .interaction_response_data(|m| {
                        m.content("Are you sure?")
                            .ephemeral(true)
                            .components(|c| c.add_action_row(ar))
                    })
            })
            .await?;
        Ok(Cow::Owned(compinter.get_interaction_response(ctx).await?))
    })
    .await?
    .unwrap_or(false);

    if !confirmation {
        compinter
            .edit_original_interaction_response(ctx, |m| {
                m.components(|c| c).content("Modmail cancelled")
            })
            .await?;
        return Ok(());
    }

    let thread = settings
        .modmailch
        .create_private_thread(ctx, |t| {
            t.name(format_args!(
                "Ticket {ticket_num:04}",
                ticket_num = settings.ticket_num
            ))
        })
        .await?;

    let guild_id = compinter.guild_id.expect("modmail message not in server");
    log_modmail(
        ctx,
        &compinter.user,
        &data,
        guild_id,
        compinter.channel_id,
        thread.id,
    )
    .await
    .contextualize(LogErrorContext::Log(guild_id, LogType::Modmail))
    .logerr();

    thread.edit_thread(ctx, |t| t.invitable(false)).await?;
    settings.ticket_num += 1;
    settings.update(gid, &trans).await?;
    trans.commit().await?;
    thread
        .send_message(ctx, |m| {
            m.content(format_args!(
                "<@{starter_id}> started a new ticket. <@&{staff_role}>",
                starter_id = compinter.user.id,
                staff_role = settings.staff_role
            ))
            .components(|c| {
                c.create_action_row(|ar| {
                    ar.create_button(|b| b.label("Close").emoji('🔒').custom_id("close_modmail"))
                })
            })
        })
        .await?;

    compinter
        .edit_original_interaction_response(ctx, |m| {
            m.content(format_args!(
                "Created new ticket: {thread}",
                thread = thread.mention()
            ))
            .components(|c| c)
        })
        .await?;
    Ok(())
}

async fn close_modmail(
    compinter: &ser::MessageComponentInteraction,
    data: &crate::Data,
    ctx: &ser::Context,
) -> Result<()> {
    let Some(gid) = compinter.guild_id else {
        return Ok(());
    };
    let conn = data.dbconn.get().await?;
    let Some(settings) = Modmail::lookup(&*conn, gid).await? else {
        return Ok(());
    };
    let members = compinter.channel_id.get_thread_members(ctx).await?;
    for member in members {
        let Some(uid) = member.user_id else { continue };
        let Ok(user) = uid.to_user(ctx).await else {
            continue;
        };
        let Ok(has_role) = user.has_role(ctx, gid, settings.staff_role).await else {
            continue;
        };
        if !has_role {
            compinter
                .channel_id
                .remove_thread_member(ctx, uid)
                .await
                .logerr();
        }
    }
    compinter
        .edit_original_message(ctx, |m| {
            m.interaction_response_data(|r| r.content("Ticket closed").components(|c| c))
        })
        .await?;
    compinter
        .channel_id
        .edit_thread(ctx, |t| t.archived(true))
        .await?;
    Ok(())
}
