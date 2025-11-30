use std::collections::HashMap;

use crate::errors::{Contextualizable, Error, LoggedError, LoggedMappedWithContext, Result};
use crate::webhook::{create_or_return_webhook_for_channel, WebhookExecutor};
use itertools::Itertools;
use log::error;
use poise::serenity_prelude::{self as ser, ExecuteWebhook, Mentionable};
use reqwest::Url;
use std::fmt::{self, Display};

type LinkableMessage = (Option<ser::GuildId>, ser::ChannelId, ser::MessageId);

#[derive(Debug, Clone)]
pub enum ReplicationErrorContext {
    CreatingWebhookForReplication,
    ReplicatingMessage {
        message: LinkableMessage,
        target: ser::ChannelId,
        thread: Option<ser::ChannelId>,
    },
}

impl Display for ReplicationErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CreatingWebhookForReplication => {
                write!(f, "creating webhook for replicating messages")
            }
            Self::ReplicatingMessage {
                message,
                target,
                thread: None,
            } => write!(
                f,
                "logging message {message_link} to {target_mention}",
                message_link = linkable_message_to_link(*message),
                target_mention = target.mention()
            ),
            Self::ReplicatingMessage {
                message,
                target,
                thread: Some(thread),
            } => write!(
                f,
                "logging message {message_link} to {target_mention} in {thread_mention}",
                message_link = linkable_message_to_link(*message),
                target_mention = target.mention(),
                thread_mention = thread.mention()
            ),
        }
    }
}

fn linkable_message_to_link((guildid, channelid, messageid): LinkableMessage) -> String {
    messageid.link(channelid, guildid)
}

/// replicates messages into a channel.
/// `channel` must be a text channel, and `thread`, if `Some`, must be a thread
pub async fn replicate_messages<H, C, M>(
    http: H,
    bot_name: &str,
    whexec: &WebhookExecutor,
    channel: impl Into<ser::ChannelId>,
    thread: Option<impl Into<ser::ChannelId>>,
    msgs: M,
    mut customize_builder: C,
) -> Result<()>
where
    C: for<'a, 'b> FnMut(&'a mut ser::ExecuteWebhook<'b>) -> &'a mut ser::ExecuteWebhook<'b>,
    H: AsRef<ser::Http>,
    M: IntoIterator<Item = ser::Message>,
{
    let thread = thread.map(Into::into);
    let channel = channel.into();
    let webhook = create_or_return_webhook_for_channel(&http, channel, bot_name)
        .await
        .contextualize(ReplicationErrorContext::CreatingWebhookForReplication)?;
    let mut referenced: HashMap<ser::MessageId, LinkableMessage> = HashMap::new();
    for msg in msgs {
        let id = msg.id;
        let guild_id = msg.guild_id;
        let channel_id = msg.channel_id;
        let referencedmsg = msg.referenced_message.as_ref().map(|r| {
            referenced
                .get(&r.id)
                .map(|x| *x)
                .unwrap_or((r.guild_id, r.channel_id, r.id))
        });
        let newres: Result<ser::Message> = whexec
            .execute(&webhook, thread, |w| {
                customize_builder(msg_to_webhook(w, msg, referencedmsg))
            })
            .await
            .map_err(Error::from)
            .contextualize(ReplicationErrorContext::ReplicatingMessage {
                message: (guild_id, channel_id, id),
                target: channel,
                thread,
            });
        let newmsg = match newres {
            Ok(msg) => msg,
            Err(err) => {
                log::info!(
                    "logging error: {loggable_err}",
                    loggable_err = LoggedMappedWithContext(&err, LoggedError)
                );
                continue;
            }
        };
        referenced.insert(id, (newmsg.guild_id, newmsg.channel_id, newmsg.id));
    }
    Ok(())
}

/// converts a message to a webhook
fn msg_to_webhook<'a, 'b>(
    w: &'a mut ser::ExecuteWebhook<'b>,
    msg: ser::Message,
    referenced: Option<LinkableMessage>,
) -> &'a mut ser::ExecuteWebhook<'b> {
    w.username(match &msg.member {
        Some(ser::PartialMember {
            nick: Some(nick), ..
        }) => format!("{nick} [{}]", msg.author.tag()),
        _ => msg.author.tag(),
    });

    w.allowed_mentions(|f| f.empty_parse());

    if let Some(url) = msg.author.avatar_url() {
        w.avatar_url(url);
    }

    convey_message_content(w, msg, referenced);

    w
}

/// generates a simple embed without the extra fluff.
fn simple_embed(
    title: impl ToString,
    description: impl ToString,
    color: impl Into<ser::Color>,
) -> ser::json::Value {
    ser::Embed::fake(|e| e.color(color).title(title).description(description))
}

/// generate appropriate embeds and/or content for replication
/// depending on the kind of the original message
fn convey_message_content<'a, 'b>(
    w: &'a mut ExecuteWebhook<'b>,
    msg: ser::Message,
    referenced: Option<LinkableMessage>,
) -> &'a mut ExecuteWebhook<'b> {
    let (attachments, mut embeds) = attachment_to_attachment_type_or_embed(msg.attachments);
    w.add_files(attachments);

    use ser::MessageType::*;
    match msg.kind {
        Regular => {
            w.content(msg.content);
        }
        GroupRecipientAddition => embeds.push(simple_embed(
            "➕ Added User(s)",
            format!(
                "Added {}.",
                msg.mentions.iter().map(|m| m.mention()).join(", ")
            ),
            ser::Color::FOOYOO,
        )),
        GroupRecipientRemoval => embeds.push(simple_embed(
            "❌ Removed User(s)",
            format!(
                "Removed {}.",
                msg.mentions.iter().map(|m| m.mention()).join(", ")
            ),
            ser::Color::RED,
        )),
        GroupCallCreation => embeds.push(simple_embed(
            "📞 Started a Call",
            "A call was started.",
            ser::Color::DARK_GREEN,
        )),
        GroupNameUpdate => embeds.push(simple_embed(
            "📛 Changed Group Name",
            format!("The group name was changed to {}", msg.content),
            ser::Color::TEAL,
        )),
        GroupIconUpdate => embeds.push(simple_embed(
            "🖼️ Changed Group Icon",
            "The group icon was changed.",
            ser::Color::TEAL,
        )),
        PinsAdd => {
            let description = match referenced {
                Some(msg) => format!("Pinned [this]({}) message.", linkable_message_to_link(msg)),
                None => format!("Pinned a message."),
            };
            embeds.push(simple_embed(
                "📌 Pinned Message",
                description,
                ser::Color::RED,
            ))
        }
        MemberJoin => embeds.push(simple_embed(
            "👋 Joined",
            "Welcome to the server!",
            ser::Color::FOOYOO,
        )),
        NitroBoost | NitroTier1 | NitroTier2 | NitroTier3 => {
            let description = match msg.kind {
                NitroBoost => "No new levels were reached.",
                NitroTier1 => "Reached Level 1!",
                NitroTier2 => "Reached Level 2!",
                NitroTier3 => "Reached Level 3!",
                _ => unreachable!(),
            };
            embeds.push(simple_embed(
                "🚀 Boosted Server",
                description,
                ser::Color::BLURPLE,
            ))
        }
        ChannelFollowAdd => embeds.push(simple_embed(
            "📣 Followed a channel",
            format!("This channel will now receive updates from {}", msg.content),
            ser::Color::DARK_RED,
        )),
        GuildDiscoveryDisqualified => {
            embeds.push(simple_embed(
                "❌ Discovery Disqualified",
                "This server was disqualified from Discovery.",
                ser::Color::RED,
            ));
        }
        GuildDiscoveryRequalified => {
            embeds.push(simple_embed(
                "✅ Discovery Requalified",
                "This server is qualified again for Discovery.",
                ser::Color::FOOYOO,
            ));
        }
        GuildDiscoveryGracePeriodInitialWarning => {
            embeds.push(simple_embed(
                "⚠️ Discovery Disqualification Warning",
                "This is the initial warning for disqualification from Discovery.",
                ser::Color::GOLD,
            ));
        }
        GuildDiscoveryGracePeriodFinalWarning => {
            embeds.push(simple_embed(
                "⚠️ Discovery Disqualification Warning",
                "This is the **final** warning for disqualification from Discovery.",
                ser::Color::GOLD,
            ));
        }
        ThreadCreated => {
            let description = match msg.thread {
                Some(thread) => format!(
                    "Started a thread named {}: {}.",
                    msg.content,
                    thread.mention()
                ),
                None => format!("Started a thread named {}.", msg.content),
            };

            embeds.push(simple_embed(
                "🧵 Started Thread",
                description,
                ser::Color::FADED_PURPLE,
            ));
        }
        InlineReply => {
            let description = match referenced {
                Some(msg) => format!(
                    "Replied to [this]({}) message.",
                    linkable_message_to_link(msg)
                ),
                None => format!("Replied to a message."),
            };

            w.content(msg.content);
            embeds.push(simple_embed("💬 Reply", description, ser::Color::KERBAL));
        }
        ChatInputCommand | ContextMenuCommand => {
            let embed = ser::Embed::fake(|e| {
                let fields = match msg.interaction {
                    Some(interaction) => interaction_to_embed(e, interaction),
                    None => e.description("Used an interaction."),
                };
                fields
                    .title("#️⃣ Used Slash Command")
                    .color(ser::Color::BLITZ_BLUE)
            });

            w.content(msg.content);
            embeds.push(embed);
        }
        ThreadStarterMessage => {
            let description = match referenced {
                Some(msg) => format!("Copy of [this]({}) message.", linkable_message_to_link(msg)),
                None => format!("Original message was deleted."),
            };

            w.content(msg.content);
            embeds.push(simple_embed(
                "🔰 Thread Starter Message",
                description,
                ser::Color::TEAL,
            ));
        }
        GuildInviteReminder => {
            embeds.push(simple_embed(
                "⏰ Guild Invite Reminder",
                "Clyde wants to let you know that this server is too empty.",
                ser::Color::BLURPLE,
            ));
        }
        AutoModerationAction => {
            embeds.push(simple_embed(
                "🤖 Automoderation Action",
                "You can see the details below.",
                ser::Color::BLURPLE,
            ));
        }
        _ => {
            embeds.push(simple_embed(
                "❓ Unknown",
                "Unknown message type.",
                ser::Color::from_rgb(0, 0, 0),
            ));
        }
    }

    embeds.extend(msg.embeds.into_iter().map(|e| {
        ser::Embed::fake(|enew| {
            *enew = ser::CreateEmbed::from(e);
            enew
        })
    }));

    embeds.truncate(10);
    w.embeds(embeds);
    w
}

/// converts an interaction to embed form.
fn interaction_to_embed(
    emb: &mut ser::CreateEmbed,
    interaction: ser::MessageInteraction,
) -> &mut ser::CreateEmbed {
    emb.field("ID", interaction.id, true);
    use ser::InteractionType::*;
    emb.field(
        "Kind",
        match interaction.kind {
            Ping => "Ping",
            ApplicationCommand => "Command",
            MessageComponent => "Message component",
            Autocomplete => "Autocomplete",
            ModalSubmit => "Modal",
            _ => "Unknown",
        },
        true,
    );
    emb.field("Name", interaction.name, true);
    emb.field("User", interaction.user.mention(), true);
    emb
}

const MAX_DOWNLOAD_SIZE: u64 = 25_000_000; // 25 MB

/// converts the attachments of a message to a webhook
fn attachment_to_attachment_type_or_embed(
    attachments: impl IntoIterator<Item = ser::Attachment>,
) -> (Vec<ser::AttachmentType<'static>>, Vec<ser::json::Value>) {
    let mut size = 0;
    let mut attachment_types = Vec::new();
    let mut embeds = Vec::new();

    for attachment in attachments.into_iter() {
        let attachment_type_opt = ({
            size += attachment.size;
            size < MAX_DOWNLOAD_SIZE
        })
        .then_some(())
        .ok_or_else(|| None)
        .and_then(|_| {
            Url::parse(&attachment.url)
                .map(ser::AttachmentType::Image)
                .map_err(|err| {
                    Some(format!(
                        "error while parsing {} as a URL: {err}",
                        attachment.url
                    ))
                })
        });
        // `Image` actually has nothing image-specific and is just the type for urls, blame serenity

        match attachment_type_opt {
            Ok(attachment_type) => attachment_types.push(attachment_type),
            Err(e) => {
                if let Some(err) = e {
                    error!("{err}");
                }
                embeds.push(simple_embed(
                    format!("📎 Attachment: {}", attachment.filename),
                    format!("[Link][{}]", attachment.url),
                    ser::Color::DARK_GREY,
                ));
            }
        }
    }

    (attachment_types, embeds)
}
