use poise::Command;

use crate::{errors::Result, CustomCommandData, PoiseContext, SuzuError};

/// The main command through which administrative actions are performed
pub fn admin(test_mode: bool) -> Option<Command<crate::Data, SuzuError>> {
    /// Administration related commands.
    #[poise::command(
        slash_command,
        guild_only,
        rename = "admin",
        default_member_permissions = "MANAGE_GUILD"
    )]
    async fn inner(_ctx: PoiseContext<'_>) -> Result<()> {
        Ok(())
    }

    Some(Command {
        subcommands: crate::ADMIN_COMMANDS
            .iter()
            .map(|f| f())
            .filter(|cmd| test_mode || !CustomCommandData::from_command_data(cmd).test_mode)
            .collect::<Vec<_>>(),
        ..inner()
    })
    .filter(|cmd| !cmd.subcommands.is_empty())
}
