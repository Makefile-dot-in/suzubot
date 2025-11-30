--# MIGRATION: 003 Modmail logging

-- I don't really care much for the precise details of this file and removing entries from logtype is a pain in the ass.

--# MIGRATION: 002 Reminders
DROP TABLE modmail;

--# MIGRATION: 001 More logging
DROP TABLE reminders;

--# MIGRATION: 000 Initial setup
ALTER TYPE logtype RENAME TO logtype_old;
CREATE TYPE logtype ('Purge', 'BotConfig');
ALTER TABLE log_channels
	  ALTER COLUMN log_typ
	  		USING (log_typ::text::logtype);

