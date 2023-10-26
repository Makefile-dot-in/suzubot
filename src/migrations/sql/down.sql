--# MIGRATION: 002 Reminders

--# MIGRATION: 001 More logging
DROP TABLE reminders;

--# MIGRATION: 000 Initial setup
ALTER TYPE logtype RENAME TO logtype_old;
CREATE TYPE logtype ('Purge', 'BotConfig');
ALTER TABLE log_channels
	  ALTER COLUMN log_typ
	  		USING (log_typ::text::logtype);

