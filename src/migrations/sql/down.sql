--# MIGRATION: 001 More logging

--# MIGRATION: 000 Initial setup
ALTER TYPE logtype RENAME TO logtype_old;
CREATE TYPE logtype ('Purge', 'BotConfig');
ALTER TABLE log_channels
	  ALTER COLUMN log_typ
	  		USING (log_typ::text::logtype);

