--# MIGRATION: 000 Initial setup
CREATE TYPE logtype AS ENUM ('Purge', 'BotConfig');
CREATE TABLE log_channels (
	   server_id bytea,
	   log_typ LogType,
	   channel_id bytea NOT NULL,
	   
	   PRIMARY KEY (server_id, log_typ)
);

CREATE TABLE suzu_table_metadata (
	   current_migration integer
);

INSERT INTO suzu_table_metadata (current_migration) VALUES (000);

--# MIGRATION: 001 More logging
ALTER TYPE logtype ADD VALUE 'MessageEdit';
ALTER TYPE logtype ADD VALUE 'MessageDelete';
ALTER TYPE logtype ADD VALUE 'UserBan';
ALTER TYPE logtype ADD VALUE 'UserKick';
ALTER TYPE logtype ADD VALUE 'UserJoin';
ALTER TYPE logtype ADD VALUE 'UserLeave';
ALTER TYPE logtype ADD VALUE 'VoiceUpdate';

