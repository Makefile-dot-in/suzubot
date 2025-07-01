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
UPDATE suzu_table_metadata SET current_migration = 001;

--# MIGRATION: 002 Reminders
CREATE TABLE reminders (
	id SERIAL PRIMARY KEY,
	server_id bytea,
	channel_id bytea NOT NULL,
	creator_id bytea NOT NULL,
	creation_time TIMESTAMP WITH TIME ZONE NOT NULL,
	target_time TIMESTAMP WITH TIME ZONE NOT NULL,
	description VARCHAR(300) NOT NULL
);

CREATE INDEX reminders_target_time ON reminders (target_time);

UPDATE suzu_table_metadata SET current_migration = 002;

--# MIGRATION: 003 Modmail

CREATE TABLE modmail (
	guild_id bytea PRIMARY KEY,
	ticket_num INTEGER NOT NULL DEFAULT 0,
	modmailch bytea NOT NULL,
	modmailmsg bytea NOT NULL,
	staff_role bytea NOT NULL
);

UPDATE suzu_table_metadata SET current_migration = 003;
