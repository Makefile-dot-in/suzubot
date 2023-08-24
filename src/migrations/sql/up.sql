--# MIGRATION: 000 Initial setup
CREATE TYPE LogType AS ENUM ('Purge', 'BotConfig');
CREATE TABLE log_channels (
	   server_id bytea,
	   log_typ LogType,
	   channel_id bytea NOT NULL,
	   
	   PRIMARY KEY (server_id, log_typ)
);

CREATE TABLE suzu_table_metadata (
	   current_migration integer
);

UPDATE suzu_table_metadata SET current_migration = 000;
