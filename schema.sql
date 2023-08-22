-- Your SQL goes here
CREATE TYPE LogType AS ENUM ('Purge');
CREATE TABLE log_channels (
	   server_id bytea,
	   log_typ LogType,
	   channel_id bytea NOT NULL,
	   
	   PRIMARY KEY (server_id, log_typ)
);
