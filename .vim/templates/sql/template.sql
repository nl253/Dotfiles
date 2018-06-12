
-- SQL Script

-- Dialect: SQLite3
-- Table: Gym

-- SUMMARY
-- -------
-- The aim of this script is...

-- every transaction must begin with this

BEGIN TRANSACTION; 

CREATE TABLE gym (
	date TEXT UNIQUE PRIMARY KEY NOT NULL,
	hour FLOAT NOT NULL,
	busyness_score FLOAT NOT NULL,
	no_sets_completed FLOAT NOT NULL,
	no_used_machines INT NOT NULL,
	workout_score FLOAT NOT NULL,
	hours_of_sleep FLOAT NOT NULL
);

INSERT INTO gym VALUES('2017-05-10',5.0,4.0,13.0,4,2.0,7.0);

-- every transaction must end with this (alias for END TRANSACTION)

COMMIT;

