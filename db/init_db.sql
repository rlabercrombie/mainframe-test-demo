DROP TABLE IF EXISTS demo_table;

CREATE TABLE demo_table (
	id serial PRIMARY KEY,
	report_date DATE NOT NULL,
	report_text VARCHAR (6) NOT NULL
);