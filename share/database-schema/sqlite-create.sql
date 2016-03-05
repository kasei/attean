CREATE TABLE IF NOT EXISTS term (
	term_id INTEGER PRIMARY KEY,
	type CHAR(7),
	datatype_id BIGINT REFERENCES term(term_id),
	value LONGTEXT NOT NULL,
	language TEXT
);

CREATE UNIQUE INDEX term_idx ON term (type, datatype_id, language, value);

CREATE TABLE IF NOT EXISTS quad (
	subject BIGINT NOT NULL REFERENCES term,
	predicate BIGINT NOT NULL REFERENCES term,
	object BIGINT NOT NULL REFERENCES term,
	graph BIGINT NOT NULL REFERENCES term,
	PRIMARY KEY (subject, predicate, object, graph)
);

CREATE INDEX quad_pogs_idx ON quad (predicate,object,graph,subject);
CREATE INDEX quad_g ON quad (graph);
