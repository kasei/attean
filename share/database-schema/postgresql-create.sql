CREATE TYPE TERM_TYPE AS ENUM ('iri', 'literal', 'blank');
CREATE TABLE IF NOT EXISTS term (
	term_id SERIAL PRIMARY KEY,
	type TERM_TYPE,
	datatype_id BIGINT REFERENCES term(term_id),
	value TEXT NOT NULL,
	language TEXT,
	UNIQUE (datatype_id, value, language)
);

CREATE TABLE IF NOT EXISTS quad (
	subject BIGINT NOT NULL REFERENCES term,
	predicate BIGINT NOT NULL REFERENCES term,
	object BIGINT NOT NULL REFERENCES term,
	graph BIGINT NOT NULL REFERENCES term,
	PRIMARY KEY (subject, predicate, object, graph)
);

CREATE INDEX IF NOT EXISTS quad_graph_idx ON quad (graph);
CREATE INDEX IF NOT EXISTS quad_pogs_idx ON quad (predicate,object,graph,subject);
