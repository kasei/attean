CREATE TABLE IF NOT EXISTS term (
	term_id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	type ENUM('iri', 'literal', 'blank'),
	datatype_id BIGINT REFERENCES term(term_id),
	value LONGTEXT NOT NULL,
	language TEXT,
	INDEX (datatype_id, value(64), language(64))
) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin;

CREATE TABLE IF NOT EXISTS quad (
	subject BIGINT UNSIGNED NOT NULL REFERENCES term,
	predicate BIGINT UNSIGNED NOT NULL REFERENCES term,
	object BIGINT UNSIGNED NOT NULL REFERENCES term,
	graph BIGINT UNSIGNED NOT NULL REFERENCES term,
	PRIMARY KEY (subject, predicate, object, graph),
	KEY (predicate,object,graph,subject),
	INDEX (graph)
);
