# cypher-formatter

## What this is

This is currently a Neo4j Cypher language parser. This will eventually be used to format cypher queries to a standard format.

## Todo

### MATCH Clause

* Equality checks, i.e. `MATCH (j:Person {name: 'Jennifer'})`

* Multiple 'grouped' matches, [see this](https://stackoverflow.com/questions/32742751/what-is-the-difference-between-multiple-match-clauses-and-a-comma-in-a-cypher-qu).

* Connectors with multiple dashes, '->' is the same as '-->' for example (I think)

### Other Clauses

* See https://neo4j.com/docs/cypher-manual/current/clauses/