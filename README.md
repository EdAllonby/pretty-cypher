# cypher-formatter

## What this is

This is currently a Neo4j Cypher language parser. This will eventually be used to format cypher queries to a standard format.

It currently supports a small subset of cypher language parsing, along with a rudimentary quasiquoter.

## Todo

### Parsing 

#### MATCH Clause

* ✔️ Equality checks, i.e. `MATCH (j:Person {name: 'Jennifer'})`

* Multiple 'grouped' matches, [see this](https://stackoverflow.com/questions/32742751/what-is-the-difference-between-multiple-match-clauses-and-a-comma-in-a-cypher-qu).

* Connectors with multiple dashes, '->' is the same as '-->' for example (I think)

#### Other Clauses

* See https://neo4j.com/docs/cypher-manual/current/clauses/

### Formatting

* We eventually need to create a formmater for our parsed Cypher DSL. 

### Quasiquotes

* Support metavariables. i.e.: `data x = [cypher|(p:Person {age: x} RETURN p)]`.  [This is a good starting point](https://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/), however there's some functions that aren't available in megaparsec. More study required.