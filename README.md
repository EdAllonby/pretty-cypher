# pretty-cypher

## What this is

This is currently a Neo4j Cypher language parser written in Haskell. This will eventually be used to format cypher queries to a standard format.

It currently supports a small subset of cypher language parsing, along with a rudimentary quasiquoter.

## Dev

Run tests with `stack test`

## Features 

### Parsing

* Match and optional match clause parsing
* Create clause parsing
* Delete and detached delete clause parsing
* With clause parsing
* Return clause parsing

For example, it is currently possible to parse the following cypher query into the DSL:

```cypher
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-[r:ACTS_IN]->()
WITH *, count(r) as roles_count
DELETE a
RETURN *
```

### Quasiquotes

* Basic quasiquote support to parse cypher text into the DSL, i.e. `data = [cypher|(p:Person {age: 30}) RETURN p]`

### Todo

See our [project board](https://github.com/EdAllonby/pretty-cypher/projects/1) for items to do.
