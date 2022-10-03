# pretty-cypher

## What this is

This is currently a Neo4j Cypher language parser written in Haskell. This will eventually be used to format cypher queries to a standard format.

It currently supports a small subset of cypher language parsing, along with a rudimentary quasiquoter.

## Dev

Run tests with `stack test`

## Features 

### Parsing

* Match Clause parsing

### Quasiquotes

* Basic quasiquote support to parse cypher text into the DSL, i.e. `data = [cypher|(p:Person {age: 30}) RETURN p]`

### Todo

See our [project board](https://github.com/EdAllonby/pretty-cypher/projects/1) for items to do.
