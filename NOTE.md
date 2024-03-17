# Notes About Json Schema and Bowtie
> This note refers to the `json-schema` and `bowtie` documents.

## Terminology
- validation API: functions that evaluate a schema against a specific instance
- test harness / test runner: a small program which accepts **test cases** sent from Bowtie, passes them through a validation API and returns the results to Bowtie
- test case: JSON schema and instance which Bowtie will pass to implementation
- IHOP: the input → harness → output protocol.

## Useful Links
- [Commands](https://github.com/bowtie-json-schema/bowtie/tree/main/bowtie/schemas/io/commands)

## Hard Problems
- There are two many draft versions of JSON Schema
- Lean4 does not have a regex library
- How to covert ecma regex to lean4 regex if possible
- Use `SchemaIndex(Nat)` to prevent recursion 
- Refs (load schemas) and cycles.

## To Do
- [ ] validators for formats