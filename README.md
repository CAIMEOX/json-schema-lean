# JSON-Schema Lean
## Description

An implementation of JSON Schema in Lean (GSoC 2024)

## Build
Build docker image
```sh
docker build --quiet -f Dockerfile -t lean-jsonschema .
```

Run tests
```sh
bowtie run --dialect 7 -i localhost/lean-jsonschema:latest
```

## Design
The project is (currently) divided into the following modules:
- Main : Entry point of the project
- Data : Basic type definitions for JSON data
- Parse: Functions to parse JSON data into Lean data types
- Validation: Functions to validate JSON data against a schema
- Harness : Command reader and dispatcher, handles stdin and stdout

The Harness module reads **test cases** from stdin. 
It then parses the commands and dispatches to specific action.
It also passes data and schema to the Validation module, which returns the result of the validation back to the Harness module. The Main module then writes the result received from Harness to stdout.

## TODO 
- [x] Containerize (Docker Image)
- [x] Basic command dispatcher
- [ ] Add basic supports for JSON-Schema validation