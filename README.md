# JSON-Schema Lean
## Description

An implementation of JSON Schema in Lean (GSoC 2024)

## Build
Build docker image
```sh
docker build -f Dockerfile -t localhost/lean-jsonschema .
```

Run tests
```sh
bowtie run --dialect 7 -i localhost/lean-jsonschema:latest
```

## Design
For integration of this implementation with **Bowtie**, the project is (currently) divided into two main parts:
- **Main** : The entry point of the harness module, which invokes the harness repl.
- **Harness** : Command reader and dispatcher, handles stdin and stdout.
- **Implementation** : JSON Schema validation implementation in Lean.

### Implementation
The implementation is divided into the following modules:
- Compiler : Provides functions to compile raw JSON Schema into Lean data types (A Validator).
- Content : Content decoder for encoded string.
- Draft : Represent the JSON Schema Draft (Draft 7 is prioritized for now)
- Error : Error type definitions for JSON Schema validation
- Format : Validation functions for specific format like `date-time`, `date`, `time` and etc.
- Loader : Resource loaders that deals with loading schema from file or url
- Resource: Resource manager for schema and data
- Schema : Basic type definitions and utils for JSON Schema
- Validation: Functions to validate JSON data against a schema

The key functions are as follows:
```haskell
compile :: Compiler -> Draft -> List Json -> Schema
validate :: Validator -> Schema -> Json -> Option Error
```

The Harness module reads **test cases** from stdin. 
It then parses the commands and dispatches to specific action.
It also passes data and schema to the Validation module, which returns the result of the validation back to the Harness module. The Main module then writes the result received from Harness to stdout.

## TODO 
- [x] Containerize (Docker Image)
- [x] Basic command dispatcher
- [x] Modularize the implementation
- [x] Separated logic for Core and Harness
- [ ] Add basic supports for JSON-Schema validation