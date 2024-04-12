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

Project Structure:

```
├── Dockerfile
├── Harness
│   ├── Command.lean
│   └── Harness.lean
├── Harness.lean
├── JsonSchema
│   ├── Compiler.lean
│   ├── Content.lean
│   ├── Draft.lean
│   ├── Error.lean
│   ├── Format.lean
│   ├── Loader.lean
│   ├── Resource.lean
│   ├── Schema.lean
│   └── Validation.lean
├── JsonSchema.lean
├── lakefile.lean
├── lake-manifest.json
├── lean-toolchain
├── Main.lean
```

The implementation is divided into the following modules:

#### Container

The Dockerfile contains the commands for building this project in a container.

#### Harness

The **Harness** module handles the interaction between Core and Bowtie. A function `repl` will read commands from the stdin and then `dispatch` them to the core through `runTest`, returning the result to stdout.

#### Json Schema (Core)

The core implementation is divided into the following modules:

- **Compiler** : Provides functions to compile raw JSON Schema into Lean Data Types for later validation.
- **Content** : Content decoder for encoded string.
- **Draft** : Represent the JSON Schema Draft (Draft 7 is prioritized for now)
- **Error** : Error type definitions for JSON Schema validation
- **Format** : Validation functions for specific format like `date-time`, `date`, `time` and etc.
- **Loader** : Resource loaders that deals with loading schema from file or url
- **Resource**: Resource manager for schema and data
- **Schema** : Basic type definitions and utils for JSON Schema
- **Validation**: Functions to validate JSON data against a schema

## TODO List

- [ ] Containerize (Docker Image)
  - [x] Build Docker Image for lean compiler
  - [x] Build Docker Image for lean-jsonschema implementation
  - [ ] Minimize Docker Image
- [x] Separated Json Schema validator and Harness module
- [x] Integrate with **Bowtie**
  - [x] Basic (run / start / dialect / stop) command dispatcher
  - [x] Read and run tests, return results
- [x] Add basic supports for JSON-Schema validation
- [ ] Complete keywords (Draft-7)
  - [x] type
  - [x] enum
  - [x] const
  - [x] minLength
  - [x] maxLength
  - [x] minimum
  - [x] maximum
  - [x] exclusiveMinimum
  - [x] exclusiveMaximum
  - [x] required
  - [x] uniqueItems
  - [x] multipleOf
  - [ ] allOf
  - [ ] anyOf
  - [ ] oneOf
  - [ ] properties
  - [ ] additionalProperties
  - [ ] not
  - [ ] if / then / else
