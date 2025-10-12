# JSON-Schema Lean

## Description

An implementation of JSON Schema Draft 7 in Lean (GSoC 2024)

**Current Status**: 94.9% Draft 7 compliance (241 of 254 test cases passing)

## Build

```sh
lake build
```

### Docker Build

Build docker image:

```sh
docker build -f Dockerfile -t localhost/lean-jsonschema:latest .
# Or with Podman
podman build -f Dockerfile -t localhost/lean-jsonschema:latest .
```

## Testing

There are some tests in `lake test`, but most tests rely on `bowtie`:

Install [bowtie](https://docs.bowtie.report/en/stable/).

Run tests:

```sh
# Run core passing tests
./test.sh

# Test specific keyword
bowtie suite -i localhost/lean-jsonschema:latest \
  https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/const.json \
  | bowtie summary --show failures

# Run full Draft 7 test suite
bowtie suite -i localhost/lean-jsonschema:latest \
  https://github.com/json-schema-org/JSON-Schema-Test-Suite/tree/main/tests/draft7 \
  | bowtie summary
```

## Design

For integration of this implementation with **Bowtie**, the project is (currently) divided into two main parts:

- **Main** : The entry point of the harness module, which invokes the harness repl.
- **Harness** : Command reader and dispatcher, handles stdin and stdout.
- **Implementation** : JSON Schema validation implementation in Lean.

### Implementation

Project Structure:

```
├── Dockerfile            # Dockerfile for Bowtie image
├── Harness/              # Bowtie interface
│   ├── Command.lean
│   └── Harness.lean
├── JsonSchema/
│   ├── Compiler.lean     # JSON Schema to Type (TODO)
│   ├── Error.lean        # Error types
│   ├── Format.lean       # Format validators (not yet integrated)
│   ├── Loader.lean       # Remote schema loading (TODO)
│   ├── PointerFragment.lean  # RFC 6901 JSON Pointer navigation
│   ├── Resolving.lean    # $ref/$id resolution and loop detection
│   ├── Schema.lean       # Schema data structures
│   ├── SchemaPointer.lean # Schema pointer utilities
│   └── Validation.lean   # Core validation logic
├── JsonSchemaTesting/    # Compile-time Testing
├── Main.lean             # Entry point for bowtie
├── TestRunner.lean       # Minimal lake test location
├── test.sh               # Test runner script
├── lakefile.toml
└── lean-toolchain
```

#### Core Modules

**Harness**: Handles interaction between validator and Bowtie test harness
- Implements IHOP protocol (start, dialect, run, stop commands)
- Reads JSON commands from stdin, dispatches to validator, returns results to stdout

**JsonSchema/Schema.lean**: Schema data structure definitions
- `Schema`: Either `Boolean` or `Object SchemaObject`
- `SchemaObject`: Contains all JSON Schema keywords
- JSON parsing and serialization

**JsonSchema/Validation.lean**: Core validation logic
- Individual `validate*` functions for each keyword
- `validateObject`: Orchestrates all validations
- `validateWithResolver`: Main entry point with fuel-based recursion limiting

**JsonSchema/Resolving.lean**: Reference resolution
- `Resolver`: Registry of schemas by URI
- Handles `$ref`, `$id`, and `definitions`
- Loop detection via dependency graph analysis

**JsonSchema/PointerFragment.lean**: JSON Pointer (RFC 6901)
- Parses pointer strings like `/definitions/foo`
- Navigates schema structures for fragment resolution

## TODO List

- [x] Containerize (Docker Image)
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
  - [x] pattern
  - [x] minimum
  - [x] maximum
  - [x] exclusiveMinimum
  - [x] exclusiveMaximum
  - [x] required
  - [x] uniqueItems
  - [x] multipleOf
  - [x] allOf
  - [x] anyOf
  - [x] oneOf
  - [x] not
  - [x] items
  - [x] contains
  - [x] maxItems
  - [x] minItems
  - [x] maxProperties
  - [x] minProperties
  - [x] properties
  - [x] patternProperties
  - [x] additionalItems
  - [x] additionalProperties
  - [x] propertyNames
  - [x] dependencies
  - [x] if / then / else
  - [x] $ref / $id / definitions
- [ ] Load schema (and refs) from a file
- [ ] Remote reference loading (i.e. http, https should load either beforehand or "on-demand")
- [ ] Draft 2019-09 support
- [ ] Draft 2020-12 support

### Maintenance

- [ ] Proper namespacing

### Extra goodies

- [ ] Proofs of termination/correctness
- [ ] Compile JSON Schema to Lean types like datamodel-code-generator
- [ ] Create JSON Schema from Lean types
