import JsonSchema.Schema
import JsonSchema.PointerFragment

namespace Schema

open JsonPointer

open LeanUri in
/-- Navigate through a schema following a list of reference tokens.
    Returns the schema at that location, or None if path cannot be resolved.

    This function handles special JSON Schema patterns like:
    - /definitions/name - access a definition
    - /properties/name - access a property schema
    - /items - access items schema (single or needs index for tuple)
    - /allOf/0 - access array element by index
    - etc.
-/
partial def navigateWithURI? (schema : Schema) (tokens : List String) (baseURI : URI) :
  Option (Schema × URI) :=
  match schema with
  | Schema.Boolean _ =>
    -- Boolean schemas have no subschemas
    match tokens with
    | [] => some (schema, baseURI) -- returns with URI of parent
    | _ => none
  | Schema.Object o =>
    let newURI := (schema.getID? baseURI).getD baseURI
    match tokens with
    | [] => some (schema, baseURI) -- returns with URI of parent
    -- Handle definitions/key pattern
    | "definitions" :: key :: rest =>
      match o.definitions >>= (·.get? key) with
      | some subschema => navigateWithURI? subschema rest newURI
      | none => none
    -- Handle properties/key pattern
    | "properties" :: key :: rest =>
      match o.properties with
      | some props =>
        match props.findSome? fun (k, v) => if k == key then some v else none with
        | some subschema => navigateWithURI? subschema rest newURI
        | none => none
      | none => none
    -- Handle dependencies/key pattern (only for schema dependencies)
    | "dependencies" :: key :: rest =>
      match o.dependencies with
      | some deps =>
        match deps.findSome? fun (k, v) =>
          if k == key then
            match v with
            | DependencySchema.SchemaDep s => some s
            | DependencySchema.PropertyDep _ => none
          else none with
        | some subschema => navigateWithURI? subschema rest newURI
        | none => none
      | none => none
    -- Handle items (single schema or tuple)
    | "items" :: rest =>
      match o.items with
      | some (ItemsSchema.Single s) => navigateWithURI? s rest newURI
      | some (ItemsSchema.Tuple schemas) =>
        -- For tuple, next token should be an index
        match rest with
        | idx :: rest' =>
          match idx.toNat? with
          | some n => schemas[n]? >>= (navigateWithURI? · rest' newURI)
          | none => none
        | [] => none  -- Can't return the tuple itself, it's not a schema
      | none => none

    -- Handle additionalItems
    | "additionalItems" :: rest =>
      o.additionalItems >>= (navigateWithURI? · rest newURI)

    -- Handle contains
    | "contains" :: rest =>
      o.contains >>= (navigateWithURI? · rest newURI)

    -- Handle not
    | "not" :: rest =>
      o.not >>= (navigateWithURI? · rest newURI)

    -- Handle if/then/else
    | "if" :: rest =>
      o.ifSchema >>= (navigateWithURI? · rest newURI)
    | "then" :: rest =>
      o.thenSchema >>= (navigateWithURI? · rest newURI)
    | "else" :: rest =>
      o.elseSchema >>= (navigateWithURI? · rest newURI)

    -- Handle array combinators: allOf, anyOf, oneOf
    | "allOf" :: idx :: rest =>
      match idx.toNat? with
      | some n => (o.allOf >>= (·[n]?)) >>= (navigateWithURI? · rest newURI)
      | none => none
    | "anyOf" :: idx :: rest =>
      match idx.toNat? with
      | some n => (o.anyOf >>= (·[n]?)) >>= (navigateWithURI? · rest newURI)
      | none => none
    | "oneOf" :: idx :: rest =>
      match idx.toNat? with
      | some n => (o.oneOf >>= (·[n]?)) >>= (navigateWithURI? · rest newURI)
      | none => none

    -- No valid pattern matched - cannot resolve
    | _ => none

def navigate? (schema : Schema) (tokens : List String) : Option Schema :=
  (schema.navigateWithURI? tokens default) <&> (·.1)

/-- Navigate a schema using a JSON Pointer string.
    Returns the schema at that location, or an error if the pointer is invalid
    or cannot be resolved. -/
def navigateByPointer (schema : Schema) (pointer : String) : Except String Schema :=
  match JsonPointer.parse pointer with
  | Except.error e => Except.error e
  | Except.ok tokens =>
    match navigate? schema tokens with
    | some s => Except.ok s
    | none => Except.error s!"Cannot resolve pointer '{pointer}' in schema"

end Schema

/-! # Tests -/

-- Test escaping/unescaping
/-- info: "~0" -/
#guard_msgs in
#eval JsonPointer.escapeToken "~"

/-- info: "~1" -/
#guard_msgs in
#eval JsonPointer.escapeToken "/"

/-- info: "~1~0" -/
#guard_msgs in
#eval JsonPointer.escapeToken "/~"

/-- info: "~" -/
#guard_msgs in
#eval JsonPointer.unescapeToken "~0"

/-- info: "/" -/
#guard_msgs in
#eval JsonPointer.unescapeToken "~1"

/-- info: "~1" -/
#guard_msgs in
#eval JsonPointer.unescapeToken "~01"

-- Test parsing
/-- info: Except.ok [] -/
#guard_msgs in
#eval JsonPointer.parse ""

/-- info: Except.ok ["foo"] -/
#guard_msgs in
#eval JsonPointer.parse "/foo"

/-- info: Except.ok ["foo", "bar"] -/
#guard_msgs in
#eval JsonPointer.parse "/foo/bar"

/-- info: Except.ok ["a/b"] -/
#guard_msgs in
#eval JsonPointer.parse "/a~1b"

/-- info: Except.ok ["definitions", "person"] -/
#guard_msgs in
#eval JsonPointer.parse "/definitions/person"

/-- info: Except.ok ["properties", "name"] -/
#guard_msgs in
#eval JsonPointer.parse "/properties/name"

/-- info: Except.ok ["allOf", "0"] -/
#guard_msgs in
#eval JsonPointer.parse "/allOf/0"

-- Test toString
/-- info: "" -/
#guard_msgs in
#eval JsonPointer.toString []

/-- info: "/foo" -/
#guard_msgs in
#eval JsonPointer.toString ["foo"]

/-- info: "/foo/bar" -/
#guard_msgs in
#eval JsonPointer.toString ["foo", "bar"]

/-- info: "/a~1b" -/
#guard_msgs in
#eval JsonPointer.toString ["a/b"]

-- Test Schema navigation
def testSchemaJson : String := "{
  \"definitions\": {
    \"person\": {
      \"type\": \"object\",
      \"properties\": {
        \"name\": { \"type\": \"string\" },
        \"age\": { \"type\": \"integer\" }
      }
    },
    \"address\": { \"type\": \"string\" }
  },
  \"properties\": {
    \"user\": { \"$ref\": \"#/definitions/person\" }
  },
  \"allOf\": [
    { \"type\": \"object\" },
    { \"required\": [\"user\"] }
  ],
  \"not\": { \"type\": \"null\" },
  \"if\": { \"type\": \"object\" },
  \"then\": { \"properties\": { \"foo\": { \"type\": \"string\" } } },
  \"items\": { \"type\": \"string\" }
}"

open Lean in
def testSchema : Schema :=
  match Json.parse testSchemaJson >>= fromJson? with
  | Except.ok s => s
  | Except.error e => panic! s!"Failed to parse test schema: {e}"

-- Test: Navigate to root (empty pointer)
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema []).isSome

-- Test: Navigate to /definitions/person
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["definitions", "person"]).isSome

-- Test: Navigate to /definitions/address
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["definitions", "address"]).isSome

-- Test: Navigate to /definitions/nonexistent should fail
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["definitions", "nonexistent"]).isNone

-- Test: Navigate to /properties/user
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["properties", "user"]).isSome

-- Test: Navigate to /allOf/0
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["allOf", "0"]).isSome

-- Test: Navigate to /allOf/1
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["allOf", "1"]).isSome

-- Test: Navigate to /allOf/2 (out of bounds)
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["allOf", "2"]).isNone

-- Test: Navigate to /not
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["not"]).isSome

-- Test: Navigate to /if
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["if"]).isSome

-- Test: Navigate to /then
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["then"]).isSome

-- Test: Navigate to /items
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["items"]).isSome

-- Test: Navigate through nested path /definitions/person/properties/name
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["definitions", "person", "properties", "name"]).isSome

-- Test: Navigate through /then/properties/foo
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["then", "properties", "foo"]).isSome

-- Test: Invalid pattern (just "definitions" without key)
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["definitions"]).isNone

-- Test: Invalid pattern (just "properties" without key)
/-- info: true -/
#guard_msgs in
#eval (Schema.navigate? testSchema ["properties"]).isNone

-- Test: navigateByPointer with full pointer strings
/-- info: true -/
#guard_msgs in
#eval (Schema.navigateByPointer testSchema "/definitions/person").isOk

/-- info: true -/
#guard_msgs in
#eval (Schema.navigateByPointer testSchema "/allOf/0").isOk

/-- info: true -/
#guard_msgs in
#eval (Schema.navigateByPointer testSchema "/not").isOk

/-- info: true -/
#guard_msgs in
#eval !(Schema.navigateByPointer testSchema "/definitions/nonexistent").isOk
