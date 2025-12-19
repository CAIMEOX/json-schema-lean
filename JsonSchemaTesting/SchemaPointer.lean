import JsonSchema.Schema
import JsonSchema.SchemaPointer
import JsonSchema.PointerFragment
import Lean

/-! # Tests for Schema Pointer Navigation -/

open Lean
open JsonPointer

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

section Schema

open JsonSchema

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

end Schema
