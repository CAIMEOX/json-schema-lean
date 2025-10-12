import JsonSchema.Schema
import JsonSchema.Resolving
import LeanUri

/-! # Tests for Schema Resolving -/

open Lean
open LeanUri
open JsonSchema

def schemaString : String := include_str "example_definition.json"

def schemaJson : Json := (Json.parse schemaString).toOption.get!

def schema : Schema := (schemaFromJson schemaJson).toOption.get!

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri;
  -- Test registerPaths: should register all definitions
  let r2 := r.registerPaths schema uri;
  -- Should have registered at least one path (the root)
  r2.registeredPaths.size == 3

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri;
  let r2 := r.addPath uri uri ["foo", "bar"];
  -- Should be able to retrieve the path we just added
  (r2.registeredPaths.get? uri).isSome

/-- info: true -/
#guard_msgs in
#eval
  -- Test getID? on a schema with no id
  (Schema.Boolean true).getID? default |>.isNone

/-- info: true -/
#guard_msgs in
#eval
  -- Test getDefinitions returns a list (should be nonempty for example schema)
  schema.getDefinitions.length > 0

/-- info: true -/
#guard_msgs in
#eval
  -- Test getDefinition? returns some for a known definition
  let defs := schema.getDefinitions;
  match defs with
  | (k, _)::_ => (schema.getDefinition? k).isSome
  | _ => false

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#/definitions/foo/definitions/bar").toOption.get!;
  getFragmentPath uri == ["definitions", "foo", "definitions", "bar"]

/-- info: true -/
#guard_msgs in
#eval
  -- Test resolvePath: should return the base and fragment path
  let uri := (URI.parse "http://example.com/root.json#/definitions/foo").toOption.get!;
  let r : Resolver := {}
  let (base, path) := r.resolvePath uri;
  base == { uri with fragment := none } && path == ["definitions", "foo"]

/-- info: true -/
#guard_msgs in
#eval
  -- Test getPath? for a valid path
  let defs := schema.getDefinitions;
  match defs with
  | (k, s)::_ => toString (schema.navigate? ["definitions", k]) == toString (some s)
  | _ => false

/-- info: true -/
#guard_msgs in
#eval
  -- Test getSchemaFromRoot? returns some for root
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri;
  r.getSchemaFromRoot? uri [] |>.isSome

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri;
  -- Should be able to resolve the root schema
  (r.getSchema? uri).isSome

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#person").toOption.get!;
  let base := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema base;
  -- Should resolve to the 'person' definition
  (r.getSchema? uri).isSome

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#address").toOption.get!;
  let base := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema base;
  -- Should resolve to the 'address' definition
  (r.getSchema? uri).isSome

/-- info: false -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#meta").toOption.get!;
  let base := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema base;
  -- Should resolve to the 'meta' definition
  (r.getSchema? uri).isSome

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#notarealdef").toOption.get!;
  let base := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema base;
  -- Should not resolve to a definition
  (r.getSchema? uri).isNone

def schemaString2 : String := include_str "bad_definition.json"

def schemaJson2 : Json := (Json.parse schemaString2).toOption.get!

def schema2 : Schema := (schemaFromJson schemaJson2).toOption.get!

/-- info: 3 -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema2 uri
  let g : ResolverGraph := Std.TreeMap.empty
  --schema2.getPath? ["A"] <&> (g.addOneSchema r · uri ["A"])
  (g.addSchema r schema2 uri).size

/--
info: some ["http://example.com/root.json#/definitions/A", "http://example.com/root.json#/definitions/B"]
-/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema2 uri
  --r.registeredPaths
  let g : ResolverGraph := .fromResolver r
  g.dfs

/-- info: none -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri
  let g : ResolverGraph := Std.TreeMap.empty
  (g.addSchema r schema uri).dfs
