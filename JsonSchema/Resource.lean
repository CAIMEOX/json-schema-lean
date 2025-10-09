import JsonSchema.Schema

/-! # Resolving Definitions in Schemas

## Schema Registration

1. Ahead of time, you should download a set of "root" schemas, which describe
all the individual Schemas under different sets of URIs.

2. Each schema contained within inside is accessible via a canonical location:
- rootsPath#/definitions/A/definitions/

Only "definitions" (and in the future $def) based paths are allowed in this view and not arbitrary keys
of the schema.

3. When we register a schema, we add it to the rootSchemas, and then
we recursively register any other new ids and anchors that can refer to
subschema.

If there is an anchor either in the $id (or in the future an $anchor), then
subchildren cannot be accessed this way.

## Schema Resolution

When you encounter a $ref, then you lazily switch to processing a new
schema found in Resolver. If there is no such schema, then fail validation.

Resolving a ref should occur in the following way, first you should
try looking up the whole thing (fragments included). If that is not found,
then strip off the fragment and look for that instead, then use the fragment
to search by appending the remaining fragment.

Afterwards, you should get a root schema + a fragment path which
can be looked up in rootSchemas.

## Loop Detection

When a schema is used to validate a JSON, there are three possible moves:
- You check some aspect of the JSON and return.
- You recurse into some substructure of the JSON with the current schema.
- You recurse into a subschema and check the JSON with that.

Since the first two "reduce" the JSON, they can be composed and will terminate
with any JSON. However, schemas including references following the "evil"
third path can cause infinite loops.

For example, two `allOf` branches could contain `$ref` to each other, causing
an infinite loop on any input JSON.

We can detect infinite loops by considering all schemas accessible, adding
edges pointing from each schema to any "evil" children, including `$ref` blocks,
and then looking for loops in the graph.

Evil blocks:
- `allOf`
- `anyOf`
- `oneOf`
- `not`
- `if`
- `then`
- `else`
- `ref`

Since you need unique IDs, this loop detection should be done
only after registering all paths so resolution can occur.

-/

open Lean
open LeanUri

def schemaString : String := include_str "example_definition.json"

def schemaJson : Json := (Json.parse schemaString).toOption.get!

def schema : Schema := (schemaFromJson schemaJson).toOption.get!

def pathToFragment (xs : List String) : String :=
  String.join (xs.map (fun x => "/definitions/" ++ x))

structure Resolver where
  rootSchemas : Std.HashMap URI Schema :=
    Std.HashMap.emptyWithCapacity
  registeredPaths : Std.HashMap URI (URI × List String) :=
    Std.HashMap.emptyWithCapacity


def Schema.getID? (schema : Schema) (baseURI : URI) : Option URI :=
  match schema with
  | Schema.Boolean _ => none
  | Schema.Object o => baseURI.resolveURIorRef <$> o.id

/- If theree are no definitions, then return [] -/
def Schema.getDefinitions (schema : Schema) : List (String × Schema) :=
  match schema with
  | Schema.Boolean _ => []
  | Schema.Object o => (o.definitions <&> Std.TreeMap.Raw.toList).getD []

def Schema.getDefinition? (schema : Schema) (key : String) : Option Schema :=
  match schema with
  | Schema.Boolean _ => none
  | Schema.Object o => o.definitions >>= (Std.TreeMap.Raw.get? · key)

def Resolver.addSchema (r : Resolver) (schema : Schema) (baseURI : URI := default)
    : Resolver :=
  { r with
    rootSchemas := r.rootSchemas.insert (
      (schema.getID? baseURI).getD baseURI
    ) schema }

def Resolver.addPath (r : Resolver) (key : URI) (baseURI : URI) (path : List String) :=
  { r with registeredPaths := r.registeredPaths.insert key (baseURI, path) }

partial def Resolver.registerPaths (r : Resolver) (schema : Schema) (rootURI : URI := default)
    (pathStack : List String) : Resolver :=
  schema.getDefinitions.foldl (init :=
    if let some id := schema.getID? rootURI then
      r.addPath id rootURI pathStack.reverse
    else r
  ) (fun r (key, definition) =>
    r.registerPaths definition rootURI (key::pathStack)
  )

def splitFragment (s : String) : List String :=
  s.splitOn "/definitions/" |>.filter (fun seg => seg != "")

def getFragmentPath (uri : URI) : List String :=
  (splitFragment <$> uri.getFragment).getD []

def Resolver.resolvePath (r : Resolver) (uri : URI) : URI × List String :=
  -- First, we check the map:
  if let some uri_and_path := r.registeredPaths[uri]? then
    uri_and_path
  else
    ({ uri with fragment := none }, getFragmentPath uri)

def Schema.getPath? (s : Schema) (path : List String) : Option Schema :=
  match path with
  | [] => some s
  | key::path =>
    if let some definition := s.getDefinition? key then
      definition.getPath? path
    else
      none

def Resolver.getSchemaFromRoot? (r : Resolver) (uri : URI) (path : List String) : Option Schema :=
  r.rootSchemas.get? uri >>= (Schema.getPath? · path)

def Resolver.getSchema? (r : Resolver) (uri : URI) : Option Schema :=
  r.resolvePath uri >>= (fun (base, path) => r.getSchemaFromRoot? base path)


/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json").toOption.get!;
  let r := Resolver.addSchema {} schema uri;
  -- Test registerPaths: should register all definitions
  let r2 := r.registerPaths schema uri [];
  -- Should have registered at least one path (the root)
  r2.registeredPaths.size > 0

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
  -- Test splitFragment and getFragmentPath
  splitFragment "/definitions/foo/definitions/bar" = ["foo", "bar"]

/-- info: true -/
#guard_msgs in
#eval
  let uri := (URI.parse "http://example.com/root.json#/definitions/foo/definitions/bar").toOption.get!;
  getFragmentPath uri = ["foo", "bar"]

/-- info: true -/
#guard_msgs in
#eval
  -- Test resolvePath: should return the base and fragment path
  let uri := (URI.parse "http://example.com/root.json#/definitions/foo").toOption.get!;
  let r : Resolver := {}
  let (base, path) := r.resolvePath uri;
  base == { uri with fragment := none } && path == ["foo"]

/-- info: true -/
#guard_msgs in
#eval
  -- Test getPath? for a valid path
  let defs := schema.getDefinitions;
  match defs with
  | (k, s)::_ => toString (schema.getPath? [k]) == toString (some s)
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
