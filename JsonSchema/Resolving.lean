import JsonSchema.Schema
import JsonSchema.SchemaPointer

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

structure Resolver where
  rootSchemas : Std.HashMap URI Schema :=
    Std.HashMap.emptyWithCapacity
  registeredPaths : Std.HashMap URI (URI × List String) :=
    Std.HashMap.emptyWithCapacity

def Resolver.addRootSchema (r : Resolver) (schema : Schema) (baseURI : URI := default)
    : Resolver :=
  { r with
    rootSchemas := r.rootSchemas.insert baseURI schema }

def Resolver.addPath (r : Resolver) (key : URI) (baseURI : URI) (path : List String) :=
  { r with registeredPaths := r.registeredPaths.insert key.normalize (baseURI, path) }

def SchemaObject.foldDefinitions (o : SchemaObject) (f : α → String × Schema → α) (init : α) : α :=
  ((o.definitions <&> Std.TreeMap.Raw.toList).getD []).foldl f init

def SchemaObject.foldProperties (o : SchemaObject) (f : α → String × Schema → α) (init : α) : α :=
  ((o.properties).getD #[]).foldl f init

def SchemaObject.foldPatternProperties (o : SchemaObject) (f : α → String × Schema → α) (init : α) : α :=
  ((o.patternProperties).getD #[]).foldl f init

def SchemaObject.foldDependencies (o : SchemaObject) (f : α → String × Schema → α) (init : α) : α :=
  let dependencySchemas : Option (Array (String × Schema)) := o.dependencies <&> fun xs =>
    xs.filterMap (fun (name, dependencySchema) =>
      match dependencySchema with
      | DependencySchema.SchemaDep schema => some (name, schema)
      | _ => none
    )
  (dependencySchemas.getD #[]).foldl f init

def SchemaObject.foldItems (o : SchemaObject) (f : α → Schema × Nat → α)  (init : α) : α :=
  let itemSchemas : Option (Array Schema) := o.items <&> fun itemschemas =>
    match itemschemas with
    | ItemsSchema.Single s => #[s]
    | .Tuple xs => xs
  (itemSchemas.getD #[]).zipIdx.foldl f init

def SchemaObject.foldAllOf (o : SchemaObject) (f : α → Schema × Nat → α)  (init : α) : α :=
  (o.allOf.getD #[]).zipIdx.foldl f init

def SchemaObject.foldAnyOf (o : SchemaObject) (f : α → Schema × Nat → α)  (init : α) : α :=
  (o.anyOf.getD #[]).zipIdx.foldl f init

def SchemaObject.foldOneOf (o : SchemaObject) (f : α → Schema × Nat → α)  (init : α) : α :=
  (o.oneOf.getD #[]).zipIdx.foldl f init

-- I think this can be simplified...
def Schema.foldStackAux (schema : Schema) (pathStack : List String) (baseURI : URI)
    (f : α → Schema → List String → URI → α) (init : α) : α :=
  let baseURI : URI := (schema.getID? baseURI).getD baseURI
  match schema with
  | Schema.Boolean _ => init
  | Schema.Object o =>
      let init : α := o.foldDefinitions (init := init) fun x (key, definition) =>
        f x definition (key::"definitions"::pathStack) baseURI
      let init : α := o.foldProperties (init := init) fun x (key, property) =>
        f x property (key::"properties"::pathStack) baseURI
      let init : α := o.foldPatternProperties (init := init) fun x (key, property) =>
        f x property (key::"patternProperties"::pathStack) baseURI
      let init : α := (o.propertyNames <&>
        (f init · ("propertyNames"::pathStack) baseURI)).getD init
      let init : α := o.foldDependencies (init := init) fun x (key, property) =>
        f x property (key::"dependencies"::pathStack) baseURI
      let init : α := o.foldItems (init := init) fun x (item, i) =>
        f x item (toString i::"items"::pathStack) baseURI
      let init : α := (o.additionalItems <&>
        (f init · ("additionalItems"::pathStack) baseURI)).getD init
      let init : α := (o.contains <&>
        (f init · ("contains"::pathStack) baseURI)).getD init
      let init : α := o.foldAllOf (init := init) fun x (allOf, i) =>
        f x allOf (toString i::"allOf"::pathStack) baseURI
      let init : α := o.foldAnyOf (init := init) fun x (anyOf, i) =>
        f x anyOf (toString i::"anyOf"::pathStack) baseURI
      let init : α := o.foldOneOf (init := init) fun x (oneOf, i) =>
        f x oneOf (toString i::"oneOf"::pathStack) baseURI
      let init : α := (o.not <&> (f init · ("not"::pathStack) baseURI)).getD init
      let init : α := (o.ifSchema <&> (f init · ("if"::pathStack) baseURI)).getD init
      let init : α := (o.thenSchema <&> (f init · ("then"::pathStack) baseURI)).getD init
      let init : α := (o.elseSchema <&> (f init · ("else"::pathStack) baseURI)).getD init
      init

-- TODO: Termination should follow from fold attach logic: everything we fold over
--  should have a smaller sizeOf.
/-- Recursively does an inorder traversal of schema along all valid subschemas. Note
  that the baseURI passed to f is the parents!
-/
partial def Schema.foldStack (schema : Schema) (pathStack : List String) (baseURI : URI)
    (f : α → Schema → List String → URI → α) (init : α) : α:=
  Schema.foldStackAux schema pathStack baseURI (init := f init schema pathStack baseURI)
    fun x s path baseURI => s.foldStack path baseURI f (f x s path baseURI)

def Resolver.registerPaths (r : Resolver) (schema : Schema) (rootURI : URI)
    : Resolver :=
  schema.foldStack [] rootURI (init := r) fun r schema pathStack baseURI =>
    if let some id := schema.getID? baseURI then
      r.addPath id rootURI pathStack.reverse
    else r

def Resolver.addSchema (r : Resolver) (schema : Schema) (baseURI : URI := default)
    : Resolver :=
  (r.addRootSchema schema baseURI).registerPaths schema baseURI

def getFragmentPath (uri : URI) : Option (List String) :=
  match JsonPointer.parse <$> uri.getFragment with
  | .some (.ok path) => .some path
  | _ => none

partial def Resolver.resolvePath (r : Resolver) (uri : URI) : URI × List String :=
  let uri := uri.normalize -- Make sure to normalize
  -- First, we check the map:
  if let some (uri, path) := r.registeredPaths[uri]? then
    (uri, path)
  else if let some fragmentPath := getFragmentPath uri then
    let (uri, path) := r.resolvePath { uri with fragment := none }
    (uri, path ++ fragmentPath)
  else
    (uri, [])

def Resolver.getSchemaFromRoot? (r : Resolver) (uri : URI) (path : List String) : Option Schema :=
  r.rootSchemas.get? uri >>= (·.navigate? path)

def Resolver.getSchemaAndURI? (r : Resolver) (uri : URI) (path : List String) : Option (Schema × URI) :=
  r.rootSchemas.get? uri >>= (·.navigateWithURI? path uri)

def Resolver.getSchema? (r : Resolver) (uri : URI) : Option Schema :=
  let (base, path) := r.resolvePath uri
  r.getSchemaFromRoot? base path

-- TODO: This is missing the evil `dependencies`
partial def Schema.getEvilRefs (s : Schema) : Array (URI ⊕ RelativeRef) :=
  match s with
  | Schema.Boolean _ => #[]
  | Schema.Object o => Id.run do
    let allOf := (Array.flatMap (·.getEvilRefs) <$> o.allOf).getD #[]
    let anyOf := (Array.flatMap (·.getEvilRefs) <$> o.anyOf).getD #[]
    let oneOf := (Array.flatMap (·.getEvilRefs) <$> o.oneOf).getD #[]
    let not := ((·.getEvilRefs) <$> o.not).getD #[]
    let ifRefs := ((·.getEvilRefs) <$> o.ifSchema).getD #[]
    let thenRefs := ((·.getEvilRefs) <$> o.thenSchema).getD #[]
    let elseRefs := ((·.getEvilRefs) <$> o.elseSchema).getD #[]
    let refs := match o.ref with
      | .some ref => #[ref]
      | .none => #[]
    return allOf ++ anyOf ++ oneOf ++ not ++
      ifRefs ++ thenRefs ++ elseRefs ++ refs

abbrev ResolverGraph := Std.TreeMap String (List String)

def fullPath (rootURI : URI) (pathStack : List String) : String :=
  toString rootURI ++ "#" ++ JsonPointer.toString pathStack

def ResolverGraph.addOneSchema (graph : ResolverGraph) (r : Resolver) (schema : Schema)
    (rootURI : URI) (pathStack : List String) : ResolverGraph := Id.run do
  let mut acc := graph
  let key := fullPath rootURI pathStack.reverse
  acc := acc.insert key []
  for ref in schema.getEvilRefs do
    let value := r.resolvePath (rootURI.resolveURIorRef ref)
    acc := acc.modify key (.cons (fullPath.uncurry value))
  acc

partial def ResolverGraph.addSchema (graph : ResolverGraph) (r : Resolver) (schema : Schema)
    (rootURI : URI) (pathStack : List String := []) : ResolverGraph :=
  schema.getDefinitions.foldl (init :=
    graph.addOneSchema r schema rootURI pathStack
  ) (fun g (key, definition) =>
    g.addSchema r definition rootURI (key::"definitions"::pathStack)
  )

def ResolverGraph.fromResolver (r : Resolver) : ResolverGraph := Id.run do
  let mut graph : ResolverGraph := Std.TreeMap.empty
  for (rootURI, schema) in r.rootSchemas.toList do
    graph := graph.addSchema r schema rootURI
  graph


inductive VisitStatus where
  | unvisited
  | processing
  | processed
  deriving BEq

inductive StackOptions where
  | candidate (parent : String)
  | postorder
  deriving BEq

structure DFSState where
  visited : Std.HashMap String VisitStatus
  stack : List (StackOptions × String)
  parents : Std.HashMap String String
  loop : Option (String × String)

partial def ResolverGraph.dfsAtV (g : ResolverGraph) (state : DFSState) : DFSState :=
  match state.stack with
  | [] => state -- Yay, we're done
  | (option, v)::vs =>
    match option with
    | .candidate parent => match state.visited.getD v VisitStatus.processed with
      | .processed => g.dfsAtV { state with stack := vs } -- We move on
      | .processing => -- We've found a loop starting at v -> parent
        { state with loop := some (v, parent) }
      | .unvisited =>
        let edges := (g.getD v []).map (.candidate v, ·)
        g.dfsAtV { state with
          visited := state.visited.insert v .processing
          parents := state.parents.insert v parent
          stack := edges ++ [(.postorder, v)] ++ vs
        }
    | .postorder => g.dfsAtV { state with
      stack := vs
      visited := state.visited.insert v .processed
    }

partial def getCycleReversed (parents : Std.HashMap String String) (head tail : String) (path : List String := []) :=
  if head == tail then head::path else
    getCycleReversed parents head (parents.getD tail head) (tail::path)


def ResolverGraph.dfs (g : ResolverGraph) : Option (List String) := Id.run do
  let mut state : DFSState := {
    visited := g.keys.foldl (init := .emptyWithCapacity) fun visited key =>
      visited.insert key .unvisited
    stack := []
    parents := .emptyWithCapacity
    loop := none
  }

  for i in g.keys do
    if state.visited.getD i .processed == .processed then
      continue
    state := g.dfsAtV { state with stack := [(.candidate i, i)] }
    if let some (head, tail) := state.loop then
      return getCycleReversed state.parents head tail

  return none
