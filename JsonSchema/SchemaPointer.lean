import JsonSchema.Schema
import JsonSchema.PointerFragment

namespace JsonSchema.Schema

open JsonPointer

open LeanUri in
/-- Navigate through a schema following a list of reference tokens.
    Returns the schema at that location, or None if path cannot be resolved.

    This function handles special JSON Schema patterns like:
    - /definitions/name - access a definition
    - /properties/name - access a property schema
    - /patternProperties/pattern - access a pattern property schema
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
    -- Handle patternProperties/key pattern
    | "patternProperties" :: key :: rest =>
      match o.patternProperties with
      | some props =>
        match props.findSome? fun (k, v) => if k == key then some v else none with
        | some subschema => navigateWithURI? subschema rest newURI
        | none => none
      | none => none
    -- Handle propertyNames
    | "propertyNames" :: rest => o.propertyNames >>= (navigateWithURI? · rest newURI)
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

end JsonSchema.Schema
