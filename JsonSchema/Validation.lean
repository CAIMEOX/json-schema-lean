import JsonSchema.Schema
import JsonSchema.Error
import JsonSchema.Resolving
import Lean.Data.Json
import Regex

def Lean.JsonNumber.isInt (n : JsonNumber) : Bool := n.mantissa % (10 ^ n.exponent) == 0
def Float.isInt (x : Float) := x.round == x && x.isFinite

namespace JsonSchema

open Lean
open Json
open Schema

-- This copys from the core library but with a few modifications on number comparison
partial def jsonSchemaEq : Json -> Json -> Bool
  | null,   null   => true
  | .bool a, .bool b => a == b
  | num a,  num b  => a.toFloat == b.toFloat
  | str a,  str b  => a == b
  | arr a,  arr b  =>
    let _ : BEq Json := ⟨jsonSchemaEq⟩
    a == b
  | obj a,  obj b =>
    let _ : BEq Json := ⟨jsonSchemaEq⟩
    let szA := a.foldl (init := 0) (fun a _ _ => a + 1)
    let szB := b.foldl (init := 0) (fun a _ _ => a + 1)
    szA == szB && a.all fun field fa =>
      match b.get? field with
      | none    => false
      | some fb => fa == fb
  | _,      _      => false

def validateConst (const: Json) (json : Json) : ValidationError :=
  if jsonSchemaEq const json
    then fine
    else reportError s!"Expected {const}, got " json

def validateMaxLength (max_length: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.str s =>
    if s.length <= max_length
      then fine
      else reportError s!"String is too long, max length is {max_length}, got " s.length
  | _ => fine

def validateMinLength (min_length: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.str s =>
    if s.length >= min_length
      then fine
      else reportError s!"String is too short, min length is {min_length}, got " s.length
  | _ => fine

def validatePattern (pattern: String) (json : Json) : ValidationError :=
  match json with
  | Json.str s =>
    -- Compile the regex pattern
    match Regex.build pattern with
    | .ok regex =>
      -- Test if the pattern matches the string
      match regex.captures s with
      | .some _ => fine  -- Pattern matched
      | .none => reportError s!"String does not match pattern {pattern}, got " s
    | .error err => reportError s!"Invalid regex pattern {pattern}: {err}" json
  | _ => fine  -- pattern only applies to strings

def validateMaximum (maximum: Float) (json : Json) : ValidationError :=
  match json with
  | Json.num n =>
    if n.toFloat <= maximum
      then fine
      else reportError s!"Number is too large, max is {maximum}, got " n.toString
  | _ => fine

def validateExclusiveMaximum (maximum: Float) (json : Json) : ValidationError :=
  match json with
  | Json.num n =>
    if n.toFloat < maximum
      then fine
      else reportError s!"Number is too large, max is {maximum}, got " n.toString
  | _ => fine

def validateExclusiveMinimum (minimum: Float) (json : Json) : ValidationError :=
  match json with
  | Json.num n =>
    if n.toFloat > minimum
      then fine
      else reportError s!"Number is too small, min is {minimum}, got " n.toString
  | _ => fine

def validateMinimum (minimum: Float) (json : Json) : ValidationError :=
  match json with
  | Json.num n =>
    if n.toFloat >= minimum
      then fine
      else reportError s!"Number is too small, min is {minimum}, got " n.toString
  | _ => fine

def validateMultipleOf (multiple_of: Float) (json : Json) : ValidationError :=
  match json with
  | Json.num n =>
    if (n.toFloat / multiple_of).isInt
      then fine
      else reportError s!"Number is not multiple of {multiple_of}, got " n.toString
  | _ => fine

def validateEnum (enum: Array Json) (json : Json) : ValidationError :=
  if enum.any (fun e => jsonSchemaEq e json)
    then fine
    else reportError s!"Expected one of {enum}, got " json

def validateUniqueItems (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    -- Check if all items are unique by verifying no item appears more than once
    let hasDuplicates := array.any (fun item =>
      (array.filter (· == item)).size > 1
    )
    if hasDuplicates
      then reportError s!"Array has duplicate items: " json
      else fine
  | _ => fine

def validateMaxItems (maxItems: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    if array.size <= maxItems
      then fine
      else reportError s!"Array has too many items, max is {maxItems}, got {array.size}" json
  | _ => fine

def validateMinItems (minItems: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    if array.size >= minItems
      then fine
      else reportError s!"Array has too few items, min is {minItems}, got {array.size}" json
  | _ => fine

def validateRequired (required: Array String) (json : Json) : ValidationError :=
  match json with
  | Json.obj _ =>
    if required.all (fun r => (json.getObjVal? r).isOk)
      then fine
      else reportError s!"Object is missing required fields: {required}, got " json
  | _ => fine

def validateMaxProperties (maxProperties: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.obj objMap =>
    if objMap.size <= maxProperties
      then fine
      else reportError s!"Object has too many properties, max is {maxProperties}, got {objMap.size}" json
  | _ => fine

def validateMinProperties (minProperties: Nat) (json : Json) : ValidationError :=
  match json with
  | Json.obj objMap =>
    if objMap.size >= minProperties
      then fine
      else reportError s!"Object has too few properties, min is {minProperties}, got {objMap.size}" json
  | _ => fine

def validateTypeSingle (typ: JsonType) (json : Json) : ValidationError :=
  match typ with
  | .StringType =>
    match json with
    | Json.str _ => fine
    | _ => reportError "Expected string, got " json
  | .NumberType =>
    match json with
    | Json.num _ => fine
    | _ => reportError "Expected number, got " json
  | .BooleanType =>
    match json with
    | Json.bool _ => fine
    | _ => reportError "Expected boolean, got " json
  | .ObjectType =>
    match json with
    | Json.obj _ => fine
    | _ => reportError "Expected object, got " json
  | .ArrayType =>
    match json with
    | Json.arr _ => fine
    | _ => reportError "Expected array, got " json
  | .NullType =>
    match json with
    | Json.null => fine
    | _ => reportError "Expected null, got " json
  | .IntegerType =>
    match json with
    | Json.num n => if n.isInt then fine else reportError "Expected integer, got " json
    | _ => reportError "Expected integer, got " json
  | .AnyType => fine

def validateTypes (types: Array JsonType) (json : Json) : ValidationError :=
  if types.any (fun t =>
    match validateTypeSingle t json with
      | .ok _ => true
      | .error _ => false
    )
    then fine
    else reportError s!"Expected one of {types}, got " json

def maybeCheck {α : Type} (arg: Option α) (f: α -> ValidationError) : ValidationError :=
  match arg with
  | none => fine
  | some a => f a

def boolCheck (arg: Bool) (f: Unit -> ValidationError) : ValidationError :=
  if arg then f () else fine

def validateAllOf (validator: Schema → Json → ValidationError) (schemas: Array Schema) (json: Json) : ValidationError :=
  schemas.foldlM (init := ()) fun _ schema => validator schema json

def validateAnyOf (validator: Schema → Json → ValidationError) (schemas: Array Schema) (json: Json) : ValidationError :=
  let (hasValid, errors) := schemas.foldl (init := (false, #[])) fun (valid, errs) schema =>
    match validator schema json with
    | .ok _ => (true, errs)
    | .error msg => (valid, errs.push msg)
  if hasValid then fine
  else reportError s!"anyOf: no schemas matched. Errors: {errors}" json

def validateOneOf (validator: Schema → Json → ValidationError) (schemas: Array Schema) (json: Json) : ValidationError :=
  let (validCount, errors) := schemas.foldl (init := (0, #[])) fun (count, errs) schema =>
    if count > 1 then (count, errs)
    else
      let result := validator schema json
      match result with
      | .ok _ => (count + 1, errs)
      | .error msg => (count, errs.push msg)
  match validCount with
  | 1 => fine
  | 0 => reportError s!"oneOf: expected exactly 1 match but got 0. Errors: {errors}" json
  | n => reportError s!"oneOf: expected exactly 1 match but got {n}" json

def validateNot (validator: Schema → Json → ValidationError) (schema: Schema) (json: Json) : ValidationError :=
  let result := validator schema json
  match result with
  | .ok _ => reportError s!"not: expected schema to NOT match but it did" json
  | .error _ => fine

def validateContains (validator: Schema → Json → ValidationError) (schema: Schema) (json: Json) : ValidationError :=
  match json with
  | .arr elements =>
    if elements.isEmpty then
      reportError s!"contains: array is empty, expected at least one matching element" json
    else
      let hasMatch := elements.any fun elem =>
        match validator schema elem with
        | .ok _ => true
        | .error _ => false
      if hasMatch then
        fine
      else
        reportError s!"contains: no array elements matched the schema" json
  | _ => fine  -- contains only applies to arrays

def validateProperties (validator: Schema → Json → ValidationError) (properties : Array (String × Schema)) (json : Json) : ValidationError :=
  properties.foldlM (init := ()) fun _ (propName, propSchema) =>
    match json.getObjVal? propName with
    | .ok propValue => validator propSchema propValue
    | .error _ => fine

def validatePropertyNames (validator: Schema → Json → ValidationError) (propertyNamesSchema : Schema) (json : Json) : ValidationError :=
  match json with
  | Json.obj objMap =>
    -- Validate each property name (as a string) against the schema
    objMap.foldlM (init := ()) fun _ propName _ =>
      validator propertyNamesSchema (Json.str propName)
  | _ => fine  -- propertyNames only applies to objects

-- Returns validation result and set of property names that matched patterns
def validatePatternProperties (validator: Schema → Json → ValidationError) (patternProperties : Array (String × Schema)) (json : Json) : Except (Array String) (Array String) := do
  match json with
  | Json.obj objMap =>
    -- First, compile all regex patterns (fail early if any are invalid)
    let compiledPatterns ← patternProperties.mapM fun (pattern, schema) =>
      match Regex.build pattern with
      | .ok regex => .ok (regex, schema)
      | .error err => .error #[s!"Invalid regex pattern {pattern}: {err} {json.compress}"]
    -- Track which properties matched any pattern
    objMap.foldlM (init := #[]) fun x propName propValue =>
      compiledPatterns.foldlM (init := x) fun _ (regex, patternSchema) =>
        match regex.captures propName with
        | .some _ =>
          validator patternSchema propValue *>
          pure (x.push propName)
        | .none => return x
  | _ => pure #[]  -- patternProperties only applies to objects

def validateAdditionalProperties (validator: Schema → Json → ValidationError) (properties : Option (Array (String × Schema))) (patternMatchedProps : Array String) (additionalProperties : Schema) (json : Json) : ValidationError :=
  match json with
  | Json.obj objMap =>
    -- Get the set of property names defined in the schema
    let definedProps := match properties with
      | some props => props.map Prod.fst
      | none => #[]
    -- Validate each property that is NOT in defined properties AND NOT matched by patterns
    objMap.foldlM (init := ()) fun _ propName propValue =>
      if definedProps.contains propName then
        fine  -- This property is defined in properties, skip it
      else if patternMatchedProps.contains propName then
        fine  -- This property was matched by a pattern, skip it
      else
        -- This is an additional property, validate it against the schema
        validator additionalProperties propValue
  | _ => fine  -- additionalProperties only applies to objects

def validateItems (validator: Schema → Json → ValidationError) (items : ItemsSchema) (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    match items with
    | ItemsSchema.Single schema =>
      -- Validate all array elements against the same schema
      array.foldlM (init := ()) fun _ item => validator schema item
    | ItemsSchema.Tuple schemas =>
      -- Validate each array element against corresponding schema (tuple validation)
      let paired := Array.zipWith (fun item schema => (item, schema)) array schemas
      paired.foldlM (init := ()) fun _ (item, schema) => validator schema item
  | _ => fine

def validateAdditionalItems (validator: Schema → Json → ValidationError) (itemsOpt : Option ItemsSchema) (additionalItems : Schema) (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    match itemsOpt with
    | some (ItemsSchema.Tuple schemas) =>
      -- Validate elements beyond the tuple length
      let extraElements := array.extract schemas.size array.size
      extraElements.foldlM (init := ()) fun _ item => validator additionalItems item
    | _ => fine  -- additionalItems is ignored when items is a single schema or not present
  | _ => fine

def validateDependencies (validator: Schema → Json → ValidationError) (dependencies : Array (String × DependencySchema)) (json : Json) : ValidationError :=
  match json with
  | Json.obj objMap =>
    dependencies.foldlM (init := ()) fun _ (propName, depSchema) =>
      -- Check if the property exists in the object
      if objMap.contains propName then
        match depSchema with
        | DependencySchema.PropertyDep requiredProps =>
          -- Property dependencies: all required properties must exist
          requiredProps.foldlM (init := ()) fun _ reqProp =>
            if objMap.contains reqProp then
              fine
            else
              reportError s!"Property '{propName}' requires property '{reqProp}' to be present" json
        | DependencySchema.SchemaDep schema =>
          -- Schema dependencies: validate the whole object against the schema
          validator schema json
      else
        fine  -- Property doesn't exist, so dependency doesn't apply
  | _ => fine  -- dependencies only apply to objects

def validateIfThenElse (validator: Schema → Json → ValidationError) (ifSchema : Option Schema) (thenSchema : Option Schema) (elseSchema : Option Schema) (json : Json) : ValidationError :=
  match ifSchema with
  | some ifSch =>
    -- Test if the data validates against the "if" schema
    let ifResult := validator ifSch json
    match ifResult with
    | Except.ok _ =>
      -- If validation succeeded, apply "then" schema if present
      match thenSchema with
      | some thenSch => validator thenSch json
      | none => fine
    | Except.error _ =>
      -- If validation failed, apply "else" schema if present
      match elseSchema with
      | some elseSch => validator elseSch json
      | none => fine
  | none => fine  -- No "if" schema, nothing to do

def validateObject (resolver : Resolver) (baseURI : LeanUri.URI)
    (fullValidator : Resolver → LeanUri.URI → Schema → Json → ValidationError)
    (schemaObj: SchemaObject) (json: Json) : ValidationError := do
  if let some uriOrRef := schemaObj.ref then
    let (base, path) := resolver.resolvePath (baseURI.resolveURIorRef uriOrRef)
    if let some (subschema, newURI) := resolver.getSchemaAndURI? base path then
      fullValidator resolver newURI subschema json
    else
      reportError s!"Could not find {(base, path)}" json
    return -- Early return for Draft 7 of Refs
  let newBaseURI := ((Schema.Object schemaObj).getID? baseURI).getD baseURI
  let validator := fullValidator resolver newBaseURI
  validateTypes schemaObj.type json *>
  maybeCheck schemaObj.const (validateConst json) *>
  maybeCheck schemaObj.maxLength (fun t => validateMaxLength t json) *>
  maybeCheck schemaObj.minLength (fun t => validateMinLength t json) *>
  maybeCheck schemaObj.pattern (fun t => validatePattern t json) *>
  maybeCheck schemaObj.maximum (fun t => validateMaximum t json) *>
  maybeCheck schemaObj.exclusiveMaximum (fun t => validateExclusiveMaximum t json) *>
  maybeCheck schemaObj.minimum (fun t => validateMinimum t json) *>
  maybeCheck schemaObj.exclusiveMinimum (fun t => validateExclusiveMinimum t json) *>
  maybeCheck schemaObj.multipleOf (fun t => validateMultipleOf t json) *>
  maybeCheck schemaObj.enum (fun t => validateEnum t json) *>
  maybeCheck schemaObj.required (fun t => validateRequired t json) *>
  maybeCheck schemaObj.maxProperties (fun maxProperties => validateMaxProperties maxProperties json) *>
  maybeCheck schemaObj.minProperties (fun minProperties => validateMinProperties minProperties json) *>
  boolCheck schemaObj.uniqueItems (fun _ => validateUniqueItems json) *>
  maybeCheck schemaObj.properties (fun properties => validateProperties validator properties json) *>
  maybeCheck schemaObj.propertyNames (fun propertyNamesSchema =>
    validatePropertyNames validator propertyNamesSchema json) *>
  -- Validation of additional properties must exclude pattern matched properties
  (do
    let patternMatchedProps ← match schemaObj.patternProperties with
    | some patternProperties => validatePatternProperties validator patternProperties json
    | none => pure #[]
    maybeCheck schemaObj.additionalProperties (fun additionalProperties =>
      validateAdditionalProperties validator schemaObj.properties patternMatchedProps additionalProperties json)
  ) *>
  maybeCheck schemaObj.dependencies (fun dependencies => validateDependencies validator dependencies json) *>
  maybeCheck schemaObj.items (fun items => validateItems validator items json) *>
  maybeCheck schemaObj.additionalItems (fun additionalItems =>
    validateAdditionalItems validator schemaObj.items additionalItems json) *>
  maybeCheck schemaObj.maxItems (fun maxItems => validateMaxItems maxItems json) *>
  maybeCheck schemaObj.minItems (fun minItems => validateMinItems minItems json) *>
  maybeCheck schemaObj.contains (fun containsSchema => validateContains validator containsSchema json) *>
  maybeCheck schemaObj.allOf (fun allOf => validateAllOf validator allOf json) *>
  maybeCheck schemaObj.anyOf (fun anyOf => validateAnyOf validator anyOf json) *>
  maybeCheck schemaObj.oneOf (fun oneOf => validateOneOf validator oneOf json) *>
  maybeCheck schemaObj.not (fun notSchema => validateNot validator notSchema json) *>
  validateIfThenElse validator schemaObj.ifSchema schemaObj.thenSchema schemaObj.elseSchema json

def validateWithResolver (resolver : Resolver) (rootURI : LeanUri.URI) (schema: Schema) (json : Json) (fuel : Nat := 1000) : ValidationError :=
  match fuel with
  | .succ fuel =>
      match schema with
      | Boolean b => if b then fine else reportError "Boolean schema 'false' rejects all values" json
      | Object schemaObj => validateObject resolver rootURI (validateWithResolver (fuel := fuel)) schemaObj json
  | 0 => reportError s!"Stack overflow: {schema}" json

def validate (schema: Schema) (json : Json) : ValidationError :=
  let resolver := Resolver.addSchema {} schema default
  validateWithResolver resolver default schema json

end JsonSchema
