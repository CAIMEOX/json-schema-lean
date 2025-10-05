import JsonSchema.Schema
import JsonSchema.Error
import Lean.Data.Json
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

def Lean.JsonNumber.isInt (n : JsonNumber) : Bool := n.mantissa % (10 ^ n.exponent) == 0

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

def Float.isInt (x : Float) := x.round == x && x.isFinite

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

def validateRequired (required: Array String) (json : Json) : ValidationError :=
  match json with
  | Json.obj _ =>
    if required.all (fun r => (json.getObjVal? r).isOk)
      then fine
      else reportError s!"Object is missing required fields: {required}, got " json
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

def validateProperties (validator: Schema → Json → ValidationError) (properties : Array (String × Schema)) (json : Json) : ValidationError :=
  properties.foldlM (init := ()) fun _ (propName, propSchema) =>
    match json.getObjVal? propName with
    | .ok propValue => validator propSchema propValue
    | .error _ => fine

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

def validateObject (validator: Schema → Json → ValidationError) (schemaObj: SchemaObject) (json: Json) : ValidationError := do
  validateTypes schemaObj.type json *>
  maybeCheck schemaObj.const (validateConst json) *>
  maybeCheck schemaObj.maxLength (fun t => validateMaxLength t json) *>
  maybeCheck schemaObj.minLength (fun t => validateMinLength t json) *>
  maybeCheck schemaObj.maximum (fun t => validateMaximum t json) *>
  maybeCheck schemaObj.exclusiveMaximum (fun t => validateExclusiveMaximum t json) *>
  maybeCheck schemaObj.minimum (fun t => validateMinimum t json) *>
  maybeCheck schemaObj.exclusiveMinimum (fun t => validateExclusiveMinimum t json) *>
  maybeCheck schemaObj.multipleOf (fun t => validateMultipleOf t json) *>
  maybeCheck schemaObj.enum (fun t => validateEnum t json) *>
  maybeCheck schemaObj.required (fun t => validateRequired t json) *>
  boolCheck schemaObj.uniqueItems (fun _ => validateUniqueItems json) *>
  maybeCheck schemaObj.properties (fun properties => validateProperties validator properties json) *>
  maybeCheck schemaObj.items (fun items => validateItems validator items json) *>
  maybeCheck schemaObj.allOf (fun allOf => validateAllOf validator allOf json) *>
  maybeCheck schemaObj.anyOf (fun anyOf => validateAnyOf validator anyOf json) *>
  maybeCheck schemaObj.oneOf (fun oneOf => validateOneOf validator oneOf json) *>
  maybeCheck schemaObj.not (fun notSchema => validateNot validator notSchema json)

partial def validate (schema: Schema) (json : Json) : ValidationError :=
  match schema with
  | Boolean b => if b then fine else reportError "Boolean schema 'false' rejects all values" json
  | Object schemaObj => validateObject validate schemaObj json
