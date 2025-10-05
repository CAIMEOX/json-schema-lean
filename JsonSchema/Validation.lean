import JsonSchema.Schema
import JsonSchema.Error
import Lean.Data.Json
open Lean
open Json

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
    if n.toFloat / multiple_of == (n.toFloat / multiple_of).round
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
    | Json.num n => if n.toFloat - n.toFloat.round == 0 then fine else reportError "Expected integer, got " json
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

def validate (schema: Schema) (json : Json) : ValidationError := do
  validateTypes schema.type json *>
  maybeCheck schema.const (validateConst json) *>
  maybeCheck schema.maxLength (fun t => validateMaxLength t json) *>
  maybeCheck schema.minLength (fun t => validateMinLength t json) *>
  maybeCheck schema.maximum (fun t => validateMaximum t json) *>
  maybeCheck schema.exclusiveMaximum (fun t => validateExclusiveMaximum t json) *>
  maybeCheck schema.minimum (fun t => validateMinimum t json) *>
  maybeCheck schema.exclusiveMinimum (fun t => validateExclusiveMinimum t json) *>
  maybeCheck schema.multipleOf (fun t => validateMultipleOf t json) *>
  maybeCheck schema.enum (fun t => validateEnum t json) *>
  maybeCheck schema.required (fun t => validateRequired t json) *>
  boolCheck schema.uniqueItems (fun _ => validateUniqueItems json)
