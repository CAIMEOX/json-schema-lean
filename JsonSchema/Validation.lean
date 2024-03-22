import JsonSchema.Schema
import JsonSchema.Error
import Lean.Data.Json
open Lean
open Json

-- This copys from the core library but with a few modifications on number comparison
partial def validateConst : Json -> Json -> Bool
  | null,   null   => true
  | .bool a, .bool b => a == b
  | num a,  num b  => a.toFloat == b.toFloat
  | str a,  str b  => a == b
  | arr a,  arr b  =>
    let _ : BEq Json := ⟨validateConst⟩
    a == b
  | obj a,  obj b =>
    let _ : BEq Json := ⟨validateConst⟩
    let szA := a.fold (init := 0) (fun a _ _ => a + 1)
    let szB := b.fold (init := 0) (fun a _ _ => a + 1)
    szA == szB && a.all fun field fa =>
      match b.find compare field with
      | none    => false
      | some fb => fa == fb
  | _,      _      => false

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
  if enum.any (fun e => validateConst e json)
    then fine
    else reportError s!"Expected one of {enum}, got " json

def validateUniqueItems (json : Json) : ValidationError :=
  match json with
  | Json.arr array =>
    if array.all (fun a => array.all (fun b => a == b || a != b))
      then fine
      else reportError s!"Array has duplicate items: " json
  | _ => fine

def validateRequired (required: Array String) (json : Json) : ValidationError :=
  match json with
  | Json.obj _ =>
    if required.all (fun r => (json.getObjVal? r).isOk)
      then fine
      else reportError s!"Object is missing required fields: {required}, got " json
  | _ => fine

def validateType (typ: JsonType) (json : Json) : ValidationError :=
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

def validate (schema: Schema) (json : Json) : ValidationError :=
  let type : Array JsonType := schema.type
  let const : Option Json := schema.const
  let type_checked : Bool := type.any (fun t =>
    let r := validateType t json
    match r with
      | .ok _ => true
      | .error _ => false
  )
  match type_checked with
    | false => Except.error #["Type mismatch"]
    | true =>
      match const with
      | none => fine
      | some c => if validateConst c json then fine else reportError s!"Expected {c} got " json
