import JsonSchema.Schema
import JsonSchema.Error
import Lean.Data.Json
open Lean
open Json

-- This copys from the core library but with a few modifications on number comparison
partial def validate_const : Json -> Json -> Bool
  | null,   null   => true
  | .bool a, .bool b => a == b
  | num a,  num b  => a.toFloat == b.toFloat
  | str a,  str b  => a == b
  | arr a,  arr b  =>
    let _ : BEq Json := ⟨validate_const⟩
    a == b
  | obj a,  obj b =>
    let _ : BEq Json := ⟨validate_const⟩
    let szA := a.fold (init := 0) (fun a _ _ => a + 1)
    let szB := b.fold (init := 0) (fun a _ _ => a + 1)
    szA == szB && a.all fun field fa =>
      match b.find compare field with
      | none    => false
      | some fb => fa == fb
  | _,      _      => false


def validate_single_type (typ: JsonType) (json : Json) : ValidationError :=
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
    let r := validate_single_type t json
    match r with
      | .ok _ => true
      | .error _ => false
  )
  match type_checked with
    | false => Except.error #["Type mismatch"]
    | true =>
      match const with
      | none => fine
      | some c => if validate_const c json then fine else reportError s!"Expected {c} got " json
