import JsonSchema.Schema
import Lean.Data.Json

open Lean

def validate_single_type (typ: JsonType) (json : Json) : Bool :=
  match typ with
  | .StringType =>
    match json with
    | Json.str _ => true
    | _ => false
  | .NumberType =>
    match json with
    | Json.num _ => true
    | _ => false
  | .BooleanType =>
    match json with
    | Json.bool _ => true
    | _ => false
  | .ObjectType =>
    match json with
    | Json.obj _ => true
    | _ => false
  | .ArrayType =>
    match json with
    | Json.arr _ => true
    | _ => false
  | .NullType =>
    match json with
    | Json.null => true
    | _ => false
  | .IntegerType =>
    match json with
    | Json.num n => n.toFloat - n.toFloat.round == 0
    | _ => false
  | .AnyType => true

def validate (schema: Schema) (json : Json) : Bool :=
  let type : Array JsonType := schema.type
  type.any (fun t => validate_single_type t json)
