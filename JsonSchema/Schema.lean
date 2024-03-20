import Lean.Data.Json

open Lean
open Json
def Number := Float deriving BEq, Repr
inductive JsonType where
  | StringType
  | NumberType
  | IntegerType
  | BooleanType
  | ObjectType
  | ArrayType
  | NullType
  | AnyType
  deriving BEq, Repr

abbrev JsonType.asType (jt: JsonType) : Type :=
  match jt with
    | JsonType.StringType => String
    | JsonType.NumberType => Float
    | JsonType.IntegerType => Int
    | JsonType.BooleanType => Bool
    | JsonType.ObjectType => Json
    | JsonType.NullType => Unit
    | JsonType.ArrayType => Array JsonType
    | JsonType.AnyType => Json

open JsonType
instance : FromJson JsonType where
  fromJson? j := match j with
    | Json.str "string" => Except.ok StringType
    | Json.str "number" => Except.ok NumberType
    | Json.str "integer" => Except.ok IntegerType
    | Json.str "boolean" => Except.ok BooleanType
    | Json.str "object" => Except.ok ObjectType
    | Json.str "array" => Except.ok ArrayType
    | Json.str "null" => Except.ok NullType
    | Json.str "any" => Except.ok AnyType
    | Json.str _ => Except.error "not a valid type"
    | _ => Except.error "not a stirng"

inductive SchemaType where
  | string (s: String): SchemaType
  | number (n : Number) : SchemaType
  | boolean (b : Bool) : SchemaType
  | array (a : Array SchemaType) : SchemaType
  | object (o : List (String × SchemaType)) : SchemaType
  | null : SchemaType
  deriving BEq, Repr

-- JSON Schema (Draft 3) Core Schema Definition
structure Schema where
  type : Array JsonType
  const : Option Json

def parseType (j : Json) : Except String (Array JsonType) := do
  let t := j.getObjVal? "type"
  match t with
    | Except.ok t =>
      match t with
      | Json.str _ => do
        let t <- (fromJson? t)
        Except.ok #[t]
      | Json.arr a => do
        let b <- a.mapM (fromJson?)
        Except.ok b
      | _ => Except.ok #[AnyType]
    | Except.error _ =>
      Except.ok #[AnyType]

def parseConst (j : Json) : Except String (Option Json) := do
  let c := j.getObjVal? "const"
  match c with
    | Except.ok j => Except.ok (some j)
    | Except.error _ => Except.ok none

instance : FromJson Schema where
  fromJson? j := do
    let type <- parseType j
    let const <- parseConst j
    Except.ok { type := type, const }
