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

instance : FromJson Schema where
  fromJson? j := do
    let t <- j.getObjVal? "type"
    match t with
      | Json.str _ => do
        let t <- (fromJson? t)
        Except.ok ({ type := #[t] } : Schema)
      | Json.arr a => do
        let b <- a.mapM (fromJson?)
        Except.ok ({ type := b } : Schema)
      | _ => Except.error ("not a valid type" ++ t.compress)
