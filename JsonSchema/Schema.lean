import Lean.Data.Json

open Lean
open Json
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

instance : ToString JsonType where
  toString jt := match jt with
    | JsonType.StringType => "string"
    | JsonType.NumberType => "number"
    | JsonType.IntegerType => "integer"
    | JsonType.BooleanType => "boolean"
    | JsonType.ObjectType => "object"
    | JsonType.ArrayType => "array"
    | JsonType.NullType => "null"
    | JsonType.AnyType => "any"

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
  | number (n : Float) : SchemaType
  | boolean (b : Bool) : SchemaType
  | array (a : Array SchemaType) : SchemaType
  | object (o : List (String × SchemaType)) : SchemaType
  | null : SchemaType
  deriving BEq, Repr

-- JSON Schema (Draft 3) Core Schema Definition
structure Schema where
  type : Array JsonType
  const : Option Json
  enum : Option $ Array Json

  maxLength : Option Nat
  minLength : Option Nat

  maximum : Option Float
  minimum : Option Float
  exclusiveMaximum : Option Float
  exclusiveMinimum : Option Float
  multipleOf : Option Float

  uniqueItems : Bool

  required : Option (Array String)

def parseRequired (j : Json) : Except String $ Option $ Array String := do
  let c := j.getObjVal? "required"
  match c with
    | Except.ok j => do
      let arr <- j.getArr?
      let b <- arr.mapM (fromJson?)
      Except.ok (some b)
    | Except.error _ => Except.ok none

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

def parseEnum (j : Json) : Except String (Option $ Array Json) := do
  let c := j.getObjVal? "enum"
  match c with
    | Except.ok j => do
      let arr <- j.getArr?
      Except.ok (some arr)
    | Except.error _ => Except.ok none

def parseMaxLength (j : Json) : Except String (Option Nat) := do
  let c := j.getObjVal? "maxLength"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat.toUInt64.toNat)
    | Except.error _ => Except.ok none

def parseMinLength (j : Json) : Except String (Option Nat) := do
  let c := j.getObjVal? "minLength"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat.toUInt64.toNat)
    | Except.error _ => Except.ok none

def parseMaximum (j : Json) : Except String (Option Float) := do
  let c := j.getObjVal? "maximum"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat)
    | Except.error _ => Except.ok none

def parseMinimum (j : Json) : Except String (Option Float) := do
  let c := j.getObjVal? "minimum"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat)
    | Except.error _ => Except.ok none

def parseExclusiveMaximum (j : Json) : Except String (Option Float) := do
  let c := j.getObjVal? "exclusiveMaximum"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat)
    | Except.error _ => Except.ok none

def parseExclusiveMinimum (j : Json) : Except String (Option Float) := do
  let c := j.getObjVal? "exclusiveMinimum"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat)
    | Except.error _ => Except.ok none

def parseMultipleOf (j : Json) : Except String (Option Float) := do
  let c := j.getObjVal? "multipleOf"
  match c with
    | Except.ok j => do
      let i <- j.getNum?
      Except.ok (some i.toFloat)
    | Except.error _ => Except.ok none

def parseUniqueItems (j : Json) : Except String Bool := do
  let c := j.getObjVal? "uniqueItems"
  match c with
    | Except.ok j => do
      let i <- j.getBool?
      Except.ok i
    | Except.error _ => Except.ok false

instance : FromJson Schema where
  fromJson? j := do
    let type <- parseType j
    let const <- parseConst j
    let maxLength <- parseMaxLength j
    let minLength <- parseMinLength j
    let maximum <- parseMaximum j
    let minimum <- parseMinimum j
    let multipleOf <- parseMultipleOf j
    let enum <- parseEnum j
    let uniqueItems <- parseUniqueItems j
    let exclusiveMaximum <- parseExclusiveMaximum j
    let exclusiveMinimum <- parseExclusiveMinimum j
    let required <- parseRequired j
    Except.ok {
      type,
      const,
      enum,
      maxLength,
      minLength,
      maximum,
      minimum,
      exclusiveMaximum,
      exclusiveMinimum,
      multipleOf,
      uniqueItems,
      required
    }
