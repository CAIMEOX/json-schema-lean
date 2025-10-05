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

open JsonType in
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
    | _ => Except.error "not a string"

mutual
  inductive Schema where
    | Boolean : Bool → Schema
    | Object : SchemaObject → Schema

  inductive ItemsSchema where
    | Single : Schema → ItemsSchema
    | Tuple : Array Schema → ItemsSchema

  structure SchemaObject where
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
    properties : Option (Array (String × Schema))

    items : Option ItemsSchema

    allOf : Option (Array Schema)
    anyOf : Option (Array Schema)
    oneOf : Option (Array Schema)
end

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
      | _ => Except.ok #[JsonType.AnyType]
    | Except.error _ =>
      Except.ok #[JsonType.AnyType]

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

def parseProperties (j : Json) : Except String (Option (Array (String × Json))) := do
  let c := j.getObjVal? "properties"
  match c with
    | Except.ok propJson => do
      let obj <- propJson.getObj?
      let props <- obj.foldlM (init := #[]) fun acc key val => do
        Except.ok (acc.push (key, val))
      Except.ok (some props)
    | Except.error _ => Except.ok none

def parseAllOf (j : Json) : Except String (Option (Array Json)) := do
  let c := j.getObjVal? "allOf"
  match c with
    | Except.ok j => do
      let arr <- j.getArr?
      Except.ok (some arr)
    | Except.error _ => Except.ok none

def parseAnyOf (j : Json) : Except String (Option (Array Json)) := do
  let c := j.getObjVal? "anyOf"
  match c with
    | Except.ok j => do
      let arr <- j.getArr?
      Except.ok (some arr)
    | Except.error _ => Except.ok none

def parseOneOf (j : Json) : Except String (Option (Array Json)) := do
  let c := j.getObjVal? "oneOf"
  match c with
    | Except.ok j => do
      let arr <- j.getArr?
      Except.ok (some arr)
    | Except.error _ => Except.ok none

def parseItems (j : Json) : Except String (Option (Sum Json (Array Json))) := do
  let c := j.getObjVal? "items"
  match c with
    | Except.ok j =>
      match j with
      | .arr ar => Except.ok (.some (.inr ar))
      | _ => Except.ok (.some (.inl j))
    | Except.error _ => Except.ok none

partial def schemaFromJson (j : Json) : Except String Schema := do
  match j with
  | Json.bool b => Except.ok (Schema.Boolean b)
  | _ => do
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

    -- Recursively parse the schema JSON values
    let properties <- match (<- parseProperties j) with
      | some props => do
        let parsed <- props.mapM fun (key, val) => do
          let schema <- schemaFromJson val
          Except.ok (key, schema)
        Except.ok (some parsed)
      | none => Except.ok none

    let allOf <- match (<- parseAllOf j) with
      | some schemas => do
        let parsed <- schemas.mapM schemaFromJson
        Except.ok (some parsed)
      | none => Except.ok none

    let anyOf <- match (<- parseAnyOf j) with
      | some schemas => do
        let parsed <- schemas.mapM schemaFromJson
        Except.ok (some parsed)
      | none => Except.ok none

    let items <- match (<- parseItems j) with
      | some (.inl j) =>
          let parsed <- schemaFromJson j
          Except.ok (some (ItemsSchema.Single parsed))
      | some (.inr array) => do
          let parsed <- array.mapM schemaFromJson
          Except.ok (some (ItemsSchema.Tuple parsed))
      | none => Except.ok none

    let oneOf <- match (<- parseOneOf j) with
      | some schemas => do
        let parsed <- schemas.mapM schemaFromJson
        Except.ok (some parsed)
      | none => Except.ok none

    Except.ok (Schema.Object {
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
      required,
      properties,
      items,
      allOf,
      anyOf,
      oneOf
    })

instance : FromJson Schema where
  fromJson? := schemaFromJson
