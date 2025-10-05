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

  inductive DependencySchema where
    | PropertyDep : Array String → DependencySchema
    | SchemaDep : Schema → DependencySchema

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
    maxProperties : Option Nat
    minProperties : Option Nat
    dependencies : Option (Array (String × DependencySchema))

    items : Option ItemsSchema
    additionalItems : Option Schema
    maxItems : Option Nat
    minItems : Option Nat
    contains : Option Schema

    allOf : Option (Array Schema)
    anyOf : Option (Array Schema)
    oneOf : Option (Array Schema)
    not : Option Schema

    ifSchema : Option Schema
    thenSchema : Option Schema
    elseSchema : Option Schema
end

-- Helper function to parse an optional field with a transformation
def parseOptionalField (j : Json) (fieldName : String) (transform : Json → Except String α) : Except String (Option α) := do
  match j.getObjVal? fieldName with
    | Except.ok val => do
      let result <- transform val
      Except.ok (some result)
    | Except.error _ => Except.ok none

-- Helper for parsing numbers as Nat
def parseNat (j : Json) : Except String Nat := do
  let i <- j.getNum?
  Except.ok i.toFloat.toUInt64.toNat

-- Helper for parsing numbers as Float
def parseFloat (j : Json) : Except String Float := do
  let i <- j.getNum?
  Except.ok i.toFloat

-- Helper to parse an optional schema field
def parseOptionalSchema (parser : Json → Except String Schema) (j : Json) (fieldName : String) : Except String (Option Schema) :=
  parseOptionalField j fieldName parser

-- Helper to parse an array of schemas
def parseSchemaArray (parser : Json → Except String Schema) (j : Json) (fieldName : String) : Except String (Option (Array Schema)) := do
  let arrayOpt <- parseOptionalField j fieldName (fun val => val.getArr?)
  match arrayOpt with
  | some array => do
    let schemas <- array.mapM parser
    Except.ok (some schemas)
  | none => Except.ok none

def parseRequired (j : Json) : Except String $ Option $ Array String := do
  parseOptionalField j "required" (fun val => do
    let arr <- val.getArr?
    arr.mapM (fromJson?))

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

def parseConst (j : Json) : Except String (Option Json) :=
  parseOptionalField j "const" Except.ok

def parseEnum (j : Json) : Except String (Option $ Array Json) :=
  parseOptionalField j "enum" (fun val => val.getArr?)

def parseMaxLength (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "maxLength" parseNat

def parseMinLength (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "minLength" parseNat

def parseMaximum (j : Json) : Except String (Option Float) :=
  parseOptionalField j "maximum" parseFloat

def parseMinimum (j : Json) : Except String (Option Float) :=
  parseOptionalField j "minimum" parseFloat

def parseExclusiveMaximum (j : Json) : Except String (Option Float) :=
  parseOptionalField j "exclusiveMaximum" parseFloat

def parseExclusiveMinimum (j : Json) : Except String (Option Float) :=
  parseOptionalField j "exclusiveMinimum" parseFloat

def parseMultipleOf (j : Json) : Except String (Option Float) :=
  parseOptionalField j "multipleOf" parseFloat

def parseUniqueItems (j : Json) : Except String Bool := do
  let c := j.getObjVal? "uniqueItems"
  match c with
    | Except.ok j => do
      let i <- j.getBool?
      Except.ok i
    | Except.error _ => Except.ok false

def parseMaxItems (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "maxItems" parseNat

def parseMinItems (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "minItems" parseNat

def parseProperties (parser : Json → Except String Schema) (j : Json) : Except String (Option (Array (String × Schema))) := do
  match j.getObjVal? "properties" with
    | Except.ok propJson => do
      let obj <- propJson.getObj?
      let props <- obj.foldlM (init := #[]) fun acc key val => do
        let schema <- parser val
        Except.ok (acc.push (key, schema))
      Except.ok (some props)
    | Except.error _ => Except.ok none

def parseMaxProperties (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "maxProperties" parseNat

def parseMinProperties (j : Json) : Except String (Option Nat) :=
  parseOptionalField j "minProperties" parseNat

def parseDependencies (parser : Json → Except String Schema) (j : Json) : Except String (Option (Array (String × DependencySchema))) := do
  match j.getObjVal? "dependencies" with
    | Except.ok depJson => do
      let obj <- depJson.getObj?
      let deps <- obj.foldlM (init := #[]) fun acc key val => do
        match val with
        | .arr array => do
          -- Property dependencies (array of strings)
          let strings <- array.mapM (fromJson? : Json → Except String String)
          Except.ok (acc.push (key, DependencySchema.PropertyDep strings))
        | _ => do
          -- Schema dependencies (schema object)
          let schema <- parser val
          Except.ok (acc.push (key, DependencySchema.SchemaDep schema))
      Except.ok (some deps)
    | Except.error _ => Except.ok none

def parseItems (parser : Json → Except String Schema) (j : Json) : Except String (Option ItemsSchema) := do
  match j.getObjVal? "items" with
    | Except.ok itemsJson =>
      match itemsJson with
      | .arr ar => do
        let schemas <- ar.mapM parser
        Except.ok (.some (ItemsSchema.Tuple schemas))
      | _ => do
        let schema <- parser itemsJson
        Except.ok (.some (ItemsSchema.Single schema))
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
    let maxItems <- parseMaxItems j
    let minItems <- parseMinItems j
    let exclusiveMaximum <- parseExclusiveMaximum j
    let exclusiveMinimum <- parseExclusiveMinimum j
    let required <- parseRequired j
    let maxProperties <- parseMaxProperties j
    let minProperties <- parseMinProperties j

    -- Parse schema values (these functions now handle the recursion)
    let properties <- parseProperties schemaFromJson j
    let dependencies <- parseDependencies schemaFromJson j
    let allOf <- parseSchemaArray schemaFromJson j "allOf"
    let anyOf <- parseSchemaArray schemaFromJson j "anyOf"
    let items <- parseItems schemaFromJson j
    let additionalItems <- parseOptionalSchema schemaFromJson j "additionalItems"
    let oneOf <- parseSchemaArray schemaFromJson j "oneOf"
    let not <- parseOptionalSchema schemaFromJson j "not"
    let ifSchema <- parseOptionalSchema schemaFromJson j "if"
    let thenSchema <- parseOptionalSchema schemaFromJson j "then"
    let elseSchema <- parseOptionalSchema schemaFromJson j "else"
    let contains <- parseOptionalSchema schemaFromJson j "contains"

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
      maxProperties,
      minProperties,
      dependencies,
      items,
      additionalItems,
      maxItems,
      minItems,
      contains,
      allOf,
      anyOf,
      oneOf,
      not,
      ifSchema,
      thenSchema,
      elseSchema
    })

instance : FromJson Schema where
  fromJson? := schemaFromJson
