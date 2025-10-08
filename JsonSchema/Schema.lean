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
    --id : Option String
    --ref : Option String -- Should this be a URI? No it could be a fragment
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

    --definitions : Option (Std.TreeMap.Raw String Schema)
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

def parseUniqueItems (j : Json) : Except String Bool := do
  let c := j.getObjVal? "uniqueItems"
  match c with
    | Except.ok j => do
      let i <- j.getBool?
      Except.ok i
    | Except.error _ => Except.ok false

def parseProperties (parser : Json → Except String Schema) (j : Json) : Except String (Option (Array (String × Schema))) := do
  match j.getObjVal? "properties" with
    | Except.ok propJson => do
      let obj <- propJson.getObj?
      let props <- obj.foldlM (init := #[]) fun acc key val => do
        let schema <- parser val
        Except.ok (acc.push (key, schema))
      Except.ok (some props)
    | Except.error _ => Except.ok none

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
    Except.ok (Schema.Object {
      type := ← parseType j
      const := ← parseOptionalField j "const" Except.ok
      enum := ← parseOptionalField j "enum" (fun val => val.getArr?)
      /- Ordinary Optional Fields-/
      maxLength := ← parseOptionalField j "maxLength" parseNat
      minLength := ← parseOptionalField j "minLength" parseNat
      maximum := ← parseOptionalField j "maximum" parseFloat
      minimum := ← parseOptionalField j "minimum" parseFloat
      exclusiveMaximum := ← parseOptionalField j "exclusiveMaximum" parseFloat
      exclusiveMinimum := ← parseOptionalField j "exclusiveMinimum" parseFloat
      multipleOf := ← parseOptionalField j "multipleOf" parseFloat
      maxProperties := ← parseOptionalField j "maxProperties" parseNat
      minProperties := ← parseOptionalField j "minProperties" parseNat
      maxItems := ← parseOptionalField j "maxItems" parseNat
      minItems := ← parseOptionalField j "minItems" parseNat
      -- Required
      required := ← parseRequired j
      uniqueItems := ← parseUniqueItems j
      -- Recursive Items
      items := ← parseItems schemaFromJson j
      properties := ← parseProperties schemaFromJson j
      dependencies := ← parseDependencies schemaFromJson j
      additionalItems := ← parseOptionalField j "additionalItems" schemaFromJson
      contains := ← parseOptionalField j "contains" schemaFromJson
      -- "Evil" schema dependencies that could cause infinite loops.
      allOf := ← parseSchemaArray schemaFromJson j "allOf"
      anyOf := ← parseSchemaArray schemaFromJson j "anyOf"
      oneOf := ← parseSchemaArray schemaFromJson j "oneOf"
      not := ← parseOptionalField j "not" schemaFromJson
      ifSchema := ← parseOptionalField j "if" schemaFromJson
      thenSchema := ← parseOptionalField j "then" schemaFromJson
      elseSchema := ← parseOptionalField j "else" schemaFromJson
    })

instance : FromJson Schema where
  fromJson? := schemaFromJson
