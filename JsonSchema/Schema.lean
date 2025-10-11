import Lean.Data.Json
import LeanUri

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
  deriving Inhabited, BEq, Repr

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
  deriving Inhabited

  inductive ItemsSchema where
    | Single : Schema → ItemsSchema
    | Tuple : Array Schema → ItemsSchema

  inductive DependencySchema where
    | PropertyDep : Array String → DependencySchema
    | SchemaDep : Schema → DependencySchema

  structure SchemaObject where
    id : Option (LeanUri.URI ⊕ LeanUri.RelativeRef)
    ref : Option (LeanUri.URI ⊕ LeanUri.RelativeRef)
    --definitions : Option (Std.TreeMap.Raw String Schema)
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

    definitions : Option (Std.TreeMap.Raw String Schema)
end

open JsonType

def Lean.Json.fromFloat (x : Float) : Json :=
  match JsonNumber.fromFloat? x with
  | .inl s => Json.str s
  | .inr f => Json.num f

def urirefToString (r : LeanUri.URI ⊕ LeanUri.RelativeRef) : String :=
  match r with
  | .inl uri => toString uri
  | .inr ref => toString ref

partial def schemaToJson (s : Schema) : Json :=
  match s with
  | Schema.Boolean b => Json.bool b
  | Schema.Object o => Id.run do
    let mut fields : List (String × Json) := []
    if let some id := o.id then fields := ("$id", Json.str (urirefToString id)) :: fields
    if let some ref := o.ref then fields := ("$ref", Json.str (urirefToString ref)) :: fields
    if o.type != #[] then
      if o.type.size == 1 then
        fields := ("type", Json.str (toString o.type[0]!)) :: fields
      else
        fields := ("type", Json.arr (o.type.map (fun t => Json.str (toString t)))) :: fields
    if let some c := o.const then fields := ("const", c) :: fields
    if let some e := o.enum then fields := ("enum", Json.arr e) :: fields
    if let some n := o.maxLength then fields := ("maxLength", Json.num n) :: fields
    if let some n := o.minLength then fields := ("minLength", Json.num n) :: fields
    if let some n := o.maximum then fields := ("maximum", Json.fromFloat n) :: fields
    if let some n := o.minimum then fields := ("minimum", Json.fromFloat n) :: fields
    if let some n := o.exclusiveMaximum then fields := ("exclusiveMaximum", Json.fromFloat n) :: fields
    if let some n := o.exclusiveMinimum then fields := ("exclusiveMinimum", Json.fromFloat n) :: fields
    if let some n := o.multipleOf then fields := ("multipleOf", Json.fromFloat n) :: fields
    if o.uniqueItems then fields := ("uniqueItems", Json.bool true) :: fields
    if let some req := o.required then fields := ("required", Json.arr (req.map Json.str)) :: fields
    if let some props := o.properties then
      let object := props.foldl (init := Std.TreeMap.Raw.empty) fun acc (k, v) => acc.insert k (schemaToJson v)
      fields := ("properties", Json.obj object) :: fields
    if let some n := o.maxProperties then fields := ("maxProperties", Json.num n) :: fields
    if let some n := o.minProperties then fields := ("minProperties", Json.num n) :: fields
    if let some deps := o.dependencies then
      let obj := deps.foldl (init := Std.TreeMap.Raw.empty) fun acc (k, v) =>
        let j := match v with
          | DependencySchema.PropertyDep array => Json.arr (array.map Json.str)
          | DependencySchema.SchemaDep s => schemaToJson s
        acc.insert k j
      fields := ("dependencies", Json.obj obj) :: fields
    if let some items := o.items then
      let j := match items with
        | ItemsSchema.Single s => schemaToJson s
        | ItemsSchema.Tuple array => Json.arr (array.map schemaToJson)
      fields := ("items", j) :: fields
    if let some s := o.additionalItems then fields := ("additionalItems", schemaToJson s) :: fields
    if let some n := o.maxItems then fields := ("maxItems", Json.num n) :: fields
    if let some n := o.minItems then fields := ("minItems", Json.num n) :: fields
    if let some s := o.contains then fields := ("contains", schemaToJson s) :: fields
    if let some array := o.allOf then fields := ("allOf", Json.arr (array.map schemaToJson)) :: fields
    if let some array := o.anyOf then fields := ("anyOf", Json.arr (array.map schemaToJson)) :: fields
    if let some array := o.oneOf then fields := ("oneOf", Json.arr (array.map schemaToJson)) :: fields
    if let some s := o.not then fields := ("not", schemaToJson s) :: fields
    if let some s := o.ifSchema then fields := ("if", schemaToJson s) :: fields
    if let some s := o.thenSchema then fields := ("then", schemaToJson s) :: fields
    if let some s := o.elseSchema then fields := ("else", schemaToJson s) :: fields
    if let some defs := o.definitions then
      let obj := defs.foldl (init := Std.TreeMap.Raw.empty) fun acc k v => acc.insert k (schemaToJson v)
      fields := ("definitions", Json.obj obj) :: fields
    Json.obj (Std.TreeMap.Raw.insertMany {} fields)

instance : ToString Schema where
  toString s := (schemaToJson s).compress

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

def parseDefinitions (parser : Json → Except String Schema) (j : Json) :
    Except String (Option (Std.TreeMap.Raw String Schema)) :=
  match j.getObjVal? "definitions" with
  | Except.ok definitionJson => do
    let obj ← definitionJson.getObj?
    let defs ← obj.foldlM (init := Std.TreeMap.Raw.empty) fun acc key val => do
      let schema ← parser val
      Except.ok (acc.insert key schema)
    return .some defs
  | Except.error _ => Except.ok none

partial def schemaFromJson (j : Json) : Except String Schema := do
  match j with
  | Json.bool b => Except.ok (Schema.Boolean b)
  | _ => do
    Except.ok (Schema.Object {
      id := ← parseOptionalField j "$id" (fun val => val.getStr? >>=
        LeanUri.parseReference)
      ref := ← parseOptionalField j "$ref" (fun val => val.getStr? >>=
        LeanUri.parseReference)
      type := ← parseType j
      const := ← parseOptionalField j "const" Except.ok
      enum := ← parseOptionalField j "enum" (fun val => val.getArr?)
      required := ← parseRequired j
      uniqueItems := ← parseUniqueItems j
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
      -- Recursive Fields
      items := ← parseItems schemaFromJson j
      properties := ← parseProperties schemaFromJson j
      dependencies := ← parseDependencies schemaFromJson j
      additionalItems := ← parseOptionalField j "additionalItems" schemaFromJson
      contains := ← parseOptionalField j "contains" schemaFromJson
      definitions := ← parseDefinitions schemaFromJson j
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

open LeanUri in
def Schema.getID? (schema : Schema) (baseURI : URI) : Option URI :=
  match schema with
  | Schema.Boolean _ => none
  | Schema.Object o => baseURI.resolveURIorRef <$> o.id

/- If theree are no definitions, then return [] -/
def Schema.getDefinitions (schema : Schema) : List (String × Schema) :=
  match schema with
  | Schema.Boolean _ => []
  | Schema.Object o => (o.definitions <&> Std.TreeMap.Raw.toList).getD []

def Schema.getDefinition? (schema : Schema) (key : String) : Option Schema :=
  match schema with
  | Schema.Boolean _ => none
  | Schema.Object o => o.definitions >>= (Std.TreeMap.Raw.get? · key)
