import Lean.Data.Json

open Lean
open Json

inductive JsonType where
  | StringType
  | NumberType
  | BooleanType
  | ObjectType
  | ArrayType : JsonType -> JsonType
  | NullType
  | AnyType
  deriving BEq, Repr

abbrev JsonType.asType (jt: JsonType) : Type :=
  match jt with
    | JsonType.StringType => String
    | JsonType.NumberType => Float
    | JsonType.BooleanType => Bool
    | JsonType.ObjectType => Json
    | JsonType.NullType => Unit
    | JsonType.ArrayType t => Array (JsonType.asType t)
    | JsonType.AnyType => Json

open JsonType
instance : Lean.FromJson JsonType where
  fromJson? j := match j with
    | Json.str "string" => Except.ok StringType
    | Json.str "number" => Except.ok NumberType
    | Json.str "boolean" => Except.ok BooleanType
    | Json.str "object" => Except.ok ObjectType
    | Json.str "array" => Except.ok (ArrayType AnyType)
    | Json.str "null" => Except.ok NullType
    | Json.str "any" => Except.ok AnyType
    | Json.str s => Except.error ("not a valid type: " ++ s)
    | _ => Except.error "not a stirng"

inductive SchemaType where
  | string (s: String): SchemaType
  | number (n : Float) : SchemaType
  | boolean (b : Bool) : SchemaType
  | array (a : Array SchemaType) : SchemaType
  | object (o : List (String × SchemaType)) : SchemaType
  | null : SchemaType
  deriving BEq, Repr

def Number := Float

-- JSON Schema (Draft 3) Core Schema Definition
structure SchemaRec (recursor: Type) where
  id: Option String
  ref: Option String
  schema: Option String

  type: Except SchemaType (recursor)
  title: Option String
  description: Option String

  default: Option SchemaType
  multipleOf: Option Number
  maximum: Option Number
  exclusiveMaximum: Bool
  minimum: Option Number
  exclusiveMinimum: Bool
  maxLength: Option Nat
  minLength: Option Nat
  pattern: Option String

  additionalItems: Option (Bool × recursor)
  unqiueItems: Bool

def getFieldWithDefault (type: JsonType) (j: Json) (field: String) (default: type.asType) : type.asType :=
  let value := j.getObjVal? field
  match value with
    | Except.error e => default
    | Except.ok s =>
      match s with
        | Json.null => default
        | Json.str s =>
          match type with
            | StringType => s
            | _ => default
        | Json.num n =>
          match type with
            | NumberType => n.toFloat
            | _ => default
        | Json.bool b =>
          match type with
            | BooleanType => b
            | _ => default
        | _ => default

instance [FromJson r] : FromJson (SchemaRec r) where
  fromJson? j := do
    let id := getFieldWithDefault StringType j "id" ""
    let ref := getFieldWithDefault StringType j "$ref" ""
    let schema := getFieldWithDefault StringType j "$schema" ""
    let type : String := getFieldWithDefault StringType j "type" "any"
    Except.ok {
      id := id,
      ref := ref,
      schema := schema,
      type := Except.ok type,
      title := getFieldWithDefault StringType j "title" "",
      description := getFieldWithDefault StringType j "description" "",
      default := getFieldWithDefault type j "default" "",
      multipleOf := getFieldWithDefault NumberType j "multipleOf" 0,
      maximum := getFieldWithDefault NumberType j "maximum" 0,
      exclusiveMaximum := getFieldWithDefault BooleanType j "exclusiveMaximum" false,
      minimum := getFieldWithDefault NumberType j "minimum" 0,
      exclusiveMinimum := getFieldWithDefault BooleanType j "exclusiveMinimum" false,
      maxLength := getFieldWithDefault NumberType j "maxLength" 0,
      minLength := getFieldWithDefault NumberType j "minLength" 0,
      pattern := getFieldWithDefault StringType j "pattern" "",
      additionalItems := getFieldWithDefault (ArrayType AnyType) j "additionalItems" (false, type),
      unqiueItems := getFieldWithDefault BooleanType j "uniqueItems" false
    }

inductive Schema (ref : Type)
  | Schema (s : SchemaRec (Schema ref)) : Schema ref
