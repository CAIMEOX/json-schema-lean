import Lean.Data.Json
open Lean

def getOrDefault (j : Json) (field: String) (default: Json) : Json :=
  match j.getObjVal? field with
  | Except.ok val => val
  | _ => default

def getOrNone (j : Json) (field: String) : Option Json :=
  match j.getObjVal? field with
  | Except.ok val => some val
  | _ => none

def join (x : Option (Except String α)) : Option α :=
  match x with
  | some (Except.ok val) => some val
  | _ => none

-- Request --
structure Command where
  cmd : String
  deriving FromJson, ToJson

structure Test where
  description: Option String
  comment: Option String
  -- Whether the instance is valid or not
  valid: Option Bool
  -- The instance to validate
  instance_: Json

instance : FromJson Test where
  fromJson? j := do
    let description := join (Json.getStr? <$> getOrNone j "description")
    let comment := join (Json.getStr? <$> getOrNone j "comment")
    let valid := join (Json.getBool? <$> getOrNone j "valid")
    let instance_ <- j.getObjVal? "instance"
    pure { description, comment, valid, instance_ }

structure Case where
  -- Description of the test case
  description: Option String
  -- Comment to help understand the test case
  comment: Option String
  -- The schema to validate the instance against
  schema: Json
  -- To support $ref, the registry of schemas (uri -> schema)
  registry: Option Json
  -- The instance to validate
  tests: Array Test

instance : FromJson Case where
  fromJson? j := do
    let description := join (Json.getStr? <$> getOrNone j "description")
    let comment := join (Json.getStr? <$> getOrNone j "comment")
    let schema <- j.getObjVal? "schema"
    let registry := getOrNone j "registry"
    let tests: Array Test <- j.getObjVal? "tests" >>= Json.getArr? >>= Array.mapM fromJson?
    pure { description, comment, schema, registry, tests }

structure RunRequest where
  -- Seq ID to match the response
  seq: Nat
  case: Case
  deriving FromJson

-- Response --
structure Implementation where
  language: String
  name: String
  homepage: String
  issues: String
  source: String
  dialects: Array String
  deriving ToJson

structure StartResponse where
  version: Nat
  implementation: Implementation
  deriving ToJson

structure DialectResponse where
  ok: Bool
  deriving ToJson

structure ErrorResponse where
  error: String
  deriving ToJson

structure TestSkipped where
  seq: Nat
  skipped: Bool
  message: String
  deriving ToJson

structure TestErrored where
  seq: Nat
  errored: Bool
  message: String
  issue_url: String
  deriving ToJson

def TestSkipped.default (seq: Nat) : TestSkipped := {
  seq,
  skipped := true,
  message := "",
}

def meta : StartResponse := {
  version := 1,
  implementation := {
    name := "jsonschema-lean",
    language := "lean",
    homepage := "https://github.com/CAIMEOX/json-schema-lean",
    issues := "https://github.com/CAIMEOX/json-schema-lean/issues",
    source := "https://github.com/CAIMEOX/json-schema-lean.git",
    dialects := #[
      "http://json-schema.org/draft-07/schema#"
    ]
  }
}
