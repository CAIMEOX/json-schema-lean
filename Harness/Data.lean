import Lean.Data.Json
import Lean.Data.Json.Basic

open Lean (Json)

structure Command where
  cmd : String
  deriving Lean.FromJson, Lean.ToJson

structure Implementation where
  language: String
  name: String
  homepage: String
  issues: String
  source: String
  dialects: Array String
  deriving Lean.FromJson, Lean.ToJson

structure StartResponse where
  version: Nat
  implementation: Implementation
  deriving Lean.FromJson, Lean.ToJson

structure DialectResponse where
  ok: Bool
  deriving Lean.FromJson, Lean.ToJson

structure ErrorResponse where
  error: String
  deriving Lean.FromJson, Lean.ToJson


def meta : StartResponse := {
      version := 1,
      implementation := {
        language := "lean",
        name := "jsonschema-lean",
        homepage := "https://github.com/CAIMEOX/json-schema-lean",
        issues := "https://github.com/CAIMEOX/json-schema-lean/issues",
        source := "https://github.com/CAIMEOX/json-schema-lean.git",
        dialects := #[
          "http://json-schema.org/draft-07/schema#",
          "http://json-schema.org/draft-06/schema#",
          "http://json-schema.org/draft-04/schema#"
        ]
      }
    }
