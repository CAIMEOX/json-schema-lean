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
