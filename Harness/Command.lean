import Lean.Data.Json

open Lean

structure Case where
  description: String
  comment: String
  schema: Json
  registry: Json
  tests: Array Test

structure Test where
  description: String
  comment: String
  valid: Bool
  instance_: Json

instance : Lean.FromJson Test where
  fromJson? j := do
    let description <- j.getObjVal? "description" >>= Json.getStr?
    let comment ← j.getObjVal? "comment" >>= Json.getStr?
    let valid ← j.getObjVal? "valid" >>= Json.getBool?
    let instance_ ← j.getObjVal? "instance"
    pure { description, comment, valid, instance_ }

structure Command where
  cmd : String
  deriving Lean.FromJson, Lean.ToJson
