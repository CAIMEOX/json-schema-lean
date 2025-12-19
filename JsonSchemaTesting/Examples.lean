import JsonSchema.Validation
import JsonSchema.Resolving
import Lean

/-! # Examples for JSON Schema Validation -/

open Lean
open JsonSchema

/-! ## Minimal Example -/

-- Create a simple schema from JSON
def minimalSchema : Schema :=
  (fromJson? (Json.mkObj [("type", Json.str "string")])).toOption.get!

-- Validate data against the schema
/-- info: Except.ok () -/
#guard_msgs in
#eval validate minimalSchema (Json.str "hello")
/-- info: Except.error #["Expected one of #[string], got  42"] -/
#guard_msgs in
#eval validate minimalSchema (Json.num 42)


/-! ## Example with Resolver and $ref -/

-- Schema with definitions and references
def schemaWithRefsJson : Json := Json.mkObj [
  ("$id", Json.str "https://example.com/person.json"),
  ("definitions", Json.mkObj [
    ("address", Json.mkObj [
      ("type", Json.str "object"),
      ("properties", Json.mkObj [
        ("street", Json.mkObj [("type", Json.str "string")]),
        ("city", Json.mkObj [("type", Json.str "string")])
      ]),
      ("required", Json.arr #[Json.str "street", Json.str "city"])
    ])
  ]),
  ("type", Json.str "object"),
  ("properties", Json.mkObj [
    ("name", Json.mkObj [("type", Json.str "string")]),
    ("home", Json.mkObj [
      ("$ref", Json.str "#/definitions/address")
    ]),
    ("work", Json.mkObj [
      ("$ref", Json.str "#/definitions/address")
    ])
  ]),
  ("required", Json.arr #[Json.str "name"])
]

def schemaWithRefs : Schema :=
  (fromJson? schemaWithRefsJson).toOption.get!

-- Create a resolver and register the schema
def resolver : Resolver :=
  Resolver.addSchema {} schemaWithRefs (LeanUri.URI.encode "https" "example.com" "/person.json")

-- Valid person with addresses
def validPersonData : Json := Json.mkObj [
  ("name", Json.str "Alice"),
  ("home", Json.mkObj [
    ("street", Json.str "123 Main St"),
    ("city", Json.str "Springfield")
  ]),
  ("work", Json.mkObj [
    ("street", Json.str "456 Office Blvd"),
    ("city", Json.str "Shelbyville")
  ])
]

-- Invalid person (work address missing required city)
def invalidPersonData : Json := Json.mkObj [
  ("name", Json.str "Bob"),
  ("work", Json.mkObj [
    ("street", Json.str "789 Business Ave")
  ])
]

-- Validate using the resolver
/-- info: Except.ok () -/
#guard_msgs in
#eval validateWithResolver resolver default schemaWithRefs validPersonData

/--
info: Except.error #["Object is missing required fields: #[street, city], got  {\"street\":\"789 Business Ave\"}"]
-/
#guard_msgs in
#eval validateWithResolver resolver default schemaWithRefs invalidPersonData
