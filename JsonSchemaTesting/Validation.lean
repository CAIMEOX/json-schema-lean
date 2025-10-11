import JsonSchema.Validation
import Lean

/-! # Tests for Schema Validation -/

open Lean

def schemaJsonString' : String := include_str "another_schema.json"

def schemaJson' : Json := (Json.parse schemaJsonString').toOption.get!

def schema' : Schema := (FromJson.fromJson? schemaJson').toOption.get!

def testString : String := include_str "another_test.json"

def test := (Json.parse testString).toOption.get!

#eval schema'.getID? default
#eval validate schema' test
