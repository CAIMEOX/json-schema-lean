import JsonSchema.Validation
import JsonSchema.Schema
import Lean.Data.Json

open Lean

def compile (raw : Json) : Except String Schema := fromJson? raw
