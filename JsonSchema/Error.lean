import Lean.Data.Json
open Lean

def ValidationError := Except (Array String) Unit

def reportError (messgae: String) (json : Json) : ValidationError :=
  Except.error #[s!"{messgae} {json.compress}"]

def fine : ValidationError :=
  Except.ok ()
