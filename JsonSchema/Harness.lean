import JsonSchema.Data
import Lean.Data.Json

open Lean (Json)

def parseCommand (s: String) : Except String Command := do
  let j : Json <- Json.parse s
  Lean.fromJson? j

def dispatchCommand (c : Command) : String :=
  match c with
  | { cmd := "start" } =>
    let res : StartResponse := {
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
    Lean.toJson res |> Json.compress
  | { cmd := "dialect", .. } =>
    let res : DialectResponse := { ok := true }
    Lean.toJson res |> Json.compress
  | { cmd := "stop" } => "stopping"
  | { cmd := "run" } => "todo"
  | { cmd := a } =>
    let e: ErrorResponse := { error := "unknown command:" ++ a }
    Lean.toJson e |> Json.compress

def dispatch (raw: String) : String :=
  let cmd : Except String Command := parseCommand raw.trim
  match cmd with
    | Except.ok c => dispatchCommand c
    | Except.error e => e

partial def repl : IO Unit := do
  let stdin <- IO.getStdin
  let stdout <- IO.getStdout
  let h <- IO.FS.Stream.getLine stdin
  if h.trim == "" then
    pure ()
  else
    IO.FS.Stream.putStrLn stdout (dispatch h)
    IO.FS.Stream.flush stdout
    repl
