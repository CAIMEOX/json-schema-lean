import JsonSchema.Data
import Lean.Data.Json

open Lean (Json)

def parseCommand (s: String) : Except String Command := do
  let j : Json <- Json.parse s
  Lean.fromJson? j

def dispatch2 (s: String) : Except String String := do
  let j : Json <- Json.parse s
  let cmds : Json <- (j.getObjVal? "cmd")
  let cmd : String <- cmds.getStr?
  match cmd with
    | "start" => do
      let version : Json <- j.getObjVal? "version"
      let v : Nat <- version.getNat?
      if v == 1 then
        pure "start"
      else
        throw "invalid version"
    | "dialect" => do
      pure "dialect"
    | "stop" => throw "stop"
    | _ => pure "unknown command"



def dispatchCommand (c : Command) : String := Id.run do
  let mut started : Bool := false
  let j : Json <- match c with
  | { cmd := "start" } =>
    started := true
    Lean.toJson meta
  | { cmd := "dialect", .. } =>
    let res : DialectResponse := { ok := true }
    Lean.toJson res
  | { cmd := "stop" } => "stopping"
  | { cmd := "run" } => "todo"
  | { cmd := a } =>
    let e: ErrorResponse := { error := "unknown command:" ++ a }
    Lean.toJson e
  Json.compress j

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
