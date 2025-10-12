import Harness.Command
import JsonSchema.Compiler
import Lean.Data.Json
open Lean
open JsonSchema

def runTest (j: Json) : Except String String := do
  let runReq : Except String RunRequest := fromJson? j
  match runReq with
    | Except.error e => Except.error e
    | Except.ok run => do
      let seq := run.seq
      let tests := run.case.tests
      let schema := run.case.schema
      let schema_compiled <- compile schema
      let result: Array Valid := tests.map (fun t =>
        let valid : ValidationError := validate schema_compiled t.instance_
        match valid with
          | .ok _ => { valid := true }
          | .error _ => { valid := false }
      )
      let response: TestValidated := {
        seq := seq,
        results := result
      }
      Except.ok (toJson response).compress

-- Dispatches the command to the appropriate handler
def dispatch (s: String) : Except String String := do
  let j : Json <- Json.parse s
  let cmd : String <- j.getObjVal? "cmd" >>= Json.getStr?
  match cmd with
    | "start" => Except.ok (Lean.toJson metadata).compress
    | "dialect" =>
      let j : DialectResponse := { ok := true }
      Except.ok (Lean.toJson j).compress
    | "stop" => Except.ok "{}"
    | "run" => runTest j
    | a => Except.error ("fatal error: unknown command:" ++ a)

-- Entry point of Harness
partial def repl : IO Unit := do
  let stdin <- IO.getStdin
  let stdout <- IO.getStdout
  let stderr <- IO.getStderr
  let h <- IO.FS.Stream.getLine stdin
  if h.trim == "" then
    pure ()
  else
    match (dispatch h) with
      | Except.ok s => do
        IO.FS.Stream.putStrLn stdout s
        IO.FS.Stream.flush stdout
      | Except.error e => do
        IO.FS.Stream.putStrLn stdout e
        IO.FS.Stream.flush stderr
    repl
