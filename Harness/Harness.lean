import Harness.Command
import Lean.Data.Json
open Lean

def runTest (j: Json) : Except String String := do
  let runReq : Except String RunRequest := fromJson? j
  match runReq with
    | Except.error e => Except.error e
    | Except.ok run => do
      let seq := run.seq
      -- Skip Test for now
      Except.ok (toJson (TestSkipped.default seq)).compress

-- Dispatches the command to the appropriate handler
def dispatch (s: String) : Except String String := do
  let j : Json <- Json.parse s
  let cmd : String <- j.getObjVal? "cmd" >>= Json.getStr?
  match cmd with
    | "start" => Except.ok (Lean.toJson meta).compress
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
