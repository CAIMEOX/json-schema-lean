import Harness.Data
import Lean.Data.Json
open Lean (Json)

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
    | "run" =>
      let seq : Nat <- j.getObjVal? "seq" >>= Json.getNat?
      let skipped : SkippedResponse := { seq := seq, skipped := true }
      Except.ok (Lean.toJson skipped).compress
    | a => Except.error ("unknown command:" ++ a)

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
