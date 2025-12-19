import Lean.Data.Json.Basic

/-! # Implementation of JSON Pointer Fragments

https://www.rfc-editor.org/rfc/rfc6901

For the purposes of JSON schema, we can use a pointer fragment to index
a _parsed_ JSON schema as long as we are going through elements which contain
subschemas like `definitions`, `properties`, `if`.

Schema.

This has been mostly coded by Claude Code.

## JSON Pointer Syntax

A JSON Pointer is a string of reference tokens separated by `/` characters.
- Empty string `""` refers to the whole document
- Each token is prefixed by `/`
- Special characters are escaped:
  - `~0` represents `~`
  - `~1` represents `/`

## Navigation Rules

For JSON Objects: token is a member name
For JSON Arrays: token is a numeric index (or "-" for after-last-element)
-/

namespace JsonPointer

/-- Unescape a single token according to RFC 6901.
    ~1 becomes / and ~0 becomes ~ -/
def unescapeToken (token : String) : String :=
  -- First replace ~1 with /, then replace ~0 with ~
  -- Order matters! We must do ~1 first, otherwise ~0 replacement
  -- would turn "~01" into "~1" which would then become "/"
  token.replace "~1" "/" |>.replace "~0" "~"

/-- Parse a JSON Pointer string into a list of unescaped reference tokens.
    Empty string returns empty list (refers to root).
    Leading "/" is required for non-empty pointers. -/
def parse (pointer : String) : Except String (List String) :=
  if pointer == "" then
    Except.ok []
  else if pointer.front != '/' then
    Except.error s!"JSON Pointer must start with '/' or be empty, got: {pointer}"
  else
    -- Drop the leading '/', split by '/', then unescape each token
    let tokens := (pointer.drop 1).splitOn "/"
    Except.ok (tokens.map unescapeToken)

/-- Escape a token for use in a JSON Pointer.
    ~ becomes ~0 and / becomes ~1 -/
def escapeToken (token : String) : String :=
  -- Order matters! Replace ~ first (to ~0), otherwise we'd double-escape
  -- For example, "/" should become "~1", not "~0~1"
  token.replace "~" "~0" |>.replace "/" "~1"

/-- Convert a list of tokens into a JSON Pointer string -/
def toString (tokens : List String) : String :=
  match tokens with
  | [] => ""
  | _ => "/" ++ String.intercalate "/" (tokens.map escapeToken)

end JsonPointer

namespace Lean.Json

def navigate? (j : Json) (tokens : List String) : Option Json :=
  match tokens with
  | [] => some j
  | head::tokens =>
    match j with
    | .arr elems =>
      String.toNat? head >>= fun i => elems[i]? >>= (navigate? · tokens)
    | .obj pairs =>
      pairs[head]? >>= (navigate? · tokens)
    | _ => none

/-- Navigate a schema using a JSON Pointer string.
    Returns the schema at that location, or an error if the pointer is invalid
    or cannot be resolved. -/
def navigateByPointer (j : Json) (pointer : String) : Except String Json :=
  match JsonPointer.parse pointer with
  | Except.error e => Except.error e
  | Except.ok tokens =>
    match navigate? j tokens with
    | some s => Except.ok s
    | none => Except.error s!"Cannot resolve pointer '{pointer}' in schema"

end Lean.Json
