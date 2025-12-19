namespace JsonSchema

inductive Draft where
  | V4
  | V6
  | V7
  | V2019_09
  | V2020_12

def Draft.fromString : String → Option Draft
  | "4" => some Draft.V4
  | "6" => some Draft.V6
  | "7" => some Draft.V7
  | "2019" => some Draft.V2019_09
  | "2020" => some Draft.V2020_12
  | _ => none

end JsonSchema
