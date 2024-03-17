structure Case where
  description: String
  comment: String
  schema: Json
  registry: Json
  tests: List Test

structure Test where
  description: String
  comment: String
  valid: Bool
  instance_: Json
