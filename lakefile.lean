import Lake
open Lake DSL

package «json-schema» where
  -- add package configuration options here

lean_lib «JsonSchema» where
  -- add library configuration options here

@[default_target]
lean_exe «json-schema» where
  root := `Main
