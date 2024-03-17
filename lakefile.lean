import Lake
open Lake DSL

package «json-schema» where

lean_lib «JsonSchema» where

lean_lib «Harness» where

@[default_target]
lean_exe «bowtie» where
  root := `Main
