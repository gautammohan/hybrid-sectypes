# hybridsys-security-typechecker

This package implements security type inference for noninterference of hybrid
systems. The hybrid systems it targets are represented as Simulink/Stateflow
models.

To get started, `stack build`. You will need to use `exportSLSFModel` on your
model to produce a JSON file readable by the type checker, and then create a
corresponding `annotations.types` file with your type annotations. This file
should have variables colon-separated by either "High" or "Low" (capitalization
matters!), one on each line.

v1 : High
v2 : Low
...

These two files can be passed in to `Main.hs` via ghc-run or into the binary executable produced by stack.







