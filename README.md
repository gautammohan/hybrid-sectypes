# hybridsys-security-typechecker

Package Description ##

This package implements security type inference to check whether or not a hybrid system obeys the property of noninterference. The hybrid systems it targets are represented as Simulink/Stateflow
models.

## Getting Started ##

To build this project, run `stack build`.

You will need to use `exportSLSFModel` on your model to produce a JSON file
readable by the type checker, and then create a corresponding
`annotations.types` file with your type annotations.

The executable currently takes **two arguments**: the json file and the type
annotation file. To run it, either use `stack runghc Main.hs <arguments>` or run the executable built by stack.

### Security Type Annotation File Format ###

Your file should contain variables paired with either "High" or "Low" separated by a colon, one per line. Here is an example:

    v1 : High
    v2 : Low
    ...etc...

## Notes ##

This project is in active development, and is highly unstable. Look forward to
more changes soon! Thanks for browsing.
