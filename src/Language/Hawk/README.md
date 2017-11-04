### Phases of the compiler
Source text is loaded into memory as one big text document. Then this document is transformed by the compiler into various syntax trees until it is assembly.

# Lexer
Performs lexical analysis upon a given text. Dump tokens as json, yaml, or binary
Can output as pretty, json, yaml, or binary.

# Parser
Takes a list of tokens, and produces a list of definitions from the given file.
Can ouput as pretty, json, yaml, or binary.

# Name Checker
Takes list of all defined names, along with parsed expressions, and checks every name is defined.
Can output as pretty, json, yaml, or binary.

# Type Checker
Takes all the known types and an expression, and fills the expression with type information.
Can output as pretty, json, yaml, or binary.

# Kind Checker
Takes all the known kinds and a fully type expressions, and fills the expression with kind information.
Can output as pretty, json, yaml, or binary.

# Code Generator
Transforms a list of definitions to a single assembly syntax tree.
Can output as assembly. Furthermore, can assemble objects and link binary.
