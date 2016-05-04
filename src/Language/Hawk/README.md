## Phases of the compiler
# Info caching
  This phase is used to collect root module information for later phases.
  If a source file's associated info file doesnt exist,
  or has an outdated timestamp, parse the source file
  into an translation unit, assign a unique id to each
  item, and serialize the translation unit as a json into a .hki file.

# Dependency graph
  Build a dependency graph for each module based on it's imports.
  When a source module or it's dependencies are out of date,
  add them to a queue to generate new object files.
     
# Target file generation/caching
  This is where the meat of compilation is, and is the most complex step.
  This stage generates missing/invalidated target files.
  A target file may be LLVM bytecode, C source/header, or javascript.
  For each source file,
  1. Program validation
      Ensures the program is valid, and makes prepares the ast for desugaring.
    a. Name check
      Ensure each name used is in scope.
      Error on missing names.
    b. Type check
      Infer absent types.
      Error on type mismatch. 
    c. Name Id check
      Infer missing name ids based on type and kind signatures
      Error on ambiguities resulting from names with the same type or kind.
  2. Desugarer
      Lowers the Syntax AST into a Core AST, which is an ast that is designed
     to be outputted as a C or LLVM AST.
  3. Optimization
      At this point, optimizations may be performed on the Core AST.
  4. Code Generation
      This translates the Core AST into the target ast, and then outputs the target
      file.
  5. The target file is then converted into an object file.
      
# Linking
  Once the target files have been generated and the object file cache is up-to-date,
  the compiler will link the program together using external tools provided by
  the system environment. The end result is either a library file or an executable file.
  
# Further work
  More complex builds can be coordinated using a build tool.
  A build tool may:
    - pull in external packages for .hki and library dependencies.
    - produce packages containing .hki file and libraries.
    - produce distributable exectuables, possibly for multiple platforms.

## Some extra thoughts...

# Semantic Analysis
Semantic analysis consists of two phases, which ensure the program is valid.
  1. Symbol validation
      Checks to ensure every symbol mentioned actually exists in a given scope.
      During this phase, items are collected into a lookup trie and given unique ids.
  2. Type Checking
      This phases assigns a type to every item, and attempts to unify missing types.
      This also assigns an id to names references in expressions and types.
      
# Desugarer
  Desurgaring takes a Syntax AST, and boils it down into a Core AST.
  The core AST is similar to the C language's AST, and is designed to be easy
  to handle for a code generator. This step also performs many operations such as
    - Producing type constructor functions
    - Moving function pattern matching into top level match statements
    - Expanding match statements and removing useless matches.
    - Transformation expression and type application into lists that are easy
      to generate code for.
    - Generating functions for interfaces.

# Renamer
The symbol table is used for symbol checking.
Symbol checking consists of two major phases.

# Phase One: Symbol Collection
  1. The Renamer queries the ast for
     all items (functions, variables, types) that are declared
     and stores them by path in a prefix trie.
  2. During each query, items are given a unique id and mangled with that id.
  3. Mangling will result

# Phase Two: Symbol Verification
  1. Each module is given a seperate verification pass.
  2. During a pass, the prefix trie associated with that module
     in the global symbol trie is found and converted to a suffix trie.
  3. That suffix trie is loaded into the symbol checker.
  4. The module is then scanned for imports, and pulls in the import targets.
     This process is complex, and has several different cases.
       * The import target is a function, variable, or type.
           In this case, the item is pulled into the suffix trie.
       * The import target is a module.
           Modules are complicated, because they have imports that can be public,
           which require a new set of targets to be pulled in, along with the
           public items in the module.
