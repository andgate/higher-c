# Variables

### Declaration
Variables are a core part of every programming langauge. They are an essential abstract for structuring computations. In Hawk, a variables can be declared simply with
```
$foo: Int = 1
```
This statement creates a variable named `foo`. Similar to Bash, Hawk uses the `$` to mark the start of a variable declaration. The name and type  seperated using `:`, and the initial value is assigned using the `=` operator. So, this statement declares a variable with the named `foo`, that has the type `Int`, which is initialized with the value `1`.

### Assignment
Similar to many languages, variables can be reassigned with the `=` operator.
```
$ x : Int = 1
x = 7
x = x + 1
```
This all works very much like other languages. The first line declares a variable named `x`, which is an `Int` that is initialized with the value `1`. The second line assigns `x` the value `7`, and the variable no longers represents `1`. In the third line, the expression `x + 1` is evaluated to `7 + 1`, since `x` represents `7`. Finally, `7 + 1` is evaluated to `8`, which is then assigned to `x`.

### Mutable and Immutable Variables
Most programming languages support mutable and immutable variables. Mutables variables can be modified, but immutable variables cannot be modified and will throw a compiler error if anything does attempt to modify them. In order to support this, Hawk has two symbols for creating variables. The `$` symbol is used for creating mutable variables. The `#` is used for creating immutable variables. They are both declared in the same manner.
```
$ x : Int = 1
# a : Int = 2
```
This time I inserted spaces between the symbols and the variable. You are free to do either.

Another caveat of immutable variables is that they **must** be initialized when they are declared. Mutable variables, on the other hand, will be implicitly initialized if an initialization is omitted from their declaration.
```
$ x: Int    // Implicitly Initialized to the default value for Int, which is 0.
x = 3       // Assigned to x 

# good: Int = 1   // OKAY: Immutable variable is initialized properly
# bad:  Int       // ERROR: Must be initialized
```