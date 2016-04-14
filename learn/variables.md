# Variables

Variables are a core part of every programming langauge. They are an essential abstract for structuring computations. In Hawk, a variables can be declared simply with
```
$foo: Int = 1
```
This statement creates a variable named `foo`. Similar to Bash, Hawk uses the `$` to mark the start of a variable declaration. The name and type  seperated using `:`, and the initial value is assigned using the `=` operator. So, this statement declares a variable with the named `foo`, that has the type `Int`, which is initialized with the value `1`.

Most programming languages support mutable and immutable variables. Mutables variables can be modified, but immutable variables cannot be modified and will throw a compiler error if anything does attempt to modify them. In order to support this, Hawk has two symbols for creating variables. The `$` symbol is used for creating mutable variables. The `#` is used for creating immutable variables. They are both declared in the same manner.
```
$ x : Int = 1
# a : Int = 2
```
