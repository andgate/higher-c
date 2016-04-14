# Introduction to Hawk

Let's dive into Hawk! A good way to get a feel for the basic langauge is to write some code. First, let's look at what a hello world in Hawk looks like. In a file named `hello.hk`, write the following
```
HelloWorld ::
  :> System.IO.print
  
  main: Void :=
    print "Hello World"
```
This is a very simple program, but let's take it line by line. The first line defines the current module as `HelloWorld` using the `::` operator to seperate the declaration from the definition. Hawk is big on using symbols whenever it can. This may seem hard to remember at first, but you will become accustom to them.

The body of a module must begin on a new line and must begin with an indent of at least one space or tab. Hawk is a white-space sensitive language, and uses it's own flavor of the offside rule to establish codeblocks, similar to Python or Haskell. See the [whitespace layout](whitespace_layout.md) for more information.

The second line imports the `System.Print` module using the `:>` symbol, which stands for `import`. This is a system module that comes with Hawk's standard library. This is a very simple import statement, and there is a lot more to the import statement than shown here.

The third line declares the name and type of a function. The function's name is `main` and has a type `Void`. The name and type are seperated with the `:` symbol. Finally, the function declaration is ended with `:=`, which marks the end of the function delcaration and beginning of the function definition. Like in other langauges, the function name `main` has special meaning to the compiler, and is used as the entry point for an executable.

The first and only statement in this `main` function tells the computer to print `Hello World`. The statement calls the `print` function, which was import from the `System.Print` module from the second line. Function arguments in hawk are seperated by spaces, and do not require parenthesis. In this case, there is only one argument given to ``print``, which is the string `"Hello World"`.
