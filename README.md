# Hawk Compiler Toolchain
[![Build Status](https://travis-ci.org/andgate/hawk.svg?branch=master)](https://travis-ci.org/andgate/hawk)

## Building with Stack

Install stack and then initalize the project with

```bash
$ stack setup
$ stack build
```

Once you have a successful build, you can run the following commands.

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation. (windows may require -j1 for a successful build)
stack haddock

# All three at once, rebuilding when changes are saved
stack build --test --bench --haddock --file-watch
```

## Learn

Learn Hawk [here](http://github.com/andgate/hawk/wiki).

