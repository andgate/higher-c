# Hawk Compiler Toolchain

## Building with Stack

Using the LLVM-3.5 toolchain requires several system libraries:

```bash
$ apt-get install llvm-3.5
$ apt-get install libedit-dev
```

The hawk compiler, hkc, can then be built and run.

```bash
$ stack build
$ stack exec hkc
```

## Learn

Learn Hawk [here](learn).
