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

## On Windows

To build on windows, llvm must be built and installed manually. This process can take a very, very long time.

First, install MSYS2. Then install the following packages.

```
pacman -Syu
pacman -S msys2-devel mingw-w64-x86_64-toolchain gcc binutils bash automake make python2 mingw-w64-x86_64-cmake
```

Next, download the llvm source and unzip it. Next, open the MSSY2 MingGW 64-bit command prompt. Navigate to the source directory. Then build llvm with the following commands.

```
mkdir build && cd build

cmake.exe .. -DCMAKE_INSTALL_PREFIX=/c/llvm -DLLVM_TARGETS_TO_BUILD="AArch64" -DBUILD_SHARED_LIBS=True -G"MSYS Makefiles"

cmake.exe --build . --target install
```

Finally, add `C:\llvm\bin` to the path.