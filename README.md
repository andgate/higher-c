# Higher-C Compiler Toolchain

This is something I'm working on over Christmas break.

## Building the test program

Exectute the following commands

```bash
cabal new-run hcc
clang test.ll -o test.exe
```

## Building with Cabal New

Install cabal and then initalize then simply run

```bash
$ cabal new-build hcc
```

In addition to building, you can also run the compiler.

``` sh
# Run the test suite.
cabal new-run hcc
```


## On Windows

LLVM complicates building on windows. First, LLVM must be built and installed manually. This process can take a very, very long time.

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