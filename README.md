## FsHlvm A High Level Virtual Machine Written In F#

FsHlvm is a cross-platform open-source virtual machine with the following features:

High-level DSL like language based on F#  
Safe  
Generics and type specialization  
Garbage collected  
High performance  
Multicore support  
Builtin FFI for C interoperability  
Commerce friendly  

The virtual machine is written in F# and uses the LLVM library for high-performance  
code generation.  

More information about FsHlvm are be available at:
http://www.kp-tech.hu/en/products/fshlvm-opensource

This work is based HLVM, written by Jon Harrop, Flying Frog Consultancy Ltd.  
More information about HLVM are available at: http://www.ffconsultancy.com/ocaml/hlvm/  

## Status

The `master` branch is for the latest version of FsHlvm.

## Build Requirements on Linux

Note: .NET Core has only initial support for local dll references, and only in the recent version of SDK:
https://github.com/dotnet/sdk/issues/120
https://github.com/dotnet/sdk/issues/1425

Requires .NET Core 2.0 preview2 (2.0.0-preview2-006497).
Requires F# 4.1 or higher.  
Requires FSLLVM (precompiled netstandard2.0 version are available under lib/).

## Execution Requirements on Linux

Tested on Ubuntu 16.04 (amd64)  

Requires LLVM-3.8  
Requires CLANG-3.8  
Requires FsHlvm fshlvmllvmwrapper shared library  
Requires FsHlvm fshlvmruntime shared library  

### Installing LLVM-3.8

apt-get install llvm-3.8-dev libllvm3.8

### Installing CLANG-3.8

apt-get install clang-3.8

## How to Build

### Linux:

#### QuickStart

The following command will:
* Build the F# codebase
* Execute the FsHlvm.Cli executable (which will generate a list llvm bitcode example list.bc)
* Optimize the list example llvm bitcode (listopt.bc)
* Create a native executable for the list example called (listopt)

```
sh run.sh
```

#### Build everything

The following command will:
* Build the F# codebase

```
sh build.sh
```

Alternative F# codebase building method:
Linux/.NET Core: open the FsHlvm.sln project file with Visual Studio Code and build the project. This will generate the FsHlvm.Core.dll assembly for you.

#### Build fshlvmllvmwrapper and fshlvmruntime shared library

```
cd $FSHLVM_PATH/src/FsHlvm.Runtime
make
make install
```

This will copy the fshlvmllvmwrapper.so and fshlvmruntime.so to $FSHLVM_PATH/lib

### OS X

Not yet tested on OS X.

### Windows (64bit), using msbuild

Not yet tested on Windows.

## Development Notes

### Using FsHlvm in your project

In order to use FsHlvm you will want to check the following:

1. Example F# code under FsHlvm.Cli.
2. Tests F# code under FsHlvm.Core.Tests.

### Editing the Project with Visual Studio, Visual Studio Code, Xamarin Studio or MonoDevelop

Open `FsHlvm.sln`, and edit in modes Debug or Release. 

## How to Test and Validate manually

### Linux 

Cli Test and Validation

```
cd $FSHLVM_PATH/src/FsHlvm.Cli
ln -sfn ../lib/libfshlvmllvmwrapper.so .
ln -sfn ../lib/libfshlvmruntime.so .
dotnet restore
dotnet run -c Release
opt-3.8 -tailcallelim -O3 < list.bc >listopt.bc
clang-3.8 -o listopt listopt.bc -ldl
./listopt
```

Additional Tests Test and Validation (each test must run individually, one by one!)

```
cd $FSHLVM_PATH/tests/FsHlvm.Core.Tests
ln -sfn ../../lib/libfshlvmllvmwrapper.so .
ln -sfn ../../lib/libfshlvmruntime.so .
ln -sfn /usr/lib/llvm-3.8/lib/libLLVM-3.8.so .
dotnet restore
./run.sh
./ffibopt
./fibopt
./foldopt
./listopt
./mandelbrot2opt
./mandelbrotopt
./tcoopt
./trigopt
```