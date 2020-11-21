## FSHlvm A High Level Virtual Machine Written In F#

FSHlvm is a cross-platform open-source virtual machine with the following features:

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

More information about FSHlvm are be available at:
http://www.kp-tech.hu/en/products/fshlvm-opensource

This work is based HLVM, written by Jon Harrop, Flying Frog Consultancy Ltd.  
More information about HLVM are available at: http://www.ffconsultancy.com/ocaml/hlvm/  

## Status

The `master` branch is for the latest version of FSHlvm.

## Build Requirements on Linux

Requires .NET Core SDK 3.1.  
Requires FSLLVM (precompiled netstandard2.0 version are available under lib/).

## Execution Requirements on Linux

Tested on Ubuntu 20.04 (amd64)  

Requires LLVM-10  
Requires CLANG-10  
Requires FSHlvm fshlvmllvmwrapper shared library  
Requires FSHlvm fshlvmruntime shared library  

### Installing LLVM-10

apt install llvm-10-dev libllvm10

### Installing CLANG-10

apt install clang-10

## How to Build

### Linux:

#### QuickStart

The following command will:
* Build the F# codebase
* Execute the FSHlvm.Cli executable (which will generate a list llvm bitcode example list.bc)
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
Linux/.NET Core: open the FSHlvm.sln project file with Visual Studio Code and build the project. This will generate the FSHlvm.Core.dll assembly for you.

#### Build fshlvmllvmwrapper and fshlvmruntime shared library

```
cd $FSHLVM_PATH/src/FSHlvm.Runtime
make
make install
```

This will copy the fshlvmllvmwrapper.so and fshlvmruntime.so to $FSHLVM_PATH/lib

### OS X

Not yet tested on OS X.

### Windows (64bit), using msbuild

Not yet tested on Windows.

## Development Notes

### Using FSHlvm in your project

In order to use FSHlvm you will want to check the following:

1. Example F# code under FSHlvm.Cli.
2. Tests F# code under FSHlvm.Core.Tests.

### Editing the Project with Visual Studio, Visual Studio Code, Xamarin Studio or MonoDevelop

Open `FSHlvm.sln`, and edit in modes Debug or Release. 

## How to Test and Validate manually

### Linux 

Cli Test and Validation

```
cd $FSHLVM_PATH/src/FSHlvm.Cli
ln -sfn ../../lib/libfshlvmllvmwrapper.so .
ln -sfn ../../lib/libfshlvmruntime.so .
dotnet restore
dotnet run -c Release
opt-10 -tailcallelim -O3 < list.bc >listopt.bc
clang-10 -o listopt listopt.bc -ldl
./listopt
```

Additional Tests Test and Validation (each test must run individually, one by one!)

```
cd $FSHLVM_PATH/tests/FSHlvm.Core.Tests
ln -sfn ../../lib/libfshlvmllvmwrapper.so .
ln -sfn ../../lib/libfshlvmruntime.so .
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
