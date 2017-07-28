#!/bin/bash
export FSHLVM_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $FSHLVM_HOME/src/FsHlvm.Runtime
make
make install
cd $FSHLVM_HOME
dotnet restore
dotnet build -c Release
