#!/bin/bash
export FSHLVM_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
sh $FSHLVM_HOME/build.sh
cd $FSHLVM_HOME
dotnet run -c Release --project $FSHLVM_HOME/src/FsHlvm.Cli/FsHlvm.Cli.fsproj
mv $FSHLVM_HOME/list.bc $FSHLVM_HOME/bin/
opt-3.8 -tailcallelim -O3 < $FSHLVM_HOME/bin/list.bc > $FSHLVM_HOME/bin/listopt.bc
clang-3.8 -o $FSHLVM_HOME/bin/listopt $FSHLVM_HOME/bin/listopt.bc -ldl
ln -sfn $FSHLVM_HOME/lib/libfshlvmllvmwrapper.so $FSHLVM_HOME/bin/
ln -sfn $FSHLVM_HOME/lib/libfshlvmruntime.so $FSHLVM_HOME/bin/
cd $FSHLVM_HOME/bin
./listopt
cd $FSHLVM_HOME
