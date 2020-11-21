#!/bin/bash
for i in `cat tests.txt`; do 
	dotnet test -c Release --filter \"KPTech.FsHlvm.Core.Tests.FsHlvmTest+applicationTests.$i\"; 
done

for i in `cat tests.txt`; do
        opt-3.8 -tailcallelim -O3 < bin/Release/netcoreapp2.0/"$i".bc >"$i"opt.bc
done

for i in `cat tests.txt`; do
        clang-3.8 -o "$i"opt "$i"opt.bc -ldl -lm
done

