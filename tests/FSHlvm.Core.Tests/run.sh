#!/bin/bash
for i in `cat tests.txt`; do 
	dotnet test -c Release --filter \"KPTech.FSHlvm.Core.Tests.FSHlvmTest+applicationTests.$i\"; 
done

for i in `cat tests.txt`; do
        opt-10 -tailcallelim -O3 < bin/Release/netcoreapp3.1/"$i".bc >"$i"opt.bc
done

for i in `cat tests.txt`; do
        clang-10 -o "$i"opt "$i"opt.bc -ldl -lm
done

