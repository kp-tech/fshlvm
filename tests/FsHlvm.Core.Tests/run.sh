#!/bin/bash
mono --runtime=v4.0 --jitmap ../../packages/NUnit.Runners.2.6.3/tools/nunit-console.exe bin/*/FsHlvm.Tests.dll $1
