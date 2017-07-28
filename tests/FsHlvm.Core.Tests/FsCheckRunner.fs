// ---------------------------------------------------------------------------
// Copyright (c) 2014-2017, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------
// This module is based on FSharpx unit tests which is also licensed under 
// Apache License 2.0.
// ---------------------------------------------------------------------------

module KPTech.FsHlvm.Tests.FsCheck

open FsCheck
open NUnit.Framework

let private nUnitRunner =
    { new IRunner with
        member x.OnStartFixture t = ()
        member x.OnArguments(ntest, args, every) = ()
        member x.OnShrink(args, everyShrink) = ()
        member x.OnFinished(name, result) = 
            match result with 
            | TestResult.True data -> 
                printfn "%s" (Runner.onFinishedToString name result)
            | _ -> Assert.Fail(Runner.onFinishedToString name result) }
   
let private nUnitConfig = { Config.Default with Runner = nUnitRunner }

let fsCheck name testable =
    FsCheck.Check.One (name, nUnitConfig, testable)

module Gen = 
    let ap x = Gen.apply x
