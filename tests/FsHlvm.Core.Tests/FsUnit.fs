// ---------------------------------------------------------------------------
// Copyright (c) 2014, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------
// This module is based on FSharpx unit tests which is also licensed under 
// Apache License 2.0.
// ---------------------------------------------------------------------------

module KPTech.FsHlvm.Core.Tests.FsUnit

    open Xunit

    // like "should equal", but validates same-type
    let shouldEqual (x: 'a) (y: 'a) =
        let msg = System.String.Format("Expected: {0}\nActual: {1}", x, y)
        Assert.Equal<'a>(x, y)

    let shouldLess (x: 'a) (y: 'a) =
        let msg = System.String.Format("Expected: {0}\nActual: {1}", x, y)
        Assert.True(x < y, msg)

    let shouldLessOrEqual (x: 'a) (y: 'a) =
        let msg = System.String.Format("Expected: {0}\nActual: {1}", x, y)
        Assert.True(x <= y, msg)

    let shouldGreater (x: 'a) (y: 'a) =
        let msg = System.String.Format("Expected: {0}\nActual: {1}", x, y)
        Assert.True(x > y, msg)

    let shouldGreaterOrEqual (x: 'a) (y: 'a) =
        let msg = System.String.Format("Expected: {0}\nActual: {1}", x, y)
        Assert.True(x >= y, msg)

    let shouldEqualPos (i: int) (x: 'a) (y: 'a)  = 
        let msg = System.String.Format("Position: {0}\n Expected: {0}\nActual: {1}", i, x, y) in
        Assert.Equal<'a>(x, y)

