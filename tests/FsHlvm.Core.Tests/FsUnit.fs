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

open NUnit.Framework
open NUnit.Framework.Constraints

let should (f : 'a -> #Constraint) x (y : obj) =
    let c = f x
    let y =
        match y with
        | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
        | _                 -> y
    Assert.That(y, c)

let equal x = new EqualConstraint(x)

// like "should equal", but validates same-type
let shouldEqual (x: 'a) (y: 'a) = 
    Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let shouldEqualPos (x: 'a) (y: 'a) (i: int) = 
    Assert.AreEqual(x, y, sprintf "Position: %d\n Expected: %A\nActual: %A" i x y)

let shouldEqualOptionPos (x: 'a option) (y: 'a option) (i: int) = 
    match x, y with
    | None, None -> Assert.AreEqual(y, x, sprintf "Position: %d\n Expected: %A\nActual: %A" i -1 -1)
    | Some xo, None -> Assert.AreEqual(y, x, sprintf "Position: %d\n Expected: %A\nActual: %A" i xo -1)
    | None, Some yo -> Assert.AreEqual(y, x, sprintf "Position: %d\n Expected: %A\nActual: %A" i -1 yo)
    | Some xo, Some yo -> Assert.AreEqual(y, x, sprintf "Position: %d\n Expected: %A\nActual: %A" i xo yo)

// overriding a standard language function is not cool
//let not x = new NotConstraint(x)

let contain x = new ContainsConstraint(x)

let haveLength n = Has.Length.EqualTo(n)

let haveCount n = Has.Count.EqualTo(n)

let be = id

let Null = new NullConstraint()

let Empty = new EmptyConstraint()

let EmptyString = new EmptyStringConstraint()

let NullOrEmptyString = new NullOrEmptyStringConstraint()

let True = new TrueConstraint()

let False = new FalseConstraint()

let sameAs x = new SameAsConstraint(x)

let throw = Throws.TypeOf   