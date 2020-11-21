// ---------------------------------------------------------------------------
// Copyright (c) 2014-2017, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------
// This work is based HLVM, written by Jon Harrop, Flying Frog Consultancy Ltd.
// ---------------------------------------------------------------------------
// Portions of Copyright (c) 2009, Jon Harrop, Flying Frog Consultancy Ltd.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// ---------------------------------------------------------------------------

module KPTech.FSHlvm.Core.Tests.FSHlvmTest

open System
open Xunit
open FsUnit

open Printf
open KPTech.FSHlvm.Core

open Type
open Expr
open RLType

let testEval name defs = createEval name defs

let ( |> ) x f = f x
let floatOfInt x = FloatOfInt(EFloat.Float64, x)

let nil = Construct("Nil", Unit)

let fill ty =
  [TLFunction
     ("fill", [ "a", TArray ty;
                "x", ty;
                "i", TInt ], TUnit,
      If(Var "i" <. Length(Var "a"),
         compound
           [ Set(Var "a", Var "i", Var "x");
             Apply(Var "fill", [Var "a"; Var "x"; Var "i" +. Int 1L]) ],
         Unit))]

// Note: Run only one test case at a time (!), because FSHlvm require proper inicialization, and the compiler also has internal mutable state
//opt-3.8 -tailcallelim -O3 <aout.bc >aoutopt.bc
//llc-3.8 -tailcallopt aoutopt.bc -o aoutopt.s
//g++ -g aoutopt.s -lsigsegv -ldl -lm -o aoutopt
//
//OR
//
//opt-3.8 -tailcallelim -O3 <aout.bc >aoutopt.bc
//clang -o aoutopt aoutopt.bc -ldl

type public exprTests() =

    [<Theory>]
    [<InlineData(1)>]
    member x.``exprPrintfIntN`` n =
        let exprPrintfIntN n =
               [TLExpr
                 (compound
                    [ Printf("\exprPrintfIntN: (%d)\n", [Int n]) ] )] in

        testEval "exprPrintfIntN.bc" (exprPrintfIntN n);
        shouldEqual true true;

    [<Theory>]
    [<InlineData(1,2)>]
    member x.``exprPrintfIntNM`` n m =
        let exprPrintfIntNM n m =
               [TLExpr
                 (compound
                    [ Printf("\exprPrintfIntNM: (%d) (%d)\n", [Int n; Int m]) ] )] in

        testEval "exprPrintfIntNM.bc" (exprPrintfIntNM n m);
        shouldEqual true true;


    [<Theory>]
    [<InlineData(1,0)>]
    member x.``exprPrintfFloatN`` n =
        let exprPrintfFloatN n =
               [TLExpr
                 (compound
                    [ Printf("\exprPrintfFloatN: (%f)\n", [Float64 n]) ] )] in

        testEval "exprPrintfFloatN.bc" (exprPrintfFloatN n);
        shouldEqual true true;       

    [<Theory>]
    [<InlineData(1)>]
    member x.``exprLetVarPrintfIntN`` n =
        let exprLetVarPrintfIntN n =
               [TLExpr
                 (compound                    
                    [Let("n", Int n, 
                        Printf("\exprLetVarPrintfIntN: (%d)\n", [Var "n"]))] )] in

        testEval "exprLetVarPrintfIntN.bc" (exprLetVarPrintfIntN n);
        shouldEqual true true;


    [<Theory>]
    [<InlineData(1,1)>]
    member x.``exprSumNM`` n m =
        let exprSumNM n m =
               [TLExpr
                 (compound
                    [Let("n", Int n,
                        Let("m", Int m,
                            Let("r", Var "n" +. Var "m",
                                Printf("\exprSumNM: (%d)\n", [Var("r")])))) ] )] in

        testEval "exprSumNM.bc" (exprSumNM n m);
        shouldEqual true true;

    [<Theory>]
    [<InlineData(1)>]
    member x.``exprArrayAllocNM`` n m =
        let exprArrayAllocNM n m =
               [TLExpr
                 (compound
                    [Let("n", Alloc(Int n, Int m),
                                Printf("\exprArrayAllocNM: (%d) (%d)\n", [Int n; Int m])) ] )] in

        testEval "exprArrayAllocNM.bc" (exprArrayAllocNM n m);
        shouldEqual true true;

    [<Fact>]
    member x.``funIntAddNM`` () =
        let funIntAddNM n m =
          let nVar = Var "n" in
          let mVar = Var "m" in
          [TLFunction
              ("funIntAddNM", ["n", TInt; "m", TInt], TInt,
               nVar +. mVar) ] @
          [TLExpr
             (compound
                [ Let("r", 
                    Apply(Var "funIntAddNM", [Int n; Int m]),
                        Printf("\nInteger add function: funIntAddNM (%d) (%d): %d\n", [Int n; Int m; Var "r"]))
                   ] )]

        testEval "funIntAddNM.bc" (funIntAddNM 1L 2L);
        shouldEqual true true;

    [<Fact>]
    member x.``funFloatAddNM`` () =
        let funFloatAddNM n m =
          let nVar = Var "n" in
          let mVar = Var "m" in
          [TLFunction
              ("funFloatAddNM", ["n", TFloat64; "m", TFloat64], TFloat64,
               nVar +. mVar) ] @
          [TLExpr
             (compound
                [ Let("r", 
                    Apply(Var "funFloatAddNM", [Float64 n; Float64 m]),
                        Printf("\nInteger add function: funIntAddNM (%f) (%f): %f\n", [Float64 n; Float64 m; Var "r"]))
                   ] )]

        testEval "funFloatAddNM.bc" (funFloatAddNM 1.0 2.0);
        shouldEqual true true;


type public applicationTests() =
    [<Fact>]
    member x.``boehm`` () =
        let boehm =
          let n = 1048576L in
          [ TLFunction
              ("fill", [ "a", TArray TInt;
                         "rand", TInt;
                         "i", TInt ], TUnit,
               If(Var "i" <. Int n,
                  compound
                    [ Set(Var "a", Var "i", Var "rand");
                      Apply(Var "fill",
                            [Var "a";
                             Var "rand" *. Int 1664525L +. Int 1013904223L;
                             Var "i" +. Int 1L]) ],
                  Unit));

            TLFunction("loop", [ "i", TInt ], TUnit,
                      If(Var "i" <. Int 1L, Unit,
                         Let("", Alloc(Var "i", UInt8 0uy),
                             Apply(Var "loop", [ Var "i" -. Int 1L ]))));

            TLExpr
              (compound
                 [ Let("p", Alloc(Int n, Int 0L),
                       compound
                         [ Apply(Var "fill", [Var "p"; Int 1L; Int 0L]);
                           Apply(Var "loop", [Int 32768L]) ]) ]) ] in
                         
        testEval "boehm.bc" (boehm);
        shouldEqual true true;

    [<Fact>]
    member x.``sieve`` () =

        (** Sieve of Eratosthenes. *)
        let sieve is =
          fill TBool @
            [ TLFunction
                ("last", ["a", TArray TBool; "i", TInt], TInt,
                 If(Get(Var "a", Var "i"), Var "i",
                    Apply(Var "last", [Var "a"; Var "i" -. Int 1L])));

              TLFunction
                ("loop2", ["a", TArray TBool; "i", TInt; "di", TInt], TUnit,
                 If(Var "i" >=. Length(Var "a"), Unit,
                    compound
                      [ Set(Var "a", Var "i", Bool false);
                        Apply(Var "loop2",
                              [Var "a"; Var "i" +. Var "di"; Var "di"]) ]));

              TLFunction
                ("loop1", ["a", TArray TBool; "i", TInt], TUnit,
                 If(Var "i" =. Length(Var "a"), Unit,
                    compound
                      [ If(Get(Var "a", Var "i"),
                           Apply(Var "loop2", [Var "a"; Int 2L *. Var "i"; Var "i"]),
                           Unit);
                        Apply(Var "loop1", [Var "a"; Var "i" +. Int 1L]) ])) ] @
            List.map
                (fun i ->
                   TLExpr(Let("a", Alloc(Int i, Bool false),
                             compound
                               [ Printf("\nSieve of Eratosthenes\n", []);
                                 Apply(Var "fill", [Var "a"; Bool true; Int 0L]);
                                 Apply(Var "loop1", [Var "a"; Int 2L]);
                                 Apply(Var "last",
                                       [Var "a"; Length(Var "a") -. Int 1L]) ])))
                is in
        testEval "sieve.bc" (sieve [1000L]);
        shouldEqual true true;


    [<Fact>]
    member x.``fib`` () =
        let fib ns =
          let n = Var "n" in
          [TLFunction
              ("fib", ["n", TInt], TInt,
               If(n >. Int 1L,
                  Apply(Var "fib", [n -. Int 2L]) +. Apply(Var "fib", [n -. Int 1L]),
                  n)) ] @
            List.map
                (fun n ->
                   TLExpr
                     (compound
                        [ Printf("\nInteger Fibonacci function: fib(%d)\n", [Int n]);
                          Apply(Var "fib", [Int n]) ])) 
                ns in
        testEval "fib.bc" (fib [10L]);
        shouldEqual true true;

    [<Fact>]
    member x.``ffib`` () =
        let ffib ns =
            let n = Var "n" in
            [TLFunction
                ("ffib", ["n", TFloat64], TFloat64,
                   If(n >. Float64 1.0,
                      Apply(Var "ffib", [n -. Float64 2.0]) +.
                        Apply(Var "ffib", [n -. Float64 1.0]),
                            n)) ] @
                List.map
                    (fun n ->
                       let n = Float64 n in
                           TLExpr
                             (compound
                                [ Printf("\nFloating-point Fibonacci function: fib(%f)\n", [n]);
                                  Apply(Var "ffib", [n]) ]))
                    ns in
        testEval "ffib.bc" (ffib [10.0]);
        shouldEqual true true;

    [<Fact>]
    member x.``mandelbrot`` () =
        let mandelbrot ns =
          [ TLFunction
              ("pixel", ["n", TInt;
                         "zr", TFloat64; "zi", TFloat64;
                         "cr", TFloat64; "ci", TFloat64], TUnit,
               If(Var "n" =. Int 65536L, Printf(" ", []),
                  If(Var "zr" *. Var "zr" +. Var "zi" *. Var "zi" >=. Float64 4.0,
                     Printf(".", []),
                     Apply(Var "pixel",
                           [Var "n" +. Int 1L;
                            Var "zr" *. Var "zr" -.
                              Var "zi" *. Var "zi" +. Var "cr";
                            Float64 2.0 *. Var "zr" *. Var "zi" +. Var "ci";
                            Var "cr"; Var "ci"]))));

            TLFunction
              ("row", ["i", TInt; "j", TInt; "n", TInt], TUnit,
               If(Var "i" >. Var "n", Unit,
                  compound
                    [ Apply(Var "pixel",
                            [Int 0L;
                             Float64 0.0; Float64 0.0;
                             Float64 2.0 *. floatOfInt(Var "i") /. floatOfInt(Var "n") -.
                               Float64 1.5;
                             Float64 2.0 *. floatOfInt(Var "j") /. floatOfInt(Var "n") -.
                               Float64 1.0]);
                      Apply(Var "row", [Var "i" +. Int 1L; Var "j"; Var "n"])]));

            TLFunction
              ("col", ["j", TInt; "n", TInt], TUnit,
               If(Var "j" >. Var "n", Unit,
                  compound
                    [ Apply(Var "row", [Int 0L; Var "j"; Var "n"]);
                      Printf("\n", []);
                      Apply(Var "col", [Var "j" +. Int 1L; Var "n"])])) ] @
            List.map
                (fun n ->
                   TLExpr
                     (compound
                        [ Printf("\nMandelbrot with inline complex arithmetic\n", []);
                          Apply(Var "col", [Int 0L; Int n]) ]))
                ns
        testEval "mandelbrot.bc" (mandelbrot [80L]);
        shouldEqual true true;

    [<Fact>]
    member x.``mandelbrot2`` () =
        let mandelbrot2 ns =
          let complex = TStruct[TFloat64; TFloat64] in
          let re z = GetValue(Var z, 0) in
          let im z = GetValue(Var z, 1) in
          [ TLFunction("znorm2", ["z", complex], TFloat64,
                      re "z" *. re "z" +. im "z" *. im "z");

            TLFunction("zsqr", ["z", complex], complex,
                      Struct[re "z" *. re "z" -. im "z" *. im "z";
                             Float64 2.0 *. re "z" *. im "z"]);

            TLFunction("zadd", ["z1", complex; "z2", complex], complex,
                      Struct[re "z1" +. re "z2"; im "z1" +. im "z2"]);

            TLFunction
              ("pixel", ["n", TInt; "z", complex; "c", complex], TUnit,
               If(Var "n" =. Int 65536L, Printf(" ", []),
                  If(Apply(Var "znorm2", [Var "z"]) >=. Float64 4.0,
                     Printf(".", []),
                     Apply(Var "pixel",
                           [Var "n" +. Int 1L;
                            Apply(Var "zadd", [Apply(Var "zsqr", [Var "z"]); Var "c"]);
                            Var "c"]))));

            TLFunction
              ("row", ["i", TInt; "j", TInt; "n", TInt], TUnit,
               If(Var "i" >. Var "n", Unit,
                  compound
                    [ Apply(Var "pixel",
                            [Int 0L;
                             Struct[Float64 0.0; Float64 0.0];
                             Struct[Float64 2.0 *. floatOfInt(Var "i") /.
                                      floatOfInt(Var "n") -. Float64 1.5;
                                    Float64 2.0 *. floatOfInt(Var "j") /.
                                      floatOfInt(Var "n") -. Float64 1.0]]);
                      Apply(Var "row", [Var "i" +. Int 1L; Var "j"; Var "n"])]));

            TLFunction
              ("col", ["j", TInt; "n", TInt], TUnit,
               If(Var "j" >. Var "n", Unit,
                  compound
                    [ Apply(Var "row", [Int 0L; Var "j"; Var "n"]);
                      Printf("\n", []);
                      Apply(Var "col", [Var "j" +. Int 1L; Var "n"])])) ] @
            List.map
                (fun n ->
                   TLExpr
                     (compound
                        [ Printf("\nMandelbrot with complex arithmetic functions\n", []);
                          Apply(Var "col", [Int 0L; Int n]) ]))
                ns

        testEval "mandelbrot2.bc" (mandelbrot2 [80L]);
        shouldEqual true true;

    [<Fact>]
    member x.``tco`` () =
        let tco n  =
          [ TLFunction("even", ["odd", TFunction([TInt], TInt); "n", TInt], TInt,
                      Apply(Var "odd", [Var "n" +. Int 1L]));

            TLFunction("odd", ["n", TInt], TInt,
                      If(Var "n" <. Int n,
                         Apply(Var "even", [Var "odd"; Var "n" +. Int 1L]),
                         Var "n"));

            TLExpr
              (compound
                 [ Printf("\nTesting TCO across a higher-order function\n", []);
                   Apply(Var "even", [Var "odd"; Int 0L]) ])] in

        testEval "tco.bc" (tco 1000000000L);
        shouldEqual true true;

    [<Fact>]
    member x.``trig`` () =
        let trig  =
          let triple = TStruct[TFloat64; TFloat64; TFloat64] in
          [ TLExtern("sin", [TFloat64], TFloat64);

            TLExtern("cos", [TFloat64], TFloat64);

            TLFunction("test", ["f", TFunction([TFloat64], TFloat64)], triple,
                      Struct[Apply(Var "f", [Float64 0.1]);
                             Apply(Var "f", [Float64 0.2]);
                             Apply(Var "f", [Float64 0.3])]);

            TLExpr
              (compound
                 [ Printf("\nTesting FFI\n", []);
                   Print(Apply(Var "test", [Var "sin"]));
                   Print(Apply(Var "test", [Var "cos"])) ]) ]

        testEval "trig.bc" (trig);
        shouldEqual true true;

    [<Fact>]
    member x.``fold`` () =
        let fold ns =
          let fold ty1 ty2 =
            [ TLFunction("Array.fold_aux", ["n", TInt;
                                     "f", TFunction([ty1; ty2], ty1);
                                     "y", ty1;
                                     "xs", TArray ty2], ty1,
                        If(Var "n" <. Length(Var "xs"),
                           Apply(Var "Array.fold_aux",
                                 [Var "n" +. Int 1L;
                                  Var "f";
                                  Apply(Var "f", [Var "y"; Get(Var "xs", Var "n")]);
                                  Var "xs"]),
                           Var "y"));

              TLFunction("Array.fold", ["f", TFunction([ty1; ty2], ty1);
                                       "y", ty1;
                                       "xs", TArray ty2], ty1,
                        Apply(Var "Array.fold_aux",
                              [Int 0L; Var "f"; Var "y"; Var "xs"])) ]
          in

          fold (TStruct[TFloat64; TFloat64]) TFloat64 @
            fill TFloat64 @
            [ TLFunction("f", ["x", TStruct[TFloat64; TFloat64];
                              "y", TFloat64], TStruct[TFloat64; TFloat64],
                        Struct[GetValue(Var "x", 0) +.
                                 Var "y" /. (Float64 1.0 +. GetValue(Var "x", 1));
                               GetValue(Var "x", 1) +. Float64 1.]) ] @
            List.map
                (fun n ->
                   TLExpr
                     (Let("xs", Alloc(Int n, Float64 1.0),
                          compound
                            [ Printf("\nArray.fold over %d elements\n", [Int n]);
                              Apply(Var "Array.fold",
                                    [Var "f"; Struct[Float64 0.; Float64 0.]; Var "xs"]) ])))
                ns

        testEval "fold.bc" (fold [1000L]);
        shouldEqual true true;



    [<Fact>]
    member x.``list`` () =
        let tyList ty =
          [ TLType("Cons", TStruct[ty; TReference]);
            TLType("Nil", TUnit) ]

        let nil = Construct("Nil", Unit)
        let cons h t = Construct("Cons", Struct[h; t])

        (** Pattern match over empty or non-empty list. *)
        let condList list h t kNil kCons =
          If(IsType(Var list, "Nil"), kNil,
             Let(h+t, Cast(Var list, "Cons"),
                 Let(h, GetValue(Var (h+t), 0),
                     Let(t, GetValue(Var (h+t), 1),
                         kCons))))

        (** Polymorphic List.fold_left in HLVM. *)
        let listFoldLeft a b =
          TLFunction("List.foldLeft", ["f", TFunction([a; b], a);
                                       "x", a;
                                       "list", TReference], a,
                    condList "list" "h" "t"
                      (Var "x")
                      (Apply(Var "List.foldLeft",
                             [Var "f";
                              Apply(Var "f", [Var "x"; Var "h"]);
                              Var "t"])))
        
        let list ns =
          tyList TInt @
            [ TLFunction("add", ["n", TInt; "m", TInt], TInt, Var "n" +. Var "m");
              
              TLFunction("List.init", ["t", TReference; "n", TInt], TReference,
                        Let("t", cons (Var "n") (Var "t"),
                            If(Var "n" =. Int 0L, Var "t",
                               Apply(Var "List.init", [Var "t"; Var "n" -. Int 1L]))));
              
              listFoldLeft TInt TInt;
              
              TLExpr(Apply(Var "List.init", [nil; Int 10L])) ] @
                List.map
                    (fun n ->
                       TLExpr
                         (compound
                            [ Printf("\nList.init and fold over %d elements\n", [Int n]);
                              Let("list", Apply(Var "List.init", [nil; Int n]),
                                  Apply(Var "List.foldLeft",
                                        [Var "add"; Int 0L; Var "list"])) ]))
                    ns
        testEval "list.bc" (list [1000L]);
        shouldEqual true true;
