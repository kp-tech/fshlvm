// ---------------------------------------------------------------------------
// Copyright (c) 2014-2017, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------
// This file incorporates work covered by the following copyright and
// permission notice:
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

module KPTech.FsHlvm.Main.Program

open KPTech.FsHlvm.Core

open Type
open Expr
open RLType
open System.IO

/// <summary>
/// sample program
/// </summary>
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
    [ TLFunction("add", ["n", TInt; "m", TInt], TInt,
                compound[
                    Var "n" +. Var "m"]);
      
      TLFunction("List.init", ["t", TReference; "n", TInt], TReference,
                    Let("t", cons (Var "n") (Var "t"),
                        If(Var "n" =. Int 0L, Var "t",
                           Apply(Var "List.init", [Var "t"; Var "n" -. Int 1L]))))
      
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
                 
[<EntryPoint>]
let main argv = 
    printfn "Args: %A" argv
    let main (args:string array) =
        let printUsage () = 
                printfn "About: FsHlvm example program (list)"
                printfn "Usage: FsHlvm.Main options"
                printfn "Possible options:"
                printfn "--enable-debug             enable debugging (default: disabled)"
                printfn "--enable-jit               enable execute the code (default: disabled)"
                printfn "--enable-view-functions    enable view function definitions as a graph using llvm internal function viewer (default: disabled)"
                printfn "--disable-shadow-stack     disable shadow stack (default: enabled)"
                printfn "--disable-gc               disable garbage collector (default: enabled)"
                printfn "--disable-tco              disable tail call elimination (default: enabled)"

        let rec parseCommandLineRec argList = 
            match argList with 
            | [] -> ()
            | "--help"::xs -> printUsage (); exit(0);
            | "--enable-debug"::xs -> Options.Debug := true
            | "--enable-jit"::xs -> Options.CompileOnly := false
            | "--enable-view-functions"::xs -> Options.View := true
            | "--disable-shadow-stack"::xs ->
                printfn "Shadow stack and GC disabled";
                Options.ShadowStackEnabled := false;
                Options.GcEnabled := false
            | "--disable-gc"::xs ->
                printfn "GC disabled.";
                Options.GcEnabled := false
            | "--disable-stack-handler"::xs ->
                printfn "Stack handler disabled.";
                Options.StackHandler := false
            | "--disable-tco"::xs ->
                printfn "Tail call elimination disabled.";
                Options.Tco := false
            | x::xs -> 
                eprintfn "Option '%s' is unrecognized" x
                parseCommandLineRec xs in
        parseCommandLineRec (List.ofArray args)
        createEval "list.bc" (list [10L]);

    main argv

    0 // return an integer exit code
