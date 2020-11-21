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
module KPTech.FsHlvm.Core

open System

open System.Collections.Generic

module LLC = LLVM.Core
module LLGT = LLVM.Generated.Types
module LLGC = LLVM.Generated.Core
module LLGEE = LLVM.Generated.ExecutionEngine

/// <summary>
/// a compatibility library module for llvm-fs
/// </summary>
module Compatibility = begin
    open LLVM.FFIUtil
    open System.Runtime.InteropServices

    let private indexNotFound () =
        raise <| KeyNotFoundException "An index satisfying the predicate was not found in the collection"

    let rec assoc x l =
        match l with
        | [] -> indexNotFound()
        | ((h,r)::t) -> if x = h then r else assoc x t       

    /// <summary>
    /// create llvm constant string in a context (null terminated)
    /// </summary>
    /// <param name="ctx">llvm context</param>
    /// <param name="s">string</param>
    /// <returns>llvm valueref</returns>
    let constStringNT (c: LLGT.ContextRef) (s:string) = 
        LLGC.constStringInContext c s ((uint32)s.Length) true

    /// <summary>
    /// create llvm constant string in a context (non null terminated)
    /// </summary>
    /// <param name="ctx">llvm context</param>
    /// <param name="s">string</param>
    /// <returns>llvm valueref</returns>
    let constStringNNT (c: LLGT.ContextRef) (s:string) =
        LLGC.constStringInContext c s ((uint32)s.Length) false


    /// <summary>
    /// create llvm global veriable in a module
    /// </summary>
    /// <param name="_Name">variable name</param>
    /// <param name="_Init">initializer function</param>
    /// <param name="_Module">llvm module</param>
    /// <returns>llvm valueref</returns>
    let makeGlobalVar _Name _Init _Module = 
        let globalVar = LLGC.addGlobal _Module (LLGC.typeOf _Init) _Name in
        LLGC.setInitializer globalVar _Init
        globalVar

    /// <summary>
    /// alias for makeGlobalVar
    /// </summary>
    let defineGlobal = 
        makeGlobalVar

    /// <summary>
    /// define function in a llvm module
    /// </summary>
    /// <param name="value">function name</param>
    /// <param name="ty">function type</param>
    /// <param name="m">llvm module</param>
    /// <returns>llvm valueref</returns>
    let defineFunction (value: string) (ty: LLGT.TypeRef) (m: LLGT.ModuleRef) =
        let fn = LLGC.addFunction m value ty in
        let _ = LLGC.appendBasicBlockInContext (LLGC.getTypeContext ty) fn "entry" in
        fn

    /// <summary>
    /// declare function in a llvm module
    /// </summary>
    /// <param name="value">function name</param>
    /// <param name="ty">function type</param>
    /// <param name="m">llvm module</param>
    /// <returns>llvm valueref</returns>
    let declareFunction (value: string) (ty:LLGT.TypeRef) (m: LLGT.ModuleRef) =
        let fn = LLGC.getNamedFunction m value in
        if not (IntPtr.Zero.Equals(fn.Ptr)) then
            let fnTy = LLGC.getElementType (LLGC.typeOf fn) in
            if ty.Equals(fnTy) then 
                LLGC.constBitCast fn (LLGC.pointerType ty 0u) 
            else 
                fn
        else
            LLGC.addFunction m value ty

    /// <summary>
    /// make an undefined type
    /// </summary>
    /// <param name="ty">type</param>
    /// <returns>llvm valueref</returns>
    let makeUndef (ty:LLGT.TypeRef) =
        LLGC.getUndef ty

    /// <summary>
    /// create llvm constant string in a context (non null terminated)
    /// </summary>
    /// <param name="ctx">llvm context</param>
    /// <param name="s">string</param>
    /// <returns>llvm valueref</returns>
    let constStringN (c: LLGT.ContextRef) (s:string) =
        LLGC.constStringInContext c s ((uint32)s.Length) false

    /// <summary>
    /// verify an llvm module
    /// </summary>
    /// <param name="_M">llvm module</param>
    /// <param name="_Action">llvm action (enum constant)</param>
    /// <returns>bool: true on success false on failure</returns>
    let verifyModule _M _Action = 
        let nullString = LLVM.Core.constInt8 64y in
        LLVM.Generated.Analysis.verifyModuleNative ((_M : LLGT.ModuleRef).Ptr, (int (_Action : LLVM.Generated.Analysis.VerifierFailureAction)), (nullString: LLGT.ValueRef).Ptr)

    /// <summary>
    /// verify an llvm function
    /// </summary>
    /// <param name="_F">llvm function</param>
    /// <param name="_Action">llvm action (enum constant)</param>
    /// <returns>bool: true on success false on failure</returns>
    let verifyFunction _F _Action = 
        let nullString = LLVM.Core.constInt8 64y in
        LLVM.Generated.Analysis.verifyFunctionNative ((_F : LLGT.ValueRef).Ptr, (int (_Action : LLVM.Generated.Analysis.VerifierFailureAction)))

    /// <summary>
    /// disable llvm signal handler
    /// Note: no longer required, as of llvm 3.4 signal handlers are disabled by defaults
    /// </summary>
    /// <param name="()">unit</param>
    /// <returns>unit</returns>
    let disableSignals () =
        printfn "disabling signals"
end
open Compatibility

module Options = begin
    /// <summary>
    /// Global boolean to enable viewing of generated functions
    /// </summary>
    let View = ref false

    /// <summary>
    /// Global boolean to enable debug output in both the compiler and the generated code. Enabled by the command-line argument "--debug"
    /// </summary>
    let Debug = ref false

    /// <summary>
    /// Global integer of the current stack depth
    /// </summary>
    let Depth = ref 0

    /// <summary>
    /// Compile without evaluating
    /// </summary>
    let CompileOnly = ref true
    
    /// <summary>
    /// Allow the GC to be disabled
    /// </summary>
    let GcEnabled = ref true

    /// <summary>
    /// Allow the shadow stack to be disabled
    /// </summary>
    let ShadowStackEnabled = ref true

    /// <summary>
    /// Maximum depth of the shadow stacks, visit stack and allocated list
    /// </summary>
    let MaxDepth = int64(1 <<< 23)

    /// <summary>
    /// Tail call elimination enabled
    /// </summary>
    let Tco = ref true

    /// <summary>
    /// Number of ticks before a GC check for cooperative synchronization
    /// </summary>                
    let Ticks = 65536L

    /// <summary>
    /// Use a stack handler to catch stack overflows
    /// </summary>                
    let StackHandler = ref false

    /// <summary>
    /// The maximum number of times a recursive function will be unrolled
    /// </summary>                
    let Unroll = 8
end
    
let rec listToString f sep () = function
  | [] -> ""
  | [h] -> f () h
  | h::t -> sprintf "%a%s%a" f h sep (listToString f sep) t

type LLvalue = LLGT.ValueRef
  
module Type = begin
    /// <summary>
    /// Expressions of a first-order intermediate language.
    /// </summary>                
    type t =
        | TUnit
        | TBool
        | TInt
        | TInt8
        | TInt16
        | TInt32
        | TInt64
        | TUInt
        | TUInt8
        | TUInt16
        | TUInt32
        | TUInt64
        | TFloat32
        | TFloat64
        | TStruct of t list
        | TArray of t
        | TFunction of t list * t
        | TReference
      
    /// <summary>
    /// type equivality check (recursive)
    /// </summary>      
    /// <param name="ty1">type1</param>
    /// <param name="ty2">type1</param>
    /// <returns>true on eq false otherwise</returns>
    let rec eq (ty1: t) (ty2: t) = 
        match ty1, ty2 with
            | TStruct tys1, TStruct tys2 -> eqs tys1 tys2
            | TArray ty1, TArray ty2 -> eq ty1 ty2
            | TFunction(ty_args1, ty_ret1), TFunction(ty_args2, ty_ret2) ->
                eqs (ty_ret1::ty_args1) (ty_ret2::ty_args2)
            | ty1, ty2 -> ty1 = ty2
    and eqs tys1 tys2 = List.forall2 eq tys1 tys2                

    /// <summary>
    /// convert type to string
    /// </summary>      
    /// <param name="arg1">type</param>
    /// <returns>string representation of a type</returns>
    let rec toStringAux () : t -> string = function
        | TUnit -> "Unit"
        | TBool -> "Bool"        
        | TInt -> "Int"
        | TInt8 -> "Int8"
        | TInt16 -> "Int16"
        | TInt32 -> "Int32"
        | TInt64 -> "Int64"
        | TUInt -> "UInt"
        | TUInt8 -> "UInt8"
        | TUInt16 -> "UInt16"
        | TUInt32 -> "UInt32"
        | TUInt64 -> "UInt64"
        | TFloat32 -> "Float32"
        | TFloat64 -> "Float64"
        | TStruct tys -> sprintf "Struct[%a]" toStrings tys
        | TArray ty -> sprintf "Array(%a)" toStringAux ty
        | TFunction(tys_arg, ty_ret) ->
            sprintf "Function([%a], %a)" toStrings tys_arg toStringAux ty_ret
        | TReference -> "Reference"
    and toString () (x:t) : string =
        let result  = toStringAux () x
        result
    and toStrings tys = listToString toString "; " tys                   
end    
open Type    

module Expr = begin
    /// <summary>
    /// The maximum number of times a recursive function will be unrolled.
    /// </summary>                
    type ENeg = Neg
    type ELogic = And|Or
    type EBinArith = Add|Sub|Mul|Div|Mod
    type ECmp = Lt|Le|Eq|Ge|Gt|Ne
    type EInt = Int|Int8|Int16|Int32|Int64|UInt|UInt8|UInt16|UInt32|UInt64
    type EFloat = Float32|Float64
    
    type t =
        | Null
        // The value () of the type unit.
        | Unit
        // A literal boolean value.
        | Bool of bool
        // A literal native int value.
        | Int of int64
        // A literal int8 value.
        | Int8 of int8
        // A literal 16-bit int value.
        | Int16 of int16
        // A literal 32-bit int value.
        | Int32 of int32
        // A literal 64-bit int value.
        | Int64 of int64
        // A literal native unsigned int value.
        | UInt of uint64
        // A literal 8-bit uint value.
        | UInt8 of uint8
        // A literal 16-bit uint value.
        | UInt16 of uint16
        // A literal 32-bit uint value.
        | UInt32 of uint32
        // A literal 64-bit uint value.
        | UInt64 of uint64
        // A literal 32-bit floating point value.
        | Float32 of single
        // A literal 64-bit floating point value.
        | Float64 of double
        // A struct literal.
        | Struct of t list
        // Extract a field from a struct.
        | GetValue of t * int
        // A variable.
        | Var of string
        // A unary arithmetic operation.
        | UnArith of ENeg * t
        // A binary arithmetic operation.
        | BinArith of EBinArith * t * t
        // A comparison.
        | Cmp of ECmp * t * t
        // An "if" expression.
        | If of t * t * t
        // Evaluate the first expression, bind the resulting value to the
        // given variable name and evaluate the last expression.
        | Let of string * t * t
        // Allocate and initialize an array the given number of elements and
        // element value.
        | Alloc of t * t
        // Find the length of the given array.
        | Length of t
        // Get(a, i) gets the element at index "i" from the array "a".
        | Get of t * t
        // Set(a, i, x) sets the element at index "i" in the array "a" to
        //    "x".
        | Set of t * t * t
        // Applying the given function pointer to the given list of
        //    arguments.
        | Apply of t * t list
        // Call C printf (unsafe).
        | Printf of string * t list
        // Convert from one integer type to another.
        | IntOfInt of EInt * t
        // Convert a float to an int.
        | IntOfFloat of EInt * t
        // Convert an int to a float.
        | FloatOfInt of EFloat * t
        // Construct a boxed value.
        | Construct of string * t
        // Check the type of a value against the type with the given name.
        | IsType of t * string
        // Generic printing of any value.
        | Print of t
        // Call the C "exit" function.
        | Exit of t
        // For internal use only. Return the address of the given reference
        //    type as a native integer.
        | AddressOf of t
        // Cast the given reference value to the given type, returning the
        //    argument of the type constructor (unsafe).
        | Cast of t * string
        // For internal use only. Deallocate the given value.
        | Free of t
        // For internal use only. Load a value from an LLVM global variable.
        | Load of LLGT.ValueRef * Type.t
        // For internal use only. Store a value to an LLVM global variable.
        | Store of LLGT.ValueRef * t
        // For internal use only. Obtain the GC visit function associated with
        //    the type of the given value.
        | Visit of t
        // For internal use only. A literal LLVM value.
        | Llvalue of LLGT.ValueRef * Type.t
        // For internal use only. Convert between the two kinds of reference
        //    types: arrays and boxed values.
        | Magic of t * Type.t
        // For internal use only. Used to propagate tail calls.
        | Return of t * Type.t
        // For internal use only. Get the mark byte of a reference type.
        | GetMark of t
        // For internal use only. Set the mark byte of a reference type.
        | SetMark of t * int
        // Read the current time as a `Float.
        | Time 
        // Create and initialize a thread and apply the given function to the
        //given argument on it.
        | CreateThread of t * t
        // Join with the given thread.
        | JoinThread of t
        // Create a mutex.
        | CreateMutex
        // Lock a mutex.
        | UnsafeLockMutex of t
        // Unlock a mutex.
        | UnlockMutex of t
        // Read thread local data using POSIX thread local state.
        | ExtGetThreadLocal
        // Write thread local data using POSIX thread local state.
        | ExtSetThreadLocal of t
        // Read thread local data.
        | GetThreadLocal
        // Write thread local data.
        | SetThreadLocal of t
        // Do not inject GC-related instructions when compiling this
        // subexpression.
        | Unsafe of t
        // Read the tick counter of the current thread (used to amortize
        // expensive GC checks).
        | GetThreadTick
        // Write the tick counter of the current thread (used to amortize
        // expensive GC checks).
        | SetThreadTick of t

    // Helper operators.
    let ( <. ) f g = Cmp(Lt, f, g)
    let ( <=. ) f g = Cmp(Le, f, g)
    let ( =. ) f g = Cmp(Eq, f, g)
    let ( <>. ) f g = Cmp(Ne, f, g)
    let ( >=. ) f g = Cmp(Ge, f, g)
    let ( >. ) f g = Cmp(Gt, f, g)
    let ( ~-. ) f g = UnArith(Neg, f)
    let ( +. ) f g = BinArith(Add, f, g)
    let ( -. ) f g = BinArith(Sub, f, g)
    let ( *. ) f g = BinArith(Mul, f, g)
    let ( /. ) f g = BinArith(Div, f, g)
    let ( %. ) f g = BinArith(Mod, f, g)
    let ( &&. ) f g = If(f, g, Bool false)
    let ( ||. ) f g = If(f, Bool true, g)
    let rec compound = function
    | [] -> Unit
    | [h] -> h
    | h::t -> Let("", h, compound t)

    /// <summary>
    /// escape a string (based on FSharp.Compatibility.OCaml String.escaped function)
    /// </summary>      
    /// <param name="s">string</param>
    /// <returns>escaped string</returns>
    let escapeString (s:string) =
       let result = System.Text.RegularExpressions.Regex.Replace (s, @"[\x00\a\b\t\n\v\f\r\x1a\x22\x27\x5c\x60]", "\\$0") in
       result

    /// <summary>
    /// remove newline from a string
    /// </summary>      
    /// <param name="s">string</param>
    /// <returns>replaced string</returns>
    let removeNewLine (s:string) =
       let result = System.Text.RegularExpressions.Regex.Replace (s, @"[\r\n]", "") in
       result
    
    /// <summary>
    /// convert logic expression to string
    /// </summary>      
    /// <param name="arg1">expr</param>
    /// <returns>string representation of a expr</returns>
    let logicToString () = function
        | And -> "And"
        | Or -> "Or"

    /// <summary>
    /// convert arith expression to string
    /// </summary>      
    /// <param name="arg1">expr</param>
    /// <returns>string representation of a expr</returns>
    let arithToString () = function
        | Add -> "Add"
        | Sub -> "Sub"
        | Mul -> "Mul"
        | Div -> "Div"
        | Mod -> "Mod"

    /// <summary>
    /// convert compare expression to string
    /// </summary>      
    /// <param name="arg1">expr</param>
    /// <returns>string representation of a expr</returns>
    let cmpToString () = function
        | Lt -> "Lt"
        | Le -> "Le"
        | Eq -> "Eq" 
        | Ne -> "Ne"
        | Ge -> "Ge"
        | Gt -> "Gt"

    /// <summary>
    /// convert expression to string
    /// </summary>      
    /// <param name="arg1">expr</param>
    /// <returns>string representation of a expr</returns>
    let rec toString () = function
        | Null -> "Null"
        | Unit -> "Unit"
        | Bool b -> sprintf "Bool %b" b
        | Int n -> sprintf "Int %d" n
        | Int8 n -> sprintf "Int8 %d" n
        | Int16 n -> sprintf "Int16 %d" n
        | Int32 n -> sprintf "Int32 %d" n
        | Int64 n -> sprintf "Int64 %d" n
        | UInt n -> sprintf "UInt %d" n
        | UInt8 n -> sprintf "UInt8 %d" n
        | UInt16 n -> sprintf "UInt16 %d" n
        | UInt32 n -> sprintf "UInt32 %d" n
        | UInt64 n -> sprintf "UInt64 %d" n
        | Float32 x -> sprintf "Float32 %g" x        
        | Float64 x -> sprintf "Float64 %g" x               
        | Struct xs ->
            sprintf "Struct[%a]" (toStrings "; ") xs
        | GetValue (s, i) -> sprintf "GetValue(%a, %d)" toString s i
        | Var s -> 
            let sx = (escapeString s)
            sprintf "Var \"%s\"" sx
        | UnArith(Neg, f) ->
            sprintf "UnArith(Neg, %a)" toString f
        | BinArith(op, f, g) ->
            sprintf "BinArith(%a, %a, %a)" arithToString op toString f toString g
        | Cmp(op, t, f) ->
            sprintf "Cmp(%a, %a, %a)" cmpToString op toString t toString f           
        | If(p, t, f) ->
            sprintf "If(%a, %a, %a)" toString p toString t toString f
        | Let(x, f, g) -> 
            sprintf "Let(\"%s\", %a, %a)" (escapeString x) toString f toString g
        | Alloc(f, g) ->
            sprintf "Alloc(%a, %a)" toString f toString g
        | Length f -> sprintf "Length(%a)" toString f
        | Get(a, i) ->
            sprintf "Get(%a, %a)" toString a toString i
        | Set(a, i, x) ->
            sprintf "Set(%a, %a, %a)" toString a toString i toString x
        | Apply(f, xs) ->
            sprintf "Apply(%a, [%a])" toString f (toStrings "; ") xs            
        | Printf(s, fs) ->
            sprintf "Printf(\"%s\", [%a])" (removeNewLine(escapeString s)) (toStrings "; ") fs
        | IntOfInt(_, f) ->
            sprintf "IntOfInt(%a)" toString f
        | IntOfFloat(_, f) ->
            sprintf "IntOfFloat(%a)" toString f
        | FloatOfInt(_, f) ->
            sprintf "FloatOfInt(%a)" toString f
        | Return(f, ty) ->
            sprintf "Return(%a, %a)" toString f Type.toString ty
        | Construct(constr, f) ->
            sprintf "Construct(\"%s\", %a)" constr toString f
        | IsType (f, constr) ->
            sprintf "IsType(%a, \"%s\")" toString f constr
        | Cast (f, constr) ->
            sprintf "Cast(%a, \"%s\")" toString f constr
        | Visit f ->
            sprintf "Visit(%a)" toString f
        | Print f ->
            sprintf "Print(%a)" toString f
        | Exit f ->
            sprintf "Exit(%a)" toString f
        | Load(_, ty) ->
            sprintf "Load(<llvalue>, %a)" Type.toString ty
        | Store(_, f) ->
            sprintf "Store(<llvalue>, %a)" toString f
        | AddressOf f ->
            sprintf "AddressOf(%a)" toString f
        | Free f ->
            sprintf "Free(%a)" toString f
        | Llvalue(_, ty) ->
            sprintf "Llvalue(<llvalue>, %a)" Type.toString ty
        | Magic(f, ty) ->
            sprintf "Magic(%a, %a)" toString f Type.toString ty
        | GetMark f ->
            sprintf "GetMark(%a)" toString f
        | SetMark (f, n) ->
            sprintf "SetMark(%a, %d)" toString f n
        | Time ->
            sprintf "Time"
        | CreateThread(f, x) ->
            sprintf "CreateThread(%a, %a)" toString f toString x
        | JoinThread f ->
            sprintf "JoinThread(%a)" toString f
        | CreateMutex ->
            sprintf "CreateMutex"
        | UnsafeLockMutex f ->
            sprintf "UnsafeLockMutex(%a)" toString f
        | UnlockMutex f ->
            sprintf "UnlockMutex(%a)" toString f
        | ExtGetThreadLocal ->
            sprintf "ExtGetThreadLocal"
        | ExtSetThreadLocal f ->
            sprintf "ExtSetThreadLocal(%a)" toString f
        | GetThreadLocal ->
            sprintf "GetThreadLocal"
        | SetThreadLocal f ->
            sprintf "SetThreadLocal(%a)" toString f
        | GetThreadTick ->
            sprintf "GetThreadTick"
        | SetThreadTick f ->
            sprintf "SetThreadTick(%a)" toString f
        | Unsafe f ->
            sprintf "Unsafe(%a)" toString f
    and toStrings sep = listToString toString sep

    /// <summary>
    /// Apply the given rule to the next level of subexpressions.
    /// </summary>      
    /// <param name="arg1">expr</param>
    /// <returns>rewritten expr</returns>
    let rewrite r = function
        | Struct es -> Struct(List.map r es)
        | GetValue(e, i) -> GetValue(r e, i)
        | UnArith(op, f) -> UnArith(op, r f)
        | BinArith(op, f, g) -> BinArith(op, r f, r g)
        | Cmp(op, f, g) -> Cmp(op, r f, r g)
        | If(p, t, f) -> If(r p, r t, r f)
        | Let(x, body, rest) -> Let(x, r body, r rest)
        | Alloc(n, x) -> Alloc(r n, r x)
        | Length e -> Length(r e)
        | Get(a, i) -> Get(r a, r i)
        | Set(a, i, x) -> Set(r a, r i, r x)
        | Apply(f, xs) -> Apply(r f, List.map r xs)
        | Printf(s, xs) -> Printf(s, List.map r xs)
        | IntOfInt(ty, x) -> IntOfInt(ty, r x)
        | IntOfFloat(ty, x) -> IntOfFloat(ty, r x)
        | FloatOfInt(ty, x) -> FloatOfInt(ty, r x)
        | Construct(c, v) -> Construct(c, r v)
        | IsType(v, c) -> IsType(r v, c)
        | Print e -> Print (r e)
        | Exit x -> Exit (r x)
        | AddressOf x -> AddressOf (r x)
        | Cast(v, c) -> Cast(r v, c)
        | Free x -> Free (r x)
        | Return (x, ty) -> Return (r x, ty)
        | Visit x -> Visit(r x)
        | Magic(v, ty) -> Magic(r v, ty)
        | GetMark v -> GetMark(r v)
        | SetMark (v, n) -> SetMark (r v, n)
        | CreateThread(f, x) -> CreateThread(r f, r x)
        | JoinThread f -> JoinThread(r f)
        | UnsafeLockMutex f -> UnsafeLockMutex (r f)
        | UnlockMutex f -> UnlockMutex (r f)
        | ExtSetThreadLocal f -> ExtSetThreadLocal (r f)
        | SetThreadLocal f -> SetThreadLocal (r f)
        | SetThreadTick f -> SetThreadTick (r f)
        | Unsafe f -> Unsafe (r f)
        | Null
        | Unit
        | Bool _
        | Int _
        | Int8 _
        | Int16 _
        | Int32 _
        | Int64 _
        | UInt _
        | UInt8 _
        | UInt16 _
        | UInt32 _
        | UInt64 _
        | Float32 _
        | Float64 _
        | Var _
        | Load _
        | Store _
        | Llvalue _
        | Time
        | CreateMutex
        | ExtGetThreadLocal
        | GetThreadLocal
        | GetThreadTick as e -> e

    /// <summary>
    /// Apply f function as tail function (after r)
    /// </summary>      
    /// <param name="r">function</param>
    /// <param name="arg1">expr</param>
    /// <returns>expr</returns>
    let rec applyTail r = function
        | If(p, t, f) -> If(p, applyTail r t, applyTail r f)
        | Let(x, body, rest) -> Let(x, body, applyTail r rest)
        | f -> r f

    /// <summary>
    /// Apply f function in a count times
    /// </summary>      
    /// <param name="f">function</param>
    /// <returns>expr</returns>
    let count f =
        let n = ref 0 in
        let rec loop f =
            incr n;
            rewrite loop f in
        ignore (loop f);
        !n


    /// <summary>
    /// unroll rule for a function
    /// </summary>      
    /// <param name="f">function</param>
    /// <param name="params1">function parameters</param>
    /// <param name="body">function body</param>
    /// <returns>expr</returns>                        
    let rec unrollRule f params1 body = function
        // If this function's name is shadowed by a "let"-binding then stop
        // rewriting subexpressions.
        | Let (x, body, rest) as e when x=f ->
            e

        // Replace recursive calls with a "let"-binding of the arguments and
        // a copy of the function body.
        | Apply(Var g, args) when f=g ->
            let rec aux (i, t) param = 
                i+1, Let(param, GetValue(Var " args", i), t) in
            let _, body = List.fold aux (0, body) params1 in
            Let(" args", Struct args, body)
        | e -> rewrite (unrollRule f params1 body) e

    /// <summary>
    /// unroll a function replacing the function body
    /// </summary>      
    /// <param name="f">function</param>
    /// <param name="args">function parameters</param>
    /// <param name="body">function body</param>
    /// <param name="body1">new function body</param>
    /// <returns>expr</returns>                        
    let rec unroll f args body body1 =
        let body2 = rewrite (unrollRule f (List.map fst args) body) body1 in
        if count body2 > 4000 then body1 else body2

    /// <summary>
    /// nest a function
    /// </summary>      
    /// <param name="n">nest level</param>
    /// <param name="f">function</param>
    /// <param name="x">current nest level</param>
    /// <returns>expr</returns>                        
    let rec nest n f x =
        if n=0 then x else nest(n-1) f (f x)
    
    /// <summary>
    /// unroll a function
    /// </summary>      
    /// <param name="f">function</param>
    /// <param name="args">function parameters</param>
    /// <param name="body">function body</param>
    /// <returns>expr</returns>                        
    let unroll1 f args body =
        nest Options.Unroll (unroll f args body) body

    /// <summary>
    /// debug print function only print when Options.Debug is enabled
    /// </summary>      
    /// <param name="string">format string</param>
    /// <param name="args">format arguments</param>
    /// <returns>expr</returns>                                                
    let dPrintf(string, args) =
        if !Options.Debug then
            Printf(string, args)
        else
            Unit

    /// <summary>
    /// debug print function always enabled
    /// </summary>      
    /// <param name="string">format string</param>
    /// <param name="args">format arguments</param>
    /// <returns>expr</returns>                                                
    let d2Printf(s, xs) = Printf(s, xs)
    let d2Print _ = Unit

    /// <summary>
    /// create a lock expr
    /// </summary>      
    /// <param name="mutex">mutex to use</param>
    /// <param name="body">function body</param>
    /// <returns>expr</returns>                                                
    let lock(mutex, body) =
        Let("mutex", mutex,
            compound
                [ UnsafeLockMutex(Var "mutex");
                  Let("result", body,
                    compound
                        [   UnlockMutex(Var "mutex");
                            Var "result"])])

    /// <summary>
    /// trace rule function
    /// </summary>      
    /// <param name="fn">function</param>
    /// <returns>expr</returns>                                                
    let rec traceRule fn = function
        | If (p, t, f) -> If (p, traceRule fn t, traceRule fn f)
        | Let(x, f, g) -> Let(x, f, traceRule fn g)
        | Apply(f, gs) ->
            let rec loop i xs = function
                | [] ->
                    compound
                        [ Printf ("Tail call from " + fn + " to ", []);
                          Apply(Var "trace", List.rev xs) ]
                | g::gs ->
                    let x = sprintf "arg%d" i in
                    Let(x, g, loop(i+1) (Var x::xs) gs) in
            Let("trace", f, loop 0 [] gs)
        | f ->
            Let("trace", f, 
                compound
                    [ Printf (fn + " return ", []);
                    Print(Var "trace");
                    Printf("\n", []);
                    Var "trace" ])

                                                    
    /// <summary>
    /// Insert debug information at the beginning of each function.
    /// </summary>      
    /// <param name="f">function</param>
    /// <param name="args">function arguments</param>
    /// <param name="tyRet">return type</param>
    /// <param name="body">function body</param>
    /// <returns>expr</returns>                                                
    let rec trace(f, args, tyRet, body) =
        let body = traceRule f body in
            printfn "%s %s\n" f (toString () body);
            (f, args, tyRet,
                compound
                    [Printf(f, []);
                     Print(Struct(List.map(fun (x,_) -> Var x) args));
                     Printf("\n", []);
                     body ])
    
    exception FsHlvmException

    /// <summary>
    /// check whether the f is a leaf (no longer rewrite)
    /// </summary>      
    /// <param name="f">function</param>
    /// <returns>expr</returns>                                                
    let isLeaf f =
        let rec r = function
            | Apply _ -> raise FsHlvmException
            | f -> rewrite r f in
        try ignore(r f); true with e -> false

end            
open Expr

// Definitions related to the list type.            
module List = begin
    let rec Between sep xs =
        match xs with
        | [] -> []
        | [x] -> [x]
        | x::xs -> x::sep::Between sep xs
end
        
open LLVM.Generated.Core
open LLVM.FFIUtil

// LLVM type
type Lltype = Lltype

// LLVM context
let llContext = LLGC.getGlobalContext()

// LLVM void type
let voidType = LLGC.voidTypeInContext(llContext)

/// <summary>
/// create llvm struct type from a type
/// </summary>      
/// <param name="tya">type</param>
/// <returns>llvm type</returns>                                                
let structType tya = LLVM.Core.structTypeInContext(llContext) tya false

// LLVM bool type
let i1Type = LLGC.int1TypeInContext(llContext)

// LLVM int8 type signed
let i8Type = LLGC.int8TypeInContext(llContext)

// LLVM int16 type signed
let i16Type = LLGC.int16TypeInContext(llContext)

// LLVM int32 type signed
let i32Type = LLGC.int32TypeInContext(llContext)

// LLVM int64 type signed
let i64Type = LLGC.int64TypeInContext(llContext)

// LLVM float type single
let singleType = LLGC.floatTypeInContext(llContext)

// LLVM float type double
let doubleType = LLGC.doubleTypeInContext(llContext)

// alias for float type
let floatType = doubleType

// LLVM function type
let functionType = LLVM.Core.functionType

/// <summary>
/// create llvm pointer type from a type
/// </summary>      
/// <param name="ty">type</param>
/// <returns>llvm type</returns>                                                
let pointerType ty = LLGC.pointerType ty 0u

// Type of a C-compatible null-terminated string.
let stringType = pointerType i8Type

/// <summary>
/// Type of a native int.
/// </summary>      
/// <returns>llvm type</returns>                                                
let intType = i64Type
        
/// <summary>
/// check whether the type is a struct
/// </summary>      
/// <param name="arg1">type</param>
/// <returns>true if struct false otherwise</returns>                                                
let isStruct : Type.t -> bool = function
    | Type.TArray _ | Type.TStruct _ | Type.TReference -> true
    | Type.TUnit | Type.TBool | Type.TInt | Type.TInt8 | Type.TInt16 | Type.TInt32 | Type.TInt64 | Type.TUInt | Type.TUInt8 | Type.TUInt16 | Type.TUInt32 | Type.TUInt64 | Type.TFloat32 | Type.TFloat64 | Type.TFunction _ -> false

/// <summary>
/// check whether the type is a reference
/// </summary>      
/// <param name="arg1">type</param>
/// <returns>true if struct false otherwise</returns>                                                
let isRefType: Type.t -> bool = function
    | Type.TArray _ | Type.TReference -> true
    | Type.TStruct _ | Type.TUnit | Type.TBool | Type.TInt | Type.TInt8 | Type.TInt16 | Type.TInt32| Type.TInt64 | Type.TUInt | Type.TUInt8 | Type.TUInt16 | Type.TUInt32 | Type.TUInt64 | Type.TFloat32 | Type.TFloat64 | Type.TFunction _ -> false

// Layout of a reference type.            
module Ref = begin
    // Run-time representation of values of reference types as an LLVM type.
    let llType = 
        structType [|stringType; intType; stringType; stringType |]

    // Index of the field containing the pointer to the run-time type.
    let llTy = 0u
    
    // Index of the field containing the int metadata.
    let tag = 1u

    // Index of the field containing the pointer to allocated data.
    let data = 2u

    // Index of the field containing the pointer to the mark bit.
    let mark = 3u
end
    
/// <summary>
/// Convert a type from our type system into LLVM's type system.
/// </summary>      
/// <param name="arg1">type</param>
/// <returns>llvm type</returns>                                                
let rec llTypeOf: Type.t ->LLGT.TypeRef = function
    | Type.TUnit -> intType
    | Type.TBool -> i1Type
    | Type.TInt -> intType
    | Type.TInt8 -> i8Type
    | Type.TInt16 -> i16Type
    | Type.TInt32 -> i32Type
    | Type.TInt64 -> i64Type
    | Type.TUInt -> intType
    | Type.TUInt8 -> i8Type
    | Type.TUInt16 -> i16Type
    | Type.TUInt32 -> i32Type
    | Type.TUInt64 -> i64Type
    | Type.TFloat32 -> singleType
    | Type.TFloat64 -> doubleType
    | Type.TStruct tys -> structTypeOf tys
    | Type.TFunction (ty, rty) -> pointerType (functionTypeOf true (ty, rty))
    | Type.TArray _ | Type.TReference -> Ref.llType

/// <summary>
/// representation of function pointers
/// </summary>      
/// <param name="passtl">FIXME</param>
/// <param name="ty">type</param>
/// <param name="rty">return type</param>
/// <returns>llvm type</returns>                                                
and functionTypeOf (passtl:bool) (ty, rty) = 
    match (ty, rty) with
    | tyArgs, tyRet when isStruct tyRet ->
        let args = pointerType(llTypeOf tyRet) :: List.map llTypeOf tyArgs in
        let args = if passtl then stringType :: args else args in
        functionType (llTypeOf Type.TUnit) (Array.ofList args)
    | tyArgs, tyRet ->
        let args = List.map llTypeOf tyArgs in
        let args = if passtl then stringType :: args else args in
        functionType (llTypeOf tyRet) (Array.ofList args)

/// <summary>
/// representation of structs
/// </summary>      
/// <param name="tys">type list</param>
/// <returns>llvm type</returns>                                                
and structTypeOf tys =
    structType (Array.ofList (List.map llTypeOf tys))

// Run-time types.        
module RTType = begin
    let tyVisit = TFunction([TReference], Type.TUnit)
    let tyPrint = TFunction([TReference], Type.TUnit)
    
    let llType = 
        llTypeOf (Type.TStruct [ TFunction([TReference], Type.TUnit);
                           TFunction([TReference], Type.TUnit) ])
    
    let visit = 0
    let print = 1
end    
    
// Global LLVM module.
let M = LLGC.moduleCreateWithNameInContext "toplevel" llContext

/// <summary>
/// create a new LLVM module
/// </summary>      
/// <param name="name">module name</param>
/// <returns>llvm module</returns>                                                
let createModule name = 
    LLGC.moduleCreateWithNameInContext name llContext

/// <summary>
/// enable llvm tail call optimization
/// FIXME: not yet possible to enable because this option is moved to TargetOptions::GuaranteedTailCallOpt
/// but llvm-c api does not expose it. As a hack lib/Target/TargetMachineC.cpp -> LLVMCreateTargetMachine 
/// was modified and added opt.GuaranteedTailCallOpt=true; just before the wrapping and return.
/// </summary>      
/// <param name="()">unit</param>
/// <returns>unit</returns>                                                
let enableTailCallOpt () = 
    printfn "Enabling LLVM Tail Call Optizations (not implemented)";

enableTailCallOpt()
    
/// <summary>
/// print an llvm type
/// </summary>      
/// <param name="v">llvm value</param>
/// <param name="m">llvm module</param>
/// <returns>unit</returns>                                                
let printTypeOf v m =
    let (result:string) = LLVM.Extra.typeToString m (llTypeOf v)
    let result = result.Replace("int1", "i1") in
    let result = result.Replace("int8", "i8") in
    let result = result.Replace("int16", "i16") in
    let result = result.Replace("int32", "i32") in
    let result = result.Replace("int64", "i64") in
    printfn "%s" result;

/// <summary>
/// convert an llvm type to string
/// </summary>      
/// <param name="v">llvm value</param>
/// <param name="m">llvm module</param>
/// <returns>unit</returns>                                                
let stringTypeOf v m =
    let result = LLVM.Extra.typeToString m (LLGC.typeOf v)
    let result = result.Replace("int1", "i1") in
    let result = result.Replace("int8", "i8") in
    let result = result.Replace("int16", "i16") in
    let result = result.Replace("int32", "i32") in
    let result = result.Replace("int64", "i64") in
    result

/// <summary>
/// determine the native int size
/// FIXME: need windows 32/64 bit testing because it use different pointer sizes for long
/// and also possible to run 32bit process on 64 bit OS (WOW64)
/// </summary>      
/// <param name="v">llvm value</param>
/// <param name="m">llvm module</param>
/// <returns>unit</returns>                                                
let nativeIntType (n:int64) = LLVM.Core.constInt64 ((int64)n)

// Create an LLVM native int.
let intn n = nativeIntType n

// LLVM value used to represent the value () of the type unit.
let unitn = LLGC.getUndef intType

// Create an LLVM 8-bit int.
let int8n n = LLVM.Core.constInt8 n

// Create an LLVM 32-bit int.
let int16n n = LLVM.Core.constInt16 n

// Create an LLVM 32-bit int.
let int32n n = LLVM.Core.constInt32 n

// Create an LLVM 64-bit int.
let int64n n = LLVM.Core.constInt64 n

let float32f x = LLVM.Core.constFloat x

let float64f x = LLVM.Core.constDouble x

let nulln = LLGC.constNull stringType

// Create a default value of the given type.
let rec nullOf = function
    | Type.TUnit -> Unit
    | Type.TBool -> Bool false
    | Type.TInt -> Int 0L
    | Type.TInt8 -> Int8 0y
    | Type.TInt16 -> Int16 0s
    | Type.TInt32 -> Int32 0
    | Type.TInt64 -> Int64 0L
    | Type.TUInt -> UInt 0UL
    | Type.TUInt8 -> UInt8 0uy
    | Type.TUInt16 -> UInt16 0us
    | Type.TUInt32 -> UInt32 0u
    | Type.TUInt64 -> UInt64 0UL
    | Type.TFloat32 -> Float32 0.0f
    | Type.TFloat64 -> Float64 0.0
    | Type.TStruct tys -> Struct(List.map nullOf tys)
    | Type.TArray ty -> Alloc(Int 0L, nullOf ty)
    | Type.TFunction (_, _) as ty -> Llvalue(nulln, ty)
    | Type.TReference -> Null

exception KeyNotFoundException    

// Search for a binding and give a comprehensible error if it is not found.                                
let findByKey k kvs =
    let f = List.tryFind (fun (x,y) -> k=x) kvs
    match f with
    | Some (h, r) -> r
    | None -> 
        eprintf "Unknown %s\n" k;
        raise KeyNotFoundException

// Type used to represent stacks and unordered sequences (bags).        
module Sequence = begin
    // Type of a bag (unsorted collection).
    let ty ty = Type.TStruct[Type.TInt; Type.TArray ty]
    
    // Construct an empty sequence.
    let empty x = Struct[Int 0L; Alloc(Int Options.MaxDepth, x)]
    
    // Extract internal array from sequence.
    let arr seq = GetValue(seq, 1)
    
    // Fetch element at index in a sequence.
    let get(seq, i) = Get(arr seq, i)
    
    // Set element at index in a sequence.
    let set(seq, i, x) = Set(arr seq, i, x)
    
    // Deallocate a sequence.
    let free seq = Free(arr seq)
    
    // Number of elements in a sequence.
    let count seq = GetValue(seq, 0)
    
    let push(seq, x) =
        compound
            [ set(seq, count seq, x);
              Struct[count seq +. Int 1L; arr seq] ]
              
    let removeAt(seq, i) =
        compound
            [ set(seq, i, get(seq, count seq -. Int 1L));
              Struct[count seq -. Int 1L; arr seq] ]              
            
end    

// Run/suspend state used for individial threads and global objective to
//    cooperatively synchronize for the stop-the-world GC phase.
module ThreadState = begin
    let run, suspend = 0L, 1L
end    

// Representation of thread-local data.
module ThreadLocal = begin
    let internalTy = TStruct[TInt; TInt; TArray TInt; Sequence.ty TReference]
    
    // Type of thread-local state.
    let ty = Type.TArray internalTy

    /// <summary>
    /// generate allocation of new thread local state.
    /// </summary>      
    /// <returns>expr</returns>                                                
    let make =
        let f s b k =
            compound
                [ dPrintf(s + "\n", []);
                  Let(s, b, k) ] in
        compound
            [ dPrintf("ThreadLocal.Make\n", []);
              f "mutex" CreateMutex
                (f "state" (Alloc(Int 1L, Int ThreadState.run))
                    (f "stack" (Sequence.empty Null)
                        (f "thread_local" (Alloc(Int 1L, Struct[Int 0L; Var "mutex"; Var "state"; Var "stack"]))
                        (compound [dPrintf("ThreadLocal.make ends\n", []);
                                   Var "thread_local"])))) ]

    /// <summary>
    /// generate get the threadlocal time field
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>time</returns>                                                
    let timeOf tl = GetValue(tl, 0)

    /// <summary>
    /// generate get the threadlocal mutex field
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>mutex</returns>                                                                
    let mutexOf tl = GetValue(tl, 1)

    /// <summary>
    /// generate get the threadlocal state field
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>state</returns>                                                
    let stateOf tl = GetValue(tl, 2)

    /// <summary>
    /// get get the threadlocal stack field
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>stack</returns>                                                
    let stackOf tl = GetValue(tl, 3)                

    /// <summary>
    /// generate release the threadlocal data
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>expr</returns>                                                
    let free tl = 
        Let("tl", Get(tl, Int 0L),
            compound
                [ dPrintf("Freeing thread-local data at %p\n", [AddressOf tl]);
                  Free(stateOf(Var "tl"));
                  Sequence.free(stackOf(Var "tl"));
                  Free tl ])

    /// <summary>
    /// generate state load from threadlocal data
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <returns>state</returns>                                                                                            
    let loadState tl =
        Let("tl", Get(tl, Int 0L),
            compound [ dPrintf("%p ThreadLocal.LoadState %p\n",
                                [AddressOf GetThreadLocal; mutexOf(Var "tl")]);
                       lock(mutexOf(Var "tl"),
                            Get(stateOf(Var "tl"), Int 0L)) ])

    /// <summary>
    /// generate state store to threadlocal data
    /// </summary>      
    /// <param name="tl">ThreadLocal</param>
    /// <param name="n">FIXME: state</param>
    /// <returns>expr</returns>                                                                                            
    let storeState tl n =
        Let("tl", Get(tl, Int 0L),
            compound [ dPrintf("%p ThreadLocal.StoreState %p\n",
                                [AddressOf GetThreadLocal; mutexOf(Var "tl")]);
                       lock(mutexOf(Var "tl"),
                            Set(stateOf(Var "tl"), Int 0L, Int n)) ])

    /// <summary>
    /// generate eq for two threadlocal
    /// </summary>      
    /// <param name="x">ThreadLocal</param>
    /// <param name="y">ThreadLocal</param>
    /// <returns>expr</returns>                                                                                                                                     
    let eq x y = AddressOf x =. AddressOf y
    
end
    
        
// Global thread data including the thread-local data of every mutator
// thread.
module ThreadGlobal = begin    

    /// <summary>
    /// generate threadglobal mutex
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                     
    let mutex = makeGlobalVar "threadglobal_mutex" (intn 0L) M

    // type of threadglobal
    let ty = Sequence.ty ThreadLocal.ty

    /// <summary>
    /// generate threadglobal list (each thread will be stored here)
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                     
    let list = makeGlobalVar "threadglobal_list" (LLGC.constNull(llTypeOf ty)) M

    /// <summary>
    /// generate threadglobal lock (to prevent concurrent modification)
    /// </summary>      
    /// <param name="f">function to apply</param>
    /// <returns>expr</returns>                                                                                                                                                     
    let lock (f: Expr.t) =
        compound [ dPrintf("%p ThreadGlobal.Lock %p\n",
                            [AddressOf GetThreadLocal; Load(mutex, Type.TInt)]);
                   lock(Load(mutex, Type.TInt), f) ]

    /// <summary>
    /// generate threadglobal load list
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                   
    let loadList = Load(list, ty)

    /// <summary>
    /// generate threadglobal store list
    /// </summary>      
    /// <param name="xs">thread list to store</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let storeList xs = Store(list, xs)

    /// <summary>
    /// generate threadglobal state
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let state = makeGlobalVar "threadglobal_state" (intn 0L) M

    /// <summary>
    /// generate threadglobal state load
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let loadState = Load(state, TInt)

    /// <summary>
    /// generate threadglobal state store
    /// </summary>      
    /// <param name="n">state to store</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let storeState n = Store(state, Int n)

    /// <summary>
    /// generate threadglobal init
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let init =
        compound [ dPrintf("Creating global thread mutex...\n", []);
                   Store(mutex, CreateMutex) ]                   
end

/// <summary>
/// generate blocking section enter expr
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let enterBlockingSection() =
    if !Options.GcEnabled then
        Unsafe
            (compound
                [ dPrintf("%p entering blocking section with %d roots\n",
                            [AddressOf GetThreadLocal;
                             Sequence.count(ThreadLocal.stackOf
                                        (Get(GetThreadLocal, Int 0L)))])   
                  ThreadGlobal.lock
                    (ThreadLocal.storeState GetThreadLocal ThreadState.suspend) ])
    else
        Unit                    

/// <summary>
/// generate blocking section leave expr
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let leaveBlockingSection() =
    if !Options.GcEnabled then
        Apply(Var "spin", [])
    else
        Unit

/// <summary>
/// generate lock mutex expr
/// </summary>      
/// <param name="f">mutex to use</param>
/// <returns>expr</returns>                                                                                                                                                                                                                   
let lockMutex f =
    Let("mutex", f,
        compound [ enterBlockingSection();
                   UnsafeLockMutex(Var "mutex");
                   leaveBlockingSection() ])

/// <summary>
/// The visit stack is an array of unvisited reference types.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let visitStack =
    makeGlobalVar "visit_stack" (LLGC.constNull(llTypeOf (Type.TArray Type.TReference))) M
           
/// <summary>
/// Number of unvisited references on the visit stack.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let nVisit = makeGlobalVar "n_visit" (intn 0L) M

/// <summary>
/// The allocated list is an array of reference types.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let allocatedMutex = makeGlobalVar "allocated_mutex" (intn 0L) M

/// <summary>
/// Allocated references.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let allocated =                 
    makeGlobalVar "allocated" (LLGC.constNull(llTypeOf (Type.TArray Type.TReference))) M

/// <summary>
/// Number of allocated references.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let nAllocated = 
    makeGlobalVar "n_allocated" (intn 0L) M

/// <summary>
/// Number of allocations required to incur a garbage collection.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let quota =
    makeGlobalVar "quota" (intn 0L) M
        
module Extern = begin
    /// <summary>
    /// generate LLVM declaration of C's putchar function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let putchar = LLGC.addFunction M "putchar" (functionType intType [|intType|])
        
    /// <summary>
    /// generate LLVM declaration of C's exit function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let exit = LLGC.addFunction M "exit" (functionType voidType [|i32Type|])

    /// <summary>
    /// generate LLVM declaration of C's printf function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let printf = LLGC.addFunction M "printf" (LLC.varArgFunctionType intType [|stringType|])

    /// <summary>
    /// generate LLVM declaration of libdl's dlopen function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let dlopen = LLGC.addFunction M "dlopen" (functionType stringType [|stringType; intType|])

    /// <summary>
    /// generate LLVM declaration of libdl's dlerror function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let dlerror = LLGC.addFunction M "dlerror" (functionType stringType [||])

    /// <summary>
    /// generate LLVM declaration of libdl's dlsym function.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let dlsym = LLGC.addFunction M "dlsym" (functionType stringType [|stringType; stringType|])

    /// <summary>
    /// dl function placeholder object to store the type and the pointer
    /// </summary>      
    /// <returns>obj</returns>                                                                                                                                                                                                                   
    type DlfnObject(_ty, _ptr) = class
        member x.ty = _ty
        member x.ptr = _ptr
    end

    /// <summary>
    /// create dl function placeholder object from definition
    /// </summary>      
    /// <param name="f">function name</param>
    /// <param name="tyRet">function return type</param>
    /// <param name="tyArgs">function args type list</param>
    /// <returns>obj</returns>                                                                                                                                                                                                                                             
    let dlfn (f:string) (tyRet:LLGT.TypeRef) (tyArgs:LLGT.TypeRef list) =
        let ty = pointerType(functionType tyRet (Array.ofList tyArgs)) in
        let ptr = makeGlobalVar f (LLGC.constNull ty) M in
        new DlfnObject(ty, ptr)
    
    /// <summary>
    /// generate ffi for alloc function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let alloc = dlfn "fshlvm_alloc" stringType [intType; intType]    

    /// <summary>
    /// generate ffi for free function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                                   
    let free = dlfn "fshlvm_free" voidType [stringType]

    /// <summary>
    /// generate ffi for time function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let time = dlfn "fshlvm_time" floatType []

    /// <summary>
    /// generate ffi for init function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let init = dlfn "fshlvm_init" voidType []

    /// <summary>
    /// generate ffi for create_thread function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let createThread = 
        dlfn "fshlvm_create_thread" stringType [stringType; intType]

    /// <summary>
    /// generate ffi for join_thread function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let joinThread =
        dlfn "fshlvm_join_thread" voidType [stringType]

    /// <summary>
    /// generate ffi for create_mutex function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let createMutex = dlfn "fshlvm_create_mutex" intType []

    /// <summary>
    /// generate ffi for lock_mutex function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let lockMutex = dlfn "fshlvm_lock_mutex" voidType [intType]

    /// <summary>
    /// generate ffi for unlock_mutex function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let unlockMutex = dlfn "fshlvm_unlock_mutex" voidType [intType]

    /// <summary>
    /// generate ffi for get_thread_local function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let getThreadLocal = dlfn "fshlvm_get_thread_local" intType []

    /// <summary>
    /// generate ffi for set_thread_local function
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    let setThreadLocal = dlfn "fshlvm_set_thread_local" voidType [intType]

    /// <summary>
    /// generate all ffi
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                                   
    let loadFns loadFn =
        loadFn alloc "fshlvm_alloc";
        loadFn free "fshlvm_free";
        loadFn time "fshlvm_time";
        loadFn init "fshlvm_init";
        loadFn createThread "fshlvm_create_thread";
        loadFn joinThread "fshlvm_join_thread";
        loadFn createMutex "fshlvm_create_mutex";
        loadFn lockMutex "fshlvm_lock_mutex";
        loadFn unlockMutex "fshlvm_unlock_mutex";
        loadFn getThreadLocal "fshlvm_get_thread_local";
        loadFn setThreadLocal "fshlvm_set_thread_local";

end

/// <summary>
/// LLVM global to store the total time spent in the suspend phase.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let suspendTime = makeGlobalVar "suspend_time" (float64f 0.0) M

/// <summary>
/// LLVM global to store the total time spent in the mark phase.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let markTime = makeGlobalVar "mark_time" (float64f 0.0) M

/// <summary>
/// LLVM global to store the total time spent in the sweep phase.
/// </summary>      
/// <returns>expr</returns>                                                                                                                                                                                                                   
let sweepTime = makeGlobalVar "sweep_time" (float64f 0.0) M

/// <summary>
/// Default calling convention used by FsHlvm.
/// </summary>      
/// <returns>cc</returns>                                                                                                                                                                                                                   
let cc = LLGC.CallConv.CCallConv

/// <summary>
/// Mapping from bound variable names to their LLVM values and FsHlvm types.
/// </summary>      
/// <returns>list</returns>                                                                                                                                                                                                                   
type vars = 
    { vals: (string * (LLvalue * Type.t)) list }

/// <summary>
/// Default variable bindings.
/// </summary>      
/// <returns>variable list</returns>                                                                                                                                                                                                                   
let vars = { vals = [] }

/// <summary>
/// create new variable bindings.
/// </summary>      
/// <returns>variable list</returns>                                                                                                                                                                                                                   
let createVars () = 
    { vals = [] }

open Microsoft.FSharp.Collections

/// <summary>
/// Bound types (including internal types such as wrapper reference types for arrays).
/// </summary>      
/// <returns>type map</returns>                                                                                                                                                                                                                   
let types = new KPTech.FsHlvm.Collections.HashMultiMap<_, _>(13, HashIdentity.Structural)

/// <summary>
/// Container of internal functions such as visitors to traverse the heap.
/// </summary>      
/// <returns>function map</returns>                                                                                                                                                                                                                   
let functions = new KPTech.FsHlvm.Collections.HashMultiMap<_, _>(13, HashIdentity.Structural)

exception FsHlvmException

/// <summary>
/// find a type in the global type map
/// </summary>      
/// <param name="name">type name</param>
/// <returns>expr</returns>                                                                                                                                                                                                                   
let findType (name:string) =
    match types.TryFind name with
    | Some (lt, t) -> (lt, t)
    | None -> raise FsHlvmException

/// <summary>
/// add a variable the the global variable list
/// </summary>      
/// <param name="f">mutex to use</param>
/// <returns>expr</returns>                                                                                                                                                                                                                   
let addVal x vars = { vals = x :: vars.vals }

/// <summary>
/// Thread local types
/// </summary>      
type ThreadLocalType =
    | CustomType of LLGT.ValueRef
    | ExternalType
    | InternalType   

/// <summary>
/// Create a state record that encapsulates all state related data
/// </summary>      
/// <param name="PassTl">FIXME</param>
/// <param name="Func">llvm function</param>
/// <param name="Blk">llvm basic block</param>
/// <param name="Odepth">stack depth</param>
/// <param name="GcEnabled">whether the gc enabled</param>
/// <param name="Roots">gc roots</param>
/// <param name="ThreadLocal">thread local storage</param>
/// <returns>state record</returns>                                                                                                                                                                                                                   
type StateRecord = { PassTl: bool; Func: LLGT.ValueRef; Blk: LLGT.BasicBlockRef; Odepth: Lazy<LLGT.ValueRef>; GcEnabled: bool; Roots: bool; ThreadLocal: ThreadLocalType }

/// <summary>
/// Create a state record that encapsulates all data our interface for emitting LLVMi nstructions.
/// </summary>      
/// <param name="passTl">FIXME</param>
/// <param name="func">llvm function</param>
/// <returns>state record</returns>                                                                                                                                                                                                                   
type State private (state: StateRecord) = class
    new (passTl: bool, func: LLGT.ValueRef) =
        State({ PassTl = passTl; 
                 Func = func; 
                 Blk = LLGC.getEntryBasicBlock func; 
                 Odepth = lazy(intn 0L); 
                 GcEnabled = true; 
                 Roots = false; 
                 ThreadLocal = if passTl then InternalType else ExternalType })

    /// <summary>
    /// create new state with new passTl
    /// </summary>      
    /// <param name="p">passTl</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithPassTl p =
        State({ state with PassTl = p })

    /// <summary>
    /// create new state with new func
    /// </summary>      
    /// <param name="f">func</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithFunc f =
        State({ state with Func = f })
    
    /// <summary>
    /// create new state with new basic block
    /// </summary>      
    /// <param name="b">basic block</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithBlk b =
        State({ state with Blk = b })

    /// <summary>
    /// create new state with new stack depth
    /// </summary>      
    /// <param name="p">depth</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithOdepth o =
        State({ state with Odepth = o })

    /// <summary>
    /// create new state with new gcEnabled
    /// </summary>      
    /// <param name="g">gcEnabled</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithGcEnabled g =
        State({ state with GcEnabled = g })

    /// <summary>
    /// create new state with new roots
    /// </summary>      
    /// <param name="r">roots</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.WithRoots r =
        State({ state with Roots = r })

    /// <summary>
    /// create new state with new threadlocal
    /// </summary>      
    /// <param name="t">threadlocal</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.withThreadLocal t =
        State({ state with ThreadLocal = t })

    /// <summary>
    /// get state passtl
    /// </summary>      
    /// <returns>passtl</returns>                                                                                                                                                                                                                                       
    member x.GetPassTl = 
        state.PassTl

    /// <summary>
    /// get state func
    /// </summary>      
    /// <returns>func</returns>                                                                                                                                                                                                                                       
    member x.GetFunc =
        state.Func

    /// <summary>
    /// get state basic block
    /// </summary>      
    /// <returns>basic block</returns>                                                                                                                                                                                                                                       
    member x.GetBlk =
        state.Blk
    
    /// <summary>
    /// get state depth
    /// </summary>      
    /// <returns>depth</returns>                                                                                                                                                                                                                                       
    member x.GetOdepth =
        state.Odepth

    /// <summary>
    /// get state gcenabled
    /// </summary>      
    /// <returns>gcenabled</returns>                                                                                                                                                                                                                                       
    member x.GetGcEnabled =
        state.GcEnabled

    /// <summary>
    /// get state roots
    /// </summary>      
    /// <returns>roots</returns>                                                                                                                                                                                                                                       
    member x.GetRoots =
        state.Roots

    /// <summary>
    /// get state threadlocal
    /// </summary>      
    /// <returns>threadlocal</returns>                                                                                                                                                                                                                                       
    member x.GetThreadLocal =
        state.ThreadLocal

    /// <summary>
    /// debug printout only available when debug is enabled
    /// </summary>      
    /// <param name="format">format string</param>
    /// <param name="args">format args</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.Printf format args = 
        d2Printf(format, args)

    /// <summary>
    /// generate extract value expr
    /// </summary>      
    /// <param name="state">state obj</param>
    /// <param name="s">array</param>
    /// <param name="i">index</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    static member ExtractValue (state:State) s i =
        LLGC.buildExtractValue state.Bb s i ""

    /// <summary>
    /// generate stack push value expr
    /// </summary>      
    /// <param name="self">state obj</param>
    /// <param name="stack">stack</param>
    /// <param name="depth">stack depth</param>
    /// <param name="v">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    static member Push (self:State) stack depth v =
        if !Options.ShadowStackEnabled then begin
            if !Options.Debug then
                printfn "State.Push";
            let d = self.Load depth [intn 0L] in
            let data = State.ExtractValue self (self.Load stack [intn 0L]) Ref.data in
            let data = self.Bitcast data (pointerType(LLGC.typeOf v)) in
            self.Store data [d] v;
            self.Store depth [intn 0L] (buildAdd self.Bb (intn 1L) d "")
        end
            
    /// <summary>
    /// generate get pointers to stack depth and stack
    /// </summary>      
    /// <param name="state">state obj</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    static member getStack (state:State) =
        assert(state.GetGcEnabled);
        let data = 
            state.Bitcast state.ThreadLocal
                (pointerType (llTypeOf ThreadLocal.internalTy)) in
        state.Gep data [intn 0L; int32n 3; int32n 0],
        state.Gep data [intn 0L; int32n 3; int32n 1]

    /// <summary>
    /// generate an LLVM builder block instruction.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Bb =
        let builder = LLGC.createBuilderInContext llContext in
        let _ = LLGC.positionBuilderAtEnd builder x.GetBlk
        builder

    /// <summary>
    /// generate an LLVM get element pointer instruction.
    /// </summary>      
    /// <param name="a">a</param>
    /// <param name="ns">ns</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Gep a ns = LLVM.Core.buildGEP x.Bb a (Array.ofList ns) "" 
    
    /// <summary>
    /// generate an LLVM load instruction.
    /// </summary>      
    /// <param name="a">a</param>
    /// <param name="ns">ns</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Load a ns = LLGC.buildLoad x.Bb (x.Gep a ns) ""
    
    /// <summary>
    /// generate an LLVM store instruction.
    /// </summary>      
    /// <param name="a">a</param>
    /// <param name="ns">ns</param>
    /// <param name="y">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Store (a: LLGT.ValueRef) (ns:LLGT.ValueRef list) (y:LLGT.ValueRef) =      
        ignore (LLGC.buildStore x.Bb y (x.Gep a ns))
    
    /// <summary>
    /// generate LLVM instructions to call the fshlvm_alloc function.
    /// </summary>      
    /// <param name="llTy">type</param>
    /// <param name="n">number of items</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Malloc llTy n =
        let size = LLGC.buildTrunc x.Bb (LLGC.sizeOf llTy) intType "" in
        let llalloc = x.Load Extern.alloc.ptr [intn 0L] in
        let data = LLVM.Core.buildCall x.Bb llalloc [|n; size|] "" in
        x.Bitcast data (pointerType llTy)
    
    /// <summary>
    /// generate LLVM instructions to call the fshlvm_free function.
    /// </summary>      
    /// <param name="y">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Free y =
        let llFree = x.Load Extern.free.ptr [intn 0L] in
        ignore(LLVM.Core.buildCall x.Bb llFree [|y|] "")

    /// <summary>
    /// generate define a global LLVM variable.
    /// </summary>      
    /// <param name="y">type</param>
    /// <param name="v">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.DefineGlobal y v = makeGlobalVar y v M
    
    /// <summary>
    /// Create a new instruction block and return a new state that will insert instructions into it.
    /// </summary>      
    /// <param name="s">block name</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Mk s = 
        x.WithBlk (LLGC.appendBasicBlockInContext llContext x.GetFunc s)
    
    /// <summary>
    /// generate an LLVM return instruction.
    /// </summary>      
    /// <param name="v">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Ret (v:LLGT.ValueRef) = ignore (LLGC.buildRet x.Bb v)

    /// <summary>
    /// generate an LLVM branch instruction.
    /// </summary>      
    /// <param name="s">state</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Br (s:State) = ignore(LLGC.buildBr x.Bb s.GetBlk)   

    /// <summary>
    /// generate an LLVM bitcast instruction.
    /// </summary>      
    /// <param name="v">value</param>
    /// <param name="ty">type to cast</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Bitcast v ty = LLGC.buildBitCast x.Bb v ty ""

    /// <summary>
    /// generate an LLVM call instruction using the given calling convention.
    /// </summary>      
    /// <param name="cc">calling convention</param>
    /// <param name="f">function</param>
    /// <param name="args">function args</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Call cc f args =
        if !Options.Debug then 
            begin
                    printf "call ";
                    List.iter (fun arg -> printf "%s " (LLVM.Extra.typeToString M (LLGC.typeOf arg)))
                        args;
                    printf "\n"
            end;
        let call = LLVM.Core.buildCall x.Bb f (Array.ofList args) "" in
        LLGC.setInstructionCallConv call cc;
        call
        
    /// <summary>
    /// Get the LLVM value of the pointer to the return struct.
    /// </summary>      
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Sret = LLGC.getParam x.GetFunc (if x.GetPassTl then 1u else 0u)

    /// <summary>
    /// generate an LLVM alloca instruction to allocate on the stack.
    /// FIXME: builder_at_end ?
    /// </summary>      
    /// <param name="tye">type</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.Alloca ty =
        let builder = LLGC.createBuilderInContext llContext in
        let _ = LLGC.positionBuilderAtEnd builder (LLGC.getEntryBasicBlock x.GetFunc) in
        LLGC.buildAlloca builder ty ""
    
    /// <summary>
    /// generate new gcroot
    /// </summary>      
    /// <param name="v">value</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.GcRoot v = 
        if x.GetGcEnabled && !Options.ShadowStackEnabled then
            let stackDepth, stack = State.getStack x in
            State.Push x stack stackDepth v;
            x.WithRoots true
        else
            x

    /// <summary>
    /// generate gc restore
    /// </summary>      
    /// <param name="()">unit</param>
    /// <returns>expr</returns>                                                                                                                                                                                                                   
    member x.GcRestore() =
        if x.GetGcEnabled && x.GetRoots then
            if !Options.ShadowStackEnabled then
                begin
                    if !Options.Debug then
                        printfn "State.Restore\n";
                    let stackDepth, _ = State.getStack x in
                        x.Store stackDepth [intn 0L] x.Odepth
                end
    
    /// <summary>
    /// set gc to new value
    /// </summary>      
    /// <param name="enabled">enabled</param>
    /// <returns>state record</returns>                                                                                                                                                                                                                   
    member x.SetGc (enabled:bool) : State = 
        x.WithGcEnabled enabled
    
    /// <summary>
    /// Depth the shadow stack was at when this function was entered.
    /// </summary>      
    /// <returns>depth</returns>                                                                                                                                                                                                                   
    member x.Odepth = x.GetOdepth.Force()

    /// <summary>
    /// Prepare to reset the shadow stack depth to this value.
    /// </summary>     
    /// <param name="d">new depth</param> 
    /// <returns>state</returns>                                                                                                                                                                                                                   
    member x.SetDepth d = 
        x.WithOdepth d

    /// <summary>
    /// Record the thread-local data.
    /// </summary>      
    /// <param name="threadLocal">new threadlocal</param> 
    /// <returns>state</returns>                                                                                                                                                                                                                   
    member x.SetThreadLocal threadLocal = 
        x.withThreadLocal (CustomType threadLocal)

    /// <summary>
    /// Get the thread local data.
    /// </summary>      
    /// <returns>threadlocal</returns>                                                                                                                                                                                                                   
    member x.ThreadLocal =
        match x.GetThreadLocal with
        | CustomType tl -> tl
        | InternalType _ -> (LLGC.getParam x.GetFunc 0u)
        | ExternalType _ ->
            let getThreadLocal =
                x.Load Extern.getThreadLocal.ptr [intn 0L] in
            let ptr = x.Call ((uint32)(CallConv.CCallConv)) getThreadLocal [] in
            x.PtrOfInt ptr stringType

    /// <summary>
    /// generate an LLVM ptrtoint instruction.
    /// </summary>      
    /// <returns>int</returns>                                                                                                                                                                                                                   
    member x.IntOfPtr ptr = LLGC.buildPtrToInt x.Bb ptr intType ""

    /// <summary>
    /// generate an LLVM inttoptr instruction.
    /// </summary>      
    /// <returns>ptr</returns>                                                                                                                                                                                                                   
    member x.PtrOfInt n ty = LLGC.buildIntToPtr x.Bb n ty ""

    /// <summary>
    /// generate an LLVM instructions to call the fshlvm_time function.
    /// </summary>      
    /// <returns>int</returns>                                                                                                                                                                                                                   
    member x.Time =
        let llTime = x.Load Extern.time.ptr [intn 0L] in
        LLVM.Core.buildCall x.Bb llTime [||] ""       
end       

/// <summary>
///  Create a struct 
/// </summary>      
/// <param name="state">state</param> 
/// <param name="vs">value list</param> 
/// <returns>struct val</returns>                                                                                                                                                                                                                   
let mkStruct (state: State) vs =
    let llTy = structType (Array.ofList(List.map LLGC.typeOf vs)) in
    let llX s x i = LLGC.buildInsertValue state.Bb s x ((uint32)i) "" in
    let aux (i, s) x = i+1, (llX s x i) in
    snd(List.fold aux (0, makeUndef llTy) vs)

/// <summary>
///  Create a state object and save the current shadow stack depth.
/// </summary>      
/// <param name="passTl">passTl</param> 
/// <param name="func">function</param> 
/// <returns>state</returns>                                                                                                                                                                                                                   
let mkState (passTl:bool) func =
    let (state:State) = new State(passTl, func) in
    let depth (state:State) = 
        if state.GetGcEnabled && !Options.ShadowStackEnabled then
            let stackDepth, _ = State.getStack state in
            (state.Load stackDepth [intn 0L])
        else
            intn 0L in
    let depth0 = lazy(depth state) in
    state.SetDepth depth0

/// <summary>
///  Create a reference 
/// </summary>      
/// <param name="state">state</param> 
/// <param name="llty">llvm type</param> 
/// <param name="tag">gc tag</param>
/// <param name="data">data</param>
/// <param name="mark">gc mark</param>
/// <returns>reference val</returns>                                                                                                                                                                                                                   
let mkRef (state: State) llty tag data mark =
    mkStruct state [ state.Bitcast llty stringType;
                         tag;
                         state.Bitcast data stringType;
                         mark ]                                   

/// <summary>
/// Create a unique string based upon the given string.   
/// </summary>      
/// <returns>string->string</returns>                                                                                                                                                                                                                   
let uniq =
    let m = new KPTech.FsHlvm.Collections.HashMultiMap<string,unit>(1, HashIdentity.Structural) in
    let rec aux s =
        match m.TryFind s with
        | Some x -> 
            aux (s+"'")
        | None ->
            m.Add (s, ());
            s in
    aux
    
exception Returned
    
/// <summary>
/// top level definitions, runtime llvm types
/// </summary>      
module RLType = begin
    type t =
        | TLUnsafeFunction of string * (string * Type.t) list * Type.t * Expr.t
        | TLFunction of string * (string * Type.t) list * Type.t * Expr.t
        | TLExpr of Expr.t
        | TLExtern of string * Type.t list * Type.t
        | TLType of string * Type.t
end
open RLType
        
/// <summary>
/// Helper function for type checking
/// </summary>      
/// <param name="err">error message</param> 
/// <param name="ty1">type1</param> 
/// <param name="ty2">type2</param> 
/// <returns>unit</returns>                                                                                                                                                                                                            
let typeCheck err ty1 ty2 =
    if not (Type.eq ty1 ty2) then
        invalidArg "arg"
            (sprintf "%s: %a != %a" err Type.toString ty1 Type.toString ty2)

/// <summary>
/// Constant string literals are memoized here.
/// </summary>      
let stringCache = new KPTech.FsHlvm.Collections.HashMultiMap<string,LLGT.ValueRef>(13, HashIdentity.Structural)
        
/// <summary>
/// Memoize a string
/// </summary>      
/// <returns>val</returns>                                                                                                                                                                                                            
let mkString (str:string) =
    match stringCache.TryFind str with
    | None -> 
        let spec = makeGlobalVar "buf" (constStringNT llContext str) M in
        stringCache.Add (str, spec);
        spec
    | Some s -> s

/// <summary>
/// List of functions that have been evaluated. The "main" function generated
/// for standalone computation calls each of these functions in turn.
/// </summary>      
let evalFunctions = ref []

/// <summary>
/// initialize llvm target
/// </summary>      
let (ee: LLGEE.ExecutionEngineRef option) =
    if not !Options.CompileOnly then
        LLVM.Target.initializeX86Target()
        LLVM.Target.initializeX86TargetInfoNative()
        LLVM.Target.initializeX86TargetMCNative()
        ignore(LLVM.ExecutionEngine.createExecutionEngineForModule M)
        Some(LLVM.ExecutionEngine.createJITCompilerForModule M 0u)
    else
        None

/// <summary>
/// Register a function and execute it.
/// </summary>      
/// <param name="llf">llvm function</param> 
/// <returns>unit</returns>                                                                                                                                                                                                            
let runFunction llf =
    evalFunctions := !evalFunctions @ [llf];
    if not !Options.CompileOnly then
        let intValue = LLVM.Generated.ExecutionEngine.createGenericValueOfInt intType 0UL true in
        match ee with
        | Some ee ->
            ignore
                (LLVM.ExecutionEngine.runFunction ee llf [|intValue|])
        | None ->
            failwith "Unable to initialize JIT"

/// <summary>
/// generate Push a reference onto the visit stack.
/// </summary>      
/// <param name="p">reference</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
let gcPush p =
        Let("p", p,
            If(AddressOf(Var "p") =. Int 0L, Unit,
                Let("a", Load(visitStack, Type.TArray Type.TReference),
                    Let("n", Load(nVisit, Type.TInt),
                        compound
                            [ Expr.Set(Var "a", Var "n", Var "p");
                              Store(nVisit, Var "n" +. Int 1L) ]))))

/// <summary>
/// generate gc check expr
/// </summary>      
/// <param name="()">unit</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
let gcCheck () =
  if !Options.GcEnabled then
        Let("tick", 
                GetThreadTick,
                If(Var "tick" <. Int Options.Ticks,
                        SetThreadTick(Int 1L +. Var "tick"),
                        Apply(Var "gc_check", [])))
  else
    Unit
                          
let llTyNull = ref None

/// <summary>
/// Compile an expression in the context of current vars into the given
/// LLVM state.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="e">expression</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
let rec expr vars (state: State) e =
    if !Options.Debug then 
        begin
            Options.Depth := !Options.Depth + 1;
            printfn "%s-> expr %s" ("") (Expr.toString () e);
        end
    let state, (x, ty_x) as ret =
        try exprAux vars state e with
        | Returned as exn -> raise exn
        | exn ->
            printfn "ERROR: %s" (escapeString ((Expr.toString () e)));
            raise exn in
    if !Options.Debug then
        begin
            Options.Depth := !Options.Depth - 1;
            printfn "<-%s %s"  ("") (escapeString ((stringTypeOf x M)));
        end
    ret
    
and exprAux vars (state: State) = function
    | Return (Unsafe f, ty) -> 
        expr vars state (Unsafe(Return(f, ty)))
    | Unsafe f ->
        let enabled = state.GetGcEnabled in
        let (state: State), result = expr vars (state.SetGc false) f in
        state.SetGc enabled, result
    | Null ->
        let llTy =
            match !llTyNull with
            | Some llTy -> llTy
            | None ->
                let name = "Null" in
                let ty = Type.TUnit in
                let llTy = makeGlobalVar name (makeUndef RTType.llType) M in
                types.Add (name, (llTy, ty));
                let llVisit = defVisit vars name name ty in
                let llPrint = defPrint vars name name ty in
                initType name llTy llVisit llPrint;
                llTyNull := Some llTy;
                llTy in
        let ret = state, (mkRef state llTy (intn 0L) nulln nulln, TReference) in
        ret
    | Unit -> 
        let ret = state, (unitn, Type.TUnit) in
        ret
    | Bool (b:bool) -> state, (constInt i1Type (if b then 1UL else 0UL) false, Type.TBool)
    | Int n -> state, (intn n, Type.TInt)
    | Int8 n -> state, (int8n ((int8)n), Type.TInt8)
    | Int16 n -> state, (int16n ((int16)n), Type.TInt16)
    | Int32 n -> state, (int32n ((int32)n), Type.TInt32)
    | Int64 n -> state, (int64n ((int64)n), Type.TInt64)
    | UInt n -> state, (intn (int64(n)), Type.TInt)
    | UInt8 n -> state, (int8n ((int8)n), Type.TUInt16)
    | UInt16 n -> state, (int16n ((int16)n), Type.TInt16)
    | UInt32 n -> state, (int32n ((int32)n), Type.TInt32)
    | UInt64 n -> state, (int64n ((int64)n), Type.TInt64)
    | Float32 x -> state, (float32f ((single)x), Type.TFloat32)
    | Float64 x -> state, (float64f ((double)x), Type.TFloat64)
    | Struct fs ->
        let state, (fs,tys_f) = exprs vars state fs in
        state, (mkStruct state fs, Type.TStruct tys_f)
    | GetValue(s, i) ->
        let state, (s, ty_s) = expr vars state s in
        begin
            match ty_s with
            | Type.TStruct tys ->
                let v = State.ExtractValue state s ((uint32)i) in
                state, (v, List.item i tys)
            | ty -> invalidArg "GetValue" (sprintf "GetValue of %a" Type.toString ty)
        end
    | Construct(f, x) ->
        let (llTy, ty) = findType f in
        let state, (x, tyX) = expr vars state x in
        typeCheck "Type constructor argument of wrong type" ty tyX;
        let (state:State), s =
            match tyX with
            | Type.TUnit -> state, mkRef state llTy (intn 0L) nulln nulln
            | _ ->
                let px = state.Malloc (llTypeOf tyX) (intn 1L) in
                state.Store px [intn 0L] x;
                let mark = state.Malloc i8Type (intn 1L) in
                state.Store mark [intn 0L] (int8n 0y);
                let s = mkRef state llTy (intn 0L) px mark in
                let state = gcRoot vars state (lazy s) TReference in
                let state = gcAlloc vars state s in
                state, s in
        state, (s, TReference)
    | IsType(f, tyName) ->
        let state, (f, ty_f) = expr vars state f in
        typeCheck "IsType of non-reference type" ty_f TReference;
        let llTyF = State.ExtractValue state f Ref.llTy in
        let llTyF = state.Bitcast llTyF (pointerType RTType.llType) in
        let llty, ty = findType tyName in
        state, (LLGC.buildICmp state.Bb LLGC.IntPredicate.IntEQ llTyF llty "IsType", Type.TBool)
    | Cast (f, tyName) ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "Cast of non-reference type" tyF TReference;
        let llTy, ty = findType tyName in
            if ty = Type.TUnit then (state, (unitn, Type.TUnit)) else
                        let v = State.ExtractValue state f Ref.data in
                        let v = state.Bitcast v (pointerType(llTypeOf ty)) in
                        let v = state.Load v [intn 0L] in
                        let state = gcRoot vars state (lazy v) ty in
                        state, (v, ty)
    | Var x ->
        let x, tyX = findByKey x vars.vals in
        state, (x, tyX)
    | UnArith(Neg, f) ->
        let state, (f, tyF) = expr vars state f in
        let build =
            match tyF with
            | Type.TInt | Type.TInt8 | Type.TInt16 | Type.TInt32 | Type.TInt64 -> LLGC.buildNeg
            | Type.TFloat32 | Type.TFloat64 -> LLGC.buildFNeg
            | _ -> invalidArg "expr.unarith" "" in       
        state, (build state.Bb f "", tyF)
    | BinArith(op, f, g) ->
        let state, (f, fTy), (g, gTy) = expr2 vars state f g in
        let build =
            match op, (fTy, gTy) with
            | Add, (Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64) -> LLGC.buildAdd
            | Sub, (Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64) -> LLGC.buildSub
            | Mul, (Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64) -> LLGC.buildMul
            | Add, (Type.TFloat32, Type.TFloat32 | Type.TFloat64, Type.TFloat64) -> LLGC.buildFAdd
            | Sub, (Type.TFloat32, Type.TFloat32 | Type.TFloat64, Type.TFloat64) -> LLGC.buildFSub
            | Mul, (Type.TFloat32, Type.TFloat32 | Type.TFloat64, Type.TFloat64) -> LLGC.buildFMul
            | Div, (Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64) -> LLGC.buildSDiv
            | Mod, (Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64) -> LLGC.buildSRem
            | Div, (Type.TFloat32, Type.TFloat32 | Type.TFloat64, Type.TFloat64) -> LLGC.buildFDiv
            | _ -> invalidArg "expr.arith"
                        (sprintf "%s, %s" (Type.toString() fTy) (Type.toString() gTy)) in

        state, (build state.Bb f g "", fTy)
    | Cmp(op, f, g) ->
        let state, (f, fTy), (g, gTy) = expr2 vars state f g in
        let build =
            match fTy, gTy with
            | Type.TInt, Type.TInt | Type.TInt8, Type.TInt8 | Type.TInt16, Type.TInt16 | Type.TInt32, Type.TInt32 | Type.TInt64, Type.TInt64 | Type.TUInt, Type.TUInt | Type.TUInt8, Type.TUInt8 | Type.TUInt16, Type.TUInt16 | Type.TUInt32, Type.TUInt32 | Type.TUInt64, Type.TUInt64 ->
                let op = 
                    match op with
                        | Lt -> LLGC.IntPredicate.IntSLT
                        | Le -> LLGC.IntPredicate.IntSLE
                        | Eq -> LLGC.IntPredicate.IntEQ
                        | Ne -> LLGC.IntPredicate.IntNE
                        | Ge -> LLGC.IntPredicate.IntSGE
                        | Gt -> LLGC.IntPredicate.IntSGT in
                (LLGC.buildICmp state.Bb op)
            | Type.TFloat32, Type.TFloat32 | Type.TFloat64, Type.TFloat64 ->
                let op = 
                    match op with
                        | Lt -> LLGC.RealPredicate.RealOLT
                        | Le -> LLGC.RealPredicate.RealOLE
                        | Eq -> LLGC.RealPredicate.RealOEQ
                        | Ne -> LLGC.RealPredicate.RealONE
                        | Ge -> LLGC.RealPredicate.RealOGE
                        | Gt -> LLGC.RealPredicate.RealOGT in
                (LLGC.buildFCmp state.Bb op)
            | _ -> invalidArg "expr.cmp" "" in
        state, (build f g "", Type.TBool)
    | Return(If(p, t, f), tyRet) ->
        let state, (p, tyP) = expr vars state p in
        typeCheck "Predicate of non-bool type" tyP Type.TBool;
        let fState, tState = state.Mk "rfail", state.Mk "rpass" in
        let _ = LLGC.buildCondBr state.Bb p tState.GetBlk fState.GetBlk in
        returnx vars tState t tyRet;
        returnx vars fState f tyRet;
        raise Returned
    | If(p, t, f) ->
        let state, (p, tyP) = expr vars state p in
        typeCheck "Predicate of non-bool type" tyP Type.TBool;
        //let tState, fState = state.Mk "pass", state.Mk "fail" in
        let fState, tState = state.Mk "fail", state.Mk "pass" in
        let _ = LLGC.buildCondBr state.Bb p tState.GetBlk fState.GetBlk in
        let kState = state.Mk "cont" in
        let tState, (t, tyT) = expr vars tState t in
        tState.Br kState;
        let fState, (f, tyF) = expr vars fState f in
        fState.Br kState;
        typeCheck "If" tyT tyF;
        if tyT = Type.TUnit then kState, (unitn, Type.TUnit) else
            let phi = LLVM.Core.buildPhiWithIncoming kState.Bb (llTypeOf tyT) [|(t, tState.GetBlk); (f, fState.GetBlk)|] "" in
            kState, (phi, tyT)
    | Return(Let(x, f, g), tyRet) ->
        expr vars state (Let(x, f, Return(g, tyRet)))
    | Let(x, f, g) ->
        let state, (f, tyF) = expr vars state f in
        let dummy0 = (addVal (x, (f, tyF)) vars) in
        let state, (g, tyG) = expr dummy0 state g in
        //printfn "Let<%s,%s>" (Type.ToString() tyF) (Type.ToString() tyG);
        state, (g, tyG)
    | Alloc(_n, _x) ->
        let state, (n, tyN), (x, tyX) = expr2 vars state _n _x in
        typeCheck "Allow with non-int length" tyN Type.TInt;
        let data = state.Malloc (llTypeOf tyX) n in
        let state, (markSize, _) =
            expr vars state (If(Llvalue(n, Type.TInt) =. Int 0L, Int 0L, Int 1L)) in
            let mark = state.Malloc i8Type markSize in
            let a = mkRef state (mkArrayType tyX) n data mark in
            let tyA = TArray tyX in
            let fill = fill vars tyX in
            let state, _ =
                expr vars state
                    (Let("a", Llvalue(a, TArray tyX),
                        compound
                            [ Apply(fill, [ Var "a";
                                            Llvalue(x, tyX);
                                            Int 0L;
                                            Llvalue(n, Type.TInt) ]);
                             If(AddressOf(Var "a") =. Int 0L, Unit, SetMark(Var "a", 0)) ])) in
        let state = gcRoot vars state (lazy a) tyA in
        let state = gcAlloc vars state a in
        state, (a, tyA)
    | Length a -> 
        let state, (a, tyA) = expr vars state a in
        (match tyA with 
            | TArray _ -> ()
            | _ -> invalidArg "Length of a non-array" "");
        state, (State.ExtractValue state a Ref.tag, Type.TInt)
    | Get(a, i) ->
        let state, (a, tyA), (i, tyI) = expr2 vars state a i in
        let tyElt = 
            match tyA with
                | TArray ty -> ty
                | _ -> invalidArg "Index into non-array type" "" in
        typeCheck "Index" tyI TInt;
        let state, _ =
            expr vars state
                (If((Llvalue(i, TInt) >=. Int 0L) &&. (Llvalue(i, Type.TInt) <. Length(Llvalue(a, tyA))), Unit,
                    compound [ Printf ("Array index out of bounds\n", []);
                               Exit(Int32 3) ])) in
        let data = State.ExtractValue state a Ref.data in
        let data = state.Bitcast data (pointerType (llTypeOf tyElt)) in
        let x, tyX = state.Load data [i], tyElt in
        let state = gcRoot vars state (lazy x) tyX in
        state, (x, tyX)
    | Set(a, i, x) ->
        let state, (a, tyA), (i, tyI), (x, tyX) = expr3 vars state a i x in
        typeCheck "Set with invalid element type" tyA (TArray tyX);
        typeCheck "Set with non-int index" tyI TInt;
        let state, _ =
            expr vars state
                (If((Llvalue(i, TInt) >=. Int 0L) &&. (Llvalue(i, Type.TInt) <. Length(Llvalue(a, tyA))), Unit,
                    compound [ Printf("Array index out of bounds\n", []);
                               Exit(Int32 2) ])) in
        let data = State.ExtractValue state a Ref.data in
        let data2 = state.Bitcast data (pointerType (llTypeOf tyX)) in
        state.Store data2 [i] x;
        state, (unitn, TUnit)
    | Return(Apply(f, args), tyRet) ->
        let state, (f, tyF) = expr vars state f in
        let state, (args, tysArg) = exprs vars state args in
        state.GcRestore();
        typeCheck "Function" tyF (Type.TFunction(tysArg, tyRet));
        let call =
            if isStruct tyRet then
                /// Tail call returning struct. Pass the sret given to us by our
                /// caller on to our tail callee.
                state.Call ((uint32)cc) f (state.ThreadLocal :: state.Sret :: args)
            else
                /// Tail call returning single value.
                state.Call ((uint32)cc) f (state.ThreadLocal :: args) in
        if !Options.Tco then
            setTailCall call true;
        state.Ret call;
        raise Returned
    | Apply (f, args) ->
        let state, (f, tyF) = expr vars state f in
        let state, (args, tysArg) = exprs vars state args in
        let ret, tyRet =
            match tyF with
            /// Non-tail call returning multiple values.
            | Type.TFunction(tysArg2, tyRet) when isStruct tyRet ->
                List.iter2 (typeCheck "Arg") tysArg tysArg2;
                let ret = state.Alloca (llTypeOf tyRet) in
                let _ = state.Call ((uint32)cc) f (state.ThreadLocal :: ret :: args) in
                state.Load ret [intn 0L], tyRet
            /// Non-tail call returning single value.
            | Type.TFunction(tysArg2, tyRet) ->
                List.iter2 (typeCheck "Arg") tysArg tysArg2;
                state.Call ((uint32)cc) f (state.ThreadLocal :: args), tyRet
            | _ -> invalidArg "Apply of non-function" "" in
        let state = gcRoot vars state (lazy ret) tyRet in
        state, (ret, tyRet)
    | Printf(spec, args) ->            
        let specx = state.Gep (mkString (spec+"\x00")) [int32n 0; intn 0L] in
        let state, (args, _) = exprs vars state args in
        let ext x = 
            if (LLGC.typeOf x <> stringType) then x else
                LLGC.buildFPExt state.Bb x doubleType "" in
        let argst = List.map ext args in
        ignore(state.Call ((uint32)CallConv.CCallConv) Extern.printf (specx::argst));
        state, (unitn, Type.TUnit)
    | IntOfInt (ty, f) ->
        let ty = TInt in
        let state, (f, tyF) = expr vars state f in
        match tyF, ty with
        | TInt8, TInt
        | TInt8, TInt16
        | TInt8, TInt32
        | TInt8, TInt64
        | TInt8, TUInt
        | TInt8, TUInt16
        | TInt8, TUInt32
        | TInt8, TUInt64
        | TInt16, TInt
        | TInt16, TInt32
        | TInt16, TInt64
        | TInt16, TUInt
        | TInt16, TUInt32
        | TInt16, TUInt64
        | TInt32, TInt
        | TInt32, TInt64
        | TInt32, TUInt
        | TInt32, TUInt64
        | TUInt8, TInt
        | TUInt8, TInt16
        | TUInt8, TInt32
        | TUInt8, TInt64
        | TUInt8, TUInt
        | TUInt8, TUInt16
        | TUInt8, TUInt32
        | TUInt8, TUInt64
        | TUInt16, TInt
        | TUInt16, TInt32
        | TUInt16, TInt64
        | TUInt16, TUInt
        | TUInt16, TUInt32
        | TUInt16, TUInt64
        | TUInt32, TInt
        | TUInt32, TInt64
        | TUInt32, TUInt
        | TUInt32, TUInt64 ->
            //extend integer
            state, (LLGC.buildZExt state.Bb f (llTypeOf ty) "", ty)
        | TInt, TInt8
        | TInt16, TInt8
        | TInt32, TInt8
        | TInt64, TInt8
        | TUInt, TInt8
        | TUInt16, TInt8
        | TUInt32, TInt8
        | TUInt64, TInt8
        | TInt, TInt16
        | TInt32, TInt16
        | TInt64, TInt16
        | TUInt, TInt16
        | TUInt32, TInt16
        | TUInt64, TInt16
        | TInt, TInt32
        | TInt64, TInt32
        | TUInt, TInt32
        | TUInt64, TInt32
        | TInt, TUInt8
        | TInt16, TUInt8
        | TInt32, TUInt8
        | TInt64, TUInt8
        | TUInt, TUInt8
        | TUInt16, TUInt8
        | TUInt32, TUInt8
        | TUInt64, TUInt8
        | TInt, TUInt16
        | TInt32, TUInt16
        | TInt64, TUInt16
        | TUInt, TUInt16
        | TUInt32, TUInt16
        | TUInt64, TUInt16
        | TInt, TUInt32
        | TInt64, TUInt32
        | TUInt, TUInt32
        | TUInt64, TUInt32 ->
            //truncate integer
            state, (LLGC.buildTrunc state.Bb f (llTypeOf ty) "", ty)
        | TInt, TInt
        | TInt8, TInt8 
        | TInt16, TInt16 
        | TInt32, TInt32 
        | TInt64, TInt64 
        | TInt, TUInt
        | TInt8, TUInt8 
        | TInt16, TUInt16 
        | TInt32, TUInt32 
        | TInt64, TUInt64 
        | TUInt, TUInt
        | TUInt8, TUInt8 
        | TUInt16, TUInt16 
        | TUInt32, TUInt32 
        | TUInt64, TUInt64 
        | TUInt, TInt
        | TUInt8, TInt8 
        | TUInt16, TInt16 
        | TUInt32, TInt32 
        | TUInt64, TInt64 
        //FIXME: 64 bit only native int
        | TInt, TInt64
        | TInt, TUInt64        
        | TUInt, TInt64
        | TUInt, TUInt64
        | TInt64, TInt
        | TUInt64, TInt
        | TInt64, TInt
        | TUInt64, TInt ->
            //no conversion
            state, (unitn, Type.TUnit)
        | _,_ -> invalidArg "IntOfInt" ""
    | IntOfFloat (ety, f) ->
        let ty = 
            match ety with
            | EInt.Int -> TInt
            | EInt.Int8 -> TInt8
            | EInt.Int16 -> TInt16
            | EInt.Int32 -> TInt32
            | EInt.Int64 -> TInt64
            | EInt.UInt -> TUInt
            | EInt.UInt8 -> TUInt8
            | EInt.UInt16 -> TUInt16
            | EInt.UInt32 -> TUInt32
            | EInt.UInt64 -> TUInt64
        let tyc = 
            match ety with
            | EInt.Int -> Type.TInt
            | EInt.Int8 -> Type.TInt8
            | EInt.Int16 -> Type.TInt16
            | EInt.Int32 -> Type.TInt32
            | EInt.Int64 -> Type.TInt64
            | EInt.UInt -> Type.TUInt
            | EInt.UInt8 -> Type.TUInt8
            | EInt.UInt16 -> Type.TUInt16
            | EInt.UInt32 -> Type.TUInt32
            | EInt.UInt64 -> Type.TUInt64
        let (state: State), (f, tyF) = expr vars state f in
        typeCheck "IntOfFloat of non-float" tyF tyc;
        state, (LLGC.buildFPToSI state.Bb f (llTypeOf ty) "", ty)
    | FloatOfInt (ety, f) ->
        let ty = 
            match ety with
            | EFloat.Float32 -> TFloat32
            | EFloat.Float64 -> TFloat64
        let (state:State), (f, tyF) = expr vars state f in
        //FIXME: support other interger types
        typeCheck "FloatOfInt of non-int" tyF Type.TInt;
        state, (LLGC.buildSIToFP state.Bb f (llTypeOf ty) "", ty)
    | Print f ->
        let state, (f, tyF) = expr vars state f in
        let vars = addVal ("x", (f, tyF)) vars in
        begin
            match tyF with
            | Type.TUnit -> expr vars state (Printf("()", []))
            | Type.TBool ->
                expr vars state
                    (If(Var "x", Printf("true", []), Printf("false", [])))
            | Type.TInt -> expr vars state (Printf("%lld", [Var "x"]))
            | Type.TInt8 -> expr vars state (Printf("%d", [Var "x"]))
            | Type.TInt16 -> expr vars state (Printf("%hd", [Var "x"]))
            | Type.TInt32 -> expr vars state (Printf("%ld", [Var "x"]))
            | Type.TInt64 -> expr vars state (Printf("%lld", [Var "x"]))
            | Type.TUInt -> expr vars state (Printf("%ulld", [Var "x"]))
            | Type.TUInt8 -> expr vars state (Printf("%ud", [Var "x"]))
            | Type.TUInt16 -> expr vars state (Printf("%uhd", [Var "x"]))
            | Type.TUInt32 -> expr vars state (Printf("%uld", [Var "x"]))
            | Type.TUInt64 -> expr vars state (Printf("%ulld", [Var "x"]))
            | Type.TFloat32 -> expr vars state (Printf("%#g", [Var "x"]))
            | Type.TFloat64 -> expr vars state (Printf("%#g", [Var "x"]))
            | Type.TStruct tys ->
                let aux i = Print(GetValue(Var "x", i)) in
                let xs = List.init (List.length tys) aux in
                expr vars state
                    (compound
                        [ Printf("(", []);
                          compound(List.Between (Printf(", ", [])) xs);
                          Printf(")", []) ])
            | Type.TFunction _ -> expr vars state (Printf("<fun>", []))
            | Type.TArray _ 
            | Type.TReference ->
                let llTy = State.ExtractValue state f Ref.llTy in
                let llTy = state.Bitcast llTy (pointerType RTType.llType) in
                let llTy = state.Load llTy [intn 0L] in
                let p = State.ExtractValue state llTy ((uint32)RTType.print) in
                let tyP = Type.TFunction([tyF], Type.TUnit) in
                let vars = addVal ("p", (p, tyP)) vars in
                expr vars state (Apply(Var "p", [Var "x"]))
        end
    | Visit f ->
        let state, (f, tyF) = expr vars state f in
        begin
            match tyF with
            | TReference ->
                let llTy = State.ExtractValue state f Ref.llTy in
                let llTy = state.Bitcast llTy (pointerType RTType.llType) in
                let llTy = state.Load llTy [intn 0L] in                
                let p = State.ExtractValue state llTy ((uint32)RTType.visit) in
                state, (p, RTType.tyVisit)
            | ty -> invalidArg "Visit of non-reference" ""
        end
    | Free f ->
        let state, (f, tyF) = expr vars state f in
        begin
            match tyF with
            | Type.TArray _ | Type.TReference _ ->
                state.Free (State.ExtractValue state f Ref.data);
                state.Free (State.ExtractValue state f Ref.mark);
            | Type.TInt ->
                state.Free (state.PtrOfInt f stringType);
            | _ -> invalidArg "Free of non (array|reference|int)" ""
        end;
        state, (unitn, Type.TUnit)
    | Exit f ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "Exit" tyF Type.TInt32;
        ignore(state.Call ((uint32)CallConv.CCallConv) Extern.exit [f]);
        state, (unitn, Type.TUnit)
    | Load(ptr, ty) ->
        state, (state.Load ptr [intn 0L], ty)
    | Store(ptr, f) ->
        let state, (f, tyF) = expr vars state f in
        state.Store ptr [intn 0L] f;
        state, (unitn, Type.TUnit)
    | AddressOf f ->
        let state, (f, tyF) = expr vars state f in
        if not (isRefType tyF) then
            invalidArg "AddressOf of non-reference" "";
        let ptr = State.ExtractValue state f Ref.data in
        let ptr = state.IntOfPtr ptr in
        state, (ptr, Type.TInt)
    | Llvalue(v, ty) -> state, (v, ty)
    | Magic(f, ty) ->
        let state, (f, tyF) = expr vars state f in
        begin
            match tyF, ty with
            | Type.TInt, Type.TArray tyElt ->
                let f = state.PtrOfInt f stringType in
                state, (mkRef state (mkArrayType tyElt) (intn 1L) f nulln, ty)
            | Type.TReference, Type.TArray _ | Type.TArray _, Type.TReference -> state, (f, ty)
            | _ -> invalidArg "Magic of non-(int-reference)" "";
        end
    | Return(f, tyRet) ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "Return" tyRet tyF;
        state.GcRestore();
        if (isStruct tyF) then begin
            state.Store state.Sret [intn 0L] f;
            state.Ret unitn;
        end else
            state.Ret f;
        raise Returned
    | GetMark f ->
        let state, (f, tyF) = expr vars state f in
        if not(isRefType tyF) then
            invalidArg "GetMark of non-reference" "";
        let mark = State.ExtractValue state f Ref.mark in
        let mark = state.Load mark [intn 0L] in
        let mark = LLGC.buildZExt state.Bb mark intType "int_of_mark" in
        state, (mark, Type.TInt)
    | SetMark(f, n) ->
        let state, (f, tyF) = expr vars state f in
        if not (isRefType tyF) then
            invalidArg "GetMark on non-reference" "";
        let mark = State.ExtractValue state f Ref.mark in
        state.Store mark [intn 0L] (int8n ((int8)n));
        state, (unitn, Type.TUnit)
    | Time -> state, (state.Time, Type.TFloat64)
    | CreateThread(f, x) ->            
        let state, (f, tyF), (x, tyX) = expr2 vars state f x in
        printfn "CreateThread<%s,%s>" (Type.toString() tyF) (Type.toString() tyX);
        typeCheck "CreateThread" tyF (Type.TFunction([tyX], Type.TUnit));
        let cf = 
            // Create a wrapper function with the C calling convention that
            // unboxes "x", initializes the thread-local state and applies "f" to
            // "x".
            // Note that the spawned thread will push its arguments onto its
            // shadow stack before the first safe point (that the GC must wait
            // for), so the argument can never get collected between becoming
            // unreachable in this parent thread and reachable in the child
            // thread.
            let fName = sprintf "fshlvm_thread_apply<%a>()" Type.toString tyX in
            mkFun false true vars CallConv.CCallConv fName ["ptr", TInt] TUnit
                (Unsafe
                    (Let("tfx",
                        Get(Magic(Var "ptr", TArray(TStruct[ThreadLocal.ty; tyF; tyX])), Int 0L),
                        compound
                            [ Free(Var "ptr");
                              dPrintf("%d registered threads\n",
                                [Sequence.count ThreadGlobal.loadList]);
                              SetThreadLocal(GetValue(Var "tfx", 0));
                              ExtSetThreadLocal GetThreadLocal;
                              
                              dPrintf("%p just starting\n",
                                [AddressOf GetThreadLocal]);
                              Apply(GetValue(Var "tfx", 1),
                                [GetValue(Var "tfx", 2)]);
                              
                              dPrintf("%p removing itself from global thread list\n",
                                [AddressOf GetThreadLocal]);
                              
                              ThreadGlobal.lock
                                (ThreadGlobal.storeList
                                    (Apply(sequenceRemove ThreadLocal.eq ThreadLocal.ty,
                                        [ThreadGlobal.loadList; GetThreadLocal; Int 0L])));
                              
                              dPrintf("%p terminating\n", [AddressOf GetThreadLocal]);
                              ThreadLocal.free GetThreadLocal;
                              dPrintf("%d registered threads\n",
                                [Sequence.count ThreadGlobal.loadList]);
                            ]))) in
        // Create new thread-local state, register it on the global thread list
        // and return it. Note that this means a GC cannot occur without the
        // cooperation of the thread we are spawning so we known it will have
        // pushed its roots onto the shadow stack before the next GC.
        let state, (t, tyT) = 
            expr vars state 
                (Unsafe
                    (compound
                        [ThreadGlobal.lock
                            (Let("t", ThreadLocal.make,
                                compound
                                    [ ThreadGlobal.storeList
                                        (Sequence.push(ThreadGlobal.loadList, Var "t"));
                                      Var "t" ])) ])) in
        typeCheck "CreateThread internal" tyT ThreadLocal.ty;
        let state, (ptr, _) =
            expr vars state
                (Unsafe(AddressOf(Alloc(Int 1L, Struct[Llvalue(t, tyT);
                                                      Llvalue(f, tyF);
                                                      Llvalue(x, tyX)])))) in
        let createThread = state.Load Extern.createThread.ptr [intn 0L] in
        let cf = state.Bitcast cf stringType in
        let thread = state.Call ((uint32)CallConv.CCallConv) createThread [cf; ptr] in
        state, (thread, Type.TInt)
    | JoinThread f ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "JoinThread" tyF Type.TInt;
        let state, _ = expr vars state (enterBlockingSection()) in
        let joinThread = state.Load Extern.joinThread.ptr [intn 0L] in
        ignore(state.Call ((uint32)CallConv.CCallConv) joinThread [f]);
        let state, _ = expr vars state (leaveBlockingSection()) in
        state, (unitn, Type.TUnit)
    | CreateMutex ->
        let createMutex = state.Load Extern.createMutex.ptr [intn 0L] in
        let m = state.Call ((uint32)CallConv.CCallConv) createMutex [] in
        state, (m, Type.TInt)
    | UnsafeLockMutex f ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "UnsafeLockMutex" tyF Type.TInt;
        let lockMutex = state.Load Extern.lockMutex.ptr [intn 0L] in
        ignore(state.Call ((uint32)CallConv.CCallConv) lockMutex [f]);
        state, (unitn, Type.TUnit)
    | UnlockMutex f ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "UnlockMutex" tyF Type.TInt;
        let unlockMutex = state.Load Extern.unlockMutex.ptr [intn 0L] in
        ignore(state.Call ((uint32)CallConv.CCallConv) unlockMutex [f]);
        state, (unitn, Type.TUnit)
    | ExtGetThreadLocal ->
        //printfn "ExtGetThreadLocal";
        let getThreadLocal = state.Load Extern.getThreadLocal.ptr [intn 0L] in
        let ptr = state.Call ((uint32)CallConv.CCallConv) getThreadLocal [] in
        let ptr = state.PtrOfInt ptr stringType in
        let llTy = mkArrayType ThreadLocal.internalTy in
        state, (mkRef state llTy (intn 1L) ptr nulln, ThreadLocal.ty)
    | ExtSetThreadLocal f ->
        let state, (f, tyF) = expr vars state f in
        //printfn "ExtSetThreadLocal<%s,%s>" (Type.ToString() tyF) (Type.ToString() ThreadLocal.ty);
        typeCheck "ExtSetThreadLocal" tyF ThreadLocal.ty;
        let ptr = State.ExtractValue state f Ref.data in
        let ptr = state.IntOfPtr ptr in
        let setThreadLocal = state.Load Extern.setThreadLocal.ptr [intn 0L] in
        ignore(state.Call ((uint32)CallConv.CCallConv) setThreadLocal [ptr]);
        state, (unitn, Type.TUnit)
    | GetThreadLocal ->
        let llTy = mkArrayType ThreadLocal.internalTy in
        let ptr = state.ThreadLocal in
        state, (mkRef state llTy (intn 1L) ptr nulln, ThreadLocal.ty)
    | SetThreadLocal f ->
        let state, _ = expr vars state (ExtSetThreadLocal f) in
        let state, (tl, ty) = expr vars state ExtGetThreadLocal in
        let ptr = State.ExtractValue state tl Ref.data in
        state.SetThreadLocal ptr, (unitn, Type.TUnit)
    | GetThreadTick ->
        let ptr = state.Bitcast state.ThreadLocal (pointerType intType) in
        state, (state.Load ptr [intn 0L], Type.TInt)
    | SetThreadTick f ->
        let state, (f, tyF) = expr vars state f in
        typeCheck "SetThreadLocal" tyF Type.TInt;
        let ptr = state.Bitcast state.ThreadLocal (pointerType intType) in
        state.Store ptr [intn 0L] f;
        state, (unitn, Type.TUnit)

/// <summary>
/// Compile two expressions
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="f">expression</param> 
/// <param name="g">expression</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and expr2 vars state f g =
        let state, f = expr vars state f in
        let state, g = expr vars state g in
        state, f, g

/// <summary>
/// Compile three expressions
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="f">expression</param> 
/// <param name="g">expression</param> 
/// <param name="h">expression</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and expr3 vars state f g h =
        let state, f = expr vars state f in
        let state, g = expr vars state g in
        let state, h = expr vars state h in
        state, f, g, h

/// <summary>
/// Compile lists of expressions
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="fs">expression list</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and exprs vars state fs =
        let aux (state, rfs, rtysF) f =
            let state, (f, tyF) = expr vars state f in
            state, f::rfs, tyF::rtysF in
        let state, rfs, rtysF = List.fold aux (state, [], []) fs in
        state, (List.rev rfs, List.rev rtysF)

/// <summary>
/// Compile an expression and return from it, marking any calls in tail
/// position as tail calls.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="f">expression list</param> 
/// <param name="tyF">expression type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and returnx vars state f tyF = 
        try
            let _ = expr vars state (Return(f, tyF)) in
            failwith "Internal error: return"
        with Returned -> ()

/// <summary>
///  Register all reference types in the given value as live roots for the GC.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="v">value</param> 
/// <param name="ty">value type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and gcRoot vars state v ty =
        if !Options.Debug then
            printfn "gc_root %s" (Type.toString () ty);
        match ty with
        | TUnit | TBool | TInt | TInt8 | TInt16 | TInt32 | TInt64 | TUInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TFloat32 | TFloat64 | TFunction _ -> 
            state
        | TStruct tys ->
            let f (i, state) ty =
                let v = lazy(State.ExtractValue state (v.Force()) i) in
                ((uint32)i+1u), gcRoot vars state v ty in
            snd(List.fold f (0u, state) tys)
        | TArray _ | TReference ->
            let state = state.GcRoot (v.Force()) in
            state

/// <summary>
/// Register an allocated value if necessary.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="state">state</param> 
/// <param name="v">value</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and gcAlloc vars state v =
        if not state.GetGcEnabled || not !Options.GcEnabled then state else
            let state, _ =
                expr vars state
                    (Let("p", Llvalue(v, TReference),
                        If(AddressOf(Var "p") =. Int 0L, Unit,
                            Expr.lock
                                (Load(allocatedMutex, TInt),
                                 Let("n", Load(nAllocated, TInt),
                                    compound
                                        [ Expr.Set(Load(allocated, TArray TReference), Var "n",
                                            Var "p");
                                          Store(nAllocated, Var "n" +. Int 1L) ]))))) in
            state

/// <summary>
/// Define a function with the given calling convention, name, arguments
/// and return type using the function argument "k" to fill in the body of the
/// defined function.
/// </summary>      
/// <param name="passTl">passTl</param> 
/// <param name="vars">variables</param> 
/// <param name="cc">calling convention</param> 
/// <param name="f">function</param> 
/// <param name="args">function args</param> 
/// <param name="tyRet">function return type</param> 
/// <param name="k">function body</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and defun passTl vars cc f args tyRet k =
    let tysArgs = List.map snd args in
    let ty = tysArgs, tyRet in
    if !Options.Debug then
        let fType = functionTypeOf passTl ty in
        let (s1:string) = LLVM.Extra.typeToString M fType in
        printfn "defun pass_tl:%b %s %s" passTl f s1
    let llvmF = defineFunction f (functionTypeOf passTl ty) M in
    setFunctionCallConv llvmF ((uint32)CallConv.CCallConv);

    let entry = mkState passTl llvmF in
    let start = entry.Mk "start" in

    let vars = addVal (f, (llvmF, TFunction ty)) vars in
    let _, vals' = 
        let aux (i, args) (arg, ty) =
            i+1, (arg, (LLGC.getParam llvmF ((uint32)i), ty))::args in
        let i = if isStruct tyRet then 1 else 0 in
        let i = if passTl then i+1 else i in
        List.fold aux (i, vars.vals) args in

    k { vals = vals'} start;

    let _ = entry.Br start in

    if !Options.View then
        LLVM.Generated.Analysis.viewFunctionCFG llvmF;        
    ignore(LLVM.Generated.Analysis.verifyFunction llvmF LLVM.Generated.Analysis.VerifierFailureAction.PrintMessageAction)

    vars

/// <summary>
/// Compile a top-level definition.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="arg1">top level definition</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and def vars = function
    | TLUnsafeFunction (f, args, tyRet, body) ->
        let (f, args, tyRet, body) =
            (if !Options.Debug then trace else (fun x -> x))
                (f, args, tyRet, body) in
        
        if !Options.Debug then
            printfn "def %s" f;

        let body = Expr.unroll1 f args body in

        if !Options.Debug then
            printfn "%s= %d subexpression" f (Expr.count body);

        defun true vars cc f args tyRet
            (fun vars state ->
                returnx vars state (Unsafe body) tyRet)
    | TLFunction(f, args, tyRet, body) ->            
        let (f, args, tyRet, body) =
            (if !Options.Debug then trace else (fun x -> x))
                (f, args, tyRet, body) in
        if !Options.Debug then
            printfn "def %s" f;

        let (body: Expr.t) = Expr.unroll1 f args body in

        if !Options.Debug then
            printfn "%s= %d subexpressions" f (Expr.count body);

        defun true vars cc f args tyRet
            (fun vars state ->  
                let ps() =
                    let _, (ps, _) =
                        expr vars state
                            (Struct(List.map (fun (s, _) -> Var s) args)) in
                    ps in
                let state =
                    gcRoot vars state (lazy(ps())) (TStruct (List.map snd args)) in
                let body =
                    if isLeaf body then body else
                        compound
                            [ gcCheck();
                              body ] in
                returnx vars state body tyRet)
    | TLExpr f ->
        if !Options.Debug then
            printfn "def <expr>";
        
        let handler k (state:State) =
            if not !Options.StackHandler then k state else
                let size = 16384L in
                let stack = state.Alloca (arrayType i8Type ((uint32)size)) in
                let stack = state.Bitcast stack stringType in

                let tyHandler = functionType voidType [|intType; stringType|] in
                let stackoverflowInstallHandler =
                    declareFunction "stackoverflowInstallHandler"
                     (functionType intType [|pointerType tyHandler; stringType; intType|]) M in
                let stackoverflowDeinstallHandler =
                    declareFunction "stackoverflowDeinstallHandler"
                     (functionType voidType [||]) M in
                
                let llvmHandler =
                    let llvmF = defineFunction "handler" tyHandler M in
                    let state = mkState false llvmF in
                    String.iter
                        (fun c ->
                            let charCode = (int64)c
                            ignore(state.Call ((uint32)CallConv.CCallConv) Extern.putchar [intn charCode]))
                        "Stack overflow";
                    let _ = state.Call ((uint32)CallConv.CCallConv) Extern.exit [int32n 1] in
                    let _ = buildRetVoid state.Bb in
                    ignore (LLVM.Generated.Analysis.verifyFunction llvmF LLVM.Generated.Analysis.VerifierFailureAction.PrintMessageAction);
                    llvmF in

                let _ =
                    if not !Options.StackHandler then unitn else
                    state.Call ((uint32)CallConv.CCallConv) stackoverflowInstallHandler
                        [llvmHandler; stack; intn size] in
                
                let (state:State) = k state in

                let _ =
                    state.Call ((uint32)CallConv.CCallConv) stackoverflowDeinstallHandler [] in
                
                state in
            
            let fName = "evalExpr" 
            let vars2 =
                defun false vars CallConv.CCallConv fName ["", TInt] TUnit
                    (fun vars state ->
                        let state = 
                            handler 
                                (fun state ->
                                    let startTime = Llvalue(state.Time, TFloat64)
                                    let state, (f, tyF) =
                                        expr vars state
                                            (compound 
                                                [ Store(suspendTime, Float64 0.0);
                                                  Store(markTime, Float64 0.0);
                                                  Store(sweepTime, Float64 0.0);
                                                  f ])
                                    let f = 
                                        compound 
                                            [ Printf (" -^ " + Type.toString () tyF + " = ", []);
                                              Print (Llvalue (f,tyF));
                                              Printf ("\n", []) ]
                                    let state, _ = expr vars state f
                                    state.GcRestore()
                                    let state, _ =
                                        expr vars state 
                                            (Printf ("Live: %d\n", [Load(nAllocated, TInt)]))
                                    let state, _ =
                                        expr vars state
                                            (Printf ("%gs total; %gs suspend; %gs mark; %gs sweep\n",
                                                [   Time -. startTime;
                                                    Load(suspendTime, TFloat64);
                                                    Load(markTime, TFloat64);
                                                    Load(sweepTime, TFloat64) ] )) in
                                    state)
                                state in
                            returnx vars state Unit TUnit) in
                let llvmF, _ = assoc fName vars2.vals in
                ignore(runFunction llvmF);

                vars
    | TLExtern(_, _, Type.TStruct _) ->
        failwith "Not yet implemented ffi returning struct\n"
    | TLExtern(f, tysArg, tyRet) ->
        if !Options.Debug then
            printfn "def extern: %s" f;
        let fn =
            let ty =
                functionType (llTypeOf tyRet)
                    (Array.ofList (List.map llTypeOf tysArg)) in
            declareFunction f ty M
        let ty = tysArg, tyRet in
            let llvmF = defineFunction (uniq("vm_" + f)) (functionTypeOf true ty) M in
            LLGC.setFunctionCallConv llvmF ((uint32)CallConv.CCallConv);
            let entry = mkState true llvmF in
            let start = entry.Mk "estart" in
            let args = 
                List.init(List.length tysArg) (fun i -> (LLGC.getParam llvmF) ((uint32)(i + 1))) in
            start.Ret (start.Call ((uint32)CallConv.CCallConv) fn args);
            let _ = entry.Br start in
            ignore (LLVM.Generated.Analysis.verifyFunction llvmF LLVM.Generated.Analysis.VerifierFailureAction.AbortProcessAction);
            addVal (f, (llvmF, TFunction ty)) vars
    | TLType(c, ty) ->
        if !Options.Debug then
            printfn "def type_= %s" c;
        // Define a new type constructor.
        let name = sprintf "%s<%a>" c Type.toString ty in
        if !Options.Debug then
            printfn "def Type= %s" name;
        let llTy = makeGlobalVar name (makeUndef RTType.llType) M in
        types.Add (c, (llTy, ty));
        let llVisit = defVisit vars name c ty in
        let llPrint = defPrint vars name c ty in
        initType name llTy llVisit llPrint;
        vars

/// <summary>
/// Define a function to traverse a reference. 
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="name">function name</param> 
/// <param name="c">c</param> 
/// <param name="ty">function type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and defVisit vars name c ty =
    let f = "visit_" + name in
    let body, vars = visit vars (Var "x") ty in
    let vars = 
        def vars
            (TLUnsafeFunction(f, ["x", TReference], TUnit, Let("x", Cast(Var "x", c), body))) in
    let llVisit, _ = assoc f vars.vals in
    llVisit

/// <summary>
/// Generate an expression that applies the function "f" to every value of a
/// reference type in the value "v".
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="v">value</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and visit vars v = function
    | TUnit | TBool | TInt | TInt8 | TInt16 | TInt32 | TInt64 | TUInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TFloat32 | TFloat64 | TFunction _ -> Unit, vars
    | TStruct tys ->
        let f (i, (fs, vars)) ty =
            let f, vars = visit vars (GetValue(v, i)) ty in
            i+1, (f::fs, vars)
        let _, (fs, vars) = List.fold f (0, ([], vars)) tys in
        compound fs, vars
    | TArray _ -> 
        gcPush(Magic(v, TReference)), vars
    | TReference -> 
        gcPush v, vars

/// <summary>
/// Define a function that visits every value of a reference type in an array
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and defVisitArray vars ty =
    let f = sprintf "visit_array<%s>" (Type.toString () ty) in
    let body, vars = visit vars (Get(Var "a", Var "i")) ty in
    if body = Unit then
        mkFun true true vars cc f [ "a", TReference ] TUnit Unit
    else
        let llVisitAux =
            let f = sprintf "visit_array_aux<%s>" (Type.toString () ty) in
            mkFun true true vars cc f [ "a", TArray ty;
                              "i", TInt ] TUnit
                (Unsafe
                    (If(Var "i" =. Length(Var "a"), Unit,
                        compound
                            [ body;
                              Apply(Var f, [Var "a"; Var "i" +. Int 1L]) ])))
        mkFun true true vars cc f [ "a", TReference ] TUnit
            (Unsafe
                (Apply(Llvalue(llVisitAux, TFunction([TArray ty; TInt], TUnit)),
                    [Magic(Var "a", TArray ty); Int 0L])))

/// <summary>
/// Define a function to print a boxed value.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="name">name</param> 
/// <param name="c">c</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and defPrint vars name c ty =
    let f = sprintf "print<%s>" name in
    mkFun false true vars cc f [ "x", TReference] TUnit
        (Unsafe
            (compound
                [ Printf (c + "(", []);
                  Print (Cast(Var "x", c));
                  Printf (")", []) ]))

/// <summary>
/// Define a function to print an array.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and defPrintArray vars ty =
    let f = "print_array<" + Type.toString () ty + ">" in
    let aux = "print_array_aux<" + Type.toString () ty + ">" in
    let aux2 =
        mkFun false true vars cc aux [ "a", TArray ty; "i", TInt] TUnit
            (Unsafe
                (compound
                    [ Print(Get(Var "a", Var "i"));
                      If(Var "i" <. Length(Var "a") -. Int 1L,
                        If(Var "i" =. Int 5L,
                           Printf("; ..", []),
                           compound
                            [ Printf ("; ", []);
                              Apply(Var aux, [Var "a"; Var "i" +. Int 1L]) ]),
                        Unit)])) in
    let result = 
     mkFun false true vars cc f ["x", TReference] TUnit
        (Unsafe
            (Let("a", Magic(Var "x", TArray ty),
                compound
                    [ Printf("[|", []);
                      If(Length(Var "a") =. Int 0L, Unit,
                        Apply(Llvalue(aux2, TFunction([TArray ty; TInt], TUnit)),
                            [Var "a"; Int 0L]));
                      Printf("|]", [])]))) in
    result

/// <summary>
/// Create and memoize a reference type. Used to create wrapper reference types
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and mkType vars ty =
    let (name0:string) = Type.toString () ty
    let name = "Box"
    if !Options.Debug then
        printfn "mk_type %s" name
    let foundName = types.TryFind name
    match foundName with
    | Some x ->  vars, (name, foundName)
    | None ->
        let vars = def vars (TLType (name, ty))
        vars, (name, foundName)

/// <summary>
/// Create and memoize a array type.
/// </summary>      
/// <param name="vars">variables</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and mkArrayType ty =
    if !Options.Debug then
        printfn "mk_array_type<%s>" (Type.toString () ty)
    let name = sprintf "mk_array_type<%a>" Type.toString ty 
    let foundName = types.TryFind name 
    match foundName with
    | Some x -> fst x
    | None -> 
        let llTy = makeGlobalVar name (makeUndef RTType.llType) M
        types.Add (name, (llTy, TArray ty))
        let llVisit = defVisitArray vars ty
        let llPrint = defPrintArray vars ty
        initType name llTy llVisit llPrint
        llTy

/// <summary>
///  Compile and run code to initialize the contents of a new type.
/// </summary>      
/// <param name="name">type name</param> 
/// <param name="llTy">llvm type</param> 
/// <param name="llVisit">llvm visit function</param> 
/// <param name="llPrint">lllvm print function</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and initType name llTy llVisit llPrint =
    if !Options.Debug then
        printfn "init_type %s\n" name
    
    let f = sprintf "init_type<%s>" name 
    let vars =
        defun false vars CallConv.CCallConv f ["", TInt] TUnit
            (fun vars state ->
                let state = state.SetGc false
                let state, _ = 
                    if not !Options.Debug then state, (unitn, TUnit) else
                        expr vars state (Printf(f+"()\n", []))
                let s =
                    Struct
                        [ Llvalue (llVisit, TFunction([TReference], TUnit));
                          Llvalue (llPrint, TFunction([TReference], TUnit)) ]
                let state, _ = expr vars state (Unsafe(Store(llTy, s)))
                let state, _ =
                    if not !Options.Debug then state, (unitn, TUnit) else
                        expr vars state (Printf((f + " end\n"), []))
                returnx vars state Unit TUnit)
    let llvmF, _ = assoc f vars.vals
    if !Options.Debug then
        printfn "Running init_type %s" name
    ignore(runFunction llvmF)
    if !Options.Debug then
        printfn "Ran init_type %s" name

/// <summary>
/// Create and memoize a function. Used to create visitor functions, print
/// functions and array fill functions.
/// </summary>      
/// <param name="debug">debug</param> 
/// <param name="passTl">passTl</param> 
/// <param name="vars">vars</param> 
/// <param name="cc">calling conventions</param> 
/// <param name="f">function</param> 
/// <param name="args">function args</param> 
/// <param name="tyRet">function return type</param> 
/// <param name="body">function body</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and mkFun (debug:bool) (passTl:bool) (vars:vars) (cc:CallConv) (f:String) (args: (string * Type.t) list) (tyRet:Type.t) (body:Expr.t) =
    let (f, args, tyRet, body) =
        (if false && debug && !Options.Debug then trace else (fun x -> x))
            (f, args, tyRet, body) in
    if !Options.Debug then
        printfn "mk_fun ~pass_tl:%b %s" passTl f;
    let foundFunction = functions.TryFind f in
    match foundFunction with
    | Some x -> x
    | None -> 
        let body = Expr.unroll1 f args body in
        let vars =
            defun passTl vars cc f args tyRet
                (fun vars state ->
                    let state = state.SetGc false
                    returnx vars state body tyRet) in
        let llTy, _ = findByKey f vars.vals in
        functions.Add (f, llTy);
        llTy

/// <summary>
/// Define a function to fill an array of the given type.
/// Note that this uses recursive subdivision to keep stack consumption down
/// to O(log n) so our bootstrapping code can work (fill the shadow stack etc.)
/// without stack overflowing even if tail calls are disabled.
/// </summary>      
/// <param name="vars">vars</param> 
/// <param name="type">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and fill vars ty =
    let f = sprintf "Array.fill<%a>" Type.toString ty in
    let llvmF = 
        mkFun true true vars cc f [ "a", TArray ty;
                                    "x", ty;
                                    "i", TInt;
                                    "j", TInt] TUnit
            (Unsafe
                (If(Var "j" -. Var "i" <. Int 2L,
                    If(Var "i" =. Var "j", Unit,
                        Expr.Set(Var "a", Var "i", Var "x")),
                    Let("m", Var "i" +. (Var "j" -. Var "i") /. Int 2L,
                        compound
                            [ Apply(Var f, [Var "a"; Var "x"; Var "i"; Var "m"]);
                              Apply(Var f, [Var "a"; Var "x"; Var "m"; Var "j"]) ])))) in
    Llvalue(llvmF, TFunction([TArray ty; ty; TInt; TInt], TUnit))

/// <summary>
/// Define a function to remove an element from a bag using linear search.
/// </summary>      
/// <param name="eq">eq</param> 
/// <param name="ty">type</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
and sequenceRemove eq ty =
    let f = 
        sprintf "Sequence.RemoveAt<%a, %a>"
            Expr.toString (eq (Var "x") (Var "y"))
            Type.toString ty in
    let llvmF =
        mkFun true true vars cc f [ "seq", Sequence.ty ty; 
                                    "x", ty;
                                    "i", TInt ] (Sequence.ty ty)
         (If(Var "i" =. Sequence.count(Var "seq"),
            compound
                [ Printf("Not found in " + f + " ", []);
                  Exit(Int32 3);
                  Var "seq" ],
            If(eq (Var "x") (Sequence.get(Var "seq", Var "i")),
                Sequence.removeAt(Var "seq", Var "i"),
                Apply(Var f, [Var "seq"; Var "x"; Var "i" +. Int 1L])))) in
    Llvalue(llvmF, TFunction([Sequence.ty ty; ty; TInt], Sequence.ty ty))
            

/// <summary>
/// Dynamically load the runtime and initialize the shadow stack and GC.
/// </summary>      
/// <param name="()">unit</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
let init() =        
    disableSignals();
    
    if !Options.Tco then
        enableTailCallOpt();
    if !Options.Debug then
        printfn "init()";
    let fName = "init_runtime" in
    let vars2 =
        defun false vars CallConv.CCallConv fName ["", TInt] TUnit
            (fun vars state ->
                let state = state.SetGc false in
                let state, _ = 
                    if not !Options.Debug then state, (unitn, TUnit) else
                        expr vars state (Printf("init_runtime()\n", [])) in
                    let str s =
                        let str = state.DefineGlobal "str" (constStringNNT llContext s) in
                        state.Gep str [int32n 0; intn 0L] in
                    let libRuntime = 
                        state.Call ((uint32)CallConv.CCallConv) Extern.dlopen
                            [str "./libfshlvmruntime.so"; intn 1L] in
                    let state, _ = 
                        let libRuntime = state.IntOfPtr libRuntime in
                        expr vars state
                            (Unsafe
                                (If(Llvalue(libRuntime, TInt) =. Int 0L,
                                    compound
                                        [Printf("ERROR: libfshlvmruntime.so not found\n", []);
                                         dPrintf("dlopen return value: %d\n",[(Llvalue(libRuntime, TInt))]);
                                         Exit(Int32 1)],
                                    Unit))) in
                     /// FIXME: We should check dlerror in case the required symbols are
                     /// not found.
                    let state, _ =
                        if not !Options.Debug then state, (unitn, TUnit) else
                            expr vars state
                                (Printf("Dynamically loading externs..\n", [])) in
                    let loadFn (ll:Extern.DlfnObject) f = 
                        let ptr = state.Call ((uint32)CallConv.CCallConv) Extern.dlsym [libRuntime; str f] in
                        state.Store ll.ptr [intn 0L] (state.Bitcast ptr ll.ty) in
                    Extern.loadFns loadFn;
                    ignore(state.Call ((uint32)CallConv.CCallConv) (state.Load Extern.init.ptr [intn 0L]) []);
                    let n = Options.MaxDepth in
                    let state, _ =
                        expr vars state
                            (Unsafe
                                (compound [ dPrintf("Storing empty global thread list...\n",[]);
                                            ThreadGlobal.storeList(Sequence.empty(Int 0L));
                                            dPrintf("Registering main thread...\n", []);
                                            Let("thread", ThreadLocal.make,
                                                compound
                                                  [ dPrintf("%p setting thread local\n",[Var "thread"]);
                                                    ExtSetThreadLocal(Var "thread");
                                                    SetThreadLocal(Var "thread");
                                                    dPrintf("%p registering\n",[AddressOf GetThreadLocal]);
                                                    ThreadGlobal.storeList(Sequence.push(ThreadGlobal.loadList, Var "thread")) ]);
                                            dPrintf("Creating visit stack...\n", []);
                                            Store(visitStack, Alloc(Int n, Null));
                                            dPrintf("Creating allocation list...\n", []);
                                            Store(allocated, Alloc(Int n, Null));
                                            dPrintf("Creating allocation list's mutex...\n",
                                                []);
                                            Store(allocatedMutex, CreateMutex);
                                            dPrintf("Initializing thread global...\n", []);
                                            ThreadGlobal.init;
                                            dPrintf("init_runtime() ends\n", []) ])) in
                     returnx vars state Unit TUnit) in

    let _ =
        let llvmF, _ = assoc fName vars2.vals in
        runFunction llvmF in
    vars

/// <summary>
/// Dynamically boot the runtime
/// </summary>      
/// <param name="()">unit</param> 
/// <returns>expr</returns>                                                                                                                                                                                                            
let boot(): t list =
    if not !Options.GcEnabled then [] else
        let printf(x, y) =
            if !Options.Debug then Printf(x, y) else Unit
        
        let getMark f = GetMark f
        let setMark (f, n) = SetMark(f, n)

        // If the given reference is non-null and unmarked then mark it and push
        // its child references onto the visit stack.
        let markOne p =
            Let("p", p,
                If(getMark(Var "p") =. Int 1L, Unit,
                    compound
                        [ setMark(Var "p", 1);
                          Apply(Visit(Var "p"), [Var "p"]) ]))
        [ TLType("Null", TUnit);

          // Mark the whole heap: while the visit stack is non-empty, pop a
          // reference and mark it.
          TLUnsafeFunction
            ("gc_mark_3", [], TUnit,
             Let("n", Load(nVisit, TInt) -. Int 1L,
                If(Var "n" <. Int 0L, Unit,
                    Let("a", Load(visitStack, TArray TReference),
                        compound
                            [ Store(nVisit, Var "n");
                              markOne(Get(Var "a", Var "n"));
                              Apply(Var "gc_mark_3", []) ]))));

          // Mark all roots on the given shadow stack.
          TLUnsafeFunction
            ("gc_mark_2", ["stack", Sequence.ty TReference; "i", TInt], TUnit,
             If(Var "i" =. Sequence.count(Var "stack"), Unit,
                compound
                    [ Let("p", Sequence.get(Var "stack", Var "i"),
                        If(AddressOf(Var "p") =. Int 0L, Unit,
                            markOne(Var "p")));
                      Apply(Var "gc_mark_2", [Var "stack"; Var "i" +. Int 1L]) ]));

          // Mark each shadow stack and then mark the whole heap.
          TLUnsafeFunction
            ("gc_mark", ["i", TInt], TUnit,
             Let("a", ThreadGlobal.loadList,
                If(Var "i" =. Sequence.count(Var "a"),
                    Apply(Var "gc_mark_3", []),
                    compound
                        [ dPrintf("Marking %d roots from %p\n",
                            [Sequence.count
                                (ThreadLocal.stackOf
                                    (Get(Sequence.get(Var "a", Var "i"), Int 0L)));
                             AddressOf(Sequence.get(Var "a", Var "i"))]);
                          Apply(Var "gc_mark_2",
                            [ ThreadLocal.stackOf
                                (Get(Sequence.get(Var "a", Var "i"), Int 0L));
                                Int 0L ]);
                          Apply(Var "gc_mark", [Var "i" +. Int 1L]) ])));

          // Search the allocated list for unmarked references and free them,
          // shrinking the allocated list if it is non-empty by overwriting the
          // freed reference with the last reference. Reset marked references to
          // unmarked.
          TLUnsafeFunction
            ("gc_sweep", ["i", TInt], TUnit,
             Let("n", Load(nAllocated, TInt),
                If(Var "i" >=. Var "n", Unit,
                    Let("a", Load(allocated, TArray TReference),
                        Let("p", Get(Var "a", Var "i"),
                            Let("i",
                                If(getMark(Var "p") =. Int 1L,
                                    compound
                                        [ setMark(Var "p", 0);
                                          Var "i" +. Int 1L],
                                    compound
                                        [ Free(Var "p");
                                          Expr.Set(Var "a", Var "i",
                                              Get(Var "a", Var "n" -. Int 1L));
                                          Store(nAllocated, Var "n" -. Int 1L);
                                          Var "i" ]),
                                Apply(Var "gc_sweep", [Var "i"])))))));

          // Check if any mutator thread is still running.
          TLUnsafeFunction
            ("gc_suspend_2", ["i", TInt], TBool,
             Let("a", ThreadGlobal.loadList,
                If(Var "i" =. Sequence.count(Var "a"),
                   Bool true,
                   Let("t", Sequence.get(Var "a", Var "i"),
                        If(ThreadLocal.eq(Var "t") GetThreadLocal,
                            Apply(Var "gc_suspend_2", [Var "i" +. Int 1L]),
                            If(ThreadLocal.loadState(Var "t") =. Int ThreadState.run,
                                compound
                                    [ dPrintf("%d/%d %p waiting for %p\n",
                                        [Var "i";
                                         Sequence.count(Var "a");
                                         AddressOf GetThreadLocal;
                                         AddressOf(Var "t")]);
                                      Let("", Apply(Var "gc_suspend_2",
                                                    [Var "i" +. Int 1L]),
                                          Bool false) ],
                                compound
                                    [ dPrintf("%d/%d %p not waiting for %p\n",
                                        [Var "i";
                                         Sequence.count(Var "a");
                                         AddressOf GetThreadLocal;
                                         AddressOf(Var "t")]);
                                      Apply(Var "gc_suspend_2",
                                        [Var "i" +. Int 1L]) ]))))));
            
          TLExtern("usleep", [TInt], TInt);
            
          // Loop until all threads have suspended themselves.
          TLUnsafeFunction
            ("gc_suspend", [], TUnit,
             If(ThreadGlobal.lock(Apply(Var "gc_suspend_2", [Int 0L])),
                Unit,
                compound
                    [ Apply(Var "gc_suspend", []) ]));

          // Clear, mark and sweep
          TLUnsafeFunction
            ("gc", [], TUnit,
           let time t fs = 
            Let("time", Time,
                compound
                    (fs @
                        [ printf("Took %gs", [Time -. Var "time"]);
                          Store(t, Load(t, TFloat64) +. Time -. Var "time") ]))
           if not !Options.GcEnabled then compound [] else
            compound
                [
                    dPrintf("GC suspending...\n", []);
                    time suspendTime [ Apply(Var "gc_suspend", []) ];
                    dPrintf("GC marking...\n", []);
                    time markTime [ Apply(Var "gc_mark", [Int 0L]) ];
                    dPrintf("GC sweeping...\n", []);
                    time sweepTime [ Apply(Var "gc_sweep", [Int 0L]) ];
                    dPrintf("GC done. Live: %d\n\n", [Load(nAllocated, TInt)]);
                    Store(quota, Int 256L +. Int 2L *. Load(nAllocated, TInt));
                    dPrintf("GC finished. Restarting %d mutators with quota %d.\n",
                        [Sequence.count(ThreadGlobal.loadList); Load(quota, TInt)]);
                    ThreadGlobal.lock(ThreadGlobal.storeState ThreadState.run);
                ]);

          // Wait in a loop until the global thread state reverts back to "run"
          //   and then set the state of this thread to running.
          TLUnsafeFunction
            ("spin", [], TUnit,
             If(ThreadGlobal.lock
                  (If(ThreadGlobal.loadState =. Int ThreadState.run,
                    compound
                        [ ThreadLocal.storeState GetThreadLocal ThreadState.run;
                        Bool true ],
                    Bool false)),
                Unit,
                Apply(Var "spin", [])));

          TLUnsafeFunction
            ("gc_check", [], TUnit,
             Let("tl", GetThreadLocal,
                Let("status",
                    ThreadGlobal.lock
                        (If(ThreadGlobal.loadState =. Int ThreadState.run,
                            If(Load(nAllocated, TInt) <=. Load(quota, TInt),
                                Int 0L,
                                compound
                                    [ dPrintf("Suspending all other thread %p\n",
                                        [AddressOf(Var "tl")]);
                                      ThreadGlobal.storeState ThreadState.suspend;
                                      Int 1L ]),
                                Int 2L)),
                    compound
                        [ If(Var "status" =. Int 0L,
                             Unit,
                             If(Var "status" =. Int 1L,
                                Apply(Var "gc", []),
                                compound
                                    [ dPrintf("%p suspending itself\n",
                                        [AddressOf(Var "tl")]);
                                      ThreadLocal.storeState (Var "tl")
                                        ThreadState.suspend;
                                      Apply(Var "spin", []);
                                      dPrintf("%p resuming itself\n",
                                              [AddressOf(Var "tl")]) ]));
                          SetThreadTick(Int 0L);
                        ])));
        ]

// Bound variables
// let varsx = ref vars

/// <summary>
/// Evaluate a statement, updating the bound variables.
/// </summary>      
/// <param name="vars">vars reference</param> 
/// <param name="stmt">statements</param> 
/// <returns>unit</returns>                                                                                                                                                                                                            
let eval varsx stmt =
    varsx := def !varsx stmt
        
/// <summary>
/// Save everything that has been evaluated as a standalone program
/// </summary>      
/// <param name="vars">vars reference</param> 
/// <param name="fileName">bitcode filename to save</param> 
/// <returns>unit</returns>                                                                                                                                                                                                            
let save varsx fileName =
    let fName = "main" in
    let _ =
        defun false !varsx CallConv.CCallConv fName [] TUnit
            (fun vars state ->
                let state = state.SetGc false in
                let call llF = 
                    ignore(state.Call ((uint32)CallConv.CCallConv) llF [intn 0L]) in
                List.iter call !evalFunctions;
                returnx vars state Unit TUnit) in
    let verify = verifyModule M LLVM.Generated.Analysis.VerifierFailureAction.PrintMessageAction in  
    let pm = LLGC.createPassManager() in
    LLVM.Generated.Core.setTarget M "x86_64-pc-linux-gnu";
    ignore(LLVM.Generated.Transforms.Scalar.addTailCallEliminationPass pm);
    ignore(LLVM.Generated.BitWriter.writeBitcodeToFile M fileName);

let createEval name defs = 
    Options.Debug := false;
    Options.CompileOnly := true;
    let vars = createVars () in
    let varsx = ref vars in
    varsx := List.fold def (init()) (boot());
    List.iter (eval varsx) defs;
    save varsx name;
    