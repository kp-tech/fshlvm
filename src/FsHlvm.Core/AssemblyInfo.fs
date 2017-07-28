// ---------------------------------------------------------------------------
// Copyright (c) 2014-2017, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------

namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsHlvm.Core")>]
[<assembly: AssemblyProductAttribute("FsHlvm")>]
[<assembly: AssemblyDescriptionAttribute("FsHlvm A High Level Virtual Machine Written In F#")>]
[<assembly: AssemblyCompanyAttribute("KP-Tech Kft.")>]
[<assembly: AssemblyCopyrightAttribute("All rights reserved.")>]
[<assembly: AssemblyVersionAttribute("2.0")>]
[<assembly: AssemblyFileVersionAttribute("2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0"
