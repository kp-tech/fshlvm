namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsHlvm.Main")>]
[<assembly: AssemblyProductAttribute("FsHlvm")>]
[<assembly: AssemblyDescriptionAttribute("FsHlvm A High Level Virtual Machine Written In F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
