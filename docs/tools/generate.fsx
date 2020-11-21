// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FSHlvm.Core.dll" ]
// Web site location for the generated documentation
let website = "/FSHlvm"

let githubLink = "http://github.com/kp-tech/FSHlvm"

// Specify more information about your project
let info =
  [ "project-name", "FSHlvm"
    "project-author", "Zoltan Podlovics, KP-Tech kft."
    "project-summary", "FSHlvm is a cross-platform open-source virtual machine written in F# and uses the LLVM library for high-performance code generation."
    "project-github", githubLink
    "project-nuget", "http://nuget.com/packages/FSHlvm" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/FSharp.Formatting.2.4.1/lib/net40"
#I "../../packages/RazorEngine.3.3.0/lib/net40"
#I "../../packages/FSharp.Compiler.Service.0.0.36/lib/net40"
#r "../../packages/Microsoft.AspNet.Razor.2.0.30506.0/lib/net40/System.Web.Razor.dll"
#r "../../packages/FAKE/tools/NuGet.Core.dll"
#r "../../packages/FAKE/tools/FakeLib.dll"
#r "RazorEngine.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Markdown.dll"
open Fake
open System.IO
open Fake.FileHelper
open FSharp.Literate
open FSharp.MetadataFormat

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin/Release" //might not work in the future
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting.2.4.1/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  CopyRecursive files output true |> Log "Copying file: "
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  for lib in referenceBinaries do
    MetadataFormat.Generate
      ( bin @@ lib, output @@ "reference", layoutRoots, 
        parameters = ("root", root)::info,
        publicOnly = true )

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    Literate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, fsiEvaluator = new FsiEvaluator(), lineNumbers=false )

// Generate
copyFiles()
buildDocumentation()
//buildReference()
