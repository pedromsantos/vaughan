// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"
#r "System.Xml.Linq"

open Fake
open System.Xml.Linq

let buildDir  = "./build/"


let appReferences  = !! "/**/*.fsproj"

let doc = System.Xml.Linq.XDocument.Load("./Vaughan.nuspec")
let version = doc.Descendants(XName.Get("version", doc.Root.Name.NamespaceName)) 

Target "Clean" (fun _ ->
    CleanDirs [buildDir]
)

Target "Build" (fun _ ->
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! (buildDir + "VaughanTests.dll")
    |> NUnit (fun p ->
        {p with
            ToolPath = "./packages/NUnit.Runners/tools"
            ToolName = "nunit-console.exe"
            DisableShadowCopy = true;
            ShowLabels = false;
        })
)

Target "BuildNuGet" (fun _ ->
    NuGet (fun p -> 
    {p with
        Version = (Seq.head version).Value
        OutputPath = buildDir
        WorkingDir = buildDir
        })  "./Vaughan.nuspec"
)

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "BuildNuGet"

RunTargetOrDefault "BuildNuGet"
