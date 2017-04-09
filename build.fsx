// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"
#r "System.Xml.Linq"

open Fake
open Fake.Testing
open System.Xml.Linq

let buildDir  = "./build/"

Target "Clean" (fun _ ->
    CleanDirs [buildDir]
)

let projects  = !! "/**/*.fsproj"

Target "Build" (fun _ ->
    MSBuildRelease buildDir "Build" projects
    |> Log "AppBuild-Output: "
)

let testAssemblies = !! (buildDir + "*Tests.dll")

Target "Test" (fun _ ->
    testAssemblies
    |> NUnit3 id
)

let doc = System.Xml.Linq.XDocument.Load("./Vaughan.nuspec")
let version = doc.Descendants(XName.Get("version", doc.Root.Name.NamespaceName)) 

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