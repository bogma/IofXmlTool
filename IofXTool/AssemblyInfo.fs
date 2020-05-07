namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("IofXTool")>]
[<assembly: AssemblyProductAttribute("IofXTool")>]
[<assembly: AssemblyCompanyAttribute("bogma")>]
[<assembly: AssemblyDescriptionAttribute("A tool for IOF XML based data manipulation.")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
[<assembly: AssemblyInformationalVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "IofXTool"
    let [<Literal>] AssemblyProduct = "IofXTool"
    let [<Literal>] AssemblyCompany = "bogma"
    let [<Literal>] AssemblyDescription = "A tool for IOF XML based data manipulation."
    let [<Literal>] AssemblyVersion = "0.1.0"
    let [<Literal>] AssemblyFileVersion = "0.1.0"
    let [<Literal>] AssemblyInformationalVersion = "0.1.0"
