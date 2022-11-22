module NewProjectCmdHandler

open System.IO
open System.Reflection

open Argu

open IofXmlLib.Logging

open Commands

let readResource (resourceName:string) =
    match resourceName.Split(',') with
    | [| _; name |] ->
        let asm = Assembly.GetExecutingAssembly()
        use sr = new StreamReader(asm.GetManifestResourceStream(name.Trim()))
        Some(sr.ReadToEnd())
    | _ -> None

let deployFiles files destination =
    // let p = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
    // let fn = Path.Combine(p, "CalcLib")
    // let fn2 = Path.ChangeExtension(fn, ".fsx")
    // let fn3 = Path.ChangeExtension(fn, ".dll")
    // let fn4 = Path.ChangeExtension(fn, ".bak")

    files |> Array.iter(fun f ->
        let res = "IofXTool, IofXTool.res." + f
        File.WriteAllText(destination, (readResource res |> Option.defaultValue "")))

let newProject (cArgs:CommonArgs) (args : ParseResults<_>) =
    tracer.Info "copying project files to %s" cArgs.wDir

    let t = args.GetResult(Kind)
    match t with
    | Cup -> deployFiles [| "cup_class_template.html"; "cup_details_template.html"; "cup_document_template.html"; "default.css"; "config.xml" |] cArgs.wDir
    | Team -> deployFiles [| "a"; "b"; "c"; "d"; "e"; "" |] cArgs.wDir
    | Sum -> deployFiles [| "a"; "b"; "c"; "d"; "e"; "" |] cArgs.wDir
