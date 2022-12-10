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

let deployFiles files destinationFolder =
    files |> Array.iter(fun (r, fn) ->
        let res = "IofXTool, IofXTool.res." + r
        let dst =
            if fn = "" then
                Path.Combine(destinationFolder, r);
            else
                Path.Combine(destinationFolder, fn);

        let fileName = Path.GetFileName(dst)
        if File.Exists(dst) then
            tracer.Warn "> %s already exists, skipping ..." fileName
        else
            File.WriteAllText(dst, (readResource res |> Option.defaultValue ""))
            tracer.Info "> %s" fileName)

let newProject (cArgs:CommonArgs) (args : ParseResults<_>) =
    let k = args.GetResult(Kind)

    tracer.Info "copying project files to %s" cArgs.wDir

    match k with
    | Cup -> deployFiles [| ("cup_class_template.html", "class_template.html");
                            ("cup_details_template.html", "details_template.html");
                            ("cup_document_template.html", "document_template.html");
                            ("default.css", "");
                            ("config.xml", "") |] cArgs.wDir
    | Sum -> deployFiles [| ("sum_class_template.html", "class_template.html");
                            ("sum_details_template.html", "details_template.html");
                            ("cup_document_template.html", "document_template.html");
                            ("default.css", "");
                            ("config.xml", "") |] cArgs.wDir
    | Team -> deployFiles [| ("team_class_template.html", "class_template.html");
                             ("team_details_template.html", "details_template.html");
                             ("cup_document_template.html", "document_template.html");
                             ("default.css", "");
                             ("config.xml", "") |] cArgs.wDir

    tracer.Info "default project files ready."
