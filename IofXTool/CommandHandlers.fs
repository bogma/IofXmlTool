module CommandHandlers

open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection

open Types
open Argu
open Helper
open Commands
open IofXmlLib.Calc
open IofXmlLib.Helper
open IofXmlLib.CalcLibBuilder

let info (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let Config = XmlConfig.Load(Path.Combine(cArgs.wDir, cArgs.cfgFile))

    let competitions = 
        match Config.Type with
        | "Team" -> Seq.empty
        | "Cup" -> getFiles cArgs.wDir ((Config.General.ResultFilePrefix) + "*_*.xml") Config.General.RecurseSubDirs
        | _ -> Seq.empty

    printfn "Type:\t%s" Config.Type

    printfn "Input files matching filter (%s)" Config.General.ResultFilePrefix
    competitions |> Seq.iter (printfn "\t%s")
    ()

let newProject (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()

let add (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()

let rules (cArgs:CommonArgs) (args : ParseResults<_>) =
 
    let action = args.GetResult(Action)
    match action with
    | Compile ->
        let ruleFile = args.TryGetResult(File) |> Option.defaultValue "calc_rules.xml"
        match tryLocateFile cArgs.wDir ruleFile with
        | Some x ->
            printfn "rule file path %s is valid" x
            buildCalcLib x
        | None ->
            printfn "rule file not found"
    | List ->
        let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
        let asm = Assembly.Load("CalcLib")
        let calc = asm.GetType("IofXmlLib.Calc")
        let methods = calc.GetMethods(staticFlags)
        methods 
        |> Array.filter (fun m -> not (m.Name.Equals("getCalcStrategy")))
        |> Array.iter (fun m -> printfn "%s" m.Name)
    | RestoreDefault ->
        restoreDefaultCalcLib
    
    ()

