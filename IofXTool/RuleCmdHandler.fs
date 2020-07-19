module RuleCmdHandler

open System.Reflection

open IofXmlLib.CalcLibBuilder
open IofXmlLib.Logging

open Argu
open Helper
open Commands

let rules (cArgs: CommonArgs) (args: ParseResults<_>) =
 
    let action = args.GetResult(Action)
    match action with
    | Compile ->
        let ruleFile = args.TryGetResult(File) |> Option.defaultValue "calc_rules.xml"
        match tryLocateFile cArgs.wDir ruleFile with
        | Some x ->
            tracer.Info "rule file path %s is valid" x
            buildCalcLib x
        | None ->
            tracer.Warn "rule file not found"
    | List ->
        let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
        let asm = Assembly.Load("CalcLib")
        let calc = asm.GetType("IofXmlLib.Calc")
        let methods = calc.GetMethods(staticFlags)
        methods 
        |> Array.filter (fun m -> not (m.Name.Equals("getCalcStrategy")))
        |> Array.iter (fun m -> tracer.Info "%s" m.Name)
    | RestoreDefault ->
        restoreDefaultCalcLib
    
    ()