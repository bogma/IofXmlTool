module RuleCmdHandler

open System.Reflection

open IofXmlLib.Calc
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
        let infos = getNames None
        infos |> List.iter (fun x -> 
            match getCalcStrategy x with
            | Some s ->
                let n = s.RuleName
                let fm = s.RuleFormat
                tracer.Info "Name: %s, Format: %s" n fm
            | None ->
                tracer.Info "No info for '%s' found in CalcLib." x)
    | ListDetails ->
        let infos = getNames None
        infos |> List.iter (fun x -> 
            match getCalcStrategy x with
            | Some s ->
                let n = s.RuleName
                let fm = s.RuleFormat
                tracer.Info "Name: %s, Format: %s" n fm
                tracer.Info "%s" s.RuleCode
            | None ->
                tracer.Info "No info for '%s' found in CalcLib." x)
    | ListFunctions ->
        let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
        let asm = Assembly.Load("CalcLib")
        let calc = asm.GetType("IofXmlLib.Calc")
        let methods = calc.GetMethods(staticFlags)
        methods 
        |> Array.filter (fun m -> not (m.Name.StartsWith("get")))
        |> Array.iter (fun m -> tracer.Info "%s" m.Name)
    | RestoreDefault ->
        restoreDefaultCalcLib

    ()