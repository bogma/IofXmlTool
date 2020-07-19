namespace IofXmlLib

open System
open System.IO
open System.Reflection
open FSharp.Compiler.SourceCodeServices

open Types
open Logging

module CalcLibBuilder =

    let template = """
namespace IofXmlLib

module Calc =

    type CalculationRule(calcFunction, format) =
        member this.Execute (winningTime:decimal) (runnersTime:decimal) (racePosition:int) = calcFunction winningTime runnersTime racePosition
        member this.FormatPoints (points:decimal) = sprintf format points

    let sumRule _ (time:decimal) _ =
        time

{0}

    let getCalcStrategy ruleName =
        match ruleName with
{1}        | _ -> CalculationRule(sumRule, "%.2f")
"""

    let readCalcRuleDefinitions files =
        files 
        |> List.collect(fun ruleFile ->
            if not(File.Exists(ruleFile)) then
                ["", ""]
            else
                let additionalRules = XmlRules.Load(ruleFile)

                let f =
                    additionalRules.Rules 
                    |> Array.map (fun x -> sprintf "%s\n" x.Value)
                    |> Array.fold (+) ""
                let m =
                    additionalRules.Rules
                    |> Array.map (fun x ->
                        let fName = x.Value.Split(" ", StringSplitOptions.RemoveEmptyEntries).[2]
                        sprintf "        | \"%s\" -> CalculationRule(%s, \"%s\")\n" x.Name fName x.Formatting)
                    |> Array.fold (+) ""

                [f, m])

    let buildCalcLib ruleFile =
        tracer.Info "compiling rules from %s" ruleFile
        let checker = FSharpChecker.Create()
        let p = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let fn = Path.Combine(p, "CalcLib")
        let fn2 = Path.ChangeExtension(fn, ".fsx")
        let fn3 = Path.ChangeExtension(fn, ".dll")
        let fn4 = Path.ChangeExtension(fn, ".bak")
        File.Delete fn4
        File.Move(fn3, fn4)
        let errors1, exitCode1 =
            let additionalFunctions, additionalMatches = readCalcRuleDefinitions [ruleFile] |> List.head
            let code = System.String.Format(template, additionalFunctions, additionalMatches)
            tracer.Info "%s" code
            File.WriteAllText(fn2, code)
            checker.Compile([| "fsc.exe"; "-o"; fn3; "-a"; fn2; |])
            |> Async.RunSynchronously
        match errors1, exitCode1 with
        | [| |], 0 ->
            tracer.Warn "Compilation successful - new calculation rules available"
        | _ -> 
            tracer.Error "Compilation failed - Exit code %d\n%A" exitCode1 errors1
            File.Delete fn2
            File.Delete fn3
            File.Move(fn4, fn3)
            File.Delete fn4
    
    let readResource (resourceName:string) =
        match resourceName.Split(',') with
        | [| _; name |] ->
            let asm = Assembly.GetExecutingAssembly()
            use sr = new StreamReader(asm.GetManifestResourceStream(name.Trim()))
            Some(sr.ReadToEnd())
        | _ -> None

    let restoreDefaultCalcLib =
        let tmp = Path.GetTempFileName()
        File.WriteAllText(tmp, (readResource "IofXmlLib, IofXmlLib.calculation_rules.xml" |> Option.defaultValue ""))
        buildCalcLib tmp
        File.Delete tmp
    

