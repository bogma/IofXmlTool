namespace IofXmlLib

open System
open System.IO
open System.Reflection
open FSharp.Compiler.SourceCodeServices

open Types
open Logging

module CalcLibBuilder =

    let part0 = """
namespace IofXmlLib

module Calc =

    type Rule = {
        Name : string
        Formatting : string
        Code : string
    }

    type CalculationRule(calcFunction, format, ruleDefinition) =
        member this.Execute (winningTime:decimal) (runnersTime:decimal) (racePosition:int) = calcFunction winningTime runnersTime racePosition
        member this.FormatPoints (points:decimal) = sprintf format points
        member this.RuleName = ruleDefinition.Name
        member this.RuleFormat = ruleDefinition.Formatting
        member this.RuleCode = ruleDefinition.Code
"""

    let t1 = """
    let sumRule _ (time:decimal) _ =
        time

{0}
"""

    let t2 = """
    let getCalcStrategy ruleName =
        match ruleName with
{0}        | "sum" -> Some (CalculationRule(sumRule, "%.2f", ~~~ Name = "sum"; Formatting = "%.2f"; Code = "let sum _ time _ = time" ~~~))
        | _ -> None
"""

    let t3 = """
    let getNames _ =
        [{0}"sum"]
"""
    let readCalcRuleDefinitions files =
        files 
        |> List.collect(fun ruleFile ->
            if not(File.Exists(ruleFile)) then
                ["", "", ""]
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
                        let code = x.Value.Replace("    ", "        ")
                        sprintf "        | \"%s\" -> Some (CalculationRule(%s, \"%s\", ~~~ Name = \"%s\"; Formatting = \"%s\"; Code = \"\"\"%s\"\"\" ~~~))\n" x.Name fName x.Formatting x.Name x.Formatting code)
                    |> Array.fold (+) ""
                let n =
                    additionalRules.Rules
                    |> Array.map (fun x -> sprintf "\"%s\"; " x.Name)
                    |> Array.fold (+) ""
                [f, m, n])

    let buildCalcLib ruleFile =
        tracer.Info "compiling rules from %s" ruleFile
        let checker = FSharpChecker.Create()
        let p = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let fn = Path.Combine(p, "CalcLib")
        let fn2 = Path.ChangeExtension(fn, ".fsx")
        let fn3 = Path.ChangeExtension(fn, ".dll")
        let fn4 = Path.ChangeExtension(fn, ".bak")

        let additionalFunctions, additionalMatches, additionalRuleNames = readCalcRuleDefinitions [ruleFile] |> List.head

        let part1 = System.String.Format(t1, additionalFunctions)
        let part2 = System.String.Format(t2, additionalMatches)
        let part3 = System.String.Format(t3, additionalRuleNames)

        let code = part0 + part1 + part2.Replace(" ~~~ ", " { ").Replace("~~~", "   }") + part3
        tracer.Info "%s" code
        File.WriteAllText(fn2, code)

        File.Delete fn4
        File.Move(fn3, fn4)

        let errors1, exitCode1 =
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
    

