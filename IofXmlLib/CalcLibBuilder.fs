namespace IofXmlLib

open System
open System.IO
open System.Reflection
open FSharp.Compiler.SourceCodeServices

open Types

module CalcLibBuilder =


    let template = """
namespace IofXmlLib

module Calc =

    type CalculationRule(calcFunction, format) =
        member this.Execute (winningTime:decimal) (runnersTime:decimal) (racePosition:int) = calcFunction winningTime runnersTime racePosition
        member this.FormatPoints (points:decimal) = sprintf format points

    let sumRule _ time _ =
        time

{0}

    let getCalcStrategy ruleName =
        match ruleName with
{1}        | _ -> CalculationRule(sumRule, "%.2f")
"""

    let additionalFunctions, additionalMatches = 
        if not(File.Exists("./calc_rules.xml")) then
            "", ""
        else
            let additionalRules = XmlRules.Load("./calc_rules.xml")

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

            f, m

    let code = System.String.Format(template, additionalFunctions, additionalMatches)
    let checker = FSharpChecker.Create()

    let p = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
    let fn = Path.Combine(p, "CalcLib")
    let fn2 = Path.ChangeExtension(fn, ".fsx")
    let fn3 = Path.ChangeExtension(fn, ".dll")

    let buildNewCalcLib = 
        let errors1, exitCode1 =
            printfn "%s %s" fn2 fn3
            File.WriteAllText(fn2, code)
            checker.Compile([| "fsc.exe"; "-o"; fn3; "-a"; fn2; |])
            |> Async.RunSynchronously
        match errors1, exitCode1 with
        | [| |], 0 ->
            printfn "Compilation successful - new calculation rules available"
        | _ -> 
            printfn "Compilation failed - Exit code %d\n%A" exitCode1 errors1
            File.Delete fn2
            File.Delete fn3
            