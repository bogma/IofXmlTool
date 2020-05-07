module CommandHandlers

open System.IO

open Types
open Argu
open Commands
open IofXmlLib.Helper
open IofXmlLib.CalcLibBuilder

let info (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let Config = XmlConfig.Load(Path.Combine(cArgs.wDir, cArgs.cfgFile))

    let competitions = 
        match Config.General.Type with
        | "Team" -> Seq.empty
        | "Cup" -> getFiles cArgs.wDir ((Config.General.ResultFilePrefix) + "*_*.xml") Config.General.RecurseSubDirs
        | _ -> Seq.empty

    printfn "Type:\t%s" Config.General.Type

    printfn "Input files matching filter (%s)" Config.General.ResultFilePrefix
    competitions |> Seq.iter (printfn "\t%s")
    ()

let newProject (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()

let add (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()

let rules (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    buildNewCalcLib
    ignore()

