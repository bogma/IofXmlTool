module CommandHandlers

open System.IO

open IofXmlLib.Logging

open Types
open Argu
open Helper
open Commands

let info (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let Config = XmlConfig.Load(Path.Combine(cArgs.wDir, cArgs.cfgFile))
    let resultFileRegex = Config.General.ResultFileRegex |> Option.defaultValue ""
    
    tracer.Info "Type:\t%s" Config.Type

    match Config.Type with
    | "Cup" | "Sum" ->
        let events = getEventInfos Config cArgs.wDir |> List.toSeq
        tracer.Info "Input files matching filter (%s)" resultFileRegex
        events |> Seq.iter (tracer.Info "\t%A")
    | _ -> tracer.Info "no input files"

    ()

let newProject (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()

let add (cArgs:CommonArgs) (args : ParseResults<_>) =

    ()
