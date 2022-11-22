module CommandHandlers

open System.IO
open System.Xml.Schema

open IofXmlLib.Logging

open Types
open Argu
open Helper
open Commands

let info (cArgs:CommonArgs) (args : ParseResults<_>) =

    let config = XmlConfig.Load(Path.Combine(cArgs.wDir, cArgs.cfgFile))
    try
        let schema = XmlConfig.GetSchema()

        config.XElement.Document.Validate(schema, validationEventHandler = null)
    with e ->
           tracer.ErrorException e "Configuration validation failed: %s" e.Message

    let resultFileRegex = config.General.ResultFileRegex |> Option.defaultValue ""

    tracer.Info "Type:\t%s" config.Type

    match config.Type with
    | "Cup" | "Sum" ->
        let events = getEventInfos config cArgs.wDir |> List.toSeq
        tracer.Info "Input files matching filter (%s)" resultFileRegex
        events |> Seq.iter (tracer.Info "\t%A")
    | _ -> tracer.Info "no input files"

    ()
