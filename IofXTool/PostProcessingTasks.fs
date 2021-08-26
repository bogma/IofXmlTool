module PostProcessingTasks

open IofXmlLib.Types
open IofXmlLib.Helper
open IofXmlLib.Calc
open IofXmlLib.Logging

open Helper
open Types

open Fue.Compiler
open Fue.Data
open FSharp.Data

open System
open System.Collections.Generic
open System.IO

type ClubSumCsvType = CsvProvider<Schema = "Rank (int), Club (string), Points (decimal), Competitors (int)", HasHeaders=false>

type OrganisationSumResult = {
    Position: int
    Name : string
    Points : decimal
    Competitors: int
}

let sumOrganisations (taskParams : IDictionary<string,string>) data =

    let recalcPositions (res : seq<OrganisationSumResult>) = 
        let totalGrouped = res
                            |> Seq.sortBy (fun x -> -x.Points)
                            |> Seq.groupBy (fun x -> x.Points)
        let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

        (totalPositions, totalGrouped)
                   ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                   |> flattenSeqOfSeq

    let buildCsvRow obj = ClubSumCsvType.Row(obj.Position, obj.Name, obj.Points, obj.Competitors)
    let buildCsvTable = (Seq.map buildCsvRow) >> Seq.toList >> ClubSumCsvType

    match data.Result with
        | CupResult cr ->
            let fileName = taskParams.["outputPrefix"] + data.Config.Output.Html.FileName
            let outputFile = Path.Combine(data.InputPath, fileName)
            let res = cr
                    |> Seq.groupBy (fun r -> r.OrganisationId.Value)
                    |> Seq.map(fun (_, results) ->
                        let org = (results |> Seq.take 1 |> Seq.exactlyOne).OrganisationId
                        let orgName, _= getNamesById data.OrgCfg data.OrgInfo "Unknown Club" org
                        let sum =
                            match taskParams.["sumType"] with
                            | "cup" ->
                                results |> Seq.sumBy(fun r -> r.TotalPoints)
                            | "all" ->
                                results |> Seq.sumBy(fun r -> r.Results |> Seq.sumBy(fun evtRes -> evtRes.PRR.Points))
                            | _ -> 0m
                        let competitors = results |> Seq.length
                        { Position = 0; Name = orgName; Points = sum; Competitors = competitors })
            let res = recalcPositions res
                    |> Seq.map (fun (p, x) -> { x with Position = p })
            let clubSumCsv = res |> buildCsvTable
            clubSumCsv.Save(outputFile, ',', '"')
            tracer.Info "post processing task 'sumOrganisations': CSV output written to %s" outputFile
        | TeamResult _ | SumResult _ ->
            tracer.Info "'sumOrganisations' not implemented for this result type."

