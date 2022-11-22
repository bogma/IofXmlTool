module PostProcessingTasks

open IofXmlLib.Types
open IofXmlLib.Helper
open IofXmlLib.Logging

open Helper
open Types

open Fue.Compiler
open Fue.Data
open FSharp.Data

open System.Collections.Generic
open System.IO

type ClubSumCsvType = CsvProvider<Schema = "Rank (int), Club (string), Points (decimal), Competitors (int), Competitions (int), ValidComptetitions (int)", HasHeaders=false>

type OrganisationSumResult = {
    Position: int
    Name : string
    Points : decimal
    Competitors: int
    Competitions: int
    ValidCompetitions: int
}

let sumOrganisations (taskParams : IDictionary<string,string>) data =

    let isSameOrg (y : CupResult) x = x.Equals y.OrganisationId.Value
    let isSameClass (y : CupResult) x = x.Equals y.ClassId.Value

    let filter fType filter comparer (input : CupResult seq) =
        match fType with
        | IncludeFilter -> input |> Seq.filter (fun a -> List.exists (comparer a) filter)
        | ExcludeFilter -> input |> Seq.filter (fun a -> List.exists (comparer a) filter |> not)
        | _ -> input

    let recalcPositions (res : seq<OrganisationSumResult>) = 
        let totalGrouped = res
                            |> Seq.sortBy (fun x -> -x.Points)
                            |> Seq.groupBy (fun x -> x.Points)
        let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

        (totalPositions, totalGrouped)
                   ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                   |> flattenSeqOfSeq

    let buildCsvRow obj = ClubSumCsvType.Row(obj.Position, obj.Name, obj.Points, obj.Competitors, obj.Competitions, obj.ValidCompetitions)
    let buildCsvTable = (Seq.map buildCsvRow) >> Seq.toList >> ClubSumCsvType
    
    let filterKind = getTaskParam taskParams "filterKind" ""
    let filterType = getTaskParam taskParams "filterType" ""
    let filterVal = (getTaskParam taskParams "filterValue" "").Split ',' |> Array.toList

    let multipliers = (getTaskParam taskParams "classMultipliers" "").Split '|' 
                        |> Array.map(fun m ->
                            try
                                let cfg = m.Split ':'
                                let c = cfg.[0]
                                let f = cfg.[1]
                                string c, decimal f
                            with
                            | _ ->
                                tracer.Warn "Class multpliers could not be parsed - using none"
                                "0", 0.0m)
 
    let getClassMultiplier id =
        let factor = multipliers |> Array.tryFind(fun (c, m) -> c = id)
        match factor with
        | Some (c, f) -> f
        | None -> 1.0m

    match data.Result with
        | CupResult cr ->
            let fileName = (getTaskParam taskParams "outputPrefix" "") + data.Config.Output.Html.FileName
            let outputFile = Path.Combine(data.InputPath, fileName)
            let fres =
                match filterKind with
                | ClassKind -> cr |> filter filterType filterVal isSameClass
                | OrganisationKind -> cr |> filter filterType filterVal isSameOrg
                | _ -> cr

            let res = fres
                    |> Seq.groupBy (fun r -> r.OrganisationId.Value)
                    |> Seq.map(fun (_, results) ->
                        let org = (results |> Seq.take 1 |> Seq.exactlyOne).OrganisationId
                        let orgName, _= getNamesById data.OrgCfg data.OrgInfo "Unknown Club" org
                        let sum =
                            match (getTaskParam taskParams "sumType" "") with
                            | "cup" ->
                                results |> Seq.sumBy(fun r -> r.TotalPoints)
                            | "all" ->
                                results |> Seq.sumBy(fun r -> r.Results 
                                                              |> Seq.sumBy(fun evtRes ->
                                                                            let m = getClassMultiplier evtRes.ClassId.Value
                                                                            m * evtRes.PRR.Points))
                            | _ -> 0m
                        let competitors = results |> Seq.length
                        let competitions = results |> Seq.sumBy(fun r -> r.Results |> Seq.length)
                        let validCompetitions = results |> Seq.sumBy(fun r -> r.Results |> Seq.filter(fun x -> x.PRR.Status = "OK") |> Seq.length)
                        { Position = 0; Name = orgName; Points = sum; Competitors = competitors; Competitions = competitions; ValidCompetitions = validCompetitions })
            let res = recalcPositions res
                    |> Seq.map (fun (p, x) -> { x with Position = p })
            let clubSumCsv = res |> buildCsvTable
            clubSumCsv.Save(outputFile, ',', '"')
            tracer.Info "post processing task 'sumOrganisations': CSV output written to %s" outputFile
        | TeamResult _ | SumResult _ ->
            tracer.Info "'sumOrganisations' not implemented for this result type."

