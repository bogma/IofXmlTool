open System.IO
open System.Xml
open System.Xml.Linq
open CupTypes
open Helper
open Parsing
open Result
open FSharp.Data

let flatten x =
    [for event, c in x do
        for category, prrSeq in c do
           for prr in prrSeq do
               yield event, category, prr]

let calcPoints pos =
    match pos with
    | 1 -> 25
    | 2 -> 20
    | 3 -> 16
    | 4 -> 13
    | 5 -> 11
    | 6 -> 10
    | 7 -> 9
    | 8 -> 8
    | 9 -> 7
    | 10 -> 6
    | 11 -> 5
    | 12 -> 4
    | 13 -> 3
    | 14 -> 2
    | 15 -> 1
    | _ -> 0

let buildEventResult (inputFile : string) =
    let clCfg = XmlConfig.GetSample().Classes |> Array.toList |> List.map (fun x -> x.Id)
    let orgCfg = XmlConfig.GetSample().Organisations  |> Array.toList |> List.map (fun x -> x.Id)
    let fileName = Path.GetFileNameWithoutExtension(inputFile)
    let eventMultiplier = 
        let s = fileName.Substring(2, 2).AsInteger()
        if XmlConfig.GetSample().Cup.Event.Num = s then XmlConfig.GetSample().Cup.Event.Multiply
        else 1

    let calcSingleResult (item : ParsedResult) i =
        { 
            OrganisationId = item.OrganisationId;
            Name = item.GivenName + " " + item.FamilyName;
            Points = calcPoints i * eventMultiplier;
            Time = item.Time;
            Position = i;
        }
    
    let r = parseResultXml inputFile
                |> List.filter (fun a -> List.exists (fun org -> org = a.OrganisationId) orgCfg)
                |> List.filter (fun a -> List.exists (fun cl -> cl = a.ClassId) clCfg)
                |> List.filter (fun a -> a.Status = "OK")
                |> Seq.groupBy (fun i -> i.ClassId)
                |> Seq.map (fun pair ->
                                let clId = fst pair
                                let clRes = snd pair
                                let timeGroupedRes = clRes 
                                                        |> Seq.sortBy (fun x -> x.Time)
                                                        |> Seq.groupBy (fun x -> x.Time)
                                let cupPositions = getPositionSeq 1 (getIntervalList timeGroupedRes)
                                let res = (cupPositions, timeGroupedRes) 
                                                ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> calcSingleResult item i1))
                                clId, flattenSeqOfSeq res)
    fileName, r

[<EntryPoint>]
let main argv =

    let inputPath = argv.[0]

    let takeBest = XmlConfig.GetSample().Cup.TakeBest
    let maxEvents = XmlConfig.GetSample().Cup.NumberOfEvents

    let competitions =
        getFiles inputPath "*SC*.xml"
        //getFiles inputPath "*SC*.csv"

    let results =
        competitions
        |> Seq.map buildEventResult
        |> flatten
        |> Seq.map (fun (event, category, prr) -> (event, category, prr, category.ToString() + "~" + prr.Name))
        |> Seq.groupBy (fun (_, _, _, gb) -> gb)
        |> Seq.map (fun pair ->
                        let r = snd pair
                                    |> Seq.map (fun (a, b, c, _) -> (a, b, c))
                        let _, catId, prr =
                            r
                            |> Seq.take 1
                            |> Seq.exactlyOne
                        let countingResults =
                            r
                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                            |> Seq.truncate takeBest

                        let sum = 
                            countingResults
                            |> Seq.sumBy (fun (_, _, prr) -> prr.Points)
                        prr.Name, catId, prr.OrganisationId, sum, r)

    let catResults =
        results
        |> Seq.groupBy (fun (_, catId, _, _, _) -> catId)

    let outputFileName = "cup_" + XmlConfig.GetSample().Cup.Year.ToString() + ".html"
    let outputFile = Path.Combine(inputPath, outputFileName) 
    File.WriteAllText(outputFile,  buildResultHtml catResults)
    File.Copy("./resources/default.css", Path.Combine(inputPath, "default.css"), true)

    printf "output written to %s" outputFile

    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
