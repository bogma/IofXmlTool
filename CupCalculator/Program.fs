open System
open System.IO
open System.Xml
open System.Xml.Linq
open CupTypes
open Calc
open Helper
open Parsing
open ProgramSettings
open HtmlOutput
open PdfOutput
open FSharp.Data

let flatten x =
    [for event, c in x do
        for category, prrSeq in c do
           for prr in prrSeq do
               yield event, category, prr]

let buildEventResult (inputFile : string) =
    
    let fileName = Path.GetFileNameWithoutExtension(inputFile)
    let eventMultiplier = 
        let p = fileName.IndexOf("_")
        let s = fileName.Substring(p-2, 2).AsInteger()
        if !eventProps |> List.exists (fun x -> x.Num = s) then !eventProps |> List.filter (fun x -> x.Num = s) |> List.map (fun x -> x.Multiply) |> List.head
        else 1.0m

    let calcSingleResult winningTime (item : ParsedResult) i =
        let strategy = getCalcStrategy !calcRule
        let points = strategy.Execute winningTime (decimal item.Time) i * eventMultiplier
        { 
            OrganisationId = item.OrganisationId;
            Name = item.GivenName + " " + item.FamilyName;
            Points = Math.Round(points, 2);
            Time = item.Time;
            Position = i;
        }
    
    let r = parseResultXml inputFile
                |> List.filter (fun a -> List.exists (fun org -> org = a.OrganisationId) !orgCfgIds)
                |> List.filter (fun a -> List.exists (fun cl -> cl = a.ClassId) !classCfgIds)
                |> List.filter (fun a -> a.Status = "OK")
                |> Seq.groupBy (fun i -> i.ClassId)
                |> Seq.map (fun pair ->
                                let clId = fst pair
                                let clRes = snd pair
                                let timeGroupedRes = clRes 
                                                        |> Seq.sortBy (fun x -> x.Time)
                                                        |> Seq.groupBy (fun x -> x.Time)
                                let winningTime, _ = timeGroupedRes |> Seq.head
                                let cupPositions = getPositionSeq 1 (getIntervalList timeGroupedRes)
                                let res = (cupPositions, timeGroupedRes) 
                                                ||> Seq.map2 (fun i1 i2 -> snd i2 
                                                                            |> Seq.map (fun item -> calcSingleResult (decimal winningTime) item i1))
                                clId, flattenSeqOfSeq res)
    fileName, r

[<EntryPoint>]
let main argv =

    let inputPath = argv.[0]
    let configFile =
        if argv.Length <= 1 || argv.[1] = "" then "config.xml"
        else argv.[1]

    let Config = XmlConfig.Load(Path.Combine(inputPath, configFile))
    cupName := Config.Cup.Name 
    takeBest := Config.Cup.TakeBest
    maxEvents := Config.Cup.NumberOfEvents
    classCfg := Config.Classes  |> Array.toList
    classCfgIds := Config.Classes |> Array.toList |> List.map (fun x -> x.Id)
    orgCfg := Config.Organisations  |> Array.toList
    orgCfgIds := Config.Organisations  |> Array.toList |> List.map (fun x -> x.Id)
    eventProps := Config.Cup.Events |> Array.toList
    year := Config.Cup.Year
    calcRule := Config.Cup.CalcRule

    let competitions = getFiles inputPath "*_*.xml" false

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
                            |> Seq.truncate !takeBest
                        let x =
                            r
                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                            |> Seq.mapi (fun i (a, b, c) -> 
                                            let counts =
                                                if i < !takeBest then true
                                                else false
                                            (a, b, c, counts))
                        let sum = 
                            countingResults
                            |> Seq.sumBy (fun (_, _, prr) -> prr.Points)
                        prr.Name, catId, prr.OrganisationId, sum, x)

    let catResults =
        results
        |> Seq.groupBy (fun (_, catId, _, _, _) -> catId)

    for cr in catResults do
        let catId = fst cr
        printfn "checking names for category %d" catId
        let cs = snd cr
                    |> Seq.map( fun (name, _,_,_,_) -> name)
                    |> Seq.toList
                    |> comb 2
                    |> List.map (fun x -> x.[0], x.[1], levDist x.[0] x.[1])
                    |> List.filter (fun (a, b, x) -> x <= 3)
    
        if cs.Length > 0 then
            printfn "%A" cs

//    let outputFileName = "cup_" + (!year).ToString() + ".html"
//    let outputFile = Path.Combine(inputPath, outputFileName) 
//    buildResultHtml catResults outputFile |> ignore

    let outputFileName = "cup_" + (!year).ToString() + ".pdf"
    let outputFile = Path.Combine(inputPath, outputFileName)   
    buildResultPdf catResults outputFile|> ignore

    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
