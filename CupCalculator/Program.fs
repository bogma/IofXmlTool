open System
open System.IO
open System.Xml
open System.Xml.Linq
open CupTypes
open Helper
open Parsing
open ProgramSettings
open Result
open FSharp.Data

let calcPointsFromPosition winningTime time pos =
    match pos with
    | 1 -> 25m
    | 2 -> 20m
    | 3 -> 16m
    | 4 -> 13m
    | 5 -> 11m
    | 6 -> 10m
    | 7 -> 9m
    | 8 -> 8m
    | 9 -> 7m
    | 10 -> 6m
    | 11 -> 5m
    | 12 -> 4m
    | 13 -> 3m
    | 14 -> 2m
    | 15 -> 1m
    | _ -> 0m

let calcPointsFromTime winningTime time pos =
    let p = 100m - (((time - winningTime) / winningTime) * 50m)
    if p < 0m then 0m
    else p

type CalculationRule(calcFunction) = 
    member this.CalcFunction with get() = calcFunction    
    member this.Execute wt rt pos = calcFunction wt rt pos

let getCalcStrategy calcRule =
    match calcRule with
        | "calcPointsFromTime" -> CalculationRule(calcPointsFromTime)
        | "calcPointsFromPosition" -> CalculationRule(calcPointsFromPosition)
        | _ -> CalculationRule(calcPointsFromTime)


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

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let levDist (strOne : string) (strTwo : string) =
    let strOne = strOne.ToCharArray ()
    let strTwo = strTwo.ToCharArray ()
 
    let (distArray : int[,]) = Array2D.zeroCreate (strOne.Length + 1) (strTwo.Length + 1)
 
    for i = 0 to strOne.Length do distArray.[i, 0] <- i
    for j = 0 to strTwo.Length do distArray.[0, j] <- j
 
    for j = 1 to strTwo.Length do
        for i = 1 to strOne.Length do
            if strOne.[i - 1] = strTwo.[j - 1] then distArray.[i, j] <- distArray.[i - 1, j - 1]
            else
                distArray.[i, j] <- List.min (
                    [distArray.[i-1, j] + 1; 
                    distArray.[i, j-1] + 1; 
                    distArray.[i-1, j-1] + 1]
                )
    distArray.[strOne.Length, strTwo.Length]

[<EntryPoint>]
let main argv =

    let inputPath = argv.[0]
    let configFile = argv.[1]

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

    let cs = catResults 
                |> Seq.map (fun pair -> snd pair)
                |> flattenSeqOfSeq
                |> Seq.map( fun (name, _,_,_,_) -> name)
                |> Seq.toList
                |> comb 2
                |> List.map (fun x -> x.[0], x.[1], levDist x.[0] x.[1])
                |> List.filter (fun (a, b, x) -> x <= 3)
    
    
    let outputFileName = "cup_" + (!year).ToString() + ".html"
    let outputFile = Path.Combine(inputPath, outputFileName) 
    File.WriteAllText(outputFile,  buildResultHtml catResults)
    File.Copy("./resources/default.css", Path.Combine(inputPath, "default.css"), true)

    printf "%A" cs
    printf "length: %d" cs.Length

    //let outputFile = "../../../../ol/cup_" + XmlConfig.GetSample().Cup.Year.ToString() + ".html"
    //File.WriteAllText(outputFile,  buildResultHtml catResults)

    //printf "output written to %s" outputFile

    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
