open System.IO
open System.Xml
open System.Xml.Linq
open CupTypes
open Helper
open Parsing
open Result
open FSharp.Data

let rec getFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getFiles d pattern }

let flatten x =
    [for event, c in x do
        for category, r in c do
            for pr1, pr2, pr3, pr4, pr5 in r do
                yield event, category, pr1, pr2, pr3, pr4, pr5]

let flattenSeqOfSeq outer =
    seq { for inner in outer do
             for s in inner do
                yield s }

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
        (item.OrganisationId, item.GivenName + " " + item.FamilyName, calcPoints i * eventMultiplier, item.Time, i)
    
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

    let takeBest = XmlConfig.GetSample().Cup.TakeBest
    let maxEvents = XmlConfig.GetSample().Cup.NumberOfEvents

    let competitions =
        getFiles "../../../../ol/data" "*SC*.xml"
        //getFiles "../../../../ol/data" "*SC*.csv"

    let results =
        competitions
        |> Seq.map buildEventResult
        |> flatten
        |> Seq.map (fun (a, catId, c, name, e, f, g) -> (a, catId, c, name, e, f, g, catId.ToString() + "~" + name))
        |> Seq.groupBy (fun (_, _, _, name, _, _, _, gb) -> gb)
        |> Seq.map (fun pair ->
                        let r = snd pair
                                    |> Seq.map (fun (a, b, c, d, e, f, g, _) -> (a, b, c, d, e, f, g))
                        let _, catId, orgId, name, _, _, _, _ =
                            snd pair
                            |> Seq.take 1
                            |> Seq.exactlyOne
                        let s = 
                            snd pair
                            |> Seq.sortBy (fun (_, _, _, _, points, _, _, _) -> -points)
                            |> Seq.truncate takeBest
                            |> Seq.sumBy (fun (_, _, _, _, points, _, _, _) -> points)
                        name, catId, orgId, s, r)

    //results |> Seq.iter (printfn "%A")

    let catResults =
        results
        |> Seq.groupBy (fun (_, i, _, _, _) -> i)

    let cs = catResults 
                |> Seq.map (fun pair -> snd pair)
                |> flattenSeqOfSeq
                |> Seq.map( fun (name, _,_,_,_) -> name)
                |> Seq.toList
                |> comb 2
                |> List.map (fun x -> x.[0], x.[1], levDist x.[0] x.[1])
                |> List.filter (fun (a, b, x) -> x <= 3)
    
    

    printf "%A" cs
    printf "length: %d" cs.Length

    //let outputFile = "../../../../ol/cup_" + XmlConfig.GetSample().Cup.Year.ToString() + ".html"
    //File.WriteAllText(outputFile,  buildResultHtml catResults)

    //printf "output written to %s" outputFile

    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
