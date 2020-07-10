module Helper

open IofXmlLib.Types
open IofXmlLib.Helper
open Types
open System.IO
open System.Reflection
open System.Text.RegularExpressions

let levenshteinCheck (data:ResultData) =

    let classResults = match data.Result with
                        | CupResult cr ->
                            cr
                            |> Seq.groupBy (fun cupResult -> cupResult.ClassId.Value)
                            |> Seq.map (fun (x,y) -> 
                                let cl = XmlResult.Id (None, string x)
                                let s = y |> Seq.map (fun x -> x.PersonName)
                                cl, s)
                        | SumResult sr ->
                            sr
                            |> Seq.groupBy (fun cupResult -> cupResult.ClassId.Value)
                            |> Seq.map (fun (x,y) ->
                                let cl = XmlResult.Id (None, string x)
                                let s = y |> Seq.map (fun x -> x.PersonName)
                                cl, s)
                        | _ -> Seq.empty

    printfn "checking names with Levenshtein-distance <= 3"

    for cr in classResults do
        let catId = fst cr
        let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" catId
        if cShort.Length > 0 then
            printf "\tclass %s (%s)" cName cShort
        else
            printf "\tclass %s" cName
        let cs = snd cr
                //|> Seq.map( fun cupResult -> cupResult.PersonName)
                |> Seq.toList
                |> comb 2
                |> List.map (fun x -> x.[0], x.[1], levDist x.[0] x.[1])
                |> List.filter (fun (a, b, x) -> x <= 3)

        if cs.Length > 0 then
            printfn "\t%A" cs
        else
            printfn "\tok"
    ()

let checkNameTypos (data:ResultData) = 
    match data.Result with
    | TeamResult _ -> printfn "no typo check on Team"
    | SumResult _ | CupResult _ -> levenshteinCheck data |> ignore

let tryLocateFile (wDir:string) (f:string) =
    // check if we have rooted path
    if Path.IsPathRooted(f) && File.Exists(f) then
        Some f
    else
        // we have rel path
        // first try working dir
        let fn = Path.Combine(wDir, f)
        printfn "testing %s" fn
        if File.Exists(fn) then
            Some fn
        else
            // check program dir
            let p = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
            let fn = Path.Combine(p, f)
            printfn "testing %s" fn
            if File.Exists(fn) then
                Some fn
            else
                None

let getEventInfos (config:XmlConfig.Configuration) dir =
    let races = [1..config.General.NumberOfEvents]
    let matchingFiles = getFiles dir config.General.ResultFileRegex "*.xml" config.General.RecurseSubDirs
    let matchingEvents = matchingFiles |> Seq.choose (fun x -> 
                                                          let fn = Path.GetFileNameWithoutExtension(x)
                                                          match fn with
                                                          | Regex config.General.ResultFileRegex [evNum] -> Some (int evNum, x)
                                                          | _ -> None)
    
    printfn "events matching the regex: %A" matchingEvents

    races |> List.map (fun i ->
                            let evCfg = config.Events |> Array.tryFind (fun e -> e.Num = i)
                            let ev = match evCfg with
                                     | Some ev ->
                                        let fName = tryLocateFile dir ev.FileName
                                        let _, evFile = matchingEvents |> Seq.tryFind (fun (n, x) -> n = i) |> Option.defaultValue (i, ev.FileName)
                                        
                                        {
                                            FileName = fName |> Option.defaultValue evFile;
                                            Name = ev.Name |> Option.defaultValue "" ;
                                            Date = ev.Date |> Option.defaultValue "";
                                            Number = i;
                                            Multiply = ev.Multiply |> Option.defaultValue 1.0m;
                                            Rule = ev.CalcRule
                                        }
                                     | None ->
                                        let _, evFile = matchingEvents |> Seq.tryFind (fun (n, x) -> n = i) |> Option.defaultValue (i, "")
                                        {FileName = evFile; Name=""; Date = ""; Number = i; Multiply = 1.0m; Rule = None}
                            ev)