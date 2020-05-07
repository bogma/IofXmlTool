module BuildHandler

open System
open System.IO
open System.Text

open FSharp.Json
open Argu

open IofXmlLib.Types
open IofXmlLib.Calc
open IofXmlLib.Helper
open IofXmlLib.Parsing

open Types
open Helper
open Commands
open HtmlOutput
open FSharp.Data

let build (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let inputFiles = args.TryGetResult(Files)
    let inputFile = match inputFiles with 
                        | Some f -> f.[1]
                        | None -> ""

    let Config = XmlConfig.Load(Path.Combine(cArgs.wDir, cArgs.cfgFile))

    let classCfg = Config.Classes.Classes |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = x.DiplayName })
    let orgCfg = Config.Organisations.Organisations |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = "" })

    let virtualClasses = Config.Classes.Combines
                        |> Array.toList
                        |> List.map (fun x -> 
                        { 
                            Id = XmlResult.Id(None, x.ClassIds)
                            Name = x.Name; 
                            ShortName = x.DiplayName;
                            Classes = x.ClassIds.Split[|','|]
                                      |> Array.toList
                                      |> List.map (fun y -> y.Trim())
                                      |> List.map (fun y -> XmlResult.Id(None, y))})

    let file = Path.Combine(cArgs.wDir, inputFile)

    let setName (name:string) =
        let names = Config.Names
        let n = names |> Array.filter (fun x -> x.AliasFor.Contains(name))
        if (n.Length > 0) then
            n.[0].Name
        else
            name

    let buildTeam (e1 : ParsedResult, e2 : ParsedResult, e3 : ParsedResult) =
        let sum = e1.Time + e2.Time + e3.Time
        let ne1 = { Name = setName (e1.GivenName + " " + e1.FamilyName); Time = e1.Time } 
        let ne2 = { Name = setName (e2.GivenName + " " + e2.FamilyName); Time = e2.Time }
        let ne3 = { Name = setName (e3.GivenName + " " + e3.FamilyName); Time = e3.Time }
        { OrganisationId = e1.OrganisationId; TotalTime = sum; TeamMembers = [ne1; ne2; ne3] }

    let combineClasses (cl : XmlResult.Id) =
        let vc = virtualClasses |> List.tryFind (fun a -> a.Classes |> List.exists (isSame cl))
        match vc with
        | Some c -> c.Id
        | None -> cl

    let buildEventResult (inputFile : string) =
        
        let fileName = Path.GetFileNameWithoutExtension(inputFile)
        let eventMultiplier = 
            let p = fileName.IndexOf("_")
            let s = fileName.Substring(p-2, 2).AsInteger()
            let eventList = Config.General.Events |> Array.toList
            let exists = eventList |> List.exists (fun x -> x.Num = s)
            if exists then 
                match eventList |> List.filter (fun x -> x.Num = s) |> List.map (fun x -> x.Multiply) |> List.head with
                | Some(x) -> x
                | _ -> 1.0m
            else 1.0m
    
        let calcSingleResult winningTime (item : ParsedResult) i =
            let cat = Config.Classes.Classes |> Array.toList |> List.filter(fun x -> string x.Id = item.ClassId.Value) |> List.head
            let catCalcRule = match cat.CalcRule with
                              | Some x ->
                                    if x = "" then
                                        Config.General.CalcRule
                                    else
                                        x
                              | None -> Config.General.CalcRule
            let strategy = getCalcStrategy catCalcRule
            let points = 
                if (item.Status = "OK") then
                    strategy.Execute winningTime (decimal item.Time) i * eventMultiplier
                else
                    0m
            { 
                OrganisationId = item.OrganisationId;
                Name = setName (item.GivenName + " " + item.FamilyName);
                Points = Math.Round(points, 2);
                Time = item.Time;
                Position = i;
                Status = item.Status;
            }

        let r = parseResultXml inputFile
                |> filter Config.Organisations.Filter (orgCfg |> List.map (fun x -> x.Id)) isSameOrg
                |> filter Config.Classes.Filter (classCfg |> List.map (fun x -> x.Id)) isSameClass
                |> Seq.groupBy (fun i -> i.ClassId)
                |> Seq.map (fun pair ->
                                    let clId = fst pair
                                    let clRes = snd pair
                                    let validResults = clRes 
                                                        |> Seq.filter (fun x -> x.Status = "OK")
                                    let winningTime = 
                                        if (validResults |> Seq.isEmpty) then 0
                                        else validResults
                                                |> Seq.map (fun x -> x.Time)
                                                |> Seq.min
                                    let timeGroupedRes = validResults
                                                            |> Seq.sortBy (fun x -> x.Time)
                                                            |> Seq.groupBy (fun x -> x.Time)
                                    let cupPositions = getPositionSeq 1 (getIntervalList timeGroupedRes)
                                    let res = (cupPositions, timeGroupedRes) 
                                                    ||> Seq.map2 (fun i1 i2 -> snd i2 
                                                                                |> Seq.map (fun item -> calcSingleResult (decimal winningTime) item i1))
                                    let includeStatus = Config.General.IncludeStatus.Replace(" ", "").Split ','
                                    let others = clRes
                                                    |> Seq.filter (fun x -> includeStatus |> Array.exists (fun y -> y = x.Status))
                                                    |> Seq.map (fun x -> {
                                                                            OrganisationId = x.OrganisationId;
                                                                            Name = setName (x.GivenName + " " + x.FamilyName);
                                                                            Points = 0m;
                                                                            Time = x.Time;
                                                                            Position = 0;
                                                                            Status = x.Status;
                                                                            })
                                    clId, Seq.append (flattenSeqOfSeq res) others)
        fileName, r

    let competitions = 
        match Config.General.Type with
        | "Team" -> Path.Combine(cArgs.wDir, inputFile) |> Seq.singleton
        | "Cup" -> getFiles cArgs.wDir ((Config.General.ResultFilePrefix) + "*_*.xml") Config.General.RecurseSubDirs
        | _ -> Seq.empty

    let res = 
        match Config.General.Type with
        | "Cup" ->

            if Config.General.ConvertToJson then
                competitions |> Seq.iter toJson |> ignore

            let v = competitions
                    |> Seq.map buildEventResult
                    |> flatten
                    |> Seq.groupBy (fun (event, category, prr) -> category.Value + "~" + prr.Name)
                    |> Seq.map (fun pair ->
                                    let r = snd pair

                                    let _, catId, prr =
                                        r
                                        |> Seq.take 1
                                        |> Seq.exactlyOne
                                    let countingResults =
                                        r
                                        |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                        |> Seq.truncate Config.General.TakeBest
                                    let x =
                                        r
                                        |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                        |> Seq.mapi (fun i (a, b, c) -> 
                                                        let counts =
                                                            if i < Config.General.TakeBest then true
                                                            else false
                                                        { EventFile = a; ClassId = b; PRR = c; ResultCounts = counts; })
                                    let sum = 
                                        countingResults
                                        |> Seq.sumBy (fun (_, _, prr) -> prr.Points)
                                    { PersonName = prr.Name; ClassId = catId; OrganisationId = prr.OrganisationId; TotalPoints = sum; Results = x })
            Some (CupResult v)
        | _ -> None
 
    match res with
    | Some r ->
        let orgInfos = extractOrganisationInfo competitions |> Seq.toList
        let classInfos = extractClassInfo competitions |> Seq.toList

        let virtualClassIdNameInfo = virtualClasses |> List.map (fun x -> { Id = x.Id; Name = x.Name; ShortName = x.ShortName })
        let data = {
            InputPath = cArgs.wDir;
            Config= Config;
            ClassCfg = classCfg;
            ClassInfo = classInfos @ virtualClassIdNameInfo;
            OrgCfg = orgCfg;
            OrgInfo = orgInfos;
            Result = r
        }

        checkNameTypos data

        // output
        if Config.Output.Html.Active then
            buildResultHtml data |> ignore

        ////if Config.Output.Pdf.Active then
        ////    let outputFile = Path.Combine(inputPath, Config.Output.Pdf.FileName)
        ////    buildResultPdf classResults outputFile|> ignore

        if Config.Output.Json.Active then
            let outputFile = Path.Combine(cArgs.wDir, Config.Output.Json.FileName)
            let json = Json.serialize r
            //let json = JsonConvert.SerializeObject(r)
            File.WriteAllText(outputFile, json, Encoding.UTF8)
            printfn "JSON output written to %s" outputFile
        ()
    | None -> ()
