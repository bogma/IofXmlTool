module BuildCmdHandler

open System
open System.IO

open Argu

open IofXmlLib.Types
open IofXmlLib.Calc
open IofXmlLib.Helper
open IofXmlLib.Logging
open IofXmlLib.PreProcessors
open IofXmlLib.XmlParser

open Types
open Helper
open Commands
open HtmlOutput
open Newtonsoft.Json
open PdfSharpCore
open TheArtOfDev.HtmlRenderer.PdfSharp
open System.Collections.Generic

let build (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let configFile = tryLocateFile cArgs.wDir cArgs.cfgFile
    match configFile with
    | None -> ()
    | Some x ->
        let config = XmlConfig.Load(x)

        let classCfg = config.Classes.Classes |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = x.ShortName })
        let orgCfg = config.Organisations.Organisations |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = "" })

        let virtualClasses = config.Classes.Combines
                            |> Array.toList
                            |> List.map (fun x -> 
                                { 
                                    Id = XmlResult.Id(None, x.ClassIds)
                                    Name = x.Name; 
                                    ShortName = x.ShortName;
                                    Classes = x.ClassIds.Split[|','|]
                                              |> Array.toList
                                              |> List.map (fun y -> y.Trim())
                                              |> List.map (fun y -> XmlResult.Id(None, y))
                                })

        let setName (name:string) =
            let names = config.Names
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

        let buildEventResult (eventInfo : Event) =

            let calcSingleResult winningTime (item : ParsedResult) i =
                let rule =
                    match eventInfo.Rule with
                    | Some r -> r
                    | None ->
                        let cat = config.Classes.Classes |> Array.filter(fun x -> string x.Id = item.ClassId.Value) |> Array.tryHead
                        match cat with
                        | Some x ->
                            match x.CalcRule with
                            | Some x ->
                                if x = "" then
                                    config.General.CalcRule
                                else
                                    x
                            | None -> config.General.CalcRule
                        | None -> config.General.CalcRule
                let strategy = getCalcStrategy rule
                let points = 
                    if (item.Status = "OK") then
                        strategy.Execute winningTime (decimal item.Time) i * eventInfo.Multiply
                    else
                        0m
                { 
                    OrganisationId = item.OrganisationId;
                    Name = setName (item.GivenName + " " + item.FamilyName);
                    Points = Math.Round(points, 2);
                    Time = item.Time;
                    TimeBehind = item.TimeBehind;
                    Position = i;
                    Status = item.Status;
                }

            let r = parseResultXml eventInfo.FileName eventInfo.Mappings
                    |> filter config.Organisations.Filter (orgCfg |> List.map (fun x -> x.Id)) isSameOrg
                    |> filter config.Classes.Filter (classCfg |> List.map (fun x -> x.Id)) isSameClass
                    |> List.map (fun a -> { a with ClassId = combineClasses a.ClassId })
                    |> Seq.groupBy (fun i -> i.ClassId.Value)
                    |> Seq.map (fun (clId, clRes) ->
                                        let validResults = clRes 
                                                            |> Seq.filter (fun x -> x.Status = "OK")
                                        let winningTime = 
                                            if (validResults |> Seq.isEmpty) then 0.0
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
                                        let includeStatus = config.General.IncludeStatus.Replace(" ", "").Split ','
                                        let others = clRes
                                                        |> Seq.filter (fun x -> includeStatus |> Array.exists (fun y -> y = x.Status))
                                                        |> Seq.map (fun x -> {
                                                                                OrganisationId = x.OrganisationId;
                                                                                Name = setName (x.GivenName + " " + x.FamilyName);
                                                                                Points = 0m;
                                                                                Time = x.Time;
                                                                                TimeBehind  = x.TimeBehind;
                                                                                Position = 0;
                                                                                Status = x.Status;
                                                                                })
                                        XmlResult.Id (None, string clId), Seq.append (flattenSeqOfSeq res) others)
            eventInfo, r

        let events = 
            match config.Type with
            | "Cup" | "Sum" -> getEventInfos config cArgs.wDir |> List.toSeq
            | "Team" ->
                let inputFile = args.TryGetResult(Files) |> Option.defaultValue [""] |> List.head
                match tryLocateFile cArgs.wDir inputFile with
                | Some x ->
                    tracer.Info "input file path %s is valid" x
                    { FileName = x; Name = ""; Number = 1; Date = ""; Multiply = 1.0m; Rule = None; Mappings = Array.empty } |> Seq.singleton
                | None ->
                    tracer.Warn "input file not found"
                    Seq.empty
            | _ -> 
                tracer.Error "no valid cup type given. please check your configuration file."
                Seq.empty



        config.PreProcessing.Tasks
            |> Array.filter(fun x -> x.Name = "fromCSV" && x.Active)
            |> Array.iter(fun x ->
                let csvResultFiles = getFiles cArgs.wDir config.General.ResultFileRegex "*.csv" config.General.RecurseSubDirs
                let csvParams : IDictionary<string,string> = x.Params |> Array.map (fun x -> x.Key, x.Value) |> Array.toSeq |> dict
                csvResultFiles |> Seq.iter (fromCSV csvParams))

        let validEventInfo (eventInfo:Event) =
            File.Exists(eventInfo.FileName) && Path.GetExtension(eventInfo.FileName) = ".xml"

        let competitions = events
                            |> Seq.filter validEventInfo
                            |> Seq.map (fun x -> x.FileName)

        config.PreProcessing.Tasks
            |> Array.filter (fun x -> x.Active)
            |> Array.iter (fun x -> 
                    match x.Name with
                    | "fromCSV" -> ()
                    | "toJson" -> competitions |> Seq.iter toJson
                    | "toUtf8" -> competitions |> Seq.iter toUtf8
                    | _ -> tracer.Warn "preprocessing option %s is not supported" x.Name)

        let events = 
            match config.General.ShowEvents with
            | "OmitTailMissing" -> events |> Seq.sortByDescending (fun x -> x.Number) |> Seq.skipWhile (fun x -> not (validEventInfo x)) |> Seq.sortBy (fun x -> x.Number)
            | "OmitAllMissing" -> events |> Seq.filter validEventInfo
            | _ -> events

        let res = 
            match config.Type with
            | "Cup" ->
                let v = events
                        |> Seq.filter validEventInfo
                        |> Seq.map buildEventResult
                        |> flatten
                        |> Seq.groupBy (fun (_, cl, prr) -> cl.Value + "~" + prr.Name)
                        |> Seq.map (fun (_, r) ->
                                        let _, catId, prr =
                                            r
                                            |> Seq.take 1
                                            |> Seq.exactlyOne
                                        let countingResults =
                                            r
                                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                            |> Seq.truncate config.General.NumberOfCountingEvents
                                        let x =
                                            r
                                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                            |> Seq.mapi (fun i (a, b, c) -> 
                                                            let counts = i < config.General.NumberOfCountingEvents
                                                            { EventDetails = a; ClassId = b; PRR = c; ResultCounts = counts; })
                                        let sum = 
                                            countingResults
                                            |> Seq.sumBy (fun (_, _, prr) -> prr.Points)
                                        { PersonName = prr.Name; ClassId = catId; OrganisationId = prr.OrganisationId; TotalPoints = sum; Results = x; EventInfos = events |> Seq.toList })
                Some (CupResult v)
            | "Sum" ->
                let v = events
                        |> Seq.filter validEventInfo
                        |> Seq.map buildEventResult
                        |> flatten
                        |> Seq.groupBy (fun (_, cl, prr) -> cl.Value + "~" + prr.Name)
                        |> Seq.map (fun (_, r) ->
                                        let _, catId, prr = r |> Seq.head
                                        let sum = r |> Seq.sumBy (fun (_, _, prr) -> prr.Points) |> float
                                        let x = r |> Seq.map (fun (a, b, c) -> { EventDetails = a; ClassId = b; PRR = c; ResultCounts = true; })
                                        let eventsOk = x |> Seq.filter (fun i -> i.PRR.Status = "OK") |> Seq.length
                                        let disq = not (eventsOk = config.General.NumberOfValidEvents)
                                        { PersonName = prr.Name; ClassId = catId; OrganisationId = prr.OrganisationId; TotalTime = sum; TimeBehind = 0.0; Disq = disq; Results = x; EventInfos = events |> Seq.toList })
                        |> Seq.groupBy (fun x -> x.ClassId)
                        |> Seq.map (fun (_, clRes) -> 
                                        let validResults = clRes |> Seq.filter (fun x -> not x.Disq)
                                        let winningTime = 
                                            if (validResults |> Seq.isEmpty) then 0.0
                                            else validResults
                                                   |> Seq.map (fun x -> x.TotalTime)
                                                   |> Seq.min
                                        clRes 
                                        |> Seq.map (fun x ->
                                            let diff = 
                                                if x.Disq then 0.0
                                                else x.TotalTime - winningTime
                                            { PersonName = x.PersonName; ClassId = x.ClassId; OrganisationId = x.OrganisationId; TotalTime = x.TotalTime; TimeBehind = diff; Disq = x.Disq; Results = x.Results; EventInfos = events |> Seq.toList }))
                        |> flattenSeqOfSeq
                Some (SumResult v)
            | "Team" ->
                let r = parseResultXml (competitions |> Seq.head) Array.empty
                        |> filter config.Organisations.Filter (orgCfg |> List.map (fun x -> x.Id)) isSameOrg
                        |> filter config.Classes.Filter (classCfg |> List.map (fun x -> x.Id)) isSameClass
                        |> List.map (fun pr -> { pr with ClassId = combineClasses pr.ClassId })
                        |> Seq.filter (fun x -> x.Status = "OK")
                        |> Seq.groupBy (fun i -> i.ClassId)
                        |> Seq.map (fun (clId, clRes) ->
                                        let validResults = clRes 
                                                           |> Seq.groupBy (fun x -> x.OrganisationId.Value)
                                                           |> Seq.map (fun (o,x) -> o, x |> Seq.sortBy (fun x -> x.Time))
                                                           |> Seq.map (fun (o,x) -> o, x |> Seq.toList |> triple)
                                                           |> Seq.filter (fun (_,x) -> List.isEmpty x |> not)
                                                           |> Seq.map (fun (o,t) -> o, t |> List.map ( fun tr -> tr |> buildTeam))
                                                           |> Seq.map (fun (_,x) -> x)
                                                           |> List.concat
                                                           |> List.sortBy (fun x -> x.TotalTime)
                                        clId, validResults)
                Some (TeamResult r)
            | _ -> None
 
        match res with
        | Some r ->
            let orgInfos = extractOrganisationInfo competitions |> Seq.toList
            let classInfos = extractClassInfo competitions |> Seq.toList
            let virtualClassIdNameInfo = virtualClasses |> List.map (fun x -> { Id = x.Id; Name = x.Name; ShortName = x.ShortName })
            
            let data = {
                InputPath = cArgs.wDir;
                Config= config;
                ClassCfg = classCfg;
                ClassInfo = classInfos @ virtualClassIdNameInfo;
                OrgCfg = orgCfg;
                OrgInfo = orgInfos;
                Result = r;
            }

            checkNameTypos data

            // output
            if config.Output.Html.Active then
                buildResultHtml data |> ignore

            if config.Output.Pdf.Active then
                if not config.Output.Html.Active then
                    tracer.Warn "PDF output can only be built upon HTML output - please activate html output"
                else
                    // apply PDF options
                    let pdfConfig = PdfGenerateConfig()

                    let pdfOptions = data.Config.Output.Pdf
                    match pdfOptions.PageSize with
                    | "A3" -> pdfConfig.PageSize <- PageSize.A3
                    | "A4" -> pdfConfig.PageSize <- PageSize.A4
                    | "A5" -> pdfConfig.PageSize <- PageSize.A5
                    | _ -> pdfConfig.PageSize <- PageSize.A4

                    match pdfOptions.PageOrientation with
                    | "Landscape" -> pdfConfig.PageOrientation <- PageOrientation.Landscape
                    | _ -> pdfConfig.PageOrientation <- PageOrientation.Portrait

                    pdfConfig.MarginLeft <- pdfOptions.MarginLeft
                    pdfConfig.MarginRight <- pdfOptions.MarginRight
                    pdfConfig.MarginTop <- pdfOptions.MarginTop
                    pdfConfig.MarginBottom <- pdfOptions.MarginBottom

                    let htmlInputFile = Path.Combine(data.InputPath, data.Config.Output.Html.FileName)
                    let outputFile = Path.Combine(data.InputPath, data.Config.Output.Pdf.FileName)

                    // disable logging of PdfSharpCore nuget package
                    let consoleOut = Console.Out
                    Console.SetOut(TextWriter.Null)
                    let doc = PdfGenerator.GeneratePdf(File.ReadAllText(htmlInputFile), pdfConfig);
                    
                    // enable logging again
                    Console.SetOut(consoleOut)
                    doc.Save(outputFile)
                    tracer.Info "PDF output written to %s" outputFile

            if config.Type = "Cup" && config.Output.Json.Active then
                let outputFile = Path.Combine(cArgs.wDir, config.Output.Json.FileName)
                let json = JsonConvert.SerializeObject(r)
                File.WriteAllText(outputFile, json, Text.Encoding.UTF8)
                tracer.Info "JSON output written to %s" outputFile

            ()
        | None -> ()
