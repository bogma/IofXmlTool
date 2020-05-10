﻿module BuildHandler

open System
open System.IO

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
open Newtonsoft.Json
open SelectPdf

let build (cArgs:CommonArgs) (args : ParseResults<_>) =
    
    let configFile = tryLocateFile cArgs.wDir cArgs.cfgFile
    match configFile with
    | None -> ()
    | Some x ->
        let config = XmlConfig.Load(x)

        let classCfg = config.Classes.Classes |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = x.DiplayName })
        let orgCfg = config.Organisations.Organisations |> Array.toList |> List.map (fun x -> { Id = XmlResult.Id(None, string x.Id); Name = x.Name; ShortName = "" })

        let virtualClasses = config.Classes.Combines
                            |> Array.toList
                            |> List.map (fun x -> 
                                { 
                                    Id = XmlResult.Id(None, x.ClassIds)
                                    Name = x.Name; 
                                    ShortName = x.DiplayName;
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

        let buildEventResult (inputFile : string) =
        
            let fileName = Path.GetFileNameWithoutExtension(inputFile)
            let eventMultiplier = 
                let p = fileName.IndexOf("_")
                let s = fileName.Substring(p-2, 2).AsInteger()
                let eventList = config.Events |> Array.toList
                let exists = eventList |> List.exists (fun x -> x.Num = s)
                if exists then 
                    match eventList |> List.filter (fun x -> x.Num = s) |> List.map (fun x -> x.Multiply) |> List.head with
                    | Some x -> x
                    | _ -> 1.0m
                else 1.0m
    
            let calcSingleResult winningTime (item : ParsedResult) i =
                let rule = 
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
                        strategy.Execute winningTime (decimal item.Time) i * eventMultiplier
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

            let r = parseResultXml inputFile
                    |> filter config.Organisations.Filter (orgCfg |> List.map (fun x -> x.Id)) isSameOrg
                    |> filter config.Classes.Filter (classCfg |> List.map (fun x -> x.Id)) isSameClass
                    |> Seq.groupBy (fun i -> i.ClassId)
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
                                        clId, Seq.append (flattenSeqOfSeq res) others)
            fileName, r

        let competitions = 
            match config.Type with
            | "Cup" | "Sum" -> getFiles cArgs.wDir ((config.General.ResultFilePrefix) + "*_*.xml") config.General.RecurseSubDirs
            | "Team" ->
                let inputFile = args.TryGetResult(Files) |> Option.defaultValue [""] |> List.head
                match tryLocateFile cArgs.wDir inputFile with
                | Some x ->
                    printfn "input file path %s is valid" x
                    x |> Seq.singleton
                | None ->
                    printfn "input file not found"
                    Seq.empty
            | _ -> Seq.empty

        let res = 
            match config.Type with
            | "Cup" ->
                if config.General.ConvertToJson then
                    competitions |> Seq.iter toJson |> ignore

                let v = competitions
                        |> Seq.map buildEventResult
                        |> flatten
                        |> Seq.groupBy (fun (event, cl, prr) -> cl.Value + "~" + prr.Name)
                        |> Seq.map (fun (_, r) ->
                                        let _, catId, prr =
                                            r
                                            |> Seq.take 1
                                            |> Seq.exactlyOne
                                        let countingResults =
                                            r
                                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                            |> Seq.truncate config.General.TakeBest
                                        let x =
                                            r
                                            |> Seq.sortBy (fun (_, _, prr) -> -prr.Points)
                                            |> Seq.mapi (fun i (a, b, c) -> 
                                                            let counts =
                                                                if i < config.General.TakeBest then true
                                                                else false
                                                            { EventFile = a; ClassId = b; PRR = c; ResultCounts = counts; })
                                        let sum = 
                                            countingResults
                                            |> Seq.sumBy (fun (_, _, prr) -> prr.Points)
                                        { PersonName = prr.Name; ClassId = catId; OrganisationId = prr.OrganisationId; TotalPoints = sum; Results = x })
                Some (CupResult v)
            | "Sum" ->
                if config.General.ConvertToJson then
                    competitions |> Seq.iter toJson |> ignore

                let v = competitions
                        |> Seq.map buildEventResult
                        |> flatten
                        |> Seq.groupBy (fun (event, cl, prr) -> cl.Value + "~" + prr.Name)
                        |> Seq.map (fun (_, r) ->
                                        let _, catId, prr = r |> Seq.head
                                        let sum = r |> Seq.sumBy (fun (_, _, prr) -> prr.Points) |> float
                                        let x = r |> Seq.map (fun (a, b, c) -> { EventFile = a; ClassId = b; PRR = c; ResultCounts = true; })
                                        let eventsOk = x |> Seq.filter (fun i -> i.PRR.Status = "OK") |> Seq.length
                                        let disq = not (eventsOk = config.General.NumberOfEvents)
                                        { PersonName = prr.Name; ClassId = catId; OrganisationId = prr.OrganisationId; TotalTime = sum; TimeBehind = 0.0; Disq = disq; Results = x })
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
                                            { PersonName = x.PersonName; ClassId = x.ClassId; OrganisationId = x.OrganisationId; TotalTime = x.TotalTime; TimeBehind = diff; Disq = x.Disq; Results = x.Results }))
                        |> flattenSeqOfSeq
                Some (SumResult v)
            | "Team" ->
                let r = parseResultXml (competitions |> Seq.head)
                        |> filter config.Organisations.Filter (orgCfg |> List.map (fun x -> x.Id)) isSameOrg
                        |> filter config.Classes.Filter (classCfg |> List.map (fun x -> x.Id)) isSameClass
                        |> List.map (fun a -> { ClassId = combineClasses a.ClassId; OrganisationId = a.OrganisationId; GivenName = a.GivenName; FamilyName = a.FamilyName; Position = a.Position; Time = a.Time; TimeBehind = a.TimeBehind; Status = a.Status })
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
                Result = r
            }

            checkNameTypos data

            // output
            if config.Output.Html.Active then
                buildResultHtml data |> ignore

            if config.Output.Pdf.Active then
                let converter = new SelectPdf.HtmlToPdf();

                // set converter options
                let pdfOptions = data.Config.Output.Pdf
                match pdfOptions.PageSize with
                | "A3" -> converter.Options.PdfPageSize <- PdfPageSize.A3
                | "A4" -> converter.Options.PdfPageSize <- PdfPageSize.A4
                | "A5" -> converter.Options.PdfPageSize <- PdfPageSize.A5
                | _ -> converter.Options.PdfPageSize <- PdfPageSize.A4

                match pdfOptions.PageOrientation with
                | "Landscape" -> converter.Options.PdfPageOrientation <- PdfPageOrientation.Landscape
                | _ -> converter.Options.PdfPageOrientation <- PdfPageOrientation.Portrait

                converter.Options.MarginLeft <- pdfOptions.MarginLeft
                converter.Options.MarginRight <- pdfOptions.MarginRight
                converter.Options.MarginTop <- pdfOptions.MarginTop
                converter.Options.MarginBottom <- pdfOptions.MarginBottom

                let htmlInputFile = Path.Combine(data.InputPath, data.Config.Output.Html.FileName)
                let doc = converter.ConvertUrl(htmlInputFile);
                let outputFile = Path.Combine(data.InputPath, data.Config.Output.Pdf.FileName)
                doc.Save(outputFile)
                doc.Close()
                printfn "PDF output written to %s" outputFile

            if config.Type = "Cup" && config.Output.Json.Active then
                let outputFile = Path.Combine(cArgs.wDir, config.Output.Json.FileName)
                let json = JsonConvert.SerializeObject(r)
                File.WriteAllText(outputFile, json, Text.Encoding.UTF8)
                printfn "JSON output written to %s" outputFile

            ()
        | None -> ()
