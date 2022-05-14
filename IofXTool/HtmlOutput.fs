module HtmlOutput

open IofXmlLib.Types
open IofXmlLib.Helper
open IofXmlLib.Calc
open IofXmlLib.Logging

open Helper
open Types

open Fue.Compiler
open Fue.Data

open System
open System.IO

let getRowStyle rowIndex =
    if (rowIndex % 2 = 0) then "row_style_even"
    else "row_style_odd"

let getStatusText (translations : XmlConfig.Translation []) (s:string) =
    let t = translations |> Array.filter (fun x -> x.Key.Contains(s))
    if (t.Length > 0) then
        t.[0].Translate
    else
        explode s |> List.filter (fun x -> Char.IsUpper(x)) |> implode

let formatTime = sprintfTime "%h\:mm\:ss\.F" TrimOptions.Both

let rowDetailsPrinter (events:Event list) (config:XmlConfig.Configuration) (rule:CalculationRule option) (results:seq<EventResult>) (template:string) =
    let races = events |> List.map (fun e -> e.Number) |> List.sort
    [ for r in races do
         let p = results |> Seq.filter (fun eventResult -> eventResult.EventDetails.Number = r)
         if Seq.isEmpty p then
              yield "<td/>"
         else 
              let eRes = p |> Seq.take 1 |> Seq.exactlyOne
              let points = rule |> Option.map (fun r -> r.FormatPoints eRes.PRR.Points) |> Option.defaultValue ""
               
              yield init
                    |> add "isStatusOk" (eRes.PRR.Status = "OK")
                    |> add "isResultCounting" eRes.ResultCounts
                    |> add "points" points
                    |> add "timeBehind" eRes.PRR.TimeBehind
                    |> add "time" eRes.PRR.Time
                    |> add "pos" eRes.PRR.Position
                    |> add "statusText" (getStatusText config.Translations eRes.PRR.Status)
                    |> add "fnFormatTime" formatTime
                    |> fromFile template]

let buildResultHtml data =

    let cssFile = Path.Combine(data.InputPath, data.Config.Output.Html.CssFile)
    let outputFile = Path.Combine(data.InputPath, data.Config.Output.Html.FileName)
    let docTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.DocTemplate)
    let classTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.ClassTemplate)
    let cssContent = File.ReadAllText(cssFile)
    let creationDate = System.DateTime.Now.ToString("R")
    let inlineCss = data.Config.Output.Html.CssInline |> Option.defaultValue false
    let numberOfCountingEvents = data.Config.General.NumberOfCountingEvents
                                 |> Option.defaultValue data.Config.General.NumberOfPlannedEvents
    let numberOfValidEvents = data.Config.General.NumberOfValidEvents
                              |> Option.defaultValue data.Config.General.NumberOfPlannedEvents
    let html =
        match data.Result with
        | TeamResult tr ->
            let catRes = tr 
                        |> Seq.map(fun (i, tr) ->
                            let (cName, cShort) = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" i
                            let getOrgNameById id = 
                                let (cName, _) = getNamesById data.OrgCfg data.OrgInfo "Unknown School" id
                                cName
                            let cr = recalcTeamPositions tr
                            
                            init
                                |> add "classFullName" cName
                                |> add "classShortName" cShort
                                |> add "catResults" cr
                                |> add "getRowStyle" getRowStyle
                                |> add "getClubNameById" getOrgNameById
                                |> add "combineListToString" combineListToString
                                |> add "formatTime" formatSeconds2Time
                                |> fromFile classTemplateFile)
                        |> Seq.toList

            let compiledHtml =
                init
                |> add "inline" inlineCss
                |> add "cssContent" cssContent
                |> add "title" (data.Config.General.Name |> Option.defaultValue "")
                |> add "year" DateTime.Now.Year
                |> add "date" creationDate
                |> add "catResult" catRes
                |> fromFile docTemplateFile
            compiledHtml
        | CupResult cr ->
            let detailsTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.DetailsTemplate)

            let classResults =
                cr
                |> Seq.groupBy (fun cupResult -> cupResult.ClassId.Value)
                |> Seq.map (fun (x,y) -> XmlResult.Id (None, string x),y)

            let classList =
                classResults
                |> Seq.map(fun (x,_) -> x)
                |> Seq.toList

            let orderedClassList = getOrderedClassList classList (data.Config.Classes.PresentationOrder |> Option.defaultValue "")

            let catRes =
                [ for cl in orderedClassList do
                    let i, catResult = classResults |> Seq.find(fun (catId, _) -> isSame catId cl)
                    let finalCatRes =
                        let numberOfCompetitorsInResult = data.Config.General.ShowCompetitors |> Option.defaultValue 0
                        match numberOfCompetitorsInResult with
                        | n when n > 0 -> recalcCupPositions catResult |> Seq.takeWhile (fun (x, _) -> x <= n)
                        | _ -> recalcCupPositions catResult
                    let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" i
                    let getOrgNameById id = 
                        let cName, _ = getNamesById data.OrgCfg data.OrgInfo "Unknown Club" id
                        cName

                    let rule =
                        let generalCalcRule = data.Config.General.CalcRule |> Option.defaultValue "sum"
                        let cat = data.Config.Classes.Classes |> Array.filter(fun x -> string x.Id = cl.Value) |> Array.tryHead
                        match cat with
                        | Some x ->
                            match x.CalcRule with
                            | Some x ->
                                if x = "" then
                                    generalCalcRule
                                else
                                    x
                            | None -> generalCalcRule
                        | None -> generalCalcRule
                    let strategy = getCalcStrategy rule
                    let eventInfos = (cr |> Seq.head).EventInfos
                    let printDetailedResultRow = rowDetailsPrinter eventInfos data.Config strategy
                    let format =
                        match strategy with
                        | Some x -> x.FormatPoints
                        | None -> sprintf "%2f"
                    yield init
                        |> add "events" [1..numberOfValidEvents]
                        |> add "generalEventTitle" (data.Config.General.EventTitle |> Option.defaultValue "")
                        |> add "eventInfos" eventInfos
                        |> add "classFullName" cName
                        |> add "classShortName" cShort
                        |> add "hasShortName" (cShort.Length > 0)
                        |> add "catResults" finalCatRes
                        |> add "getRowStyle" getRowStyle
                        |> add "getClubNameById" getOrgNameById
                        |> add "format" format
                        |> add "printDetailedResultRow" printDetailedResultRow
                        |> add "templateFile" detailsTemplateFile
                        |> add "combineListToString" combineListToString
                        |> fromFile classTemplateFile]

            let compiledHtml =
                init
                |> add "inline" inlineCss
                |> add "cssContent" cssContent
                |> add "title" (data.Config.General.Name |> Option.defaultValue "")
                |> add "takeBest" numberOfCountingEvents
                |> add "numberOfEvents" numberOfValidEvents
                |> add "year" DateTime.Now.Year
                |> add "date" creationDate
                |> add "catResult" catRes
                |> fromFile docTemplateFile
            compiledHtml
        | SumResult sr ->
            let detailsTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.DetailsTemplate)

            let classResults =
                sr
                |> Seq.groupBy (fun cupResult -> cupResult.ClassId.Value)
                |> Seq.map (fun (x,y) -> XmlResult.Id (None, string x),y)

            let catRes =
                [ for cfg in data.ClassInfo do
                    let exists = classResults |> Seq.exists(fun (catId, _) -> isSame catId cfg.Id)
                    if exists then
                        let i, catResult = classResults 
                                           |> Seq.find(fun (catId, _) -> isSame catId cfg.Id)
                        let invalid = catResult |> Seq.filter (fun x -> x.Disq) |> Seq.map (fun x -> 0, x)
                        let valid = recalcSumPositions catResult
                        let showCompetitors = data.Config.General.ShowCompetitors |> Option.defaultValue 0
                        let cr =
                            match showCompetitors with
                            | n when n > 0 -> valid |> Seq.takeWhile (fun (x, _) -> x <= n)
                            | _ -> Seq.append valid invalid
                        
                        let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" i
                        let getOrgNameById id = 
                            let cName, _ = getNamesById data.OrgCfg data.OrgInfo "Unknown Club" id
                            cName

                        let isWinner rank = rank = 1
                        let eventInfos = (sr |> Seq.head).EventInfos
                        let printDetailedResultRow = rowDetailsPrinter eventInfos data.Config None

                        yield init
                            |> add "events" [1..numberOfValidEvents]
                            |> add "generalEventTitle" (data.Config.General.EventTitle |> Option.defaultValue "")
                            |> add "eventInfos" eventInfos
                            |> add "classFullName" cName
                            |> add "classShortName" cShort
                            |> add "hasShortName" (cShort.Length > 0)
                            |> add "isWinner" isWinner
                            |> add "catResults" cr
                            |> add "getRowStyle" getRowStyle
                            |> add "getClubNameById" getOrgNameById
                            |> add "formatTime" formatTime
                            |> add "printDetailedResultRow" printDetailedResultRow
                            |> add "templateFile" detailsTemplateFile
                            |> add "combineListToString" combineListToString
                            |> fromFile classTemplateFile]

            let compiledHtml =
                init
                |> add "inline" inlineCss
                |> add "cssContent" cssContent
                |> add "title" (data.Config.General.Name |> Option.defaultValue "")
                |> add "takeBest" data.Config.General.NumberOfCountingEvents
                |> add "numberOfEvents" numberOfValidEvents
                |> add "year" DateTime.Now.Year
                |> add "date" creationDate
                |> add "catResult" catRes
                |> fromFile docTemplateFile
            compiledHtml

    File.WriteAllText(outputFile, html)
    tracer.Info "HTML output written to %s" outputFile
