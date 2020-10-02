module HtmlOutput

open IofXmlLib.Types
open IofXmlLib.Helper
open IofXmlLib.Calc
open IofXmlLib.Logging

open Types
open Helper

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

let buildResultHtml data =

    let cssFile = Path.Combine(data.InputPath, data.Config.Output.Html.CssFile)
    let outputFile = Path.Combine(data.InputPath, data.Config.Output.Html.FileName)
    let docTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.DocTemplate)
    let classTemplateFile = Path.Combine(data.InputPath, data.Config.Output.Html.ClassTemplate)
    let cssContent = File.ReadAllText(cssFile)
    let creationDate = System.DateTime.Now.ToString("R")

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
                |> add "inline" data.Config.Output.Html.CssInline
                |> add "cssContent" cssContent
                |> add "title" data.Config.General.Name
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

            let classWeight =
                if data.Config.Classes.PresentationOrder = "" then
                    List.Empty
                else
                    data.Config.Classes.PresentationOrder.Split[|','|]
                    |> Array.toList
                    |> List.map (fun y -> y.Trim())
                    |> List.map (fun y -> XmlResult.Id(None, y))
                    |> List.filter (fun x -> classList |> List.exists (fun y -> isSame y x))

            let unsortedClassList =
                classList
                |> List.filter(fun x -> classWeight |> List.exists(fun y -> isSame y x) |> not)

            let orderdClassList =
                classWeight @ unsortedClassList

            let catRes =
                [ for cl in orderdClassList do
                    let i, catResult = classResults |> Seq.find(fun (catId, _) -> isSame catId cl)
                    let finalCatRes = recalcCupPositions catResult
                    let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" i
                    let getOrgNameById id = 
                        let cName, _ = getNamesById data.OrgCfg data.OrgInfo "Unknown Club" id
                        cName

                    let rule = 
                        let cat = data.Config.Classes.Classes |> Array.filter(fun x -> string x.Id = cl.Value) |> Array.tryHead
                        match cat with
                        | Some x ->
                            match x.CalcRule with
                            | Some x ->
                                if x = "" then
                                    data.Config.General.CalcRule
                                else
                                    x
                            | None -> data.Config.General.CalcRule
                        | None -> data.Config.General.CalcRule
                    let strategy = getCalcStrategy rule

                    let printDetailedResultRow (results : seq<EventResult>) (template:string) =
                        [ for r in [1..data.Config.General.NumberOfEvents] do
                                let p = results |> Seq.filter (fun eventResult -> eventResult.EventDetails.Number = r)
                                if Seq.isEmpty p then
                                    yield "<td/>"
                                else 
                                    let eRes = p |> Seq.take 1 |> Seq.exactlyOne
                                    yield init
                                        |> add "statusOk" (eRes.PRR.Status = "OK")
                                        |> add "resultCounts" eRes.ResultCounts
                                        |> add "points" (strategy.FormatPoints eRes.PRR.Points)
                                        |> add "time" (formatSeconds2Time eRes.PRR.Time)
                                        |> add "pos" eRes.PRR.Position
                                        |> add "status" (getStatusText data.Config.Translations eRes.PRR.Status)
                                        |> fromFile template ]

                    yield init
                        |> add "events" [1..data.Config.General.NumberOfEvents]
                        |> add "generalEventTitle" data.Config.General.EventTitle
                        |> add "eventInfos" (cr |> Seq.head).EventInfos
                        |> add "classFullName" cName
                        |> add "classShortName" cShort
                        |> add "hasShortName" (cShort.Length > 0)
                        |> add "catResults" finalCatRes
                        |> add "getRowStyle" getRowStyle
                        |> add "getClubNameById" getOrgNameById
                        |> add "format" strategy.FormatPoints
                        |> add "printDetailedResultRow" printDetailedResultRow
                        |> add "templateFile" detailsTemplateFile
                        |> add "combineListToString" combineListToString
                        |> fromFile classTemplateFile]

            let compiledHtml =
                init
                |> add "inline" data.Config.Output.Html.CssInline
                |> add "cssContent" cssContent
                |> add "title" data.Config.General.Name
                |> add "takeBest" data.Config.General.TakeBest
                |> add "numberOfEvents" data.Config.General.NumberOfEvents
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
                        let cr = Seq.append valid invalid
                        let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" i
                        let getOrgNameById id = 
                            let cName, _ = getNamesById data.OrgCfg data.OrgInfo "Unknown Club" id
                            cName

                        let isWinner rank = rank = 1
                        let printDetailedResultRow (results : seq<EventResult>) (template:string) =
                            let races = [1..data.Config.General.NumberOfEvents] // |> List.map (fun i -> (data.Config.General.ResultFilePrefix) + i.ToString("D2") + "_" + (data.Config.General.Year).ToString())
                            [ for r in races do
                                 let p = results |> Seq.filter (fun eventResult -> eventResult.EventDetails.Number = r)
                                 if Seq.isEmpty p then
                                      yield "<td/>"
                                 else 
                                      let eRes = p |> Seq.take 1 |> Seq.exactlyOne
                                      yield init
                                            |> add "isStatusOk" (eRes.PRR.Status = "OK")
                                            |> add "isResultTaken" eRes.ResultCounts
                                            |> add "timeBehind" eRes.PRR.TimeBehind
                                            |> add "time" eRes.PRR.Time
                                            |> add "pos" eRes.PRR.Position
                                            |> add "status" (getStatusText data.Config.Translations eRes.PRR.Status)
                                            |> add "formatTime" formatTime
                                            |> fromFile template]

                        yield init
                            |> add "events" [1..data.Config.General.NumberOfEvents]
                            |> add "eventInfos" (sr |> Seq.head).EventInfos
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
                |> add "inline" data.Config.Output.Html.CssInline
                |> add "cssContent" cssContent
                |> add "title" data.Config.General.Name
                |> add "takeBest" data.Config.General.TakeBest
                |> add "numberOfEvents" data.Config.General.NumberOfEvents
                |> add "year" DateTime.Now.Year
                |> add "date" creationDate
                |> add "catResult" catRes
                |> fromFile docTemplateFile
            compiledHtml

    File.WriteAllText(outputFile, html)
    tracer.Info "HTML output written to %s" outputFile
