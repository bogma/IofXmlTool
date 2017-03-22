module HtmlOutput

open CupTypes
open Calc
open Helper

open Fue.Compiler
open Fue.Data

open System
open System.IO

let getRowStyle rowIndex =
    if (rowIndex % 2 = 0) then "row_style_even"
    else "row_style_odd"

let printDetailedResultRow (results : seq<EventResult>) =
    let races = [1..Config.Cup.NumberOfEvents] |> List.map (fun i -> (Config.Cup.ResultFilePrefix) + i.ToString("D2") + "_" + (Config.Cup.Year).ToString())
    [ for r in races do
          let p = results |> Seq.filter (fun eventResult -> eventResult.EventFile = r)
          if Seq.isEmpty p then yield "<td/>"
          else 
            let eRes = p |> Seq.take 1 |> Seq.exactlyOne
            yield init
                    |> add "statusOk" (eRes.PRR.Status = "OK")
                    |> add "resultCounts" eRes.ResultCounts
                    |> add "points" ((getCalcStrategy Config.Cup.CalcRule).FormatPoints eRes.PRR.Points)
                    |> add "time" (formatSeconds2Time eRes.PRR.Time)
                    |> add "pos" eRes.PRR.Position
                    |> add "status" (explode eRes.PRR.Status |> List.filter (fun x -> Char.IsUpper(x)) |> implode)
                    |> fromFile Config.Output.Html.DetailsTemplate]

let buildResultHtml classResults (outputFile:string)=
    let classCfg = Config.Classes |> Array.toList
    let catRes =
        [ for cfg in classCfg do
            let exists = classResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = classResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)            
                let strategy = getCalcStrategy Config.Cup.CalcRule
                let cr = recalcPositions catResult
                yield
                    init
                    |> add "events" [1..Config.Cup.NumberOfEvents]
                    |> add "classFullName" cfg.Name
                    |> add "classShortName" cfg.DiplayName
                    |> add "catResults" cr
                    |> add "getRowStyle" getRowStyle
                    |> add "getClubNameById" getClubNameById
                    |> add "printDetailedResultRow" printDetailedResultRow
                    |> add "combineListToString" combineListToString
                    |> fromFile Config.Output.Html.ClassTemplate]

    let cssContent = File.ReadAllText(Config.Output.Html.CssFile)
    let creationDate = System.DateTime.Now.ToString("R")
    let compiledHtml =
        init
        |> add "inline" Config.Output.Html.CssInline
        |> add "cssContent" cssContent
        |> add "title" Config.Cup.Name
        |> add "takeBest" Config.Cup.TakeBest
        |> add "numberOfEvents" Config.Cup.NumberOfEvents
        |> add "year" DateTime.Now.Year
        |> add "date" creationDate
        |> add "catResult" catRes
        |> fromFile Config.Output.Html.DocTemplate

    File.WriteAllText(outputFile, compiledHtml)

    let path = Path.GetDirectoryName(outputFile)
    File.Copy(Config.Output.Html.CssFile, Path.Combine(path, "default.css"), true)
    printfn "HTML output written to %s" outputFile
