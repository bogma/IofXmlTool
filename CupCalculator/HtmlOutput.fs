module HtmlOutput

open CupTypes
open Calc
open Helper
open System.IO

let buildRankingHeader i =
    let p1 = """<td class="ranking_header" valign="bottom" align="center"><div>"""
    let p2 = """</div></td>"""
    let s = sprintf "%i. SC" i
    p1 + s + p2

let printSingleDetailedResult eventResult =
    let p1 =
        if eventResult.ResultCounts then """<td align="center"><div style="overflow:hidden">"""
        else """<td align="center"><div style="overflow:hidden;text-decoration:line-through;">""" // + points i.e 100,00
    let p2 = """</div><div class="time" style="overflow:hidden">""" // + time (pos) i.e 45:29 (1)
    let p3 = """</div></td>"""
    if (eventResult.PRR.Status = "OK") then
        let strategy = getCalcStrategy Config.Cup.CalcRule
        let pointsFormated = strategy.FormatPoints eventResult.PRR.Points
        let tsString = formatSeconds2Time eventResult.PRR.Time
        let t = sprintf "%s (%i)" tsString eventResult.PRR.Position
        p1 + pointsFormated + p2 + t + p3
    else
        """<td align="center"><div style="overflow:hidden">""" + eventResult.PRR.Status + p3

let printDetailedResultRow (results : seq<EventResult>) =
    let races = [1..Config.Cup.NumberOfEvents] |> List.map (fun i -> (Config.Cup.ResultFilePrefix) + i.ToString("D2") + "_" + (Config.Cup.Year).ToString())
    [ for r in races do
          let p = results |> Seq.filter (fun eventResult -> eventResult.EventFile = r)
          if Seq.isEmpty p then yield "<td/>"
          else 
            let eRes = p |> Seq.take 1 |> Seq.exactlyOne
            yield printSingleDetailedResult eRes]

let printResult classHeader (classResult : seq<CupResult>) =
    let part1 = """<div><div class="category_title">""" + classHeader + """</div><br/><table border="0" cellpadding="2" cellspacing="0" width="750"><tbody><tr><td class="ranking_header" valign="bottom" align="right">Pl</td><td class="ranking_header" valign="bottom">Name<br/>Verein</td><td class="ranking_header" valign="bottom" align="center" style="border-right:1px solid #888;">Punkte</td>"""
    let part2 = [1..Config.Cup.NumberOfEvents] |> List.map buildRankingHeader |> combineListToString
    let part3 = "</tr>"

    let sRes = recalcPositions classResult
               |> Seq.mapi (fun i (rank, item) ->
                            let c = getClubNameById item.OrganisationId
                            let rowClass = 
                                if (i % 2 = 0) then "cal_out_white"
                                else "cal_out_grey"
                            let r1 = """<tr class=" """ + rowClass + """ "><td align="right"><div style="overflow:hidden;">"""
                            let r2 = """</div></td><td><div style="overflow:hidden;"><b>"""
                            let r3 = """</b></div><div style="overflow:hidden;">"""
                            let r4 = """</div></td><td align="center" style="border-right:1px solid #888;"><div style="overflow:hidden;font-weight:bold;">"""
                            let r5 = """</div></td>"""
                            let details = printDetailedResultRow item.Results |> combineListToString
                            let r6 = "</tr>"
                            let strategy = getCalcStrategy Config.Cup.CalcRule
                            let totalFormated = strategy.FormatPoints item.TotalPoints
                            r1 + rank.ToString() + r2 + item.PersonName + r3 + c + r4 + totalFormated + r5 + details + r6)
                |> Seq.fold (fun str x -> str + x) ""

    let part4 = """</tbody></table><br/><br/><br/></div>"""
    part1 + part2 + part3 + sRes + part4

let buildResultHtml classResults (outputFile:string)=
    let htmlOpen = "<html>"
    let htmlClose = "</html>"
    let head = """<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /><link href="./default.css" rel="stylesheet" type="text/css" /><title>""" + Config.Cup.Name + """</title></head>"""
    let bodyTop = """<body><h1>Rangliste</h1><br/><br/><div>Gewertet werden die <strong>""" + Config.Cup.TakeBest.ToString() + """</strong> besten Ergebnisse von <strong>""" + Config.Cup.NumberOfEvents.ToString() + """</strong>.</div><br/><br/><br/><br/>"""
    let bodyBottom = """<br/><div>(c) """ + System.DateTime.Now.Year.ToString() + """ by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein</div><br/><div>webmaster@solv.at</div><div>Erstellt: """ + System.DateTime.Now.ToString("R") + """</div></body>"""

    let classCfg = Config.Classes |> Array.toList
    let catRes =
        [ for cfg in classCfg do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName
            let exists = classResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = classResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)
                yield printResult classHeader catResult ]

    let catResString = catRes |> combineListToString
    
    File.WriteAllText(outputFile, htmlOpen + head + bodyTop + catResString + bodyBottom + htmlClose)
    let path = Path.GetDirectoryName(outputFile)
    File.Copy("./resources/default.css", Path.Combine(path, "default.css"), true)
    printfn "HTML output written to %s" outputFile
