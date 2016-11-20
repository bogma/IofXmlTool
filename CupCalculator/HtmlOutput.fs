module HtmlOutput

open CupTypes
open Calc
open Helper
open ProgramSettings
open System.IO

let buildRankingHeader i =
    let p1 = """<td class="ranking_header" valign="bottom" align="center"><div>"""
    let p2 = """</div></td>"""
    let s = sprintf "%i. SC" i
    p1 + s + p2

let printSingleDetailedResult points time pos counts =
    let p1 = 
        if counts then """<td align="center"><div style="overflow:hidden">"""
        else """<td align="center"><div style="overflow:hidden;text-decoration:line-through;">""" // + points i.e 100,00
    let p2 = """</div><div class="time" style="overflow:hidden">""" // + time (pos) i.e 45:29 (1)
    let p3 = """</div></td>"""
    let strategy = getCalcStrategy !calcRule
    let pointsFormated = strategy.FormatPoints points
    let tsString = formatSeconds2Time time
    let t = sprintf "%s (%i)" tsString pos
    p1 + pointsFormated + p2 + t + p3

let printDetailedResultRow results =
    let races = [1..!maxEvents] |> List.map (fun i -> (!resultFilePrefix) + i.ToString("D2") + "_" + (!year).ToString())
    [ for r in races do
          let p = results |> Seq.filter (fun (file, _, _, _) -> file = r)
          if Seq.isEmpty p then yield "<td/>"
          else 
            let _, _, prr, counts = p |> Seq.take 1 |> Seq.exactlyOne
            yield printSingleDetailedResult prr.Points prr.Time prr.Position counts ]

let printResult classHeader (catResult : seq<string * 'a * int * decimal * seq<string * 'c * PersonalRaceResult * bool>>) =
    let part1 = """<div><div class="category_title">""" + classHeader + """</div><br/><table border="0" cellpadding="2" cellspacing="0" width="750"><tbody><tr><td class="ranking_header" valign="bottom" align="right">Pl</td><td class="ranking_header" valign="bottom">Name<br/>Verein</td><td class="ranking_header" valign="bottom" align="center" style="border-right:1px solid #888;">Punkte</td>"""
    let part2 = [1..!maxEvents] |> List.map buildRankingHeader |> combineListToString
    let part3 = "</tr>"

    let sRes = recalcPositions catResult
               |> Seq.mapi (fun i (rank, item) ->
                            let name, cat, clubId, total, singleResults = item
                            let c = getClubNameById !orgCfg clubId
                            let rowClass = 
                                if (i % 2 = 0) then "cal_out_white"
                                else "cal_out_grey"
                            let r1 = """<tr class=" """ + rowClass + """ "><td align="right"><div style="overflow:hidden;">"""
                            let r2 = """</div></td><td><div style="overflow:hidden;"><b>"""
                            let r3 = """</b></div><div style="overflow:hidden;">"""
                            let r4 = """</div></td><td align="center" style="border-right:1px solid #888;"><div style="overflow:hidden;font-weight:bold;">"""
                            let r5 = """</div></td>"""
                            let details = printDetailedResultRow singleResults |> combineListToString
                            let r6 = "</tr>"
                            let strategy = getCalcStrategy !calcRule
                            let totalFormated = strategy.FormatPoints total
                            r1 + rank.ToString() + r2 + name + r3 + c + r4 + totalFormated + r5 + details + r6)
                |> Seq.fold (fun str x -> str + x) ""

    let part4 = """</tbody></table><br/><br/><br/></div>"""
    part1 + part2 + part3 + sRes + part4

let buildResultHtml catResults (outputFile:string)=
    let htmlOpen = "<html>"
    let htmlClose = "</html>"
    let head = """<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /><link href="./default.css" rel="stylesheet" type="text/css" /><title>""" + (!cupName) + """</title></head>"""
    let bodyTop = """<body><h1>Rangliste</h1><br/><br/><div>Gewertet werden die <strong>""" + (!takeBest).ToString() + """</strong> besten Ergebnisse von <strong>""" + (!maxEvents).ToString() + """</strong>.</div><br/><br/><br/><br/>"""
    let bodyBottom = """<br/><div>(c) """ + System.DateTime.Now.Year.ToString() + """ by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein</div><br/><div>webmaster@solv.at</div><div>Erstellt: """ + System.DateTime.Now.ToString("R") + """</div></body>"""

    let catRes =
        [ for cfg in !classCfg do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName
            let exists = catResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = catResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)
                yield printResult classHeader catResult ]

    let catResString = catRes |> combineListToString
    
    File.WriteAllText(outputFile, htmlOpen + head + bodyTop + catResString + bodyBottom + htmlClose)
    let path = Path.GetDirectoryName(outputFile)
    File.Copy("./resources/default.css", Path.Combine(path, "default.css"), true)
    printfn "HTML output written to %s" outputFile
