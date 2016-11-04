module Result

open CupTypes

let getClubName id =
    let n = XmlConfig.GetSample().Organisations
                |> Array.toList
                |> List.find(fun x -> x.Id = id)
    n.Name

let buildRankingHeader i =
    let p1 = """<td class="ranking_header" valign="bottom" align="center"><div>"""
    let p2 = """</div></td>"""
    let s = sprintf "%i. SC" i
    p1 + s + p2

let formatSeconds2Time time =
    let t1 = float time
    let ts = System.TimeSpan.FromSeconds(t1)
    let h = 
        if ts.Hours > 0 then ts.Hours.ToString() + ":"
        else ""
    h + ts.ToString(@"mm\:ss")

let printSingleDetailedResult points time pos =
//<div style="overflow:hidden;text-decoration:line-through;">87,51</div>
    let p1 = """<td align="center"><div style="overflow:hidden">""" // + points i.e 100,00
    let p2 = """</div><div class="time" style="overflow:hidden">""" // + time (pos) i.e 45:29 (1)
    let p3 = """</div></td>"""
    let tsString = formatSeconds2Time time
    let t = sprintf "%s (%i)" tsString pos
    p1 + points.ToString() + p2 + t + p3

let printDetailedResultRow results =
    let events = XmlConfig.GetSample().Cup.NumberOfEvents
    let races = [1..events] |> List.map (fun i -> "SC" + i.ToString("D2") + "_" + XmlConfig.GetSample().Cup.Year.ToString())
    [ for r in races do
          let p = results |> Seq.filter (fun (file, _, _, _, _, _, _) -> file = r)
          if Seq.isEmpty p then yield "<td/>"
          else 
            let _, _, _, _, points, time, pos = p |> Seq.take 1 |> Seq.exactlyOne
            yield printSingleDetailedResult points time pos ]

let printResult classHeader catResult =
    let part1 = """<div><div class="category_title">""" + classHeader + """</div><br/><table border="0" cellpadding="2" cellspacing="0" width="750"><tbody><tr><td class="ranking_header" valign="bottom" align="right">Pl</td><td class="ranking_header" valign="bottom">Name<br/>Verein</td><td class="ranking_header" valign="bottom" align="center" style="border-right:1px solid #888;">Punkte</td>"""
    let events = XmlConfig.GetSample().Cup.NumberOfEvents
    let part2 = [1..events] |> List.map buildRankingHeader |> List.fold (fun str x -> str + sprintf "%s" x) ""
    let part3 = "</tr>"
    let sRes = catResult
               |> Seq.sortBy (fun (_, _, _, total, _) -> -total) 
               |> Seq.mapi (fun i (name, cat, club, total, singleResults) ->
                            let n = name
                            let t = total
                            let c = getClubName club
                            let rowClass = 
                                if (i % 2 = 0) then "cal_out_white"
                                else "cal_out_grey"
                            let r1 = """<tr class=" """ + rowClass + """ "><td align="right"><div style="overflow:hidden;">"""
                            let r2 = """</div></td><td><div style="overflow:hidden;"><b>"""
                            let r3 = """</b></div><div style="overflow:hidden;">"""
                            let r4 = """</div></td><td align="center" style="border-right:1px solid #888;"><div style="overflow:hidden;font-weight:bold;">"""
                            let r5 = """</div></td>"""
                            let details = printDetailedResultRow singleResults |> Seq.fold (fun str x -> str + x) ""
                            let r6 = "</tr>"
                            r1 + (i+1).ToString() + r2 + n + r3 + c + r4 + t.ToString() + r5 + details + r6)
                |> Seq.fold (fun str x -> str + x) ""
    let part4 = """</tbody></table><br/><br/><br/></div>"""
    part1 + part2 + part3 + sRes + part4

let buildResultHtml catResults =
    let takeBest = XmlConfig.GetSample().Cup.TakeBest
    let maxEvents = XmlConfig.GetSample().Cup.NumberOfEvents
    let htmlOpen = "<html>"
    let htmlClose = "</html>"
    let head = """<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /><link href="./result-template_files/default.css" rel="stylesheet" type="text/css" /><title>""" + XmlConfig.GetSample().Cup.Name + """</title></head>"""
    let bodyTop = """<body><h1>Rangliste</h1><br/><br/><div>Gewertet werden die <strong>""" + takeBest.ToString() + """</strong> besten Ergebnisse von <strong>""" + maxEvents.ToString() + """</strong>.</div><br/><br/><br/><br/>"""
    let bodyBottom = """<br/><div>(c) """ + System.DateTime.Now.Year.ToString() + """ by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein</div><br/><div>webmaster@solv.at</div><div>Erstellt: """ + System.DateTime.Now.ToString("R") + """</div></body>"""

    let cfgList = XmlConfig.GetSample().Classes |> Array.toList

    let catRes =
        [ for cfg in cfgList do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName 
            let _, catResult = catResults 
                                |> Seq.find(fun (catId, res) -> catId = cfg.Id)
            yield printResult classHeader catResult ]

    let catResString =
        catRes |> List.fold (fun str x -> str + x) ""
    htmlOpen + head + bodyTop + catResString + bodyBottom + htmlClose


