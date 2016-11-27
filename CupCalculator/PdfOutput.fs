module PdfOutput

open CupTypes
open Calc
open Helper

open MigraDoc.DocumentObjectModel
open MigraDoc.DocumentObjectModel.Tables
open MigraDoc.DocumentObjectModel.Shapes
open MigraDoc.Rendering

open System

let setStyles (document:Document) =
    // default style
    let style = document.Styles.[StyleNames.Normal]
    style.Font.Name <- "Segoe UI"

    // Heading1 to Heading9 are predefined styles with an outline level -> creates bookmark
    let style = document.Styles.[StyleNames.Heading1]
    style.Font.Name <- "Segoe UI Light"
    style.Font.Size <- new Unit(12.0)
    style.Font.Bold <- true
    style.Font.Color <- Colors.DarkBlue
    style.ParagraphFormat.PageBreakBefore <- false
    style.ParagraphFormat.SpaceBefore <- new Unit(6.0)
    style.ParagraphFormat.SpaceAfter <- new Unit(4.0)
    style.ParagraphFormat.KeepWithNext <- true

    let style = document.Styles.[StyleNames.Header]
    style.ParagraphFormat.AddTabStop(new Unit(16.0, UnitType.Centimeter), TabAlignment.Center) |> ignore

    let style = document.Styles.[StyleNames.Footer]
    style.ParagraphFormat.AddTabStop(new Unit(8.0, UnitType.Centimeter), TabAlignment.Center) |> ignore
    0

let defineContentSection (document:Document) =
    let section = document.AddSection()
    section.PageSetup.OddAndEvenPagesHeaderFooter <- false
    section.PageSetup.StartingNumber <- 1

//    section.PageSetup.TopMargin <- new Unit(15.0, UnitType.Millimeter)
//    section.PageSetup.BottomMargin <- new Unit(10.0, UnitType.Millimeter)
//    section.PageSetup.LeftMargin <- new Unit(5.0, UnitType.Millimeter)
//    section.PageSetup.RightMargin <- new Unit(5.0, UnitType.Millimeter)

    match Config.Output.Pdf.Orientation with
    | "Portrait" -> section.PageSetup.Orientation <- Orientation.Portrait
    | _ -> section.PageSetup.Orientation <- Orientation.Landscape

    let header = section.Headers.Primary

    let formattedText = new FormattedText()

    let paragraph = header.AddParagraph()
    let formattedText = paragraph.AddFormattedText("Rangliste - " + Config.Cup.Name)
    formattedText.Size <- new Unit(14.0)
    formattedText.Color <- Colors.DarkRed

    let formattedText = paragraph.AddFormattedText(" (Gewertet werden die " + Config.Cup.TakeBest.ToString() + " besten Ergebnisse von " + Config.Cup.NumberOfEvents.ToString() + ".)");
    formattedText.Size <- new Unit(10.0)
    formattedText.Color <- Colors.Black;

    let footer = section.Footers.Primary  

    let footerTxt = "(c) " + System.DateTime.Now.Year.ToString() + " by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein webmaster@solv.at Erstellt: " + System.DateTime.Now.ToString("R")
    let paragraph = footer.AddParagraph()

    paragraph.AddText(footerTxt) |> ignore

let printSingleDetailedResult eventResult =
    let formattedText = new FormattedText()
    let strategy = getCalcStrategy Config.Cup.CalcRule
    let pointsFormatted = strategy.FormatPoints eventResult.PRR.Points
    let tsString = formatSeconds2Time eventResult.PRR.Time

    if (eventResult.PRR.Status = "OK") then
        let format = 
             if eventResult.ResultCounts then TextFormat.Bold
             else TextFormat.Italic
        formattedText.AddFormattedText(pointsFormatted, format) |> ignore
        formattedText.AddFormattedText("\n") |> ignore
        formattedText.AddFormattedText(sprintf "%s (%i)" tsString eventResult.PRR.Position) |> ignore
        formattedText
    else
        let r = explode eventResult.PRR.Status |> List.filter (fun x -> Char.IsUpper(x)) |> implode
        formattedText.AddFormattedText(r) |> ignore
        formattedText

let printDetailedResultCells (results:seq<EventResult>) (row:Row) =              
    let races = [1..Config.Cup.NumberOfEvents] |> List.map (fun i -> Config.Cup.ResultFilePrefix + i.ToString("D2") + "_" + Config.Cup.Year.ToString())
    races |> List.iteri (fun i r ->
                             let cell = row.Cells.[i + 3]
                             let p = results |> Seq.filter (fun eventResult -> eventResult.EventFile = r)
                             if Seq.isEmpty p then 
                                 cell.AddParagraph("") |> ignore
                             else 
                                let eventResult = p |> Seq.take 1 |> Seq.exactlyOne
                                let txt = printSingleDetailedResult eventResult
                                let paragraph = cell.AddParagraph()
                                paragraph.Add(txt) |> ignore)

let addTable (document:Document) classHeader (classResult : seq<CupResult>) =
    document.LastSection.AddParagraph(classHeader, StyleNames.Heading1) |> ignore

    let table = new Table()
    table.Borders.Width <- new Unit(0.75)

    // add all columns
    let column = table.AddColumn(Unit.FromCentimeter(1.0))
    column.Format.Alignment <- ParagraphAlignment.Center
    let column = table.AddColumn(Unit.FromCentimeter(5.0))
    column.Format.Alignment <- ParagraphAlignment.Left
    let column = table.AddColumn(Unit.FromCentimeter(1.5))
    column.Format.Alignment <- ParagraphAlignment.Right
    [1..Config.Cup.NumberOfEvents] |> List.iteri (fun i x -> 
                                       let column = table.AddColumn(Unit.FromCentimeter(2.0))
                                       column.Format.Alignment <- ParagraphAlignment.Center)

    // add header row
    let row = table.AddRow()
    row.Shading.Color <- Colors.PaleGoldenrod
    let cell = row.Cells.[0]
    cell.AddParagraph("Pl") |> ignore
    let cell = row.Cells.[1]
    cell.AddParagraph("Name") |> ignore
    let cell = row.Cells.[2]
    cell.AddParagraph("Punkte") |> ignore
    [1..Config.Cup.NumberOfEvents] |> List.iteri (fun i x ->
                                       let cell = row.Cells.[i + 3]
                                       cell.AddParagraph(sprintf "%i. SC" x) |> ignore)

    recalcPositions classResult 
       |> Seq.iteri (fun i (rank, item) ->
                            let c = getClubNameById item.OrganisationId
                            let row = table.AddRow()

                            if (i % 2 = 0) then row.Shading.Color <- Colors.White
                            else row.Shading.Color <- Colors.LightGray

                            let strategy = getCalcStrategy Config.Cup.CalcRule
                            let totalFormated = strategy.FormatPoints item.TotalPoints

                            let cell = row.Cells.[0]
                            cell.AddParagraph(rank.ToString()) |> ignore
                            let cell = row.Cells.[1]
                            cell.AddParagraph(sprintf "%s\n%s" item.PersonName c) |> ignore
                            let cell = row.Cells.[2]
                            let para = cell.AddParagraph()
                            let txt = new FormattedText()
                            txt.AddFormattedText(totalFormated, TextFormat.Bold) |> ignore
                            para.Add(txt)
                            printDetailedResultCells item.Results row)
        |> ignore

    //table.SetEdge(0, 0, 3, 3, Edge.Box, BorderStyle.Single, new Unit(1.5), Colors.Black)
    document.LastSection.Add(table)

let buildResultPdf catResults (outputFile:string) =
    let doc = new Document()
    doc.Info.Title <- Config.Cup.Name
    
    setStyles doc |> ignore
    defineContentSection doc |> ignore

    let classCfg = Config.Classes |> Array.toList
    let catRes =
        [ for cfg in classCfg do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName
            let exists = catResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = catResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)
                yield addTable doc classHeader catResult ]
    
    let renderer = PdfDocumentRenderer(true)
    renderer.Document <- doc
    renderer.RenderDocument()
    renderer.PdfDocument.Save(outputFile)
    printfn "PDF output written to %s" outputFile