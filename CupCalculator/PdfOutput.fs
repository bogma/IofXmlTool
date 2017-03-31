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
    style.Font.Size <- new Unit(8.0)

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
    section.PageSetup.LeftMargin <- new Unit(10.0, UnitType.Millimeter)
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

    let cultureInfo = System.Globalization.CultureInfo("de-AT")
    let footerTxt = "(c) " + System.DateTime.Now.Year.ToString() + " by SOLV | Erstellt: " + System.DateTime.Now.ToString("R", cultureInfo)
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
        formattedText.AddFormattedText(sprintf " / %s (%i)" tsString eventResult.PRR.Position) |> ignore
        formattedText
    else
        let r = getStatusText eventResult.PRR.Status
        formattedText.AddFormattedText(r) |> ignore
        formattedText

let printDetailedResultCells (results:seq<EventResult>) (row:Row) =
    let races = [1..Config.Cup.NumberOfEvents] |> List.map (fun i -> Config.Cup.ResultFilePrefix + i.ToString("D2") + "_" + Config.Cup.Year.ToString())
    races |> List.iteri (fun i r ->
                             let cell = row.Cells.[i + 4]
                             let p = results |> Seq.filter (fun eventResult -> eventResult.EventFile = r)
                             if Seq.isEmpty p then 
                                 cell.AddParagraph("") |> ignore
                             else 
                                let eventResult = p |> Seq.take 1 |> Seq.exactlyOne
                                let txt = printSingleDetailedResult eventResult
                                let paragraph = cell.AddParagraph()
                                paragraph.Add(txt) |> ignore)

let createTable (document:Document) =
    let table = new Table()
    table.Borders.Width <- new Unit(0.75)
        
    // add all columns:
    // overall position
    let column = table.AddColumn(Unit.FromCentimeter(0.7))
    column.Format.Alignment <- ParagraphAlignment.Center
    // name
    let column = table.AddColumn(Unit.FromCentimeter(3.8))
    column.Format.Alignment <- ParagraphAlignment.Left
    // club
    let columnWidth =
        if Config.Output.Pdf.Orientation = "Portrait" then 4.0
        else 2.5
    let column = table.AddColumn(Unit.FromCentimeter(columnWidth))
    column.Format.Alignment <- ParagraphAlignment.Left
    // overall points
    let column = table.AddColumn(Unit.FromCentimeter(1.0))
    column.Format.Alignment <- ParagraphAlignment.Right
    // cups
    [1..Config.Cup.NumberOfEvents] |> List.iteri (fun i x ->
                                       // race details
                                       let column = table.AddColumn(Unit.FromCentimeter(2.2))
                                       column.Format.Alignment <- ParagraphAlignment.Center)

    // add header row
    let row = table.AddRow()
    row.Shading.Color <- Colors.PaleGoldenrod

    let cell = row.Cells.[0]
    cell.MergeDown <- 1
    cell.VerticalAlignment <- VerticalAlignment.Center
    cell.AddParagraph("Rg") |> ignore
    let cell = row.Cells.[1]
    cell.MergeDown <- 1
    cell.VerticalAlignment <- VerticalAlignment.Center
    cell.AddParagraph("Name") |> ignore
    let cell = row.Cells.[2]
    cell.MergeDown <- 1
    cell.VerticalAlignment <- VerticalAlignment.Center
    cell.AddParagraph("Verein") |> ignore
    let cell = row.Cells.[3]
    cell.MergeDown <- 1
    cell.VerticalAlignment <- VerticalAlignment.Center
    cell.AddParagraph("Pkte") |> ignore
    [1..Config.Cup.NumberOfEvents] |> List.iteri (fun i x ->
                                       let cell = row.Cells.[i + 4]
                                       cell.AddParagraph(sprintf "%i. Sbg.Cup" x) |> ignore)

    let row = table.AddRow()
    row.Shading.Color <- Colors.PaleGoldenrod
    row.Cells.[0].MergeRight <- 3
    [1..Config.Cup.NumberOfEvents] |> List.iteri (fun i x ->
                                       let cell = row.Cells.[i + 4]
                                       cell.AddParagraph("Pkte / Zeit (Rg)") |> ignore)


    //table.SetEdge(0, 0, 3, 3, Edge.Box, BorderStyle.Single, new Unit(1.5), Colors.Black)
    document.LastSection.Add(table)
    table

let addCategoryResult (table:Table) classHeader (classResult : seq<CupResult>) =
    let blankRow= table.AddRow()
    let cell = blankRow.Cells.[0]
    cell.MergeRight <- Config.Cup.NumberOfEvents + 3
    let paragraph = cell.AddParagraph()
    let formattedText = paragraph.AddFormattedText("", TextFormat.Bold)
    formattedText.Size <- new Unit(3.0)
    
    let row = table.AddRow()
    let cell = row.Cells.[1]
    cell.MergeRight <- Config.Cup.NumberOfEvents + 2
    let paragraph = cell.AddParagraph()
    let formattedText = paragraph.AddFormattedText(classHeader, TextFormat.Bold)
    formattedText.Size <- new Unit(10.0)
    formattedText.Color <- Colors.DarkBlue;

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
                            cell.AddParagraph(item.PersonName) |> ignore
                            let cell = row.Cells.[2]
                            cell.AddParagraph(c) |> ignore

                            let cell = row.Cells.[3]
                            let para = cell.AddParagraph()
                            let txt = new FormattedText()
                            txt.AddFormattedText(totalFormated, TextFormat.Bold) |> ignore
                            para.Add(txt)
                            printDetailedResultCells item.Results row)
       |> ignore


let buildResultPdf catResults (outputFile:string) =
    let doc = new Document()
    doc.Info.Title <- Config.Cup.Name
    
    setStyles doc |> ignore
    defineContentSection doc |> ignore

    let mutable resultTable = createTable doc
    let mutable lines = 0
    let brake =
        if Config.Output.Pdf.Orientation = "Portrait" then 59
        else 37

    let classCfg = Config.Classes |> Array.toList
    let catRes =
        [ for cfg in classCfg do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName
            let exists = catResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = catResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)
                lines <- lines + 2 + (catResult |> Seq.length)
                if lines > brake then
                    doc.LastSection.AddPageBreak()
                    resultTable <- createTable doc
                    lines <- 2 + (catResult |> Seq.length)
                yield addCategoryResult resultTable classHeader catResult ]
    
    let renderer = PdfDocumentRenderer(true)
    renderer.Document <- doc
    renderer.RenderDocument()
    renderer.PdfDocument.Save(outputFile)
    printfn "PDF output written to %s" outputFile