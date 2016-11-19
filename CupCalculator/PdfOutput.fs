﻿module PdfOutput

open CupTypes
open Calc
open Helper
open ProgramSettings

open MigraDoc.DocumentObjectModel
open MigraDoc.DocumentObjectModel.Tables
open MigraDoc.DocumentObjectModel.Shapes
open MigraDoc.Rendering

let setStyles (document:Document) =
    // default style
    let style = document.Styles.[StyleNames.Normal]
    style.Font.Name <- "Segoe UI"

    // Heading1 to Heading9 are predefined styles with an outline level -> creates bookmark
    let style = document.Styles.[StyleNames.Heading1]
    style.Font.Name <- "Segoe UI Light"
    style.Font.Size <- new Unit(16.0)
    style.Font.Bold <- true
    style.Font.Color <- Colors.DarkBlue
    style.ParagraphFormat.PageBreakBefore <- true
    style.ParagraphFormat.SpaceAfter <- new Unit(6.0)
    style.ParagraphFormat.KeepWithNext <- true

    let style = document.Styles.[StyleNames.Heading2]
    style.Font.Size <- new Unit(14.0)
    style.Font.Bold <- true
    style.ParagraphFormat.PageBreakBefore <- false
    style.ParagraphFormat.SpaceBefore <- new Unit(6.0)
    style.ParagraphFormat.SpaceAfter <- new Unit(6.0)

    let style = document.Styles.[StyleNames.Header]
    style.ParagraphFormat.AddTabStop(new Unit(16.0, UnitType.Centimeter), TabAlignment.Right) |> ignore

    let style = document.Styles.[StyleNames.Footer]
    style.ParagraphFormat.AddTabStop(new Unit(8.0, UnitType.Centimeter), TabAlignment.Right) |> ignore
    0

let head (document:Document) =
    let section = document.AddSection()
    section.PageSetup.Orientation <- Orientation.Landscape
    let paragraph = section.AddParagraph()
    paragraph.Format.SpaceAfter <- new Unit(3.0, UnitType.Centimeter)

    //let image = section.AddImage("../../../../assets/images/Logo landscape.png")
    //image.Width <- new Unit(10.0, UnitType.Centimeter)
    
    let paragraph = section.AddParagraph("Rangliste - " + (!cupName));
    paragraph.Format.Font.Size <- new Unit(16.0)
    paragraph.Format.Font.Color <- Colors.DarkRed;

    let paragraph = section.AddParagraph("Gewertet werden die " + (!takeBest).ToString() + " besten Ergebnisse von " + (!maxEvents).ToString() + ".");
    paragraph.Format.Font.Size <- new Unit(12.0)
    paragraph.Format.Font.Color <- Colors.DarkRed;

    let paragraph = section.AddParagraph("Erstellt am: ")
    paragraph.AddDateField() |> ignore
    0

let defineFooter (document:Document) =
    let section = document.AddSection()
    let paragraph = section.AddParagraph()

    let footer = "(c) " + System.DateTime.Now.Year.ToString() + " by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein webmaster@solv.at Erstellt: " + System.DateTime.Now.ToString("R")

    let paragraph = section.Footers.Primary.AddParagraph();
    paragraph.AddText(footer) |> ignore
    paragraph.Format.Font.Size <- new Unit(4.0)
    paragraph.Format.Alignment <- ParagraphAlignment.Center
    0

let printSingleDetailedResult points time pos counts =
//    let p1 = 
//        if counts then normal
//        else strikethrough // + points i.e 100,00
    let strategy = getCalcStrategy !calcRule
    let pointsFormated = strategy.FormatPoints points
    let tsString = formatSeconds2Time time
    sprintf "%s\n%s (%i)" pointsFormated tsString pos

let printDetailedResultCells results (row:Row) =              
    let races = [1..!maxEvents] |> List.map (fun i -> (!resultFilePrefix) + i.ToString("D2") + "_" + (!year).ToString())
    races |> List.iteri (fun i r ->
                             let cell = row.Cells.[i + 3]
                             let p = results |> Seq.filter (fun (file, _, _, _) -> file = r)
                             if Seq.isEmpty p then 
                                 cell.AddParagraph("") |> ignore
                             else 
                                let _, _, prr, counts = p |> Seq.take 1 |> Seq.exactlyOne
                                let txt = printSingleDetailedResult prr.Points prr.Time prr.Position counts
                                cell.AddParagraph(txt) |> ignore)

let addTable (document:Document) classHeader (catResult : seq<string * 'a * int * decimal * seq<string * 'c * PersonalRaceResult * bool>>) =
    document.LastSection.AddParagraph(classHeader, StyleNames.Heading2) |> ignore

    let table = new Table()
    table.Borders.Width <- new Unit(0.75)

    // add all columns
    let column = table.AddColumn(Unit.FromCentimeter(1.0))
    column.Format.Alignment <- ParagraphAlignment.Center
    let column = table.AddColumn(Unit.FromCentimeter(5.0))
    column.Format.Alignment <- ParagraphAlignment.Left
    let column = table.AddColumn(Unit.FromCentimeter(2.0))
    column.Format.Alignment <- ParagraphAlignment.Right
    [1..!maxEvents] |> List.iteri (fun i x -> 
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
    [1..!maxEvents] |> List.iteri (fun i x ->
                                       let cell = row.Cells.[i + 3]
                                       cell.AddParagraph(sprintf "%i. SC" x) |> ignore)

    let totalGrouped = catResult
                        |> Seq.sortBy (fun (_, _, _, total, _) -> -total)
                        |> Seq.groupBy (fun (_, _, _, total, _) -> total)
    let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

    let res = (totalPositions, totalGrouped) 
                   ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                   |> flattenSeqOfSeq

    res |> Seq.iteri (fun i (rank, item) ->
                            let name, cat, club, total, singleResults = item
                            let c = getClubNameById (!orgCfg) club
                            let row = table.AddRow()

                            if (i % 2 = 0) then row.Shading.Color <- Colors.White
                            else row.Shading.Color <- Colors.LightGray

                            let strategy = getCalcStrategy !calcRule
                            let totalFormated = strategy.FormatPoints total

                            let cell = row.Cells.[0]
                            cell.AddParagraph(rank.ToString()) |> ignore
                            let cell = row.Cells.[1]
                            cell.AddParagraph(sprintf "%s\n%s" name c) |> ignore
                            let cell = row.Cells.[2]
                            cell.AddParagraph(totalFormated) |> ignore
                            printDetailedResultCells singleResults row)
        |> ignore

    //table.SetEdge(0, 0, 3, 3, Edge.Box, BorderStyle.Single, new Unit(1.5), Colors.Black)
    document.LastSection.Add(table)

let buildResultPdf catResults (outputFile:string) =
    let TableBorder = new Color(81uy, 125uy, 192uy)
    let TableBlue = new Color(235uy, 240uy, 249uy)
    let TableGray = new Color(242uy, 242uy, 242uy)

    let doc = new Document()
    doc.Info.Title <- (!cupName)
    
    setStyles doc |> ignore
    head doc |> ignore
    defineFooter doc |> ignore

    let catRes =
        [ for cfg in !classCfg do  
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