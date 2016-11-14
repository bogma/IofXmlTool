module PdfOutput

open CupTypes
open Calc
open Helper
open ProgramSettings

open MigraDoc.DocumentObjectModel;
open MigraDoc.DocumentObjectModel.Tables;
open MigraDoc.DocumentObjectModel.Shapes;


let buildOutput catResults =
    let doc = new Document()
    doc.Info.Title <- (!cupName)

    let bodyTop = "Rangliste - Gewertet werden die" + (!takeBest).ToString() + "besten Ergebnisse von" + (!maxEvents).ToString() + "."
    let bodyBottom = "(c) " + System.DateTime.Now.Year.ToString() + " by solv.at | Daten: ANNE / oefol.at und der veranstaltende Verein webmaster@solv.at Erstellt: " + System.DateTime.Now.ToString("R")

    let section = doc.AddSection()

    let table = section.AddTable()
    table.Style <- "Table"
    table.Borders.Color <- TableBorder
    table.Borders.Width <- 0.25
    table.Borders.Left.Width <- 0.5
    table.Borders.Right.Width <- 0.5
    table.Rows.LeftIndent <- 0

    (*
    let catRes =
        [ for cfg in !classCfg do  
            let classHeader = sprintf "%s (%s)" cfg.Name cfg.DiplayName
            let exists = catResults |> Seq.exists(fun (catId, _) -> catId = cfg.Id)
            if exists then
                let _, catResult = catResults 
                                    |> Seq.find(fun (catId, res) -> catId = cfg.Id)
                yield printResult classHeader catResult ]
    
    let catResString = catRes |> combineListToString
    *)

    htmlOpen + head + bodyTop