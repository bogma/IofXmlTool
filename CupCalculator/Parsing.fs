module Parsing

open CupTypes
open FSharp.Data
open FSharp.Data.CsvExtensions
open System.IO

let getPosition (result : XmlResult.Result) =
    match result.Status with
        | "OK" -> result.Position
        | _ -> 0

let getTime (result : XmlResult.Result) =
    match result.Status with
        | "OK" -> result.Time
        | _ -> 0

let parseResultXml (uri : string) : list<ParsedResult>=
    let content = File.ReadAllText(uri, System.Text.Encoding.UTF8)
    let info = XmlResult.Parse(content)
    printfn "parsing %s" uri
    [for classRes in info.ClassResults do
        for pr in classRes.PersonResults do
            let r =
                {
                    ClassId = classRes.Class.Id;
                    OrganisationId = pr.Organisation.Id;
                    GivenName = pr.Person.Name.Given;
                    FamilyName = pr.Person.Name.Family;
                    Position = getPosition pr.Result;
                    Time = getTime pr.Result;
                    Status = pr.Result.Status
                }
            yield r]
(*
let toSeconds (timeString : string) =
    let z1 = timeString.Split([|":"|], System.StringSplitOptions.None) |> Array.rev
    match z1.Length with
        | 3 -> z1.[0].AsInteger() + z1.[1].AsInteger() * 60 + z1.[2].AsInteger() * 60 * 60
        | 2 -> z1.[0].AsInteger() + z1.[1].AsInteger() * 60
        | 1 -> 0
        | _ -> 0

let parseResultCsv (uri : string) : list<ParsedResult>=
    let info = CsvFile.Load(uri, separators=";", quote='"', hasHeaders=true)
    printfn "parsing %s" uri
    [for row in info.Rows do
        printf "%A" row
        let ts = toSeconds (row.GetColumn "Zeit")
        let n = row.GetColumn "Nachname"
        printf "%i %s" ts n
        let r =
            {
                ClassId = row.["Katnr"].AsInteger();
                OrganisationId = row.["Club-Nr."].AsInteger();
                GivenName = row.["Vorname"];
                FamilyName = row.["Nachname"];
                Position = row.["Platz"].AsInteger();
                Time = ts;
                Status = match ts with
                            | 0 -> "NOK"
                            | _ -> "OK"
            }
        yield r]

let parseResultHtml (uri : string) : list<ParsedResult>=
    let result = HtmlDocument.Load(uri)
    printfn "parsing %s" uri
    let tables = result.Descendants ["table"]
    [for table in tables do
         if table.ToString().Contains("Herren 35-") then
            let r =
                {
                    ClassId = 1;
                    OrganisationId = 1;
                    GivenName = "Tom";
                    FamilyName = "Turbo";
                    Position = 1;
                    Time = 100;
                    Status = "OK"
                }
            yield r]
*)