namespace IofXmlLib

module Parsing =

    open Helper
    open Types
    open System
    open System.IO
    open System.Text
    open FSharp.Data


    let parseResultXml (uri : string) : list<ParsedResult> =

        let getPosition (result : XmlResult.Result) =
            match result.Status with
            | "OK" -> result.Position |> Option.defaultValue "0" |> int
            | _ -> 0

        let getTime (result : XmlResult.Result) =
            match result.Status with
            | "OK" -> result.Time |> Option.defaultValue 0.0
            | _ -> 0.0

        let getTimeBehind (result : XmlResult.Result) =
            match result.Status with
            | "OK" -> result.TimeBehind |> Option.defaultValue 0.0
            | _ -> 0.0

        let enc = getXmlEncoding uri |> getEncoding
        let content = File.ReadAllText(uri, enc)
        printfn "parsing %s" uri
        let parsedXml = XmlResult.Parse(content)

        match parsedXml.ResultList with
        | None -> list.Empty
        | Some result ->
            [for classRes in result.ClassResults do
                for pr in classRes.PersonResults do
                    let r = pr.Results.[0]
                    let c = classRes.Class.Id
                    let o = pr.Organisation
                    match c, o with
                    | Some c, Some o ->
                        match o.Id with
                        | Some id ->
                        yield {
                            ClassId = c;
                            OrganisationId = id;
                            GivenName = pr.Person.Name.Given;
                            FamilyName = pr.Person.Name.Family;
                            Position = getPosition r;
                            Time = getTime r;
                            TimeBehind = getTimeBehind r;
                            Status = r.Status
                        }
                        | None -> ()
                    | _, _ -> ()]

    let parseResultCsv (fields: string) separator (encodingName: string) (uri: string) : list<ParsedResult> =

        let enc = getEncoding encodingName
        printfn "parsing %s" uri
        let parsedCsv = CsvFile.Load(uri, hasHeaders = true, separators = separator, encoding = enc, ignoreErrors = true)

        let getStatus =
            function
            | "0" -> "OK"
            | "1" -> "DidNotStart"
            | "2" -> "DidNotFinish"
            | "3" -> "MissingPunsh"
            | _ -> "unknown"
        
        let getPosition status (pos: string) =
            match status with
            | "OK" ->
                match Int32.TryParse(pos) with
                | true, x -> x
                | false, _ -> 0
            | _ -> 0

        let getTime status (time: string) =
            match status with
            | "OK" ->
                let ts = 
                    if (time.Length < 8) then
                        let t1 = sprintf "00:%s" time
                        TimeSpan.Parse(t1)
                    else
                        TimeSpan.Parse(time)
                ts.TotalSeconds
            | _ -> 0.0

        let fieldNames = fields.Split(separator)

        // csvFields="Wertung;AK;Vorname;Nachname;Zeit;Platz;Club-Nr.;Katnr"
        // 0 .. Status
        // 1 .. not competing (0 if competing)
        // 2 .. surename
        // 3 .. familyname
        // 4 .. time (in mm:ss or hh:mm:ss)
        // 5 .. pos
        // 6 .. club id
        // 7 .. class id
        [ for row in parsedCsv.Rows do
//            printfn "%s %s %s %s %s %s %s" row.[fieldNames.[0]] row.[fieldNames.[1]] row.[fieldNames.[2]] row.[fieldNames.[3]] row.[fieldNames.[4]] row.[fieldNames.[5]] row.[fieldNames.[6]]
            let notCompeting = row.[fieldNames.[1]]
            let status = 
                if notCompeting = "0" then
                    getStatus (row.[fieldNames.[0]])
                else
                    "NotCompeting"
            yield {
                ClassId = XmlResult.Id(None, row.[fieldNames.[7]]);
                OrganisationId = XmlResult.Id(None, row.[fieldNames.[6]]);
                GivenName = row.[fieldNames.[2]];
                FamilyName = row.[fieldNames.[3]];
                Position = getPosition status (row.[fieldNames.[5]]);
                Time = getTime status (row.[fieldNames.[4]]);
                TimeBehind = 0.0;
                Status = status
            }]

    let extractOrganisationInfo uris =
        uris
        |> Seq.collect (fun uri ->
            let enc = getXmlEncoding uri |> getEncoding
            let content = File.ReadAllText(uri, enc)
            let parsedXml = XmlResult.Parse(content)
            match parsedXml.ResultList with
            | None -> list.Empty
            | Some result ->
                let orgList = 
                    [for cr in result.ClassResults do
                        for pr in cr.PersonResults do
                            match pr.Organisation with
                            | Some o ->
                                match o.Id with
                                | Some id -> yield {Id = id; Name = o.Name; ShortName = o.ShortName |> Option.defaultValue ""}
                                | None -> ()
                            | None -> ()]
                orgList)
        |> Seq.distinctBy (fun x -> x.Id.Value)

    let extractClassInfo uris =
        uris
        |> Seq.collect (fun uri -> 
            let enc = getXmlEncoding uri |> getEncoding
            let content = File.ReadAllText(uri, enc)
            let parsedXml = XmlResult.Parse(content)
            match parsedXml.ResultList with
            | None -> list.Empty
            | Some result ->
                let classList = 
                    [for cr in result.ClassResults do
                        match cr.Class.Id with
                        | Some c -> yield {Id = c; Name = cr.Class.Name; ShortName = cr.Class.ShortName |> Option.defaultValue ""}
                        | None -> ()]
                classList)
        |> Seq.distinctBy (fun x -> x.Id.Value)

