namespace IofXmlLib

module XmlParser =

    open Helper
    open Types
    open System.IO

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

