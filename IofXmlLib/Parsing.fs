namespace IofXmlLib

module Parsing =

    open Types
    open System.IO

    let getPosition (result : XmlResult.Result) =
        match result.Status with
        | "OK" -> result.Position |> Option.defaultValue "0" |> int
        | _ -> 0

    let getTime (result : XmlResult.Result) =
        match result.Status with
        | "OK" -> result.Time |> Option.defaultValue 0.0 |> int
        | _ -> 0

    let parseResultXml (uri : string) : list<ParsedResult>=
        let content = File.ReadAllText(uri, System.Text.Encoding.UTF7)
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
                            Status = r.Status
                        }
                        | None -> ()
                    | _, _ -> ()]

    let extractOrganisationInfo files =
        files 
        |> Seq.collect (fun uri -> 
            let content = File.ReadAllText(uri, System.Text.Encoding.UTF7)
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

    let extractClassInfo files =
        files 
        |> Seq.collect (fun uri -> 
            let content = File.ReadAllText(uri, System.Text.Encoding.UTF7)
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

