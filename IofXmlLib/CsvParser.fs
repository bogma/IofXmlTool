namespace IofXmlLib

module CsvParser =
    
    open System
    open System.Collections.Generic
    open FSharp.Data
    open IofXmlLib.Helper

    type ParsedCsvResult = {
        ClassId : int
        ClassName : string
        ClassNameShort : string
        OrganisationId : int
        OrganisationName : string
        ControlCard : string
        GivenName : string
        FamilyName : string
        Position : int
        ////Start : DateTime
        ////Finish : DateTime
        Time : float
        Status : string
    }

    let parseResultCsv (fields: IDictionary<string,string>) separator (encodingName: string) (uri: string) : list<ParsedCsvResult> =

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

        [ for row in parsedCsv.Rows do
            let notCompeting = row.[fields.["notCompeting"]]
            let status = 
                if notCompeting = "0" then
                    getStatus (row.[fields.["status"]])
                else
                    "NotCompeting"
            yield {
                ClassId = int row.[fields.["classId"]];
                ClassName = row.[fields.["className"]];
                ClassNameShort = row.[fields.["classNameShort"]];
                OrganisationId = int row.[fields.["orgId"]];
                OrganisationName = row.[fields.["orgName"]];
                ControlCard = row.[fields.["controlCard"]];
                GivenName = row.[fields.["givenName"]];
                FamilyName = row.[fields.["familyName"]];
                Position = getPosition status (row.[fields.["position"]]);
                Time = getTime status (row.[fields.["time"]]);
                Status = status
            }]

