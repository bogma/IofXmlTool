namespace IofXmlLib

module PreProcessors =

    open System
    open System.Collections.Generic
    open System.IO
    open System.Text
    open System.Xml
    open System.Xml.Linq
    open IofXmlLib.Helper
    open IofXmlLib.CsvParser
    open IofXmlLib.Logging
    open IofXmlLib.XmlToolkit

    let toJson (inputFile : string) =

        let outputFile = Path.ChangeExtension(inputFile, "json")

        if (isNewer inputFile outputFile) then
            let content = File.ReadAllText(inputFile, System.Text.Encoding.UTF8)
            let doc = XDocument.Parse(content)
            let json = fromXml doc.Root
            tracer.Info "write JSON %s" outputFile
            let enc = UTF8Encoding(false);
            File.WriteAllText(outputFile, json.ToString(), enc)
        else
            tracer.Debug "no need to process JSON %s" inputFile

    let toUtf8 (inputFile : string) =

        let encName = getXmlEncoding inputFile
        if not (encName.ToLower().Equals("utf-8")) then
            let enc = CodePagesEncodingProvider.Instance.GetEncoding(encName)
            let content = File.ReadAllText(inputFile, enc)
            let doc = XDocument.Parse(content)
            let xws = XmlWriterSettings()
            xws.Indent <- true
            xws.Encoding <- Encoding.UTF8
            use xw = XmlWriter.Create(inputFile, xws)
            tracer.Info "write XML %s" inputFile
            doc.WriteTo(xw)
        else
            tracer.Debug "%s is already in UTF-8 encoding" inputFile

    let fromCSV (csvParams : IDictionary<string,string>) (inputFile : string) =
        let outputFile = Path.ChangeExtension(inputFile, "xml")

        try
            if (isNewer inputFile outputFile) then
                let res = parseResultCsv csvParams csvParams.["separator"] csvParams.["encoding"] inputFile

                let grouped = res |> List.groupBy (fun i -> i.ClassId)

                let currentTime = DateTime.UtcNow.ToString("s")

                let xmlns = XTNamespace "http://www.orienteering.org/datastandard/3.0"
                let xsins = XTNamespace "http://www.w3.org/2001/XMLSchema-instance"
                let xsiAttribute = XAttribute(XNamespace.Xmlns + "xsi", "http://www.w3.org/2001/XMLSchema-instance")

                let doc =
                    XTDocument (XTDeclaration "1.0" "UTF-8" "yes") [
                        XTElementNS "ResultList" xmlns.NamespaceName [
                            XTAttribute "xmlns" "http://www.orienteering.org/datastandard/3.0"
                            xsiAttribute
                            //XTAttributeNS "xsi" xmlns.NamespaceName "http://www.w3.org/2001/XMLSchema-instance"
                            XTAttribute "iofVersion" "3.0"
                            XTAttribute "createTime" currentTime
                            XTAttribute "creator" "IofXmlTool"
                            for (crId, crList) in grouped do
                                XTElementNS "ClassResult" xmlns.NamespaceName [
                                    XTElementNS "Class" xmlns.NamespaceName [
                                        XTElementNS "Id" xmlns.NamespaceName [crId]
                                        XTElementNS "Name" xmlns.NamespaceName [crList.[0].ClassName]
                                        XTElementNS "ShortName" xmlns.NamespaceName [crList.[0].ClassNameShort]
                                    ]
                                    for pr in crList do
                                        XTElementNS "PersonResult" xmlns.NamespaceName [
                                            XTElementNS "Person" xmlns.NamespaceName [
                                                XTElementNS "Name" xmlns.NamespaceName [
                                                    XTElementNS "Family" xmlns.NamespaceName [pr.FamilyName]
                                                    XTElementNS "Given" xmlns.NamespaceName [pr.GivenName]
                                                ]
                                            ]
                                            XTElementNS "Organisation" xmlns.NamespaceName [
                                                XTElementNS "Id" xmlns.NamespaceName [pr.OrganisationId]
                                                XTElementNS "Name"xmlns.NamespaceName [pr.OrganisationName]
                                            ]
                                            XTElementNS "Result" xmlns.NamespaceName [
                                                XTElementNS "Time" xmlns.NamespaceName [pr.Time]
                                                XTElementNS "Position" xmlns.NamespaceName [pr.Position]
                                                XTElementNS "Status" xmlns.NamespaceName [pr.Status]
                                                XTElementNS "ControlCard" xmlns.NamespaceName [pr.ControlCard]
                                            ]
                                        ]
                                ]
                        ]
                    ]

                File.Delete outputFile
                let xws = XmlWriterSettings()
                xws.Indent <- true
                xws.Encoding <- Encoding.UTF8
                use xw = XmlWriter.Create(outputFile, xws)
                doc.WriteTo(xw)

                tracer.Info "CSV parsed: %d entries" res.Length
            else
                tracer.Debug "no need to process CSV %s" inputFile
        with
        | ex -> tracer.WarnException ex "parsing %s failed" inputFile


