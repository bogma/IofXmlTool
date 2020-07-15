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
    open IofXmlLib.Types
    open IofXmlLib.XmlToolkit

    let toJson (inputFile : string) =

        let outputFile = Path.ChangeExtension(inputFile, "json")

        if (isNewer inputFile outputFile) then
            let content = File.ReadAllText(inputFile, System.Text.Encoding.UTF8)
            let doc = XDocument.Parse(content)
            let json = fromXml doc.Root
            printfn "write JSON %s" outputFile
            let enc = new UTF8Encoding(false);
            File.WriteAllText(outputFile, json.ToString(), enc)
        else
            printfn "no need to process JSON %s" inputFile

    let toUtf8 (inputFile : string) =

        let encName = getXmlEncoding inputFile
        if not (encName.ToLower().Equals("utf-8")) then
            let enc = CodePagesEncodingProvider.Instance.GetEncoding(encName)
            let content = File.ReadAllText(inputFile, enc)
            let doc = XDocument.Parse(content)
            let xws = new XmlWriterSettings()
            xws.Indent <- true
            xws.Encoding <- Encoding.UTF8
            use xw = XmlWriter.Create(inputFile, xws)
            printfn "write XML %s" inputFile
            doc.WriteTo(xw)
        else
            printfn "%s is already in UTF-8 encoding" inputFile

    let fromCSV (csvParams : IDictionary<string,string>) (inputFile : string) =
        let outputFile = Path.ChangeExtension(inputFile, "xml")
        if (isNewer inputFile outputFile) then
            File.Delete outputFile
            let res = parseResultCsv csvParams csvParams.["separator"] csvParams.["encoding"] inputFile
            let grouped = res |> List.groupBy (fun i -> i.ClassId)

            let currentTime = DateTime.UtcNow.ToString("s")
//            let defaultNS = XTNamespace "http://www.orienteering.org/datastandard/3.0"
            let doc =
                XTDocument (XTDeclaration "1.0" "UTF-8" "yes") [
                    XTElement "ResultList" [
                        //XTAttribute "xmlns" "http://www.orienteering.org/datastandard/3.0"
                        //XTAttributeNS "xsi" "xmlns" "http://www.w3.org/2001/XMLSchema-instance"
                        XTAttribute "iofVersion" "3.0"
                        XTAttribute "createTime" currentTime
                        XTAttribute "creator" "IofXmlTool"
                        for (crId, crList) in grouped do
                            XTElement "ClassResult" [
                                XTElement "Class" [
                                    XTElement "Id" [crId]
                                    XTElement "Name" [crList.[0].ClassName]
                                    XTElement "ShortName" [crList.[0].ClassNameShort]
                                ]
                                for pr in crList do
                                    XTElement "PersonResult" [
                                        XTElement "Person" [
                                            XTElement "Name" [
                                                XTElement "Family" pr.FamilyName
                                                XTElement "Given" pr.GivenName
                                            ]
                                            XTElement "Organisation" [
                                                XTElement "Id" [pr.OrganisationId]
                                                XTElement "Name" pr.OrganisationName
                                            ]
                                            XTElement "Result" [
                                                XTElement "Time" [pr.Time]
                                                XTElement "Position" [pr.Position]
                                                XTElement "Status" [pr.Status]
                                                XTElement "ControlCard" [pr.ControlCard]
                                            ]
                                        ]
                                    ]
                            ]
                    ]
                ]
             
            let xws = new XmlWriterSettings()
            xws.Indent <- true
            xws.Encoding <- Encoding.UTF8
            use xw = XmlWriter.Create(outputFile, xws)
            doc.WriteTo(xw)
            
            printfn "CSV parsed: %d entries" res.Length
        else
            printfn "no need to process CSV %s" inputFile


