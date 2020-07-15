namespace IofXmlLib

module XmlToolkit =
    open System.Text
    open System.Xml
    open System.Xml.Linq
    open System.IO
 
    let XTDeclaration version encoding standalone = XDeclaration(version, encoding, standalone)
    let XTLocalName localName namespaceName = XName.Get(localName, namespaceName)
    let XTName expandedName = XName.Get(expandedName)
    let XTDocument xdecl content = XDocument(xdecl, content |> Seq.map (fun v -> v :> obj) |> Seq.toArray)
    let XTComment (value:string) = XComment(value) :> obj
    let XTElementNS localName namespaceName content = XElement(XTLocalName localName namespaceName, content |> Seq.map (fun v -> v :> obj) |> Seq.toArray) :> obj
    let XTElement expandedName content = XElement(XTName expandedName, content |> Seq.map (fun v -> v :> obj) |> Seq.toArray) :> obj
    let XTAttributeNS localName namespaceName value = XAttribute(XTLocalName localName namespaceName, value) :> obj
    let XTAttribute expandedName value = XAttribute(XTName expandedName, value) :> obj
    let XTNamespace uri = XNamespace.Get uri
 
    type XDocument with
        /// Saves the XML document to a MemoryStream using UTF-8 encoding, indentation and character checking.
        member doc.Save() =
            let ms = new MemoryStream()
            use xtw = XmlWriter.Create(ms, XmlWriterSettings(Encoding = Encoding.UTF8, Indent = true, CheckCharacters = true))
            doc.Save(xtw)
            ms.Position <- 0L
            ms

