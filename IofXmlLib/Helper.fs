namespace IofXmlLib

module Helper =

    open System
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open System.Xml
    open System.Xml.Linq

    open FSharp.Data

    open Types

    let runningTotal = List.scan (+) 0 >> List.tail

    let getIntervalList grouped =
        grouped
            |> Seq.map (fun pair -> snd pair |> Seq.length)
            |> Seq.toList

    let getPositionSeq startIndex intervalList =
        startIndex :: intervalList 
            |> runningTotal 
            |> List.toSeq
            |> Seq.take intervalList.Length

    let combineListToString = List.fold (fun str x -> str + x) ""
    
    let flatten x =
        [for event, c in x do
            for category, prrSeq in c do
               for prr in prrSeq do
                   yield event, category, prr]

    let flattenSeqOfSeq outer =
        seq { for inner in outer do
                 for s in inner do
                    yield s }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let rec getFiles dir regex pattern subdirs =
        seq { let files = Directory.EnumerateFiles(dir, pattern)
              let filtered = files |> Seq.filter (fun x -> 
                                                    let fn = Path.GetFileNameWithoutExtension(x)
                                                    Regex.IsMatch(fn, regex))
              ////printfn "#files = %d, #filtered = %d" (files |> Seq.length) (filtered |> Seq.length)
              ////printfn "filtered files: %A" filtered
              yield! filtered
              if subdirs then
                  for d in Directory.EnumerateDirectories(dir) do
                      yield! getFiles d regex pattern subdirs }

    // build all combinations of lenght n from list l
    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    // calc Levensthein distance
    let levDist (strOne : string) (strTwo : string) =
        let strOne = strOne.ToCharArray ()
        let strTwo = strTwo.ToCharArray ()
 
        let (distArray : int[,]) = Array2D.zeroCreate (strOne.Length + 1) (strTwo.Length + 1)
 
        for i = 0 to strOne.Length do distArray.[i, 0] <- i
        for j = 0 to strTwo.Length do distArray.[0, j] <- j
 
        for j = 1 to strTwo.Length do
            for i = 1 to strOne.Length do
                if strOne.[i - 1] = strTwo.[j - 1] then distArray.[i, j] <- distArray.[i - 1, j - 1]
                else
                    distArray.[i, j] <- List.min (
                        [distArray.[i-1, j] + 1; 
                        distArray.[i, j-1] + 1; 
                        distArray.[i-1, j-1] + 1]
                    )
        distArray.[strOne.Length, strTwo.Length]

    let sprintfTime (fmt:string) trimOptions time =
        
        let fmtTime = 
            if fmt.Contains("mmm") then
                "not implemented"
            else
                TimeSpan.FromSeconds(time).ToString(fmt)

        let trimmedTime = match trimOptions with
                          | Start -> fmtTime.TrimStart(' ',':','0')
                          | End -> fmtTime.TrimEnd('.', '0')
                          | Both -> fmtTime.TrimStart(' ',':','0').TrimEnd('.')

        if (time < 60.0) then
            if (time < 10.0) then
                sprintf "0:0%s" trimmedTime
            else
                sprintf "0:%s" trimmedTime
        else
            trimmedTime

    let formatSeconds2Time time =
        let f = floor time
        let r = time - f
        let ts = System.TimeSpan.FromSeconds(f)
        let h = 
            if ts.Hours > 0 then ts.Hours.ToString() + ":"
            else ""
        if r = 0.0 then
            h + ts.ToString(@"mm\:ss")
        else
            h + ts.ToString(@"mm\:ss") + sprintf "%.1f" r

    let recalcTeamPositions (teamResult : seq<MyTeamResult>) = 
        let totalGrouped = teamResult
                            |> Seq.sortBy (fun r -> r.TotalTime)
                            |> Seq.groupBy (fun r -> r.TotalTime)
        let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

        (totalPositions, totalGrouped) 
                       ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                       |> flattenSeqOfSeq

    let recalcCupPositions (classResult : seq<CupResult>) = 
        let totalGrouped = classResult
                            |> Seq.sortBy (fun cupResult -> -cupResult.TotalPoints)
                            |> Seq.groupBy (fun cupResult -> cupResult.TotalPoints)
        let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

        (totalPositions, totalGrouped) 
                       ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                       |> flattenSeqOfSeq

    let recalcSumPositions (classResult : seq<SumResult>) = 
        let totalGrouped = classResult
                            |> Seq.filter (fun x -> not (x.Disq))
                            |> Seq.sortBy (fun cupResult -> cupResult.TotalTime)
                            |> Seq.groupBy (fun cupResult -> cupResult.TotalTime)
        let totalPositions = getPositionSeq 1 (getIntervalList totalGrouped)

        (totalPositions, totalGrouped)
                       ||> Seq.map2 (fun i1 i2 -> snd i2 |> Seq.map (fun item -> i1, item))
                       |> flattenSeqOfSeq

    let explode (s:string) =
            [for c in s -> c]
    let implode (xs:char list) =
            let sb = System.Text.StringBuilder(xs.Length)
            xs |> List.iter (sb.Append >> ignore)
            sb.ToString()

    let rec triple l =
        match l with
            | x :: y :: z :: t -> (x,y,z) :: triple t
            | _ -> []

    let isSame (a : XmlResult.Id) (b : XmlResult.Id) =
        a.Type = b.Type && a.Value = b.Value

    let isSameOrg (y : ParsedResult) x = isSame x y.OrganisationId
    let isSameClass (y : ParsedResult) x = isSame x y.ClassId

    // filter results
    let filter fType filter comparer (input : IofXmlLib.Types.ParsedResult list) =
        match fType with
        | "include" -> 
            input |> List.filter (fun a -> List.exists (comparer a) filter)
        | "exclude" -> 
            input |> List.filter (fun a -> List.exists (comparer a) filter |> not)
        | _ -> input

    let getNamesById (cfg : IdNameInfo list) (fileInfo : IdNameInfo list) defaultValue (id : XmlResult.Id) =
        let n1 = cfg |> List.tryFind(fun x -> isSame x.Id id)
        let n2 = fileInfo |> List.tryFind(fun x -> isSame x.Id id)
        let nameFromResult = 
            match n2 with
            | Some n2 -> (n2.Name, n2.ShortName)
            | None -> (defaultValue, defaultValue)

        match n1 with
        | Some n1 -> 
            if n1.Name = "" then
                nameFromResult
            else
                (n1.Name, n1.ShortName)
        | None -> nameFromResult

    let rec fromXml (xml:XElement) =

      // Create a collection of key/value pairs for all attributes
      let attrs (xele:XElement) = 
        [ for attr in xele.Attributes() ->
            ("-" + attr.Name.LocalName, JsonValue.String attr.Value) ]

      // Function that turns a collection of XElement values
      // into an array of JsonValue (using fromXml recursively)
      let createArray xelems =
        [| for xelem in xelems -> fromXml xelem |]
        |> JsonValue.Array

      // Group child elements by their name and then turn all single-
      // element groups into a record (recursively) and all multi-
      // element groups into a JSON array using createArray
      let children =
        xml.Elements() 
        |> Seq.groupBy (fun x -> x.Name.LocalName)
        |> Seq.map (fun (key, childs) ->
            match Seq.toList childs with
            | [child] when child.HasElements -> key, fromXml child
            | [child] when not(child.HasElements) && not(child.HasAttributes) -> key, JsonValue.String child.Value
            | [child] when not(child.HasElements) && child.HasAttributes -> 
                let a = attrs child
                key, List.append a [("#text", JsonValue.String child.Value)] |> Array.ofList |> JsonValue.Record
            | children -> key, createArray children )

      // Concatenate elements produced for child elements & attributes
      let attrList = attrs xml
      if (isNull xml.Parent) then
        Array.append (Array.ofList attrList) [|(xml.Name.LocalName, (Array.ofSeq children |> JsonValue.Record))|] |> JsonValue.Record
      else
        Array.append (Array.ofList attrList) (Array.ofSeq children) |> JsonValue.Record

    let isNewer file1 file2 =
        if not(File.Exists(file1)) then
            false
        elif not(File.Exists(file2)) then
            true
        else
            let fi1 = FileInfo(file1)
            let fi2 = FileInfo(file2)
            fi1.LastWriteTime > fi2.LastWriteTime

    let getEncoding (encodingName: string) =
        let enc = CodePagesEncodingProvider.Instance.GetEncoding(encodingName)
        if isNull enc then
            let enc = Encoding.GetEncoding(encodingName)
            if isNull enc then
                Encoding.Default
            else
               enc
        else
            enc

    let getXmlEncoding xmlFile =

        let content = File.ReadAllText(xmlFile)
        use sr = new StringReader(content)
        let xrs = XmlReaderSettings()
        xrs.ConformanceLevel <- ConformanceLevel.Fragment
        use xr = XmlReader.Create(sr, xrs)
        
        if not (xr.Read()) then
            ""
        else
            xr.GetAttribute("encoding")

