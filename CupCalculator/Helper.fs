module Helper

open CupTypes

open System
open System.IO
open System.Xml.Linq
open FSharp.Data
open System.Xml

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

let flattenSeqOfSeq outer =
    seq { for inner in outer do
             for s in inner do
                yield s }

let rec getFiles dir pattern subdirs =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          if subdirs then
              for d in Directory.EnumerateDirectories(dir) do
                  yield! getFiles d pattern subdirs }


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
        | children -> key + "s", createArray children )

  // Concatenate elements produced for child elements & attributes
  let attrList = attrs xml
  Array.append (Array.ofList attrList) (Array.ofSeq children)
    |> JsonValue.Record
  
let toJson (inputFile : string) =    

    let isNewer file1 file2 =
        if not(File.Exists(file1)) then
            false
        elif not(File.Exists(file2)) then
            true
        else
            let fi1 = FileInfo(file1)
            let fi2 = FileInfo(file2)
            fi1.LastWriteTime > fi2.LastWriteTime
    let getDoc x =
        let doc = XDocument.Parse(x)
        doc

    let outputFile = Path.ChangeExtension(inputFile, "json")
    if (isNewer inputFile outputFile) then
        let content = File.ReadAllText(inputFile, System.Text.Encoding.UTF7)
        let doc = XDocument.Parse(content)
        let json = fromXml doc.Root
        printfn "write JSON %s" outputFile
        File.WriteAllText(outputFile, json.ToString(), System.Text.Encoding.UTF8)
    else
        printfn "no need to write JSON %s" outputFile
    //let fileName = Path.GetFileNameWithoutExtension(inputFile)
//    let json = JsonConvert.SerializeObject(results)
//    File.WriteAllText(outputFile, json, Encoding.UTF8)

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

let getClubNameById (id:int) =
    let n = Config.Organisations |> Array.toList |> List.find(fun x -> x.Id = id)
    n.Name

let getClassNameById (id:int) =
    let n = Config.Classes |> Array.toList |> List.find(fun x -> x.Id = id)
    n.Name

let formatSeconds2Time time =
    let t1 = float time
    let ts = System.TimeSpan.FromSeconds(t1)
    let h = 
        if ts.Hours > 0 then ts.Hours.ToString() + ":"
        else ""
    h + ts.ToString(@"mm\:ss")

let recalcPositions (classResult : seq<CupResult>) = 
    let totalGrouped = classResult
                        |> Seq.sortBy (fun cupResult -> -cupResult.TotalPoints)
                        |> Seq.groupBy (fun cupResult -> cupResult.TotalPoints)
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

let getStatusText s =
    let translations = Config.Translations
    let t = translations |> Array.filter (fun x -> x.Key.Contains(s))
    if (t.Length > 0) then
        t.[0].Translate
    else
        explode s |> List.filter (fun x -> Char.IsUpper(x)) |> implode
