module Helper

open System.IO

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

let rec getFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getFiles d pattern }