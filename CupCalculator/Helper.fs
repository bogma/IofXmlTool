module Helper

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