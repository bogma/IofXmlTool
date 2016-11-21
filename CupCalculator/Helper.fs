module Helper

open CupTypes
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

let rec getFiles dir pattern subdirs =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          if subdirs then
              for d in Directory.EnumerateDirectories(dir) do
                  yield! getFiles d pattern subdirs }

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
