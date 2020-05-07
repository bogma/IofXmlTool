module Helper

open IofXmlLib.Types
open IofXmlLib.Helper
open Types


let checkNameTypos (data:ResultData) = 
    match data.Result with
    | TeamResult tr -> printfn "no typo check on Team"
    | CupResult cr ->
        let classResults =
            cr
            |> Seq.groupBy (fun cupResult -> cupResult.ClassId.Value)
            |> Seq.map (fun (x,y) -> XmlResult.Id (None, string x),y)

        printfn "checking names with Levenshtein-distance <= 3"
        for cr in classResults do
            let catId = fst cr
            let cName, cShort = getNamesById data.ClassCfg data.ClassInfo "Unknown Class" catId
            printf "\tclass %s (%s)" cName cShort
            let cs = snd cr
                    |> Seq.map( fun cupResult -> cupResult.PersonName)
                    |> Seq.toList
                    |> comb 2
                    |> List.map (fun x -> x.[0], x.[1], levDist x.[0] x.[1])
                    |> List.filter (fun (a, b, x) -> x <= 3)

            if cs.Length > 0 then
                printfn "\t%A" cs
            else
                printfn "\tok"



