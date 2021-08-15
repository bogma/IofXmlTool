namespace IofXmlLib

module Calc =

    type RuleDefinition = {
        Name : string
        Formatting : string
        Code : string
    }

    let r1 = {
        Name = "time";
        Formatting = "%.2f";
        Code = "let calcPointsFromTime winningTime time _ =
        let p = 100m - (((time - winningTime) / winningTime) * 50m)
        if p < 0m then 0m
        else p" }
    let r2 = { 
        Name = "position";
        Formatting = "%.0f";
        Code = "let calcPointsFromPosition _ _ pos =
        match pos with
        | 1 -> 25m
        | 2 -> 20m
        | 3 -> 16m
        | 4 -> 13m
        | 5 -> 11m
        | 6 -> 10m
        | 7 -> 9m
        | 8 -> 8m
        | 9 -> 7m
        | 10 -> 6m
        | 11 -> 5m
        | 12 -> 4m
        | 13 -> 3m
        | 14 -> 2m
        | 15 -> 1m
        | _ -> 0m" }

    type CalculationRule(calcFunction, format, ruleDefinition) =
        member this.Execute wt rt pos = calcFunction wt rt pos
        member this.FormatPoints (points : decimal) = sprintf format points
        member this.RuleName = ruleDefinition.Name
        member this.RuleFunction = ruleDefinition.Code.Substring(4, ruleDefinition.Code.IndexOf(" ", 4) - 4)
        member this.RuleFormat = ruleDefinition.Formatting
        member this.RuleCode = ruleDefinition.Code

    let calcPointsFromPosition _ _ pos =
        match pos with
        | 1 -> 25m
        | 2 -> 20m
        | 3 -> 16m
        | 4 -> 13m
        | 5 -> 11m
        | 6 -> 10m
        | 7 -> 9m
        | 8 -> 8m
        | 9 -> 7m
        | 10 -> 6m
        | 11 -> 5m
        | 12 -> 4m
        | 13 -> 3m
        | 14 -> 2m
        | 15 -> 1m
        | _ -> 0m
    
    let calcPointsFromTime winningTime time _ =
        let p = 100m - (((time - winningTime) / winningTime) * 50m)
        if p < 0m then 0m
        else p

    let sum _ time _ =
        time

    let getCalcStrategy calcRule =
        match calcRule with
            | "time" -> Some (CalculationRule(calcPointsFromTime, "%.2f", { Name = "time"; Formatting = "%.2f"; Code = "let calcPointsFromTime winningTime time _ =
                let p = 100m - (((time - winningTime) / winningTime) * 50m)
                if p < 0m then 0m
                else p" }))
            | "position" -> Some (CalculationRule(calcPointsFromPosition, "%.0f", { Name = "position"; Formatting = "%.0f"; Code = "let calcPointsFromPosition _ _ pos =
                match pos with
                | 1 -> 25m
                | 2 -> 20m
                | 3 -> 16m
                | 4 -> 13m
                | 5 -> 11m
                | 6 -> 10m
                | 7 -> 9m
                | 8 -> 8m
                | 9 -> 7m
                | 10 -> 6m
                | 11 -> 5m
                | 12 -> 4m
                | 13 -> 3m
                | 14 -> 2m
                | 15 -> 1m
                | _ -> 0m" }))
            | "sum" -> Some (CalculationRule(sum, "%.2f", { Name = "sum"; Formatting = "%.2f"; Code = "let sum _ time _ =
                time" }))
            | _ -> None

    let getNames _ =
        ["time"; "position"; "sum"]

    


