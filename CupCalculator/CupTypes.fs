module CupTypes

open System.IO
open FSharp.Data

type XmlConfig = XmlProvider<"resources/config.xml">
//type XmlCalculations = XmlProvider<"D:/ol/data/calculationRules.xml">

type XmlResult = XmlProvider<"resources/ResultListSampleData.xml">
//type CsvResult = CsvProvider<"resources/ResultListSampleData.csv">
//type HtmlResult = HtmlProvider<"resources/ResultListSampleData.html">

let version = "1.0.0.0"
let debugMode = ref false

let classCfgIds : int list ref = ref List.Empty
let orgCfgIds : int list ref = ref List.Empty

let mutable Config : XmlConfig.CupConfiguration = XmlConfig.Load("./resources/config.xml")


type ParsedResult = {
    ClassId : int;
    OrganisationId : int;
    GivenName : string;
    FamilyName : string;
    Position : int;
    Time : int;
    Status : string
}

type PersonalRaceResult = {
    OrganisationId : int;
    Name : string;
    Points : decimal;
    Time : int;
    Position : int
}

type EventResult = {
    EventFile : string;
    ClassId : int;
    PRR : PersonalRaceResult;
    ResultCounts : bool
}

type CupResult = {
    PersonName : string;
    ClassId : int;
    OrganisationId : int;
    TotalPoints : decimal;
    Results : seq<EventResult>
}