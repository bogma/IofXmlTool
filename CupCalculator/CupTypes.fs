module CupTypes

open System.IO
open FSharp.Data

type XmlConfig = XmlProvider<"resources/config.xml">
//type XmlCalculations = XmlProvider<"D:/ol/data/calculationRules.xml">

type XmlResult = XmlProvider<"resources/ResultListSampleData.xml">
//type CsvResult = CsvProvider<"resources/ResultListSampleData.csv">
//type HtmlResult = HtmlProvider<"resources/ResultListSampleData.html">

//type PersonResult = { GivenName : string; FamilyName : string; Organisation : string; Status : string; Time : int; Position : int; Points : int }
//type ClassResult = { ClassName : string; Results : PersonResult list }
//type EventResult = { Name : string; Date : System.DateTime; Results : ClassResult list }
//type Cup = { Title : string; Year : int; Results : EventResult list }

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
    Points : int;
    Time : int;
    Position : int
}