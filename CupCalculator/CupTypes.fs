module CupTypes

open System.IO
open FSharp.Data

type XmlConfig = XmlProvider<"../../ol/data/config.xml">
//type XmlCalculations = XmlProvider<"D:/ol/data/calculationRules.xml">

type XmlResult = XmlProvider<"../../ol/data/ResultListSampleData.xml">
type CsvResult = CsvProvider<"../../ol/data/ResultListSampleData.csv">
type HtmlResult = HtmlProvider<"../../ol/data/ResultListSampleData.html">

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
