module Types

open IofXmlLib.Types
open FSharp.Data

type XmlConfig = XmlProvider<Schema="./res/config.xsd", EmbeddedResource="IofXTool, IofXTool.res.config.xsd">
//type XmlConfig = XmlProvider<"res/config.xml">

type ResultType =
    | TeamResult of (XmlResult.Id * MyTeamResult list) seq
    | CupResult of CupResult seq
    | SumResult of SumResult seq

type ResultData = {
        InputPath : string
        Config : XmlConfig.Configuration
        ClassCfg : IdNameInfo list
        ClassInfo : IdNameInfo list
        OrgCfg : IdNameInfo list
        OrgInfo : IdNameInfo list
        Result : ResultType
    }
