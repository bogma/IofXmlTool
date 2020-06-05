namespace IofXmlLib

module CustomConverter =
    open FSharp.Data
    open Newtonsoft.Json
    type private XmlResult = XmlProvider<Schema="./datastandard_IOF_30.xsd", EmbeddedResource="IofXmlLib, IofXmlLib.datastandard_IOF_30.xsd">
    type XmlResultIdConverter() =
        inherit JsonConverter()

            override __.CanConvert(t) = t.GetType() = typedefof<XmlResult.Id>

            override __.WriteJson(writer, v, serializer) =
                let value = (v :?> XmlResult.Id).Value
                serializer.Serialize(writer, value)

            override __.ReadJson(reader, t, existingValue, serializer) =
                XmlResult.Id(None, "dummy") :> obj

module Types =

    open FSharp.Data
    open Newtonsoft.Json

    type XmlRules = XmlProvider<"calculation_rules.xml", EmbeddedResource="IofXmlLib, IofXmlLib.calculation_rules.xml">
    type XmlResult = XmlProvider<Schema="./datastandard_IOF_30.xsd", EmbeddedResource="IofXmlLib, IofXmlLib.datastandard_IOF_30.xsd">

    type IofClassId = XmlResult.Id
    type IofOrganisationId = XmlResult.Id

    type ParsedResult = {
        ClassId : XmlResult.Id
        OrganisationId : XmlResult.Id
        GivenName : string
        FamilyName : string
        Position : int
        Time : float
        TimeBehind : float
        Status : string
    }

    type SimplePersonalRaceResult = {
        Name : string
        Time : float
    }

    type MyTeamResult = {
        OrganisationId : XmlResult.Id
        TotalTime : float
        TeamMembers : SimplePersonalRaceResult list
    }

    type IdNameInfo = {
        Id : XmlResult.Id
        Name : string
        ShortName: string
    }

    type VirtualClass = {
        Id : XmlResult.Id
        Name : string
        ShortName : string
        Classes : XmlResult.Id list
    }

    type PersonalRaceResult = {
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        OrganisationId : XmlResult.Id
        Name : string
        Points : decimal
        Time : float
        TimeBehind : float
        Position : int
        Status : string
    }

    type Event = {
        FileName : string
        Name : string
        Number : int
        Date : string
        Multiply : decimal
        Rule : string option
    }
    
    type EventResult = {
        EventDetails : Event
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        ClassId : XmlResult.Id
        PRR : PersonalRaceResult
        ResultCounts : bool
    }
    
    type CupResult = {
        PersonName : string;
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        ClassId : XmlResult.Id
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        OrganisationId : XmlResult.Id
        TotalPoints : decimal
        Results : seq<EventResult>
        EventInfos : Event list
    }

    type SumResult = {
        PersonName : string
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        ClassId : XmlResult.Id
        [<JsonConverter(typeof<CustomConverter.XmlResultIdConverter>)>]
        OrganisationId : XmlResult.Id
        TotalTime : float
        Disq : bool
        TimeBehind : float
        Results : seq<EventResult>
        EventInfos : Event list
    }

    type TrimOptions = 
        | Start
        | End
        | Both

