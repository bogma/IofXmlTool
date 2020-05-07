namespace IofXmlLib

module Transforms =

    open FSharp.Data
    open FSharp.Json

    type private XmlResult = XmlProvider<Schema="./datastandard_IOF_30.xsd", EmbeddedResource="IofXmlLib, IofXmlLib.datastandard_IOF_30.xsd">

    /// Implementation of [ITypeTransform] converting DateTime into int64 as epoch time.
    type XmlResultId() =
        interface ITypeTransform with
            member x.targetType () = (fun _ -> typeof<string>) ()
            member x.toTargetType value = (fun (v:obj) -> ((v :?> XmlResult.Id).Value :> obj)) value
            member x.fromTargetType value = (fun (v:obj) -> XmlResult.Id(None, string v) :> obj) value


module Types =

    open FSharp.Data
    open FSharp.Json

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
        Time : int
        Status : string
    }

    type SimplePersonalRaceResult = {
        Name : string
        Time : int
    }

    type MyTeamResult = {
        OrganisationId : XmlResult.Id
        TotalTime : int
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
        OrganisationId : XmlResult.Id;
        Name : string;
        Points : decimal;
        Time : int;
        Position : int
        Status : string
    }
    
    type EventResult = {
        EventFile : string;
        ClassId : XmlResult.Id;
        PRR : PersonalRaceResult;
        ResultCounts : bool
    }
    
    type CupResult = {
        PersonName : string;
        [<JsonField(Transform=typeof<Transforms.XmlResultId>)>]
        ClassId : XmlResult.Id;
        OrganisationId : XmlResult.Id;
        TotalPoints : decimal;
        Results : seq<EventResult>
    }

