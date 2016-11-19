module ProgramSettings

    let version = "1.0.0.0"

    let debugMode = ref false

    let cupName = ref ""
    let takeBest = ref 0
    let maxEvents = ref 0
    let year = ref 0
    let classCfg : CupTypes.XmlConfig.Class list ref = ref List.Empty
    let classCfgIds : int list ref = ref List.Empty
    let orgCfg : CupTypes.XmlConfig.Organisation list ref = ref List.Empty
    let orgCfgIds : int list ref = ref List.Empty
    let eventProps : CupTypes.XmlConfig.Event list ref = ref List.Empty
    let calcRule = ref ""
    let resultFilePrefix = ref ""
