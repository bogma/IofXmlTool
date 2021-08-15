module Commands

open System
open Argu

type AddArgs =
    | [<ExactlyOnce>] File of file:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File _ -> "File name to add"

type InfoArgs =
    | Full of full:bool
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Full _ -> "print xml file also"

/// new command
type NewArgs =
    | [<ExactlyOnce; AltCommandLine("-k")>] Kind of kind:NewArgsType
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Kind _ -> "What kind of new config (Cup, Team)"
and NewArgsType =
    | Cup
    | Team

/// build command
type BuildArgs =
    | [<ExactlyOnce>] Watch of watch:bool
    | [<ExactlyOnce; AltCommandLine("-f")>] Files of files:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Files _ -> "input file containing results"
            | Watch _ -> "Start watching for changes and recalculate"


/// rule command
type RuleAction =
    | Compile
    | List
    | ListDetails
    | ListFunctions
    | RestoreDefault

type RulesArgs =
    | [<MandatoryAttribute; AltCommandLine("-a")>] Action of action:RuleAction
    | [<ExactlyOnce; AltCommandLine("-f")>] File of files:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File _ -> "file containing rule definitions (defaults to 'calc_rules.xml')"
            | Action _ -> "sub command action: Compile, List, RestoreDefault"

type Command =
    // global options
    |                                                   Version
    | [<AltCommandLine("-p"); Inherit>]                 Working_Directory of path:string
    | [<AltCommandLine("-c"); Inherit>]                 Config_File of config:string
    | [<AltCommandLine("-s"); Inherit>]                 Silent
    | [<AltCommandLine("-v"); Inherit>]                 Verbose
    | [<Inherit>]                                       Log_File of path:string
    // subcommands
    | [<CustomCommandLine("new")>]                      New of ParseResults<NewArgs>
    | [<CustomCommandLine("add")>]                      Add of ParseResults<AddArgs>
    | [<CustomCommandLine("build")>]                    Build of ParseResults<BuildArgs>
    | [<CustomCommandLine("info")>]                     Info of ParseResults<InfoArgs>
    | [<CustomCommandLine("rules")>]                    Rules of ParseResults<RulesArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | New _ ->                  "create a new project"
            | Add _ ->                  "add a new resource to project"
            | Build _ ->                "build a project"
            | Info _ ->                 "print project info"
            | Rules _ ->                "add new caluclation rules"
            | Working_Directory _ ->    "the working directory"
            | Config_File _ ->          "the project configuration file"
            | Log_File _ ->             "print output to a file"
            | Silent ->                 "suppress console output"
            | Verbose ->                "print detailed information to the console"
            | Version ->                "show tool version"


type CommonArgs = {
    wDir : string
    cfgFile : string
}