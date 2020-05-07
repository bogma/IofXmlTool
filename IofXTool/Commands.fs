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

type BuildArgs =
    | [<ExactlyOnce>] Watch of watch:bool
    | [<ExactlyOnce; Last>] Files of files:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Watch _ -> "Start watching for changes and recalculate"
            | Files _ -> "List of (additional) input file(s)"

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

type RulesArgs =
    | Compile
    | Reset
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Compile _ -> "Compile new version of CalcLib"
            | Reset -> "Restore default CalcLib"

type Command =
    // global options
    |                                                   Version
    | [<AltCommandLine("-p");Inherit>]                  Working_Directory of path:string
    | [<AltCommandLine("-c");Inherit>]                  Config_File of config:string
    | [<AltCommandLine("-s");Inherit>]                  Silent
    | [<AltCommandLine("-v");Inherit>]                  Verbose
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
            | New _ -> "create a new project"
            | Add _ -> "add a new resource to project"
            | Build _ -> "build a project"
            | Info _ -> "print project info"
            | Rules _ -> "add new caluclation rules"
            | Working_Directory _ -> "the working directory"
            | Config_File _ -> "the project configuration file"
            | Log_File _ -> "print output to a file"
            | Silent -> "suppress console output"
            | Verbose -> "print detailed information to the console"
            | Version -> "show Paket version"


type CommonArgs = {
    wDir : string
    cfgFile : string
    silent : bool
}