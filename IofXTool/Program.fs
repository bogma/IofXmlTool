open System
open Argu

open Commands
open System.IO
open IofXmlLib.Logging

let toolVersion = AssemblyVersionInformation.AssemblyInformationalVersion

let traceVersion silent =
    if not silent then
        printfn "IOF XML tool version %s" toolVersion

let processWithValidationEx printUsage cArgs validateF subCmd subArgs =
    if not (validateF subArgs) then
        tracer.Warn "Command was:"
        Environment.GetCommandLineArgs() |> Array.iter (tracer.Warn "%s ")
        printUsage subArgs

        Environment.ExitCode <- 1
    else
        try
            subCmd cArgs subArgs
        finally
            Environment.ExitCode <- 1


let processWithValidation cArgs validateF subCmd (subArgs : ParseResults<'T>) =
    processWithValidationEx (fun (r:ParseResults<'T>) -> r.Parser.PrintUsage() |> ignore) cArgs validateF subCmd subArgs

let processCommand cArgs subCmd subArgs =
    processWithValidation cArgs (fun _ -> true) subCmd subArgs


let handleCommand cArgs command =
    match command with
    | New a -> processCommand cArgs NewProjectCmdHandler.newProject a
    | Build a -> processCommand cArgs BuildCmdHandler.build a
    | Info a -> processCommand cArgs CommandHandlers.info a
    | Rules a -> processCommand cArgs RuleCmdHandler.rules a
    // global options; list here in order to maintain compiler warnings
    | Verbose
    | Silent
    | Version
    | Working_Directory _ -> failwithf "internal error: this code should never be reached."
    | Config_File _ -> failwithf "internal error: this code should never be reached."
    | Log_File _ -> failwithf "internal error: this code should never be reached."

[<EntryPoint>]
let main argv =

    try
        let commandParser = ArgumentParser.Create<Command>(
                                programName = "iofxtool",
                                errorHandler = ProcessExiter(),
                                checkStructure = false)
        let args = commandParser.ParseCommandLine(
                                inputs = argv,
                                raiseOnUsage = true)

        let commonArgs = {
            wDir = args.GetResult (Working_Directory, defaultValue = Directory.GetCurrentDirectory());
            cfgFile = args.GetResult (Config_File, defaultValue = "config.xml");
        }

        let version = args.Contains <@ Version @>
        let isSilent = args.Contains <@ Silent @>
        let isVerbose = args.Contains <@ Verbose @>

        traceVersion isSilent

        if not version then
            let logFileName = args.TryGetResult <@ Log_File @>
            configureNLog isVerbose isSilent logFileName commonArgs.wDir

            handleCommand commonArgs (args.GetSubCommand())

    with e ->
        tracer.ErrorException e "%s %s" e.Message e.StackTrace
    0
