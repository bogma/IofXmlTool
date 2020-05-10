open System
open Argu

open Commands
open BuildHandler
open CommandHandlers
open System.IO

let toolVersion = AssemblyVersionInformation.AssemblyInformationalVersion

let mutable tracedVersion = false

let traceVersion silent =
    if not silent && not tracedVersion then
        tracedVersion <- true
        printfn "IOF XML tool version %s" toolVersion

let processWithValidationEx printUsage cArgs validateF subCmd subArgs =
    traceVersion cArgs.silent
    if not (validateF subArgs) then
        printfn "Command was:"
        Environment.GetCommandLineArgs() |> Array.iter (printf "%s ")
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
    | New a -> processCommand cArgs newProject a
    | Add a -> processCommand cArgs add a
    | Build a -> processCommand cArgs build a
    | Info a -> processCommand cArgs info a
    | Rules a -> processCommand cArgs rules a
    // global options; list here in order to maintain compiler warnings
    // in case of new subcommands added
    ////| Verbose
    ////| Silent
    | Version
    | Working_Directory _ -> failwithf "internal error: this code should never be reached."
    | Config_File _ -> failwithf "internal error: this code should never be reached."
    ////| Log_File _ -> failwithf "internal error: this code should never be reached."

[<EntryPoint>]
let main argv =

    try
        let commandParser = ArgumentParser.Create<Command>(
                                programName = "iofxtool",
                                errorHandler = new ProcessExiter(),
                                checkStructure = false)
        let args = commandParser.ParseCommandLine(
                                inputs = argv,
                                raiseOnUsage = true)

        let commonArgs = {
            wDir = args.GetResult (Working_Directory, defaultValue = Directory.GetCurrentDirectory());
            cfgFile = args.GetResult (Config_File, defaultValue = "config.xml");
            silent = false ////args.Contains <@ Silent @>
        }
        
        traceVersion commonArgs.silent

        ////if pArgs.Contains <@ Verbose @> then
        ////    Logging.verbose <- true
        ////    Logging.verboseWarnings <- true

        let version = args.Contains <@ Command.Version @>

        if not version then
             ////use fileTrace =
             ////    match pArgs.TryGetResult <@ Log_File @> with
             ////    | Some lf -> setLogFile lf
             ////    | None -> null

             handleCommand commonArgs (args.GetSubCommand())

    with e ->
         printfn "%s" e.Message
    0


