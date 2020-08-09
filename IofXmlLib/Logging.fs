namespace IofXmlLib

module Logging =

    open System
    open System.IO
    open NLog

    let configureNLogWithFile (cfgPath: string) =
        let xmlConfig = Config.XmlLoggingConfiguration(cfgPath)
        LogManager.Configuration <- xmlConfig

    let configureNLog isVerbose isSilent (logFile: string option) wDir =
        let config = Config.LoggingConfiguration()
        match logFile with
        | Some lf ->
            let fn = 
                if Path.IsPathRooted(lf) then
                    lf
                else
                    Path.Combine(wDir, lf)

            let target = new Targets.FileTarget("file")
            let layout = Layouts.SimpleLayout("${longdate} ${level} ${message}")
            target.Layout <- layout
            target.FileName <- new Layouts.SimpleLayout(fn)
            target.AutoFlush <- true
            let rule =
                if isVerbose then
                    Config.LoggingRule("*",LogLevel.Trace, target)
                else
                    Config.LoggingRule("*",LogLevel.Info, target)
            config.AddTarget("file", target)
            config.LoggingRules.Add(rule)
        | None -> ()

        if not isSilent then
            let target = new Targets.ColoredConsoleTarget()
            let layout = Layouts.SimpleLayout("${longdate} ${level} ${message}")
            target.Layout <- layout
            let rule =
                if isVerbose then
                    Config.LoggingRule("*",LogLevel.Trace, target)
                else
                    Config.LoggingRule("*",LogLevel.Info, target)

            let layout = Layouts.SimpleLayout("${message}");
            target.Layout <- layout

            config.AddTarget("console", target)
            config.LoggingRules.Add(rule)

        LogManager.Configuration <- config

    /// for nlog config debug purposes
    let printNLogConfig () = 

        let nLogConfigToString () = 
            let targets = LogManager.Configuration.AllTargets
            let out = ""
            let out = Seq.fold (fun out target -> out + (sprintf "%A\n" target)) out targets
            let rules = LogManager.Configuration.LoggingRules
            let out = Seq.fold (fun out rule -> out + (sprintf "%A\n" rule)) out rules
            out

        printfn "%s" (nLogConfigToString ())

    
    type ILogger =
        abstract Trace: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract TraceException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract Debug: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract DebugException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract Info: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract InfoException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract Warn: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract WarnException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract Error: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract ErrorException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract Fatal: fmt: Printf.StringFormat<'a, unit> -> 'a
        abstract FatalException: e: Exception -> fmt: Printf.StringFormat<'a, unit> -> 'a
    
    type Logger(logger: NLog.Logger) =
        let logger = logger
    
        new(name: string) =
            Logger(NLog.LogManager.GetLogger(name))
    
        new() = 
            let callerType = 
                System.Diagnostics.StackTrace(1, false)
                    .GetFrames().[0]
                    .GetMethod()
                    .DeclaringType
            Logger(callerType.Name)
        
        member __.Trace fmt =
            Printf.kprintf (fun s -> logger.Trace(s)) fmt
        member __.TraceException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Trace(e, s)) fmt
        member __.Debug fmt = 
            Printf.kprintf (fun s -> logger.Debug(s)) fmt
        member __.DebugException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Debug(e, s)) fmt
        member __.Info fmt = 
            Printf.kprintf (fun s -> logger.Info(s)) fmt
        member __.InfoException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Info(e, s)) fmt
        member __.Warn fmt =
            Printf.kprintf (fun s -> logger.Warn(s)) fmt
        member __.WarnException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Warn(e, s)) fmt
        member __.Error fmt = 
            Printf.kprintf (fun s -> logger.Error(s)) fmt
        member __.ErrorException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Error(e, s)) fmt
        member __.Fatal fmt =
            Printf.kprintf (fun s -> logger.Fatal(s)) fmt
        member __.FatalException (e:Exception) fmt =
            Printf.kprintf (fun s -> logger.Fatal(e, s)) fmt
    
        interface ILogger with 
            member __.Trace fmt = __.Trace fmt
            member __.TraceException e fmt = __.TraceException e fmt
            member __.Debug fmt = __.Debug fmt
            member __.DebugException e fmt = __.DebugException e fmt
            member __.Info fmt = __.Info fmt
            member __.InfoException e fmt = __.InfoException e fmt
            member __.Warn fmt = __.Warn fmt
            member __.WarnException e fmt = __.WarnException e fmt
            member __.Error fmt = __.Error fmt
            member __.ErrorException e fmt = __.ErrorException e fmt
            member __.Fatal fmt = __.Fatal fmt
            member __.FatalException e fmt = __.FatalException e fmt
    
    
    ////let tracer = LogManager.GetLogger("iofxlogger")
    let tracer = new Logger("iofxlogger")
