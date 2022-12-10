# IOF XML Tool

This tool is mainly for calculating cup results of an orienteering series. It uses IOF XML v3.0 format as input. The output can be rendered as HTML and JSON. It's based on .NET core v3.1 and thus can be run on Windows, Linux and Mac OS.

### Motivation
This tool started as cup calculator and was equally motivated by
+ learning F#
+ automatic calculation of our regional orienteering cup based on result exports from timing software

### Plans, Ideas
A lot more use cases are possible with the current basic infrasturcture, i.e.
+ inplement a GUI for the tool
+ enhance PDF rendering
+ split time analysis (analog to winsplits)
+ graphical representation of race result (analog to SplitsBrowser)
+ race result presentation mode

## Installation

### Windows

### Linux

### Mac OS

## Documentation

You can run the tool on command line only. If were following the installation documentation you should have you path variable set. Thus by typing

    iofxtool

in a console window the program should run and output an usage help message. If you type

    iofxtool <subcommand>

with one of the available subcommands you will get an usage help for the subcommand.

There are some parameters that are valid for all commands, namely:

- *--working-directory* or *-p*: This is the working directory containing the configuration, template and input files. Normally it's convinient to place all your cup related files into one directory. But you can also override the values for the config file with the *--config-file* parameter and the location of the other files in the config file.
- *--config-file* or *-c*: This defaults to 'config.xml' if not given. The config file will be searched relative to the working directory (i.e. '.\myEvent\cup.xml') or a full path can be used (i.e. 'C:\events\2019\myCup.xml').
- *--silent* or *-s*: All console logging output will be suppressed
- *--verbose* or *-v*: All available logging output will be printed to console
- *--log-file \<path>*: Logging output will be written to file instead of console
- *--help* or *\<subcommand> --help*: Prints usage help to console.

### Available subcommands

+ **new** -> creates a new project by copying all relevant files to the given project directory
    + *--kind* or *-k*: which kind of project should be created
        + 'Cup' ... default files to creat a cup result of serveral single events
        + 'Sum' ... default files to create a summary result of several single events
        + 'Team' ... special 'Schulcup' team result
+ **build** -> build/update your cup result
+ info
+ **rules** -> show existing rules or complile new calulation rules for cups


