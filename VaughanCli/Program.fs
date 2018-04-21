open Vaughan.Domain
open Vaughan.Chords
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.SpeechToMusic

open System
open Argu

type ChordForms =
    | Closed = 0
    | Drop2 = 1
    | Drop3 = 2

type ChordInversions = 
    | Root = 0
    | First = 1
    | Second = 2
    | Third = 3

type BassStrings =
    | Third = 3
    | Fourth = 4
    | Fifth = 5
    | Sixth = 6

type TabSubCommands =
    | Chord = 0
    | Scale = 1
    | Arpeggio = 2

type ScaleArguments =
    | [<AltCommandLine("-r")>]Root of string
    | [<AltCommandLine("-t")>]Type of string
    | [<AltCommandLine("-min")>]MinFret of int
    | [<AltCommandLine("-max")>]MaxFret of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Root _ -> "Specify a root note." 
            | Type _ -> "Specify a scale type." 
            | MinFret _ -> "specify the minimum fret for the scale."
            | MaxFret _ -> "specify the maximum fret for the scale."
type ChordArguments =
    | [<AltCommandLine("-r")>]Root of string
    | [<AltCommandLine("-b")>]Bass of BassStrings
    | [<AltCommandLine("-q")>]Quality of string
    | [<AltCommandLine("-f")>]Form of ChordForms
    | [<AltCommandLine("-i")>]Inversion of ChordInversions
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Root _ -> "Specify a root note."
            | Quality  _ -> "Specify a chord quality." 
            | Form _ -> "Specify a chord form."
            | Inversion _ -> "specify a chord inversion."
            | Bass _ -> "specify a guitar bass string for the chord."
type ArpeggioArguments =
    | [<AltCommandLine("-c")>]Chord of ParseResults<ChordArguments>
    | [<AltCommandLine("-min")>]MinFret of int
    | [<AltCommandLine("-max")>]MaxFret of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Chord _ -> "Specify a chord." 
            | MinFret _ -> "specify the minimum fret for the arpeggio."
            | MaxFret _ -> "specify the maximum fret for the arpeggio."
and TabArguments =
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-c")>]Chord of ParseResults<ChordArguments>
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-s")>]Scale of ParseResults<ScaleArguments>
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-a")>]Arpeggio of ParseResults<ArpeggioArguments>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Chord _ -> "Specify a chord"
            | Scale _ -> "Specify a scale"
            | Arpeggio _ -> "Specify an arpeggio"
and CLIArguments =
    | [<CliPrefix(CliPrefix.None)>]Tab of ParseResults<TabArguments>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Tab _ -> "Create tab for <option>."

let handleGuitarString bass = 
    match bass with
    | BassStrings.Third -> ThirdString
    | BassStrings.Fourth -> FourthString
    | BassStrings.Fifth -> FifthString
    | BassStrings.Sixth -> SixthString
    | _ -> SixthString 

let handleChordForm form =
    match form with
    | ChordForms.Closed -> toClosed
    | ChordForms.Drop2 -> toDrop2
    | ChordForms.Drop3 -> toDrop3
    | _ -> toClosed

let handleChordInversion (inversion:ChordInversions) = 
    match inversion with
    | ChordInversions.First -> invert
    | ChordInversions.Second -> invert >> invert
    | ChordInversions.Third -> invert >> invert >> invert
    | _ -> id

let handleError (e:ArguParseException) (parser:ArgumentParser<_>) subCommand =
    printf "%s" e.Message
    let parser = parser.GetSubCommandParser subCommand
    parser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n\n"
    Environment.Exit(-1) 

let handleChord (chordArguments:ParseResults<ChordArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let root = chordArguments.GetResult ChordArguments.Root
        let quality = chordArguments.GetResult ChordArguments.Quality
        
        parseChord (sprintf "%s %s" root quality)
    with | :? ArguParseException as e ->
        handleError e parser Chord 
        chord C Major

let handleTabChord (chordArguments:ParseResults<ChordArguments>) (parser:ArgumentParser<TabArguments>)=
    try
        let bass = chordArguments.GetResult Bass
        let form = chordArguments.GetResult ChordArguments.Form
        let inversion = chordArguments.GetResult ChordArguments.Inversion
        let chord = (handleChord chordArguments parser) 
                    |> handleChordForm form 
                    |> (handleChordInversion inversion)

        Vaughan.Domain.Chord(guitarChord (handleGuitarString bass) chord)
    with | :? ArguParseException as e ->
        handleError e parser Chord
        Rest

let handleTabArpeggio (arpeggioArguments:ParseResults<ArpeggioArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let chordArguments = arpeggioArguments.GetResult ArpeggioArguments.Chord
        let minFret = arpeggioArguments.GetResult ArpeggioArguments.MinFret
        let maxFret = arpeggioArguments.GetResult ArpeggioArguments.MaxFret
        let chord = handleChord chordArguments parser

        Vaughan.Domain.Arpeggio(guitarArpeggio minFret maxFret chord)
    with | :? ArguParseException as e ->
        handleError e parser Arpeggio
        Rest

let handleTabScale (scaleArguments:ParseResults<ScaleArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let root = scaleArguments.GetResult ScaleArguments.Root
        let scaleType = scaleArguments.GetResult ScaleArguments.Type
        let minFret = scaleArguments.GetResult ScaleArguments.MinFret
        let maxFret = scaleArguments.GetResult ScaleArguments.MaxFret

        let scale = parseScale (sprintf "%s %s" root scaleType)
        
        Vaughan.Domain.Scale(guitarScale minFret maxFret scale)
    with | :? ArguParseException as e ->
        handleError e parser Scale
        Rest

let handleTab (tabArguments:ParseResults<TabArguments>) (parser:ArgumentParser<CLIArguments>) =
    try 
        let tabParser = parser.GetSubCommandParser <@ Tab @>
        match tabArguments.GetSubCommand() with
        | Chord c -> handleTabChord c tabParser
        | Scale s -> handleTabScale s tabParser
        | Arpeggio a -> handleTabArpeggio a tabParser
    with | :? ArguParseException as e ->
        handleError e parser Tab
        Rest

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "VaughanCLI",  helpTextMessage = "VaughanCLI -- guitar tab CLI tool")
    
    try
        let results = parser.ParseCommandLine(argv)

        match results.GetSubCommand() with
        | Tab t -> [StandardTunning; Start] @ [handleTab t parser] @ [End] |> renderTab |> printf "%s\n"

        0
    with e ->
        printf "%s\n" e.Message
        0