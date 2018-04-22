open Vaughan.Domain
open Vaughan.Chords
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.SpeechToMusic
open Vaughan.ChordVoiceLeading

open System
open Argu

type ChordShapes =
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

type VoiceLeadOptions =
    | Lead = 0
    | Bass = 1

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
and ChordArguments =
    | [<AltCommandLine("-r")>]Root of string
    | [<AltCommandLine("-b")>]Bass of BassStrings
    | [<AltCommandLine("-q")>]Quality of string
    | [<AltCommandLine("-s")>]Shape of ChordShapes
    | [<AltCommandLine("-i")>]Inversion of ChordInversions
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Root _ -> "Specify a root note."
            | Quality  _ -> "Specify a chord quality." 
            | Shape _ -> "Specify a chord form."
            | Inversion _ -> "specify a chord inversion."
            | Bass _ -> "specify a guitar bass string for the chord."
and ChordsArguments =
    | [<AltCommandLine("-c")>]Chords of string list
    | [<AltCommandLine("-b")>]Bass of BassStrings
    | [<AltCommandLine("-s")>]Shape of ChordShapes 
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Chords _ -> "Specify a list of chords."
            | Bass _ -> "specify a guitar bass string for the chords."
            | Shape _ -> "Specify a shape for the chords."
and VoiceLeadArguments =
    | [<AltCommandLine("-c")>]Chords of string list
    | [<AltCommandLine("-b")>]Bass of BassStrings
    | [<AltCommandLine("-s")>]Shape of ChordShapes
    | [<AltCommandLine("-v")>]Voice of VoiceLeadOptions
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Chords _ -> "Specify a list of chords."
            | Bass _ -> "specify a guitar bass string for the chords."
            | Shape _ -> "Specify a shape for the chords."
            | Voice _ -> "Specify a chord voice to use as lead."
and ArpeggioArguments =
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
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-cs")>]Chords of ParseResults<ChordsArguments>
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-v")>]VoiceLead of ParseResults<VoiceLeadArguments>
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-s")>]Scale of ParseResults<ScaleArguments>
    | [<CliPrefix(CliPrefix.None)>][<AltCommandLine("-a")>]Arpeggio of ParseResults<ArpeggioArguments>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Chord _ -> "Specify a chord"
            | Chords _ -> "Specify a list of chords" 
            | VoiceLead _ -> "Specify a list of chords" 
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

let handleChordShape shape =
    match shape with
    | ChordShapes.Closed -> toClosed
    | ChordShapes.Drop2 -> toDrop2
    | ChordShapes.Drop3 -> toDrop3
    | _ -> toClosed

let handleChordInversion (inversion:ChordInversions) = 
    match inversion with
    | ChordInversions.First -> invert
    | ChordInversions.Second -> invert >> invert
    | ChordInversions.Third -> invert >> invert >> invert
    | _ -> id

let handleVoiceLead voice =
    match voice with
    | VoiceLeadOptions.Lead -> lead
    | VoiceLeadOptions.Bass -> bass
    | _ -> lead

let handleError (e:ArguParseException) (parser:ArgumentParser<_>) subCommand =
    printf "%s" e.Message
    let parser = parser.GetSubCommandParser subCommand
    parser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n\n"
    Environment.Exit(-1) 

let handleChord (chordArguments:ParseResults<ChordArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let root = chordArguments.GetResult ChordArguments.Root
        let quality = chordArguments.GetResult(ChordArguments.Quality, defaultValue = "Major")
        
        parseChord (sprintf "%s %s" root quality)
    with | :? ArguParseException as e ->
        handleError e parser Chord 
        chord C Major

let handleTabChord (chordArguments:ParseResults<ChordArguments>) (parser:ArgumentParser<TabArguments>) =
    try
        let bass = chordArguments.GetResult(ChordArguments.Bass, defaultValue = BassStrings.Sixth)
        let shape = chordArguments.GetResult(ChordArguments.Shape, defaultValue = ChordShapes.Closed)
        let inversion = chordArguments.GetResult(ChordArguments.Inversion, defaultValue = ChordInversions.Root)
        let chord = (handleChord chordArguments parser) 
                    |> handleChordShape shape 
                    |> (handleChordInversion inversion)

        Vaughan.Domain.Chord(guitarChord (handleGuitarString bass) chord)
    with | :? ArguParseException as e ->
        handleError e parser Chord
        Rest

let handleTabArpeggio (arpeggioArguments:ParseResults<ArpeggioArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let chordArguments = arpeggioArguments.GetResult ArpeggioArguments.Chord
        let minFret = arpeggioArguments.GetResult(ArpeggioArguments.MinFret, defaultValue = 0)
        let maxFret = arpeggioArguments.GetResult(ArpeggioArguments.MaxFret, defaultValue = 3)
        let chord = handleChord chordArguments parser

        Vaughan.Domain.Arpeggio(guitarArpeggio minFret maxFret chord)
    with | :? ArguParseException as e ->
        handleError e parser Arpeggio
        Rest

let handleTabScale (scaleArguments:ParseResults<ScaleArguments>) (parser:ArgumentParser<TabArguments>) = 
    try
        let root = scaleArguments.GetResult ScaleArguments.Root
        let scaleType = scaleArguments.GetResult(ScaleArguments.Type, defaultValue = "ionian")
        let minFret = scaleArguments.GetResult(ScaleArguments.MinFret, defaultValue = 0)
        let maxFret = scaleArguments.GetResult(ScaleArguments.MaxFret, defaultValue = 3)

        let scale = parseScale (sprintf "%s %s" root scaleType)
        
        Vaughan.Domain.Scale(guitarScale minFret maxFret scale)
    with | :? ArguParseException as e ->
        handleError e parser Scale
        Rest

let handleTabChords (chordsArguments:ParseResults<ChordsArguments>) (parser:ArgumentParser<TabArguments>) =
    try
        let bass = chordsArguments.GetResult(ChordsArguments.Bass, defaultValue = BassStrings.Sixth)
        let shape = chordsArguments.GetResult(ChordsArguments.Shape, defaultValue = ChordShapes.Closed)
        let chordNames = chordsArguments.GetResult(ChordsArguments.Chords, defaultValue = [""]) 
        
        let shape = handleChordShape shape
        let guitarChord = guitarChord (handleGuitarString bass) 

        chordNames
        |> List.map (parseChord >> shape >> guitarChord >> Vaughan.Domain.Chord)

    with | :? ArguParseException as e ->
        handleError e parser Chords
        []

let handleTabVoiceLeadChords (voiceLeadArguments:ParseResults<VoiceLeadArguments>) (parser:ArgumentParser<TabArguments>) =
    try
        let bass = voiceLeadArguments.GetResult(VoiceLeadArguments.Bass, defaultValue = BassStrings.Sixth)
        let shape = voiceLeadArguments.GetResult(VoiceLeadArguments.Shape, defaultValue = ChordShapes.Closed)
        let chordNames = voiceLeadArguments.GetResult(VoiceLeadArguments.Chords, defaultValue = [""])
        let leadingVoice = voiceLeadArguments.GetResult(VoiceLeadArguments.Voice, defaultValue = VoiceLeadOptions.Lead) 
        
        let toShape = handleChordShape shape
        let toGuitarChord = guitarChord (handleGuitarString bass)
        let voiceLeadStrategy = handleVoiceLead leadingVoice

        chordNames
        |> List.map (parseChord >> toShape)
        |> voiceLead voiceLeadStrategy 
        |> List.map (toGuitarChord >> Vaughan.Domain.Chord)

    with | :? ArguParseException as e ->
        handleError e parser Chords
        []

let handleTab (tabArguments:ParseResults<TabArguments>) (parser:ArgumentParser<CLIArguments>) =
    try
        let tabParser = parser.GetSubCommandParser <@ Tab @>
        match tabArguments.GetSubCommand() with
        | Chord c -> [handleTabChord c tabParser]
        | Chords cs -> handleTabChords cs tabParser 
        | VoiceLead v -> handleTabVoiceLeadChords v tabParser 
        | Scale s -> [handleTabScale s tabParser]
        | Arpeggio a -> [handleTabArpeggio a tabParser]
    with | :? ArguParseException as e ->
        handleError e parser Tab
        [Rest]

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "VaughanCLI", helpTextMessage = "Guitar tab CLI")
    
    try
        let results = parser.ParseCommandLine(argv)

        match results.GetSubCommand() with
        | Tab t -> [StandardTunning; Start] @ handleTab t parser @ [End] |> renderTab |> printf "%s\n"

        0
    with e ->
        printf "%s\n" e.Message
        0