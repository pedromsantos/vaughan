open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Keys
open Vaughan.Scales
open Vaughan.ScaleHarmonizer
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.Infrastructure
open Vaughan.ImprovisationGuitar
open Vaughan.ChordVoiceLeading
open Vaughan.SpeechToMusic

open System
open Argu
open System

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
    | Chrod = 0
    | Scale = 1
    | Arpeggio = 2

type ScaleArguments =
    | [<AltCommandLine("-i")>]Type of ScaleType
    | [<AltCommandLine("-min")>]MinFret of int
    | [<AltCommandLine("-max")>]MaxFret of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Type _ -> "Specify a scale type." 
            | MinFret _ -> "specify the minimum fret for the scale."
            | MaxFret _ -> "specify the maximum fret for the scale."
type ChordArguments =
    | [<AltCommandLine("-r")>]Root of Note
    | [<AltCommandLine("-b")>]Bass of BassStrings
    | [<AltCommandLine("-q")>]Quality of ChordQuality 
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
            | Tab _ -> "Specify an action."

let usage (parser:ArgumentParser<CLIArguments>) = 
    parser.PrintUsage() |> printf "%si\n"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "VaughanCLI",  helpTextMessage = "VaughanCLI -- guitar tab CLI tool")

    let tabParser = parser.GetSubCommandParser <@ Tab @>
    let chordParser = tabParser.GetSubCommandParser <@ Chord @>
    let scaleParser = tabParser.GetSubCommandParser <@ Scale @>
    let arpeggioParser = tabParser.GetSubCommandParser <@ Arpeggio @>

    usage parser

    parser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n"
    tabParser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n"
    arpeggioParser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n"
    scaleParser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n"    
    chordParser.PrintCommandLineSyntax(usageStringCharacterWidth = 20) |> printf "%s\n"

    0