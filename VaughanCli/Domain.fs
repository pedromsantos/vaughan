namespace VaughanCLI

module Domain =
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
        | Line = 3

    type ScaleArguments =
        | [<AltCommandLine("-s")>] Scale of string
        | [<AltCommandLine("-min")>] MinFret of int
        | [<AltCommandLine("-max")>] MaxFret of int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Scale _ -> "Specify a scale."
                | MinFret _ -> "specify the minimum fret for the scale."
                | MaxFret _ -> "specify the maximum fret for the scale."

    type ChordArguments =
        | [<AltCommandLine("-c")>] Chord of string
        | [<AltCommandLine("-b")>] Bass of BassStrings
        | [<AltCommandLine("-s")>] Shape of ChordShapes
        | [<AltCommandLine("-i")>] Inversion of ChordInversions
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chord _ -> "Specify a root note."
                | Shape _ -> "Specify a chord form."
                | Inversion _ -> "specify a chord inversion."
                | Bass _ -> "specify a guitar bass string for the chord."

    type ChordsArguments =
        | [<AltCommandLine("-c")>] Chords of string list
        | [<AltCommandLine("-b")>] Bass of BassStrings
        | [<AltCommandLine("-s")>] Shape of ChordShapes
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chords _ -> "Specify a list of chords."
                | Bass _ -> "specify a guitar bass string for the chords."
                | Shape _ -> "Specify a shape for the chords."

    type VoiceLeadArguments =
        | Chords of ParseResults<ChordsArguments>
        | [<AltCommandLine("-b")>] Bass of BassStrings
        | [<AltCommandLine("-v")>] Voice of VoiceLeadOptions
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chords _ -> "Specify a list of chords."
                | Bass _ -> "specify a guitar bass string for the chords."
                | Voice _ -> "Specify a chord voice to use as lead."

    type ArpeggioArguments =
        | [<AltCommandLine("-c")>] Chord of string
        | [<AltCommandLine("-min")>] MinFret of int
        | [<AltCommandLine("-max")>] MaxFret of int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chord _ -> "Specify a chord."
                | MinFret _ -> "specify the minimum fret for the arpeggio."
                | MaxFret _ -> "specify the maximum fret for the arpeggio."

    type CommonScalesArguments =
        | [<AltCommandLine("-c")>] Chords of string list
        | [<AltCommandLine("-min")>] MinFret of int
        | [<AltCommandLine("-max")>] MaxFret of int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chords _ -> "Specify a list of chords."
                | MinFret _ -> "specify the minimum fret for the scale."
                | MaxFret _ -> "specify the maximum fret for the scale."

    type ScalesForChordArguments =
        | [<AltCommandLine("-c")>] Chord of string
        | [<AltCommandLine("-min")>] MinFret of int
        | [<AltCommandLine("-max")>] MaxFret of int
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chord _ -> "Specify a chord."
                | MinFret _ -> "specify the minimum fret for the scale."
                | MaxFret _ -> "specify the maximum fret for the scale."

    type CLIArguments =
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-c")>] Chord of ParseResults<ChordArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-cs")>] Chords of ParseResults<ChordsArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-v")>] VoiceLead of ParseResults<VoiceLeadArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-s")>] Scale of ParseResults<ScaleArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-a")>] Arpeggio of ParseResults<ArpeggioArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-scs")>] CommonScales of ParseResults<CommonScalesArguments>
        | [<CliPrefix(CliPrefix.None); AltCommandLine("-sc")>] ScalesForChord of ParseResults<ScalesForChordArguments>
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Chord _ -> "Specify a chord"
                | Chords _ -> "Specify a list of chords"
                | VoiceLead _ -> "Specify a list of chords"
                | Scale _ -> "Specify a scale"
                | Arpeggio _ -> "Specify an arpeggio"
                | CommonScales _ -> "Find common scales for a specified a list of chords"
                | ScalesForChord _ -> "Find scales that contain all chord tones of a chord"
