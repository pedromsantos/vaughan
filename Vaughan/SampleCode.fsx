#load "Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "Keys.fs"
#load "Scales.fs"
#load "ScaleHarmonizer.fs"
#load "Guitar.fs"
#load "ImprovisationGuitar.fs"
#load "ChordVoiceLeading.fs"

open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Keys
open Vaughan.Scales
open Vaughan.ScaleHarmonizer
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.ImprovisationGuitar
open Vaughan.ChordVoiceLeading

let scaleName scale =
    sprintf "%s %A" (noteName (scale.Scale.Notes.[0])) scale.Scale.Scale 

let chordToneNames (chord:Chord) =
    sprintf "%s" (chord.Notes 
                    |> List.map (fst >> noteName) 
                    |> List.fold (fun r s -> r + s + " ") "")

let scaleNoteNames scale = 
    sprintf "%s" (scale.Scale.Notes 
                    |> List.map noteName 
                    |> List.fold (fun r s -> r + s + " ") "")

let chords = [
              chord C Minor7;
              chord F Dominant7;
              chord BFlat Major7;
              chord EFlat Major7;
              chord A Minor7b5;
              chord D Dominant7;
              chord G Minor7
             ]

(createScalesForChords 5 8 chords)
|> List.map (fun scalesPerChord -> scalesPerChord |> List.map scaleName)
|> Seq.map Set.ofList
|> Seq.reduce Set.intersect
|> Seq.fold (fun r s -> r + s + "\n") ""
|> printf "Common scales for all chords: \n%s\n"

(createScalesForChords 5 8 chords)
|> List.mapi (fun i scalesPerChord -> (name chords.[i], scalesPerChord |> List.map scaleName))
|> List.map (fun sc -> 
                printf "# Scales than include all chord tones of %s\n" (fst sc)
                snd sc |> List.map (fun s -> printf "* %s\n" s)
                )

(createScalesForChords 5 8 chords)
|> List.mapi (fun i scalesPerChord ->
                printf "# Scales than include all chord tones of %s\n" (name chords.[i])
                scalesPerChord 
                |> List.map (fun scale ->
                                printf "## %s\n" (scaleName scale)
                                printf "* Chord notes %s\n" (chordToneNames chords.[i])
                                printf "* Scale notes %s\n" (scaleNoteNames scale)
                                [StandardTunning; Start; Scale(scale); End]
                                |> renderTab
                                |> printf "```\n%s```\n")
                )

let generateArpeggioExercise arpeggio =
    [
        ascEightsRootAbove, "Root above"; ascEightsRootBelow, "Root below";
        ascEightsThirdAbove, "Third above"; ascEightsThirdBelow, "Third below";
        ascEightsSeventhAbove, "Seventh above"; ascEightsSeventhBelow, "Seventh below";
        descEightsRootAbove, "Root above"; descEightsRootBelow, "Root below";
        descEightsThirdAbove, "Third above"; descEightsThirdBelow, "Third below";
        descEightsSeventhAbove, "Seventh above"; descEightsSeventhBelow, "Seventh below"
        ascEightsRootEnclosed, "Root enclosed"; descEightsRootEnclosed, "Root enclosed";
        ascEightsThirdEnclosed, "Third enclosed"; descEightsThirdEnclosed, "Third enclosed";
        ascEightsSeventhEnclosed, "Seventh enclosed"; descEightsSeventhEnclosed, "Seventh enclosed";
    ]
    |> List.map (fun form ->
                    [StandardTunning; Start; Notes((fst form) arpeggio); End]
                    |> renderTab
                    |> printf "### %s\n```\n%s```\n" (snd form))

[
    guitarArpeggio 5 8 (chord C Minor7);
    guitarArpeggio 5 8 (chord F Dominant7);
    guitarArpeggio 5 8 (chord BFlat Major7);
    guitarArpeggio 5 8 (chord EFlat Major7);
    guitarArpeggio 5 8 (chord A Minor7b5);
    guitarArpeggio 5 9 (chord D Dominant7);
    guitarArpeggio 5 8 (chord G Minor7) 
]
|> List.map (fun a -> 
                        printf "# ====== %s ======\n" (name a.BaseChord)
                        printf "## === Arpeggio ==="
                        [StandardTunning; Start; Arpeggio(a); End]
                        |> renderTab
                        |> printf "\n```\n%s```\n"
                        printf "## === Exercises ===\n"
                        generateArpeggioExercise a)


[StandardTunning; Start; Notes(ascEightsRootAbove (guitarArpeggio 5 8 (chord BFlat Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(descEightsRootBelow (guitarArpeggio 5 8 (chord BFlat Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsRootEnclosed (guitarArpeggio 5 8 (chord BFlat Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(descEightsThirdEnclosed (guitarArpeggio 2 5 (chord C Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsRootEnclosed (guitarArpeggio 2 5 (chord C Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsThirdEnclosed (guitarArpeggio 2 5 (chord C Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsRootEnclosed (guitarArpeggio 5 8 (chord C Minor7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsSeventhEnclosed (guitarArpeggio 5 8 (chord C Minor7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Notes(ascEightsThirdEnclosed (guitarArpeggio 5 8 (chord C Minor7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; ArbArpeggio(descendingArpeggioFrom root (guitarArpeggio 2 5 (chord C Major7))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(ascendingArpeggioFrom root (guitarArpeggio 2 5 (chord C Major))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(ascendingArpeggioFrom root (guitarArpeggio 2 5 (chord G Major))); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Scale(createScale Ionian C |> guitarScale 2 6); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(guitarArpeggio 9 22 (chord C Major)); End]
|> renderTab 
|> printf "\n%s"

[
    StandardTunning;
    Start;
    Note({GuitarString=FirstString; Fret=8; Note=C})
    End
]
|> renderTab
|> printf "\n%s"

[
    StandardTunning;
    Start;
    Chord(chord D Minor7 |> toDrop3 |> guitarChord SixthString);
    Chord(chord G Dominant7 |> toDrop3 |> guitarChord SixthString);
    Chord(chord C Major7 |> toDrop3 |> guitarChord SixthString);
    End
]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Scale(createScale Ionian C |> guitarScale 4 8); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Scale(createScale Ionian C |> guitarScale 7 10); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Scale(createScale Ionian C |> guitarScale 9 13); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 9 22); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 7 10); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 4 5); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 1 4); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 7 10); End]
|> renderTab
|> printf "\n%s"

[StandardTunning; Start; Arpeggio(chord C Major |> guitarArpeggio 0 13); End]
|> renderTab
|> printf "\n%s"

chord C Major7
|> toDrop3
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%s"

chord C Major7
|> toDrop2
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Major
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Dominant9
|> skipFunction Fifth
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Major9
|> skipFunction Fifth
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Aolian DSharp
|> triadsHarmonizer ScaleDegree.III
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Ionian A
|> seventhsHarmonizer ScaleDegree.I
|> toDrop2
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDegree.I
|> toDrop3
|> guitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Aolian FSharp
|> seventhsHarmonizer ScaleDegree.III
|> toDrop3
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor BFlat
|> seventhsHarmonizer ScaleDegree.VII
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%A"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> toOpen
|> guitarChord SixthString
|> tabifyChord
|> printf "\n%A"

noteName C |> printf "\n%A"
noteName CSharp |> printf "\n%A"
noteName DFlat |> printf "\n%A"
sharp EFlat |> printf "\n%A"
flat E |> printf "\n%A"
measureAbsoluteSemitones C G |> printf "\n%A"
intervalBetween C FSharp |> printf "\n%A"
transpose C MajorSixth |> printf "\n%A"
intervalName DiminishedFifth |> printf "\n%A"
fromDistance 6<ht> |> printf "\n%A"
toDistance PerfectFifth |> printf "\n%A"

keyNotes CMajor |> printf "\n%A"
keyNotes EFlatMajor |> printf "\n%A"
keyNotes DMinor |> printf "\n%A"

createScaleNotes Phrygian C |> printf "\n%A"
createScaleNotes LydianAugmented C  |> printf "\n%A"

let cMaj7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed; Name="CMaj7"}
let cMaj = chord C Major

noteNames cMaj7 |> printf "\n%A"
bass cMaj7 |> printf "\n%A"
lead cMaj7 |> printf "\n%A"
name cMaj7 |> printf "\n%A"
cMaj7.Notes |> printf "\n%A"
(cMaj7 |> invert).Notes |> printf "\n%A"
(cMaj7 |> invert |> invert).Notes |> printf "\n%A"
(cMaj7 |> invert |> invert |> invert).Notes |> printf "\n%A"
(cMaj7 |> toDrop2).Notes |> printf "\n%A"
(cMaj7 |> toDrop3).Notes |> printf "\n%A"


inversionForFunctionAsLead cMaj Third |> printf "\n%A"
inversionForFunctionAsBass cMaj Fifth |> printf "\n%A"
invertionWithLeadClosestToNote cMaj CSharp |> printf "\n%A"
invertionWithBassClosestToNote cMaj F |> printf "\n%A"

printfn "\n"
printfn "Chords Fitting"

chordsFitting [D; F; A] |> printf "\n%A"
chordsFitting [C; E; G; B] |> printf "\n%A"

printfn "\n"
printfn "Scales Fitting"

let chord = chord C ChordQuality.Dominant7

scalesFitting chord