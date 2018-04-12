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

[
    Note({GuitarString=FirstString; Fret=8; Note=C})
]
|> renderTab
|> printf "\n%s"

[
    StandardTunning;
    Start;
    Chord(chord D Minor7 |> toDrop3 |> createGuitarChord SixthString);
    Chord(chord G Dominant7 |> toDrop3 |> createGuitarChord SixthString);
    Chord(chord C Major7 |> toDrop3 |> createGuitarChord SixthString);
    End
]
|> renderTab
|> printf "\n%s"

[chord C Minor7; chord F Dominant7; chord BFlat Major7]
|> tabifyArpeggiosFromChords 5 8
|> List.map (fun t -> printf "\n%s" t)

[chord BFlat Dominant7; chord EFlat Dominant7; chord F Dominant7]
|> tabifyArpeggiosFromChords 5 8
|> List.map (fun t -> printf "\n%s" t)

[chord C Minor7; chord F Dominant7;chord BFlat Major7]
|> tabifyScalesFromChords 5 8
|> List.map (fun t -> printf "\n%s" t)

createScale Ionian BFlat 
|> createGuitarScale 2 6
|> createGuitarMelodicLineFromScale
|> tabifyMelodicLine
|> printf "\n%s"

createScale Ionian C 
|> createGuitarScale 4 8
|> tabifyScale
|> printf "\n%s"

createScale Ionian C 
|> createGuitarScale 7 10
|> tabifyScale
|> printf "\n%s"

createScale Ionian C 
|> createGuitarScale 9 13
|> tabifyScale
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 9 22
|> tabifyArpeggio
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 7 10
|> tabifyArpeggio
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 2 5
|> tabifyArpeggio
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 1 4
|> tabifyArpeggio
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 7 10
|> tabifyArpeggio
|> printf "\n%s"

chord C Major
|> createGuitarArpeggio 0 3
|> tabifyArpeggio
|> printf "\n%s"

chord C Major7
|> toDrop3
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%s"

chord C Major7
|> toDrop2
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Major
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Dominant9
|> skipFunction Fifth
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

chord C Major9
|> skipFunction Fifth
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Aolian DSharp
|> triadsHarmonizer ScaleDegree.III
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Ionian A
|> seventhsHarmonizer ScaleDegree.I
|> toDrop2
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDegree.I
|> toDrop3
|> createGuitarChord FifthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes Aolian FSharp
|> seventhsHarmonizer ScaleDegree.III
|> toDrop3
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor BFlat
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%A"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> toOpen
|> createGuitarChord SixthString
|> tabifyChord
|> printf "\n%A"

[(!*(G=>Major) |~ SixthString);
(!*(C=>Major) |~ FifthString);
(!*(A=>Minor) |~ FifthString);
(!*(D=>Major) |~ FourthString)]
|> tabifyAll
|> printf "\n%s"

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