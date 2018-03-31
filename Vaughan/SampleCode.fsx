#load "Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "Keys.fs"
#load "Scales.fs"
#load "ScaleHarmonizer.fs"
#load "Guitar.fs"
#load "ChordVoiceLeading.fs"

open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Keys
open Vaughan.Scales
open Vaughan.ScaleHarmonizer
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.ChordVoiceLeading

let cIonian = createScaleNotes Ionian C

(cIonian
|> seventhsHarmonizer ScaleDegree.I
|> createGuitarArpeggio 7 10)
|> tabifyArpeggio
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegree.I
|> createGuitarArpeggio 2 5)
|> tabifyArpeggio
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegree.I
|> createGuitarArpeggio 1 4)
|> tabifyArpeggio
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegree.I
|> createGuitarArpeggio 7 10)
|> tabifyArpeggio
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegree.I
|> createGuitarArpeggio 0 3)
|> tabifyArpeggio
|> printf "\n%s"

(cIonian
|> seventhsHarmonizer ScaleDegree.I
|> toDrop3
|> createGuitarChord SixthString)
|> tabify
|> printf "\n%s"

(cIonian
|> seventhsHarmonizer ScaleDegree.I
|> toDrop2
|> createGuitarChord FifthString)
|> tabify
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegree.I
|> createGuitarChord FifthString)
|> tabify
|> printf "\n%s"

chord C Dominant9
|> skipFunction Fifth
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

chord C Major9
|> skipFunction Fifth
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScaleNotes Aolian DSharp
|> triadsHarmonizer ScaleDegree.III
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes Ionian A
|> seventhsHarmonizer ScaleDegree.I
|> toDrop2
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDegree.I
|> toDrop3
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScaleNotes Aolian FSharp
|> seventhsHarmonizer ScaleDegree.III
|> toDrop3
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor BFlat
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%A"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegree.VII
|> toOpen
|> createGuitarChord SixthString
|> tabify
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