#load "./Vaughan/Infrastructure.fs"
#load "./Vaughan/Domain.fs"
#load "./Vaughan/Notes.fs"
#load "./Vaughan/Chords.fs"
#load "./Vaughan/Keys.fs"
#load "./Vaughan/Scales.fs"
#load "./Vaughan/ScaleHarmonizer.fs"
#load "./Vaughan/Guitar.fs"
#load "./Vaughan/ChordVoiceLeading.fs"

open Vaughan.Infrastructure
open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Keys
open Vaughan.Scales
open Vaughan.ScaleHarmonizer
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.ChordVoiceLeading

let cIonian = createScale Ionian C

(cIonian
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop3
|> createGuitarChord SixthString)
|> tabify
|> printf "\n%s"

(cIonian
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop2
|> createGuitarChord FifthString)
|> tabify
|> printf "\n%s"

(cIonian
|> triadsHarmonizer ScaleDegrees.I
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

createScale Aolian DSharp
|> triadsHarmonizer ScaleDegrees.III
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScale Ionian A
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop2
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScale Ionian C
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop3
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScale Aolian FSharp
|> seventhsHarmonizer ScaleDegrees.III
|> toDrop3
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScale HarmonicMinor BFlat
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScale HarmonicMinor C
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScale HarmonicMinor C
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%A"

createScale HarmonicMinor C
|> seventhsHarmonizer ScaleDegrees.VII
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
fromDistance 6 |> printf "\n%A"

keyNotes CMajor |> printf "\n%A"
keyNotes EFlatMajor |> printf "\n%A"
keyNotes DMinor |> printf "\n%A"

createScale Phrygian C |> printf "\n%A"
createScale LydianAugmented C  |> printf "\n%A"

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
printfn "ChordsFitting"

chordsFitting [D; F; A] |> printf "\n%A"
chordsFitting [C; E; G; B] |> printf "\n%A"