# Vaughan - F# music library

Vaughan is named after Blues guitarist [Stevie Ray Vaughan](https://en.wikipedia.org/wiki/Stevie_Ray_Vaughan)

[![NuGet](http://img.shields.io/nuget/v/Vaughan.svg)](https://www.nuget.org/packages/Vaughan)

## Getting started

### Online

Navigate to https://repl.it/FJHh/79 there is some sample code included with the source. The SpeechToMusic module is not available as I have not found a way to add FParsec dependency in repl.it

### On your own evironment

1. Clone the repository to your machine
2. Build
  1. On Windows execute build.cmd (requires Powershell)
  2. On macos / unix execute build.sh (requires mono, curl and jq)

## Usage

### SonicPI integration (Experimental, still very new, may change a lot)

There is now a DSL for communicating with SonicPi, requires SonicPi on machine and requires SonicPi to be running.

### Example usage

```fsharp
#r "../packages/Rug.Osc/lib/Rug.Osc.dll"

#load "Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "SonicPi.fs"

open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.SonicPi

Statments[
    WithSynth(Fm, [
                WithFx(Reverb, [
                            PlayNote(C, OneLine, [Amplitude(0.5<loud>); 
                                                    Panning(0.0<pan>); 
                                                    Attack(2.0<beat>); 
                                                    Release(1.0<beat>)])
                            Sleep(1<s>);
                            PlayChord(chord C Major, TwoLine, [Amplitude(1.0<loud>); Panning(1.0<pan>)])
                            ])
                ])]
|> toSonicPiScript
|> sonicPiSend
```

### Notes

```fsharp
open Vaughan.Notes
```

| Example                      | Output          |
| ---------------------------- | --------------- |
| noteName C                   | "C"             |
| noteName CSharp              | "C#"            |
| noteName DFlat               | "Db"            |
| sharp EFlat                  | E               |
| flat E                       | EFlat           |
| measureAbsoluteSemitones C G | 7               |
| intervalBetween C FSharp     | DiminishedFifth |
| transpose C MajorSixth       | A               |

### Intervals

```fsharp
open Vaughan.Notes
```

| Example                      | Output            |
| ---------------------------- | ----------------- |
| intervalName DiminishedFifth | "DiminishedFifth" |
| fromDistance 6               | DiminishedFifth   |

### Keys

```fsharp
open Vaughan.Notes
open Vaughan.Keys
```

| Example              | Output                              |
| -------------------- | ----------------------------------- |
| keyNotes CMajor      | [ C; D; E; F; G; A; B ]             |
| keyNotes EFlatMajor  | [ EFlat; F; G; AFlat; BFlat; C; D ] |
| keyNotes DMinor      | [ D; E; F; G; A; BFlat; C ]         |

### Scales

```fsharp
open Vaughan.Notes
open Vaughan.Scales
```

| Example                            | Output                                     |
| ---------------------------------- | ------------------------------------------ |
| createScaleNotes Phrygian C        | [ C; DFlat; EFlat; F; G; AFlat; BFlat]     |
| createScaleNotes LydianAugmented C | [ C; D; E; FSharp; GSharp; A; B ]          |


### Chords

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords
open Vaughan.ChordVoiceLeading

let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
let cMaj = chordFromRootAndFunction c Major
```

| Example                                                     | Output                                            |
| ----------------------------------------------------------- | ------------------------------------------------- |
| noteNames cMaj7                                             | ["C"; "E"; "G"; "B"]                              |
| bass cMaj7                                                  | C                                                 |
| lead cMaj7                                                  | B                                                 |
| name cMaj7                                                  | "CMaj7"                                           |
| cMaj7.Notes                                                 | [(C, Root); (E, Third); (G, Fifth); (B, Seventh)] |
| (cMaj7 &#124;> invert).Notes                                | [(E, Third); (G, Fifth); (B, Seventh); (C, Root)] |
| (cMaj7 &#124;> invert &#124;> invert).Notes                 | [(G, Fifth); (B, Seventh); (C, Root); (E, Third)] |
| (cMaj7 &#124;> invert &#124;> invert &#124;> invert).Notes  | [(B, Seventh); (C, Root); (E, Third); (G, Fifth)] |
| (cMaj7 &#124;> toDrop2).Notes                               | [(C, Root); (G, Fifth); (B, Seventh); (E, Third)] |
| (cMaj7 &#124;> toDrop3).Notes                               | [(C, Root); (B, Seventh); (E, Third); (G, Fifth)] |
| inversionForFunctionAsLead cMaj Third                       | cMaj &#124;> invert &#124;> invert                |
| inversionForFunctionAsBass cMaj Fifth                       | cMaj &#124;> invert &#124;> invert                |
| invertionWithLeadClosestToNote cMaj CSharp                  | cMaj &#124;> invert                               |
| invertionWithBassClosestToNote cMaj F                      | cMaj &#124;> invert                               |

### Scale harmonizing

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords
open Vaughan.ScaleHarmonizer

let cMaj = {Notes= [(C, Root); (E, Third); (G, Fifth)]; ChordType=Closed; Name="CMaj"}
let cMin = {Notes= [(C, Root); (EFlat, Third); (G, Fifth)]; ChordType=Closed; Name="CMin"}

let cMaj7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed; Name="CMaj7"}

let cIonian = createScaleNotes Ionian C
let cMinor = createScaleNotes HarmonicMinor C
```

 Example                                       | Output         |
| -------------------------------------------- | -------------- |
| triadsHarmonizer ScaleDgrees.I cIonian       | cMaj           |
| triadsHarmonizer ScaleDgrees.I cMinor        | cMin           |
| triadsHarmonizer ScaleDgrees.I cMinor        | cMin           |
| seventhsHarmonizer ScaleDgrees.I cIonian     | cMaj7          |

### Guitar chord tab drawing

```fsharp
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.ScaleHarmonizer
open Vaughan.Scales
```

```fsharp
createScaleNotes Ionian C
|> triadsHarmonizer ScaleDgrees.I
|> createGuitarChord SixthString
|> tabify
```
Output:
```
  CMaj
E|---|
B|---|
G|---|
D|-5-|
A|-7-|
E|-8-|
```

```fsharp
createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop2
|> createGuitarChord FifthString
|> tabify
```
Output:
```
  CMaj7
E|---|
B|-5-|
G|-4-|
D|-5-|
A|-3-|
E|---|
```

```fsharp
createScaleNotes Ionian A
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop2
|> createGuitarChord FifthString
|> tabify
```
Output:
```
  AMaj7
E|----|
B|-14-|
G|-13-|
D|-14-|
A|-12-|
E|----|
```
```fsharp
createScaleNotes Ionian F
|> seventhsHarmonizer ScaleDgrees.I
|> createGuitarChord FourthString
|> tabify
```
Output:
```
  FMaj7
E|-12-|
B|-13-|
G|-14-|
D|-15-|
A|----|
E|----|
```

```fsharp
createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop3
|> createGuitarChord SixthString
|> tabify
```
Output:
```
  CMaj7
E|---|
B|-8-|
G|-9-|
D|-9-|
A|---|
E|-8-|
```

```fsharp
createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop3
|> createGuitarChord FifthString
|> tabify
```
Output:
```
  CMaj7
E|-3-|
B|-5-|
G|-4-|
D|---|
A|-3-|
E|---|
```
```fsharp
createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop2
|> createGuitarChord FifthString
|> shapify
Output:
```
Output:
```
CMaj7
EADGBE
X3545X
```
```fsharp
let cIonian = createScaleNotes Ionian C
let cMaj = seventhsHarmonizer ScaleDgrees.I cIonian
let dMin = seventhsHarmonizer ScaleDgrees.II cIonian
let eMin = seventhsHarmonizer ScaleDgrees.III cIonian
let fMaj = seventhsHarmonizer ScaleDgrees.IV cIonian

let guitarChords =  [cMaj; dMin; eMin; fMaj]
                    |> List.map (toDrop2 >> (createGuitarChord FifthString))

tabifyAll guitarChords
```
Output:
```
      CMaj7   DMin7   EMin7   FMaj7
E|-------------------------------------|
B|----5-------6-------8-------10-------|
G|----4-------5-------7-------9--------|
D|----5-------7-------9-------10-------|
A|----3-------5-------7-------8--------|
E|-------------------------------------|
```

### Guitar chord tab drawing from textual chord

```fsharp
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.Scales
open Vaughan.Guitar
open Vaughan.GuitarTab
open Vaughan.ScaleHarmonizer
open Vaughan.SpeechToMusic
```

```fsharp
"C Major"
|> parseChord
|> createChord
|> createGuitarChord SixthString
|> tabify
```
Output:
```
  CMaj
E|---|
B|---|
G|---|
D|-5-|
A|-7-|
E|-8-|
```

### Example usage

```fsharp
#load "./Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "Keys.fs"
#load "Scales.fs"
#load "ScaleHarmonizer.fs"
#load "Guitar.fs"
#load "ChordVoiceLeading.fs"

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

let cIonian = createScaleNotes Ionian C

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

createScaleNotes Aolian DSharp
|> triadsHarmonizer ScaleDegrees.III
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes Ionian A
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop2
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScaleNotes Ionian C
|> seventhsHarmonizer ScaleDegrees.I
|> toDrop3
|> createGuitarChord FifthString
|> tabify
|> printf "\n%s"

createScaleNotes Aolian FSharp
|> seventhsHarmonizer ScaleDegrees.III
|> toDrop3
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor BFlat
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%s"

createScaleNotes HarmonicMinor C
|> seventhsHarmonizer ScaleDegrees.VII
|> createGuitarChord SixthString
|> tabify
|> printf "\n%A"

createScaleNotes HarmonicMinor C
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
let chordNotes = chord.Notes |> List.map fst |> List.sort

scalesFitting chord
```

#### Output

```
      CMaj7
E||-------------||
B||----8--------||
G||----9--------||
D||----9--------||
A||-------------||
E||----8--------||

      CMaj7
E||-------------||
B||----5--------||
G||----4--------||
D||----5--------||
A||----3--------||
E||-------------||

      CMaj
E||------------||
B||------------||
G||----12------||
D||----14------||
A||----15------||
E||------------||

      C9
E||----------||
B||----3-----||
G||----3-----||
D||----2-----||
A||----3-----||
E||----------||

      CMaj9
E||-------------||
B||----3--------||
G||----4--------||
D||----2--------||
A||----3--------||
E||-------------||

      GbMaj
E||-------------||
B||-------------||
G||-------------||
D||----11-------||
A||----13-------||
E||----14-------||

      AMaj7
E||-------------||
B||----14-------||
G||----13-------||
D||----14-------||
A||----12-------||
E||-------------||

      CMaj7
E||----3--------||
B||----5--------||
G||----4--------||
D||-------------||
A||----3--------||
E||-------------||

      AMaj7
E||-------------||
B||----5--------||
G||----6--------||
D||----6--------||
A||-------------||
E||----5--------||

      ADim7
E||----2--------||
B||----1--------||
G||----2--------||
D||----1--------||
A||----3--------||
E||----2--------||

      BDim7
E||----1--------||
B||----3--------||
G||----1--------||
D||----3--------||
A||----2--------||
E||----1--------||

"      BDim7
E||----1--------||
B||----3--------||
G||----1--------||
D||----3--------||
A||----2--------||
E||----1--------||
"
"      BDim7
E||----1--------||
B||----0--------||
G||----1--------||
D||----0--------||
A||----2--------||
E||----1--------||
"
      GMaj   CMaj   AMin   DMaj
E||----3------0------0------2-------||
B||----0------1------1------3-------||
G||----0------0------2------2-------||
D||----0------2------2------0-------||
A||----2------3------0--------------||
E||----3----------------------------||

"C"
"C#"
"Db"
E
EFlat
7
DiminishedFifth
A
"DiminishedFifth"
DiminishedFifth
[C; D; E; F; G; A; B]
[EFlat; F; G; AFlat; BFlat; C; D]
[D; E; F; G; A; BFlat; C]
[C; DFlat; EFlat; F; G; AFlat; BFlat]
[C; D; E; FSharp; GSharp; A; B]
["C"; "E"; "G"; "B"]
C
B
"CMaj7"
[(C, Root); (E, Third); (G, Fifth); (B, Seventh)]
[(E, Third); (G, Fifth); (B, Seventh); (C, Root)]
[(G, Fifth); (B, Seventh); (C, Root); (E, Third)]
[(B, Seventh); (C, Root); (E, Third); (G, Fifth)]
[(C, Root); (G, Fifth); (B, Seventh); (E, Third)]
[(C, Root); (B, Seventh); (E, Third); (G, Fifth)]
{Notes = [(G, Fifth); (C, Root); (E, Third)];
 ChordType = Closed;
 Name = "CMaj";}
{Notes = [(G, Fifth); (C, Root); (E, Third)];
 ChordType = Closed;
 Name = "CMaj";}
{Notes = [(E, Third); (G, Fifth); (C, Root)];
 ChordType = Closed;
 Name = "CMaj";}
{Notes = [(E, Third); (G, Fifth); (C, Root)];
 ChordType = Closed;
 Name = "CMaj";}

chordsFitting

[{Notes = [(A, Root); (D, Third); (F, Fifth)];
  ChordType = Closed;
  Name = "ASus4Aug";}; {Notes = [(D, Root); (F, Third); (A, Fifth)];
                        ChordType = Closed;
                        Name = "DMin";}]
[{Notes = [(C, Root); (E, Third); (G, Fifth); (B, Seventh)];
  ChordType = Closed;
  Name = "CMaj7";}]
 ```
