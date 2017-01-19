# Vaughan - F# music library

Vaughan is named after Blues guitarist [Stevie Ray Vaughan](https://en.wikipedia.org/wiki/Stevie_Ray_Vaughan)

![](https://ci.appveyor.com/api/projects/status/github/pedromsantos/vaughan?branch=master&svg=true)
[![NuGet](http://img.shields.io/nuget/v/Vaughan.svg)](https://www.nuget.org/packages/Vaughan)

## Getting started

1. Clone the repository to your machine
2. Build
  1. On Windows execute build.cmd (requires Powershell)
  2. On macos / unix execute build.sh (requires mono, curl and jq)

## Usage

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

| Example           | Output                              |
| ----------------- | ----------------------------------- |
| notes CMajor      | [ C; D; E; F; G; A; B ]             |
| notes EFlatMajor  | [ EFlat; F; G; AFlat; BFlat; C; D ] |
| notes DMinor      | [ D; E; F; G; A; BFlat; C ]         |

### Scales

```fsharp
open Vaughan.Notes
open Vaughan.Scales
```

| Example                       | Output                                     |
| ----------------------------- | ------------------------------------------ |
| createScale Phrygian C        | [ C; DFlat; EFlat; F; G; AFlat; BFlat]     |
| createScale LydianAugmented C | [ C; D; E; FSharp; GSharp; A; B ]          |


### Chords

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords

let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
let cMaj = chordFromRootAndFunction c Major
```

| Example                                                     | Output                                            |
| ----------------------------------------------------------- | ------------------------------------------------- |
| noteForFunction cMaj7 Root                                  | C                                                 |
| noteForFunction cMaj7 Third                                 | E                                                 |
| noteForFunction cMaj7 Fifth                                 | G                                                 |
| noteForFunction cMaj7 Sevent                                | B                                                 |
| noteNames cMaj7                                             | ["C"; "E"; "G"; "B"]                              |
| bass cMaj7                                                  | C                                                 |
| lead cMaj7                                                  | B                                                 |
| name cMaj7                                                  | "CMaj7"                                           |
| cMaj7.notes                                                 | [(C, Root); (E, Third); (G, Fifth); (B, Seventh)] |
| (cMaj7 &#124;> invert).notes                                | [(E, Third); (G, Fifth); (B, Seventh); (C, Root)] |
| (cMaj7 &#124;> invert &#124;> invert).notes                 | [(G, Fifth); (B, Seventh); (C, Root); (E, Third)] |
| (cMaj7 &#124;> invert &#124;> invert &#124;> invert).notes  | [(B, Seventh); (C, Root); (E, Third); (G, Fifth)] |
| (cMaj7 &#124;> toDrop2).notes                               | [(C, Root); (G, Fifth); (B, Seventh); (E, Third)] |
| (cMaj7 &#124;> toDrop3).notes                               | [(C, Root); (B, Seventh); (E, Third); (G, Fifth)] |
| inversionForFunctionAsLead cMaj Third                       | cMaj &#124;> invert &#124;> invert                |
| inversionForFunctionAsBass cMaj Fifth                       | cMaj &#124;> invert &#124;> invert                |
| invertionWithLeadClosestToNote cMaj CSharp                  | cMaj &#124;> invert                               |
| invertionWithBAssdClosestToNote cMaj F                      | cMaj &#124;> invert                               |

### Scale harmonizing

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords
open Vaughan.ScaleHarmonizer

let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
let cMin = {notes= [(C, Root); (EFlat, Third); (G, Fifth)]; chordType=Closed}

let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}

let cIonian = createScale Ionian C
let cMinor = createScale HarmonicMinor C
```

 Example                                       | Output         |
| -------------------------------------------- | -------------- |
| thirds ScaleDgrees.I cIonian                 | [ C; E; G; B ] |
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
createScale Ionian C
|> triadsHarmonizer ScaleDgrees.I
|> chordToGuitarClosedChord SixthString
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
createScale Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop2
|> chordToGuitarClosedChord FifthString
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
createScale Ionian A
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop2
|> chordToGuitarClosedChord FifthString
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
createScale Ionian F
|> seventhsHarmonizer ScaleDgrees.I
|> chordToGuitarClosedChord FourthString
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
createScale Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop3
|> chordToGuitarClosedChord SixthString
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
createScale Ionian C
|> seventhsHarmonizer ScaleDgrees.I
|> toDrop3
|> chordToGuitarClosedChord FifthString
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
let cIonian = createScale Ionian C
let cMaj = seventhsHarmonizer ScaleDgrees.I cIonian
let dMin = seventhsHarmonizer ScaleDgrees.II cIonian
let eMin = seventhsHarmonizer ScaleDgrees.III cIonian
let fMaj = seventhsHarmonizer ScaleDgrees.IV cIonian

let guitarChords =  [cMaj; dMin; eMin; fMaj] 
                    |> List.map (toDrop2 >> (chordToGuitarClosedChord FifthString))

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
|> chordToGuitarClosedChord SixthString
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
