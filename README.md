# Vaughan - F# music library

Vaughan is named after Blues guitarist [Stevie Ray Vaughan](https://en.wikipedia.org/wiki/Stevie_Ray_Vaughan)

## Getting started

1. Clone the repository to your machine
2. Build
  1. On Windows execute build.cmd
  2. On macos / unix execute build.sh (requires mono)

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
| intervalNameDiminishedFifth  | "DiminishedFifth" |
| fromDistance 6               | "DiminishedFifth" |

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
| chordFromRootAndFunction c Major7                           | cMaj7                                             |
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
