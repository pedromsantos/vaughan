# Vaughan - F# music library

Vaughan is named after Blues guitarist [Stevie Ray Vaughan](https://en.wikipedia.org/wiki/Stevie_Ray_Vaughan)

## Usage

### Notes

```fsharp
open Vaughan.Notes
```

| Example                                   | Output          |
| ----------------------------------------- | --------------- |
| ```fsharp noteName C```                   | "C"             |
| ```fsharp noteName CSharp```              | "C#"            |
| ```fsharp noteName DFlat```               | "Db"            |
| ```fsharp sharp EFlat```                  | E               |
| ```fsharp flat E```                       | EFlat           |
| ```fsharp measureAbsoluteSemitones C G``` | 7               |
| ```fsharp intervalBetween C FSharp```     | DiminishedFifth |
| ```fsharp transpose C MajorSixth```       | A               |

### Intervals

```fsharp
open Vaughan.Notes
```

| Example                                   | Output            |
| ----------------------------------------- | ----------------- |
| ```fsharp intervalNameDiminishedFifth```  | "DiminishedFifth" |
| ```fsharp fromDistance 6```               | "DiminishedFifth" |

### Keys

```fsharp
open Vaughan.Notes
open Vaughan.Keys
```

| Example           | Output                              |
| ----------------- | ----------------------------------- |
| ```fsharp notes CMajor```      | ```fsharp [ C; D; E; F; G; A; B ]```             |
| ```fsharp notes EFlatMajor```  | ```fsharp [ EFlat; F; G; AFlat; BFlat; C; D ]``` |
| ```fsharp notes DMinor```     | ```fsharp [ D; E; F; G; A; BFlat; C ]```         |

### Scales

```fsharp
open Vaughan.Notes
open Vaughan.Scales
```

| Example                       | Output                                     |
| ----------------------------- | ------------------------------------------ |
| ```fsharp createScale Phrygian C```        | ```fsharp [ C; DFlat; EFlat; F; G; AFlat; BFlat]```     |
| ```fsharp createScale LydianAugmented C``` | ```fsharp [ C; D; E; FSharp; GSharp; A; B ]```          |


### Chords

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords

    let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
```

| Example                                        | Output                                            |
| ---------------------------------------------- | ------------------------------------------------- |
| ```fsharp noteForFunction cMaj7 Root```                     | ```fsharp C```                                                 |
| ```fsharp noteForFunction cMaj7 Third```                    | ```fsharp E```                                                 |
| ```fsharp noteForFunction cMaj7 Fifth```                    | ```fsharp G```                                                 |
| ```fsharp noteForFunction cMaj7 Sevent```                   | ```fsharp B```                                                 |
| ```fsharp noteNames cMaj7```                                | ```fsharp ["C"; "E"; "G"; "B"]```                              |
| ```fsharp bass cMaj7```                                     | ```fsharp C```                                                 |
| ```fsharp lead cMaj7```                                     | ```fsharp B```                                                 |
| ```fsharp name cMaj7```                                     | ```fsharp "CMaj7"```                                           |
| ```fsharp chordFromRootAndFunction cMaj7 Major7```          | ```fsharp cMaj7```                                             |
| ```fsharp cMaj7.notes```                                    | ```fsharp [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]``` |
| ```fsharp (cMaj7 |> invert).notes```                        | ```fsharp [(E, Third); (G, Fifth); (B, Seventh); (C, Root)]``` |
| ```fsharp (cMaj7 |> invert |> invert).notes```              | ```fsharp [(G, Fifth); (B, Seventh); (C, Root); (E, Third)]``` |
| ```fsharp (cMaj7 |> invert |> invert |> invert).notes```    | ```fsharp [(B, Seventh); (C, Root); (E, Third); (G, Fifth)]``` |
| ```fsharp (cMaj7 |> toDrop2).notes```                       | ```fsharp [(C, Root); (G, Fifth); (B, Seventh); (E, Third)]``` |
| ```fsharp (cMaj7 |> toDrop3).notes```                       | ```fsharp [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]``` |

### Scale harmonizing

```fsharp
open Vaughan.Notes
open Vaughan.Scales
open Vaughan.Chords
open Vaughan.ScaleHarmonizer

let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
let cMin = {notes= [(C, Root); (EFlat, Third); (G, Fifth)]; chordType=Closed}
```

 Example                                       | Output                                 |
| -------------------------------------------- | -------------------------------------- |
| ```fsharp thirds ScaleDgrees.I cIonian```                 | ```fsharp [ C; E; G; B ]```                        |
| ```fsharp triadsHarmonizer ScaleDgrees.I cIonian```       | ```fsharp cMaj```                                  |
| ```fsharp triadsHarmonizer ScaleDgrees.I cMinor```        | ```fsharp cMin```                                  |