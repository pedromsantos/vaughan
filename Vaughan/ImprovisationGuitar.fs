namespace Vaughan

    module ImprovisationGuitar =
        open System

        open Notes
        open Chords
        open Scales
        open Guitar
        open GuitarTab

        let createArpeggioGuitarMelodicLineFromChords minFret maxFret chords =
            chords
            |> List.map ((fun c -> createGuitarArpeggio minFret maxFret c) 
                                   >> createGuitarMelodicLineFromArpeggio)

        let createScaleGuitarMelodicLineFromChords minFret maxFret chords =
            chords
            |> List.map (scalesFitting 
                            >> ((fun sl -> sl |> List.map (fun s -> createGuitarScale minFret maxFret s)) 
                            >> (fun sl -> sl |> List.map (fun s -> sprintf "%s %A" (noteName s.Scale.Notes.[0]) s.Scale.Scale, createGuitarMelodicLineFromScale s))))
        
        let tabifyArpeggiosFromChords minFret maxFret chords =
            chords
            |> createArpeggioGuitarMelodicLineFromChords minFret maxFret
            |> List.map tabifyMelodicLine
            |> List.mapi (fun i ml -> (name chords.[i]) + Environment.NewLine + ml)

        let tabifyScalesFromChords minFret maxFret chords =
            chords
            |> createScaleGuitarMelodicLineFromChords minFret maxFret
            |> List.map (fun mls -> mls |> List.map (fun ml -> (fst ml) + Environment.NewLine + (snd ml |> tabifyMelodicLine)))
            |> List.mapi (fun i mls -> (name chords.[i])::mls)
            |> List.collect id