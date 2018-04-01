namespace Vaughan

    module ImprovisationGuitar =
        open System

        open Chords
        open Guitar
        open GuitarTab

        let createGuitarMelodicLineFromChords minFret maxFret chords =
            chords
            |> List.map ((fun c -> createGuitarArpeggio minFret maxFret c) 
                                   >> createGuitarMelodicLineFromArpeggio)
        
        let tabifyArpeggiosFromChords minFret maxFret chords =
            chords
            |> createGuitarMelodicLineFromChords minFret maxFret
            |> List.map tabifyMelodicLine
            |> List.mapi (fun i ml -> (name chords.[i]) + Environment.NewLine + ml)