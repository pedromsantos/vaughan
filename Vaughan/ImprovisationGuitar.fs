namespace Vaughan

    module ImprovisationGuitar =
        open System

        open Notes
        open Chords
        open Scales
        open Guitar
        open GuitarTab

        let createArpeggiosFromChords minFret maxFret chords =
            chords
            |> List.map (fun c -> guitarArpeggio minFret maxFret c) 

        let createScalesForChords minFret maxFret chords =
            chords
            |> List.map (scalesFitting >> (fun sl -> sl |> List.map (fun s -> guitarScale minFret maxFret s)))
        
        let tabifyArpeggiosFromChords minFret maxFret chords =
            let arpeggios = chords |> createArpeggiosFromChords minFret maxFret
            [StandardTunning; Start] @ (arpeggios |> List.map (fun a -> Arpeggio(a))) @ [End]
            |> renderTab
            
        let tabifyScalesFromChords minFret maxFret chords =
            chords 
            |> createScalesForChords minFret maxFret
            |> List.map (fun ss -> [StandardTunning; Start] @ (ss |> List.map (fun s -> Scale(s))) @ [End] |> renderTab)