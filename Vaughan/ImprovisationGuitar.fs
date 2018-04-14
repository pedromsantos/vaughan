namespace Vaughan

    module ImprovisationGuitar =
        open Notes
        open Scales
        open Guitar

        let private enclosureAbove fret =
            {
                GuitarString = fret.GuitarString; 
                Fret = fret.Fret + 1; 
                Note = sharp fret.Note
            }
        
        let private enclosureBelow fret =
            {
                GuitarString = fret.GuitarString; 
                Fret = fret.Fret - 1; 
                Note = flat fret.Note
            }
            
        let createArpeggiosFromChords minFret maxFret chords =
            chords
            |> List.map (fun c -> guitarArpeggio minFret maxFret c) 

        let createScalesForChords minFret maxFret chords =
            chords
            |> List.map (scalesFitting >> (fun sl -> sl |> List.map (fun s -> guitarScale minFret maxFret s)))

        let createTabArpeggiosFromChords minFret maxFret chords =
            chords
            |> createArpeggiosFromChords minFret maxFret
            |> List.map (fun a -> Arpeggio(a))
        
        let createTabScalesFromChords minFret maxFret scales =
            scales
            |> createScalesForChords minFret maxFret
            |> List.map (fun ss -> ss |> List.map (fun s -> Scale(s)))

        let ascendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let firstTone = arpeggio.ArpeggioFrets |> List.filter (fun f -> f.Note = tone arpeggio.BaseChord) |> List.last
            let positionFirstTone = arpeggio.ArpeggioFrets |> List.findIndex (fun af -> af = firstTone)
            {
                BaseChord = arpeggio.BaseChord;
                ArpeggioFrets = arpeggio.ArpeggioFrets 
                                |> List.take (positionFirstTone + 1)
                                |> List.sortByDescending (fun f -> f.GuitarString, f.Fret)
            }

        let enclosedAscendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> ascendingArpeggioFrom tone).ArpeggioFrets |> List.rev
            (arpeggioFrets.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [arpeggioFrets.Head] @ [enclosureBelow arpeggioFrets.Head] @ [enclosureAbove arpeggioFrets.Head]

        let descendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let firstTone = arpeggio.ArpeggioFrets |> List.filter (fun f -> f.Note = tone arpeggio.BaseChord) |> List.head
            let positionFirstTone = arpeggio.ArpeggioFrets |> List.findIndex (fun af -> af = firstTone)
            {
                BaseChord = arpeggio.BaseChord;
                ArpeggioFrets = arpeggio.ArpeggioFrets 
                                |> List.skip (positionFirstTone)
                                |> List.sortBy (fun f -> f.GuitarString, f.Fret)
            }
        
        let enclosedDescendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> descendingArpeggioFrom tone).ArpeggioFrets
            let enclosedTone = arpeggioFrets |> List.last
            (arpeggioFrets |> List.rev |> List.tail |> List.rev)
            @ [enclosedTone] @ [enclosureBelow enclosedTone] @ [enclosureAbove enclosedTone]