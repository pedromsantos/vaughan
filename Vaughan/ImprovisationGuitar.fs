namespace Vaughan

    module ImprovisationGuitar =
        open Notes
        open Scales
        open Guitar

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

        let arpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let firstRoot = arpeggio.ArpeggioFrets |> List.filter (fun f -> f.Note = tone arpeggio.BaseChord) |> List.last
            let positionFirstRoot = arpeggio.ArpeggioFrets |> List.findIndex (fun af -> af = firstRoot)
            {
                BaseChord = arpeggio.BaseChord;
                ArpeggioFrets = arpeggio.ArpeggioFrets |> List.take (positionFirstRoot + 1)
            }

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

        let enclosedArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let rootArpeggioFrets = (arpeggio |> arpeggioFrom tone).ArpeggioFrets |> List.rev
            (rootArpeggioFrets.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [rootArpeggioFrets.Head] @ [enclosureBelow rootArpeggioFrets.Head] @ [enclosureAbove rootArpeggioFrets.Head]