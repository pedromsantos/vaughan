namespace Vaughan

    module ImprovisationGuitar =
        open Notes
        open Chords
        open Scales
        open Guitar

        let private limitNoteLineTo maxNotes notes =
            if notes |> List.length <= maxNotes
            then notes
            else
                notes
                |> List.rev
                |> List.take maxNotes
                |> List.rev

        let passingToneAbove fret =
            {
                GuitarString = fret.GuitarString; 
                Fret = fret.Fret + 1; 
                Note = sharp fret.Note
            }
        
        let passingToneBelow fret =
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

        let private arpeggioFromArpeggio chord arpeggioNotes start =
            {
                BaseChord = chord;
                ArpeggioFrets = arpeggioNotes |> List.take (start + 1)
            }

        let ascendingArpeggioFrom tone (arpeggio:GuitarArpeggio)  =
            let sortedArpeggioNotes = 
                arpeggio.ArpeggioFrets 
                |> List.sortByDescending (fun f -> f.GuitarString, f.Fret)

            let firstTone = sortedArpeggioNotes |> List.filter (fun f -> f.Note = tone arpeggio.BaseChord) |> List.last
            let positionFirstTone = sortedArpeggioNotes |> List.findIndex (fun af -> af = firstTone)
            arpeggioFromArpeggio arpeggio.BaseChord sortedArpeggioNotes positionFirstTone

        let descendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            let sortedArpeggioNotes = 
                arpeggio.ArpeggioFrets 
                |> List.sortBy (fun f -> f.GuitarString, f.Fret)
            
            let firstTone = sortedArpeggioNotes |> List.filter (fun f -> f.Note = tone arpeggio.BaseChord) |> List.last
            let positionFirstTone = sortedArpeggioNotes |> List.findIndex (fun af -> af = firstTone)
            arpeggioFromArpeggio arpeggio.BaseChord sortedArpeggioNotes positionFirstTone

        let aproachAscendingArpeggioFrom tone passing maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> ascendingArpeggioFrom tone).ArpeggioFrets |> List.rev
            (arpeggioFrets.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [arpeggioFrets.Head] @ [passing arpeggioFrets.Head]
            |> limitNoteLineTo maxNotes
            
        let aproachDescendingArpeggioFrom tone passing maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> descendingArpeggioFrom tone).ArpeggioFrets
            let enclosedTone = arpeggioFrets |> List.last
            (arpeggioFrets |> List.rev |> List.tail |> List.rev)
            @ 
            [enclosedTone] @ [passing enclosedTone]
            |> limitNoteLineTo maxNotes

        let enclosedAscendingArpeggioFrom tone maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> ascendingArpeggioFrom tone).ArpeggioFrets |> List.rev
            (arpeggioFrets.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [arpeggioFrets.Head] @ [passingToneBelow arpeggioFrets.Head] @ [passingToneAbove arpeggioFrets.Head]
            |> limitNoteLineTo maxNotes
            
        let enclosedDescendingArpeggioFrom tone maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioFrets = (arpeggio |> descendingArpeggioFrom tone).ArpeggioFrets
            let enclosedTone = arpeggioFrets |> List.last
            (arpeggioFrets |> List.rev |> List.tail |> List.rev)
            @ 
            [enclosedTone] @ [passingToneBelow enclosedTone] @ [passingToneAbove enclosedTone]
            |> limitNoteLineTo maxNotes

        let ascEightsRootEnclosed = enclosedAscendingArpeggioFrom root 8
        let descEightsRootEnclosed = enclosedDescendingArpeggioFrom root 8
        let ascEightsThirdEnclosed = enclosedAscendingArpeggioFrom third 8
        let descEightsThirdEnclosed = enclosedDescendingArpeggioFrom third 8
        let ascEightsSeventhEnclosed = enclosedAscendingArpeggioFrom seventh 8
        let descEightsSeventhEnclosed = enclosedDescendingArpeggioFrom seventh 8

        let ascEightsRootAbove = aproachAscendingArpeggioFrom root passingToneAbove 8
        let descEightsRootAbove = aproachDescendingArpeggioFrom root passingToneAbove 8
        let ascEightsRootBelow = aproachAscendingArpeggioFrom root passingToneBelow 8
        let descEightsRootBelow = aproachDescendingArpeggioFrom root passingToneBelow 8

        let ascEightsThirdAbove = aproachAscendingArpeggioFrom third passingToneAbove 8
        let descEightsThirdAbove = aproachDescendingArpeggioFrom third passingToneAbove 8
        let ascEightsThirdBelow = aproachAscendingArpeggioFrom third passingToneBelow 8
        let descEightsThirdBelow = aproachDescendingArpeggioFrom third passingToneBelow 8

        let ascEightsSeventhAbove = aproachAscendingArpeggioFrom seventh passingToneAbove 8
        let descEightsSeventhAbove = aproachDescendingArpeggioFrom seventh passingToneAbove 8
        let ascEightsSeventhBelow = aproachAscendingArpeggioFrom seventh passingToneBelow 8
        let descEightsSeventhBelow = aproachDescendingArpeggioFrom seventh passingToneBelow 8