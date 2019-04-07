namespace Vaughan

    module ImprovisationGuitar =
        open Notes
        open Chords
        open Scales
        open Guitar
        open Infrastructure

        let private limitLineTo maxNotes notes =
            if notes |> List.length <= maxNotes
            then notes
            else
                notes
                |> List.rev
                |> List.take maxNotes
                |> List.rev

        let private createAprpeggioSequence tone chord arpeggioNotes = 
            let firstTone = arpeggioNotes |> List.filter (fun f -> f.Note = tone chord) |> List.last
            let firstToneIndex = arpeggioNotes |> List.findIndex (fun af -> af = firstTone)
            {
                BaseChord = chord;
                ArpeggioFrets = arpeggioNotes |> List.take (firstToneIndex + 1)
            }

        let private approachFromAbove fret =
            {
                GuitarString = fret.GuitarString;
                Fret = fret.Fret + 1;
                Note = sharp fret.Note
            }

        let private approachFromBelow fret =
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
            arpeggio.ArpeggioFrets
            |> List.sortByDescending (fun f -> f.GuitarString, f.Fret)
            |> createAprpeggioSequence tone arpeggio.BaseChord

        let descendingArpeggioFrom tone (arpeggio:GuitarArpeggio) =
            arpeggio.ArpeggioFrets
            |> List.sortBy (fun f -> f.GuitarString, f.Fret)
            |> createAprpeggioSequence tone arpeggio.BaseChord 

        let aproachAscendingArpeggioFrom tone approachNote maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioNotes = (arpeggio |> ascendingArpeggioFrom tone).ArpeggioFrets |> List.rev
            let aprochedTone = arpeggioNotes.Head
            (arpeggioNotes.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [aprochedTone] @ [approachNote aprochedTone]
            |> limitLineTo maxNotes

        let aproachDescendingArpeggioFrom tone approachNote maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioNotes = (arpeggio |> descendingArpeggioFrom tone).ArpeggioFrets
            let aprochedTone = arpeggioNotes |> List.last
            (arpeggioNotes |> List.rev |> List.tail |> List.rev)
            @
            [aprochedTone] @ [approachNote aprochedTone]
            |> limitLineTo maxNotes

        let enclosedAscendingArpeggioFrom tone maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioNotes = (arpeggio |> ascendingArpeggioFrom tone).ArpeggioFrets |> List.rev
            (arpeggioNotes.Tail |> List.sortByDescending (fun f -> f.GuitarString, f.Fret))
            @
            [arpeggioNotes.Head] @ [approachFromBelow arpeggioNotes.Head] @ [approachFromAbove arpeggioNotes.Head]
            |> limitLineTo maxNotes

        let enclosedDescendingArpeggioFrom tone maxNotes (arpeggio:GuitarArpeggio) =
            let arpeggioNotes = (arpeggio |> descendingArpeggioFrom tone).ArpeggioFrets
            let enclosedTone = arpeggioNotes |> List.last
            (arpeggioNotes |> List.rev |> List.tail |> List.rev)
            @
            [enclosedTone] @ [approachFromBelow enclosedTone] @ [approachFromAbove enclosedTone]
            |> limitLineTo maxNotes

        let ascEightsRootEnclosed = enclosedAscendingArpeggioFrom root 8
        let descEightsRootEnclosed = enclosedDescendingArpeggioFrom root 8
        let ascEightsThirdEnclosed = enclosedAscendingArpeggioFrom third 8
        let descEightsThirdEnclosed = enclosedDescendingArpeggioFrom third 8
        let ascEightsSeventhEnclosed = enclosedAscendingArpeggioFrom seventh 8
        let descEightsSeventhEnclosed = enclosedDescendingArpeggioFrom seventh 8

        let ascEightsRootAbove = aproachAscendingArpeggioFrom root approachFromAbove 8
        let descEightsRootAbove = aproachDescendingArpeggioFrom root approachFromAbove 8
        let ascEightsRootBelow = aproachAscendingArpeggioFrom root approachFromBelow 8
        let descEightsRootBelow = aproachDescendingArpeggioFrom root approachFromBelow 8

        let ascEightsThirdAbove = aproachAscendingArpeggioFrom third approachFromAbove 8
        let descEightsThirdAbove = aproachDescendingArpeggioFrom third approachFromAbove 8
        let ascEightsThirdBelow = aproachAscendingArpeggioFrom third approachFromBelow 8
        let descEightsThirdBelow = aproachDescendingArpeggioFrom third approachFromBelow 8

        let ascEightsSeventhAbove = aproachAscendingArpeggioFrom seventh approachFromAbove 8
        let descEightsSeventhAbove = aproachDescendingArpeggioFrom seventh approachFromAbove 8
        let ascEightsSeventhBelow = aproachAscendingArpeggioFrom seventh approachFromBelow 8
        let descEightsSeventhBelow = aproachDescendingArpeggioFrom seventh approachFromBelow 8

        let generateArpeggioPatterns tone (arpeggio:GuitarArpeggio) =
            (ascendingArpeggioFrom tone arpeggio).ArpeggioFrets |> List.take arpeggio.BaseChord.Notes.Length
            |> permutations
            |> Seq.toList

        let createAscendingScaleSequence minFret maxFret maxNotes startingTone (scale : Scale) = 
            (guitarScale minFret maxFret scale).Frets
            |> List.sortBy (fun n -> n.GuitarString, n.Fret)
            |> List.skipWhile (fun n -> n.Note <> startingTone)
            |> List.sortByDescending (fun n -> n.GuitarString, n.Fret)
            |> limitLineTo maxNotes

        let createDescendingScaleSequence minFret maxFret maxNotes startingTone (scale : Scale) = 
            (guitarScale minFret maxFret scale).Frets
            |> List.sortBy (fun n -> n.GuitarString, n.Fret)
            |> List.skipWhile (fun n -> n.Note <> startingTone)
            |> List.take (maxNotes)

        let createAscendingScaleSequenceFromRootToSeventh minFret maxFret (scale : Scale) = 
            (guitarScale minFret maxFret scale).Frets
            |> List.sortBy (fun n -> n.GuitarString, n.Fret)
            |> List.skipWhile (fun n -> n.Note <> scale.Notes.[0])
            |> List.sortByDescending (fun n -> n.GuitarString, n.Fret)
            |> limitLineTo 7

        let createDescendingScaleSequenceFromSeventhToRoot minFret maxFret (scale : Scale) = 
            (guitarScale minFret maxFret scale).Frets
            |> List.sortBy (fun n -> n.GuitarString, n.Fret)
            |> List.skipWhile (fun n -> n.Note <> scale.Notes.[0])
            |> List.take (7)

        let createScaleSequenceFromSeventhToRoot minFret maxFret (scale : Scale) = 
            createDescendingScaleSequenceFromSeventhToRoot minFret maxFret scale
            @
            (createAscendingScaleSequenceFromRootToSeventh minFret maxFret scale
            |> List.skip 1)
