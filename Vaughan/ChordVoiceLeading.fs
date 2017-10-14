namespace Vaughan

    module ChordVoiceLeading =
        open Chords
        open Domain
        open Notes
        open Infrastructure

        let private isLeadFunctionOnChordDesiredFunction (chord:Chord) desiredNoteFunction desiredPosition =
            snd (chord.Notes |> desiredPosition) = desiredNoteFunction

        let rec private repeatInversion (chord:Chord) times =
            match times with
            | 0 -> chord
            | _ -> repeatInversion (chord |> invert) (times - 1)

        let private allInversions (chord:Chord) =
            let notesInChord = chord.Notes |> List.length
            [for index in 1 .. notesInChord do yield repeatInversion chord index]

        let private inversionForFunction (chord:Chord) desiredNoteFunction desiredPosition =
            allInversions chord
            |> List.filter (fun (c:Chord) -> isLeadFunctionOnChordDesiredFunction c desiredNoteFunction desiredPosition)
            |> List.head

        let closestChord (minOf:Chord->Chord->Chord) (list:Chord list) =
            list |> List.fold minOf (list |> List.head)

        let distanceBetweenNoteAndChordNote chord desiredPosition note =
            measureAbsoluteSemitones (desiredPosition chord) note

        let chordWithNoteInDesiredPositionCosestToNote note desiredPosition c1 c2 =
            if (distanceBetweenNoteAndChordNote c1 desiredPosition note) <
                (distanceBetweenNoteAndChordNote c2 desiredPosition note)
            then c1 else c2

        let private invertionWithNoteClosestToNote chord note desiredPosition =
            allInversions chord
            |> closestChord (chordWithNoteInDesiredPositionCosestToNote note desiredPosition)

        let inversionForFunctionAsLead:InversionForFunctionAsLead = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.last

        let inversionForFunctionAsBass:InversionForFunctionAsBass = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.head

        let invertionWithLeadClosestToNote:InvertionWithLeadClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note lead

        let invertionWithBassClosestToNote:InvertionWithBassClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note bass