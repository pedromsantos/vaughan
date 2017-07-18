namespace Vaughan

    module ChordVoiceLeading =
        open Chords
        open Domain
        open Notes
        open Infrastructure

        let private isLeadFunctionOnChordDesiredFunction (chord:Chord) desiredNoteFunction desiredPosition =
            snd (chord.Notes |> desiredPosition) = desiredNoteFunction

        let rec private repeatInversion chord times =
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

        let private invertionWithNoteClosestToNote chord note desiredPosition =
            (allInversions chord
            |> min (fun c1 c2 ->
                if (measureAbsoluteSemitones (desiredPosition c1) note) < (measureAbsoluteSemitones (desiredPosition c2) note)
                then c1 else c2)).Value

        let inversionForFunctionAsLead:IInversionForFunctionAsLead = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.last

        let inversionForFunctionAsBass:IInversionForFunctionAsBass = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.head

        let invertionWithLeadClosestToNote:IInvertionWithLeadClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note lead

        let invertionWithBassClosestToNote:IinvertionWithBassClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note bass