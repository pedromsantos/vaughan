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
            [for index in 1 .. (chord.Notes |> List.length) do yield chord]
            |> List.fold (fun x acc -> [acc] @ x) []

        let private inversionForFunction (chord:Chord) desiredNoteFunction desiredPosition =
            allInversions chord
            |> List.filter (fun (c:Chord) -> isLeadFunctionOnChordDesiredFunction c desiredNoteFunction desiredPosition)
            |> List.head

        let private invertionWithNoteClosestToNote chord note desiredPosition =
            allInversions chord
            |> min (fun c1 c2 ->
                if (measureAbsoluteSemitones (desiredPosition c1) note) < (measureAbsoluteSemitones (desiredPosition c2) note)
                then c1 else c2

        let inversionForFunctionAsLead:InversionForFunctionAsLead = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.last

        let inversionForFunctionAsBass:InversionForFunctionAsBass = fun chord desiredNoteFunction ->
            inversionForFunction chord desiredNoteFunction List.head

        let invertionWithLeadClosestToNote:InvertionWithLeadClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note lead

        let invertionWithBassClosestToNote:InvertionWithBassClosestToNote = fun  chord note ->
            invertionWithNoteClosestToNote chord note bass