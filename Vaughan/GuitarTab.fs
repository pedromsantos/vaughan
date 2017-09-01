module GuitarTab
    open System
    open Domain
    open Notes
    open Chords
    open Guitar
    open Infrastructure

    type private TabColumn = string list

    type private TabColumns = TabColumn list

    type private TabLine = string list

    type private TabLines = TabLine list

    let private standardTunningTab = ["E"; "B"; "G"; "D"; "A"; "E"]

    let private startTab = "||-" |> List.replicate 6

    let private barTab = "-|-" |> List.replicate 6

    let private endTab =  ("-||" + Environment.NewLine) |> List.replicate 6

    [<AutoOpen>]
    module private TabifyChords =
        let private stringForLead guitarChord =
            (guitarChord.Frets |> List.last).GuitarString

        let private stringForBass guitarChord =
            (guitarChord.Frets |> List.head).GuitarString

        let private isNinthChord (chord:Chord) =
            chord.Notes |> List.exists (fun n -> snd n = Ninth) 

        let chordName guitarChord =
            guitarChord.Chord.Name

        let numberOfMutedHighStrings guitarChord =
            match stringForLead guitarChord with
                | SecondString -> 1
                | ThirdString -> 2
                | FourthString -> 3
                | _ -> 0

        let numberOfMutedLowStrings guitarChord =
            match stringForBass guitarChord with
            | FifthString -> 1
            | FourthString  -> 2
            | ThirdString  -> 3
            | _ -> 0

        let private chordNameLength guitarChord =
            (guitarChord |> chordName).Length

        let private tabifyMutedString guitarChord =
            String.replicate (guitarChord |> chordNameLength) "-"

        let private tabifyMutedHigherStrings mutedStringTab guitarChord =
            let mutedStrings = numberOfMutedHighStrings guitarChord
            List.replicate mutedStrings mutedStringTab

        let private tabifyMutedLowerStrings mutedStringTab guitarChord =
            let mutedStrings = numberOfMutedLowStrings guitarChord
            List.replicate mutedStrings mutedStringTab

        let private tabifyFret mutedStringTab fret =
            if fret.Fret = -1 then
                sprintf "%s" mutedStringTab
            else
                sprintf "%i%s" fret.Fret (mutedStringTab.[(string(fret.Fret)).Length..])

        let private tabifyFrets mutedStringTab guitarChord =
            guitarChord.Frets
            |> List.map (fun fret -> tabifyFret mutedStringTab fret)
            |> List.rev

        let private convertTabColumsToTabLines stringOrdinal (tabifiedChords: TabColumns) =
            tabifiedChords |> List.map (fun f -> f.[stringOrdinal])

        let private convertColumnChordsToGuitarStringLines (tabifiedChords: TabColumns) =
            [0 .. 5]
            |> List.map (fun stringOrdinal -> convertTabColumsToTabLines stringOrdinal tabifiedChords)

        let private renderGuitarStringLine (tabifiedFretsForString:TabLine) =
            tabifiedFretsForString
            |> List.fold (fun acc fret -> acc + fret + "---" ) "---"

        let private renderGuitarStringLines (tabifiedFretsForStrings:TabLines) =
            tabifiedFretsForStrings
            |> List.map renderGuitarStringLine

        let private renderTabifiedChords tabifiedChords =
            tabifiedChords
            |> List.mapi (fun guitarStringNumber tabifiedGuitarString ->
                            standardTunningTab.[guitarStringNumber] +
                            startTab.[guitarStringNumber] +
                            tabifiedGuitarString +
                            endTab.[guitarStringNumber])

        let private tabifyChord guitarChord =
            let mutedStringTab = tabifyMutedString guitarChord
            (tabifyMutedHigherStrings mutedStringTab guitarChord)
            @ (tabifyFrets mutedStringTab guitarChord)
            @ (tabifyMutedLowerStrings mutedStringTab guitarChord)

        let tabifyChords guitarChords =
            guitarChords
            |> List.map tabifyChord
            |> convertColumnChordsToGuitarStringLines
            |> renderGuitarStringLines
            |> renderTabifiedChords

        let tabifyChordNames guitarChords =
            let chordNameSeparator = "   "
            let separatedChordNames =
                guitarChords |> List.map (fun guitarChord -> chordNameSeparator + guitarChord.Chord.Name)
            [chordNameSeparator] @ separatedChordNames @ [chordNameSeparator; Environment.NewLine;]

    [<AutoOpen>]
    module private ShapifyChords =
        let private shapifyMutedLowerStrings guitarChord =
            let mutedStrings = numberOfMutedLowStrings guitarChord
            List.replicate mutedStrings "X"

        let private shapifyMutedHigherStrings guitarChord =
            let mutedStrings = numberOfMutedHighStrings guitarChord
            List.replicate mutedStrings "X"

        let private shapifyFret fret =
            if fret.Fret = -1 then
                "X"
            else
                sprintf "%i" fret.Fret

        let private shapifyFrets guitarChord =
            guitarChord.Frets |> List.map shapifyFret

        let shapifyChord guitarChord =
            (shapifyMutedLowerStrings guitarChord)
            @ (shapifyFrets guitarChord)
            @ (shapifyMutedHigherStrings guitarChord)

    let tabifyAll:TabifyAll = fun guitarChords ->
        (tabifyChordNames guitarChords) @ (tabifyChords guitarChords)
        |> List.fold (+) ""

    let tabify:Tabify = fun guitarChord ->
        tabifyAll [guitarChord]

    let shapify:Shapify = fun guitarChord ->
        guitarChord.Chord.Name + Environment.NewLine +
        "EADGBE" + Environment.NewLine +
        (guitarChord |> shapifyChord |> List.fold (+) "") + Environment.NewLine

    let (|~) chord bassString =
        createGuitarChord bassString chord

    let (>|<) (chords:GuitarChord list) (chord:GuitarChord) =
        chord :: chords |> rotateByOne
