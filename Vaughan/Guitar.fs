namespace Vaughan

    module Guitar =
        [<AutoOpen>]
        module private GuitarFrets =
            open Domain
            open Notes

            let private isOpen fret =
                fret.Fret = 0

            let private isMuted fret =
                fret.Fret = -1

            let private isRaised fret =
                fret.Fret > 11

            let private fretDistance fret other =
                abs(fret.Fret - other.Fret)

            let private isStretched fret other =
                (fretDistance fret other) > 5

            let private raiseOctave fret =
                {fret with Fret = fret.Fret + (toDistance PerfectOctave)}

            let private isRaisable fret =
                not (isRaised fret) && not (isMuted fret)

            let private unstretchFret highestFret fret = 
                if isStretched fret highestFret && isRaisable fret
                then raiseOctave fret else fret

            let private highestFret frets = 
                frets |> List.maxBy (fun f -> f.Fret)

            let raiseOpenFrets frets =
                frets
                |> List.map (fun fret -> if isOpen fret then raiseOctave fret else fret)
            
            let unstretch frets = 
                frets |> List.map (fun f -> unstretchFret (highestFret frets) f)

         [<AutoOpen>]
         module private GuitarStrings =
            open Domain
            open Notes

            type private GuitarStringAttributes = {Name:string; OpenStringNote:Note; Index:int}

            let private guitarStringAttributes = function
                | SixthString -> { Name="Sixth"; OpenStringNote=E; Index=6}
                | FifthString -> { Name="Fifth"; OpenStringNote=A; Index=5}
                | FourthString -> { Name="Fourth"; OpenStringNote=D; Index=4}
                | ThirdString -> { Name="Third"; OpenStringNote=G; Index=3}
                | SecondString -> { Name="Second"; OpenStringNote=B; Index=2}
                | FirstString -> { Name="First"; OpenStringNote=E; Index=1}

            let guitarStringOrdinal guitarString =
                (guitarStringAttributes guitarString).Index

            let indexToGuitarString (nth:int) =
                match nth with
                | 6 -> SixthString
                | 5 -> FifthString
                | 4 -> FourthString
                | 3 -> ThirdString
                | 2 -> SecondString
                | _ -> FirstString
            
            let fretForMutedString = -1

            let openStringNote guitarString =
                (guitarStringAttributes guitarString).OpenStringNote

            let nextString guitarString =
                indexToGuitarString ((guitarStringOrdinal guitarString) - 1)

            let fretNoteOnString note guitarString =
                measureAbsoluteSemitones (openStringNote guitarString) note

            let createMutedStringFret guitarString =
                { GuitarString = guitarString; Fret = fretForMutedString; Note = openStringNote guitarString }

            let createStringFret guitarString note =
                { createMutedStringFret guitarString with Fret = fretNoteOnString note guitarString; Note = note }  

        open Domain
        open Notes
        open Chords
        open Infrastructure 

        [<AutoOpen>]
        module private MapDropChords =
            let private unmapedChordNotes chordNotes mutedString guitarString =
                match mutedString guitarString with
                | true -> chordNotes
                | false -> chordNotes |> List.tail

            let private mapNoteToFret guitarString note mutedString =
                match mutedString guitarString with
                | true -> createMutedStringFret guitarString
                | false -> createStringFret guitarString note

            let private skipString bassString chord guitarString =
                chord.ChordType = Drop3 && guitarString = nextString bassString

            let private mapChordToGuitarFrets bassString chord =
                let shouldSkipString = skipString bassString chord
                let rec mapChordNoteToString guitarString chordNotes mappedChordNotes =
                    match chordNotes with
                    | [] -> mappedChordNotes
                    | _ ->
                        let fret = mapNoteToFret guitarString (fst chordNotes.[0]) shouldSkipString
                        let unmapedChordNotes = unmapedChordNotes chordNotes shouldSkipString guitarString
                        mapChordNoteToString (nextString guitarString) unmapedChordNotes (fret::mappedChordNotes)
                mapChordNoteToString bassString chord.Notes []

            let dropChordToGuitarChord bassString chord =
                let guitarChord = { Chord = chord; Frets = mapChordToGuitarFrets bassString chord |> List.rev }
                let closedChord = {guitarChord with Frets = raiseOpenFrets guitarChord.Frets}
                {closedChord with Frets = unstretch closedChord.Frets}

        [<AutoOpen>]
        module private MapNonDropChords =
            let private mapAllChordNotesToFretsOnString allowedFrets guitarStringIndex chord =
                let guitarString = indexToGuitarString guitarStringIndex
                [for chordNoteIndex in 0 .. (chord.Notes.Length - 1)
                    do yield (createStringFret guitarString (fst chord.Notes.[chordNoteIndex]))]
                |> List.filter allowedFrets

            let private generateAllFretCombinations allowedFrets bassString chord =
                let bassStringOrdinal = guitarStringOrdinal bassString
                [for guitarStringIndex in 1 .. bassStringOrdinal
                    do yield (mapAllChordNotesToFretsOnString allowedFrets guitarStringIndex chord)]
                |> combineAll

            let private fitFretingCombinations fretingCombinations =
                fretingCombinations
                |> List.map (fun fc -> (fc, fc |> List.sumBy (fun f -> f.Fret)))
                |> List.minBy (fun l -> (snd l))
                |> fst

            let private chordToGuitarChord allowedFrets bassString chord =
                {
                    Chord = chord;
                    Frets = chord
                            |> generateAllFretCombinations allowedFrets bassString
                            |> fitFretingCombinations
                            |> List.rev
                }

            let chordToGuitarOpenChord bassString chord =
                chordToGuitarChord (fun f -> f = f) bassString chord

            let chordToGuitarClosedChord bassString chord =
                chordToGuitarChord (fun f -> f.Fret <> 0) bassString chord

        let chordName guitarChord =
            guitarChord.Chord.Name

        let private stringForLead guitarChord =
            (guitarChord.Frets |> List.last).GuitarString

        let private stringForBass guitarChord =
            (guitarChord.Frets |> List.head).GuitarString

        let private isNinthChord chord =
            chord.Notes |> List.exists (fun n -> snd n = Ninth) 

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

        let createGuitarChord bassString chord =
            match chord.ChordType with
            | Drop2 | Drop3 | Triad -> dropChordToGuitarChord bassString chord
            | Open -> chordToGuitarOpenChord bassString chord
            | Closed when chord.Notes |> List.exists (fun n -> snd n = Ninth) -> dropChordToGuitarChord bassString chord
            | Closed -> chordToGuitarClosedChord bassString chord

        let (|~) chord bassString =
            createGuitarChord bassString chord

        let (>|<) (chords:GuitarChord list) (chord:GuitarChord) =
            chord :: chords |> rotateByOne

    module GuitarTab =
        open System
        open Domain
        open Notes
        open Chords
        open Guitar

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

        let tabifyAll guitarChords =
            (tabifyChordNames guitarChords) @ (tabifyChords guitarChords)
            |> List.fold (+) ""

        let tabify guitarChord =
            tabifyAll [guitarChord]

        let shapify guitarChord =
            guitarChord.Chord.Name + Environment.NewLine +
            "EADGBE" + Environment.NewLine +
            (guitarChord |> shapifyChord |> List.fold (+) "") + Environment.NewLine