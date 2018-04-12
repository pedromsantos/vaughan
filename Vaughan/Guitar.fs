namespace Vaughan

    module Guitar =
        [<AutoOpen>]
        module private GuitarFrets =
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

            let raiseOctave (fret:Fret) =
                {fret with Fret = fret.Fret + int(toDistance PerfectOctave)}

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
                { createMutedStringFret guitarString with Fret = int(fretNoteOnString note guitarString); Note = note }

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
            let private mapAllChordNotesToFretsOnString allowedFrets guitarStringIndex (chord:Chord) =
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
            
        [<AutoOpen>]
        module private MapMelodicLines =
            let private filterAllowedFrets allowedFrets (frets: Fret list) =
                frets |> List.map (fun f -> if allowedFrets f then f else raiseOctave f)
                |> List.filter allowedFrets 

            let private mapAllNotesToFretsOnString allowedFrets guitarStringIndex (notes:Note list) =
                let guitarString = indexToGuitarString guitarStringIndex
                let frets = 
                    [for noteIndex in 0 .. (notes.Length - 1)
                        do yield (createStringFret guitarString notes.[noteIndex])]
                    |> filterAllowedFrets allowedFrets
                match frets with
                | [] -> [createMutedStringFret guitarString]
                | _ -> frets
            
            let private generateAllFretCombinations allowedFrets (notes:Note list) =
                [for guitarStringIndex in 1 .. 6 
                    do yield (mapAllNotesToFretsOnString allowedFrets guitarStringIndex notes)]
                |> List.collect id 
                            
            let chordToGuitarArpeggio allowedFrets chord =
                {
                    BaseChord = chord;
                    ArpeggioFrets = generateAllFretCombinations allowedFrets (chord.Notes |> List.map fst)
                }

            let scaleToGuitarScale allowedFrets scale =
                {
                    Scale = scale;
                    Frets = generateAllFretCombinations allowedFrets (scale.Notes)
                }

            let notesGuitarNotes allowedFrets (notes:Note list) =
                notes |> generateAllFretCombinations allowedFrets
            
        let createGuitarChord:CreateGuitarChord = fun bassString chord ->
            match chord.ChordType with
            | Drop2 | Drop3 | Triad -> dropChordToGuitarChord bassString chord
            | Open -> chordToGuitarOpenChord bassString chord
            | Closed when chord.Notes |> List.exists (fun n -> snd n = Ninth) -> dropChordToGuitarChord bassString chord
            | Closed -> chordToGuitarClosedChord bassString chord
        
        open Chords
        
        let createGuitarArpeggio:CreateGuitarArpeggio = fun minFret maxFret chord ->
            chord
            |> toClosed  
            |> chordToGuitarArpeggio (fun f -> f.Fret >= minFret && f.Fret <= maxFret)

        let createGuitarScale:CreateGuitarScale = fun minFret maxFret scale ->
            scale
            |> scaleToGuitarScale (fun f -> f.Fret >= minFret && f.Fret <= maxFret)

        let createGuitarNotes minFret maxFret notes =
            notes
            |> notesGuitarNotes (fun f -> f.Fret >= minFret && f.Fret <= maxFret)

    module GuitarTab =
        open System
        open Guitar
        open Infrastructure

        type private TabColumn = string list

        type private TabColumns = TabColumn list

        type private TabLine = string list

        type private TabLines = TabLine list

        let private standardTunningTab = ["e"; "B"; "G"; "D"; "A"; "E"]

        let private startTab = "||-" |> List.replicate 6

        let private emptyTab = "---" |> List.replicate 6

        let private barTab = "|" |> List.replicate 6

        let private endTab =  ("-||" + Environment.NewLine) |> List.replicate 6

        [<AutoOpen>]
        module private Tabify =
            let private stringForLead frets =
                (frets |> List.last).GuitarString

            let private stringForBass frets =
                (frets |> List.head).GuitarString

            let numberOfMutedHighStrings frets =
                match stringForLead frets with
                    | SecondString -> 1
                    | ThirdString -> 2
                    | FourthString -> 3
                    | FifthString -> 4
                    | SixthString -> 5
                    | _ -> 0

            let numberOfMutedLowStrings frets =
                match stringForBass frets with
                | FifthString -> 1
                | FourthString  -> 2
                | ThirdString  -> 3
                | SecondString  -> 4
                | FirstString  -> 5
                | _ -> 0

            let private renderMutedHigherStrings (mutedStringTab:string) (frets: Fret list) =
                let mutedStrings = numberOfMutedHighStrings frets
                List.replicate mutedStrings mutedStringTab

            let private renderMutedLowerStrings (mutedStringTab:string) (frets: Fret list) =
                let mutedStrings = numberOfMutedLowStrings frets
                List.replicate mutedStrings mutedStringTab

            let private renderChordNote (mutedStringTab:string) (fret:Fret) =
                if fret.Fret = -1 then
                    mutedStringTab
                else
                    sprintf "-%i-" fret.Fret

            let private renderChordNotes mutedStringTab frets =
                frets
                |> List.map (fun fret -> renderChordNote mutedStringTab fret)
                |> List.rev

            let renderNote (fret:Fret) =
                let mutedStringTab = sprintf "-%s-" (if fret.Fret > 9 then "--" else "-")
                (renderMutedHigherStrings mutedStringTab [fret])
                @ ([sprintf "-%i-" fret.Fret])
                @ (renderMutedLowerStrings mutedStringTab [fret])

            let renderChord (chord:GuitarChord) =
                let highestFet = chord.Frets |> List.map (fun f -> f.Fret) |> List.max
                let mutedStringTab = sprintf "-%s-" (if highestFet > 9 then "--" else "-")
                (renderMutedHigherStrings mutedStringTab chord.Frets)
                @ (renderChordNotes mutedStringTab chord.Frets)
                @ (renderMutedLowerStrings mutedStringTab chord.Frets)

            let private mapTabColumsToTabLines stringOrdinal (tabifiedChords: TabColumns) =
                tabifiedChords |> List.map (fun f -> f.[stringOrdinal])

            let mapTabToGuitarStrings (tabifiedChords: TabColumns) =
                [0 .. 5]
                |> List.map (fun stringOrdinal -> mapTabColumsToTabLines stringOrdinal tabifiedChords)

            let private renderNotes (frets:Fret list) =
                frets
                |> List.sortByDescending (fun f -> f.GuitarString, f.Fret)
                |> List.map renderNote
                |> mapTabToGuitarStrings 
                |> List.map (fun gss -> gss |> List.fold (fun acc gs -> gs + acc) "") 

            let renderTabPart = function
                | Rest -> emptyTab
                | Bar -> barTab
                | Start -> startTab
                | End -> endTab
                | Note n -> renderNote n
                | Chord c -> renderChord c
                | Arpeggio a -> renderNotes a.ArpeggioFrets
                | Scale s -> renderNotes s.Frets
                | Mute m -> ["-x-"]
                | PalmMute pm -> ["-_-"]
                | Harmonic h -> ["-*-"]
                | Vibrato v -> ["-~-"]
                | HammerOn (fs, fe) -> ["-h-"]
                | PullOff (fs, fe) -> ["-p-"]
                | Bend (fs, fe) -> ["-b-"]
                | StandardTunning -> standardTunningTab

        [<AutoOpen>]
        module private Shapify =
            let private shapifyMutedLowerStrings frets =
                let mutedStrings = numberOfMutedLowStrings frets
                List.replicate mutedStrings "X"

            let private shapifyMutedHigherStrings frets =
                let mutedStrings = numberOfMutedHighStrings frets
                List.replicate mutedStrings "X"

            let private shapifyFret fret =
                if fret.Fret = -1 then
                    "X"
                else
                    sprintf "%i" fret.Fret

            let private shapifyFrets frets =
                frets |> List.map shapifyFret

            let shapifyChord (guitarChord:GuitarChord) =
                (shapifyMutedLowerStrings guitarChord.Frets)
                @ (shapifyFrets guitarChord.Frets)
                @ (shapifyMutedHigherStrings guitarChord.Frets)

        let renderTab tab = 
            tab 
            |> List.map renderTabPart
            |> List.rev
            |> mapTabToGuitarStrings
            |> List.map (fun gss -> gss |> List.fold (fun acc gs -> gs + acc) "")
            |> List.fold (+) ""

        let tabifyAll:TabifyAll = fun guitarChords ->
            [StandardTunning; Start] @ (guitarChords |> List.map (fun c -> Chord(c))) @ [End] 
            |> renderTab

        let tabifyChord:TabifyChord = fun guitarChord ->
            tabifyAll [guitarChord]
        
        let tabifyArpeggio:TabifyArpeggio = fun guitarArpeggio ->
            [StandardTunning; Start] @ ([Arpeggio(guitarArpeggio)]) @ [End] 
            |> renderTab

        let tabifyScale:TabifyScale = fun guitarScale ->
           [StandardTunning; Start] @ ([Scale(guitarScale)]) @ [End] |> renderTab

        let shapify:Shapify = fun guitarChord ->
            guitarChord.Chord.Name + Environment.NewLine +
            "EADGBE" + Environment.NewLine +
            (guitarChord |> shapifyChord |> List.fold (+) "") + Environment.NewLine

        let (|~) chord bassString =
            createGuitarChord bassString chord

        let (>|<) (chords:GuitarChord list) (chord:GuitarChord) =
            chord :: chords |> rotateByOne