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

        let createGuitarMelodicLineFromArpeggio:CreateGuitarMelodicLineFromArpeggio = fun arpeggio ->
            arpeggio.ArpeggioFrets
            |> List.groupBy (fun f -> f.GuitarString)
            |> List.map (snd >> (fun f -> f |> List.sort |> List.map (fun f -> f.Fret)))

        let createGuitarMelodicLineFromScale:CreateGuitarMelodicLineFromScale = fun scale ->
            scale.Frets
            |> List.groupBy (fun f -> f.GuitarString)
            |> List.map (snd >> (fun f -> f |> List.sort |> List.map (fun f -> f.Fret))) 

        let createGuitarMelodicLineFromNotes:CreateGuitarMelodicLineFromNotes = fun frets ->
            frets 
            |> List.groupBy (fun f -> f.GuitarString)
            |> List.map (snd >> (fun f -> f |> List.sort |> List.map (fun f -> f.Fret))) 

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
                    | _ -> 0

            let numberOfMutedLowStrings frets =
                match stringForBass frets with
                | FifthString -> 1
                | FourthString  -> 2
                | ThirdString  -> 3
                | _ -> 0

            let private chordNameLength chord =
                (chord.Name).Length

            let private tabifyMutedString chord =
                String.replicate (chord |> chordNameLength) "-"

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

            let private tabifyFrets mutedStringTab frets =
                frets
                |> List.map (fun fret -> tabifyFret mutedStringTab fret)
                |> List.rev

            let private mapTabColumsToTabLines stringOrdinal (tabifiedChords: TabColumns) =
                tabifiedChords |> List.map (fun f -> f.[stringOrdinal])

            let private mapTabToGuitarStrings (tabifiedChords: TabColumns) =
                [0 .. 5]
                |> List.map (fun stringOrdinal -> mapTabColumsToTabLines stringOrdinal tabifiedChords)

            let private renderGuitarStringTab (tabifiedFretsForString:TabLine) =
                tabifiedFretsForString
                |> List.fold (fun acc fret -> acc + fret + "---" ) "---"

            let private renderGuitarStringsTab (tabifiedFretsForStrings:TabLines) =
                tabifiedFretsForStrings
                |> List.map renderGuitarStringTab

            let private tabifyChord (guitarChord:GuitarChord) =
                let mutedStringTab = tabifyMutedString guitarChord.Chord
                (tabifyMutedHigherStrings mutedStringTab guitarChord.Frets)
                @ (tabifyFrets mutedStringTab guitarChord.Frets)
                @ (tabifyMutedLowerStrings mutedStringTab guitarChord.Frets)

            let private notesOn melodicLine =                  
                melodicLine
                |> List.sumBy (fun f -> f |> List.fold (fun acc _ -> acc + 1 ) 0)

            let private melodicLinePartForGuitarString (guitarString:int) (melodicLine:GuitarMelodicLine) =
                melodicLine.[guitarString]

            let private renderMelodicLineForGuitarString melodicLine =
                melodicLine
                |> List.map (fun f -> sprintf "--%i" f) 
                |> List.reduce (+)

            let private renderTab tab =
                tab
                |> List.mapi (fun index tabifiedGuitarString ->
                                standardTunningTab.[index] +
                                startTab.[index] +
                                tabifiedGuitarString +
                                endTab.[index])

            let tabifyMelodicLine (melodicLine:GuitarMelodicLine) =
                let rec loop gs t (acc:string list) =
                    match gs with
                    | 6 -> acc |> List.rev
                    | _ ->
                            let guitarStringTab = melodicLinePartForGuitarString gs melodicLine |> renderMelodicLineForGuitarString
                            
                            let notesToTab = melodicLinePartForGuitarString gs melodicLine |> List.length
                            let leftPad = String.replicate (t - notesToTab) "---"
                            let rigtPads = (notesOn melodicLine) - t - notesToTab + 1
                            let rigtPad = String.replicate (if rigtPads < 0 then 0 else rigtPads) "---"
                            let paddedTab = sprintf "%s%s%s" leftPad guitarStringTab rigtPad
                            let equalizedTab = if acc.Length > 0 && paddedTab.Length > acc.Head.Length then paddedTab.Substring (0, (acc.Head.Length)) else paddedTab
                            let tab = if acc.Length > 0 && equalizedTab.Length < acc.Head.Length then equalizedTab + String.replicate (acc.Head.Length - equalizedTab.Length) "-" else equalizedTab

                            loop (gs + 1) (t - notesToTab) (tab::acc)

                loop 0 (notesOn melodicLine) []
                |> renderTab 
                            
            let tabifyChords guitarChords =
                guitarChords
                |> List.map tabifyChord
                |> mapTabToGuitarStrings
                |> renderGuitarStringsTab
                |> renderTab

            let tabifyChordNames guitarChords =
                let chordNameSeparator = "   "
                let separatedChordNames =
                    guitarChords |> List.map (fun guitarChord -> chordNameSeparator + guitarChord.Chord.Name)
                [chordNameSeparator] @ separatedChordNames @ [chordNameSeparator; Environment.NewLine;]

            let mapToTab (fret:Fret) =
                if fret.Fret = -1 then Mute(fret) else Note(fret)

            let mapScaleToTab (guitarScale:GuitarScale) =
                guitarScale.Frets
                |> List.map mapToTab

            let mapChordToTab (guitarChord:GuitarChord) =
                Chord(guitarChord)

            let mutedHighStrings fret =
                match fret.GuitarString with
                    | SecondString -> 1
                    | ThirdString -> 2
                    | FourthString -> 3
                    | _ -> 0

            let mutedLowStrings fret =
                match fret.GuitarString with
                | FifthString -> 1
                | FourthString  -> 2
                | ThirdString  -> 3
                | _ -> 0

            let renderNote (fret:Fret) =
                (List.replicate (mutedHighStrings fret) "---")
                @
                [sprintf "-%i-" fret.Fret] 
                @
                (List.replicate (mutedLowStrings fret) "---")

            let private renderMutedHigherStrings (mutedStringTab:string) (frets: Fret list) =
                let mutedStrings = numberOfMutedHighStrings frets
                List.replicate mutedStrings mutedStringTab

            let private renderMutedLowerStrings (mutedStringTab:string) (frets: Fret list) =
                let mutedStrings = numberOfMutedLowStrings frets
                List.replicate mutedStrings mutedStringTab

            let private renderFret (mutedStringTab:string) (fret:Fret) =
                if fret.Fret = -1 then
                    mutedStringTab
                else
                    sprintf "-%i-" fret.Fret

            let private renderFrets mutedStringTab frets =
                frets
                |> List.map (fun fret -> renderFret mutedStringTab fret)
                |> List.rev

            let renderChord (chord:GuitarChord) =
                let mutedStringTab = "---"
                (renderMutedHigherStrings mutedStringTab chord.Frets)
                @ (renderFrets mutedStringTab chord.Frets)
                @ (renderMutedLowerStrings mutedStringTab chord.Frets)

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

        let private mapTabColumsToTabLines stringOrdinal (tabifiedChords: TabColumns) =
                tabifiedChords |> List.map (fun f -> f.[stringOrdinal])

        let private mapTabToGuitarStrings (tabifiedChords: TabColumns) =
            [0 .. 5]
            |> List.map (fun stringOrdinal -> mapTabColumsToTabLines stringOrdinal tabifiedChords)

        let private renderTabPart = function
            | Rest -> emptyTab
            | Bar -> barTab
            | Start -> startTab
            | End -> endTab
            | Note n -> renderNote n
            | Chord c -> renderChord c
            | Mute m -> ["-x-"]
            | PalmMute pm -> ["-_-"]
            | Harmonic h -> ["-*-"]
            | Vibrato v -> ["-~-"]
            | HammerOn (fs, fe) -> ["-h-"]
            | PullOff (fs, fe) -> ["-p-"]
            | Bend (fs, fe) -> ["-b-"]
            | StandardTunning -> standardTunningTab 

        let tabifyAll:TabifyAll = fun guitarChords ->
            (tabifyChordNames guitarChords) @ (tabifyChords guitarChords)
            |> List.fold (+) ""

        let tabifyChord:TabifyChord = fun guitarChord ->
            tabifyAll [guitarChord]
        
        let tabifyArpeggio:TabifyArpeggio = fun guitarArpeggio ->
           guitarArpeggio
           |> createGuitarMelodicLineFromArpeggio
           |> tabifyMelodicLine 
           |> List.fold (+) ""

        let tabifyScale:TabifyScale = fun guitarScale ->
           guitarScale
           |> createGuitarMelodicLineFromScale
           |> tabifyMelodicLine 
           |> List.fold (+) ""

        let tabifyMelodicLine:TabifyMelodicLine = fun melodicLine ->
           melodicLine
           |> tabifyMelodicLine 
           |> List.fold (+) ""
                    
        let shapify:Shapify = fun guitarChord ->
            guitarChord.Chord.Name + Environment.NewLine +
            "EADGBE" + Environment.NewLine +
            (guitarChord |> shapifyChord |> List.fold (+) "") + Environment.NewLine

        let (|~) chord bassString =
            createGuitarChord bassString chord

        let (>|<) (chords:GuitarChord list) (chord:GuitarChord) =
            chord :: chords |> rotateByOne

        let renderTab tab = 
            tab 
            |> List.map renderTabPart
            |> mapTabToGuitarStrings
            |> List.map (fun gss -> gss |> List.fold (fun acc gs -> gs + acc) Environment.NewLine)
            |> List.fold (+) ""
