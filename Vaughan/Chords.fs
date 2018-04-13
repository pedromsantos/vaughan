namespace Vaughan

    module Chords =
        open Notes
        open Infrastructure

        type private ChordPattern = Interval list
        type private ChordAttributes = {Name:string; Quality:ChordQuality; Pattern:ChordPattern}

        let private chordFormula =
            [
                {Name="Maj"; Quality=Major; Pattern=[MajorThird; PerfectFifth]}
                {Name="Aug"; Quality=Augmented; Pattern=[MajorThird; AugmentedFifth]}
                {Name="Min"; Quality=Minor; Pattern=[MinorThird; PerfectFifth]}
                {Name="Dim"; Quality=Diminished; Pattern=[MinorThird; DiminishedFifth]}
                {Name="Maj7"; Quality=Major7; Pattern=[MajorThird; PerfectFifth; MajorSeventh]}
                {Name="Maj9"; Quality=Major9; Pattern=[MajorThird; PerfectFifth; MajorSeventh; MajorNinth]}
                {Name="Maj9(#11)"; Quality=Major9Sharp11; Pattern=[MajorThird; PerfectFifth; MajorSeventh; MajorNinth; AugmentedEleventh]}
                {Name="Maj11"; Quality=Major11; Pattern=[MajorThird; PerfectFifth; MajorSeventh; PerfectEleventh]}
                {Name="Maj13"; Quality=Major13; Pattern=[MajorThird; PerfectFifth; MajorSeventh; MajorThirteenth]}
                {Name="Maj13(#11)"; Quality=Major13Sharp11; Pattern=[MajorThird; PerfectFifth; MajorSeventh; MajorThirteenth; AugmentedEleventh]}
                {Name="6"; Quality=Major6; Pattern=[MajorThird; PerfectFifth; MajorSixth]}
                {Name="6add9"; Quality=Major6Add9; Pattern=[MajorThird; PerfectFifth; MajorSixth; MajorNinth]}
                {Name="6(b5)add9"; Quality=Major6Flat5Add9; Pattern=[MajorThird; DiminishedFifth; MajorSixth; MajorNinth]}
                {Name="Aug7"; Quality=Augmented7; Pattern=[MajorThird; AugmentedFifth; MajorSeventh]}
                {Name="Min7"; Quality=Minor7; Pattern=[MinorThird; PerfectFifth; MinorSeventh]}
                {Name="Min9"; Quality=Minor9; Pattern=[MinorThird; PerfectFifth; MinorSeventh; MajorNinth]}
                {Name="Min6"; Quality=Minor6; Pattern=[MinorThird; PerfectFifth; MajorSixth]}
                {Name="Min6Add9"; Quality=Minor6Add9; Pattern=[MinorThird; PerfectFifth; MajorSixth; MajorNinth]}
                {Name="Min7b5"; Quality=Minor7b5; Pattern=[MinorThird; DiminishedFifth; MinorSeventh]}
                {Name="MinMaj7"; Quality=MinorMaj7; Pattern=[MinorThird; PerfectFifth; MajorSeventh]}
                {Name="MinMaj9"; Quality=MinorMaj9; Pattern=[MinorThird; PerfectFifth; MajorSeventh; MajorNinth]}
                {Name="Min7(b9)"; Quality=MinorMaj9; Pattern=[MinorThird; PerfectFifth; MinorSeventh; MinorNinth]}
                {Name="Min7(b5b9)"; Quality=MinorMaj9; Pattern=[MinorThird; DiminishedFifth; MinorSeventh; MinorNinth]}
                {Name="Dim7"; Quality=Diminished7; Pattern=[MinorThird; DiminishedFifth; DiminishedSeventh]}
                {Name="Dim7"; Quality=Diminished7; Pattern=[MinorThird; DiminishedFifth; MajorSixth]}
                {Name="7"; Quality=Dominant7; Pattern=[MajorThird; PerfectFifth; MinorSeventh]}
                {Name="7(b5)"; Quality=Dominant7Flat5; Pattern=[MajorThird; DiminishedFifth; MinorSeventh]}
                {Name="7(b9)"; Quality=Dominant7Flat9; Pattern=[MajorThird; PerfectFifth; MinorSeventh; MinorNinth]}
                {Name="7(#9)"; Quality=Dominant7Sharp9; Pattern=[MajorThird; PerfectFifth; MinorSeventh; AugmentedNinth]}
                {Name="7(b5b9)"; Quality=Dominant7Flat5Flat9; Pattern=[MajorThird; DiminishedFifth; MinorSeventh; MinorNinth]}
                {Name="7(b5#9)"; Quality=Dominant7Flat5Sharp9; Pattern=[MajorThird; DiminishedFifth; MinorSeventh; AugmentedNinth]}
                {Name="9"; Quality=Dominant9; Pattern=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth]}
                {Name="11"; Quality=Dominant11; Pattern=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth; PerfectEleventh]}
                {Name="13"; Quality=Dominant13; Pattern=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth; PerfectEleventh; MajorThirteenth]}
                {Name="Sus2"; Quality=Sus2; Pattern=[MajorSecond; PerfectFifth]}
                {Name="Sus2Dim"; Quality=Sus2Diminished; Pattern=[MajorSecond; DiminishedFifth]}
                {Name="Sus2Aug"; Quality=Sus2Augmented; Pattern=[MajorSecond; AugmentedFifth]}
                {Name="Sus4"; Quality=Sus4; Pattern=[PerfectFourth; PerfectFifth]}
                {Name="Sus4Dim"; Quality=Sus4Diminished; Pattern=[PerfectFourth; DiminishedFifth]}
                {Name="Sus4Aug"; Quality=Sus4Augmented; Pattern=[PerfectFourth; AugmentedFifth]}
            ]

        let private qualityForPattern pattern =
            (chordFormula
             |> List.filter (fun c -> c.Pattern = pattern)
             |> List.head).Quality

        let private findQualityForPattern pattern =
            match (chordFormula |> List.filter (fun c -> c.Pattern = pattern)) with
            | [] -> None
            | l -> Some (l |> List.head).Quality

        let private chordPatternFromNotes (notes :Note list) =
            let root = notes.Head
            notes
            |> List.map (fun n -> intervalBetween root n)
            |> List.skip 1

        let private findFittingQuality (fittingNotes :Note list) =
            fittingNotes
            |> chordPatternFromNotes
            |> findQualityForPattern

        let private intervalsForQuality quality =
             (chordFormula
            |> List.filter (fun c -> c.Quality = quality)
            |> List.head).Pattern

        let private nameForQuality quality =
            (chordFormula
            |> List.filter (fun c -> c.Quality = quality)
            |> List.head).Name

        let private functionForInterval = function
            | Unisson -> Root
            | MajorThird | MinorThird | MajorSecond | MinorSecond | PerfectFourth | AugmentedFourth -> Third
            | PerfectFifth | DiminishedFifth | AugmentedFifth  -> Fifth
            | MajorSixth -> Sixth
            | MajorSeventh | MinorSeventh | DiminishedSeventh -> Seventh
            | MajorNinth | MinorNinth | AugmentedNinth -> Ninth
            | PerfectEleventh | AugmentedEleventh -> Eleventh
            | MajorThirteenth -> Thirteenth
            | _ -> Root

        let private note chordNote =
            fst chordNote

        let private noteFunction chordNote =
            snd chordNote

        let private noteForFunction (chord:Chord) (chordNoteFunction:ChordNoteFunction) =
            note (chord.Notes |> List.find (fun n -> noteFunction n = chordNoteFunction))

        let private adjustIntervalForFunctionsAboveSeventh interval noteFunction =
            match noteFunction with
            | Ninth | Eleventh | Thirteenth -> fromDistance ((toDistance interval) + (toDistance PerfectOctave))
            | _ -> interval

        let private intervalsForChord chord =
            let root = noteForFunction chord Root
            chord.Notes
            |> List.map (fun n -> adjustIntervalForFunctionsAboveSeventh (intervalBetween root (note n)) (noteFunction n))
            |> List.skip 1

        let private invertOpenOrClosed (chord:Chord) =
            {chord with Notes = rotateByOne chord.Notes;}

        let private invertDrop2 (chord:Chord) =
            {
                chord with Notes = [chord.Notes |> List.last]
                                   @
                                   (chord.Notes
                                    |> List.take (chord.Notes.Length - 1)
                                    |> rotateByOne
                                    |> rotateByOne)
            }

        let private invertDrop3 (chord:Chord) =
            {chord with Notes = chord.Notes |> rotateByOne |> rotateByOne |> swapSecondTwo;}

        let name:ChordName = fun chord ->
            noteName (noteForFunction chord Root)
            + nameForQuality (qualityForPattern(intervalsForChord chord))

        let invert:Invert = fun chord ->
            match chord.ChordType with
            | Closed | Open | Triad -> invertOpenOrClosed chord
            | Drop2 -> invertDrop2 chord
            | Drop3 -> invertDrop3 chord

        let root:Root = fun chord ->
            noteForFunction chord Root

        let third chord =
            noteForFunction chord Third

        let seventh chord =
            noteForFunction chord Seventh

        let bass:Bass = fun chord ->
            note (chord.Notes |> List.head)

        let lead:Lead = fun chord ->
            note (chord.Notes |> List.last)

        let noteNames:NoteNames = fun chord ->
            chord.Notes |> List.map (note >> noteName)

        let notesMidiNumbers = fun (chord:Chord) octave ->
            chord.Notes |> List.map (fun n -> midiNumber (fst n) octave)

        let chord:CreateChord = fun root quality ->
            {
                Notes= [(root, Root)] @ (intervalsForQuality quality |> List.map (fun i -> ((transpose root i), functionForInterval i)));
                ChordType = Closed
                Name =  noteName root + nameForQuality (qualityForPattern(intervalsForQuality quality))
            }

        let add chords chord =
            chord :: chords |> rotateByOne

        let toDrop2:ToDrop2 = fun chord ->
            if chord.Notes.Length = 4
            then {chord with Notes = chord.Notes |> swapFirstTwo |> rotateByOne; ChordType = Drop2}
            else chord

        let toDrop3:ToDrop2 = fun chord ->
            if chord.Notes.Length = 4
            then {chord with Notes= (chord |> toDrop2 |> toDrop2).Notes; ChordType = Drop3}
            else chord

        let toTriad:ToTriad = fun chord ->
            if chord.Notes.Length = 3 then {chord with ChordType=Triad}
            else chord

        let toOpen:ToOpen = fun chord ->
            {chord with ChordType=Open}

        let (=>) root quality =
            chord root quality

        let (/./) chords chord =
            add chords chord

        let ( !* ) chord =
            toOpen chord

        let toClosed:ToClosed = fun chord ->
            {chord with ChordType=Closed}

        let skipFunction:SkipChordFunction = fun functionToSkipp chord ->
            {chord with Notes = chord.Notes |> List.filter (fun nf -> snd nf <> functionToSkipp)}

        let private addFittingChord notes fittingChords =
            match findFittingQuality notes with
            | Some(q) -> chord notes.Head q :: fittingChords
            | None-> fittingChords

        let chordsFitting:ChordsFitting = fun notes ->
            if notes.Length < 3 then []
            else let rec addChord rotation fittingChords (fittingNotes :Note list) =
                    if rotation = fittingNotes.Length then fittingChords
                    else
                       let fittedChords = addFittingChord fittingNotes fittingChords
                       addChord (rotation + 1) fittedChords (fittingNotes |> rotateByOne)

                 addChord 0 [] notes