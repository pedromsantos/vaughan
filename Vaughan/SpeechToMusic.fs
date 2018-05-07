namespace Vaughan

    module SpeechToMusic =
        open FParsec
        open Notes
        open Scales
        open Chords
        open GuitarTab
        open ImprovisationGuitar

        type private ChordIntent = { Root: Note; Quality:ChordQuality; ToStructure: (Chord -> Chord) }

        type private UserState = unit
        type private Parser<'t> = Parser<'t, UserState>

        let private skip parser skiped = parser .>> skiped

        let private skipSpaces parser = skip parser spaces

        let private any parsers = parsers |> List.reduce (<|>)

        let private parseNoteName: Parser<_> =
            any [
                    (stringCIReturn "a" A);
                    (stringCIReturn "b" B);
                    (stringCIReturn "c" C);
                    (stringCIReturn "d" D);
                    (stringCIReturn "e" E);
                    (stringCIReturn "f" F);
                    (stringCIReturn "g" G)
                ] |> skipSpaces

        let private parseAccident: Parser<_> =
            any [
                    (stringReturn "#" sharp);
                    (stringReturn "b" flat);
                    (notFollowedByString "#" >>% natural);
                    (notFollowedByString "b" >>% natural)
                ] |> skipSpaces

        let private parseMidiOctave: Parser<_> =
                any [
                        (stringReturn "0" SubContra);
                        (stringReturn "10" SevenLine);
                        (stringReturn "1" Contra);
                        (stringReturn "2" Great);
                        (stringReturn "3" Small);
                        (stringReturn "4" OneLine);
                        (stringReturn "5" TwoLine);
                        (stringReturn "6" ThreeLine);
                        (stringReturn "7" FourLine);
                        (stringReturn "8" FiveLine);
                        (stringReturn "9" SixLine);
                ] |> skipSpaces

        let private parseScaleType: Parser<_> =
            any [
                    (stringCIReturn "Ionian" Ionian);
                    (stringCIReturn "Dorianb2" Dorianb2);
                    (stringCIReturn "Dorian" Dorian);
                    (stringCIReturn "Phrygian" Phrygian);
                    (stringCIReturn "LydianDominant" LydianDominant);
                    (stringCIReturn "LydianAugmented" LydianAugmented);
                    (stringCIReturn "Lydian" Lydian);
                    (stringCIReturn "Mixolydianb6" Mixolydianb6);
                    (stringCIReturn "Mixolydian" Mixolydian);
                    (stringCIReturn "Aolian" Aolian);
                    (stringCIReturn "LocrianSharp2" LocrianSharp2);
                    (stringCIReturn "Locrian" Locrian);
                    (stringCIReturn "MajorPentatonic" MajorPentatonic);
                    (stringCIReturn "MinorPentatonic" MinorPentatonic);
                    (stringCIReturn "Blues" Blues);
                    (stringCIReturn "HarmonicMinor" HarmonicMinor);
                    (stringCIReturn "MelodicMinor" MelodicMinor);
                    (stringCIReturn "NeapolitanMinor" NeapolitanMinor);
                    (stringCIReturn "Bebop" Bebop);
                    (stringCIReturn "AlteredDominant" AlteredDominant);
                    (stringCIReturn "HalfWholeDiminished" HalfWholeDiminished);
                    (stringCIReturn "WholeTone" WholeTone);
                    (stringCIReturn "MajorSixthDiminishedScale" MajorSixthDiminishedScale);
                    (stringCIReturn "MinorSixthDiminishedScale" MinorSixthDiminishedScale);
                    (stringCIReturn "DominantDiminishedScale" DominantDiminishedScale);
                    (stringCIReturn "Dominantb5DiminishedScale" Dominantb5DiminishedScale);
                ] |> skipSpaces

        let private intervalParser: Parser<_> =
            any [
                    (stringCIReturn "Unisson" Unisson);
                    (stringCIReturn "MinorSecond" MinorSecond);
                    (stringCIReturn "MajorSecond" MajorSecond);
                    (stringCIReturn "AugmentedSecond" AugmentedSecond);
                    (stringCIReturn "MinorThird" MinorThird);
                    (stringCIReturn "MajorThird" MajorThird);
                    (stringCIReturn "PerfectFourth" PerfectFourth);
                    (stringCIReturn "AugmentedFourth" AugmentedFourth);
                    (stringCIReturn "DiminishedFifth" DiminishedFifth);
                    (stringCIReturn "PerfectFifth" PerfectFifth);
                    (stringCIReturn "AugmentedFifth" AugmentedFifth);
                    (stringCIReturn "MinorSixth" MinorSixth);
                    (stringCIReturn "MajorSixth" MajorSixth);
                    (stringCIReturn "DiminishedSeventh" DiminishedSeventh);
                    (stringCIReturn "MinorSeventh" MinorSeventh);
                    (stringCIReturn "MajorSeventh" MajorSeventh);
                    (stringCIReturn "PerfectOctave" PerfectOctave);
                    (stringCIReturn "MinorNinth" MinorNinth);
                    (stringCIReturn "MajorNinth" MajorNinth);
                    (stringCIReturn "AugmentedNinth" AugmentedNinth);
                    (stringCIReturn "PerfectEleventh" PerfectEleventh);
                    (stringCIReturn "MinorThirteenth" MinorThirteenth);
                    (stringCIReturn "AugmentedEleventh" AugmentedEleventh);
                    (stringCIReturn "MajorThirteenth" MajorThirteenth)
                ] |> skipSpaces

        let private octaveParser: Parser<_> =
             any [
                     (stringCIReturn "SubContra" SubContra);
                     (stringCIReturn "Contra" Contra);
                     (stringCIReturn "Great" Great);
                     (stringCIReturn "Small" Small);
                     (stringCIReturn "OneLine" OneLine);
                     (stringCIReturn "TwoLine" TwoLine);
                     (stringCIReturn "ThreeLine" ThreeLine);
                     (stringCIReturn "FourLine" FourLine);
                     (stringCIReturn "FiveLine" FiveLine);
                     (stringCIReturn "SixLine" SixLine);
                     (stringCIReturn "SevenLine" SevenLine);
             ] |> skipSpaces

        let private parseMajorQuality: Parser<_> =
            any [
                    (stringCIReturn "major" Major)
                    (stringCIReturn "maj" Major)
                    (stringReturn "M" Major)
                ] |> skipSpaces

        let private parseMinorQuality: Parser<_> =
            any [
                    (stringCIReturn "Minor" Minor);
                    (stringCIReturn "minor" Minor);
                    (stringCIReturn "min" Minor);
                    (stringReturn "m" Minor);
                ] |> skipSpaces

        let private parseAugmentedQuality: Parser<_> =
             any [
                    (stringCIReturn "augmented" Augmented);
                    (stringCIReturn "aug" Augmented)
                 ] |> skipSpaces

        let private parseDiminishedQuality: Parser<_> =
            any [
                    (stringCIReturn "diminished" Diminished);
                    (stringCIReturn "dim" Diminished)
                    (stringCIReturn "Minor7b5" Minor7b5);
                    (stringCIReturn "minor7b5" Minor7b5);
                    (stringCIReturn "min7b5" Minor7b5);
                    (stringReturn "m7b5" Minor7b5);
                ] |> skipSpaces

        let private parseDominantQuality: Parser<_> =
            any [
                    (stringCIReturn "7" Dominant7);
                    (stringCIReturn "7th" Dominant7);
                    (stringCIReturn "seventh" Dominant7);
                    (stringCIReturn "seven" Dominant7);
                    (stringCIReturn "dominant" Dominant7);
                    (stringCIReturn "dom" Dominant7)
                ] |> skipSpaces

        let private qualityParser: Parser<_> =
            any [
                    parseAugmentedQuality
                    parseDiminishedQuality
                    parseMajorQuality
                    parseMinorQuality
                    parseDominantQuality
                ] |> skipSpaces

        let private chordStructureParser: Parser<_> =
            any [
                    (stringCIReturn "drop2" toDrop2);
                    (stringCIReturn "drop 2" toDrop2);
                    (stringCIReturn "drop3" toDrop3);
                    (stringCIReturn "drop 3" toDrop3);
                    (stringCIReturn "closed" toClosed);
                    (stringCIReturn "" toClosed);
                ] |> skipSpaces

        let private updateChordIntentWithSeventhQuality chord =
            match chord with
            | {Quality=Major} -> { chord with Quality = Major7 }
            | {Quality=Minor} -> { chord with Quality = Minor7 }
            | {Quality=Diminished} -> { chord with Quality = Diminished7 }
            | {Quality=Augmented} -> { chord with Quality = Augmented7 }
            | {Quality=_} -> { chord with Quality = Dominant7 }

        let private seventhQualityParser: Parser<_> =
            any [
                    (stringCIReturn "7" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "7th" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "seventh" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "seven" updateChordIntentWithSeventhQuality);
                    (notFollowedByString "7" >>% id);
                    (notFollowedByString "7th" >>% id);
                    (notFollowedByString "seventh" >>% id);
                    (notFollowedByString "seven" >>% id)
                ] |> skipSpaces

        let private noteParser: Parser<_> =
            pipe2 parseNoteName parseAccident
                (fun note applyAccidentToNote -> applyAccidentToNote note)

        let private midiNoteParser: Parser<_> =
            pipe3 parseNoteName parseAccident parseMidiOctave
                (fun note applyAccidentToNote octave -> (applyAccidentToNote note), octave)

        let private scaleParser: Parser<_> =
            pipe2 noteParser parseScaleType
                (fun r t -> createScale t r)

        let private triadParser: Parser<_> =
            pipe2 noteParser qualityParser
                (fun r q-> {Root=r; Quality=q; ToStructure = toClosed})

        let private seventhChordParser: Parser<_> =
            pipe3 triadParser seventhQualityParser chordStructureParser
                (fun triad seventhQualityUpdater structure->
                    {(triad |> seventhQualityUpdater) with ToStructure = structure})

        let private chordParser: Parser<_> =
            any [
                    seventhChordParser
                    triadParser;
                ]

        let private createChordFrom = fun chordIntent ->
            (chord chordIntent.Root chordIntent.Quality)
            |> chordIntent.ToStructure

        let parseNote:ParseNote = fun text ->
            let parsed = run noteParser text
            match parsed with
            | Success(note, _, _) -> note
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let parseMidiNote:ParseMidiNote = fun text ->
            let parsed = run midiNoteParser text
            match parsed with
            | Success(note, _, _) -> note
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let parseScale:ParseScale = fun text ->
            let parsed = run scaleParser text
            match parsed with
            | Success(scale, _, _) -> scale
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let parseInterval:ParseInterval = fun text ->
            let parsed = run intervalParser text
            match parsed with
            | Success(interval, _, _) -> interval
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let parseOctave:ParseOctave = fun text ->
            let parsed = run octaveParser text
            match parsed with
            | Success(octave, _, _) -> octave
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let parseChord:ParseChord = fun text ->
            let parsed = run chordParser text
            match parsed with
            | Success(chordDefinition, _, _) -> createChordFrom chordDefinition
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let tabifyArpeggiosFromChordNames (minFret:int) (maxFret:int) (chords:string list) =
            chords
            |> List.map parseChord
            |> createArpeggiosFromChords minFret maxFret
            |> List.map (fun a -> Arpeggio(a))
            |> renderTab