namespace Vaughan

    module SpeechToMusic =
        open FParsec
        open Domain
        open Notes
        open Chords

        type private UserState = unit
        type private Parser<'t> = Parser<'t, UserState>

        let private skip parser skiped = parser .>> skiped

        let private skipSpaces parser = skip parser spaces

        let private any parsers = parsers |> List.reduce (<|>)

        let private parseNote: Parser<_> =
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

        let private parseMajorQuality: Parser<_> =
            any [
                    (stringCIReturn "major" Major)
                    (stringCIReturn "maj" Major)
                    (stringReturn "M" Major)
                ] |> skipSpaces

        let private parseMinorQuality: Parser<_> =
            any [
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

        let private parseQuality: Parser<_> =
            any [
                    parseMajorQuality
                    parseMinorQuality
                    parseAugmentedQuality
                    parseDiminishedQuality
                    parseDominantQuality
                ] |> skipSpaces

        let private updateChordIntentWithSeventhQuality chord =
            match chord with
            | {Quality=Major} -> { chord with Quality = Major7 }
            | {Quality=Minor} -> { chord with Quality = Minor7 }
            | {Quality=Diminished} -> { chord with Quality = Diminished7 }
            | {Quality=Augmented} -> { chord with Quality = Augmented7 }
            | {Quality=_} -> { chord with Quality = Dominant7 }

        let private parseSeventhQuality: Parser<_> =
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

        let private rootParser: Parser<_> =
            pipe2 parseNote parseAccident
                (fun note applyAccidentToNote -> applyAccidentToNote note)

        let private triadParser: Parser<_> =
            pipe2 rootParser parseQuality
                (fun r q -> {Root=r; Quality=q;})

        let private seventhChordParser: Parser<_> =
            pipe2 triadParser parseSeventhQuality
                (fun triad seventhQualityUpdater -> seventhQualityUpdater triad)

        let private chordParser: Parser<_> =
            any [
                    seventhChordParser
                    triadParser;
                ]

        let parseChord:IParseChord = fun text ->
            let parsed = run chordParser text
            match parsed with
            | Success(chordDefinition, _, _) -> chordDefinition
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let createChord:ICreateChordFromIntent = fun chordIntent ->
            chord chordIntent.Root chordIntent.Quality