namespace VaughanTests
    module ScaleTests =
        open Xunit
        open FsUnit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Scales
        open Vaughan.Chords

        [<Fact>]
        let ``Should have notes for scales``() =
            createScaleNotes Ionian C |> should equal [ C; D; E; F; G; A; B ]
            createScaleNotes Dorian C |> should equal [ C; D; EFlat; F; G; A; BFlat ]
            createScaleNotes Phrygian C |> should equal [ C; DFlat; EFlat; F; G; AFlat; BFlat ]
            createScaleNotes Lydian C |> should equal [ C; D; E; FSharp; G; A; B ]
            createScaleNotes Mixolydian C |> should equal [ C; D; E; F; G; A; BFlat ]
            createScaleNotes Aolian C |> should equal [ C; D; EFlat; F; G; AFlat; BFlat ]
            createScaleNotes Locrian C |> should equal [ C; DFlat; EFlat; F; GFlat; AFlat; BFlat ]
            createScaleNotes MajorPentatonic C |> should equal [ C; D; E; G; A;]
            createScaleNotes MinorPentatonic C |> should equal [ C; EFlat; F; G; BFlat ]
            createScaleNotes Blues C |> should equal [ C; EFlat; F; GFlat; G; BFlat ]
            createScaleNotes HarmonicMinor C |> should equal [ C; D; EFlat; F; G; AFlat; B ]
            createScaleNotes MelodicMinor C |> should equal [ C; D; EFlat; F; G; A; B ]
            createScaleNotes Dorianb2 C |> should equal [ C; DFlat; EFlat; F; G; A; BFlat ]
            createScaleNotes NeapolitanMinor C |> should equal [ C; DFlat; EFlat; F; G; AFlat; B ]
            createScaleNotes LydianAugmented C |> should equal [ C; D; E; FSharp; GSharp; A; B ]
            createScaleNotes LydianDominant C |> should equal [ C; D; E; FSharp; G; A; BFlat ]
            createScaleNotes Mixolydianb6 C |> should equal [ C; D; E; F; G; AFlat; BFlat ]
            createScaleNotes LocrianSharp2 C |> should equal [ C; D; EFlat; F; GFlat; AFlat; BFlat ]
            createScaleNotes AlteredDominant C |> should equal [ C; DFlat; DSharp; E; GFlat; GSharp; BFlat ]
            createScaleNotes HalfWholeDiminished C |> should equal [ C; DFlat; EFlat; E; FSharp; G; A; BFlat ]
            createScaleNotes WholeTone C |> should equal [ C; D; E; GFlat; GSharp; BFlat ]

        [<Property>]
        let ``It should return scales fitting a major triad`` (root :Note) =
            let chord = chord root ChordQuality.Major
            let chordNotes = chord.Notes |> List.map fst |> List.sort

            let scales = scalesFitting chord

            scales |> List.forall (
                fun s -> s.Notes |> List.filter (fun x -> (List.contains x chordNotes)) |> List.sort = chordNotes)

        [<Property>]
        let ``It should return scales fitting a minor triad`` (root :Note) =
            let chord = chord root ChordQuality.Minor
            let chordNotes = chord.Notes |> List.map fst |> List.sort

            let scales = scalesFitting chord

            scales |> List.forall (
                fun s -> s.Notes |> List.filter (fun x -> (List.contains x chordNotes)) |> List.sort = chordNotes)

        [<Property>]
        let ``It should return scales fitting a chord`` (root :Note) (quality: ChordQuality)=
            let chord = chord root quality
            let chordNotes = chord.Notes |> List.map fst |> List.sort

            let scales = scalesFitting chord

            scales |> List.forall (
                fun s -> s.Notes |> List.filter (fun x -> (List.contains x chordNotes)) |> List.sort = chordNotes)