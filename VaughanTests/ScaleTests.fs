namespace VaughanTests

module ScaleTests =
    open Xunit
    open FsUnit.Xunit
    open FsCheck.Xunit
    open Vaughan.Domain
    open Vaughan.Notes
    open Vaughan.Scales
    open Vaughan.Chords

    [<Fact>]
    let ``Should have notes for scales``() =
        createScaleNotes Ionian C |> should equal [ C; D; E; F; G; A; B ]
        createScaleNotes Dorian C
        |> should equal [ C; D; EFlat; F; G; A; BFlat ]
        createScaleNotes Phrygian C
        |> should equal [ C; DFlat; EFlat; F; G; AFlat; BFlat ]
        createScaleNotes Lydian C |> should equal [ C; D; E; FSharp; G; A; B ]
        createScaleNotes Mixolydian C
        |> should equal [ C; D; E; F; G; A; BFlat ]
        createScaleNotes Aolian C
        |> should equal [ C; D; EFlat; F; G; AFlat; BFlat ]
        createScaleNotes Locrian C
        |> should equal [ C; DFlat; EFlat; F; GFlat; AFlat; BFlat ]
        createScaleNotes MajorPentatonic C |> should equal [ C; D; E; G; A ]
        createScaleNotes MinorPentatonic C
        |> should equal [ C; EFlat; F; G; BFlat ]
        createScaleNotes Blues C
        |> should equal [ C; EFlat; F; GFlat; G; BFlat ]
        createScaleNotes HarmonicMinor C
        |> should equal [ C; D; EFlat; F; G; AFlat; B ]
        createScaleNotes MelodicMinor C
        |> should equal [ C; D; EFlat; F; G; A; B ]
        createScaleNotes Dorianb2 C
        |> should equal [ C; DFlat; EFlat; F; G; A; BFlat ]
        createScaleNotes NeapolitanMinor C
        |> should equal [ C; DFlat; EFlat; F; G; AFlat; B ]
        createScaleNotes LydianAugmented C
        |> should equal [ C; D; E; FSharp; GSharp; A; B ]
        createScaleNotes LydianDominant C
        |> should equal [ C; D; E; FSharp; G; A; BFlat ]
        createScaleNotes Mixolydianb6 C
        |> should equal [ C; D; E; F; G; AFlat; BFlat ]
        createScaleNotes LocrianSharp2 C
        |> should equal [ C; D; EFlat; F; GFlat; AFlat; BFlat ]
        createScaleNotes AlteredDominant C
        |> should equal [ C; DFlat; DSharp; E; GFlat; GSharp; BFlat ]
        createScaleNotes HalfWholeDiminished C
        |> should equal [ C; DFlat; EFlat; E; FSharp; G; A; BFlat ]
        createScaleNotes WholeTone C
        |> should equal [ C; D; E; GFlat; GSharp; BFlat ]

    [<Property>]
    let ``It should return scales fitting a major triad`` (root : Note) =
        let chord = chord root ChordQuality.Major

        let chordNotes =
            chord.Notes
            |> List.map fst
            |> List.sort

        let scales = scalesFitting chord
        scales
        |> List.forall (fun s ->
               s.Notes
               |> List.filter (fun x -> (List.contains x chordNotes))
               |> List.sort = chordNotes)

    [<Property>]
    let ``It should return scales fitting a minor triad`` (root : Note) =
        let chord = chord root ChordQuality.Minor

        let chordNotes =
            chord.Notes
            |> List.map fst
            |> List.sort

        let scales = scalesFitting chord
        scales
        |> List.forall (fun s ->
               s.Notes
               |> List.filter (fun x -> (List.contains x chordNotes))
               |> List.sort = chordNotes)

    [<Property>]
    let ``It should return scales fitting a chord`` (root : Note)
        (quality : ChordQuality) =
        let chord = chord root quality

        let chordNotes =
            chord.Notes
            |> List.map fst
            |> List.sort

        let scales = scalesFitting chord
        scales
        |> List.forall (fun s ->
               s.Notes
               |> List.filter (fun x -> (List.contains x chordNotes))
               |> List.sort = chordNotes)

    [<Fact>]
    let ``Should create thirds melodic line from C major scale``() =
        let melodicLine =
            { Scale = createScale Ionian C
              Pattern =
                  [ ScaleDegree(ScaleDegree.I)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.II)
                    ScaleDegree(ScaleDegree.IV)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.IV)
                    ScaleDegree(ScaleDegree.VI)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII) ] }

        let melodicLineNotes = toNotes melodicLine
        melodicLineNotes |> should equal [ C; E; D; F; E; G; F; A; G; B ]

    [<Fact>]
    let ``Should create triads melodic line from C major scale``() =
        let melodicLine =
            { Scale = createScale Ionian C
              Pattern =
                  [ ScaleDegree(ScaleDegree.I)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.II)
                    ScaleDegree(ScaleDegree.IV)
                    ScaleDegree(ScaleDegree.VI)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII) ] }

        let melodicLineNotes = toNotes melodicLine
        melodicLineNotes |> should equal [ C; E; G; D; F; A; E; G; B ]

    [<Fact>]
    let ``Should create melodic line from C major scale``() =
        let melodicLine =
            { Scale = createScale Ionian C
              Pattern =
                  [ ScaleDegree(ScaleDegree.I)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII)
                    ScaleDegree(ScaleDegree.II)
                    ScaleDegree(ScaleDegree.IV)
                    ScaleDegree(ScaleDegree.VI)
                    ScaleDegree(ScaleDegree.I)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII)
                    ScaleDegree(ScaleDegree.II) ] }

        let melodicLineNotes = toNotes melodicLine
        melodicLineNotes |> should equal [ C; E; G; B; D; F; A; C; E; G; B; D ]

    [<Fact>]
    let ``Should create melodic line from C major scale with passing tones``() =
        let melodicLine =
            { Scale = createScale Ionian C
              Pattern =
                  [ ScaleDegree(ScaleDegree.I)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII)
                    NonScaleDegree(ScaleDegree.II, flat)
                    ScaleDegree(ScaleDegree.IV)
                    ScaleDegree(ScaleDegree.VI)
                    NonScaleDegree(ScaleDegree.I, sharp)
                    ScaleDegree(ScaleDegree.III)
                    ScaleDegree(ScaleDegree.V)
                    ScaleDegree(ScaleDegree.VII)
                    ScaleDegree(ScaleDegree.II) ] }

        let melodicLineNotes = toNotes melodicLine
        melodicLineNotes
        |> should equal [ C; E; G; B; DFlat; F; A; CSharp; E; G; B; D ]

    [<Fact>]
    let ``Should create melodic lines from C major scale using half step rules for root``() =
        let melodicLines = halfStepsMajorScale C ScaleDegree.I
        toNotes melodicLines.[0] |> should equal [ C; B; A; AFlat; G; F; E; D ]
        toNotes melodicLines.[1]
        |> should equal [ C; B; A; AFlat; G; F; E; EFlat; D; DFlat ]

    [<Fact>]
    let ``Should create melodic lines from C major scale using half step rules for second``() =
        let melodicLines = halfStepsMajorScale C ScaleDegree.II
        toNotes melodicLines.[0] |> should equal [ D; C; B; A; G; F; E ]
        let melodicLine = toNotes melodicLines.[1]
        melodicLine |> should equal [ D; DFlat; C; B; A; AFlat; G; F; E ]

    [<Fact>]
    let ``Should create melodic lines from C dominant scale using half step rules for root``() =
        let melodicLines = halfStepsDominantScale C ScaleDegree.I
        toNotes melodicLines.[0] |> should equal [ C; B; BFlat; A; G; F; E; D ]
        toNotes melodicLines.[1]
        |> should equal [ C; B; BFlat; A; G; F; E; EFlat; D; DFlat ]

    [<Fact>]
    let ``Should create melodic lines from C dominant scale using half step rules for second``() =
        let melodicLines = halfStepsDominantScale C ScaleDegree.II
        toNotes melodicLines.[0] |> should equal [ D; C; BFlat; A; G; F; E ]
        let melodicLine = toNotes melodicLines.[1]
        melodicLine |> should equal [ D; DFlat; C; B; BFlat; A; G; F; E ]
