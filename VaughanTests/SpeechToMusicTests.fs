namespace VaughanTests
    module SpeechToMusicTests =
            open Xunit
            open FsUnit
            open FsUnit.Xunit
            open FsCheck
            open FsCheck.Xunit
            open System
            open Vaughan.Domain
            open Vaughan.Scales
            open Vaughan.SpeechToMusic
            open Vaughan.Chords
            open Vaughan.Guitar
            open Vaughan.GuitarTab
            open Vaughan.ScaleHarmonizer

            [<Fact>]
            let ``Should parse textual representation of notes``() =
                (parseNote "C") |> should equal C
                (parseNote "C#") |> should equal CSharp
                (parseNote "Db") |> should equal DFlat
                (parseNote "D") |> should equal D
                (parseNote "D#") |> should equal DSharp
                (parseNote "Eb") |> should equal EFlat
                (parseNote "E") |> should equal E
                (parseNote "F") |> should equal F
                (parseNote "F#") |> should equal FSharp
                (parseNote "Gb") |> should equal GFlat
                (parseNote "G") |> should equal G
                (parseNote "G#") |> should equal GSharp
                (parseNote "Ab") |> should equal AFlat
                (parseNote "A") |> should equal A
                (parseNote "A#") |> should equal ASharp
                (parseNote "Bb") |> should equal BFlat
                (parseNote "B") |> should equal B

            [<Fact>]
            let ``Should parse textual representation of scales``() =
                (parseScale "C Ionian") |> should equal (createScale Ionian C)
                (parseScale "C# MajorPentatonic") |> should equal (createScale MajorPentatonic CSharp)
                (parseScale "Db MinorPentatonic") |> should equal (createScale MinorPentatonic DFlat)
                (parseScale "D Dorian") |> should equal (createScale Dorian D)
                (parseScale "D# Blues") |> should equal (createScale Blues DSharp)
                (parseScale "Eb HarmonicMinor") |> should equal (createScale HarmonicMinor EFlat)
                (parseScale "E Phrygian") |> should equal (createScale Phrygian E)
                (parseScale "F Lydian") |> should equal (createScale Lydian F)
                (parseScale "F# MelodicMinor") |> should equal (createScale MelodicMinor FSharp)
                (parseScale "Gb Dorianb2") |> should equal (createScale Dorianb2 GFlat)
                (parseScale "G Mixolydian") |> should equal (createScale Mixolydian G)
                (parseScale "G# LydianAugmented") |> should equal (createScale LydianAugmented GSharp)
                (parseScale "Ab NeapolitanMinor") |> should equal (createScale NeapolitanMinor AFlat)
                (parseScale "A Aolian") |> should equal (createScale Aolian A)
                (parseScale "A# LydianDominant") |> should equal (createScale LydianDominant ASharp)
                (parseScale "Bb Bebop") |> should equal (createScale Bebop BFlat)
                (parseScale "B Locrian") |> should equal (createScale Locrian B)
                
            [<Fact>]
            let ``Should parse textual representation of intervals``() =
                (parseInterval "Unisson") |> should equal Unisson
                (parseInterval "MinorSecond") |> should equal MinorSecond
                (parseInterval "MajorSecond") |> should equal MajorSecond
                (parseInterval "AugmentedSecond") |> should equal AugmentedSecond
                (parseInterval "MinorThird") |> should equal MinorThird
                (parseInterval "MajorThird") |> should equal MajorThird
                (parseInterval "PerfectFourth") |> should equal PerfectFourth
                (parseInterval "AugmentedFourth") |> should equal AugmentedFourth
                (parseInterval "DiminishedFifth") |> should equal DiminishedFifth 
                (parseInterval "PerfectFifth") |> should equal PerfectFifth
                (parseInterval "AugmentedFifth") |> should equal AugmentedFifth
                (parseInterval "MinorSixth") |> should equal MinorSixth
                (parseInterval "MajorSixth") |> should equal MajorSixth
                (parseInterval "DiminishedSeventh") |> should equal DiminishedSeventh
                (parseInterval "MinorSeventh") |> should equal MinorSeventh
                (parseInterval "MajorSeventh") |> should equal MajorSeventh
                (parseInterval "PerfectOctave") |> should equal PerfectOctave
                (parseInterval "MajorNinth") |> should equal MajorNinth
                (parseInterval "MajorNinth") |> should equal MajorNinth
                (parseInterval "AugmentedNinth") |> should equal AugmentedNinth
                (parseInterval "PerfectEleventh") |> should equal PerfectEleventh
                (parseInterval "AugmentedEleventh") |> should equal AugmentedEleventh
                (parseInterval "MinorThirteenth") |> should equal MinorThirteenth
                (parseInterval "MajorThirteenth") |> should equal MajorThirteenth

            [<Fact>]
            let ``Should parse textual representation of triads``() =
                (parseChord "C Major") |> should equal { Root=C; Quality=Major }
                (parseChord "C# Maj") |> should equal { Root=CSharp; Quality=Major }
                (parseChord "C minor") |> should equal { Root=C; Quality=Minor }
                (parseChord "Db min") |> should equal { Root=DFlat; Quality=Minor }
                (parseChord "Cmin") |> should equal { Root=C; Quality=Minor }
                (parseChord "Cm") |> should equal { Root=C; Quality=Minor }
                (parseChord "C augmented") |> should equal { Root=C; Quality=Augmented }
                (parseChord "C Aug") |> should equal { Root=C; Quality=Augmented }
                (parseChord "C diminished") |> should equal { Root=C; Quality=Diminished }
                (parseChord "C dim") |> should equal { Root=C; Quality=Diminished }

            [<Fact>]
            let ``Should parse textual representation of seventh chords``() =
                (parseChord "C Major 7") |> should equal { Root=C; Quality=Major7 }
                (parseChord "C Maj 7") |> should equal { Root=C; Quality=Major7 }
                (parseChord "CMaj7") |> should equal { Root=C; Quality=Major7 }
                (parseChord "C minor 7") |> should equal { Root=C; Quality=Minor7 }
                (parseChord "C min 7") |> should equal { Root=C; Quality=Minor7 }
                (parseChord "Cmin7") |> should equal { Root=C; Quality=Minor7 }
                (parseChord "C augmented 7") |> should equal { Root=C; Quality=Augmented7 }
                (parseChord "C aug 7") |> should equal { Root=C; Quality=Augmented7 }
                (parseChord "C diminished 7") |> should equal { Root=C; Quality=Diminished7 }
                (parseChord "C dim 7") |> should equal { Root=C; Quality=Diminished7 }
                (parseChord "Caug7") |> should equal { Root=C; Quality=Augmented7 }
                (parseChord "Cdom7") |> should equal { Root=C; Quality=Dominant7 }
                (parseChord "C#7") |> should equal { Root=CSharp; Quality=Dominant7 }

            [<Fact>]
            let ``Should create chord from chord intent``() =
                let cIonian = createScaleNotes Ionian C
                let cMaj = triadsHarmonizer ScaleDegree.I cIonian
                let dMin = triadsHarmonizer ScaleDegree.II cIonian
                let eMin = triadsHarmonizer ScaleDegree.III cIonian
                let fMaj = triadsHarmonizer ScaleDegree.IV cIonian
                let gMaj = triadsHarmonizer ScaleDegree.V cIonian
                let aMin = triadsHarmonizer ScaleDegree.VI cIonian
                let bDim = triadsHarmonizer ScaleDegree.VII cIonian

                createChord { Root=C; Quality=Major } |> should equal (cMaj |> toClosed)
                createChord { Root=D; Quality=Minor } |> should equal (dMin |> toClosed)
                createChord { Root=E; Quality=Minor } |> should equal (eMin |> toClosed)
                createChord { Root=F; Quality=Major } |> should equal (fMaj |> toClosed)
                createChord { Root=G; Quality=Major } |> should equal (gMaj |> toClosed)
                createChord { Root=A; Quality=Minor } |> should equal (aMin |> toClosed)
                createChord { Root=B; Quality=Diminished } |> should equal (bDim |> toClosed)

            [<Fact>]
            let ``Should tabify chord from text``() =
                "A Major"
                |> parseChord
                |> createChord
                |> toTriad
                |> createGuitarChord SixthString
                |> tabify |> should equal ("      AMaj   " + Environment.NewLine+
                            "E||------------||" + Environment.NewLine +
                            "B||------------||" + Environment.NewLine +
                            "G||------------||" + Environment.NewLine +
                            "D||----2-------||" + Environment.NewLine +
                            "A||----4-------||" + Environment.NewLine +
                            "E||----5-------||" + Environment.NewLine)