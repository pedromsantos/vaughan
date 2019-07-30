namespace VaughanTests

module SpeechToMusicTests =
    open Xunit
    open FsUnit.Xunit
    open System
    open Vaughan.Domain
    open Vaughan.Scales
    open Vaughan.SpeechToMusic
    open Vaughan.Chords
    open Vaughan.Guitar
    open Vaughan.GuitarTab

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
    let ``Should parse textual representation of midi notes``() =
        (parseMidiNote "C0") |> should equal (C, SubContra)
        (parseMidiNote "C#1") |> should equal (CSharp, Contra)
        (parseMidiNote "Db2") |> should equal (DFlat, Great)
        (parseMidiNote "D3") |> should equal (D, Small)
        (parseMidiNote "D#4") |> should equal (DSharp, OneLine)
        (parseMidiNote "Eb5") |> should equal (EFlat, TwoLine)
        (parseMidiNote "E6") |> should equal (E, ThreeLine)
        (parseMidiNote "F7") |> should equal (F, FourLine)
        (parseMidiNote "F#8") |> should equal (FSharp, FiveLine)
        (parseMidiNote "Gb9") |> should equal (GFlat, SixLine)
        (parseMidiNote "G10") |> should equal (G, SevenLine)
        (parseMidiNote "G#4") |> should equal (GSharp, OneLine)
        (parseMidiNote "Ab5") |> should equal (AFlat, TwoLine)
        (parseMidiNote "A6") |> should equal (A, ThreeLine)
        (parseMidiNote "A#3") |> should equal (ASharp, Small)
        (parseMidiNote "Bb4") |> should equal (BFlat, OneLine)
        (parseMidiNote "B5") |> should equal (B, TwoLine)

    [<Fact>]
    let ``Should parse textual representation of scales``() =
        (parseScale "C Ionian") |> should equal (createScale Ionian C)
        (parseScale "C# MajorPentatonic")
        |> should equal (createScale MajorPentatonic CSharp)
        (parseScale "Db MinorPentatonic")
        |> should equal (createScale MinorPentatonic DFlat)
        (parseScale "D Dorian") |> should equal (createScale Dorian D)
        (parseScale "D# Blues") |> should equal (createScale Blues DSharp)
        (parseScale "Eb HarmonicMinor")
        |> should equal (createScale HarmonicMinor EFlat)
        (parseScale "E Phrygian") |> should equal (createScale Phrygian E)
        (parseScale "F Lydian") |> should equal (createScale Lydian F)
        (parseScale "F# MelodicMinor")
        |> should equal (createScale MelodicMinor FSharp)
        (parseScale "Gb Dorianb2") |> should equal (createScale Dorianb2 GFlat)
        (parseScale "G Mixolydian") |> should equal (createScale Mixolydian G)
        (parseScale "G# LydianAugmented")
        |> should equal (createScale LydianAugmented GSharp)
        (parseScale "Ab NeapolitanMinor")
        |> should equal (createScale NeapolitanMinor AFlat)
        (parseScale "A Aolian") |> should equal (createScale Aolian A)
        (parseScale "A# LydianDominant")
        |> should equal (createScale LydianDominant ASharp)
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
        (parseInterval "MinorNinth") |> should equal MinorNinth
        (parseInterval "MajorNinth") |> should equal MajorNinth
        (parseInterval "AugmentedNinth") |> should equal AugmentedNinth
        (parseInterval "PerfectEleventh") |> should equal PerfectEleventh
        (parseInterval "AugmentedEleventh") |> should equal AugmentedEleventh
        (parseInterval "MinorThirteenth") |> should equal MinorThirteenth
        (parseInterval "MajorThirteenth") |> should equal MajorThirteenth

    [<Fact>]
    let ``Should parse textual representation of octaves``() =
        (parseOctave "SubContra") |> should equal SubContra
        (parseOctave "Contra") |> should equal Contra
        (parseOctave "Great") |> should equal Great
        (parseOctave "Small") |> should equal Small
        (parseOctave "OneLine") |> should equal OneLine
        (parseOctave "TwoLine") |> should equal TwoLine
        (parseOctave "ThreeLine") |> should equal ThreeLine
        (parseOctave "FourLine") |> should equal FourLine
        (parseOctave "FiveLine") |> should equal FiveLine
        (parseOctave "SixLine") |> should equal SixLine
        (parseOctave "SevenLine") |> should equal SevenLine

    [<Fact>]
    let ``Should parse textual representation of triads``() =
        (parseChord "C Major") |> should equal (chord C Major)
        (parseChord "C# Maj") |> should equal (chord CSharp Major)
        (parseChord "C minor") |> should equal (chord C Minor)
        (parseChord "Db min") |> should equal (chord DFlat Minor)
        (parseChord "Cmin") |> should equal (chord C Minor)
        (parseChord "Cm") |> should equal (chord C Minor)
        (parseChord "C augmented") |> should equal (chord C Augmented)
        (parseChord "C Aug") |> should equal (chord C Augmented)
        (parseChord "C diminished") |> should equal (chord C Diminished)
        (parseChord "C dim") |> should equal (chord C Diminished)

    [<Fact>]
    let ``Should parse textual representation of seventh chords``() =
        (parseChord "C Major 7") |> should equal (chord C Major7)
        (parseChord "C Maj 7") |> should equal (chord C Major7)
        (parseChord "CMaj7") |> should equal (chord C Major7)
        (parseChord "C minor 7") |> should equal (chord C Minor7)
        (parseChord "C minor 7") |> should equal (chord C Minor7)
        (parseChord "Cminor7") |> should equal (chord C Minor7)
        (parseChord "C min 7") |> should equal (chord C Minor7)
        (parseChord "Cmin7") |> should equal (chord C Minor7)
        (parseChord "C augmented 7") |> should equal (chord C Augmented7)
        (parseChord "C aug 7") |> should equal (chord C Augmented7)
        (parseChord "C diminished 7") |> should equal (chord C Diminished7)
        (parseChord "C dim 7") |> should equal (chord C Diminished7)
        (parseChord "Caug7") |> should equal (chord C Augmented7)
        (parseChord "Cdom7") |> should equal (chord C Dominant7)
        (parseChord "C#7") |> should equal (chord CSharp Dominant7)

    [<Fact>]
    let ``Should parse textual representation of chords with structure``() =
        let cMajor7 = chord C Major7
        (parseChord "C Major 7 closed") |> should equal (cMajor7 |> toClosed)
        (parseChord "C Major 7 drop2") |> should equal (cMajor7 |> toDrop2)
        (parseChord "C Major 7 drop 2") |> should equal (cMajor7 |> toDrop2)
        (parseChord "C Major 7 drop3") |> should equal (cMajor7 |> toDrop3)
        (parseChord "C Major 7 drop 3") |> should equal (cMajor7 |> toDrop3)

    [<Fact>]
    let ``Should tabify chord from text``() =
        "A Major"
        |> parseChord
        |> toTriad
        |> guitarChord SixthString
        |> tabifyChord
        |> should equal
               ("e||-----||" + Environment.NewLine + "B||-----||"
                + Environment.NewLine + "G||-----||" + Environment.NewLine
                + "D||--2--||" + Environment.NewLine + "A||--4--||"
                + Environment.NewLine + "E||--5--||" + Environment.NewLine)

    [<Fact>]
    let ``Should tabify arpeggios from chord names``() =
        [ "Cm7"; "F7"; "CMaj7" ]
        |> tabifyArpeggiosFromChordNames 2 5
        |> should not' (be Empty)
