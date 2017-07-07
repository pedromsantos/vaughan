namespace VaughanTests    
    module NoteTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open FsUnit
        open Vaughan.Domain
        open Vaughan.Notes

        [<Test>]
        let ``Should relate note with its name``() =
            noteName C |> should equal "C"
            noteName CSharp |> should equal "C#"
            noteName DFlat |> should equal "Db"
            noteName D |> should equal "D"
            noteName DSharp |> should equal "D#"
            noteName EFlat |> should equal "Eb"
            noteName E |> should equal "E"
            noteName F |> should equal "F"
            noteName FSharp |> should equal "F#"
            noteName GFlat |> should equal "Gb"
            noteName G |> should equal "G"
            noteName GSharp |> should equal "G#"
            noteName AFlat |> should equal "Ab"
            noteName A |> should equal "A"
            noteName ASharp |> should equal "A#"
            noteName BFlat |> should equal "Bb"
            noteName B |> should equal "B"

        [<Test>]
        let ``Should sharp note``() =
            sharp C |> should equal CSharp
            sharp CSharp |> should equal D
            sharp DFlat |> should equal D
            sharp D |> should equal DSharp
            sharp DSharp |> should equal E
            sharp EFlat |> should equal E
            sharp E |> should equal F
            sharp F |> should equal FSharp
            sharp FSharp |> should equal G
            sharp GFlat |> should equal G
            sharp G |> should equal GSharp
            sharp GSharp |> should equal A
            sharp AFlat |> should equal A
            sharp A |> should equal ASharp
            sharp ASharp |> should equal B
            sharp BFlat |> should equal B
            sharp B |> should equal C

        [<Test>]
        let ``Should flat note``() =
            flat C |> should equal B
            flat CSharp |> should equal C
            flat DFlat |> should equal C
            flat D |> should equal DFlat
            flat DSharp |> should equal D
            flat EFlat |> should equal D
            flat E |> should equal EFlat
            flat F |> should equal E
            flat FSharp |> should equal F
            flat GFlat |> should equal F
            flat G |> should equal GFlat
            flat GSharp |> should equal G
            flat AFlat |> should equal G
            flat A |> should equal AFlat
            flat ASharp |> should equal A
            flat BFlat |> should equal A
            flat B |> should equal BFlat

        [<Property>]
        let ``Sharping and flating a note should go back to original note pitch`` (note :Note)  =
            pitch (note |> sharp |> flat) = (pitch note)

        [<Test>]
        let ``Should measure semitones distance``() =
            measureAbsoluteSemitones C C |> should equal 0
            measureAbsoluteSemitones C CSharp |> should equal 1
            measureAbsoluteSemitones C DFlat |> should equal 1
            measureAbsoluteSemitones C D |> should equal 2
            measureAbsoluteSemitones C DSharp |> should equal 3
            measureAbsoluteSemitones C EFlat |> should equal 3
            measureAbsoluteSemitones C E |> should equal 4
            measureAbsoluteSemitones C F |> should equal 5
            measureAbsoluteSemitones C FSharp |> should equal 6
            measureAbsoluteSemitones C GFlat |> should equal 6
            measureAbsoluteSemitones C G |> should equal 7
            measureAbsoluteSemitones C GSharp |> should equal 8
            measureAbsoluteSemitones C AFlat |> should equal 8
            measureAbsoluteSemitones C A |> should equal 9
            measureAbsoluteSemitones C ASharp |> should equal 10
            measureAbsoluteSemitones C BFlat |> should equal 10
            measureAbsoluteSemitones C B |> should equal 11

        [<Test>]
        let ``Should create interval from distance``() =
            intervalBetween C C |> should equal Unisson
            intervalBetween C CSharp |> should equal MinorSecond
            intervalBetween C DFlat |> should equal MinorSecond
            intervalBetween C D |> should equal MajorSecond
            intervalBetween C DSharp |> should equal MinorThird
            intervalBetween C EFlat |> should equal MinorThird
            intervalBetween C E |> should equal MajorThird
            intervalBetween C F |> should equal PerfectFourth
            intervalBetween C FSharp |> should equal DiminishedFifth
            intervalBetween C GFlat |> should equal DiminishedFifth
            intervalBetween C G |> should equal PerfectFifth
            intervalBetween C GSharp |> should equal AugmentedFifth
            intervalBetween C AFlat |> should equal AugmentedFifth
            intervalBetween C A |> should equal MajorSixth
            intervalBetween C ASharp |> should equal MinorSeventh
            intervalBetween C BFlat |> should equal MinorSeventh
            intervalBetween C B |> should equal MajorSeventh

        [<Property>]
        let ``Interval between same note is a unisson interval`` (note :Note) =
            (intervalBetween note note) = Unisson

        [<Property>]
        let ``Semitone difference between two notes is a minor second interval`` (note :Note) =
            (intervalBetween note (sharp note)) = MinorSecond
            &&
            (intervalBetween (note |> flat) note) = MinorSecond

        [<Property>]
        let ``Two semi tones difference between two notes is a major second interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp)) = MajorSecond
            &&
            (intervalBetween (note |> flat |> flat) note) = MajorSecond

        [<Property>]
        let ``Three semi tones difference between two notes is a minor third interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp |> sharp)) = MinorThird
            &&
            (intervalBetween (note |> flat |> flat |> flat) note) = MinorThird

        [<Property>]
        let ``Four semi tones difference between two notes is a major third interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp |> sharp |> sharp)) = MajorThird
            &&
            (intervalBetween (note |> flat |> flat |> flat |> flat) note) = MajorThird

        [<Property>]
        let ``Five semi tones difference between two notes is a perfect fourth interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp |> sharp |> sharp |> sharp)) = PerfectFourth
            &&
            (intervalBetween (note |> flat |> flat |> flat |> flat |> flat) note) = PerfectFourth

        [<Property>]
        let ``Six semi tones difference between two notes is a diminished fifth interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp)) = DiminishedFifth
            &&
            (intervalBetween (note |> flat |> flat |> flat |> flat |> flat |> flat) note) = DiminishedFifth

        [<Property>]
        let ``Seven semi tones difference between two notes is a perfect fifth interval`` (note :Note) =
            (intervalBetween note (note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp)) = PerfectFifth
            &&
            (intervalBetween (note |> flat |> flat |> flat |> flat |> flat |> flat |> flat) note) = PerfectFifth

        [<Test>]
        let ``Should transpose note using interval``() =
            transpose C Unisson |> should equal C
            transpose C MinorSecond |> should equal DFlat
            transpose C MajorSecond |> should equal D
            transpose C MinorThird |> should equal EFlat
            transpose C MajorThird |> should equal E
            transpose C PerfectFourth |> should equal F
            transpose C DiminishedFifth |> should equal GFlat
            transpose C PerfectFifth |> should equal G
            transpose C AugmentedFifth |> should equal GSharp
            transpose C MajorSixth |> should equal A
            transpose C MinorSeventh |> should equal BFlat
            transpose C MajorSeventh |> should equal B

        [<Property>]
        let ``Transposing a note by a unisson gives the same the note`` (note :Note) =
            (transpose note Unisson) = note

        [<Property>]
        let ``Transposing a note by an augmented fifth gives a note 8 semi tones above`` (note :Note) =
            let sharpedNote = note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp
            let flatedNote = note |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat
            pitch (transpose note AugmentedFifth) = pitch sharpedNote
            &&
            pitch (transpose flatedNote AugmentedFifth) = pitch note

        [<Property>]
        let ``Transposing a note by an major sixth gives a note 9 semi tones above`` (note :Note) =
            let sharpedNote = note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp
            let flatedNote = note |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat
            pitch (transpose note MajorSixth) = pitch sharpedNote
            &&
            pitch (transpose flatedNote MajorSixth) = pitch note

        [<Property>]
        let ``Transposing a note by an minor seventh gives a note 10 semi tones above`` (note :Note) =
            let sharpedNote = note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp
            let flatedNote = note |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat
            pitch (transpose note MinorSeventh) = pitch sharpedNote
            &&
            pitch (transpose flatedNote MinorSeventh) = pitch note

        [<Property>]
        let ``Transposing a note by an major seventh gives a note 11 semi tones above`` (note :Note) =
            let sharpedNote = note |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp |> sharp
            let flatedNote = note |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat |> flat
            pitch (transpose note MajorSeventh) = pitch sharpedNote
            &&
            pitch (transpose flatedNote MajorSeventh) = pitch note

        [<Test>]
        let ``Should relate interval with its name``() =
            intervalName Unisson |> should equal "Unisson"
            intervalName MinorSecond |> should equal "MinorSecond"
            intervalName MajorSecond |> should equal "MajorSecond"
            intervalName MinorThird |> should equal "MinorThird"
            intervalName MajorThird |> should equal "MajorThird"
            intervalName PerfectFourth |> should equal "PerfectForth"
            intervalName DiminishedFifth |> should equal "DiminishedFifth"
            intervalName PerfectFifth |> should equal "PerfectFifth"
            intervalName AugmentedFifth |> should equal "AugmentedFifth"
            intervalName MajorSixth |> should equal "MajorSixth"
            intervalName MinorSeventh |> should equal "MinorSeventh"
            intervalName MajorSeventh |> should equal "MajorSeventh"
            intervalName PerfectOctave |> should equal "PerfectOctave"

        [<Test>]
        let ``Should relate interval with distances``() =
            fromDistance 0 |> should equal Unisson
            fromDistance 1 |> should equal MinorSecond
            fromDistance 2 |> should equal MajorSecond
            fromDistance 3 |> should equal MinorThird
            fromDistance 4 |> should equal MajorThird
            fromDistance 5 |> should equal PerfectFourth
            fromDistance 6 |> should equal DiminishedFifth
            fromDistance 7 |> should equal PerfectFifth
            fromDistance 8 |> should equal AugmentedFifth
            fromDistance 9 |> should equal MajorSixth
            fromDistance 10 |> should equal MinorSeventh
            fromDistance 11 |> should equal MajorSeventh
            fromDistance 12 |> should equal PerfectOctave