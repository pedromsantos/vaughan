namespace VaughanTests    
    module NoteTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Notes

        [<Test>]
        let ``Should relate note with its name``() =
            noteName C =! "C"
            noteName CSharp =! "C#"
            noteName DFlat =! "Db"
            noteName D =! "D"
            noteName DSharp =! "D#"
            noteName EFlat =! "Eb"
            noteName E =! "E"
            noteName F =! "F"
            noteName FSharp =! "F#"
            noteName GFlat =! "Gb"
            noteName G =! "G"
            noteName GSharp =! "G#"
            noteName AFlat =! "Ab"
            noteName A =! "A"
            noteName ASharp =! "A#"
            noteName BFlat =! "Bb"
            noteName B =! "B"

        [<Test>]
        let ``Should sharp note``() =
            sharp C =! CSharp
            sharp CSharp =! D
            sharp DFlat =! D
            sharp D =! DSharp
            sharp DSharp =! E
            sharp EFlat =! E
            sharp E =! F
            sharp F =! FSharp
            sharp FSharp =! G
            sharp GFlat =! G
            sharp G =! GSharp
            sharp GSharp =! A
            sharp AFlat =! A
            sharp A =! ASharp
            sharp ASharp =! B
            sharp BFlat =! B
            sharp B =! C

        [<Test>]
        let ``Should flat note``() =
            flat C =! B
            flat CSharp =! C
            flat DFlat =! C
            flat D =! DFlat
            flat DSharp =! D
            flat EFlat =! D
            flat E =! EFlat
            flat F =! E
            flat FSharp =! F
            flat GFlat =! F
            flat G =! GFlat
            flat GSharp =! G
            flat AFlat =! G
            flat A =! AFlat
            flat ASharp =! A
            flat BFlat =! A
            flat B =! BFlat

        [<Property>]
        let ``Sharping and flating a note should go back to original note pitch`` (note :Note)  =
            pitch (note |> sharp |> flat) = (pitch note)

        [<Test>]
        let ``Should measure semitones distance``() =
            measureAbsoluteSemitones C C =! 0
            measureAbsoluteSemitones C CSharp =! 1
            measureAbsoluteSemitones C DFlat =! 1
            measureAbsoluteSemitones C D =! 2
            measureAbsoluteSemitones C DSharp =! 3
            measureAbsoluteSemitones C EFlat =! 3
            measureAbsoluteSemitones C E =! 4
            measureAbsoluteSemitones C F =! 5
            measureAbsoluteSemitones C FSharp =! 6
            measureAbsoluteSemitones C GFlat =! 6
            measureAbsoluteSemitones C G =! 7
            measureAbsoluteSemitones C GSharp =! 8
            measureAbsoluteSemitones C AFlat =! 8
            measureAbsoluteSemitones C A =! 9
            measureAbsoluteSemitones C ASharp =! 10
            measureAbsoluteSemitones C BFlat =! 10
            measureAbsoluteSemitones C B =! 11

        [<Test>]
        let ``Should create interval from distance``() =
            intervalBetween C C =! Unisson
            intervalBetween C CSharp =! MinorSecond
            intervalBetween C DFlat =! MinorSecond
            intervalBetween C D =! MajorSecond
            intervalBetween C DSharp =! MinorThird
            intervalBetween C EFlat =! MinorThird
            intervalBetween C E =! MajorThird
            intervalBetween C F =! PerfectFourth
            intervalBetween C FSharp =! DiminishedFifth
            intervalBetween C GFlat =! DiminishedFifth
            intervalBetween C G =! PerfectFifth
            intervalBetween C GSharp =! AugmentedFifth
            intervalBetween C AFlat =! AugmentedFifth
            intervalBetween C A =! MajorSixth
            intervalBetween C ASharp =! MinorSeventh
            intervalBetween C BFlat =! MinorSeventh
            intervalBetween C B =! MajorSeventh

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
            transpose C Unisson =! C
            transpose C MinorSecond =! DFlat
            transpose C MajorSecond =! D
            transpose C MinorThird =! EFlat
            transpose C MajorThird =! E
            transpose C PerfectFourth =! F
            transpose C DiminishedFifth =! GFlat
            transpose C PerfectFifth =! G
            transpose C AugmentedFifth =! GSharp
            transpose C MajorSixth =! A
            transpose C MinorSeventh =! BFlat
            transpose C MajorSeventh =! B

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
            intervalName Unisson =! "Unisson"
            intervalName MinorSecond =! "MinorSecond"
            intervalName MajorSecond =! "MajorSecond"
            intervalName MinorThird =! "MinorThird"
            intervalName MajorThird =! "MajorThird"
            intervalName PerfectFourth =! "PerfectForth"
            intervalName DiminishedFifth =! "DiminishedFifth"
            intervalName PerfectFifth =! "PerfectFifth"
            intervalName AugmentedFifth =! "AugmentedFifth"
            intervalName MajorSixth =! "MajorSixth"
            intervalName MinorSeventh =! "MinorSeventh"
            intervalName MajorSeventh =! "MajorSeventh"
            intervalName PerfectOctave =! "PerfectOctave"

        [<Test>]
        let ``Should relate interval with distances``() =
            fromDistance 0 =! Unisson
            fromDistance 1 =! MinorSecond
            fromDistance 2 =! MajorSecond
            fromDistance 3 =! MinorThird
            fromDistance 4 =! MajorThird
            fromDistance 5 =! PerfectFourth
            fromDistance 6 =! DiminishedFifth
            fromDistance 7 =! PerfectFifth
            fromDistance 8 =! AugmentedFifth
            fromDistance 9 =! MajorSixth
            fromDistance 10 =! MinorSeventh
            fromDistance 11 =! MajorSeventh
            fromDistance 12 =! PerfectOctave