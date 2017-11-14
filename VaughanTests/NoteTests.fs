namespace VaughanTests
    module NoteTests =
        open Xunit
        open FsUnit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Notes

        [<Fact>]
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

        [<Fact>]
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

        [<Fact>]
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

        [<Fact>]
        let ``Should measure semitones distance``() =
            measureAbsoluteSemitones C C |> should equal 0<ht>
            measureAbsoluteSemitones C CSharp |> should equal 1<ht>
            measureAbsoluteSemitones C DFlat |> should equal 1<ht>
            measureAbsoluteSemitones C D |> should equal 2<ht>
            measureAbsoluteSemitones C DSharp |> should equal 3<ht>
            measureAbsoluteSemitones C EFlat |> should equal 3<ht>
            measureAbsoluteSemitones C E |> should equal 4<ht>
            measureAbsoluteSemitones C F |> should equal 5<ht>
            measureAbsoluteSemitones C FSharp |> should equal 6<ht>
            measureAbsoluteSemitones C GFlat |> should equal 6<ht>
            measureAbsoluteSemitones C G |> should equal 7<ht>
            measureAbsoluteSemitones C GSharp |> should equal 8<ht>
            measureAbsoluteSemitones C AFlat |> should equal 8<ht>
            measureAbsoluteSemitones C A |> should equal 9<ht>
            measureAbsoluteSemitones C ASharp |> should equal 10<ht>
            measureAbsoluteSemitones C BFlat |> should equal 10<ht>
            measureAbsoluteSemitones C B |> should equal 11<ht>

        [<Fact>]
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

        [<Fact>]
        let ``Should create beats from timeSignature and duration``() =
            (durationToBeats (1.0<beat>, Whole) Whole) |> should equal 1.0<beat>
            (durationToBeats (2.0<beat>, Half) Half) |> should equal 1.0<beat>

            (durationToBeats (4.0<beat>, Quarter) Whole) |> should equal 4.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Half) |> should equal 2.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Quarter) |> should equal 1.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Eigth) |> should equal 0.5<beat>
            (durationToBeats (4.0<beat>, Quarter) Sixteenth) |> should equal 0.25<beat>
            (durationToBeats (4.0<beat>, Quarter) ThirtySecond) |> should equal 0.125<beat>

            (durationToBeats (3.0<beat>, Quarter) Quarter) |> should equal 1.0<beat>

            (durationToBeats (12.0<beat>, Eigth) Eigth) |> should equal 1.0<beat>

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

        [<Fact>]
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

        [<Fact>]
        let ``Should relate interval with its name``() =
            intervalName Unisson |> should equal "Unisson"
            intervalName MinorSecond |> should equal "MinorSecond"
            intervalName MajorSecond |> should equal "MajorSecond"
            intervalName AugmentedSecond |> should equal "AugmentedSecond"
            intervalName MinorThird |> should equal "MinorThird"
            intervalName MajorThird |> should equal "MajorThird"
            intervalName PerfectFourth |> should equal "PerfectFourth"
            intervalName AugmentedFourth |> should equal "AugmentedFourth"
            intervalName DiminishedFifth |> should equal "DiminishedFifth"
            intervalName PerfectFifth |> should equal "PerfectFifth"
            intervalName AugmentedFifth |> should equal "AugmentedFifth"
            intervalName MinorSixth |> should equal "MinorSixth"
            intervalName MajorSixth |> should equal "MajorSixth"
            intervalName DiminishedSeventh |> should equal "DiminishedSeventh"
            intervalName MinorSeventh |> should equal "MinorSeventh"
            intervalName MajorSeventh |> should equal "MajorSeventh"
            intervalName PerfectOctave |> should equal "PerfectOctave"
            intervalName MinorNinth |> should equal "MinorNinth"
            intervalName MajorNinth |> should equal "MajorNinth"
            intervalName AugmentedNinth |> should equal "AugmentedNinth"
            intervalName PerfectEleventh |> should equal "PerfectEleventh"
            intervalName AugmentedEleventh |> should equal "AugmentedEleventh"
            intervalName MinorThirteenth |> should equal "MinorThirteenth"
            intervalName MajorThirteenth |> should equal "MajorThirteenth"

        [<Fact>]
        let ``Should relate interval with distances``() =
            fromDistance 0<ht> |> should equal Unisson
            fromDistance 1<ht> |> should equal MinorSecond
            fromDistance 2<ht> |> should equal MajorSecond
            fromDistance 3<ht> |> should equal MinorThird
            fromDistance 4<ht> |> should equal MajorThird
            fromDistance 5<ht> |> should equal PerfectFourth
            fromDistance 6<ht> |> should equal DiminishedFifth
            fromDistance 7<ht> |> should equal PerfectFifth
            fromDistance 8<ht> |> should equal AugmentedFifth
            fromDistance 9<ht> |> should equal MajorSixth
            fromDistance 10<ht> |> should equal MinorSeventh
            fromDistance 11<ht> |> should equal MajorSeventh
            fromDistance 12<ht> |> should equal PerfectOctave

        [<Fact>]
        let ``Should have frequency of 55Hz for A1``() =
            (float(440.0<hz> / 8.0)) |> should (equalWithin 0.1) (float(frequency A Octave.Contra))

        [<Fact>]
        let ``Should have frequency of 110Hz for A2``() =
            (float(440.0<hz> / 4.0)) |> should (equalWithin 0.1) (float(frequency A Octave.Great))

        [<Fact>]
        let ``Should have frequency of 220Hz for A3``() =
            (float(440.0<hz> / 2.0)) |> should (equalWithin 0.1) (float(frequency A Octave.Small))

        [<Fact>]
        let ``Should have frequency of 440Hz for A4``() =
            (float(440.0<hz> / 1.0)) |> should (equalWithin 0.1) (float(frequency A Octave.OneLine))

        [<Fact>]
        let ``Should have frequency of 880Hz for A5``() =
            (440.0 * 2.0) |> should (equalWithin 0.1) (float(frequency A Octave.TwoLine))

        [<Fact>]
        let ``Should have frequency of 1760Hz for A6``() =
            (440.0 * 4.0) |> should (equalWithin 0.1) (float(frequency A Octave.ThreeLine))

        [<Fact>]
        let ``Should have frequency of 7040Hz for A8``() =
            (440.0 * 16.0) |> should (equalWithin 0.1) (float(frequency A Octave.FiveLine))

        [<Fact>]
        let ``Should have frequency of 14080Hz for A9``() =
            (440.0 * 32.0) |> should (equalWithin 0.1) (float(frequency A Octave.SixLine))

        [<Fact>]
        let ``Should have frequency of 3520Hz for A7``() =
            (440.0 * 8.0) |> should (equalWithin 0.1) (float(frequency A Octave.FourLine))

        [<Fact>]
        let ``Should have frequency of 261.626Hz for C4``() =
            (261.626) |> should (equalWithin 0.1) (float(frequency C Octave.OneLine))

        [<Property>]
        let ``Small octave divides by 2 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine / 2.0)) |> should (equalWithin 0.1) (float(frequency note Octave.Small))

        [<Property>]
        let ``Great octave divides by 4 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine / 4.0)) |> should (equalWithin 0.1) (float(frequency note Octave.Great))

        [<Property>]
        let ``Contra octave divides by 8 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine / 8.0)) |> should (equalWithin 0.1) (float(frequency note Octave.Contra))

        [<Property>]
        let ``Two line octave doubles frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine * 2.0)) |> should (equalWithin 0.1) (float(frequency note Octave.TwoLine))

        [<Property>]
        let ``Three line octave quadruples frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine * 4.0)) |> should (equalWithin 0.1) (float(frequency note Octave.ThreeLine))

        [<Property>]
        let ``Four line octave multiplies by 8 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine * 8.0)) |> should (equalWithin 0.1) (float(frequency note Octave.FourLine))

        [<Property>]
        let ``Five line octave multiplies by 16 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine * 16.0)) |> should (equalWithin 0.1) (float(frequency note Octave.FiveLine))

        [<Property>]
        let ``Six line octave multiplies by 32 frequency of One line octave`` (note :Note) =
           (float(frequency note Octave.OneLine * 32.0)) |> should (equalWithin 0.1) (float(frequency note Octave.SixLine))

        [<Property>]
        let ``Should have midi name note+0 for note at sub contra octave``(note :Note) =
            midiName note Octave.SubContra |> should equal (noteName note + "0")

        [<Property>]
        let ``Should have midi name note+1 for note at contra octave``(note :Note) =
            midiName note Octave.Contra |> should equal (noteName note + "1")

        [<Property>]
        let ``Should have midi name note+2 for note at great octave``(note :Note) =
            midiName note Octave.Great |> should equal (noteName note + "2")

        [<Property>]
        let ``Should have midi name note+3 for note at small octave``(note :Note) =
            midiName note Octave.Small |> should equal (noteName note + "3")

        [<Property>]
        let ``Should have midi name note+4 for note at one line octave``(note :Note) =
            midiName note Octave.OneLine |> should equal (noteName note + "4")

        [<Property>]
        let ``Should have midi name note+5 for note at two line octave``(note :Note) =
            midiName note Octave.TwoLine |> should equal (noteName note + "5")

        [<Property>]
        let ``Should have midi name note+6 for note at three line octave``(note :Note) =
            midiName note Octave.ThreeLine |> should equal (noteName note + "6")

        [<Property>]
        let ``Should have midi name note+7 for note at four line octave``(note :Note) =
            midiName note Octave.FourLine |> should equal (noteName note + "7")

        [<Property>]
        let ``Should have midi name note+8 for note at five line octave``(note :Note) =
            midiName note Octave.FiveLine |> should equal (noteName note + "8")

        [<Property>]
        let ``Should have midi name note+9 for note at six line octave``(note :Note) =
            midiName note Octave.SixLine |> should equal (noteName note + "9")

        [<Property>]
        let ``Should have midi number from 0 to 11 on sub contra octave``(note :Note) =
            (midiNumber note Octave.SubContra) |> should be (greaterThan -1<midiNote>)
            (midiNumber note Octave.SubContra) |> should be (lessThan 12<midiNote>)

        [<Property>]
        let ``Should have midi number from 12 to 23 on contra octave``(note :Note) =
            (midiNumber note Octave.Contra) |> should be (greaterThan 11<midiNote>)
            (midiNumber note Octave.Contra) |> should be (lessThan 24<midiNote>)

        [<Property>]
        let ``Should have midi number from 24 to 35 on great octave``(note :Note) =
            (midiNumber note Octave.Great) |> should be (greaterThan 23<midiNote>)
            (midiNumber note Octave.Great) |> should be (lessThan 36<midiNote>)

        [<Property>]
        let ``Should have midi number from 36 to 47 on small octave``(note :Note) =
            (midiNumber note Octave.Small) |> should be (greaterThan 35<midiNote>)
            (midiNumber note Octave.Small) |> should be (lessThan 48<midiNote>)

        [<Property>]
        let ``Should have midi number from 48 to 59 on one line octave``(note :Note) =
            (midiNumber note Octave.OneLine) |> should be (greaterThan 47<midiNote>)
            (midiNumber note Octave.OneLine) |> should be (lessThan 60<midiNote>)

        [<Property>]
        let ``Should have midi number from 60 to 71 on two line octave``(note :Note) =
            (midiNumber note Octave.TwoLine) |> should be (greaterThan 59<midiNote>)
            (midiNumber note Octave.TwoLine) |> should be (lessThan 84<midiNote>)

        [<Property>]
        let ``Should have midi number from 72 to 83 on three line octave``(note :Note) =
            (midiNumber note Octave.ThreeLine) |> should be (greaterThan 71<midiNote>)
            (midiNumber note Octave.ThreeLine) |> should be (lessThan 84<midiNote>)

        [<Property>]
        let ``Should have midi number from 84 to 95 on four line octave``(note :Note) =
            (midiNumber note Octave.FourLine) |> should be (greaterThan 83<midiNote>)
            (midiNumber note Octave.FourLine) |> should be (lessThan 96<midiNote>)

        [<Property>]
        let ``Should have midi number from 96 to 107 on five line octave``(note :Note) =
            (midiNumber note Octave.FiveLine) |> should be (greaterThan 95<midiNote>)
            (midiNumber note Octave.FiveLine) |> should be (lessThan 108<midiNote>)

        [<Property>]
        let ``Should have midi number from 108 to 119 on six line octave``(note :Note) =
            (midiNumber note Octave.SixLine) |> should be (greaterThan 107<midiNote>)
            (midiNumber note Octave.SixLine) |> should be (lessThan 120<midiNote>)

        [<Property>]
        let ``Should have midi number from 120 to 131 on seven line octave``(note :Note) =
            (midiNumber note Octave.SevenLine) |> should be (greaterThan 119<midiNote>)
            (midiNumber note Octave.SevenLine) |> should be (lessThan 132<midiNote>)