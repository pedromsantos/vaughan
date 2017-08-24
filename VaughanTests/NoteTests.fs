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
            measureAbsoluteSemitones C C =! 0<ht>
            measureAbsoluteSemitones C CSharp =! 1<ht>
            measureAbsoluteSemitones C DFlat =! 1<ht>
            measureAbsoluteSemitones C D =! 2<ht>
            measureAbsoluteSemitones C DSharp =! 3<ht>
            measureAbsoluteSemitones C EFlat =! 3<ht>
            measureAbsoluteSemitones C E =! 4<ht>
            measureAbsoluteSemitones C F =! 5<ht>
            measureAbsoluteSemitones C FSharp =! 6<ht>
            measureAbsoluteSemitones C GFlat =! 6<ht>
            measureAbsoluteSemitones C G =! 7<ht>
            measureAbsoluteSemitones C GSharp =! 8<ht>
            measureAbsoluteSemitones C AFlat =! 8<ht>
            measureAbsoluteSemitones C A =! 9<ht>
            measureAbsoluteSemitones C ASharp =! 10<ht>
            measureAbsoluteSemitones C BFlat =! 10<ht>
            measureAbsoluteSemitones C B =! 11<ht>

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

        [<Test>]
        let ``Should create beats from timeSignature and duration``() =
            (durationToBeats (1.0<beat>, Whole) Whole) =! 1.0<beat>
            (durationToBeats (2.0<beat>, Half) Half) =! 1.0<beat>

            (durationToBeats (4.0<beat>, Quarter) Whole) =! 4.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Half) =! 2.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Quarter) =! 1.0<beat>
            (durationToBeats (4.0<beat>, Quarter) Eigth) =! 0.5<beat>
            (durationToBeats (4.0<beat>, Quarter) Sixteenth) =! 0.25<beat>
            (durationToBeats (4.0<beat>, Quarter) ThirtySecond) =! 0.125<beat>

            (durationToBeats (3.0<beat>, Quarter) Quarter) =! 1.0<beat>

            (durationToBeats (12.0<beat>, Eigth) Eigth) =! 1.0<beat>

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
            fromDistance 0<ht> =! Unisson
            fromDistance 1<ht> =! MinorSecond
            fromDistance 2<ht> =! MajorSecond
            fromDistance 3<ht> =! MinorThird
            fromDistance 4<ht> =! MajorThird
            fromDistance 5<ht> =! PerfectFourth
            fromDistance 6<ht> =! DiminishedFifth
            fromDistance 7<ht> =! PerfectFifth
            fromDistance 8<ht> =! AugmentedFifth
            fromDistance 9<ht> =! MajorSixth
            fromDistance 10<ht> =! MinorSeventh
            fromDistance 11<ht> =! MajorSeventh
            fromDistance 12<ht> =! PerfectOctave

        [<Test>]
        let ``Should have frequency of 55Hz for A1``() =
            Assert.AreEqual(float(440.0<hz> / 8.0), float(frequency A Octave.Contra), 0.100)

        [<Test>]
        let ``Should have frequency of 110Hz for A2``() =
            Assert.AreEqual(float(440.0<hz> / 4.0), float(frequency A Octave.Great), 0.100)

        [<Test>]
        let ``Should have frequency of 220Hz for A3``() =
            Assert.AreEqual(float(440.0<hz> / 2.0), float(frequency A Octave.Small), 0.100)

        [<Test>]
        let ``Should have frequency of 440Hz for A4``() =
            Assert.AreEqual(float(440.0<hz> / 1.0), float(frequency A Octave.OneLine), 0.100)

        [<Test>]
        let ``Should have frequency of 880Hz for A5``() =
            Assert.AreEqual(440.0 * 2.0, float(frequency A Octave.TwoLine), 0.100)

        [<Test>]
        let ``Should have frequency of 1760Hz for A6``() =
            Assert.AreEqual(440.0 * 4.0, float(frequency A Octave.ThreeLine), 0.100)

        [<Test>]
        let ``Should have frequency of 7040Hz for A8``() =
            Assert.AreEqual(440.0 * 16.0, float(frequency A Octave.FiveLine), 0.100)

        [<Test>]
        let ``Should have frequency of 14080Hz for A9``() =
            Assert.AreEqual(440.0 * 32.0, float(frequency A Octave.SixLine), 0.100)

        [<Test>]
        let ``Should have frequency of 3520Hz for A7``() =
            Assert.AreEqual(440.0 * 8.0, float(frequency A Octave.FourLine), 0.100)

        [<Test>]
        let ``Should have frequency of 261.626Hz for C4``() =
            Assert.AreEqual(261.626, float(frequency C Octave.OneLine), 0.100)

        [<Property>]
        let ``Small octave divides by 2 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine / 2.0), float(frequency note Octave.Small), 0.100) 

        [<Property>]
        let ``Great octave divides by 4 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine / 4.0), float(frequency note Octave.Great), 0.100) 

        [<Property>]
        let ``Contra octave divides by 8 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine / 8.0), float(frequency note Octave.Contra), 0.100)  

        [<Property>]
        let ``Two line octave doubles frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine * 2.0), float(frequency note Octave.TwoLine), 0.100) 

        [<Property>]
        let ``Three line octave quadruples frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine * 4.0), float(frequency note Octave.ThreeLine), 0.100) 

        [<Property>]
        let ``Four line octave multiplies by 8 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine * 8.0), float(frequency note Octave.FourLine), 0.100) 

        [<Property>]
        let ``Five line octave multiplies by 16 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine * 16.0), float(frequency note Octave.FiveLine), 0.100) 

        [<Property>]
        let ``Six line octave multiplies by 32 frequency of One line octave`` (note :Note) =
           Assert.AreEqual(float(frequency note Octave.OneLine * 32.0), float(frequency note Octave.SixLine), 0.100)
        
        [<Property>]
        let ``Should have midi name note+0 for note at sub contra octave``(note :Note) =
            midiName note Octave.SubContra =! noteName note + "0"

        [<Property>]
        let ``Should have midi name note+1 for note at contra octave``(note :Note) =
            midiName note Octave.Contra =! noteName note + "1"

        [<Property>]
        let ``Should have midi name note+2 for note at great octave``(note :Note) =
            midiName note Octave.Great =! noteName note + "2"

        [<Property>]
        let ``Should have midi name note+3 for note at small octave``(note :Note) =
            midiName note Octave.Small =! noteName note + "3"

        [<Property>]
        let ``Should have midi name note+4 for note at one line octave``(note :Note) =
            midiName note Octave.OneLine =! noteName note + "4"
        
        [<Property>]
        let ``Should have midi name note+5 for note at two line octave``(note :Note) =
            midiName note Octave.TwoLine =! noteName note + "5"

        [<Property>]
        let ``Should have midi name note+6 for note at three line octave``(note :Note) =
            midiName note Octave.ThreeLine =! noteName note + "6"

        [<Property>]
        let ``Should have midi name note+7 for note at four line octave``(note :Note) =
            midiName note Octave.FourLine =! noteName note + "7"

        [<Property>]
        let ``Should have midi name note+8 for note at five line octave``(note :Note) =
            midiName note Octave.FiveLine =! noteName note + "8"

        [<Property>]
        let ``Should have midi name note+9 for note at six line octave``(note :Note) =
            midiName note Octave.SixLine =! noteName note + "9"

        [<Property>]
        let ``Should have midi number from 0 to 11 on sub contra octave``(note :Note) =
            (midiNumber note Octave.SubContra) >! -1<midiNote>
            (midiNumber note Octave.SubContra) <! 12<midiNote>

        [<Property>]
        let ``Should have midi number from 12 to 23 on contra octave``(note :Note) =
            (midiNumber note Octave.Contra) >! 11<midiNote>
            (midiNumber note Octave.Contra) <! 24<midiNote>

        [<Property>]
        let ``Should have midi number from 24 to 35 on great octave``(note :Note) =
            (midiNumber note Octave.Great) >! 23<midiNote>
            (midiNumber note Octave.Great) <! 36<midiNote>

        [<Property>]
        let ``Should have midi number from 36 to 47 on small octave``(note :Note) =
            (midiNumber note Octave.Small) >! 35<midiNote>
            (midiNumber note Octave.Small) <! 48<midiNote>

        [<Property>]
        let ``Should have midi number from 48 to 59 on one line octave``(note :Note) =
            (midiNumber note Octave.OneLine) >! 47<midiNote>
            (midiNumber note Octave.OneLine) <! 60<midiNote>
        
        [<Property>]
        let ``Should have midi number from 60 to 71 on two line octave``(note :Note) =
            (midiNumber note Octave.TwoLine) >! 59<midiNote>
            (midiNumber note Octave.TwoLine) <! 84<midiNote>

        [<Property>]
        let ``Should have midi number from 72 to 83 on three line octave``(note :Note) =
            (midiNumber note Octave.ThreeLine) >! 71<midiNote>
            (midiNumber note Octave.ThreeLine) <! 84<midiNote>

        [<Property>]
        let ``Should have midi number from 84 to 95 on four line octave``(note :Note) =
            (midiNumber note Octave.FourLine) >! 83<midiNote>
            (midiNumber note Octave.FourLine) <! 96<midiNote>

        [<Property>]
        let ``Should have midi number from 96 to 107 on five line octave``(note :Note) =
            (midiNumber note Octave.FiveLine) >! 95<midiNote>
            (midiNumber note Octave.FiveLine) <! 108<midiNote>

        [<Property>]
        let ``Should have midi number from 108 to 119 on six line octave``(note :Note) =
            (midiNumber note Octave.SixLine) >! 107<midiNote>
            (midiNumber note Octave.SixLine) <! 120<midiNote>

        [<Property>]
        let ``Should have midi number from 120 to 131 on seven line octave``(note :Note) =
            (midiNumber note Octave.SevenLine) >! 119<midiNote>
            (midiNumber note Octave.SevenLine) <! 132<midiNote>