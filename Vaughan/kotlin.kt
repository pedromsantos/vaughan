enum class Note(val noteName: String, val pitch: Int) {
    C("C", 0) {
        override fun sharp(): Note = CSharp
        override fun flat(): Note = B
    },
    CSharp("C#", 1) {
        override fun sharp(): Note = D
        override fun flat(): Note = C
    },
    DFlat("Db", 1) {
        override fun sharp(): Note = D
        override fun flat(): Note = C
    },
    D("D", 2) {
        override fun sharp(): Note = DSharp
        override fun flat(): Note = DFlat
    },
    DSharp("D#", 3) {
        override fun sharp(): Note = E
        override fun flat(): Note = D
    },
    EFlat("Eb", 3) {
        override fun sharp(): Note = E
        override fun flat(): Note = D
    },
    E("E", 4) {
        override fun sharp(): Note = F
        override fun flat(): Note = EFlat
    },
    F("F", 5) {
        override fun sharp(): Note = FSharp
        override fun flat(): Note = E
    },
    FSharp("F#", 6) {
        override fun sharp(): Note = G
        override fun flat(): Note = F
    },
    GFlat("Gb", 6) {
        override fun sharp(): Note = G
        override fun flat(): Note = F
    },
    G("G", 7) {
        override fun sharp(): Note = GSharp
        override fun flat(): Note = GFlat
    },
    GSharp("G#", 8) {
        override fun sharp(): Note = A
        override fun flat(): Note = G
    },
    AFlat("Ab", 8) {
        override fun sharp(): Note = A
        override fun flat(): Note = G
    },
    A("A", 9) {
        override fun sharp(): Note = ASharp
        override fun flat(): Note = AFlat
    },
    ASharp("A#", 10){
        override fun sharp(): Note = B
        override fun flat(): Note = A
    },
    BFlat("Bb", 10) {
        override fun sharp(): Note = B
        override fun flat(): Note = A
    },
    B("B", 11) {
        override fun sharp(): Note = C
        override fun flat(): Note = BFlat
    };

    public fun measureAbsoluteSemitones(to: Note) : Int {
		return to.pitch - this.pitch
    }

    public fun transpose(interval : Interval) : Note {
    	return when (interval) {
            Interval.Unisson -> this;
            else -> this
        }
    }

    abstract fun sharp(): Note
    abstract fun flat(): Note
}

enum class Interval(val intervalName: String, val abreviature: String, val distance: Int) {
    Unisson("Unisson", "U", 0),
    MinorSecond("MinorSecond", "m2", 1),
    MajorSecond("MajorSecond", "M2", 2),
    AugmentedSecond("AugmentedSecond", "A2", 3),
    MinorThird("MinorThird", "m3", 3),
    MajorThird("MajorThird", "M3", 4),
    PerfectFourth("PerfectFourth", "P4", 5),
    AugmentedFourth("AugmentedFourth", "A4", 6),
    DiminishedFifth("DiminishedFifth", "d5", 6),
    PerfectFifth("PerfectFifth", "P5", 7),
    AugmentedFifth("AugmentedFifth", "A5", 8),
    MinorSixth("MinorSixth", "m6", 8),
    MajorSixth("MajorSixth", "M6", 9),
    DiminishedSeventh("DiminishedSeventh", "d7", 9),
    MinorSeventh("MinorSeventh", "m7", 10),
    MajorSeventh("MajorSeventh", "M7", 11),
    PerfectOctave("PerfectOctave", "PO", 12),
    MinorNinth("MinorNinth", "m9", 13),
    MajorNinth("MajorNinth", "M9", 14),
    AugmentedNinth("AugmentedNinth", "A9", 15),
    PerfectEleventh("PerfectEleventh", "P11", 17),
    AugmentedEleventh("AugmentedEleventh", "A11", 18),
    MinorThirteenth("MinorThirteenth", "m13", 20),
    MajorThirteenth("MajorThirteenth", "M13", 21);

    public fun fromDistance(distance: Int) : Interval {
        return values().find { it -> it.distance == this.distance }
        		?: Unisson
    }
}

enum class NoteDuration(val durationName: String, val multiplier: Float) {
    Whole("Whole", 1.0F),
    Half("Half", 1.0F / 2.0F),
    Quarter("Quarter", 1.0F / 4.0F);

    public fun toBeats(timeSignature : Float) : Float {
        return this.multiplier * timeSignature
    }
}

enum class Octave(val octaveName: String, val value: Int, val midiBaseValue: Int){
    SubContra("Sub contra", -16, 0),
    Contra("Contra", -8, 12),
    Great("Great", -4, 24),
    Small("Small", -2, 36),
    OneLine("One line", 1, 48),
    TwoLine("Two line", 2, 60),
    Threeline("Tree line", 4, 72),
    FourLine("Four line", 8, 84),
    FiveLine("Five line", 16, 96),
    SixLine("Six line", 32, 108),
    SevenLine("Seven line", 64, 120);
}

enum class Key(val root: Note, val accidentals: Int) {
    AFlatMajor(Note.AFlat, -4),
    AMajor(Note.A, 3),
    BMajor(Note.B, 5),
    BFlatMajor(Note.BFlat, 2),
    CMajor(Note.C, 0),
    DFlatMajor(Note.DFlat, 5),
    DMajor(Note.D, 2),
    EMajor(Note.E, 4),
    EFlatMajor(Note.EFlat, 3),
    FMajor(Note.F, 1),
    FSharpMajor(Note.FSharp, 6),
    GMajor(Note.G, 1),
    GFlatMajor(Note.GFlat, 6),
    AMinor(Note.A, 0),
    BMinor(Note.B, 2),
    BFlatMinor(Note.BFlat, 5),
    CMinor(Note.C, 3),
    CSharpMinor(Note.CSharp, 4),
    DMinor(Note.D, 1),
    EMinor(Note.E, 1),
    FMinor(Note.F, 4),
    FSharpMinor(Note.FSharp, 3),
    GMinor(Note.G, 2),
    GSharpMinor(Note.GSharp, 5),
    EFlatMinor(Note.EFlat, 6);

    final val fifths = listOf(Note.F, Note.C, Note.G, Note.D, Note.A, Note.E, Note.B)

    private fun flatKey() : Set<Note> {
        return (fifths
            .asReversed()
            .drop(Math.abs(this.accidentals)))
            .union(fifths
                    .asReversed()
                    .takeLast(Math.abs(this.accidentals))
                    .map{it -> it.flat()})
    }

    private fun sharpKey() : Set<Note> {
        return (fifths
            .drop(this.accidentals))
            .union(fifths
                    .takeLast(this.accidentals)
                    .map{it -> it.sharp()})
    }

    private fun allNotes() : Set<Note> {
        return when {
        	this.accidentals < 0  -> flatKey()
            this.accidentals > 0 -> sharpKey()
            else -> fifths.toSet()
        }
    }

    public fun notes() : Set<Note> {
        val notes = allNotes()
        return notes
            	.sortedBy { it.pitch }
            	.dropWhile {it -> it != this.root}
                .union(
                	notes
                    .sortedBy { it.pitch }
                    .takeLastWhile {it -> it != this.root}
                )
    }
}

enum class ChordPattern(val patternName: String, val abreviature: String, val pattern: List<Interval>) {
    Major("Major", "Maj", listOf(Interval.Unisson)),
    Augmented("Major", "Maj", listOf(Interval.Unisson)),
    Major6("Major", "Maj", listOf(Interval.Unisson)),
    Major6Add9("Major", "Maj", listOf(Interval.Unisson)),
    Major6Flat5Add9("Major", "Maj", listOf(Interval.Unisson)),
    Major7("Major", "Maj", listOf(Interval.Unisson)),
    Major9("Major", "Maj", listOf(Interval.Unisson)),
    Major9Sharp11("Major", "Maj", listOf(Interval.Unisson)),
    Major11("Major", "Maj", listOf(Interval.Unisson)),
    Major13("Major", "Maj", listOf(Interval.Unisson)),
    Major13Sharp11("Major", "Maj", listOf(Interval.Unisson)),
    Augmented7("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7Flat5("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7Flat9("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7Sharp9("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7Flat5Flat9("Major", "Maj", listOf(Interval.Unisson)),
    Dominant7Flat5Sharp9("Major", "Maj", listOf(Interval.Unisson)),
    Dominant9("Major", "Maj", listOf(Interval.Unisson)),
    Dominant11("Major", "Maj", listOf(Interval.Unisson)),
    Dominant13("Major", "Maj", listOf(Interval.Unisson)),
    Minor("Major", "Maj", listOf(Interval.Unisson)),
    Diminished("Major", "Maj", listOf(Interval.Unisson)),
    Minor7("Major", "Maj", listOf(Interval.Unisson)),
    Minor6("Major", "Maj", listOf(Interval.Unisson)),
    Minor6Add9("Major", "Maj", listOf(Interval.Unisson)),
    Minor9("Major", "Maj", listOf(Interval.Unisson)),
    Diminished7("Major", "Maj", listOf(Interval.Unisson)),
    Minor7b5("Major", "Maj", listOf(Interval.Unisson)),
    MinorMaj7("Major", "Maj", listOf(Interval.Unisson)),
    MinorMaj9("Major", "Maj", listOf(Interval.Unisson)),
    Sus2("Major", "Maj", listOf(Interval.Unisson)),
    Sus2Diminished("Major", "Maj", listOf(Interval.Unisson)),
    Sus2Augmented("Major", "Maj", listOf(Interval.Unisson)),
    Sus4("Major", "Maj", listOf(Interval.Unisson)),
    Sus4Diminished("Major", "Maj", listOf(Interval.Unisson)),
    Sus4Augmented("Major", "Maj", listOf(Interval.Unisson));
}

enum class Scale(val pattern: List<Interval>) {
    Ionian(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    Dorian(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh)),
    Phrygian(listOf(Interval.Unisson, Interval.MinorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSixth, Interval.MinorSeventh)),
    Lydian(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.AugmentedFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    Mixolydian(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh)),
    Aolian(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSixth, Interval.MinorSeventh)),
    Locrian(listOf(Interval.Unisson, Interval.MinorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.DiminishedFifth, Interval.MinorSixth, Interval.MinorSeventh)),
    MajorPentatonic(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFifth, Interval.MajorSixth)),
    MinorPentatonic(listOf(Interval.Unisson, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSeventh)),
    Blues(listOf(Interval.Unisson, Interval.MinorThird, Interval.PerfectFourth, Interval.DiminishedFifth, Interval.PerfectFifth, Interval.MinorSeventh)),
    HarmonicMinor(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSixth, Interval.MajorSeventh)),
    MelodicMinor(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    DorianFlat2(listOf(Interval.Unisson, Interval.MinorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh)),
    NeapolitanMinor(listOf(Interval.Unisson, Interval.MinorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSixth, Interval.MajorSeventh)),
    LydianAugmented(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.AugmentedFourth, Interval.AugmentedFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    LydianDominant(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.AugmentedFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh)),
    MixolydianFlat6(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MinorSixth, Interval.MinorSeventh)),
    Bebop(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh, Interval.MajorSeventh)),
    LocrianSharp2(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.DiminishedFifth, Interval.MinorSixth, Interval.MinorSeventh)),
    AlteredDominant(listOf(Interval.Unisson, Interval.MinorSecond, Interval.AugmentedSecond, Interval.MajorThird, Interval.DiminishedFifth, Interval.AugmentedFifth, Interval.MinorSeventh)),
    HalfWholeDiminished(listOf(Interval.Unisson, Interval.MinorSecond, Interval.MinorThird, Interval.MajorThird, Interval.AugmentedFourth, Interval.PerfectFifth, Interval.MajorSixth, Interval.MinorSeventh)),
    WholeTone(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.DiminishedFifth, Interval.AugmentedFifth, Interval.MinorSeventh)),
    MajorSixthDiminishedScale(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.AugmentedFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    MinorSixthDiminishedScale(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MinorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.AugmentedFifth, Interval.MajorSixth, Interval.MajorSeventh)),
    DominantDiminishedScale(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.PerfectFifth, Interval.AugmentedFifth, Interval.MinorSeventh, Interval.MajorSeventh)),
    DominantFlat5DiminishedScale(listOf(Interval.Unisson, Interval.MajorSecond, Interval.MajorThird, Interval.PerfectFourth, Interval.DiminishedFifth, Interval.AugmentedFifth, Interval.MinorSeventh, Interval.MajorSeventh));
}

fun main() {
    println(Note.CSharp.sharp())
}