namespace Vaughan

    [<AutoOpen>]
    module Domain =
        [<Measure>] type s
        [<Measure>] type hz
        [<Measure>] type ht
        [<Measure>] type pan
        [<Measure>] type bpm
        [<Measure>] type loud
        [<Measure>] type beat
        [<Measure>] type midiNote

        type Note =
            | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp
            | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B

        type Duration =
            | Whole | Half | Quarter | Eigth
            | Sixteenth | ThirtySecond
            | SixtyFourth | HundredTwentyEighth

        type SharpNote = Note -> Note
        type FlatNote = Note -> Note
        type NaturalNote = Note -> Note
        type NoteName = Note -> string
        type NotePitch = Note -> int

        type Interval =
            | Unisson | MinorSecond | MajorSecond | AugmentedSecond | MinorThird
            | MajorThird | PerfectFourth | AugmentedFourth | DiminishedFifth
            | PerfectFifth | AugmentedFifth | MinorSixth | MajorSixth
            | DiminishedSeventh | MinorSeventh | MajorSeventh | PerfectOctave
            | MajorNinth | MinorNinth | AugmentedNinth
            | PerfectEleventh | AugmentedEleventh
            | MinorThirteenth | MajorThirteenth

        type IntervalName = Interval -> string
        type IntervalToDistance = Interval -> int<ht>
        type IntervalToOctaveDistance = Interval -> int<ht>
        type IntervalFromDistance = int<ht> -> Interval
        type MeasureAbsoluteSemitones = Note -> Note -> int<ht>
        type IntervalBetween = Note -> Note -> Interval
        type TransposeNote = Note -> Interval -> Note

        type Octave =
            | SubContra | Contra | Great | Small
            | OneLine | TwoLine | ThreeLine | FourLine
            | FiveLine | SixLine | SevenLine

        type MidiNote = Note * Octave

        type OctaveName = Octave -> string
        type OctaveMidiNumber = Octave -> int
        type Frequency = Note -> Octave -> float<hz>
        type NoteMidiName = Note -> Octave -> string
        type NoteMidiNumber = Note -> Octave -> int<midiNote>

        type BeatNote = Note * Octave * Duration
        type BeatNotes = BeatNote list
        type BarBeat = float<beat> * BeatNotes
        type TimeSignature = int<beat> * Duration
        type Bar = BarBeat list

        type Key =
            | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
            | DMajor | DFlatMajor | EMajor | EFlatMajor
            | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
            | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
            | EMinor | FMinor | FSharpMinor | GMinor
            | GSharpMinor | EFlatMinor

        type KeyNotes = Note list

        type IKeyNotes = Key -> KeyNotes

        type Section = TimeSignature * Key * Bar list

        type Song = Section list

        type ChordQuality =
            | Major | Augmented
            | Major6 | Major6Add9 | Major6Flat5Add9
            | Major7 | Major9 | Major9Sharp11 | Major11 | Major13 | Major13Sharp11 | Augmented7
            | Dominant7 | Dominant7Flat5 | Dominant7Flat9 | Dominant7Sharp9
            | Dominant7Flat5Flat9 | Dominant7Flat5Sharp9
            | Dominant9 | Dominant11 | Dominant13
            | Minor | Diminished
            | Minor7 | Minor6 | Minor6Add9 | Minor9 | Diminished7 | Minor7b5
            | MinorMaj7 | MinorMaj9
            | Sus2 | Sus2Diminished | Sus2Augmented
            | Sus4 | Sus4Diminished | Sus4Augmented

        type ChordNoteFunction =
            | Root | Third | Fifth | Sixth | Seventh | Ninth | Eleventh | Thirteenth

        type ChordNote = Note * ChordNoteFunction
        type ChordNotes = ChordNote list
        type ChordType = | Open | Closed | Triad | Drop2 | Drop3
        type Chord = {Notes:ChordNotes; ChordType:ChordType; Name:string}

        type ChordName = Chord -> string
        type Invert = Chord -> Chord
        type Root = Chord -> Note
        type Bass = Chord -> Note
        type Lead = Chord -> Note
        type ToOpen = Chord -> Chord
        type ToDrop2 = Chord -> Chord
        type ToDrop3 = Chord -> Chord
        type ToTriad = Chord -> Chord
        type ToClosed = Chord -> Chord
        type NoteNames = Chord -> string List
        type ChordsFitting = Note list -> Chord list
        type CreateChord = Note -> ChordQuality -> Chord
        type SkipChordFunction = ChordNoteFunction -> Chord -> Chord
        type InvertionWithBassClosestToNote = Chord -> Note -> Chord
        type InvertionWithLeadClosestToNote = Chord -> Note -> Chord
        type InversionForFunctionAsBass= Chord -> ChordNoteFunction -> Chord
        type InversionForFunctionAsLead = Chord -> ChordNoteFunction -> Chord

        type ScaleDegree =
            | I = 0 | II = 1 | III = 2 | IV = 3 | V = 4 | VI = 5 | VII = 6

        type ScaleType =
            | Ionian | Dorian | Phrygian | Lydian | Mixolydian
            | Aolian | Locrian | MajorPentatonic | MinorPentatonic
            | Blues | HarmonicMinor | MelodicMinor | Dorianb2 | LydianAugmented | NeapolitanMinor
            | LydianDominant | Bebop | Mixolydianb6 | LocrianSharp2
            | AlteredDominant | HalfWholeDiminished | WholeTone
            | SixthDiminishedScale | MinorSixthDiminishedScale
            | DominantDiminishedScale | Dominantb5DiminishedScale

        type ScaleNotes = Note list
        type Scale = {Scale:ScaleType; Notes:ScaleNotes}

        type ScalesFitting = Chord -> Scale list
        type CreateScale = ScaleType -> Note -> Scale
        type CreateScaleNotes = ScaleType -> Note -> ScaleNotes
        type NinthsHarmonizer = ScaleDegree -> ScaleNotes -> Chord
        type SeventhsHarmonizer = ScaleDegree -> ScaleNotes -> Chord
        type TriadsHarmonizer = ScaleDegree -> ScaleNotes -> Chord

        type GuitarString =
            | SixthString | FifthString | FourthString
            | ThirdString | SecondString | FirstString

        type Fret = {GuitarString:GuitarString; Fret:int; Note:Note}
        type Frets = Fret list
        type GuitarChord = {Chord:Chord; Frets:Frets}

        type Tabify = GuitarChord -> string
        type Shapify = GuitarChord -> string
        type TabifyAll = GuitarChord list -> string

        type ChordIntent = { Root: Note; Quality:ChordQuality; }

        type ParseChord = string -> ChordIntent
        type ParseInterval = string -> Interval
        type ParseOctave = string -> Octave
        type ParseScale = string -> Scale
        type ParseNote = string -> Note
        type ParseMidiNote = string -> MidiNote
        type CreateChordFromIntent = ChordIntent -> Chord