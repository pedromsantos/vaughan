namespace Vaughan

    module Domain =
        type Note =
            | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp
            | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B

        type ISharpNote = Note -> Note
        type IFlatNote = Note -> Note
        type INaturalNote = Note -> Note
        type INoteName = Note -> string
        type IPitchNote = Note -> int

        type Interval =
            | Unisson | MinorSecond | MajorSecond | AugmentedSecond | MinorThird
            | MajorThird | PerfectFourth | AugmentedFourth | DiminishedFifth
            | PerfectFifth | AugmentedFifth | MinorSixth | MajorSixth
            | DiminishedSeventh | MinorSeventh | MajorSeventh | PerfectOctave
            | MajorNinth | MinorNinth | AugmentedNinth
            | PerfectEleventh | AugmentedEleventh
            | MinorThirteenth | MajorThirteenth

        type IIntervalName = Interval -> string
        type IIntervalToDistance = Interval -> int
        type IIntervalToOctaveDistance = Interval -> int
        type IIntervalFromDistance = int -> Interval
        type IMeasureAbsoluteSemitones = Note -> Note -> int
        type IIntervalBetween = Note -> Note -> Interval
        type ITransposeNote = Note -> Interval -> Note

        type Scale =
            | Ionian | Dorian | Phrygian | Lydian | Mixolydian
            | Aolian | Locrian | MajorPentatonic | MinorPentatonic
            | Blues | HarmonicMinor | MelodicMinor | Dorianb2 | LydianAugmented | NeapolitanMinor
            | LydianDominant | Bebop | Mixolydianb6 | LocrianSharp2
            | AlteredDominant | HalfWholeDiminished | WholeTone

        type ScaleNotes = Note list
        type ICreateScale = Scale -> Note -> ScaleNotes

        type Key =
            | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
            | DMajor | DFlatMajor | EMajor | EFlatMajor
            | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
            | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
            | EMinor | FMinor | FSharpMinor | GMinor
            | GSharpMinor | EFlatMinor

        type KeyNotes = Note list
        type IKeyNotes = Key -> KeyNotes

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

        type ScaleDegrees =
            | I = 0 | II = 1 | III = 2 | IV = 3 | V = 4 | VI = 5 | VII = 6

        type GuitarString =
            | SixthString | FifthString | FourthString
            | ThirdString | SecondString | FirstString

        type Fret = {GuitarString:GuitarString; Fret:int; Note:Note}

        type Frets = Fret list

        type GuitarChord = {Chord:Chord; Frets:Frets}

        type ChordIntent = { Root: Note; Quality:ChordQuality; }