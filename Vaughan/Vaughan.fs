namespace Vaughan

    //https://repl.it/FJHh/63

    module Infrastructure =
        let rotateByOne list =
            match list with
            | [] -> []
            | f::t -> t @ [f]

        let swapFirstTwo list =
            match list with
            | [] -> []
            | f::s::r -> s::f::r
            | f -> f

        let swapSecondTwo list =
            match list with
            | [] -> []
            | f::s::t::r -> f::t::s::r
            | f::s::t -> f::s::t
            | f -> f

        let circularSequenceFromList (lst:'a list) =
            let rec next () =
                seq {
                    for element in lst do
                        yield element
                    yield! next()
                }
            next()

        let private sequenceToIndexValueTupleSequence sequence =
            sequence |> Seq.mapi (fun i v -> i, v)

        let filterOddIndexElements sequence =
            sequence
            |> sequenceToIndexValueTupleSequence
            |> Seq.filter (fun (i, _) -> i % 2 = 0)
            |> Seq.map snd

        let rec min (minOf:'a->'a->'a) (list:'a list) =
            match list with
            | [] -> None
            | [x] -> Some(x)
            | c1::c2::rest -> min minOf ((minOf c1 c2)::rest)

        let cappedMinimum number cap =
            if number < cap then cap else number

        let minimumPositive number =
            cappedMinimum number 0

        let cappedMaximum number cap =
            if number > cap then cap else number

        let allCombinations frets =
            let prefix fs pfx = pfx :: fs
            let prefixWith pfxs fs = List.map (prefix fs) pfxs
            let prefixAll pfxs fs = List.collect (prefixWith pfxs) fs
            List.foldBack prefixAll frets [[]]

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
            | Blues | HarmonicMinor | MelodicMinor | Dorianb2 | LydianAugmented
            | LydianDominant | Mixolydianb6 | LocrianSharp2
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

    module Notes =
        open Domain

        type private NoteAttributes = {Name:string; Sharp:Note; Flat:Note; Pitch:int}
        type private IntervalAttributes = {Name:string; Distance:int; Transpose: (Note -> Note)}
        type private INoteAttributes = Note -> NoteAttributes
        type private IIntervalAttributes = Interval -> ISharpNote -> IFlatNote -> INaturalNote -> IntervalAttributes
        type private ITransposeNoteForInterval = Note -> Interval -> Note

        let private noteAttributes:INoteAttributes = function
            | C -> {Name="C"; Sharp=CSharp; Flat=B; Pitch=0}
            | CSharp -> {Name="C#"; Sharp=D; Flat=C; Pitch=1}
            | DFlat -> {Name="Db"; Sharp=D; Flat=C; Pitch=1}
            | D -> {Name="D"; Sharp=DSharp; Flat=DFlat; Pitch=2}
            | DSharp -> {Name="D#"; Sharp=E; Flat=D; Pitch=3}
            | EFlat -> {Name="Eb"; Sharp=E; Flat=D; Pitch=3}
            | E -> {Name="E"; Sharp=F; Flat=EFlat; Pitch=4}
            | F -> {Name="F"; Sharp=FSharp; Flat=E; Pitch=5}
            | FSharp -> {Name="F#"; Sharp=G; Flat=F; Pitch=6}
            | GFlat -> {Name="Gb"; Sharp=G; Flat=F; Pitch=6}
            | G -> {Name="G"; Sharp=GSharp; Flat=GFlat; Pitch=7}
            | GSharp -> {Name="G#"; Sharp=A; Flat=G; Pitch=8}
            | AFlat -> {Name="Ab"; Sharp=A; Flat=G; Pitch=8}
            | A -> {Name="A"; Sharp=ASharp; Flat=AFlat; Pitch=9}
            | ASharp -> {Name="A#"; Sharp=B; Flat=A; Pitch=10}
            | BFlat -> {Name="Bb"; Sharp=B; Flat=A; Pitch=10}
            | B -> {Name="B"; Sharp=C; Flat=BFlat; Pitch=11}

        let private intervalAttributes:IIntervalAttributes = fun interval sharp flat natural ->
            match interval with
            | Unisson -> {Name="Unisson"; Distance=0; Transpose=natural}
            | MinorSecond -> {Name="MinorSecond"; Distance=1; Transpose=flat}
            | MajorSecond -> {Name="MajorSecond"; Distance=2; Transpose=sharp}
            | AugmentedSecond -> {Name="AugmentedSecond"; Distance=3; Transpose=sharp}
            | MinorThird -> {Name="MinorThird"; Distance=3; Transpose=flat}
            | MajorThird -> {Name="MajorThird"; Distance=4; Transpose=sharp}
            | PerfectFourth -> {Name="PerfectForth"; Distance=5; Transpose=sharp}
            | AugmentedFourth -> {Name="AugmentedForth"; Distance=6; Transpose=sharp}
            | DiminishedFifth -> {Name="DiminishedFifth"; Distance=6; Transpose=flat}
            | PerfectFifth -> {Name="PerfectFifth"; Distance=7; Transpose=sharp}
            | AugmentedFifth -> {Name="AugmentedFifth"; Distance=8; Transpose=sharp}
            | MinorSixth -> {Name="MinorSixth"; Distance=8; Transpose=flat}
            | MajorSixth -> {Name="MajorSixth"; Distance=9; Transpose=sharp}
            | DiminishedSeventh -> {Name="DiminishSeventh"; Distance=9; Transpose=flat}
            | MinorSeventh -> {Name="MinorSeventh"; Distance=10; Transpose=flat}
            | MajorSeventh -> {Name="MajorSeventh"; Distance=11; Transpose=sharp}
            | PerfectOctave -> {Name="PerfectOctave"; Distance=12; Transpose=natural}
            | MinorNinth -> {Name="MinorNinth"; Distance=13; Transpose=flat}
            | MajorNinth -> {Name="MajorNinth"; Distance=14; Transpose=sharp}
            | AugmentedNinth -> {Name="AugmentedNinth"; Distance=15; Transpose=sharp}
            | PerfectEleventh -> {Name="PerfectEleventh"; Distance=17; Transpose=sharp}
            | AugmentedEleventh -> {Name="AugmentedEleventh"; Distance=18; Transpose=sharp}
            | MinorThirteenth -> {Name="MinorThirteenth"; Distance=20; Transpose=flat}
            | MajorThirteenth -> {Name="MajorThirteenth"; Distance=21; Transpose=sharp}

        let private intervalAttributesWithDefaults interval =
            (intervalAttributes interval (fun n -> (noteAttributes n).Sharp) (fun n -> (noteAttributes n).Flat) id)

        let private transposeNoteForInterval:ITransposeNoteForInterval = fun note interval ->
            (intervalAttributesWithDefaults interval).Transpose note

        let sharp:ISharpNote = fun note ->
            (noteAttributes note).Sharp

        let flat:IFlatNote = fun note ->
            (noteAttributes note).Flat

        let natural:INaturalNote = id

        let noteName:INoteName = fun note ->
            (noteAttributes note).Name

        let pitch:IPitchNote = fun note ->
            (noteAttributes note).Pitch

        let intervalName:IIntervalName = fun interval ->
            (intervalAttributesWithDefaults interval).Name

        let toDistance:IIntervalToDistance = fun interval ->
            (intervalAttributesWithDefaults interval).Distance

        let toOctaveDistance:IIntervalToDistance = fun interval ->
            let distance = toDistance interval
            let octaveDistance = toDistance PerfectOctave
            if distance > octaveDistance then distance - octaveDistance else distance

        let fromDistance:IIntervalFromDistance = function
            | 0 -> Unisson
            | 1 -> MinorSecond
            | 2 -> MajorSecond
            | 3 -> MinorThird
            | 4 -> MajorThird
            | 5 -> PerfectFourth
            | 6 -> DiminishedFifth
            | 7 -> PerfectFifth
            | 8 -> AugmentedFifth
            | 9 -> MajorSixth
            | 10 -> MinorSeventh
            | 11 -> MajorSeventh
            | 12 -> PerfectOctave
            | 13 -> MinorNinth
            | 14 -> MajorNinth
            | 15 -> AugmentedNinth
            | 17 -> PerfectEleventh
            | 18 -> AugmentedEleventh
            | 20 -> MinorThirteenth
            | 21 -> MajorThirteenth
            | _ -> Unisson

        let measureAbsoluteSemitones:IMeasureAbsoluteSemitones = fun note other ->
            let distance = (pitch other) - (pitch note)
            if distance < (toOctaveDistance Unisson)
            then (toDistance PerfectOctave) - distance * -1
            else distance

        let intervalBetween:IIntervalBetween = fun note other ->
            fromDistance (measureAbsoluteSemitones note other)

        let private isSameInterval interval otherInterval =
            toOctaveDistance interval = toOctaveDistance otherInterval

        let transpose:ITransposeNote = fun noteToTranspose transposingInterval ->
            let rec loop note =
                let transposedNote = transposeNoteForInterval note transposingInterval
                let newInterval = intervalBetween noteToTranspose transposedNote
                if isSameInterval newInterval transposingInterval
                    then transposedNote
                    else loop transposedNote

            loop noteToTranspose

    module Scales =
        open Domain
        open Notes

        type private ScaleFormula = Interval list
        type private IScaleFormula = Scale -> ScaleFormula

        let private scaleFormula:IScaleFormula = function
            | Ionian -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorian -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Phrygian -> [Unisson; MinorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | Lydian -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Mixolydian -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Aolian -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | Locrian -> [Unisson; MinorSecond; MinorThird; PerfectFourth; DiminishedFifth; MinorSixth; MinorSeventh]
            | MajorPentatonic -> [Unisson; MajorSecond; MajorThird; PerfectFifth; MajorSixth]
            | MinorPentatonic -> [Unisson; MinorThird; PerfectFourth; PerfectFifth; MinorSeventh]
            | Blues -> [Unisson; MinorThird; PerfectFourth; DiminishedFifth; PerfectFifth; MinorSeventh]
            | HarmonicMinor -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MajorSeventh]
            | MelodicMinor -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorianb2 -> [Unisson; MinorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | LydianAugmented -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; AugmentedFifth; MajorSixth; MajorSeventh]
            | LydianDominant -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Mixolydianb6 -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | LocrianSharp2 -> [Unisson; MajorSecond; MinorThird; PerfectFourth; DiminishedFifth; MinorSixth; MinorSeventh]
            | AlteredDominant -> [Unisson; MinorSecond; AugmentedSecond; MajorThird; DiminishedFifth;  AugmentedFifth; MinorSeventh]
            | HalfWholeDiminished -> [Unisson; MinorSecond; MinorThird; MajorThird; AugmentedFourth;  PerfectFifth; MajorSixth; MinorSeventh]
            | WholeTone -> [Unisson; MajorSecond; MajorThird; DiminishedFifth; AugmentedFifth; MinorSeventh]

        let createScale:ICreateScale = fun scale root ->
            scaleFormula scale |> List.map (fun interval -> transpose root interval)

    module Keys =
        open Domain
        open Notes

        type private KeyAttributes = {Root:Note; Accidentals:int}
        type private IKeyAttributes = Key -> KeyAttributes

        let private keyAttributes = function
            | AMajor -> {Root=A; Accidentals=3}
            | AFlatMajor -> {Root=AFlat; Accidentals=(-4)}
            | BMajor -> {Root=B; Accidentals=5}
            | BFlatMajor -> {Root=BFlat; Accidentals=(-2)}
            | CMajor -> {Root=C; Accidentals=0}
            | DMajor -> {Root=D; Accidentals=2}
            | DFlatMajor -> {Root=DFlat; Accidentals=(-5)}
            | EMajor -> {Root=E; Accidentals=4}
            | EFlatMajor -> {Root=EFlat; Accidentals=(-3)}
            | FMajor -> {Root=F; Accidentals=(-1)}
            | FSharpMajor -> {Root=FSharp; Accidentals=6}
            | GMajor -> {Root=G; Accidentals=1}
            | GFlatMajor -> {Root=GFlat; Accidentals=(-6)}
            | AMinor -> {Root=A; Accidentals=0}
            | BMinor -> {Root=B; Accidentals=2}
            | BFlatMinor -> {Root=BFlat; Accidentals=(-5)}
            | CMinor -> {Root=C; Accidentals=(-3)}
            | CSharpMinor -> {Root=CSharp; Accidentals=4}
            | DMinor -> {Root=D; Accidentals=(-1)}
            | EMinor -> {Root=E; Accidentals=1}
            | FMinor -> {Root=F; Accidentals=(-4)}
            | FSharpMinor -> {Root=FSharp; Accidentals=3}
            | GMinor -> {Root=G; Accidentals=(-2)}
            | GSharpMinor -> {Root=GSharp; Accidentals=5}
            | EFlatMinor -> {Root=EFlat; Accidentals=(-6)}

        let private root key =
            (keyAttributes key).Root

        let private accidentals key =
            (keyAttributes key).Accidentals

        let private flatKey fifths keyAccidents =
            (fifths |> List.rev |> List.skip -keyAccidents)
            @ (fifths
            |> List.rev
            |> List.take(-keyAccidents)
            |> List.map flat)

        let private sharpKey fifths keyAccidents =
            ((fifths |> List.skip keyAccidents) )
            @ (fifths
            |> List.take(keyAccidents)
            |> List.map sharp)

        let private rawKeyNotes key =
            let fifths = [F; C; G; D; A; E; B;]
            let keyAccidents = accidentals key

            if keyAccidents = 0 then
                fifths
            else
                if keyAccidents < 0 then
                    flatKey fifths keyAccidents
                else
                    sharpKey fifths keyAccidents

        let keyNotes:IKeyNotes = fun key ->
            (rawKeyNotes key
            |> List.sortBy pitch
            |> List.skipWhile (fun n -> n <> root key))
            @
            (rawKeyNotes key
            |> List.sortBy pitch
            |> List.takeWhile (fun n -> n <> root key))

    module Chords =
        open Domain
        open Notes
        open Infrastructure

        type private ChordAttributes = {Name:string; Quality:ChordQuality; Formula:Interval list}

        let private chordAttributes =
            [
                {Name="Maj"; Quality=Major; Formula=[MajorThird; PerfectFifth]}
                {Name="Aug"; Quality=Augmented; Formula=[MajorThird; AugmentedFifth]}
                {Name="Min"; Quality=Minor; Formula=[MinorThird; PerfectFifth]}
                {Name="Dim"; Quality=Diminished; Formula=[MinorThird; DiminishedFifth]}
                {Name="Maj7"; Quality=Major7; Formula=[MajorThird; PerfectFifth; MajorSeventh]}
                {Name="Maj9"; Quality=Major9; Formula=[MajorThird; PerfectFifth; MajorSeventh; MajorNinth]}
                {Name="Maj9(#11)"; Quality=Major9Sharp11; Formula=[MajorThird; PerfectFifth; MajorSeventh; MajorNinth; AugmentedEleventh]}
                {Name="Maj11"; Quality=Major11; Formula=[MajorThird; PerfectFifth; MajorSeventh; PerfectEleventh]}
                {Name="Maj13"; Quality=Major13; Formula=[MajorThird; PerfectFifth; MajorSeventh; MajorThirteenth]}
                {Name="Maj13(#11)"; Quality=Major13Sharp11; Formula=[MajorThird; PerfectFifth; MajorSeventh; MajorThirteenth; AugmentedEleventh]}
                {Name="6"; Quality=Major6; Formula=[MajorThird; PerfectFifth; MajorSixth]}
                {Name="6add9"; Quality=Major6Add9; Formula=[MajorThird; PerfectFifth; MajorSixth; MajorNinth]}
                {Name="6(b5)add9"; Quality=Major6Flat5Add9; Formula=[MajorThird; DiminishedFifth; MajorSixth; MajorNinth]}
                {Name="Aug7"; Quality=Augmented7; Formula=[MajorThird; AugmentedFifth; MajorSeventh]}
                {Name="Min7"; Quality=Minor7; Formula=[MinorThird; PerfectFifth; MinorSeventh]}
                {Name="Min9"; Quality=Minor9; Formula=[MinorThird; PerfectFifth; MinorSeventh; MajorNinth]}
                {Name="Min6"; Quality=Minor6; Formula=[MinorThird; PerfectFifth; MajorSixth]}
                {Name="Min6Add9"; Quality=Minor6Add9; Formula=[MinorThird; PerfectFifth; MajorSixth; MajorNinth]}
                {Name="Min7b5"; Quality=Minor7b5; Formula=[MinorThird; DiminishedFifth; MinorSeventh]}
                {Name="MinMaj7"; Quality=MinorMaj7; Formula=[MinorThird; PerfectFifth; MajorSeventh]}
                {Name="MinMaj9"; Quality=MinorMaj9; Formula=[MinorThird; PerfectFifth; MajorSeventh; MajorNinth]}
                {Name="Min7(b9)"; Quality=MinorMaj9; Formula=[MinorThird; PerfectFifth; MinorSeventh; MinorNinth]}
                {Name="Min7(b5b9)"; Quality=MinorMaj9; Formula=[MinorThird; DiminishedFifth; MinorSeventh; MinorNinth]}
                {Name="Dim7"; Quality=Diminished7; Formula=[MinorThird; DiminishedFifth; DiminishedSeventh]}
                {Name="Dim7"; Quality=Diminished7; Formula=[MinorThird; DiminishedFifth; MajorSixth]}
                {Name="7"; Quality=Dominant7; Formula=[MajorThird; PerfectFifth; MinorSeventh]}
                {Name="7(b5)"; Quality=Dominant7Flat5; Formula=[MajorThird; DiminishedFifth; MinorSeventh]}
                {Name="7(b9)"; Quality=Dominant7Flat9; Formula=[MajorThird; PerfectFifth; MinorSeventh; MinorNinth]}
                {Name="7(#9)"; Quality=Dominant7Sharp9; Formula=[MajorThird; PerfectFifth; MinorSeventh; AugmentedNinth]}
                {Name="7(b5b9)"; Quality=Dominant7Flat5Flat9; Formula=[MajorThird; DiminishedFifth; MinorSeventh; MinorNinth]}
                {Name="7(b5#9)"; Quality=Dominant7Flat5Sharp9; Formula=[MajorThird; DiminishedFifth; MinorSeventh; AugmentedNinth]}
                {Name="9"; Quality=Dominant9; Formula=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth]}
                {Name="11"; Quality=Dominant11; Formula=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth; PerfectEleventh]}
                {Name="13"; Quality=Dominant13; Formula=[MajorThird; PerfectFifth; MinorSeventh; MajorNinth; PerfectEleventh; MajorThirteenth]}
                {Name="Sus2"; Quality=Sus2; Formula=[MajorSecond; PerfectFifth]}
                {Name="Sus2Dim"; Quality=Sus2Diminished; Formula=[MajorSecond; DiminishedFifth]}
                {Name="Sus2Aug"; Quality=Sus2Augmented; Formula=[MajorSecond; AugmentedFifth]}
                {Name="Sus4"; Quality=Sus4; Formula=[PerfectFourth; PerfectFifth]}
                {Name="Sus4Dim"; Quality=Sus4Diminished; Formula=[PerfectFourth; DiminishedFifth]}
                {Name="Sus4Aug"; Quality=Sus4Augmented; Formula=[PerfectFourth; AugmentedFifth]}
            ]

        let private qualityForIntervals intervals =
            (chordAttributes
            |> List.filter (fun c -> c.Formula = intervals)
            |> List.head).Quality

        let private intervalsForQuality quality =
            (chordAttributes
            |> List.filter (fun c -> c.Quality = quality)
            |> List.head).Formula

        let private nameForQuality quality =
            (chordAttributes
            |> List.filter (fun c -> c.Quality = quality)
            |> List.head).Name

        let private functionForInterval = function
            | Unisson -> Root
            | MajorThird | MinorThird | MajorSecond | MinorSecond | PerfectFourth | AugmentedFourth -> Third
            | PerfectFifth | DiminishedFifth | AugmentedFifth  -> Fifth
            | MajorSixth -> Sixth
            | MajorSeventh | MinorSeventh | DiminishedSeventh -> Seventh
            | MajorNinth | MinorNinth | AugmentedNinth -> Ninth
            | PerfectEleventh | AugmentedEleventh -> Eleventh
            | MajorThirteenth -> Thirteenth
            | _ -> Root

        let private note chordNote =
            fst chordNote

        let private noteFunction chordNote =
            snd chordNote

        let private noteForFunction chord chordNoteFunction =
            note (chord.Notes |> List.find (fun n -> noteFunction n = chordNoteFunction))

        let private adjustIntervalForFunctionsAboveSeventh interval noteFunction =
            match noteFunction with
            | Ninth | Eleventh | Thirteenth -> fromDistance ((toDistance interval) + (toDistance PerfectOctave))
            | _ -> interval

        let private intervalsForChord chord =
            let root = noteForFunction chord Root
            chord.Notes
            |> List.map (fun n -> adjustIntervalForFunctionsAboveSeventh (intervalBetween root (note n)) (noteFunction n))
            |> List.skip 1

        let private invertOpenOrClosed chord =
            {chord with Notes= rotateByOne chord.Notes;}

        let private invertDrop2 chord =
            {
                chord with Notes= [chord.Notes |> List.last]
                                    @ (chord.Notes
                                        |> List.take (chord.Notes.Length - 1)
                                        |> rotateByOne
                                        |> rotateByOne)
            }

        let private invertDrop3 chord =
            {chord with Notes= chord.Notes |> rotateByOne |> rotateByOne |> swapSecondTwo;}

        let name chord =
            noteName (noteForFunction chord Root)
            + nameForQuality (qualityForIntervals(intervalsForChord chord))

        let invert chord =
            match chord.ChordType with
            | Closed | Open | Triad -> invertOpenOrClosed chord
            | Drop2 -> invertDrop2 chord
            | Drop3 -> invertDrop3 chord

        let bass chord =
            note (chord.Notes |> List.head)

        let lead chord =
            note (chord.Notes |> List.last)

        let noteNames chord =
            chord.Notes |> List.map (note >> noteName)

        let chord root quality =
            {
                Notes= [(root, Root)] @ (intervalsForQuality quality |> List.map (fun i -> ((transpose root i), functionForInterval i)));
                ChordType = Closed
                Name =  noteName root + nameForQuality (qualityForIntervals(intervalsForQuality quality))
            }

        let (=>) root quality =
            chord root quality

        let add chords chord =
            chord :: chords |> rotateByOne

        let (/./) chords chord =
            add chords chord

        let toDrop2 chord =
            if chord.Notes.Length = 4
            then {chord with Notes = chord.Notes |> swapFirstTwo |> rotateByOne; ChordType=Drop2}
            else chord

        let toDrop3 chord =
            if chord.Notes.Length = 4
            then {chord with Notes= (chord |> toDrop2 |> toDrop2).Notes; ChordType=Drop3}
            else chord

        let toTriad chord =
            if chord.Notes.Length = 3 then {chord with ChordType=Triad}
            else chord

        let toOpen chord =
            {chord with ChordType=Open}

        let toClosed chord =
            {chord with ChordType=Closed}

        let skipFunction functionToSkipp chord =
            {chord with Notes = chord.Notes |> List.filter (fun nf -> snd nf <> functionToSkipp)}

    module ChordVoiceLeading =
        open Chords
        open Domain
        open Notes
        open Infrastructure

        let private isLeadFunctionOnChordDesiredFunction chord desiredNoteFunction desiredPosition =
            snd (chord.Notes |> desiredPosition) = desiredNoteFunction

        let rec private repeatInversion chord times =
            match times with
            | 0 -> chord
            | _ -> repeatInversion (chord |> invert) (times - 1)

        let private allInversions chord =
            let notesInChord = chord.Notes |> List.length
            [for index in 1 .. notesInChord do yield repeatInversion chord index]

        let private inversionForFunction chord desiredNoteFunction desiredPosition =
            allInversions chord
            |> List.filter (fun c -> isLeadFunctionOnChordDesiredFunction c desiredNoteFunction desiredPosition)
            |> List.head

        let private invertionWithNoteClosestToNote chord note desiredPosition =
            (allInversions chord
            |> min (fun c1 c2 ->
                if (measureAbsoluteSemitones (desiredPosition c1) note) < (measureAbsoluteSemitones (desiredPosition c2) note)
                then c1 else c2)).Value

        let inversionForFunctionAsLead chord desiredNoteFunction =
            inversionForFunction chord desiredNoteFunction List.last

        let inversionForFunctionAsBass chord desiredNoteFunction =
            inversionForFunction chord desiredNoteFunction List.head

        let invertionWithLeadClosestToNote chord note =
            invertionWithNoteClosestToNote chord note lead

        let invertionWithBassClosestToNote chord note =
            invertionWithNoteClosestToNote chord note bass

    module ScaleHarmonizer =
        open Infrastructure
        open Domain
        open Chords

        let private thirds (fromPosition:ScaleDegrees) scale =
            let octave = 16

            scale
            |> circularSequenceFromList
            |> Seq.skip (int fromPosition)
            |> Seq.take octave
            |> filterOddIndexElements
            |> Seq.toList

        let private harmonizer forDegree scale =
            let thirdsList = scale |> thirds forDegree |> List.take 7
            {
                Notes = [(thirdsList.[0], Root);
                        (thirdsList.[1], Third);
                        (thirdsList.[2], Fifth);
                        (thirdsList.[3], Seventh);
                        (thirdsList.[4], Ninth);
                        (thirdsList.[5], Eleventh);
                        (thirdsList.[6], Thirteenth)];
                ChordType = Closed;
                Name =  ""
            }

        let private harmonizeScaleDegreeWithNotes forDegree scale notes =
            let complete = harmonizer forDegree scale
            let chord = {complete with Notes = complete.Notes |> List.take notes}
            {chord with Name = name chord}

        let private harmonize forDegree lastFunction scale =
            match lastFunction with
            | Seventh -> harmonizeScaleDegreeWithNotes forDegree scale 4
            | Ninth -> harmonizeScaleDegreeWithNotes forDegree scale 5
            | _ -> harmonizeScaleDegreeWithNotes forDegree scale 3

        let ninthsHarmonizer forDegree scale =
            harmonize forDegree Ninth scale

        let seventhsHarmonizer forDegree scale =
            harmonize forDegree Seventh scale

        let triadsHarmonizer forDegree scale =
            let complete = harmonize forDegree Fifth scale
            {complete with ChordType = Triad}

    module Guitar =
        module private GuitarFrets =
            open Infrastructure
            open Domain
            open Notes
            open Scales
            open ScaleHarmonizer

            let private isOpen fret =
                fret.Fret = 0

            let private isMuted fret =
                fret.Fret = -1

            let private isRaised fret =
                fret.Fret > 11

            let private fretDistance fret other =
                abs(fret - other)

            let private isStretched fret other =
                (fretDistance fret other) > 5

            let private raiseOctave fret =
                {fret with Fret = fret.Fret + (toDistance PerfectOctave)}

            let private isRaisable fret =
                not (isRaised fret) && not (isMuted fret)

            let raiseOpenFrets frets =
                frets
                |> List.map (fun fret -> if isOpen fret then raiseOctave fret else fret)

            let unstretch frets =
                let maxFret = frets |> List.map (fun f -> f.Fret) |> List.max
                frets
                |> List.map (fun f ->
                                        if isStretched f.Fret maxFret && isRaisable f
                                        then raiseOctave f
                                        else f)

        open Domain
        open Notes
        open Chords
        open Scales
        open ScaleHarmonizer
        open GuitarFrets
        open Infrastructure

        type private GuitarStringAttributes = {Name:string; OpenStringNote:Note; Index:int}

        let private guitarStringAttributes = function
            | SixthString -> { Name="Sixth"; OpenStringNote=E; Index=6}
            | FifthString -> { Name="Fifth"; OpenStringNote=A; Index=5}
            | FourthString -> { Name="Fourth"; OpenStringNote=D; Index=4}
            | ThirdString -> { Name="Third"; OpenStringNote=G; Index=3}
            | SecondString -> { Name="Second"; OpenStringNote=B; Index=2}
            | FirstString -> { Name="First"; OpenStringNote=E; Index=1}

        let private guitarStringOrdinal guitarString =
            (guitarStringAttributes guitarString).Index

        let private indexToGuitarString (nth:int) =
            match nth with
            | 6 -> SixthString
            | 5 -> FifthString
            | 4 -> FourthString
            | 3 -> ThirdString
            | 2 -> SecondString
            | _ -> FirstString

        let private openStringNote guitarString =
            (guitarStringAttributes guitarString).OpenStringNote

        let private nextString guitarString =
            indexToGuitarString ((guitarStringOrdinal guitarString) - 1)

        let private createMutedStringFret guitarString =
            let note = openStringNote guitarString
            { GuitarString = guitarString; Fret = -1; Note = note }

        let private findFretForNote note guitarString =
            measureAbsoluteSemitones (guitarStringAttributes guitarString).OpenStringNote note

        let private createFret guitarString note =
            { GuitarString = guitarString; Fret = findFretForNote note guitarString; Note = note }

        let private skipString bassString chord guitarString =
            chord.ChordType = Drop3 && guitarString = nextString bassString

        let private chordNotesExceptBass (chordNotes:ChordNotes) =
            chordNotes |> List.tail

        let private mapNoteToFret guitarString note shouldSkipString =
            if shouldSkipString then
                createMutedStringFret guitarString
            else
                createFret guitarString note

        let private remainingChordNotesToMap chordNotes shouldSkipString =
            if shouldSkipString then
                chordNotes
            else
                chordNotesExceptBass chordNotes

        let private mapChordToGuitarFrets bassString chord =
            let rec mapChordNoteToString guitarString chordNotes mappedChordNotes =
                match chordNotes with
                | [] -> mappedChordNotes
                | _ ->
                    let shouldSkipString = skipString bassString chord guitarString
                    let fret = mapNoteToFret guitarString (fst chordNotes.[0]) shouldSkipString
                    let unmapedChordNotes = remainingChordNotesToMap chordNotes shouldSkipString
                    mapChordNoteToString (nextString guitarString) unmapedChordNotes (fret::mappedChordNotes)
            mapChordNoteToString bassString chord.Notes []

        let private mapsChordNotesToFrets guitarStringIndex chord =
                [for chordNoteIndex in 0 .. (chord.Notes.Length - 1)
                        do yield (mapNoteToFret (indexToGuitarString guitarStringIndex) (fst chord.Notes.[chordNoteIndex]) false)]

        let private mapChordNotesToStrings bassString chord =
            [for guitarStringIndex in 1 .. (guitarStringOrdinal bassString)
                do yield (mapsChordNotesToFrets guitarStringIndex chord)]

        let private mapChordToOpen mappedChord =
            mappedChord
            |> allCombinations
            |> List.map (fun m -> (m, List.sumBy (fun f -> f.Fret) m) )
            |> List.minBy (fun l -> (snd l))
            |> fst

        let private mapChordToClosed mappedChord =
            mappedChord
            |> allCombinations
            |> List.map (fun m -> (m, List.sumBy (fun f -> f.Fret) m) )
            |> List.filter (fun l -> not( (fst l) |> List.exists (fun f -> f.Fret = 0) ))
            |> List.minBy (fun l -> (snd l))
            |> fst

        let private chordToGuitarOpenChord bassString chord =
            let frets = chord
                        |> mapChordNotesToStrings bassString
                        |> mapChordToOpen
            { Chord=chord; Frets= frets |> List.rev }

        let private nonDropChordToGuitarClosedChord bassString chord =
            let frets = chord
                        |> mapChordNotesToStrings bassString
                        |> mapChordToClosed
            { Chord=chord; Frets= frets |> List.rev }

        let private chordToGuitarChord bassString chord =
            { Chord=chord; Frets= mapChordToGuitarFrets bassString chord |> List.rev }

        let private chordToGuitarClosedChord bassString chord =
            let guitarChord = chordToGuitarChord bassString chord
            let closedChord = {guitarChord with Frets = raiseOpenFrets guitarChord.Frets}
            {closedChord with Frets = unstretch closedChord.Frets}

        let chordName guitarChord =
            guitarChord.Chord.Name

        let stringForLead guitarChord =
            (guitarChord.Frets |> List.last).GuitarString

        let stringForBass guitarChord =
            (guitarChord.Frets |> List.head).GuitarString

        let numberOfMutedHighStrings guitarChord =
            match stringForLead guitarChord with
                | SecondString -> 1
                | ThirdString -> 2
                | FourthString -> 3
                | _ -> 0

        let numberOfMutedLowStrings guitarChord =
            match stringForBass guitarChord with
            | FifthString -> 1
            | FourthString  -> 2
            | ThirdString  -> 3
            | _ -> 0

        let createGuitarChord bassString chord =
            match chord.ChordType with
            | Drop2 | Drop3 | Triad -> chordToGuitarClosedChord bassString chord
            | Open -> chordToGuitarOpenChord bassString chord
            | Closed ->
                if chord.Notes |> List.exists (fun n -> snd n = Ninth)
                then chordToGuitarClosedChord bassString chord
                else nonDropChordToGuitarClosedChord bassString chord

    module GuitarTab =
        open System
        open Domain
        open Notes
        open Chords
        open Guitar

        module private Tabify =

            type private TabColumn = string list

            type private TabColumns = TabColumn list

            type private TabLine = string list

            type private TabLines = TabLine list

            let private standardTunningTab = ["E"; "B"; "G"; "D"; "A"; "E"]

            let private startTab = "||-" |> List.replicate 6

            let private barTab = "-|-" |> List.replicate 6

            let private endTab =  ("-||" + Environment.NewLine) |> List.replicate 6

            let private chordNameLength guitarChord =
                (guitarChord |> chordName).Length

            let private tabifyMutedString guitarChord =
                String.replicate (guitarChord |> chordNameLength) "-"

            let private tabifyMutedHigherStrings mutedStringTab guitarChord =
                let mutedStrings = numberOfMutedHighStrings guitarChord
                List.replicate mutedStrings mutedStringTab

            let private tabifyMutedLowerStrings mutedStringTab guitarChord =
                let mutedStrings = numberOfMutedLowStrings guitarChord
                List.replicate mutedStrings mutedStringTab

            let private tabifyFret mutedStringTab fret =
                if fret.Fret = -1 then
                    sprintf "%s" mutedStringTab
                else
                    sprintf "%i%s" fret.Fret (mutedStringTab.[(string(fret.Fret)).Length..])

            let private tabifyFrets mutedStringTab guitarChord =
                guitarChord.Frets
                |> List.map (fun fret -> tabifyFret mutedStringTab fret)
                |> List.rev

            let private convertTabColumsToTabLines stringOrdinal (tabifiedChords: TabColumns) =
                tabifiedChords |> List.map (fun f -> f.[stringOrdinal])

            let private convertColumnChordsToGuitarStringLines (tabifiedChords: TabColumns) =
                [0 .. 5]
                |> List.map (fun stringOrdinal -> convertTabColumsToTabLines stringOrdinal tabifiedChords)

            let private renderGuitarStringLine (tabifiedFretsForString:TabLine) =
                tabifiedFretsForString
                |> List.fold (fun acc fret -> acc + fret + "---" ) "---"

            let private renderGuitarStringLines (tabifiedFretsForStrings:TabLines) =
                tabifiedFretsForStrings
                |> List.map renderGuitarStringLine

            let private renderTabifiedChords tabifiedChords =
                tabifiedChords
                |> List.mapi (fun guitarStringNumber tabifiedGuitarString ->
                                standardTunningTab.[guitarStringNumber] +
                                startTab.[guitarStringNumber] +
                                tabifiedGuitarString +
                                endTab.[guitarStringNumber])

            let private tabifyChord guitarChord =
                let mutedStringTab = tabifyMutedString guitarChord
                (tabifyMutedHigherStrings mutedStringTab guitarChord)
                @ (tabifyFrets mutedStringTab guitarChord)
                @ (tabifyMutedLowerStrings mutedStringTab guitarChord)

            let tabifyChords guitarChords =
                guitarChords
                |> List.map tabifyChord
                |> convertColumnChordsToGuitarStringLines
                |> renderGuitarStringLines
                |> renderTabifiedChords

            let tabifyChordNames guitarChords =
                let chordNameSeparator = "   "
                let separatedChordNames =
                    guitarChords |> List.map (fun guitarChord -> chordNameSeparator + guitarChord.Chord.Name)
                [chordNameSeparator] @ separatedChordNames @ [chordNameSeparator; Environment.NewLine;]

        module private Shapify =
            let private shapifyMutedLowerStrings guitarChord =
                let mutedStrings = numberOfMutedLowStrings guitarChord
                List.replicate mutedStrings "X"

            let private shapifyMutedHigherStrings guitarChord =
                let mutedStrings = numberOfMutedHighStrings guitarChord
                List.replicate mutedStrings "X"

            let private shapifyFret fret =
                if fret.Fret = -1 then
                    "X"
                else
                    sprintf "%i" fret.Fret

            let private shapifyFrets guitarChord =
                guitarChord.Frets |> List.map shapifyFret

            let shapifyChord guitarChord =
                (shapifyMutedLowerStrings guitarChord)
                @ (shapifyFrets guitarChord)
                @ (shapifyMutedHigherStrings guitarChord)

        open Shapify
        open Tabify

        let tabifyAll guitarChords =
            (tabifyChordNames guitarChords) @ (tabifyChords guitarChords)
            |> List.fold (+) ""

        let tabify guitarChord =
            tabifyAll [guitarChord]

        let shapify guitarChord =
            guitarChord.Chord.Name + Environment.NewLine +
            "EADGBE" + Environment.NewLine +
            (guitarChord |> shapifyChord |> List.fold (+) "") + Environment.NewLine

    module SpeechToMusic =
        open FParsec
        open Domain
        open Notes
        open Chords

        type private UserState = unit
        type private Parser<'t> = Parser<'t, UserState>

        let private skip parser skiped = parser .>> skiped

        let private skipSpaces parser = skip parser spaces

        let private any parsers = parsers |> List.reduce (<|>)

        let private parseNote: Parser<_> =
            any [
                    (stringCIReturn "a" A);
                    (stringCIReturn "b" B);
                    (stringCIReturn "c" C);
                    (stringCIReturn "d" D);
                    (stringCIReturn "e" E);
                    (stringCIReturn "f" F);
                    (stringCIReturn "g" G)
                ] |> skipSpaces

        let private parseAccident: Parser<_> =
            any [
                    (stringReturn "#" sharp);
                    (stringReturn "b" flat);
                    (notFollowedByString "#" >>% natural);
                    (notFollowedByString "b" >>% natural)
                ] |> skipSpaces

        let private parseMajorQuality: Parser<_> =
            any [
                    (stringCIReturn "major" Major)
                    (stringCIReturn "maj" Major)
                    (stringReturn "M" Major)
                ] |> skipSpaces

        let private parseMinorQuality: Parser<_> =
            any [
                    (stringCIReturn "minor" Minor);
                    (stringCIReturn "min" Minor);
                    (stringReturn "m" Minor);
                ] |> skipSpaces

        let private parseAugmentedQuality: Parser<_> =
             any [
                    (stringCIReturn "augmented" Augmented);
                    (stringCIReturn "aug" Augmented)
                 ] |> skipSpaces

        let private parseDiminishedQuality: Parser<_> =
            any [
                    (stringCIReturn "diminished" Diminished);
                    (stringCIReturn "dim" Diminished)
                ] |> skipSpaces

        let private parseDominantQuality: Parser<_> =
            any [
                    (stringCIReturn "7" Dominant7);
                    (stringCIReturn "7th" Dominant7);
                    (stringCIReturn "seventh" Dominant7);
                    (stringCIReturn "seven" Dominant7);
                    (stringCIReturn "dominant" Dominant7);
                    (stringCIReturn "dom" Dominant7)
                ] |> skipSpaces

        let private parseQuality: Parser<_> =
            any [
                    parseMajorQuality
                    parseMinorQuality
                    parseAugmentedQuality
                    parseDiminishedQuality
                    parseDominantQuality
                ] |> skipSpaces

        let private updateChordIntentWithSeventhQuality chord =
            match chord with
            | {Quality=Major} -> { chord with Quality = Major7 }
            | {Quality=Minor} -> { chord with Quality = Minor7 }
            | {Quality=Diminished} -> { chord with Quality = Diminished7 }
            | {Quality=Augmented} -> { chord with Quality = Augmented7 }
            | {Quality=_} -> { chord with Quality = Dominant7 }

        let private parseSeventhQuality: Parser<_> =
            any [
                    (stringCIReturn "7" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "7th" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "seventh" updateChordIntentWithSeventhQuality);
                    (stringCIReturn "seven" updateChordIntentWithSeventhQuality);
                    (notFollowedByString "7" >>% id);
                    (notFollowedByString "7th" >>% id);
                    (notFollowedByString "seventh" >>% id);
                    (notFollowedByString "seven" >>% id)
                ] |> skipSpaces

        let private rootParser: Parser<_> =
            pipe2 parseNote parseAccident
                (fun note applyAccidentToNote -> applyAccidentToNote note)

        let private triadParser: Parser<_> =
            pipe2 rootParser parseQuality
                (fun r q -> {Root=r; Quality=q;})

        let private seventhChordParser: Parser<_> =
            pipe2 triadParser parseSeventhQuality
                (fun triad seventhQualityUpdater -> seventhQualityUpdater triad)

        let private chordParser: Parser<_> =
            any [
                    seventhChordParser
                    triadParser;
                ]

        let parseChord (text:string) =
            let parsed = run chordParser text
            match parsed with
            | Success(chordDefinition, _, _) -> chordDefinition
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let createChord chordIntent =
            chord chordIntent.Root chordIntent.Quality