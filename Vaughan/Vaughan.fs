namespace Vaughan

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

    module Notes =
        type Note = | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp 
                    | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B
                    
        type Interval = | Unisson | MinorSecond | MajorSecond | AugmentedSecond | MinorThird
                        | MajorThird | PerfectForth | AugmentedForth | DiminishedFifth
                        | PerfectFifth | AugmentedFifth | MinorSixth | MajorSixth
                        | MinorSeventh | MajorSeventh | PerfectOctave
        
        type private NoteAttributes = {Name:string; Sharp:Note; Flat:Note; Pitch:int}
        type private IntervalAttributes = {Name:string; Distance:int}

        let private noteAttributes note =
            match note with
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

        let noteName note =
            (noteAttributes note).Name
                
        let sharp note =
            (noteAttributes note).Sharp

        let flat note =
            (noteAttributes note).Flat
            
        let pitch note =
            (noteAttributes note).Pitch

        let private intervalAttributes interval =
            match interval with
            | Unisson -> {Name="Unisson"; Distance=0} 
            | MinorSecond -> {Name="MinorSecond"; Distance=1} 
            | MajorSecond -> {Name="MajorSecond"; Distance=2} 
            | AugmentedSecond -> {Name="AugmentedSecond"; Distance=3} 
            | MinorThird -> {Name="MinorThird"; Distance=3} 
            | MajorThird -> {Name="MajorThird"; Distance=4} 
            | PerfectForth -> {Name="PerfectForth"; Distance=5} 
            | AugmentedForth -> {Name="AugmentedForth"; Distance=6} 
            | DiminishedFifth -> {Name="DiminishedFifth"; Distance=6} 
            | PerfectFifth -> {Name="PerfectFifth"; Distance=7} 
            | AugmentedFifth -> {Name="AugmentedFifth"; Distance=8} 
            | MinorSixth -> {Name="MinorSixth"; Distance=8} 
            | MajorSixth -> {Name="MajorSixth"; Distance=9} 
            | MinorSeventh -> {Name="MinorSeventh"; Distance=10} 
            | MajorSeventh -> {Name="MajorSeventh"; Distance=11} 
            | PerfectOctave -> {Name="PerfectOctave"; Distance=12} 

        let intervalName interval =
            (intervalAttributes interval).Name
        
        let toDistance interval =
            (intervalAttributes interval).Distance
            
        let fromDistance distance =
            match distance with
            | 0 -> Unisson
            | 1 -> MinorSecond
            | 2 -> MajorSecond
            | 3 -> MinorThird
            | 4 -> MajorThird
            | 5 -> PerfectForth
            | 6 -> DiminishedFifth
            | 7 -> PerfectFifth
            | 8 -> AugmentedFifth
            | 9 -> MajorSixth
            | 10 -> MinorSeventh
            | 11 -> MajorSeventh
            | 12 -> PerfectOctave
            | _ -> Unisson
        
        let measureAbsoluteSemitones note other =
            let distance = (pitch other) - (pitch note)
            if distance < (toDistance Unisson) 
            then (toDistance PerfectOctave) - distance * -1 
            else distance    
                
        let private transposeDirection note interval =
            match interval with
            | Unisson -> note 
            | MajorSecond | AugmentedSecond | PerfectFifth | MajorThird | PerfectForth
            | AugmentedFifth | MajorSixth | PerfectOctave | AugmentedForth -> sharp note
            | MinorSecond | DiminishedFifth | MinorThird
            | MinorSixth | MinorSeventh | MajorSeventh  -> flat note
            
        let intervalBetween note other =
            fromDistance (measureAbsoluteSemitones note other)
            
        let transpose noteToTranspose transposingInterval =
            let rec loop note interval =
                let newNote = transposeDirection note transposingInterval
                let newInterval = intervalBetween noteToTranspose newNote
                
                if toDistance newInterval = toDistance transposingInterval then
                    newNote
                else
                    loop newNote newInterval
                
            loop noteToTranspose transposingInterval

    module Scales =
        open Notes

        type Scales = 
            | Ionian | Dorian | Phrygian | Lydian | Mixolydian
            | Aolian | Locrian | MajorPentatonic | MinorPentatonic
            | Blues | HarmonicMinor | MelodicMinor | Dorianb2 | LydianAugmented
            | LydianDominant | Mixolydianb6 | LocrianSharp2
            | AlteredDominant | HalfWholeDiminished | WholeTone

        let formula scale =
            match scale with
            | Ionian -> [Unisson; MajorSecond; MajorThird; PerfectForth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorian -> [Unisson; MajorSecond; MinorThird; PerfectForth; PerfectFifth; MajorSixth; MinorSeventh]
            | Phrygian -> [Unisson; MinorSecond; MinorThird; PerfectForth; PerfectFifth; MinorSixth; MinorSeventh]
            | Lydian -> [Unisson; MajorSecond; MajorThird; AugmentedForth; PerfectFifth; MajorSixth; MajorSeventh]
            | Mixolydian -> [Unisson; MajorSecond; MajorThird; PerfectForth; PerfectFifth; MajorSixth; MinorSeventh]
            | Aolian -> [Unisson; MajorSecond; MinorThird; PerfectForth; PerfectFifth; MinorSixth; MinorSeventh]
            | Locrian -> [Unisson; MinorSecond; MinorThird; PerfectForth; DiminishedFifth; MinorSixth; MinorSeventh]
            | MajorPentatonic -> [Unisson; MajorSecond; MajorThird; PerfectFifth; MajorSixth]
            | MinorPentatonic -> [Unisson; MinorThird; PerfectForth; PerfectFifth; MinorSeventh]
            | Blues -> [Unisson; MinorThird; PerfectForth; DiminishedFifth; PerfectFifth; MinorSeventh]
            | HarmonicMinor -> [Unisson; MajorSecond; MinorThird; PerfectForth; PerfectFifth; MinorSixth; MajorSeventh]
            | MelodicMinor -> [Unisson; MajorSecond; MinorThird; PerfectForth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorianb2 -> [Unisson; MinorSecond; MinorThird; PerfectForth; PerfectFifth; MajorSixth; MinorSeventh]
            | LydianAugmented -> [Unisson; MajorSecond; MajorThird; AugmentedForth; AugmentedFifth; MajorSixth; MajorSeventh]
            | LydianDominant -> [Unisson; MajorSecond; MajorThird; AugmentedForth; PerfectFifth; MajorSixth; MinorSeventh]
            | Mixolydianb6 -> [Unisson; MajorSecond; MajorThird; PerfectForth; PerfectFifth; MinorSixth; MinorSeventh]
            | LocrianSharp2 -> [Unisson; MajorSecond; MinorThird; PerfectForth; DiminishedFifth; MinorSixth; MinorSeventh]
            | AlteredDominant -> [Unisson; MinorSecond; AugmentedSecond; MajorThird; DiminishedFifth;  AugmentedFifth; MinorSeventh]
            | HalfWholeDiminished -> [Unisson; MinorSecond; MinorThird; MajorThird; AugmentedForth;  PerfectFifth; MajorSixth; MinorSeventh]
            | WholeTone -> [Unisson; MajorSecond; MajorThird; DiminishedFifth; AugmentedFifth; MinorSeventh]
        
        let createScale scale root = 
            formula scale |> List.map (fun interval -> transpose root interval)

    module Keys =
        open Notes

        type Key = 
            | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
            | DMajor | DFlatMajor | EMajor | EFlatMajor
            | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
            | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
            | EMinor | FMinor | FSharpMinor | GMinor 
            | GSharpMinor | EFlatMinor

        type private KeyAttributes = {Root:Note; Accidentals:int}

        let private keyAttributes key =
            match key with
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

        let root key =
            (keyAttributes key).Root

        let private accidentals key =
            (keyAttributes key).Accidentals
        
        let private flatedKey fifths keyAccidents =
            (fifths |> List.rev |> List.skip -keyAccidents) 
            @ (fifths
            |> List.rev
            |> List.take(-keyAccidents)
            |> List.map flat)
        
        let private sharpedKey fifths keyAccidents =
            ((fifths |> List.skip keyAccidents) )
            @ (fifths
            |> List.take(keyAccidents)
            |> List.map sharp)
        
        let private rawNotes scale = 
            let fifths = [F; C; G; D; A; E; B;]
            let keyAccidents = accidentals scale
            
            if keyAccidents = 0 then
                fifths
            else 
                if keyAccidents < 0 then 
                    flatedKey fifths keyAccidents
                else
                    sharpedKey fifths keyAccidents

        let notes scale = 
            (rawNotes scale
            |> List.sortBy pitch
            |> List.skipWhile (fun n -> n <> root scale))
            @
            (rawNotes scale
            |> List.sortBy pitch
            |> List.takeWhile (fun n -> n <> root scale))  

    module Chords =
        open Notes
        open Infrastructure

        type ChordFunction = 
            | Major | Augmented | Minor | Diminished
            | Major7 | Augmented7 | Minor7 | Diminished7 
            | Dominant7 | Minor7b5 | MinorMaj7
            | Sus2 | Sus2Diminished | Sus2Augmented
            | Sus4 | Sus4Diminished | Sus4Augmented
            
        type ChordNoteFunction = | Root | Third | Fifth | Seventh | Ninth | Eleventh | Thirteenth
        type ChordType = | Open | Closed | Drop2 | Drop3
        type ChordNote = Note * ChordNoteFunction

        type Chord = {notes:ChordNote list; chordType:ChordType;}
        
        let functionForInterval interval =
            match interval with
            | Unisson -> Root
            | MajorThird | MinorThird -> Third 
            | PerfectFifth | DiminishedFifth | AugmentedFifth  -> Fifth
            | MajorSeventh | MinorSeventh | MajorSixth -> Seventh
            | _ -> Root
            
        let intervalsForFunction chordFunction =
            match chordFunction with
            | Major -> [MajorThird; PerfectFifth]
            | Augmented -> [MajorThird; AugmentedFifth]
            | Minor -> [MinorThird; PerfectFifth]
            | Diminished -> [MinorThird; DiminishedFifth]
            | Major7 -> [MajorThird; PerfectFifth; MajorSeventh]
            | Augmented7 -> [MajorThird; AugmentedFifth; MajorSeventh]
            | Minor7 -> [MinorThird; PerfectFifth; MinorSeventh]
            | Diminished7 -> [MinorThird; DiminishedFifth; MajorSixth]
            | Dominant7 -> [MajorThird; PerfectFifth; MinorSeventh]
            | Minor7b5 -> [MinorThird; DiminishedFifth; MinorSeventh]
            | MinorMaj7 -> [MinorThird; PerfectFifth; MajorSeventh]
            | Sus2 -> [MajorSecond; PerfectFifth]
            | Sus2Diminished -> [MajorSecond; DiminishedFifth]
            | Sus2Augmented -> [MajorSecond; AugmentedFifth]
            | Sus4 -> [PerfectForth; PerfectFifth]
            | Sus4Diminished -> [PerfectForth; DiminishedFifth]
            | Sus4Augmented -> [PerfectForth; AugmentedFifth]

        let functionForIntervals intervals =
            match intervals with
            | [MajorThird; PerfectFifth] -> Major
            | [MajorThird; AugmentedFifth] -> Augmented
            | [MinorThird; PerfectFifth] -> Minor
            | [MinorThird; DiminishedFifth] -> Diminished
            | [MajorThird; PerfectFifth; MajorSeventh] -> Major7
            | [MajorThird; AugmentedFifth; MajorSeventh] -> Augmented7
            | [MinorThird; PerfectFifth; MinorSeventh] -> Minor7
            | [MinorThird; DiminishedFifth; MajorSixth] -> Diminished7
            | [MajorThird; PerfectFifth; MinorSeventh] -> Dominant7
            | [MinorThird; DiminishedFifth; MinorSeventh] -> Minor7b5
            | [MinorThird; PerfectFifth; MajorSeventh] -> MinorMaj7
            | [MajorSecond; PerfectFifth] -> Sus2
            | [MajorSecond; DiminishedFifth] -> Sus2Diminished 
            | [MajorSecond; AugmentedFifth] -> Sus2Augmented
            | [PerfectForth; PerfectFifth] -> Sus4
            | [PerfectForth; DiminishedFifth] -> Sus4Diminished
            | [PerfectForth; AugmentedFifth] -> Sus4Augmented
            | _ -> Major

        let abreviatedName chordFunction =
            match chordFunction with
            | Major -> "Maj" | Augmented -> "Aug" | Minor -> "Min" 
            | Diminished -> "Dim" | Major7 -> "Maj7" 
            | Augmented7 -> "Aug7" | Minor7 -> "Min7" 
            | Diminished7 -> "Dim7" | Dominant7 -> "Dom7" 
            | Minor7b5 -> "Min7b5" | MinorMaj7 -> "MinMaj7"
            | Sus2 -> "Sus2" | Sus2Diminished -> "Sus2Dim" 
            | Sus2Augmented -> "Sus2Aug"
            | Sus4 -> "Sus4" | Sus4Diminished -> "SusDim" 
            | Sus4Augmented -> "Sus4Aug"

        let note chordNote =
            fst chordNote

        let noteFunction chordNote =
            snd chordNote

        let noteForFunction chord chordNoteFunction =
            note (chord.notes |> List.find (fun n -> noteFunction n = chordNoteFunction))
        
        let bass chord =
            note (chord.notes |> List.head)
        
        let lead chord =
            note (chord.notes |> List.last)
        
        let intervalsForChord chord =
            let root = noteForFunction chord Root
            chord.notes
            |> List.map (fun n -> intervalBetween root (note n))
            |> List.sortBy toDistance
            |> List.skip 1
            
        let name chord =
            noteName (noteForFunction chord Root) 
            + abreviatedName (functionForIntervals(intervalsForChord chord))
            
        let noteNames chord =
            chord.notes |> List.map (fun n -> noteName (note n))
            
        let chordFromRootAndFunction root chordFunction =
            {notes=
                [(root, Root)]@
                (intervalsForFunction chordFunction
                |> List.map (fun i -> ((transpose root i), functionForInterval i)));
             chordType = Closed}      

        let invertOpenOrClosed chord =
            {notes= rotateByOne chord.notes; chordType=chord.chordType}

        let invertDrop2 chord = 
            {
                notes= [chord.notes |> List.last] 
                    @ (chord.notes 
                        |> List.take (chord.notes.Length - 1) 
                        |> rotateByOne 
                        |> rotateByOne) 
                chordType=chord.chordType
             }     

        let invertDrop3 chord =
            {notes= chord.notes |> rotateByOne |> rotateByOne |> swapSecondTwo; chordType=chord.chordType}

        let invert chord =
            match chord.chordType with
            | Closed | Open -> invertOpenOrClosed chord
            | Drop2 -> invertDrop2 chord
            | Drop3 -> invertDrop3 chord

        let toDrop2 chord =
            {notes= chord.notes |> swapFirstTwo |> rotateByOne; chordType=Drop2}

        let toDrop3 chord =
            {notes= (chord |> toDrop2 |> toDrop2).notes; chordType=Drop3}

    module ScaleHarmonizer =
        open Infrastructure
        open Chords
        open Scales
        open Notes
        
        type ScaleDgrees = | I = 0 | II = 1 | III = 2 | IV = 3 | V = 4 | VI = 5 | VII = 6

        let thirds fromPosition scale =
            scale 
            |> circularSequenceFromList
            |> Seq.skip (int fromPosition)
            |> Seq.take 7 
            |> Seq.mapi (fun i v -> i, v)
            |> Seq.filter (fun (i, v) -> i % 2 = 0)
            |> Seq.map snd
            |> Seq.toList

        let seventhsHarmonizer forDegree scale =
            let thirdsList = 
                scale
                |> thirds forDegree
                |> List.take 4
            
            {notes= [(thirdsList.[0], Root); 
                     (thirdsList.[1] , Third); 
                     (thirdsList.[2], Fifth); 
                     (thirdsList.[3], Seventh)]; 
             chordType = Closed}

        let triadsHarmonizer forDegree scale =
            let seventh = seventhsHarmonizer forDegree scale
            {seventh with notes = seventh.notes |> List.take 3}

    module InfrastructureTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Infrastructure

        [<Test>]
        let ``Should rotate list``() =
            test <@ rotateByOne [] |> List.isEmpty @>
            test <@ rotateByOne [1] = [1] @>
            test <@ rotateByOne [1; 2] = [2; 1] @>
            test <@ rotateByOne [1; 2; 3] = [2; 3; 1] @>

        [<Test>]
        let ``Should swap first 2 elements in list``() =
            test <@ swapFirstTwo [1; 2; 3] = [2; 1; 3] @>

        [<Test>]
        let ``Should swap second 2 elements in list``() =
            test <@ swapSecondTwo [1; 2; 3] = [1; 3; 2] @>

        [<Test>]
        let ``Should create circular sequence from list``() =
            test <@ circularSequenceFromList [1; 2; 3] |> Seq.take 3 |> Seq.last = 3 @>
            test <@ circularSequenceFromList [1; 2; 3] |> Seq.take 4 |> Seq.last = 1 @>
            test <@ circularSequenceFromList [1; 2; 3] |> Seq.take 5 |> Seq.last = 2 @>
        
    module NotesTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Notes

        [<Test>]
        let ``Should relate note with its name``() =
            test <@ noteName C = "C" @>
            test <@ noteName CSharp = "C#" @>
            test <@ noteName DFlat = "Db" @>
            test <@ noteName D = "D" @>
            test <@ noteName DSharp = "D#" @>
            test <@ noteName EFlat = "Eb" @>
            test <@ noteName E = "E" @>
            test <@ noteName F = "F" @>
            test <@ noteName FSharp = "F#" @>
            test <@ noteName GFlat = "Gb" @>
            test <@ noteName G = "G" @>
            test <@ noteName GSharp = "G#" @>
            test <@ noteName AFlat = "Ab" @>
            test <@ noteName A = "A" @>
            test <@ noteName ASharp = "A#" @>
            test <@ noteName BFlat = "Bb" @>
            test <@ noteName B = "B" @>
            
        [<Test>]
        let ``Should sharp note``() =
            test <@ sharp C = CSharp @>
            test <@ sharp CSharp = D @>
            test <@ sharp DFlat = D @>
            test <@ sharp D = DSharp @>
            test <@ sharp DSharp = E @>
            test <@ sharp EFlat = E @>
            test <@ sharp E = F @>
            test <@ sharp F = FSharp @>
            test <@ sharp FSharp = G @>
            test <@ sharp GFlat = G @>
            test <@ sharp G = GSharp @>
            test <@ sharp GSharp = A @>
            test <@ sharp AFlat = A @>
            test <@ sharp A = ASharp @>
            test <@ sharp ASharp = B @>
            test <@ sharp BFlat = B @>
            test <@ sharp B = C @>
            
        [<Test>]
        let ``Should flat note``() =
            test <@ flat C = B @>
            test <@ flat CSharp = C @>
            test <@ flat DFlat = C @>
            test <@ flat D = DFlat @>
            test <@ flat DSharp = D @>
            test <@ flat EFlat = D @>
            test <@ flat E = EFlat @>
            test <@ flat F = E @>
            test <@ flat FSharp = F @>
            test <@ flat GFlat = F @>
            test <@ flat G = GFlat @>
            test <@ flat GSharp = G @>
            test <@ flat AFlat = G @>
            test <@ flat A = AFlat @>
            test <@ flat ASharp = A @>
            test <@ flat BFlat = A @>
            test <@ flat B = BFlat @>
            
        [<Test>]
        let ``Should measure semitones distance``() =
            test <@ measureAbsoluteSemitones C C = 0 @>
            test <@ measureAbsoluteSemitones C CSharp = 1 @>
            test <@ measureAbsoluteSemitones C DFlat = 1 @>
            test <@ measureAbsoluteSemitones C D = 2 @>
            test <@ measureAbsoluteSemitones C DSharp = 3 @>
            test <@ measureAbsoluteSemitones C EFlat = 3 @>
            test <@ measureAbsoluteSemitones C E = 4 @>
            test <@ measureAbsoluteSemitones C F = 5 @>
            test <@ measureAbsoluteSemitones C FSharp = 6 @>
            test <@ measureAbsoluteSemitones C GFlat = 6 @>
            test <@ measureAbsoluteSemitones C G = 7 @>
            test <@ measureAbsoluteSemitones C GSharp = 8 @>
            test <@ measureAbsoluteSemitones C AFlat = 8 @>
            test <@ measureAbsoluteSemitones C A = 9 @>
            test <@ measureAbsoluteSemitones C ASharp = 10 @>
            test <@ measureAbsoluteSemitones C BFlat = 10 @>
            test <@ measureAbsoluteSemitones C B = 11 @>
            
        [<Test>]
        let ``Should create interval from distance``() =
            test <@ intervalBetween C C = Unisson @>
            test <@ intervalBetween C CSharp = MinorSecond @>
            test <@ intervalBetween C DFlat = MinorSecond @>
            test <@ intervalBetween C D = MajorSecond @>
            test <@ intervalBetween C DSharp = MinorThird @>
            test <@ intervalBetween C EFlat = MinorThird @>
            test <@ intervalBetween C E = MajorThird @>
            test <@ intervalBetween C F = PerfectForth @>
            test <@ intervalBetween C FSharp = DiminishedFifth @>
            test <@ intervalBetween C GFlat = DiminishedFifth @>
            test <@ intervalBetween C G = PerfectFifth @>
            test <@ intervalBetween C GSharp = AugmentedFifth @>
            test <@ intervalBetween C AFlat = AugmentedFifth @>
            test <@ intervalBetween C A = MajorSixth @>
            test <@ intervalBetween C ASharp = MinorSeventh @>
            test <@ intervalBetween C BFlat = MinorSeventh @>
            test <@ intervalBetween C B = MajorSeventh @>
            
        [<Test>]
        let ``Should transpose note using interval``() =
            test <@ transpose C Unisson = C @>
            test <@ transpose C MinorSecond = DFlat @>
            test <@ transpose C MajorSecond = D @>
            test <@ transpose C MinorThird = EFlat @>
            test <@ transpose C MajorThird = E @>
            test <@ transpose C PerfectForth = F @>
            test <@ transpose C DiminishedFifth = GFlat @>
            test <@ transpose C PerfectFifth = G @>
            test <@ transpose C AugmentedFifth = GSharp @>
            test <@ transpose C MajorSixth = A @>
            test <@ transpose C MinorSeventh = BFlat @>
            test <@ transpose C MajorSeventh = B @>
            
        [<Test>]
        let ``Should relate interval with its name``() =
            test <@ intervalName Unisson = "Unisson" @>
            test <@ intervalName MinorSecond = "MinorSecond" @>
            test <@ intervalName MajorSecond = "MajorSecond" @>
            test <@ intervalName MinorThird = "MinorThird" @>
            test <@ intervalName MajorThird = "MajorThird" @>
            test <@ intervalName PerfectForth = "PerfectForth" @>
            test <@ intervalName DiminishedFifth = "DiminishedFifth" @>
            test <@ intervalName PerfectFifth = "PerfectFifth" @>
            test <@ intervalName AugmentedFifth = "AugmentedFifth" @>
            test <@ intervalName MajorSixth = "MajorSixth" @>
            test <@ intervalName MinorSeventh = "MinorSeventh" @>
            test <@ intervalName MajorSeventh = "MajorSeventh" @>
            test <@ intervalName PerfectOctave = "PerfectOctave" @>
            
        [<Test>]
        let ``Should relate interval with distances``() =
            test <@ fromDistance 0 = Unisson @>
            test <@ fromDistance 1 = MinorSecond @>
            test <@ fromDistance 2 = MajorSecond @>
            test <@ fromDistance 3 = MinorThird @>
            test <@ fromDistance 4 = MajorThird @>
            test <@ fromDistance 5 = PerfectForth @>
            test <@ fromDistance 6 = DiminishedFifth @>
            test <@ fromDistance 7 = PerfectFifth @>
            test <@ fromDistance 8 = AugmentedFifth @>
            test <@ fromDistance 9 = MajorSixth @>
            test <@ fromDistance 10 = MinorSeventh @>
            test <@ fromDistance 11 = MajorSeventh @>
            test <@ fromDistance 12 = PerfectOctave @>

     module KeyTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Notes
        open Keys

        [<Test>]
        let ``Should have notes for key``() =
            test <@ notes CMajor = [ C; D; E; F; G; A; B ] @>
            test <@ notes GMajor = [ G; A; B; C; D; E; FSharp ] @>
            test <@ notes DMajor = [ D; E; FSharp; G; A; B; CSharp ] @>
            test <@ notes AMajor = [ A; B; CSharp; D; E; FSharp; GSharp ] @>
            test <@ notes EMajor = [ E; FSharp; GSharp; A; B; CSharp; DSharp ] @>
            test <@ notes BMajor = [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ] @>
            test <@ notes FSharpMajor = [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ] @>
            test <@ notes DFlatMajor = [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ] @>
            test <@ notes AFlatMajor = [ AFlat; BFlat; C; DFlat; EFlat; F; G ] @>
            test <@ notes GFlatMajor = [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ] @>
            test <@ notes EFlatMajor = [ EFlat; F; G; AFlat; BFlat; C; D ] @>
            test <@ notes BFlatMajor = [ BFlat; C; D; EFlat; F; G; A ] @>
            test <@ notes FMajor = [ F; G; A; BFlat; C; D; E ] @>

            test <@ notes AMinor = [ A; B; C; D; E; F; G ] @>
            test <@ notes EMinor = [ E; FSharp; G; A; B; C; D ] @>
            test <@ notes BMinor = [ B; CSharp; D; E; FSharp; G; A ] @>
            test <@ notes FSharpMinor = [ FSharp; GSharp; A; B; CSharp; D; E ] @>
            test <@ notes CSharpMinor = [ CSharp; DSharp; E; FSharp; GSharp; A; B ] @>
            test <@ notes GSharpMinor = [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ] @>
            test <@ notes EFlatMinor = [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ] @>
            test <@ notes BFlatMinor = [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ] @>
            test <@ notes FMinor = [ F; G; AFlat; BFlat; C; DFlat; EFlat ] @>
            test <@ notes CMinor = [ C; D; EFlat; F; G; AFlat; BFlat ] @>
            test <@ notes GMinor = [ G; A; BFlat; C; D; EFlat; F ] @>
            test <@ notes DMinor = [ D; E; F; G; A; BFlat; C ] @>

    module ScaleTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Notes
        open Scales

        [<Test>]
        let ``Should have notes for scales``() =
            test <@ createScale Ionian C = [ C; D; E; F; G; A; B ] @>
            test <@ createScale Dorian C = [ C; D; EFlat; F; G; A; BFlat ] @>
            test <@ createScale Phrygian C = [ C; DFlat; EFlat; F; G; AFlat; BFlat ] @>
            test <@ createScale Lydian C = [ C; D; E; FSharp; G; A; B ] @>
            test <@ createScale Mixolydian C = [ C; D; E; F; G; A; BFlat ] @>
            test <@ createScale Aolian C = [ C; D; EFlat; F; G; AFlat; BFlat ] @>
            test <@ createScale Locrian C = [ C; DFlat; EFlat; F; GFlat; AFlat; BFlat ] @>
            test <@ createScale MajorPentatonic C = [ C; D; E; G; A;] @>
            test <@ createScale MinorPentatonic C = [ C; EFlat; F; G; BFlat ] @>
            test <@ createScale Blues C = [ C; EFlat; F; GFlat; G; BFlat ] @>
            test <@ createScale HarmonicMinor C = [ C; D; EFlat; F; G; AFlat; B ] @>
            test <@ createScale MelodicMinor C = [ C; D; EFlat; F; G; A; B ] @>
            test <@ createScale Dorianb2 C = [ C; DFlat; EFlat; F; G; A; BFlat ] @>
            test <@ createScale LydianAugmented C = [ C; D; E; FSharp; GSharp; A; B ] @>
            test <@ createScale LydianDominant C = [ C; D; E; FSharp; G; A; BFlat ] @>
            test <@ createScale Mixolydianb6 C = [ C; D; E; F; G; AFlat; BFlat ] @>
            test <@ createScale LocrianSharp2 C = [ C; D; EFlat; F; GFlat; AFlat; BFlat ] @>
            test <@ createScale AlteredDominant C = [ C; DFlat; DSharp; E; GFlat; GSharp; BFlat ] @>
            test <@ createScale HalfWholeDiminished C = [ C; DFlat; EFlat; E; FSharp; G; A; BFlat ] @>
            test <@ createScale WholeTone C = [ C; D; E; GFlat; GSharp; BFlat ] @> 

    module ChordsTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Scales
        open Notes
        open Chords

        let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
        let cAug = {notes= [(C, Root); (E, Third); (GSharp, Fifth)]; chordType=Closed}
        let cMin = {notes= [(C, Root); (EFlat, Third); (G, Fifth)]; chordType=Closed}
        let cDim = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth)]; chordType=Closed}
        let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
        let cAug7 = {notes= [(C, Root); (E, Third); (GSharp, Fifth); (B, Seventh)];chordType=Closed}
        let cMin7 = {notes= [(C, Root); (EFlat, Third); (G, Fifth); (BFlat, Seventh)]; chordType=Closed}
        let cDim7 = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (A, Seventh)]; chordType=Closed}
        let cMin7b5 = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (BFlat, Seventh)]; chordType=Closed}
                
        [<Test>]
        let ``Chord should have notes for function``() =
            test <@ noteForFunction cMaj7 Root = C @>
            test <@ noteForFunction cAug Third = E @>
            test <@ noteForFunction cDim Fifth = GFlat @>
            test <@ noteForFunction cMin7b5 Seventh = BFlat @>
            
        [<Test>]
        let ``Chord should return note names``() =
            test <@ noteNames cMaj7 = ["C"; "E"; "G"; "B"] @>

        [<Test>]
        let ``Chord should return lowest note for bass``() =
            test <@ bass cDim7 = C @>

        [<Test>]
        let ``Chord should return highest note for lead``() =
            test <@ lead cMin7 = BFlat @>

        [<Test>]
        let ``Chord should be named after the root``() =
            test <@ (name cMin7b5).StartsWith("C") @>

        [<Test>]
        let ``Chord should be named after the function``() = 
            test <@ (name cMaj).StartsWith("CMaj") @>
            test <@ (name cAug).StartsWith("CAug") @>
            test <@ (name cMin).StartsWith("CMin") @>
            test <@ (name cDim).StartsWith("CDim") @>
            test <@ (name cMaj7).StartsWith("CMaj7") @>
            test <@ (name cAug7).StartsWith("CAug7") @>
            test <@ (name cMin7).StartsWith("CMin7") @>
            test <@ (name cDim7).StartsWith("CDim7") @>
            
        [<Test>]
        let ``Should create chord from root and function``() =
            test <@ chordFromRootAndFunction C Major = cMaj @>
            test <@ chordFromRootAndFunction C Augmented = cAug @>
            test <@ chordFromRootAndFunction C Minor = cMin @>
            test <@ chordFromRootAndFunction C Diminished = cDim @>
            test <@ chordFromRootAndFunction C Major7 = cMaj7 @>
            test <@ chordFromRootAndFunction C Augmented7 = cAug7 @>
            test <@ chordFromRootAndFunction C Minor7 = cMin7 @>
            test <@ chordFromRootAndFunction C Diminished7 = cDim7 @>
            test <@ chordFromRootAndFunction C Minor7b5 = cMin7b5 @>

        [<Test>]
        let ``Should invert chord for first inversion``() =
            test <@ (invert cMaj).notes = [(E, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cAug).notes = [(E, Third); (GSharp, Fifth); (C, Root)]  @>
            test <@ (invert cMin).notes = [(EFlat, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cDim).notes = [(EFlat, Third); (GFlat, Fifth); (C, Root)]  @>
            test <@ (invert cMaj7).notes = [(E, Third); (G, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cAug7).notes = [(E, Third); (GSharp, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cMin7).notes = [(EFlat, Third); (G, Fifth); (BFlat, Seventh); (C, Root)]  @>
            test <@ (invert cDim7).notes = [(EFlat, Third); (GFlat, Fifth); (A, Seventh); (C, Root)]  @>
            test <@ (invert cMin7b5).notes = [(EFlat, Third); (GFlat, Fifth); (BFlat, Seventh); (C, Root)]  @>

        [<Test>]
        let ``Should invert chord for second inversion``() =
            test <@ (cMaj |> invert |> invert).notes = [(G, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cAug |> invert |> invert).notes = [(GSharp, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cMin |> invert |> invert).notes = [(G, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim |> invert |> invert).notes = [(GFlat, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cMaj7 |> invert |> invert).notes = [(G, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cAug7 |> invert |> invert).notes = [(GSharp, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cMin7 |> invert |> invert).notes = [(G, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim7 |> invert |> invert).notes = [(GFlat, Fifth); (A, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cMin7b5 |> invert |> invert).notes = [(GFlat, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>

        [<Test>]
        let ``Should invert chord for third inversion``() =
            test <@ (cMaj7 |> invert |> invert |> invert).notes = [(B, Seventh); (C, Root); (E, Third); (G, Fifth)]  @>
            test <@ (cAug7 |> invert |> invert |> invert).notes = [(B, Seventh); (C, Root); (E, Third); (GSharp, Fifth)]  @>
            test <@ (cMin7 |> invert |> invert |> invert).notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (G, Fifth)]  @>
            test <@ (cDim7 |> invert |> invert |> invert).notes = [(A, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>
            test <@ (cMin7b5 |> invert |> invert |> invert).notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>

        [<Test>]
        let ``Should transform chord to drop2``() =
            test <@ (cMaj7 |> toDrop2).notes = [(C, Root); (G, Fifth); (B, Seventh); (E, Third); ]  @>

        [<Test>]
        let ``Should transform chord to drop3``() =
            test <@ (cMaj7 |> toDrop3).notes = [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]  @>

        [<Test>]
        let ``Should invert drop2``() =
            test <@ (cMaj7 |> toDrop2 |> invert).notes = [(E, Third); (B, Seventh); (C, Root); (G, Fifth);]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert).notes = [(G, Fifth); (C, Root); (E, Third); (B, Seventh);]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert |> invert ).notes = [(B, Seventh); (E, Third); (G, Fifth); (C, Root); ]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert |> invert |> invert).notes = [(C, Root); (G, Fifth); (B, Seventh); (E, Third);]  @>

        [<Test>]
        let ``Should invert drop3``() =
            test <@ (cMaj7 |> toDrop3 |> invert).notes = [(E, Third); (C, Root); (G, Fifth); (B, Seventh)]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert).notes = [(G, Fifth); (E, Third); (B, Seventh); (C, Root);]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert |> invert).notes = [(B, Seventh); (G, Fifth); (C, Root); (E, Third);]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert |> invert |> invert).notes = [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]  @>

    module ScalesHormonizerTests =
        open NUnit.Framework
        open Swensen.Unquote
        open ScaleHarmonizer
        open Scales
        open Chords
        open Notes
        
        let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
        let dMin = {notes= [(D, Root); (F, Third); (A, Fifth)]; chordType=Closed}
        let eMin = {notes= [(E, Root); (G, Third); (B, Fifth)]; chordType=Closed}
        let fMaj = {notes= [(F, Root); (A, Third); (C, Fifth)]; chordType=Closed}
        let gMaj = {notes= [(G, Root); (B, Third); (D, Fifth)]; chordType=Closed}
        let aMin = {notes= [(A, Root); (C, Third); (E, Fifth)]; chordType=Closed}
        let bDim = {notes= [(B, Root); (D, Third); (F, Fifth)]; chordType=Closed}

        let cMin = {notes= [(C, Root); (EFlat, Third); (G, Fifth)]; chordType=Closed}
        let dDim = {notes= [(D, Root); (F, Third); (AFlat, Fifth)]; chordType=Closed}
        let eFlatAug = {notes= [(EFlat, Root); (G, Third); (B, Fifth)]; chordType=Closed}
        let fMin = {notes= [(F, Root); (AFlat, Third); (C, Fifth)]; chordType=Closed}
        let aFlatMaj = {notes= [(AFlat, Root); (C, Third); (EFlat, Fifth)]; chordType=Closed}

        let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
        let dMin7 = {notes= [(D, Root); (F, Third); (A, Fifth); (C, Seventh)]; chordType=Closed}
        let eMin7 = {notes= [(E, Root); (G, Third); (B, Fifth); (D, Seventh)]; chordType=Closed}
        let fMaj7 = {notes= [(F, Root); (A, Third); (C, Fifth); (E, Seventh)]; chordType=Closed}
        let gDom7 = {notes= [(G, Root); (B, Third); (D, Fifth); (F, Seventh)]; chordType=Closed}
        let aMin7 = {notes= [(A, Root); (C, Third); (E, Fifth); (G, Seventh)]; chordType=Closed}
        let bMin7b5 = {notes= [(B, Root); (D, Third); (F, Fifth); (A, Seventh)]; chordType=Closed}

        let cMinMaj7 = {notes= [(C, Root); (EFlat, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
        let dMin7b5 = {notes= [(D, Root); (F, Third); (AFlat, Fifth); (C, Seventh)]; chordType=Closed}
        let eFlatAug7 = {notes= [(EFlat, Root); (G, Third); (B, Fifth); (D, Seventh)]; chordType=Closed}
        let fMin7 = {notes= [(F, Root); (AFlat, Third); (C, Fifth); (EFlat, Seventh)]; chordType=Closed}
        let aFlatMaj7 = {notes= [(AFlat, Root); (C, Third); (EFlat, Fifth); (G, Seventh)]; chordType=Closed}
        let bDim7 = {notes= [(B, Root); (D, Third); (F, Fifth); (AFlat, Seventh)]; chordType=Closed}

        [<Test>]
        let ``Should filter scale in thirds`` () =
            let cIonian = createScale Ionian C
            test <@ thirds ScaleDgrees.I cIonian  = [ C; E; G; B ] @>
            test <@ thirds ScaleDgrees.II cIonian  = [ D; F; A; C ] @>
            test <@ thirds ScaleDgrees.III cIonian  = [ E; G; B; D ] @>
            test <@ thirds ScaleDgrees.IV cIonian  = [ F; A; C; E ] @>
            test <@ thirds ScaleDgrees.V cIonian  = [ G; B; D; F ] @>
            test <@ thirds ScaleDgrees.VI cIonian  = [ A; C; E; G ] @>
            test <@ thirds ScaleDgrees.VII cIonian  = [ B; D; F; A ] @>

        [<Test>]
        let ``Should create triads for Ionian scale`` () =
            let cIonian = createScale Ionian C
            test <@ triadsHarmonizer ScaleDgrees.I cIonian = cMaj @>
            test <@ triadsHarmonizer ScaleDgrees.II cIonian = dMin @>
            test <@ triadsHarmonizer ScaleDgrees.III cIonian = eMin @>
            test <@ triadsHarmonizer ScaleDgrees.IV cIonian = fMaj @>
            test <@ triadsHarmonizer ScaleDgrees.V cIonian = gMaj @>
            test <@ triadsHarmonizer ScaleDgrees.VI cIonian = aMin @>
            test <@ triadsHarmonizer ScaleDgrees.VII cIonian = bDim @>

        [<Test>]
        let ``Should create triads for Harmonic Minor scale`` () =
            let cMinor = createScale HarmonicMinor C
            test <@ triadsHarmonizer ScaleDgrees.I cMinor = cMin @>
            test <@ triadsHarmonizer ScaleDgrees.II cMinor = dDim @>
            test <@ triadsHarmonizer ScaleDgrees.III cMinor = eFlatAug @>
            test <@ triadsHarmonizer ScaleDgrees.IV cMinor = fMin @>
            test <@ triadsHarmonizer ScaleDgrees.V cMinor = gMaj @>
            test <@ triadsHarmonizer ScaleDgrees.VI cMinor = aFlatMaj @>
            test <@ triadsHarmonizer ScaleDgrees.VII cMinor = bDim @>

        [<Test>]
        let ``Should create seventh chords for Ionian scale`` () =
            let cIonian = createScale Ionian C
            test <@ seventhsHarmonizer ScaleDgrees.I cIonian = cMaj7 @>
            test <@ seventhsHarmonizer ScaleDgrees.II cIonian = dMin7 @>
            test <@ seventhsHarmonizer ScaleDgrees.III cIonian = eMin7 @>
            test <@ seventhsHarmonizer ScaleDgrees.IV cIonian = fMaj7 @>
            test <@ seventhsHarmonizer ScaleDgrees.V cIonian = gDom7 @>
            test <@ seventhsHarmonizer ScaleDgrees.VI cIonian = aMin7 @>
            test <@ seventhsHarmonizer ScaleDgrees.VII cIonian = bMin7b5 @>

        [<Test>]
        let ``Should create seventh chords for Harmonic Minor scale`` () =
            let cMinor = createScale HarmonicMinor C
            test <@ seventhsHarmonizer ScaleDgrees.I cMinor = cMinMaj7 @>
            test <@ seventhsHarmonizer ScaleDgrees.II cMinor = dMin7b5 @>
            test <@ seventhsHarmonizer ScaleDgrees.III cMinor = eFlatAug7 @>
            test <@ seventhsHarmonizer ScaleDgrees.IV cMinor = fMin7 @>
            test <@ seventhsHarmonizer ScaleDgrees.V cMinor = gDom7 @>
            test <@ seventhsHarmonizer ScaleDgrees.VI cMinor = aFlatMaj7 @>
            test <@ seventhsHarmonizer ScaleDgrees.VII cMinor = bDim7 @>