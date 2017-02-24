namespace Vaughan
    
    //https://repl.it/FJHh/0

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

        let cappedMaximum number cap =
            if number > cap then cap else number

    module Domain =
        type Note = 
            | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp 
            | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B
                    
        type Interval = 
            | Unisson | MinorSecond | MajorSecond | AugmentedSecond | MinorThird
            | MajorThird | PerfectForth | AugmentedForth | DiminishedFifth
            | PerfectFifth | AugmentedFifth | MinorSixth | MajorSixth
            | MinorSeventh | MajorSeventh | PerfectOctave

        type Scales = 
            | Ionian | Dorian | Phrygian | Lydian | Mixolydian
            | Aolian | Locrian | MajorPentatonic | MinorPentatonic
            | Blues | HarmonicMinor | MelodicMinor | Dorianb2 | LydianAugmented
            | LydianDominant | Mixolydianb6 | LocrianSharp2
            | AlteredDominant | HalfWholeDiminished | WholeTone

        type ScaleFormula = Interval list

        type Key = 
            | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
            | DMajor | DFlatMajor | EMajor | EFlatMajor
            | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
            | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
            | EMinor | FMinor | FSharpMinor | GMinor 
            | GSharpMinor | EFlatMinor

        type Quality = 
            | Major | Augmented | Minor | Diminished
            | Major7 | Augmented7 | Minor7 | Diminished7 
            | Dominant7 | Minor7b5 | MinorMaj7
            | Sus2 | Sus2Diminished | Sus2Augmented
            | Sus4 | Sus4Diminished | Sus4Augmented
            
        type ChordNoteFunction = 
            | Root | Third | Fifth | Seventh | Ninth | Eleventh | Thirteenth
        
        type ChordNote = Note * ChordNoteFunction

        type ChordType = | Open | Closed | Drop2 | Drop3

        type Chord = {Notes:ChordNote list; ChordType:ChordType;}

        type ScaleDgrees = 
            | I = 0 | II = 1 | III = 2 | IV = 3 | V = 4 | VI = 5 | VII = 6

        type GuitarString = 
            | SixthString | FifthString | FourthString 
            | ThirdString | SecondString | FirstString
        
        type Fret = {GuitarString:GuitarString; Fret:int; Note:Note}
        
        type GuitarChord = {Chord:Chord; Frets:Fret list}

        type ChordIntent = { Root: Note; Quality:Quality; }

    module Notes =
        open Domain

        type private NoteAttributes = {Name:string; Sharp:Note; Flat:Note; Pitch:int}
        type private IntervalAttributes = {Name:string; Distance:int}

        let private noteAttributes = function
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
            
        let natural note =
            note
            
        let pitch note =
            (noteAttributes note).Pitch

        let private intervalAttributes = function
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
        
        let private transposeDirection note = function
            | Unisson -> note 
            | MajorSecond | AugmentedSecond | PerfectFifth | MajorThird 
            | PerfectForth | AugmentedFifth | MajorSixth | PerfectOctave 
            | AugmentedForth | MajorSeventh -> sharp note
            | MinorSecond | DiminishedFifth | MinorThird
            | MinorSixth | MinorSeventh  -> flat note
            
        let intervalName interval =
            (intervalAttributes interval).Name
        
        let toDistance interval =
            (intervalAttributes interval).Distance
            
        let fromDistance = function
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
            
        let intervalBetween note other =
            fromDistance (measureAbsoluteSemitones note other)
            
        let transpose noteToTranspose transposingInterval =
            let rec loop note =
                let newNote = transposeDirection note transposingInterval
                let newInterval = intervalBetween noteToTranspose newNote
                
                if toDistance newInterval = toDistance transposingInterval then
                    newNote
                else
                    loop newNote
                
            loop noteToTranspose

    module Scales =
        open Domain
        open Notes

        let formula = function
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
        open Domain
        open Notes

        type private KeyAttributes = {Root:Note; Accidentals:int}

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
        open Domain
        open Notes
        open Infrastructure

        let private functionForInterval = function
            | Unisson -> Root
            | MajorThird | MinorThird -> Third 
            | PerfectFifth | DiminishedFifth | AugmentedFifth  -> Fifth
            | MajorSeventh | MinorSeventh | MajorSixth -> Seventh
            | _ -> Root

        let private intervalsForQuality = function
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

        let private functionForIntervals = function
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

        let private abreviatedName = function
            | Major -> "Maj" | Augmented -> "Aug" | Minor -> "Min" 
            | Diminished -> "Dim" | Major7 -> "Maj7" 
            | Augmented7 -> "Aug7" | Minor7 -> "Min7" 
            | Diminished7 -> "Dim7" | Dominant7 -> "Dom7" 
            | Minor7b5 -> "Min7b5" | MinorMaj7 -> "MinMaj7"
            | Sus2 -> "Sus2" | Sus2Diminished -> "Sus2Dim" 
            | Sus2Augmented -> "Sus2Aug"
            | Sus4 -> "Sus4" | Sus4Diminished -> "SusDim" 
            | Sus4Augmented -> "Sus4Aug"

        let private note chordNote =
            fst chordNote

        let private noteFunction chordNote =
            snd chordNote

        let private rawNotes chord =
            chord.Notes |>  List.map note

        let private rawNoteForIndex nth chord =
            (List.item nth (rawNotes chord))
        
        let private noteForFunction chord chordNoteFunction =
            note (chord.Notes |> List.find (fun n -> noteFunction n = chordNoteFunction))
        
        let private intervalsForChord chord =
            let root = noteForFunction chord Root
            chord.Notes
            |> List.map (fun n -> intervalBetween root (note n))
            |> List.sortBy toDistance
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

        let invert chord =
            match chord.ChordType with
            | Closed | Open -> invertOpenOrClosed chord
            | Drop2 -> invertDrop2 chord
            | Drop3 -> invertDrop3 chord
        let bass chord =
            note (chord.Notes |> List.head)
        
        let lead chord =
            note (chord.Notes |> List.last)
            
        let name chord =
            noteName (noteForFunction chord Root) 
            + abreviatedName (functionForIntervals(intervalsForChord chord))
            
        let noteNames chord =
            chord.Notes |> List.map (note >> noteName)
            
        let chordFromRootAndFunction root quality =
            {
                Notes= [(root, Root)] @ (intervalsForQuality quality |> List.map (fun i -> ((transpose root i), functionForInterval i)));
                ChordType = Closed
            }

        let toDrop2 chord =
            {Notes= chord.Notes |> swapFirstTwo |> rotateByOne; ChordType=Drop2}

        let toDrop3 chord =
            {Notes= (chord |> toDrop2 |> toDrop2).Notes; ChordType=Drop3}

    module ChordVoiceLeading =
        open Chords
        open Domain
        open Notes
        open Infrastructure

        let private isLeadFunctionOnChordDesiredFunction chord desiredNoteFunction listFilter =
            snd (chord.Notes |> listFilter) = desiredNoteFunction

        let rec private repeatInversion chord times =
            match times with
            | 0 -> chord
            | _ -> repeatInversion (chord |> invert) (times - 1)

        let private generateChordInversions chord =
            let notesInChord = chord.Notes |> List.length
            [for index in 1 .. notesInChord do yield repeatInversion chord index]

        let private inversionForFunction chord desiredNoteFunction listFilter =
            generateChordInversions chord
            |> List.filter (fun c -> isLeadFunctionOnChordDesiredFunction c desiredNoteFunction listFilter)
            |> List.head
        
        let private invertionWithNoteClosestToNote chord note chordNote =
            (generateChordInversions chord
            |> min (fun c1 c2 -> 
                if (measureAbsoluteSemitones (chordNote c1) note) < (measureAbsoluteSemitones (chordNote c2) note) 
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

        let thirds (fromPosition:ScaleDgrees) scale =
            let octave = 16
            
            scale 
            |> circularSequenceFromList
            |> Seq.skip (int fromPosition)
            |> Seq.take octave
            |> filterOddIndexElements
            |> Seq.toList

        let harmonizer forDegree scale =
            let thirdsList = 
                scale
                |> thirds forDegree
                |> List.take 7
            
            {Notes= [(thirdsList.[0], Root); 
                     (thirdsList.[1], Third); 
                     (thirdsList.[2], Fifth); 
                     (thirdsList.[3], Seventh); 
                     (thirdsList.[4], Ninth); 
                     (thirdsList.[5], Eleventh); 
                     (thirdsList.[6], Thirteenth)]; 
             ChordType = Closed}

        let private harmonizeScaleDegreeWithNotes forDegree scale notes =
            let complete = harmonizer forDegree scale
            {complete with Notes = complete.Notes |> List.take notes}

        let seventhsHarmonizer forDegree scale =
             harmonizeScaleDegreeWithNotes forDegree scale 4

        let triadsHarmonizer forDegree scale =
            harmonizeScaleDegreeWithNotes forDegree scale 3

    module Guitar =
        open Domain
        open Notes
        open Chords
        open Infrastructure

        type private GuitarStringAttributes = {Name:string; OpenStringNote:Note; Index:int}
        let private guitarStringAttributes = function
            | SixthString -> { Name="Sixth"; OpenStringNote=E; Index=6}
            | FifthString -> { Name="Fifth"; OpenStringNote=A; Index=5}
            | FourthString -> { Name="Fourth"; OpenStringNote=D; Index=4}
            | ThirdString -> { Name="Third"; OpenStringNote=G; Index=3}
            | SecondString -> { Name="Second"; OpenStringNote=B; Index=2}
            | FirstString -> { Name="First"; OpenStringNote=E; Index=1}

        let fretForNote note guitarString =
            measureAbsoluteSemitones (guitarStringAttributes guitarString).OpenStringNote note

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

        let private isOpenFret fret =
            fret.Fret = 0

        let private raiseOctave fret =
            {fret with Fret = fret.Fret + 12}

        let private fretDistance fret other =
            abs(fret.Fret - other.Fret)

        let private isStretching fret other =
            let maxStrech = 5
            (fretDistance fret other) > maxStrech

        let private raiseOctaveOnStretch previous current next =
            if (isStretching current previous) || (isStretching current next)
            then {current with Fret= current.Fret + 12}
            else current

        let private isRaised fret =
            fret.Fret > 11

        let private hasRaised frets =
            frets
            |> List.exists isRaised

        let private raiseOpenFrets frets =
            frets 
            |> List.map (fun fret -> if isOpenFret fret then raiseOctave fret else fret)

        let private raiseUnraisedFrets frets =
            if hasRaised frets then
                frets
                |> List.mapi (fun i fret -> 
                    if isRaised fret then fret
                    else
                        let minimumIndex = cappedMinimum (i-1) 0
                        let maxIndex = (frets |> List.length) - 1
                        let maximumIndex = cappedMaximum (i+1) maxIndex 
                        raiseOctaveOnStretch frets.[minimumIndex] fret frets.[maximumIndex])
            else
                frets

        let private unstretch frets =
            let rec loop fx i =
               match i with
               | 0 -> fx
               | _ -> loop (fx |> raiseUnraisedFrets) (i-1)
            
            loop frets ((frets |> List.length) - 1)

        let private createMutedStringFret guitarString note =
            let note = openStringNote guitarString
            { GuitarString = guitarString; Fret = -1; Note = note }
            
        let private createFret guitarString note =
            { GuitarString = guitarString; Fret = fretForNote note guitarString; Note = note }

        let private skipString bassString guitarString chord =
            let afterBassString = (indexToGuitarString ((guitarStringOrdinal bassString) - 1))
            chord.ChordType = Drop3 && guitarString = afterBassString

        let private mapChordToGuitarFrets bassString chord =
            let rec mapChordNoteToString guitarString chordNotes mappedChordNotes =
                if List.isEmpty chordNotes then
                    mappedChordNotes
                else
                    let nextString = nextString guitarString
                    if skipString bassString guitarString chord then
                        let fret = createMutedStringFret guitarString (fst chordNotes.[0])
                        mapChordNoteToString nextString chordNotes (fret::mappedChordNotes)
                    else
                        let fret = createFret guitarString (fst chordNotes.[0])
                        mapChordNoteToString nextString (chordNotes |> List.skip 1) (fret::mappedChordNotes)

            mapChordNoteToString bassString chord.Notes []

        let chordToGuitarChord bassString chord =
            { Chord=chord; Frets= mapChordToGuitarFrets bassString chord |> List.rev }

        let chordToGuitarClosedChord bassString chord =
            let guitarChord = chordToGuitarChord bassString chord 
            let closedChord = {guitarChord with Frets = raiseOpenFrets guitarChord.Frets}
            {closedChord with Frets = unstretch closedChord.Frets}

    module GuitarTab =
        open System
        open Domain
        open Notes
        open Chords
        open Guitar

        let private startTab = 
            [
                "E|-";
                "B|-";
                "G|-";
                "D|-";
                "A|-";
                "E|-"
            ]

        let private bar = 
                    [
                        "-|-";
                        "-|-";
                        "-|-";
                        "-|-";
                        "-|-";
                        "-|-"
                    ]

        let private endTab = 
                    [
                        "-|" + Environment.NewLine;
                        "-|" + Environment.NewLine;
                        "-|" + Environment.NewLine;
                        "-|" + Environment.NewLine;
                        "-|" + Environment.NewLine;
                        "-|" + Environment.NewLine;
                    ]

        let private mutedStringDashes guitarChord = 
            String.replicate (name guitarChord.Chord).Length "-"

        let private fretedStringDashes guitarChord fret = 
            String.replicate ((name guitarChord.Chord).Length - (string(fret)).Length) "-"

        let private mutedHigherStrings guitarChord =
            match (guitarChord.Frets |> List.last).GuitarString with
                | SecondString -> 1
                | ThirdString -> 2
                | FourthString -> 3
                | _ -> 0

        let private mutedLowerStrings guitarChord =
            match (guitarChord.Frets |> List.head).GuitarString with
            | FifthString -> 1
            | FourthString  -> 2
            | ThirdString  -> 3
            | _ -> 0

        let private tabifyMutedHigherStrings guitarChord =
            let mutedStrings = mutedHigherStrings guitarChord
            List.replicate mutedStrings (mutedStringDashes guitarChord)

        let private shapifyMutedHigherStrings guitarChord =
            let mutedStrings = mutedHigherStrings guitarChord
            List.replicate mutedStrings "X"

        let private tabifyMutedLowerStrings guitarChord =
            let mutedStrings = mutedLowerStrings guitarChord
            List.replicate mutedStrings (mutedStringDashes guitarChord)

        let private shapifyMutedLowerStrings guitarChord =
            let mutedStrings = mutedLowerStrings guitarChord
            List.replicate mutedStrings "X"

        let private tabifyFret fret guitarChord =
            if fret.Fret = -1 then
                sprintf "%s" (mutedStringDashes guitarChord)
            else
                sprintf "%i%s" fret.Fret (fretedStringDashes guitarChord fret.Fret)

        let private shapifyFret fret =
            if fret.Fret = -1 then
                "X"
            else
                sprintf "%i" fret.Fret

        let private tabifyFrets guitarChord =
            guitarChord.Frets |> List.map (fun fret -> tabifyFret fret guitarChord) |> List.rev

        let private shapifyFrets guitarChord =
            guitarChord.Frets |> List.map shapifyFret

        let private tabifyChord guitarChord = 
            (tabifyMutedHigherStrings guitarChord) 
            @ (tabifyFrets guitarChord)
            @ (tabifyMutedLowerStrings guitarChord)

        let private shapifyChord guitarChord = 
            (shapifyMutedLowerStrings guitarChord)
            @ (shapifyFrets guitarChord)
            @ (shapifyMutedHigherStrings guitarChord)

        let private groupByString (tabifiedChords: string list list) =
            [0 .. 5] 
            |> List.map (fun index -> tabifiedChords |> List.map (fun l -> l.[index]))

        let private tabifyStrings guitarStrings =
            guitarStrings 
            |> List.map (fun tabifiedFrets -> tabifiedFrets |> List.fold (fun acc fret -> acc + fret + "---" ) "---")
            |> List.mapi (fun index tabifiedFrets -> startTab.[index] + tabifiedFrets + endTab.[index])

        let private tabifyChordNames guitarChords = 
            let chordNameSeparator = "   "
            let separatedChordNames = 
                guitarChords
                |> List.map (fun guitarChord -> chordNameSeparator + name guitarChord.Chord)  
            [chordNameSeparator] @ separatedChordNames @ [chordNameSeparator; Environment.NewLine;]

        let tabifyAll guitarChords = 
            let tabifiedChordNames = guitarChords |> tabifyChordNames
            let tabifiedChords = 
                        guitarChords 
                        |> List.map tabifyChord
                        |> groupByString
                        |> tabifyStrings
            (tabifiedChordNames @ tabifiedChords) |> List.fold (+) ""

        let tabify guitarChord =
            tabifyAll [guitarChord]

        let shapify guitarChord =
            name guitarChord.Chord +
            Environment.NewLine +
            "EADGBE" +
            Environment.NewLine +
            (guitarChord
                |> shapifyChord
                |> List.fold (+) "") +
            Environment.NewLine

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
        
        let private note: Parser<_> =
            any [
                    (stringCIReturn "a" A);
                    (stringCIReturn "b" B);
                    (stringCIReturn "c" C);
                    (stringCIReturn "d" D);
                    (stringCIReturn "e" E);
                    (stringCIReturn "f" F);
                    (stringCIReturn "g" G)
                ] |> skipSpaces

        let private accident: Parser<_> =
            any [
                    (stringReturn "#" sharp);
                    (stringReturn "b" flat);
                    (notFollowedByString "#" >>% natural);
                    (notFollowedByString "b" >>% natural)
                ] |> skipSpaces

        let private majorQuality: Parser<_> =
            any [
                    (stringCIReturn "major" Major)
                    (stringCIReturn "maj" Major)
                    (stringReturn "M" Major) 
                ] |> skipSpaces

        let private minorQuality: Parser<_> =
            any [
                    (stringCIReturn "minor" Minor);
                    (stringCIReturn "min" Minor);
                    (stringReturn "m" Minor);
                ] |> skipSpaces

        let private augmentedQuality: Parser<_> =
             any [
                    (stringCIReturn "augmented" Augmented);
                    (stringCIReturn "aug" Augmented)
                 ] |> skipSpaces

        let private diminishedQuality: Parser<_> =
            any [
                    (stringCIReturn "diminished" Diminished);
                    (stringCIReturn "dim" Diminished)
                ] |> skipSpaces

        let private dominantQuality: Parser<_> =
            any [
                    (stringCIReturn "7" Dominant7);
                    (stringCIReturn "7th" Dominant7);
                    (stringCIReturn "seventh" Dominant7);
                    (stringCIReturn "seven" Dominant7);
                    (stringCIReturn "dominant" Dominant7);
                    (stringCIReturn "dom" Dominant7)
                ] |> skipSpaces

        let private quality: Parser<_> =
            any [
                    majorQuality
                    minorQuality
                    augmentedQuality
                    diminishedQuality
                    dominantQuality
                ] |> skipSpaces

        let private seventh chord =
            match chord with
            | {Quality=Major} -> { chord with Quality = Major7 }
            | {Quality=Minor} -> { chord with Quality = Minor7 }
            | {Quality=Diminished} -> { chord with Quality = Diminished7 }
            | {Quality=Augmented} -> { chord with Quality = Augmented7 }
            | {Quality=_} -> { chord with Quality = Dominant7 }

        let private noSeventh chord = 
            chord

        let private seventhQuality: Parser<_> =
            any [
                    (stringCIReturn "7" seventh);
                    (stringCIReturn "7th" seventh);
                    (stringCIReturn "seventh" seventh);
                    (stringCIReturn "seven" seventh);
                    (notFollowedByString "7" >>% noSeventh);
                    (notFollowedByString "7th" >>% noSeventh);
                    (notFollowedByString "seventh" >>% noSeventh);
                    (notFollowedByString "seven" >>% noSeventh)
                ] |> skipSpaces

        let private chordParser: Parser<_> =
            pipe4 note accident quality seventhQuality
                (fun n a q s -> s { Root=(a n); Quality=q; })

        let parseChord (text:string) =
            let parsed = run chordParser text
            match parsed with
            | Success(chordDefinition, _, _) -> chordDefinition
            | Failure(errorMsg, _, _) -> invalidOp errorMsg

        let createChord chordIntent =
            chordFromRootAndFunction chordIntent.Root chordIntent.Quality