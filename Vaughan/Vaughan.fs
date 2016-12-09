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

        let rec min (minOf:'a->'a->'a) (list:'a list) =
           match list with
           | [] -> invalidArg "list" "Empty list"
           | [x] -> x
           | c1::c2::rest -> min minOf ((minOf c1 c2)::rest)

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
            let rec loop note =
                let newNote = transposeDirection note transposingInterval
                let newInterval = intervalBetween noteToTranspose newNote
                
                if toDistance newInterval = toDistance transposingInterval then
                    newNote
                else
                    loop newNote
                
            loop noteToTranspose

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

        let rec private repeatInversion chord times =
            match times with
            | 0 -> chord
            | _ -> repeatInversion (chord |> invert) (times - 1)

        let private generateChordInversions chord =
            let notesInChord = chord.notes |> List.length
            [for index in 1 .. notesInChord do yield repeatInversion chord index]

        let private isLeadFunctionOnChordDesiredFunction chord desiredNoteFunction listFilter =
            noteFunction (chord.notes |> listFilter) = desiredNoteFunction

        let private inversionForFunction chord desiredNoteFunction listFilter =
            generateChordInversions chord
            |> List.filter (fun c -> isLeadFunctionOnChordDesiredFunction c desiredNoteFunction listFilter)
            |> List.head

        let inversionForFunctionAsLead chord desiredNoteFunction =
            inversionForFunction chord desiredNoteFunction List.last

        let inversionForFunctionAsBass chord desiredNoteFunction =
            inversionForFunction chord desiredNoteFunction List.head

        let invertionWithLeadClosestToNote chord note =
            generateChordInversions chord
            |> min (fun c1 c2 -> 
                if (measureAbsoluteSemitones (lead c1) note) < (measureAbsoluteSemitones (lead c2) note) 
                then c1 else c2)

    module ScaleHarmonizer = 
        open Infrastructure
        open Chords
        
        type ScaleDgrees = | I = 0 | II = 1 | III = 2 | IV = 3 | V = 4 | VI = 5 | VII = 6

        let thirds (fromPosition:ScaleDgrees) scale =
            scale 
            |> circularSequenceFromList
            |> Seq.skip (int fromPosition)
            |> Seq.take 16 
            |> Seq.mapi (fun i v -> i, v)
            |> Seq.filter (fun (i, _) -> i % 2 = 0)
            |> Seq.map snd
            |> Seq.toList

        let harmonizer forDegree scale =
            let thirdsList = 
                scale
                |> thirds forDegree
                |> List.take 7
            
            {notes= [(thirdsList.[0], Root); 
                     (thirdsList.[1] , Third); 
                     (thirdsList.[2], Fifth); 
                     (thirdsList.[3], Seventh); 
                     (thirdsList.[4], Ninth); 
                     (thirdsList.[5], Eleventh); 
                     (thirdsList.[6], Thirteenth)]; 
             chordType = Closed}

        let reducedHarmonizer forDegree scale notes =
            let complete = harmonizer forDegree scale
            {complete with notes = complete.notes |> List.take notes}

        let seventhsHarmonizer forDegree scale =
             reducedHarmonizer forDegree scale 4

        let triadsHarmonizer forDegree scale =
            reducedHarmonizer forDegree scale 3