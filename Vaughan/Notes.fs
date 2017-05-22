namespace Vaughan

    module Notes =
        open Domain

        type private NoteAttributes = {Name:string; Sharp:Note; Flat:Note; Pitch:int}
        type private IntervalAttributes = {Name:string; Distance:int; Transpose: (Note -> Note)}
        type private INoteAttributes = Note -> NoteAttributes
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

        let private intervalAttributes interval =
            let sharp = fun n -> (noteAttributes n).Sharp
            let flat = fun n -> (noteAttributes n).Flat
            let natural = id

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


        let private sharpOrFlatNoteForInterval:ITransposeNoteForInterval = fun note interval ->
            (intervalAttributes interval).Transpose note

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
            (intervalAttributes interval).Name

        let toDistance:IIntervalToDistance = fun interval ->
            (intervalAttributes interval).Distance

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
                if isSameInterval (intervalBetween noteToTranspose note) transposingInterval
                    then note
                    else loop (sharpOrFlatNoteForInterval note transposingInterval)

            loop (sharpOrFlatNoteForInterval noteToTranspose transposingInterval)