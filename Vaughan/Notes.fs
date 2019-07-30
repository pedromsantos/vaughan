namespace Vaughan

module Notes =
    type private NoteAttributes =
        { Name : string
          Sharp : Note
          Flat : Note
          Pitch : int }

    type private IntervalAttributes =
        { Name : string
          Distance : int<ht> }

    type private OctaveAttributes =
        { Value : float
          MidiName : string
          MidiNumber : int<midiNote> }

    type private INoteAttributes = Note -> NoteAttributes

    let private noteAttributes : INoteAttributes =
        function
        | C ->
            { Name = "C"
              Sharp = CSharp
              Flat = B
              Pitch = 0 }
        | CSharp ->
            { Name = "C#"
              Sharp = D
              Flat = C
              Pitch = 1 }
        | DFlat ->
            { Name = "Db"
              Sharp = D
              Flat = C
              Pitch = 1 }
        | D ->
            { Name = "D"
              Sharp = DSharp
              Flat = DFlat
              Pitch = 2 }
        | DSharp ->
            { Name = "D#"
              Sharp = E
              Flat = D
              Pitch = 3 }
        | EFlat ->
            { Name = "Eb"
              Sharp = E
              Flat = D
              Pitch = 3 }
        | E ->
            { Name = "E"
              Sharp = F
              Flat = EFlat
              Pitch = 4 }
        | F ->
            { Name = "F"
              Sharp = FSharp
              Flat = E
              Pitch = 5 }
        | FSharp ->
            { Name = "F#"
              Sharp = G
              Flat = F
              Pitch = 6 }
        | GFlat ->
            { Name = "Gb"
              Sharp = G
              Flat = F
              Pitch = 6 }
        | G ->
            { Name = "G"
              Sharp = GSharp
              Flat = GFlat
              Pitch = 7 }
        | GSharp ->
            { Name = "G#"
              Sharp = A
              Flat = G
              Pitch = 8 }
        | AFlat ->
            { Name = "Ab"
              Sharp = A
              Flat = G
              Pitch = 8 }
        | A ->
            { Name = "A"
              Sharp = ASharp
              Flat = AFlat
              Pitch = 9 }
        | ASharp ->
            { Name = "A#"
              Sharp = B
              Flat = A
              Pitch = 10 }
        | BFlat ->
            { Name = "Bb"
              Sharp = B
              Flat = A
              Pitch = 10 }
        | B ->
            { Name = "B"
              Sharp = C
              Flat = BFlat
              Pitch = 11 }

    let private intervalAttributes interval =
        match interval with
        | Unisson ->
            { Name = "Unisson"
              Distance = 0<ht> }
        | MinorSecond ->
            { Name = "MinorSecond"
              Distance = 1<ht> }
        | MajorSecond ->
            { Name = "MajorSecond"
              Distance = 2<ht> }
        | AugmentedSecond ->
            { Name = "AugmentedSecond"
              Distance = 3<ht> }
        | MinorThird ->
            { Name = "MinorThird"
              Distance = 3<ht> }
        | MajorThird ->
            { Name = "MajorThird"
              Distance = 4<ht> }
        | PerfectFourth ->
            { Name = "PerfectFourth"
              Distance = 5<ht> }
        | AugmentedFourth ->
            { Name = "AugmentedFourth"
              Distance = 6<ht> }
        | DiminishedFifth ->
            { Name = "DiminishedFifth"
              Distance = 6<ht> }
        | PerfectFifth ->
            { Name = "PerfectFifth"
              Distance = 7<ht> }
        | AugmentedFifth ->
            { Name = "AugmentedFifth"
              Distance = 8<ht> }
        | MinorSixth ->
            { Name = "MinorSixth"
              Distance = 8<ht> }
        | MajorSixth ->
            { Name = "MajorSixth"
              Distance = 9<ht> }
        | DiminishedSeventh ->
            { Name = "DiminishedSeventh"
              Distance = 9<ht> }
        | MinorSeventh ->
            { Name = "MinorSeventh"
              Distance = 10<ht> }
        | MajorSeventh ->
            { Name = "MajorSeventh"
              Distance = 11<ht> }
        | PerfectOctave ->
            { Name = "PerfectOctave"
              Distance = 12<ht> }
        | MinorNinth ->
            { Name = "MinorNinth"
              Distance = 13<ht> }
        | MajorNinth ->
            { Name = "MajorNinth"
              Distance = 14<ht> }
        | AugmentedNinth ->
            { Name = "AugmentedNinth"
              Distance = 15<ht> }
        | PerfectEleventh ->
            { Name = "PerfectEleventh"
              Distance = 17<ht> }
        | AugmentedEleventh ->
            { Name = "AugmentedEleventh"
              Distance = 18<ht> }
        | MinorThirteenth ->
            { Name = "MinorThirteenth"
              Distance = 20<ht> }
        | MajorThirteenth ->
            { Name = "MajorThirteenth"
              Distance = 21<ht> }

    let private octaveProperties octave =
        match octave with
        | SubContra ->
            { Value = -16.0
              MidiName = "0"
              MidiNumber = 0<midiNote> }
        | Contra ->
            { Value = -8.0
              MidiName = "1"
              MidiNumber = 12<midiNote> }
        | Great ->
            { Value = -4.0
              MidiName = "2"
              MidiNumber = 24<midiNote> }
        | Small ->
            { Value = -2.0
              MidiName = "3"
              MidiNumber = 36<midiNote> }
        | OneLine ->
            { Value = 1.0
              MidiName = "4"
              MidiNumber = 48<midiNote> }
        | TwoLine ->
            { Value = 2.0
              MidiName = "5"
              MidiNumber = 60<midiNote> }
        | ThreeLine ->
            { Value = 4.0
              MidiName = "6"
              MidiNumber = 72<midiNote> }
        | FourLine ->
            { Value = 8.0
              MidiName = "7"
              MidiNumber = 84<midiNote> }
        | FiveLine ->
            { Value = 16.0
              MidiName = "8"
              MidiNumber = 96<midiNote> }
        | SixLine ->
            { Value = 32.0
              MidiName = "9"
              MidiNumber = 108<midiNote> }
        | SevenLine ->
            { Value = 64.0
              MidiName = "10"
              MidiNumber = 120<midiNote> }

    let private octaveValue octave = (octaveProperties octave).Value
    let private octaveMidiName octave = (octaveProperties octave).MidiName
    let private octaveMidiNoteMultiplier octave =
        (octaveProperties octave).MidiNumber

    let private adjustFrequencyForOctave octave frequency =
        let octaveValue = octaveValue octave * 1.0<hz>

        let noteFrequency =
            if octaveValue < 0.0<hz> then frequency / (-1.0</hz> * octaveValue)
            else float (frequency * octaveValue) * 1.0<hz>

        let roundToThousands = 1000.0
        (round (roundToThousands * float (noteFrequency)) / roundToThousands)
        * 1.0<hz>

    let durationMultipliers timeSignature =
        let durations =
            [ 1.0
              1.0 / 2.0
              1.0 / 4.0
              1.0 / 8.0
              1.0 / 16.0
              1.0 / 32.0
              1.0 / 64.0
              1.0 / 128.0 ]
        match (snd timeSignature) with
        | Whole -> durations |> List.map (fun d -> d * 1.0)
        | Half -> durations |> List.map (fun d -> d * 2.0)
        | Quarter -> durations |> List.map (fun d -> d * 4.0)
        | Eigth -> durations |> List.map (fun d -> d * 8.0)
        | Sixteenth -> durations |> List.map (fun d -> d * 16.0)
        | ThirtySecond -> durations |> List.map (fun d -> d * 32.0)
        | SixtyFourth -> durations |> List.map (fun d -> d * 64.0)
        | HundredTwentyEighth -> durations |> List.map (fun d -> d * 128.0)

    let sharp : SharpNote = fun note -> (noteAttributes note).Sharp
    let flat : FlatNote = fun note -> (noteAttributes note).Flat
    let natural : NaturalNote = id
    let noteName : NoteName = fun note -> (noteAttributes note).Name
    let midiName : NoteMidiName =
        fun note octave -> noteName note + octaveMidiName octave
    let pitch : NotePitch = fun note -> (noteAttributes note).Pitch
    let midiNumber : NoteMidiNumber =
        fun note octave ->
            ((pitch note) + int (octaveMidiNoteMultiplier octave)) * 1<midiNote>
    let intervalName : IntervalName =
        fun interval -> (intervalAttributes interval).Name
    let toDistance : IntervalToDistance =
        fun interval -> (intervalAttributes interval).Distance

    let toOctaveDistance : IntervalToDistance =
        fun interval ->
            let distance = toDistance interval
            let octaveDistance = toDistance PerfectOctave
            if distance > octaveDistance then distance - octaveDistance
            else distance

    let fromDistance : IntervalFromDistance =
        function
        | 0<ht> -> Unisson
        | 1<ht> -> MinorSecond
        | 2<ht> -> MajorSecond
        | 3<ht> -> MinorThird
        | 4<ht> -> MajorThird
        | 5<ht> -> PerfectFourth
        | 6<ht> -> DiminishedFifth
        | 7<ht> -> PerfectFifth
        | 8<ht> -> AugmentedFifth
        | 9<ht> -> MajorSixth
        | 10<ht> -> MinorSeventh
        | 11<ht> -> MajorSeventh
        | 12<ht> -> PerfectOctave
        | 13<ht> -> MinorNinth
        | 14<ht> -> MajorNinth
        | 15<ht> -> AugmentedNinth
        | 17<ht> -> PerfectEleventh
        | 18<ht> -> AugmentedEleventh
        | 20<ht> -> MinorThirteenth
        | 21<ht> -> MajorThirteenth
        | _ -> Unisson

    let measureAbsoluteSemitones : MeasureAbsoluteSemitones =
        fun note other ->
            let distance = (pitch other) - (pitch note)
            if (distance * 1<ht>) < (toOctaveDistance Unisson) then
                (toDistance PerfectOctave) - distance * -1<ht>
            else (distance * 1<ht>)

    let intervalBetween : IntervalBetween =
        fun note other -> fromDistance (measureAbsoluteSemitones note other)
    let private isSameInterval interval otherInterval =
        toOctaveDistance interval = toOctaveDistance otherInterval

    let transpose : TransposeNote =
        fun noteToTranspose transposingInterval ->
            match transposingInterval with
            | Unisson
            | PerfectOctave -> [ noteToTranspose ]
            | MinorSecond
            | MinorThird
            | DiminishedFifth
            | MinorSixth
            | DiminishedSeventh
            | MinorSeventh
            | MinorNinth
            | MajorNinth
            | MinorThirteenth
            | PerfectFifth
            | PerfectFourth ->
                [ C; DFlat; D; EFlat; E; F; GFlat; G; AFlat; A; BFlat; B ]
            | MajorSecond
            | AugmentedSecond
            | MajorThird
            | AugmentedFourth
            | AugmentedFifth
            | MajorSixth
            | MajorSeventh
            | AugmentedNinth
            | PerfectEleventh
            | AugmentedEleventh
            | MajorThirteenth ->
                [ C; CSharp; D; DSharp; E; F; FSharp; G; GSharp; A; ASharp; B ]
            |> List.filter
                   (fun n ->
                   isSameInterval (intervalBetween noteToTranspose n)
                       transposingInterval)
            |> List.head

    let frequency : Frequency =
        fun note octave ->
            let octaveRange = 12.0
            let a4Frequency = 440.0<hz>
            let power = double (pitch note - pitch A) / octaveRange
            let frequency = (2.0 ** power) * a4Frequency
            adjustFrequencyForOctave octave frequency

    let notesMidiNumbers =
        fun (notes : ScaleNotes) octave ->
            notes |> List.map (fun n -> midiNumber n octave)

    let durationToBeats timeSignature duration =
        (match duration with
         | Whole -> (durationMultipliers timeSignature).[0]
         | Half -> (durationMultipliers timeSignature).[1]
         | Quarter -> (durationMultipliers timeSignature).[2]
         | Eigth -> (durationMultipliers timeSignature).[3]
         | Sixteenth -> (durationMultipliers timeSignature).[4]
         | ThirtySecond -> (durationMultipliers timeSignature).[5]
         | SixtyFourth -> (durationMultipliers timeSignature).[6]
         | HundredTwentyEighth -> (durationMultipliers timeSignature).[7])
        * 1.0<beat>

    let octaveName : OctaveName =
        fun (octave : Octave) ->
            match octave with
            | SubContra -> "SubContra"
            | Contra -> "Contra"
            | Great -> "Great"
            | Small -> "Small"
            | OneLine -> "OneLine"
            | TwoLine -> "TwoLine"
            | ThreeLine -> "ThreeLine"
            | FourLine -> "FourLine"
            | FiveLine -> "FiveLine"
            | SixLine -> "SixLine"
            | SevenLine -> "SevenLine"

    let octaveMidiNumber : OctaveMidiNumber =
        fun octave ->
            match octave with
            | SubContra -> 0
            | Contra -> 1
            | Great -> 2
            | Small -> 3
            | OneLine -> 4
            | TwoLine -> 5
            | ThreeLine -> 6
            | FourLine -> 7
            | FiveLine -> 8
            | SixLine -> 9
            | SevenLine -> 10
