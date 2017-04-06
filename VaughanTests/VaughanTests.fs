namespace VaughanTests

    module InfrastructureTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit 
        open Swensen.Unquote
        open Swensen.Unquote.Extensions
        open Vaughan.Infrastructure

        [<Test>]
        let ``Should rotate list``() =
            rotateByOne [] |> List.isEmpty =! true
            rotateByOne [1] =! [1]
            rotateByOne [1; 2] =! [2; 1]
            rotateByOne [1; 2; 3] =! [2; 3; 1]

        [<Property>]
        let ``Rotating list puts head in last`` (list :int list)  =
            ( not (List.isEmpty list) ) 
                ==> lazy ((list |> rotateByOne |> List.last) = (list |> List.head))

        [<Test>]
        let ``Should swap first 2 elements in list``() =
            swapFirstTwo [1; 2; 3] =! [2; 1; 3]

        [<Property>]
        let ``Swapping first 2 elements in list twice undoes first swap`` (list :int list)  =
            ( list |> List.length > 2 ) 
                ==> lazy ((list |> swapFirstTwo |> swapFirstTwo) = list)

        [<Test>]
        let ``Should filter odd index elements``() =
            filterOddIndexElements {1 .. 6} |> List.ofSeq =! ({1 .. 2 .. 6} |> List.ofSeq)
 
        [<Test>]
        let ``Should swap second 2 elements in list``() =
            swapSecondTwo [1; 2; 3] =! [1; 3; 2]

        [<Property>]
        let ``Swapping second two elements in list twice undoes first swap`` (list :int list)  =
            ( list |> List.length > 2 ) 
                ==> lazy ((list |> swapSecondTwo |> swapSecondTwo) = list)

        [<Test>]
        let ``Should create circular sequence from list``() =
            circularSequenceFromList [1; 2; 3] |> Seq.take 3 |> Seq.last =! 3
            circularSequenceFromList [1; 2; 3] |> Seq.take 4 |> Seq.last =! 1
            circularSequenceFromList [1; 2; 3] |> Seq.take 5 |> Seq.last =! 2
        
    module NotesTests =
        open NUnit.Framework
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
            
        [<Test>]
        let ``Should measure semitones distance``() =
            measureAbsoluteSemitones C C =! 0
            measureAbsoluteSemitones C CSharp =! 1
            measureAbsoluteSemitones C DFlat =! 1
            measureAbsoluteSemitones C D =! 2
            measureAbsoluteSemitones C DSharp =! 3
            measureAbsoluteSemitones C EFlat =! 3
            measureAbsoluteSemitones C E =! 4
            measureAbsoluteSemitones C F =! 5
            measureAbsoluteSemitones C FSharp =! 6
            measureAbsoluteSemitones C GFlat =! 6
            measureAbsoluteSemitones C G =! 7
            measureAbsoluteSemitones C GSharp =! 8
            measureAbsoluteSemitones C AFlat =! 8
            measureAbsoluteSemitones C A =! 9
            measureAbsoluteSemitones C ASharp =! 10
            measureAbsoluteSemitones C BFlat =! 10
            measureAbsoluteSemitones C B =! 11
            
        [<Test>]
        let ``Should create interval from distance``() =
            intervalBetween C C =! Unisson
            intervalBetween C CSharp =! MinorSecond
            intervalBetween C DFlat =! MinorSecond
            intervalBetween C D =! MajorSecond
            intervalBetween C DSharp =! MinorThird
            intervalBetween C EFlat =! MinorThird
            intervalBetween C E =! MajorThird
            intervalBetween C F =! PerfectForth
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
        let ``Should transpose note using interval``() =
            transpose C Unisson =! C
            transpose C MinorSecond =! DFlat
            transpose C MajorSecond =! D
            transpose C MinorThird =! EFlat
            transpose C MajorThird =! E
            transpose C PerfectForth =! F
            transpose C DiminishedFifth =! GFlat
            transpose C PerfectFifth =! G
            transpose C AugmentedFifth =! GSharp
            transpose C MajorSixth =! A
            transpose C MinorSeventh =! BFlat
            transpose C MajorSeventh =! B
            
        [<Test>]
        let ``Should relate interval with its name``() =
            intervalName Unisson =! "Unisson"
            intervalName MinorSecond =! "MinorSecond"
            intervalName MajorSecond =! "MajorSecond"
            intervalName MinorThird =! "MinorThird"
            intervalName MajorThird =! "MajorThird"
            intervalName PerfectForth =! "PerfectForth"
            intervalName DiminishedFifth =! "DiminishedFifth"
            intervalName PerfectFifth =! "PerfectFifth"
            intervalName AugmentedFifth =! "AugmentedFifth"
            intervalName MajorSixth =! "MajorSixth"
            intervalName MinorSeventh =! "MinorSeventh"
            intervalName MajorSeventh =! "MajorSeventh"
            intervalName PerfectOctave =! "PerfectOctave"
            
        [<Test>]
        let ``Should relate interval with distances``() =
            fromDistance 0 =! Unisson
            fromDistance 1 =! MinorSecond
            fromDistance 2 =! MajorSecond
            fromDistance 3 =! MinorThird
            fromDistance 4 =! MajorThird
            fromDistance 5 =! PerfectForth
            fromDistance 6 =! DiminishedFifth
            fromDistance 7 =! PerfectFifth
            fromDistance 8 =! AugmentedFifth
            fromDistance 9 =! MajorSixth
            fromDistance 10 =! MinorSeventh
            fromDistance 11 =! MajorSeventh
            fromDistance 12 =! PerfectOctave

     module KeyTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Keys

        [<Test>]
        let ``Should have notes for key``() =
            keyNotes CMajor =! [ C; D; E; F; G; A; B ]
            keyNotes GMajor =! [ G; A; B; C; D; E; FSharp ]
            keyNotes DMajor =! [ D; E; FSharp; G; A; B; CSharp ]
            keyNotes AMajor =! [ A; B; CSharp; D; E; FSharp; GSharp ]
            keyNotes EMajor =! [ E; FSharp; GSharp; A; B; CSharp; DSharp ]
            keyNotes BMajor =! [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ]
            keyNotes FSharpMajor =! [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ]
            keyNotes DFlatMajor =! [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ]
            keyNotes AFlatMajor =! [ AFlat; BFlat; C; DFlat; EFlat; F; G ]
            keyNotes GFlatMajor =! [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ]
            keyNotes EFlatMajor =! [ EFlat; F; G; AFlat; BFlat; C; D ]
            keyNotes BFlatMajor =! [ BFlat; C; D; EFlat; F; G; A ]
            keyNotes FMajor =! [ F; G; A; BFlat; C; D; E ]

            keyNotes AMinor =! [ A; B; C; D; E; F; G ]
            keyNotes EMinor =! [ E; FSharp; G; A; B; C; D ]
            keyNotes BMinor =! [ B; CSharp; D; E; FSharp; G; A ]
            keyNotes FSharpMinor =! [ FSharp; GSharp; A; B; CSharp; D; E ]
            keyNotes CSharpMinor =! [ CSharp; DSharp; E; FSharp; GSharp; A; B ]
            keyNotes GSharpMinor =! [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ]
            keyNotes EFlatMinor =! [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ]
            keyNotes BFlatMinor =! [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ]
            keyNotes FMinor =! [ F; G; AFlat; BFlat; C; DFlat; EFlat ]
            keyNotes CMinor =! [ C; D; EFlat; F; G; AFlat; BFlat ]
            keyNotes GMinor =! [ G; A; BFlat; C; D; EFlat; F ]
            keyNotes DMinor =! [ D; E; F; G; A; BFlat; C ]

    module ScaleTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Scales

        [<Test>]
        let ``Should have notes for scales``() =
            createScale Ionian C =! [ C; D; E; F; G; A; B ]
            createScale Dorian C =! [ C; D; EFlat; F; G; A; BFlat ]
            createScale Phrygian C =! [ C; DFlat; EFlat; F; G; AFlat; BFlat ]
            createScale Lydian C =! [ C; D; E; FSharp; G; A; B ]
            createScale Mixolydian C =! [ C; D; E; F; G; A; BFlat ]
            createScale Aolian C =! [ C; D; EFlat; F; G; AFlat; BFlat ]
            createScale Locrian C =! [ C; DFlat; EFlat; F; GFlat; AFlat; BFlat ]
            createScale MajorPentatonic C =! [ C; D; E; G; A;]
            createScale MinorPentatonic C =! [ C; EFlat; F; G; BFlat ]
            createScale Blues C =! [ C; EFlat; F; GFlat; G; BFlat ]
            createScale HarmonicMinor C =! [ C; D; EFlat; F; G; AFlat; B ]
            createScale MelodicMinor C =! [ C; D; EFlat; F; G; A; B ]
            createScale Dorianb2 C =! [ C; DFlat; EFlat; F; G; A; BFlat ]
            createScale LydianAugmented C =! [ C; D; E; FSharp; GSharp; A; B ]
            createScale LydianDominant C =! [ C; D; E; FSharp; G; A; BFlat ]
            createScale Mixolydianb6 C =! [ C; D; E; F; G; AFlat; BFlat ]
            createScale LocrianSharp2 C =! [ C; D; EFlat; F; GFlat; AFlat; BFlat ]
            createScale AlteredDominant C =! [ C; DFlat; DSharp; E; GFlat; GSharp; BFlat ]
            createScale HalfWholeDiminished C =! [ C; DFlat; EFlat; E; FSharp; G; A; BFlat ]
            createScale WholeTone C =! [ C; D; E; GFlat; GSharp; BFlat ] 

    module ChordsTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Chords
        open Vaughan.ChordVoiceLeading

        let cMaj = {Notes= [(C, Root); (E, Third); (G, Fifth)]; ChordType=Closed; Name=""}
        let cAug = {Notes= [(C, Root); (E, Third); (GSharp, Fifth)]; ChordType=Closed; Name=""}
        let cMin = {Notes= [(C, Root); (EFlat, Third); (G, Fifth)]; ChordType=Closed; Name=""}
        let cDim = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth)]; ChordType=Closed; Name=""}
        let cMaj7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed; Name=""}
        let cAug7 = {Notes= [(C, Root); (E, Third); (GSharp, Fifth); (B, Seventh)]; ChordType=Closed; Name=""}
        let cMin7 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (BFlat, Seventh)]; ChordType=Closed; Name=""}
        let cMin6 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (A, Sixth)]; ChordType=Closed; Name=""}
        let cMin6add9 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (A, Sixth);(D, Ninth)]; ChordType=Closed; Name=""}
        let cDim7 = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (A, Seventh)]; ChordType=Closed; Name=""}
        let cMin7b5 = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (BFlat, Seventh)]; ChordType=Closed; Name=""}
        let cMinMaj7 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed; Name=""}
        let cMinMaj9 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (B, Seventh); (D, Ninth)]; ChordType=Closed; Name=""}
        let cMin9 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth)]; ChordType=Closed; Name=""}
        let c6 = {Notes= [(C, Root); (E, Third); (G, Fifth); (A, Sixth)]; ChordType=Closed; Name=""}
        let c6add9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (A, Sixth); (D, Ninth)]; ChordType=Closed; Name=""}
        let c6flat5add9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (A, Sixth); (D, Ninth)]; ChordType=Closed; Name=""}
        let c7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh)]; ChordType=Closed; Name=""}
        let c7flat5 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh)]; ChordType=Closed; Name=""}    
        let c9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth)]; ChordType=Closed; Name=""}
        let c7flat9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (DFlat, Ninth)]; ChordType=Closed; Name=""}
        let c7sharp9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (DSharp, Ninth)]; ChordType=Closed; Name=""}
        let c7flat5flat9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh); (DFlat, Ninth)]; ChordType=Closed; Name=""}    
        let c7flat5sharp9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh); (DSharp, Ninth)]; ChordType=Closed; Name=""}
        let c11 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth); (F, Eleventh)]; ChordType=Closed; Name=""}
        let c13 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth); (F, Eleventh); (A, Thirteenth)]; ChordType=Closed; Name=""}
        let cMaj9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (D, Ninth)]; ChordType=Closed; Name=""}
        let cMaj9Sharp11 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (D, Ninth); (FSharp, Eleventh)]; ChordType=Closed; Name=""}
        let cMaj11 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (F, Eleventh)]; ChordType=Closed; Name=""}
        let cMaj13 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (A, Thirteenth)]; ChordType=Closed; Name=""}
        let cMaj13sharp11 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (A, Thirteenth); (FSharp, Eleventh)]; ChordType=Closed; Name=""}

        [<Test>]
        let ``Chord should return note names``() =
            noteNames cMaj7 =! ["C"; "E"; "G"; "B"]

        [<Test>]
        let ``Chord should return lowest note for bass``() =
            bass cDim7 =! C

        [<Test>]
        let ``Chord should return highest note for lead``() =
            lead cMin7 =! BFlat

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
            test <@ (name c6).StartsWith("C6") @>
            
        [<Test>]
        let ``Should create chord from root and function``() =
            (chordFromRootAndFunction C Major).Notes =! cMaj.Notes
            (chordFromRootAndFunction C Augmented).Notes =! cAug.Notes
            (chordFromRootAndFunction C Minor).Notes =! cMin.Notes
            (chordFromRootAndFunction C Diminished).Notes =! cDim.Notes
            (chordFromRootAndFunction C Major7).Notes =! cMaj7.Notes
            (chordFromRootAndFunction C Augmented7).Notes =! cAug7.Notes
            (chordFromRootAndFunction C Minor7).Notes =! cMin7.Notes
            (chordFromRootAndFunction C Diminished7).Notes =! cDim7.Notes
            (chordFromRootAndFunction C Minor7b5).Notes =! cMin7b5.Notes
            (chordFromRootAndFunction C Major6).Notes =! c6.Notes
            (chordFromRootAndFunction C Major6Add9).Notes =! c6add9.Notes
            (chordFromRootAndFunction C Major6Flat5Add9).Notes =! c6flat5add9.Notes
            (chordFromRootAndFunction C Dominant7Flat5).Notes =! c7flat5.Notes
            (chordFromRootAndFunction C Dominant7Flat9).Notes =! c7flat9.Notes
            (chordFromRootAndFunction C Dominant7Sharp9).Notes =! c7sharp9.Notes
            (chordFromRootAndFunction C Dominant7Flat5Flat9).Notes =! c7flat5flat9.Notes
            (chordFromRootAndFunction C Dominant7Flat5Sharp9).Notes =! c7flat5sharp9.Notes
            (chordFromRootAndFunction C Dominant9).Notes =! c9.Notes
            (chordFromRootAndFunction C Dominant11).Notes =! c11.Notes
            (chordFromRootAndFunction C Dominant13).Notes =! c13.Notes
            (chordFromRootAndFunction C Major9).Notes =! cMaj9.Notes
            (chordFromRootAndFunction C Major11).Notes =! cMaj11.Notes
            (chordFromRootAndFunction C Major13).Notes =! cMaj13.Notes
            (chordFromRootAndFunction C Major9Sharp11).Notes =! cMaj9Sharp11.Notes
            (chordFromRootAndFunction C Major13Sharp11).Notes =! cMaj13sharp11.Notes
            (chordFromRootAndFunction C Minor6).Notes =! cMin6.Notes
            (chordFromRootAndFunction C Minor6Add9).Notes =! cMin6add9.Notes
            (chordFromRootAndFunction C MinorMaj7).Notes =! cMinMaj7.Notes
            (chordFromRootAndFunction C Minor9).Notes =! cMin9.Notes
            (chordFromRootAndFunction C MinorMaj9).Notes =! cMinMaj9.Notes

        [<Test>]
        let ``Should filter function from chord``() =
            (c9 |> skipFunction Fifth).Notes =! [(C, Root); (E, Third); (BFlat, Seventh); (D, Ninth)] 
            (c9 |> skipFunction Root).Notes =! [(E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth)] 
            (c9 |> skipFunction Seventh).Notes =! [(C, Root); (E, Third); (G, Fifth); (D, Ninth)] 

        [<Test>]
        let ``Should invert chord for first inversion``() =
            (invert cMaj).Notes =! [(E, Third); (G, Fifth); (C, Root)] 
            (invert cAug).Notes =! [(E, Third); (GSharp, Fifth); (C, Root)] 
            (invert cMin).Notes =! [(EFlat, Third); (G, Fifth); (C, Root)] 
            (invert cDim).Notes =! [(EFlat, Third); (GFlat, Fifth); (C, Root)] 
            (invert cMaj7).Notes =! [(E, Third); (G, Fifth); (B, Seventh); (C, Root)] 
            (invert cAug7).Notes =! [(E, Third); (GSharp, Fifth); (B, Seventh); (C, Root)] 
            (invert cMin7).Notes =! [(EFlat, Third); (G, Fifth); (BFlat, Seventh); (C, Root)] 
            (invert cDim7).Notes =! [(EFlat, Third); (GFlat, Fifth); (A, Seventh); (C, Root)] 
            (invert cMin7b5).Notes =! [(EFlat, Third); (GFlat, Fifth); (BFlat, Seventh); (C, Root)] 

        [<Test>]
        let ``Should invert chord for second inversion``() =
            (cMaj |> invert |> invert).Notes =! [(G, Fifth); (C, Root); (E, Third)] 
            (cAug |> invert |> invert).Notes =! [(GSharp, Fifth); (C, Root); (E, Third)] 
            (cMin |> invert |> invert).Notes =! [(G, Fifth); (C, Root); (EFlat, Third)] 
            (cDim |> invert |> invert).Notes =! [(GFlat, Fifth); (C, Root); (EFlat, Third)] 
            (cMaj7 |> invert |> invert).Notes =! [(G, Fifth); (B, Seventh); (C, Root); (E, Third)] 
            (cAug7 |> invert |> invert).Notes =! [(GSharp, Fifth); (B, Seventh); (C, Root); (E, Third)] 
            (cMin7 |> invert |> invert).Notes =! [(G, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)] 
            (cDim7 |> invert |> invert).Notes =! [(GFlat, Fifth); (A, Seventh); (C, Root); (EFlat, Third)] 
            (cMin7b5 |> invert |> invert).Notes =! [(GFlat, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)] 

        [<Test>]
        let ``Should invert chord for third inversion``() =
            (cMaj7 |> invert |> invert |> invert).Notes =! [(B, Seventh); (C, Root); (E, Third); (G, Fifth)] 
            (cAug7 |> invert |> invert |> invert).Notes =! [(B, Seventh); (C, Root); (E, Third); (GSharp, Fifth)] 
            (cMin7 |> invert |> invert |> invert).Notes =! [(BFlat, Seventh); (C, Root); (EFlat, Third); (G, Fifth)] 
            (cDim7 |> invert |> invert |> invert).Notes =! [(A, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)] 
            (cMin7b5 |> invert |> invert |> invert).Notes =! [(BFlat, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)] 

        [<Test>]
        let ``Should loop inversions``() =
            cMaj7 |> invert |> invert |> invert |> invert =! cMaj7 
            cAug7 |> invert |> invert |> invert |> invert =! cAug7 

        [<Test>]
        let ``Should transform chord to drop2``() =
            (cMaj7 |> toDrop2).Notes =! [(C, Root); (G, Fifth); (B, Seventh); (E, Third); ] 

        [<Test>]
        let ``Should transform chord to drop3``() =
            (cMaj7 |> toDrop3).Notes =! [(C, Root); (B, Seventh); (E, Third); (G, Fifth)] 

        [<Test>]
        let ``Should invert drop2``() =
            (cMaj7 |> toDrop2 |> invert).Notes =! [(E, Third); (B, Seventh); (C, Root); (G, Fifth);] 
            (cMaj7 |> toDrop2 |> invert |> invert).Notes =! [(G, Fifth); (C, Root); (E, Third); (B, Seventh);] 
            (cMaj7 |> toDrop2 |> invert |> invert |> invert ).Notes =! [(B, Seventh); (E, Third); (G, Fifth); (C, Root); ] 
            (cMaj7 |> toDrop2 |> invert |> invert |> invert |> invert).Notes =! [(C, Root); (G, Fifth); (B, Seventh); (E, Third);] 

        [<Test>]
        let ``Should invert drop3``() =
            (cMaj7 |> toDrop3 |> invert).Notes =! [(E, Third); (C, Root); (G, Fifth); (B, Seventh)] 
            (cMaj7 |> toDrop3 |> invert |> invert).Notes =! [(G, Fifth); (E, Third); (B, Seventh); (C, Root);] 
            (cMaj7 |> toDrop3 |> invert |> invert |> invert).Notes =! [(B, Seventh); (G, Fifth); (C, Root); (E, Third);] 
            (cMaj7 |> toDrop3 |> invert |> invert |> invert |> invert).Notes =! [(C, Root); (B, Seventh); (E, Third); (G, Fifth)] 

        [<Test>]
        let ``Should choose invertion that satisfies having a specific function as lead``() =
            (inversionForFunctionAsLead cMaj Third).Notes =! (cMaj |> invert |> invert).Notes

        [<Test>]
        let ``Should choose invertion that satisfies having a specific function as bass``() =
            (inversionForFunctionAsBass cMaj Fifth).Notes =! (cMaj |> invert |> invert).Notes

        [<Test>]
        let ``Should choose invertion that satisfies having a lead that is closest to a provided note``() =
            (invertionWithLeadClosestToNote cMaj A).Notes =! (cMaj).Notes
            (invertionWithLeadClosestToNote cMaj CSharp).Notes =! (cMaj |> invert).Notes
            (invertionWithLeadClosestToNote cMaj F).Notes =! (cMaj |> invert |> invert).Notes

        [<Test>]
        let ``Should choose invertion that satisfies having a bass that is closest to a provided note``() =
            (invertionWithBassClosestToNote cMaj CSharp).Notes =! (cMaj).Notes
            (invertionWithBassClosestToNote cMaj F).Notes =! (cMaj |> invert).Notes
            (invertionWithBassClosestToNote cMaj A).Notes =! (cMaj |> invert |> invert).Notes           

    module ScalesHormonizerTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        
        let chord = {Notes= []; ChordType=Closed; Name=""}
        
        let cMaj = {chord with Notes= [(C, Root); (E, Third); (G, Fifth)]}
        let dMin = {chord with Notes= [(D, Root); (F, Third); (A, Fifth)]}
        let eMin = {chord with Notes= [(E, Root); (G, Third); (B, Fifth)]}
        let fMaj = {chord with Notes= [(F, Root); (A, Third); (C, Fifth)]}
        let gMaj = {chord with Notes= [(G, Root); (B, Third); (D, Fifth)]}
        let aMin = {chord with Notes= [(A, Root); (C, Third); (E, Fifth)]}
        let bDim = {chord with Notes= [(B, Root); (D, Third); (F, Fifth)]}

        let cMin = {chord with Notes= [(C, Root); (EFlat, Third); (G, Fifth)]}
        let dDim = {chord with Notes= [(D, Root); (F, Third); (AFlat, Fifth)]}
        let eFlatAug = {chord with Notes= [(EFlat, Root); (G, Third); (B, Fifth)]}
        let fMin = {chord with Notes= [(F, Root); (AFlat, Third); (C, Fifth)]}
        let aFlatMaj = {chord with Notes= [(AFlat, Root); (C, Third); (EFlat, Fifth)]}

        let cMaj7 = {chord with Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]}
        let dMin7 = {chord with Notes= [(D, Root); (F, Third); (A, Fifth); (C, Seventh)]}
        let eMin7 = {chord with Notes= [(E, Root); (G, Third); (B, Fifth); (D, Seventh)]}
        let fMaj7 = {chord with Notes= [(F, Root); (A, Third); (C, Fifth); (E, Seventh)]}
        let gDom7 = {chord with Notes= [(G, Root); (B, Third); (D, Fifth); (F, Seventh)]}
        let aMin7 = {chord with Notes= [(A, Root); (C, Third); (E, Fifth); (G, Seventh)]}
        let bMin7b5 = {chord with Notes= [(B, Root); (D, Third); (F, Fifth); (A, Seventh)]}

        let cMaj9 = {chord with Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (D, Ninth)]}
        let dMin9 = {chord with Notes= [(D, Root); (F, Third); (A, Fifth); (C, Seventh); (E, Ninth)]}
        let eMin9 = {chord with Notes= [(E, Root); (G, Third); (B, Fifth); (D, Seventh); (F, Ninth)]}
        let fMaj9 = {chord with Notes= [(F, Root); (A, Third); (C, Fifth); (E, Seventh); (G, Ninth)]}
        let gDom9 = {chord with Notes= [(G, Root); (B, Third); (D, Fifth); (F, Seventh); (A, Ninth)]}
        let aMin9 = {chord with Notes= [(A, Root); (C, Third); (E, Fifth); (G, Seventh); (B, Ninth)]}
        let bMin9b5 = {chord with Notes= [(B, Root); (D, Third); (F, Fifth); (A, Seventh); (C, Ninth)]}

        let cMinMaj7 = {chord with Notes= [(C, Root); (EFlat, Third); (G, Fifth); (B, Seventh)]}
        let dMin7b5 = {chord with Notes= [(D, Root); (F, Third); (AFlat, Fifth); (C, Seventh)]}
        let eFlatAug7 = {chord with Notes= [(EFlat, Root); (G, Third); (B, Fifth); (D, Seventh)]}
        let fMin7 = {chord with Notes= [(F, Root); (AFlat, Third); (C, Fifth); (EFlat, Seventh)]}
        let aFlatMaj7 = {chord with Notes= [(AFlat, Root); (C, Third); (EFlat, Fifth); (G, Seventh)]}
        let bDim7 = {chord with Notes= [(B, Root); (D, Third); (F, Fifth); (AFlat, Seventh)]}

        [<Test>]
        let ``Should create triads for Ionian scale`` () =
            let cIonian = createScale Ionian C
            (triadsHarmonizer ScaleDgrees.I cIonian).Notes =! cMaj.Notes
            (triadsHarmonizer ScaleDgrees.II cIonian).Notes =! dMin.Notes
            (triadsHarmonizer ScaleDgrees.III cIonian).Notes =! eMin.Notes
            (triadsHarmonizer ScaleDgrees.IV cIonian).Notes =! fMaj.Notes
            (triadsHarmonizer ScaleDgrees.V cIonian).Notes =! gMaj.Notes
            (triadsHarmonizer ScaleDgrees.VI cIonian).Notes =! aMin.Notes
            (triadsHarmonizer ScaleDgrees.VII cIonian).Notes =! bDim.Notes

        [<Test>]
        let ``Should create triads for Harmonic Minor scale`` () =
            let cMinor = createScale HarmonicMinor C
            (triadsHarmonizer ScaleDgrees.I cMinor).Notes =! cMin.Notes
            (triadsHarmonizer ScaleDgrees.II cMinor).Notes =! dDim.Notes
            (triadsHarmonizer ScaleDgrees.III cMinor).Notes =! eFlatAug.Notes
            (triadsHarmonizer ScaleDgrees.IV cMinor).Notes =! fMin.Notes
            (triadsHarmonizer ScaleDgrees.V cMinor).Notes =! gMaj.Notes
            (triadsHarmonizer ScaleDgrees.VI cMinor).Notes =! aFlatMaj.Notes
            (triadsHarmonizer ScaleDgrees.VII cMinor).Notes =! bDim.Notes

        [<Test>]
        let ``Should create seventh chords for Ionian scale`` () =
            let cIonian = createScale Ionian C
            (seventhsHarmonizer ScaleDgrees.I cIonian).Notes =! cMaj7.Notes
            (seventhsHarmonizer ScaleDgrees.II cIonian).Notes =! dMin7.Notes
            (seventhsHarmonizer ScaleDgrees.III cIonian).Notes =! eMin7.Notes
            (seventhsHarmonizer ScaleDgrees.IV cIonian).Notes =! fMaj7.Notes
            (seventhsHarmonizer ScaleDgrees.V cIonian).Notes =! gDom7.Notes
            (seventhsHarmonizer ScaleDgrees.VI cIonian).Notes =! aMin7.Notes
            (seventhsHarmonizer ScaleDgrees.VII cIonian).Notes =! bMin7b5.Notes
            
        [<Test>]
        let ``Should create seventh chords for Harmonic Minor scale`` () =
            let cMinor = createScale HarmonicMinor C
            (seventhsHarmonizer ScaleDgrees.I cMinor).Notes =! cMinMaj7.Notes
            (seventhsHarmonizer ScaleDgrees.II cMinor).Notes =! dMin7b5.Notes
            (seventhsHarmonizer ScaleDgrees.III cMinor).Notes =! eFlatAug7.Notes
            (seventhsHarmonizer ScaleDgrees.IV cMinor).Notes =! fMin7.Notes
            (seventhsHarmonizer ScaleDgrees.V cMinor).Notes =! gDom7.Notes
            (seventhsHarmonizer ScaleDgrees.VI cMinor).Notes =! aFlatMaj7.Notes
            (seventhsHarmonizer ScaleDgrees.VII cMinor).Notes =! bDim7.Notes

        [<Test>]
        let ``Should create ninth chords for Ionian scale`` () =
            let cIonian = createScale Ionian C
            (ninthsHarmonizer ScaleDgrees.I cIonian).Notes =! cMaj9.Notes
            (ninthsHarmonizer ScaleDgrees.II cIonian).Notes =! dMin9.Notes
            (ninthsHarmonizer ScaleDgrees.III cIonian).Notes =! eMin9.Notes
            (ninthsHarmonizer ScaleDgrees.IV cIonian).Notes =! fMaj9.Notes
            (ninthsHarmonizer ScaleDgrees.V cIonian).Notes =! gDom9.Notes
            (ninthsHarmonizer ScaleDgrees.VI cIonian).Notes =! aMin9.Notes
            (ninthsHarmonizer ScaleDgrees.VII cIonian).Notes =! bMin9b5.Notes

    module GuitarTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales

        let cIonian = createScale Ionian C
        let cMaj = triadsHarmonizer ScaleDgrees.I cIonian

        [<Test>]
        let ``Should map note to fret on sixth string``() =
            fretForNote E SixthString =! 0
            fretForNote F SixthString =! 1
            fretForNote GFlat SixthString =! 2
            fretForNote FSharp SixthString =! 2
            fretForNote G SixthString =! 3
            fretForNote AFlat SixthString =! 4
            fretForNote GSharp SixthString =! 4
            fretForNote A SixthString =! 5
            fretForNote BFlat SixthString =! 6
            fretForNote ASharp SixthString =! 6
            fretForNote B SixthString =! 7
            fretForNote C SixthString =! 8
            fretForNote DFlat SixthString =! 9
            fretForNote CSharp SixthString =! 9
            fretForNote D SixthString =! 10
            fretForNote EFlat SixthString =! 11
            fretForNote DSharp SixthString =! 11

        [<Test>]
        let ``Should map note to fret on fifth string``() =
            fretForNote A FifthString =! 0
            fretForNote BFlat FifthString =! 1
            fretForNote ASharp FifthString =! 1
            fretForNote B FifthString =! 2
            fretForNote C FifthString =! 3
            fretForNote DFlat FifthString =! 4
            fretForNote CSharp FifthString =! 4
            fretForNote D FifthString =! 5
            fretForNote EFlat FifthString =! 6
            fretForNote DSharp FifthString =! 6
            fretForNote E FifthString =! 7
            fretForNote F FifthString =! 8
            fretForNote GFlat FifthString =! 9
            fretForNote FSharp FifthString =! 9
            fretForNote G FifthString =! 10
            fretForNote AFlat FifthString =! 11
            fretForNote GSharp FifthString =! 11

        [<Test>]
        let ``Should map note to fret on fourth string``() =
            fretForNote D FourthString =! 0
            fretForNote EFlat FourthString =! 1
            fretForNote DSharp FourthString =! 1
            fretForNote E FourthString =! 2
            fretForNote F FourthString =! 3
            fretForNote GFlat FourthString =! 4
            fretForNote FSharp FourthString =! 4
            fretForNote G FourthString =! 5
            fretForNote AFlat FourthString =! 6
            fretForNote GSharp FourthString =! 6
            fretForNote A FourthString =! 7
            fretForNote BFlat FourthString =! 8
            fretForNote ASharp FourthString =! 8
            fretForNote B FourthString =! 9
            fretForNote C FourthString =! 10
            fretForNote DFlat FourthString =! 11
            fretForNote CSharp FourthString =! 11

        [<Test>]
        let ``Should map note to fret on third string``() =
            fretForNote G ThirdString =! 0
            fretForNote AFlat ThirdString =! 1
            fretForNote GSharp ThirdString =! 1
            fretForNote A ThirdString =! 2
            fretForNote BFlat ThirdString =! 3
            fretForNote ASharp ThirdString =! 3
            fretForNote B ThirdString =! 4
            fretForNote C ThirdString =! 5
            fretForNote DFlat ThirdString =! 6
            fretForNote CSharp ThirdString =! 6
            fretForNote D ThirdString =! 7
            fretForNote EFlat ThirdString =! 8
            fretForNote DSharp ThirdString =! 8
            fretForNote E ThirdString =! 9
            fretForNote F ThirdString =! 10
            fretForNote GFlat ThirdString =! 11
            fretForNote FSharp ThirdString =! 11

        [<Test>]
        let ``Should map note to fret on second string``() =
            fretForNote B SecondString =! 0
            fretForNote C SecondString =! 1
            fretForNote DFlat SecondString =! 2
            fretForNote CSharp SecondString =! 2
            fretForNote D SecondString =! 3
            fretForNote EFlat SecondString =! 4
            fretForNote DSharp SecondString =! 4
            fretForNote E SecondString =! 5
            fretForNote F SecondString =! 6
            fretForNote GFlat SecondString =! 7
            fretForNote FSharp SecondString =! 7
            fretForNote G SecondString =! 8
            fretForNote AFlat SecondString =! 9
            fretForNote GSharp SecondString =! 9
            fretForNote A SecondString =! 10
            fretForNote BFlat SecondString =! 11
            fretForNote ASharp SecondString =! 11
        
        [<Test>]
        let ``Should map note to fret on first string``() =
            fretForNote E FirstString =! 0
            fretForNote F FirstString =! 1
            fretForNote GFlat FirstString =! 2
            fretForNote FSharp FirstString =! 2
            fretForNote G FirstString =! 3
            fretForNote AFlat FirstString =! 4
            fretForNote GSharp FirstString =! 4
            fretForNote A FirstString =! 5
            fretForNote BFlat FirstString =! 6
            fretForNote ASharp FirstString =! 6
            fretForNote B FirstString =! 7
            fretForNote C FirstString =! 8
            fretForNote DFlat FirstString =! 9
            fretForNote CSharp FirstString =! 9
            fretForNote D FirstString =! 10
            fretForNote EFlat FirstString =! 11
            fretForNote DSharp FirstString =! 11

        [<Test>]
        let ``Should map c major to guitar fretboard``() =
            (chordToGuitarChord SixthString cMaj).Frets =! [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=7; Note=E};
                        {GuitarString=FourthString; Fret=5; Note=G};
                    ]

        [<Test>]
        let ``Should map c major to guitar fretboard on fifth string``() =
            (chordToGuitarChord FifthString cMaj).Frets =! [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=0; Note=G};
                    ]
        
        [<Test>]
        let ``Should map c major to guitar fretboard on fourth string``() =
            (chordToGuitarChord FourthString cMaj).Frets =! [
                        {GuitarString=FourthString; Fret=10; Note=C};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]
        
        [<Test>]
        let ``Should map c major to guitar fretboard on third string``() =
            (chordToGuitarChord ThirdString cMaj).Frets =! [
                        {GuitarString=ThirdString; Fret=5; Note=C};
                        {GuitarString=SecondString; Fret=5; Note=E};
                        {GuitarString=FirstString; Fret=3; Note=G};
                    ]

        [<Test>]
        let ``Should map c major to guitar fretboard on fifth string closed``() =
            (chordToGuitarClosedChord FifthString cMaj).Frets =! [
                        {GuitarString=FifthString; Fret=15; Note=C};
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=12; Note=G};
                    ]

        [<Test>]
        let ``Should map F major to guitar fretboard on sixth string closed``() =
            let fMaj = triadsHarmonizer ScaleDgrees.IV cIonian 
            (chordToGuitarClosedChord SixthString fMaj).Frets =! [
                        {GuitarString=SixthString; Fret=13; Note=F};
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=10; Note=C};
                    ]

        [<Test>]
        let ``Should map D major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian D
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            (chordToGuitarClosedChord FourthString chord).Frets =! [
                        {GuitarString=FourthString; Fret=12; Note=D};
                        {GuitarString=ThirdString; Fret=11; Note=FSharp};
                        {GuitarString=SecondString; Fret=10; Note=A};
                        {GuitarString=FirstString; Fret=9; Note=CSharp};
                    ]

        [<Test>]
        let ``Should map EFlat major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian EFlat
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            (chordToGuitarClosedChord FourthString chord).Frets =! [
                        {GuitarString=FourthString; Fret=13; Note=EFlat};
                        {GuitarString=ThirdString; Fret=12; Note=G};
                        {GuitarString=SecondString; Fret=11; Note=ASharp};
                        {GuitarString=FirstString; Fret=10; Note=D};
                    ]

        [<Test>]
        let ``Should map E major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian E
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            (chordToGuitarClosedChord FourthString chord).Frets =! [
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=13; Note=GSharp};
                        {GuitarString=SecondString; Fret=12; Note=B};
                        {GuitarString=FirstString; Fret=11; Note=DSharp};
                    ]
        
        [<Test>]
        let ``Should map F major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian F
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            (chordToGuitarClosedChord FourthString chord).Frets =! [
                        {GuitarString=FourthString; Fret=15; Note=F};
                        {GuitarString=ThirdString; Fret=14; Note=A};
                        {GuitarString=SecondString; Fret=13; Note=C};
                        {GuitarString=FirstString; Fret=12; Note=E};
                    ]

        [<Test>]
        let ``Should map C major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let chord = seventhsHarmonizer ScaleDgrees.I cIonian |> toDrop2
            (chordToGuitarClosedChord FifthString chord).Frets =! [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=5; Note=G};
                        {GuitarString=ThirdString; Fret=4; Note=B};
                        {GuitarString=SecondString; Fret=5; Note=E};
                    ]

        [<Test>]
        let ``Should map A major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let scale = createScale Ionian A
            let chord = seventhsHarmonizer ScaleDgrees.I scale |> toDrop2
            (chordToGuitarClosedChord FifthString chord).Frets =! [
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=13; Note=GSharp};
                        {GuitarString=SecondString; Fret=14; Note=CSharp};
                    ]

        [<Test>]
        let ``Should map C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            let chord = seventhsHarmonizer ScaleDgrees.I cIonian |> toDrop3
            (chordToGuitarClosedChord SixthString chord).Frets =! [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=(-1); Note=A};
                        {GuitarString=FourthString; Fret=9; Note=B};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]

        [<Test>]
        let ``Should map C major9 ignoring 5th to guitar fretboard on fifth string closed``() =
            let chord = chordFromRootAndFunction C Major9
                        |> skipFunction Fifth
            (chordToGuitarClosedChord FifthString chord).Frets =! [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=4; Note=B};
                        {GuitarString=SecondString; Fret=3; Note=D};
                    ]
        
        [<Test>]
        let ``Should map C9 ignoring 5th to guitar fretboard on fifth string closed``() =
            let chord = chordFromRootAndFunction C Dominant9
                        |> skipFunction Fifth
            (chordToGuitarClosedChord FifthString chord).Frets =! [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=3; Note=BFlat};
                        {GuitarString=SecondString; Fret=3; Note=D};
                    ]

    module GuitarTabTests =
        open System
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.GuitarTab
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales

        let cIonian = createScale Ionian C
        let cMaj = triadsHarmonizer ScaleDgrees.I cIonian

        [<Test>]
        let ``Should draw C major 7 drop 2 to guitar fretboard on fifth string closed ``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop2
                |> chordToGuitarClosedChord FifthString)
            guitarChord |> tabify =! "      CMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----5--------|" + Environment.NewLine +
                                            "G|----4--------|" + Environment.NewLine +
                                            "D|----5--------|" + Environment.NewLine +
                                            "A|----3--------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine

        [<Test>]
        let ``Should draw A major 7 to guitar fretboard on fifth string closed ``() =
            let guitarChord = 
                (createScale Ionian A
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop2
                |> chordToGuitarClosedChord FifthString)
            guitarChord |> tabify =! "      AMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----14-------|" + Environment.NewLine +
                                            "G|----13-------|" + Environment.NewLine +
                                            "D|----14-------|" + Environment.NewLine +
                                            "A|----12-------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine

        [<Test>]
        let ``Should draw F major 7 to guitar fretboard on fourth string closed``() =
            let guitarChord = 
                (createScale Ionian F
                |> seventhsHarmonizer ScaleDgrees.I
                |> chordToGuitarClosedChord FourthString)
            guitarChord |> tabify =! "      FMaj7   " + Environment.NewLine +
                                            "E|----12-------|" + Environment.NewLine +
                                            "B|----13-------|" + Environment.NewLine +
                                            "G|----14-------|" + Environment.NewLine +
                                            "D|----15-------|" + Environment.NewLine +
                                            "A|-------------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine

        [<Test>]
        let ``Should draw c major to guitar fretboard on sixth string``() =
            let guitarChord = chordToGuitarClosedChord SixthString cMaj
            guitarChord |> tabify =! "      CMaj   " + Environment.NewLine +
                                            "E|------------|" + Environment.NewLine +
                                            "B|------------|" + Environment.NewLine +
                                            "G|------------|" + Environment.NewLine +
                                            "D|----5-------|" + Environment.NewLine +
                                            "A|----7-------|" + Environment.NewLine +
                                            "E|----8-------|" + Environment.NewLine

        [<Test>]
        let ``Should draw C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord SixthString)
            guitarChord |> tabify =! "      CMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----8--------|" + Environment.NewLine +
                                            "G|----9--------|" + Environment.NewLine +
                                            "D|----9--------|" + Environment.NewLine +
                                            "A|-------------|" + Environment.NewLine +
                                            "E|----8--------|" + Environment.NewLine

        [<Test>]        
        let ``Should draw C major 7 drop 3 to guitar fretboard on fifth string closed``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord FifthString)
            guitarChord |> tabify =! "      CMaj7   " + Environment.NewLine +
                                            "E|----3--------|" + Environment.NewLine +
                                            "B|----5--------|" + Environment.NewLine +
                                            "G|----4--------|" + Environment.NewLine +
                                            "D|-------------|" + Environment.NewLine +
                                            "A|----3--------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine
                                                        
        [<Test>]
        let ``Should map C9 ignoring 5th to guitar fretboard on fifth string closed``() =
            let guitarChord = chordFromRootAndFunction C Dominant9 
                              |> skipFunction Fifth
                              |> chordToGuitarClosedChord FifthString

            guitarChord |> tabify =! "      C9   " + Environment.NewLine +
                                            "E|----------|" + Environment.NewLine +
                                            "B|----3-----|" + Environment.NewLine +
                                            "G|----3-----|" + Environment.NewLine +
                                            "D|----2-----|" + Environment.NewLine +
                                            "A|----3-----|" + Environment.NewLine +
                                            "E|----------|" + Environment.NewLine
            
        [<Test>]
        let ``Should tabify multiple chords``() =
            let cIonian = createScale Ionian C
            let cMaj7 = seventhsHarmonizer ScaleDgrees.I cIonian
            let dMin7 = seventhsHarmonizer ScaleDgrees.II cIonian
            let eMin7 = seventhsHarmonizer ScaleDgrees.III cIonian
            let fMaj7 = seventhsHarmonizer ScaleDgrees.IV cIonian

            let guitarChords =  
                [cMaj7; dMin7; eMin7; fMaj7] 
                |> List.map (
                    toDrop2 >> (chordToGuitarClosedChord FifthString))
            
            tabifyAll guitarChords =! 
                                "      CMaj7   DMin7   EMin7   FMaj7   " + Environment.NewLine +
                                "E|-------------------------------------|" + Environment.NewLine +
                                "B|----5-------6-------8-------10-------|" + Environment.NewLine +
                                "G|----4-------5-------7-------9--------|" + Environment.NewLine +
                                "D|----5-------7-------9-------10-------|" + Environment.NewLine +
                                "A|----3-------5-------7-------8--------|" + Environment.NewLine +
                                "E|-------------------------------------|" + Environment.NewLine 

        [<Test>]
        let ``Should draw shape of C major 7 drop 3 on sixth string``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord SixthString)
            guitarChord |> shapify =! "CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "8X998X" + Environment.NewLine


        [<Test>]
        let ``Should draw shape of C major 7 drop 2 on fifth string``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop2
                |> chordToGuitarClosedChord FifthString)
            guitarChord |> shapify =! "CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "X3545X" + Environment.NewLine

    module SpeechToMusicTests =
            open System
            open NUnit.Framework
            open Swensen.Unquote
            open Vaughan.Domain
            open Vaughan.Scales
            open Vaughan.SpeechToMusic
            open Vaughan.Guitar
            open Vaughan.GuitarTab
            open Vaughan.ScaleHarmonizer

            [<Test>]
            let ``Should parse textual representation of chord``() =
                (parseChord "C Major") =! { Root=C; Quality=Major }
                (parseChord "C# Maj") =! { Root=CSharp; Quality=Major }
                (parseChord "C minor") =! { Root=C; Quality=Minor }
                (parseChord "Db min") =! { Root=DFlat; Quality=Minor }
                (parseChord "Cmin") =! { Root=C; Quality=Minor }
                (parseChord "Cm") =! { Root=C; Quality=Minor }
                (parseChord "C augmented") =! { Root=C; Quality=Augmented }
                (parseChord "C Aug") =! { Root=C; Quality=Augmented }
                (parseChord "C diminished") =! { Root=C; Quality=Diminished }
                (parseChord "C dim") =! { Root=C; Quality=Diminished }
                (parseChord "C Major 7") =! { Root=C; Quality=Major7 }
                (parseChord "C Maj 7") =! { Root=C; Quality=Major7 }
                (parseChord "CMaj7") =! { Root=C; Quality=Major7 }
                (parseChord "C minor 7") =! { Root=C; Quality=Minor7 }
                (parseChord "C min 7") =! { Root=C; Quality=Minor7 }
                (parseChord "Cmin7") =! { Root=C; Quality=Minor7 }
                (parseChord "C augmented 7") =! { Root=C; Quality=Augmented7 }
                (parseChord "C aug 7") =! { Root=C; Quality=Augmented7 }
                (parseChord "C diminished 7") =! { Root=C; Quality=Diminished7 }
                (parseChord "C dim 7") =! { Root=C; Quality=Diminished7 }
                (parseChord "Caug7") =! { Root=C; Quality=Augmented7 }
                (parseChord "Cdom7") =! { Root=C; Quality=Dominant7 }
                (parseChord "C#7") =! { Root=CSharp; Quality=Dominant7 }
                             
            [<Test>]
            let ``Should create chord from chord intent``() =
                let cIonian = createScale Ionian C
                let cMaj = triadsHarmonizer ScaleDgrees.I cIonian
                let dMin = triadsHarmonizer ScaleDgrees.II cIonian
                let eMin = triadsHarmonizer ScaleDgrees.III cIonian
                let fMaj = triadsHarmonizer ScaleDgrees.IV cIonian
                let gMaj = triadsHarmonizer ScaleDgrees.V cIonian
                let aMin = triadsHarmonizer ScaleDgrees.VI cIonian
                let bDim = triadsHarmonizer ScaleDgrees.VII cIonian

                createChord { Root=C; Quality=Major } =! cMaj
                createChord { Root=D; Quality=Minor } =! dMin
                createChord { Root=E; Quality=Minor } =! eMin
                createChord { Root=F; Quality=Major } =! fMaj
                createChord { Root=G; Quality=Major } =! gMaj
                createChord { Root=A; Quality=Minor } =! aMin
                createChord { Root=B; Quality=Diminished } =! bDim
                
            [<Test>]
            let ``Should tabify chord from text``() =
                "A Major"
                        |> parseChord
                        |> createChord
                        |> chordToGuitarClosedChord SixthString
                        |> tabify =! "      AMaj   " + Environment.NewLine+
                                    "E|------------|" + Environment.NewLine +
                                    "B|------------|" + Environment.NewLine +
                                    "G|------------|" + Environment.NewLine + 
                                    "D|----2-------|" + Environment.NewLine + 
                                    "A|----4-------|" + Environment.NewLine + 
                                    "E|----5-------|" + Environment.NewLine
            [<Test>]
            let ``Should tabify open chord from text``() =                 
                "C Major"
                        |> parseChord
                        |> createChord
                        |> chordToGuitarChord FifthString
                        |> tabify =! "      CMaj   " + Environment.NewLine+
                                    "E|------------|" + Environment.NewLine +
                                    "B|------------|" + Environment.NewLine +
                                    "G|----0-------|" + Environment.NewLine + 
                                    "D|----2-------|" + Environment.NewLine + 
                                    "A|----3-------|" + Environment.NewLine + 
                                    "E|------------|" + Environment.NewLine