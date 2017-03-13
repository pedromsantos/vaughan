namespace VaughanTests

    module InfrastructureTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Infrastructure

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
        let ``Should filter odd index elements``() =
            test <@ filterOddIndexElements {1 .. 6} |> List.ofSeq = ({1 .. 2 .. 6} |> List.ofSeq) @>
 
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
        open Vaughan.Domain
        open Vaughan.Notes

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
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Keys

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
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Scales

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
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Chords
        open Vaughan.ChordVoiceLeading

        let cMaj = {Notes= [(C, Root); (E, Third); (G, Fifth)]; ChordType=Closed}
        let cAug = {Notes= [(C, Root); (E, Third); (GSharp, Fifth)]; ChordType=Closed}
        let cMin = {Notes= [(C, Root); (EFlat, Third); (G, Fifth)]; ChordType=Closed}
        let cDim = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth)]; ChordType=Closed}
        let cMaj7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed}
        let cAug7 = {Notes= [(C, Root); (E, Third); (GSharp, Fifth); (B, Seventh)]; ChordType=Closed}
        let cMin7 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (BFlat, Seventh)]; ChordType=Closed}
        let cDim7 = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (A, Seventh)]; ChordType=Closed}
        let cMin7b5 = {Notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (BFlat, Seventh)]; ChordType=Closed}
        let c6 = {Notes= [(C, Root); (E, Third); (G, Fifth); (A, Sixth)]; ChordType=Closed}
        let c6add9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (A, Sixth); (D, Ninth)]; ChordType=Closed}
        let c6flat5add9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (A, Sixth); (D, Ninth)]; ChordType=Closed}
        let c7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh)]; ChordType=Closed}
        let c7flat5 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh)]; ChordType=Closed}    
        let c9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth)]; ChordType=Closed}
        let c7flat9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (DFlat, Ninth)]; ChordType=Closed}
        let c7sharp9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (DSharp, Ninth)]; ChordType=Closed}
        let c7flat5flat9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh); (DFlat, Ninth)]; ChordType=Closed}    
        let c7flat5sharp9 = {Notes= [(C, Root); (E, Third); (GFlat, Fifth); (BFlat, Seventh); (DSharp, Ninth)]; ChordType=Closed}    
        let c11 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth); (F, Eleventh)]; ChordType=Closed}
        let c13 = {Notes= [(C, Root); (E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth); (F, Eleventh); (A, Thirteenth)]; ChordType=Closed}
        let cMaj9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (D, Ninth)]; ChordType=Closed}

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
            test <@ (name c6).StartsWith("C6") @>
            
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
            test <@ chordFromRootAndFunction C Major6 = c6 @>
            test <@ chordFromRootAndFunction C Major6Add9 = c6add9 @>
            test <@ chordFromRootAndFunction C Major6Flat5Add9 = c6flat5add9 @>
            test <@ chordFromRootAndFunction C Dominant7Flat5 = c7flat5 @>
            test <@ chordFromRootAndFunction C Dominant7Flat9 = c7flat9 @>
            test <@ chordFromRootAndFunction C Dominant7Sharp9 = c7sharp9 @>
            test <@ chordFromRootAndFunction C Dominant7Flat5Flat9 = c7flat5flat9 @>
            test <@ chordFromRootAndFunction C Dominant7Flat5Sharp9 = c7flat5sharp9 @>
            test <@ chordFromRootAndFunction C Dominant9 = c9 @>
            test <@ chordFromRootAndFunction C Dominant11 = c11 @>
            test <@ chordFromRootAndFunction C Dominant13 = c13 @>
            test <@ chordFromRootAndFunction C Major9 = cMaj9@>

        [<Test>]
        let ``Should invert chord for first inversion``() =
            test <@ (invert cMaj).Notes = [(E, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cAug).Notes = [(E, Third); (GSharp, Fifth); (C, Root)]  @>
            test <@ (invert cMin).Notes = [(EFlat, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cDim).Notes = [(EFlat, Third); (GFlat, Fifth); (C, Root)]  @>
            test <@ (invert cMaj7).Notes = [(E, Third); (G, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cAug7).Notes = [(E, Third); (GSharp, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cMin7).Notes = [(EFlat, Third); (G, Fifth); (BFlat, Seventh); (C, Root)]  @>
            test <@ (invert cDim7).Notes = [(EFlat, Third); (GFlat, Fifth); (A, Seventh); (C, Root)]  @>
            test <@ (invert cMin7b5).Notes = [(EFlat, Third); (GFlat, Fifth); (BFlat, Seventh); (C, Root)]  @>

        [<Test>]
        let ``Should invert chord for second inversion``() =
            test <@ (cMaj |> invert |> invert).Notes = [(G, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cAug |> invert |> invert).Notes = [(GSharp, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cMin |> invert |> invert).Notes = [(G, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim |> invert |> invert).Notes = [(GFlat, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cMaj7 |> invert |> invert).Notes = [(G, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cAug7 |> invert |> invert).Notes = [(GSharp, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cMin7 |> invert |> invert).Notes = [(G, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim7 |> invert |> invert).Notes = [(GFlat, Fifth); (A, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cMin7b5 |> invert |> invert).Notes = [(GFlat, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>

        [<Test>]
        let ``Should invert chord for third inversion``() =
            test <@ (cMaj7 |> invert |> invert |> invert).Notes = [(B, Seventh); (C, Root); (E, Third); (G, Fifth)]  @>
            test <@ (cAug7 |> invert |> invert |> invert).Notes = [(B, Seventh); (C, Root); (E, Third); (GSharp, Fifth)]  @>
            test <@ (cMin7 |> invert |> invert |> invert).Notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (G, Fifth)]  @>
            test <@ (cDim7 |> invert |> invert |> invert).Notes = [(A, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>
            test <@ (cMin7b5 |> invert |> invert |> invert).Notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>

        [<Test>]
        let ``Should loop inversions``() =
            test <@ cMaj7 |> invert |> invert |> invert |> invert = cMaj7  @>
            test <@ cAug7 |> invert |> invert |> invert |> invert = cAug7  @>

        [<Test>]
        let ``Should transform chord to drop2``() =
            test <@ (cMaj7 |> toDrop2).Notes = [(C, Root); (G, Fifth); (B, Seventh); (E, Third); ]  @>

        [<Test>]
        let ``Should transform chord to drop3``() =
            test <@ (cMaj7 |> toDrop3).Notes = [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]  @>

        [<Test>]
        let ``Should invert drop2``() =
            test <@ (cMaj7 |> toDrop2 |> invert).Notes = [(E, Third); (B, Seventh); (C, Root); (G, Fifth);]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert).Notes = [(G, Fifth); (C, Root); (E, Third); (B, Seventh);]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert |> invert ).Notes = [(B, Seventh); (E, Third); (G, Fifth); (C, Root); ]  @>
            test <@ (cMaj7 |> toDrop2 |> invert |> invert |> invert |> invert).Notes = [(C, Root); (G, Fifth); (B, Seventh); (E, Third);]  @>

        [<Test>]
        let ``Should invert drop3``() =
            test <@ (cMaj7 |> toDrop3 |> invert).Notes = [(E, Third); (C, Root); (G, Fifth); (B, Seventh)]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert).Notes = [(G, Fifth); (E, Third); (B, Seventh); (C, Root);]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert |> invert).Notes = [(B, Seventh); (G, Fifth); (C, Root); (E, Third);]  @>
            test <@ (cMaj7 |> toDrop3 |> invert |> invert |> invert |> invert).Notes = [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]  @>

        [<Test>]
        let ``Should choose invertion that satisfies having a specific function as lead``() =
            test<@ (inversionForFunctionAsLead cMaj Third).Notes = (cMaj |> invert |> invert).Notes @>

        [<Test>]
        let ``Should choose invertion that satisfies having a specific function as bass``() =
            test<@ (inversionForFunctionAsBass cMaj Fifth).Notes = (cMaj |> invert |> invert).Notes @>

        [<Test>]
        let ``Should choose invertion that satisfies having a lead that is closest to a provided note``() =
            test<@ (invertionWithLeadClosestToNote cMaj A).Notes = (cMaj).Notes @>
            test<@ (invertionWithLeadClosestToNote cMaj CSharp).Notes = (cMaj |> invert).Notes @>
            test<@ (invertionWithLeadClosestToNote cMaj F).Notes = (cMaj |> invert |> invert).Notes @>

        [<Test>]
        let ``Should choose invertion that satisfies having a bass that is closest to a provided note``() =
            test<@ (invertionWithBassClosestToNote cMaj CSharp).Notes = (cMaj).Notes @>
            test<@ (invertionWithBassClosestToNote cMaj F).Notes = (cMaj |> invert).Notes @>
            test<@ (invertionWithBassClosestToNote cMaj A).Notes = (cMaj |> invert |> invert).Notes @>           

    module ScalesHormonizerTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open Vaughan.Chords
        open Vaughan.Notes
        open Vaughan.Domain
        
        let cMaj = {Notes= [(C, Root); (E, Third); (G, Fifth)]; ChordType=Closed}
        let dMin = {Notes= [(D, Root); (F, Third); (A, Fifth)]; ChordType=Closed}
        let eMin = {Notes= [(E, Root); (G, Third); (B, Fifth)]; ChordType=Closed}
        let fMaj = {Notes= [(F, Root); (A, Third); (C, Fifth)]; ChordType=Closed}
        let gMaj = {Notes= [(G, Root); (B, Third); (D, Fifth)]; ChordType=Closed}
        let aMin = {Notes= [(A, Root); (C, Third); (E, Fifth)]; ChordType=Closed}
        let bDim = {Notes= [(B, Root); (D, Third); (F, Fifth)]; ChordType=Closed}

        let cMin = {Notes= [(C, Root); (EFlat, Third); (G, Fifth)]; ChordType=Closed}
        let dDim = {Notes= [(D, Root); (F, Third); (AFlat, Fifth)]; ChordType=Closed}
        let eFlatAug = {Notes= [(EFlat, Root); (G, Third); (B, Fifth)]; ChordType=Closed}
        let fMin = {Notes= [(F, Root); (AFlat, Third); (C, Fifth)]; ChordType=Closed}
        let aFlatMaj = {Notes= [(AFlat, Root); (C, Third); (EFlat, Fifth)]; ChordType=Closed}

        let cMaj7 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed}
        let dMin7 = {Notes= [(D, Root); (F, Third); (A, Fifth); (C, Seventh)]; ChordType=Closed}
        let eMin7 = {Notes= [(E, Root); (G, Third); (B, Fifth); (D, Seventh)]; ChordType=Closed}
        let fMaj7 = {Notes= [(F, Root); (A, Third); (C, Fifth); (E, Seventh)]; ChordType=Closed}
        let gDom7 = {Notes= [(G, Root); (B, Third); (D, Fifth); (F, Seventh)]; ChordType=Closed}
        let aMin7 = {Notes= [(A, Root); (C, Third); (E, Fifth); (G, Seventh)]; ChordType=Closed}
        let bMin7b5 = {Notes= [(B, Root); (D, Third); (F, Fifth); (A, Seventh)]; ChordType=Closed}

        let cMaj9 = {Notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh); (D, Ninth)]; ChordType=Closed}
        let dMin9 = {Notes= [(D, Root); (F, Third); (A, Fifth); (C, Seventh); (E, Ninth)]; ChordType=Closed}
        let eMin9 = {Notes= [(E, Root); (G, Third); (B, Fifth); (D, Seventh); (F, Ninth)]; ChordType=Closed}
        let fMaj9 = {Notes= [(F, Root); (A, Third); (C, Fifth); (E, Seventh); (G, Ninth)]; ChordType=Closed}
        let gDom9 = {Notes= [(G, Root); (B, Third); (D, Fifth); (F, Seventh); (A, Ninth)]; ChordType=Closed}
        let aMin9 = {Notes= [(A, Root); (C, Third); (E, Fifth); (G, Seventh); (B, Ninth)]; ChordType=Closed}
        let bMin9b5 = {Notes= [(B, Root); (D, Third); (F, Fifth); (A, Seventh); (C, Ninth)]; ChordType=Closed}

        let cMinMaj7 = {Notes= [(C, Root); (EFlat, Third); (G, Fifth); (B, Seventh)]; ChordType=Closed}
        let dMin7b5 = {Notes= [(D, Root); (F, Third); (AFlat, Fifth); (C, Seventh)]; ChordType=Closed}
        let eFlatAug7 = {Notes= [(EFlat, Root); (G, Third); (B, Fifth); (D, Seventh)]; ChordType=Closed}
        let fMin7 = {Notes= [(F, Root); (AFlat, Third); (C, Fifth); (EFlat, Seventh)]; ChordType=Closed}
        let aFlatMaj7 = {Notes= [(AFlat, Root); (C, Third); (EFlat, Fifth); (G, Seventh)]; ChordType=Closed}
        let bDim7 = {Notes= [(B, Root); (D, Third); (F, Fifth); (AFlat, Seventh)]; ChordType=Closed}

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

        [<Test>]
        let ``Should create ninth chords for Ionian scale`` () =
            let cIonian = createScale Ionian C
            test <@ ninthsHarmonizer ScaleDgrees.I cIonian = cMaj9 @>
            test <@ ninthsHarmonizer ScaleDgrees.II cIonian = dMin9 @>
            test <@ ninthsHarmonizer ScaleDgrees.III cIonian = eMin9 @>
            test <@ ninthsHarmonizer ScaleDgrees.IV cIonian = fMaj9 @>
            test <@ ninthsHarmonizer ScaleDgrees.V cIonian = gDom9 @>
            test <@ ninthsHarmonizer ScaleDgrees.VI cIonian = aMin9 @>
            test <@ ninthsHarmonizer ScaleDgrees.VII cIonian = bMin9b5 @>

    module GuitarTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales

        let cIonian = createScale Ionian C
        let cMaj = triadsHarmonizer ScaleDgrees.I cIonian

        [<Test>]
        let ``Should map note to fret on sixth string``() =
            test <@ fretForNote E SixthString = 0 @>
            test <@ fretForNote F SixthString = 1 @>
            test <@ fretForNote GFlat SixthString = 2 @>
            test <@ fretForNote FSharp SixthString = 2 @>
            test <@ fretForNote G SixthString = 3 @>
            test <@ fretForNote AFlat SixthString = 4 @>
            test <@ fretForNote GSharp SixthString = 4 @>
            test <@ fretForNote A SixthString = 5 @>
            test <@ fretForNote BFlat SixthString = 6 @>
            test <@ fretForNote ASharp SixthString = 6 @>
            test <@ fretForNote B SixthString = 7 @>
            test <@ fretForNote C SixthString = 8 @>
            test <@ fretForNote DFlat SixthString = 9 @>
            test <@ fretForNote CSharp SixthString = 9 @>
            test <@ fretForNote D SixthString = 10 @>
            test <@ fretForNote EFlat SixthString = 11 @>
            test <@ fretForNote DSharp SixthString = 11 @>

        [<Test>]
        let ``Should map note to fret on fifth string``() =
            test <@ fretForNote A FifthString = 0 @>
            test <@ fretForNote BFlat FifthString = 1 @>
            test <@ fretForNote ASharp FifthString = 1 @>
            test <@ fretForNote B FifthString = 2 @>
            test <@ fretForNote C FifthString = 3 @>
            test <@ fretForNote DFlat FifthString = 4 @>
            test <@ fretForNote CSharp FifthString = 4 @>
            test <@ fretForNote D FifthString = 5 @>
            test <@ fretForNote EFlat FifthString = 6 @>
            test <@ fretForNote DSharp FifthString = 6 @>
            test <@ fretForNote E FifthString = 7 @>
            test <@ fretForNote F FifthString = 8 @>
            test <@ fretForNote GFlat FifthString = 9 @>
            test <@ fretForNote FSharp FifthString = 9 @>
            test <@ fretForNote G FifthString = 10 @>
            test <@ fretForNote AFlat FifthString = 11 @>
            test <@ fretForNote GSharp FifthString = 11 @>

        [<Test>]
        let ``Should map note to fret on fourth string``() =
            test <@ fretForNote D FourthString = 0 @>
            test <@ fretForNote EFlat FourthString = 1 @>
            test <@ fretForNote DSharp FourthString = 1 @>
            test <@ fretForNote E FourthString = 2 @>
            test <@ fretForNote F FourthString = 3 @>
            test <@ fretForNote GFlat FourthString = 4 @>
            test <@ fretForNote FSharp FourthString = 4 @>
            test <@ fretForNote G FourthString = 5 @>
            test <@ fretForNote AFlat FourthString = 6 @>
            test <@ fretForNote GSharp FourthString = 6 @>
            test <@ fretForNote A FourthString = 7 @>
            test <@ fretForNote BFlat FourthString = 8 @>
            test <@ fretForNote ASharp FourthString = 8 @>
            test <@ fretForNote B FourthString = 9 @>
            test <@ fretForNote C FourthString = 10 @>
            test <@ fretForNote DFlat FourthString = 11 @>
            test <@ fretForNote CSharp FourthString = 11 @>

        [<Test>]
        let ``Should map note to fret on third string``() =
            test <@ fretForNote G ThirdString = 0 @>
            test <@ fretForNote AFlat ThirdString = 1 @>
            test <@ fretForNote GSharp ThirdString = 1 @>
            test <@ fretForNote A ThirdString = 2 @>
            test <@ fretForNote BFlat ThirdString = 3 @>
            test <@ fretForNote ASharp ThirdString = 3 @>
            test <@ fretForNote B ThirdString = 4 @>
            test <@ fretForNote C ThirdString = 5 @>
            test <@ fretForNote DFlat ThirdString = 6 @>
            test <@ fretForNote CSharp ThirdString = 6 @>
            test <@ fretForNote D ThirdString = 7 @>
            test <@ fretForNote EFlat ThirdString = 8 @>
            test <@ fretForNote DSharp ThirdString = 8 @>
            test <@ fretForNote E ThirdString = 9 @>
            test <@ fretForNote F ThirdString = 10 @>
            test <@ fretForNote GFlat ThirdString = 11 @>
            test <@ fretForNote FSharp ThirdString = 11 @>

        [<Test>]
        let ``Should map note to fret on second string``() =
            test <@ fretForNote B SecondString = 0 @>
            test <@ fretForNote C SecondString = 1 @>
            test <@ fretForNote DFlat SecondString = 2 @>
            test <@ fretForNote CSharp SecondString = 2 @>
            test <@ fretForNote D SecondString = 3 @>
            test <@ fretForNote EFlat SecondString = 4 @>
            test <@ fretForNote DSharp SecondString = 4 @>
            test <@ fretForNote E SecondString = 5 @>
            test <@ fretForNote F SecondString = 6 @>
            test <@ fretForNote GFlat SecondString = 7 @>
            test <@ fretForNote FSharp SecondString = 7 @>
            test <@ fretForNote G SecondString = 8 @>
            test <@ fretForNote AFlat SecondString = 9 @>
            test <@ fretForNote GSharp SecondString = 9 @>
            test <@ fretForNote A SecondString = 10 @>
            test <@ fretForNote BFlat SecondString = 11 @>
            test <@ fretForNote ASharp SecondString = 11 @>
        
        [<Test>]
        let ``Should map note to fret on first string``() =
            test <@ fretForNote E FirstString = 0 @>
            test <@ fretForNote F FirstString = 1 @>
            test <@ fretForNote GFlat FirstString = 2 @>
            test <@ fretForNote FSharp FirstString = 2 @>
            test <@ fretForNote G FirstString = 3 @>
            test <@ fretForNote AFlat FirstString = 4 @>
            test <@ fretForNote GSharp FirstString = 4 @>
            test <@ fretForNote A FirstString = 5 @>
            test <@ fretForNote BFlat FirstString = 6 @>
            test <@ fretForNote ASharp FirstString = 6 @>
            test <@ fretForNote B FirstString = 7 @>
            test <@ fretForNote C FirstString = 8 @>
            test <@ fretForNote DFlat FirstString = 9 @>
            test <@ fretForNote CSharp FirstString = 9 @>
            test <@ fretForNote D FirstString = 10 @>
            test <@ fretForNote EFlat FirstString = 11 @>
            test <@ fretForNote DSharp FirstString = 11 @>

        [<Test>]
        let ``Should map c major to guitar fretboard``() =
            test <@ (chordToGuitarChord SixthString cMaj).Frets = [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=7; Note=E};
                        {GuitarString=FourthString; Fret=5; Note=G};
                    ]@>

        [<Test>]
        let ``Should map c major to guitar fretboard on fifth string``() =
            test <@ (chordToGuitarChord FifthString cMaj).Frets = [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=0; Note=G};
                    ]@>
        
        [<Test>]
        let ``Should map c major to guitar fretboard on fourth string``() =
            test <@ (chordToGuitarChord FourthString cMaj).Frets = [
                        {GuitarString=FourthString; Fret=10; Note=C};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]@>
        
        [<Test>]
        let ``Should map c major to guitar fretboard on third string``() =
            test <@ (chordToGuitarChord ThirdString cMaj).Frets = [
                        {GuitarString=ThirdString; Fret=5; Note=C};
                        {GuitarString=SecondString; Fret=5; Note=E};
                        {GuitarString=FirstString; Fret=3; Note=G};
                    ]@>

        [<Test>]
        let ``Should map c major to guitar fretboard on fifth string closed``() =
            test <@ (chordToGuitarClosedChord FifthString cMaj).Frets = [
                        {GuitarString=FifthString; Fret=15; Note=C};
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=12; Note=G};
                    ]@>

        [<Test>]
        let ``Should map F major to guitar fretboard on sixth string closed``() =
            let fMaj = triadsHarmonizer ScaleDgrees.IV cIonian 
            test <@ (chordToGuitarClosedChord SixthString fMaj).Frets = [
                        {GuitarString=SixthString; Fret=13; Note=F};
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=10; Note=C};
                    ]@>

        [<Test>]
        let ``Should map D major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian D
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            test <@ (chordToGuitarClosedChord FourthString chord).Frets = [
                        {GuitarString=FourthString; Fret=12; Note=D};
                        {GuitarString=ThirdString; Fret=11; Note=FSharp};
                        {GuitarString=SecondString; Fret=10; Note=A};
                        {GuitarString=FirstString; Fret=9; Note=CSharp};
                    ]@>

        [<Test>]
        let ``Should map EFlat major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian EFlat
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            test <@ (chordToGuitarClosedChord FourthString chord).Frets = [
                        {GuitarString=FourthString; Fret=13; Note=EFlat};
                        {GuitarString=ThirdString; Fret=12; Note=G};
                        {GuitarString=SecondString; Fret=11; Note=ASharp};
                        {GuitarString=FirstString; Fret=10; Note=D};
                    ]@>

        [<Test>]
        let ``Should map E major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian E
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            test <@ (chordToGuitarClosedChord FourthString chord).Frets = [
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=13; Note=GSharp};
                        {GuitarString=SecondString; Fret=12; Note=B};
                        {GuitarString=FirstString; Fret=11; Note=DSharp};
                    ]@>
        
        [<Test>]
        let ``Should map F major 7 to guitar fretboard on fourth string closed``() =
            let scale = createScale Ionian F
            let chord = seventhsHarmonizer ScaleDgrees.I scale
            test <@ (chordToGuitarClosedChord FourthString chord).Frets = [
                        {GuitarString=FourthString; Fret=15; Note=F};
                        {GuitarString=ThirdString; Fret=14; Note=A};
                        {GuitarString=SecondString; Fret=13; Note=C};
                        {GuitarString=FirstString; Fret=12; Note=E};
                    ]@>

        [<Test>]
        let ``Should map C major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let chord = seventhsHarmonizer ScaleDgrees.I cIonian |> toDrop2
            test <@ (chordToGuitarClosedChord FifthString chord).Frets = [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=5; Note=G};
                        {GuitarString=ThirdString; Fret=4; Note=B};
                        {GuitarString=SecondString; Fret=5; Note=E};
                    ]@>

        [<Test>]
        let ``Should map A major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let scale = createScale Ionian A
            let chord = seventhsHarmonizer ScaleDgrees.I scale |> toDrop2
            test <@ (chordToGuitarClosedChord FifthString chord).Frets = [
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=13; Note=GSharp};
                        {GuitarString=SecondString; Fret=14; Note=CSharp};
                    ]@>

        [<Test>]
        let ``Should map C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            let chord = seventhsHarmonizer ScaleDgrees.I cIonian |> toDrop3
            test <@ (chordToGuitarClosedChord SixthString chord).Frets = [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=(-1); Note=A};
                        {GuitarString=FourthString; Fret=9; Note=B};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]@>

    module GuitarTabTests =
        open System
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Notes
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
            test <@ guitarChord |> tabify = "      CMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----5--------|" + Environment.NewLine +
                                            "G|----4--------|" + Environment.NewLine +
                                            "D|----5--------|" + Environment.NewLine +
                                            "A|----3--------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine @>

        [<Test>]
        let ``Should draw A major 7 to guitar fretboard on fifth string closed ``() =
            let guitarChord = 
                (createScale Ionian A
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop2
                |> chordToGuitarClosedChord FifthString)
            test <@ guitarChord |> tabify = "      AMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----14-------|" + Environment.NewLine +
                                            "G|----13-------|" + Environment.NewLine +
                                            "D|----14-------|" + Environment.NewLine +
                                            "A|----12-------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine @>

        [<Test>]
        let ``Should draw F major 7 to guitar fretboard on fourth string closed``() =
            let guitarChord = 
                (createScale Ionian F
                |> seventhsHarmonizer ScaleDgrees.I
                |> chordToGuitarClosedChord FourthString)
            test <@ guitarChord |> tabify = "      FMaj7   " + Environment.NewLine +
                                            "E|----12-------|" + Environment.NewLine +
                                            "B|----13-------|" + Environment.NewLine +
                                            "G|----14-------|" + Environment.NewLine +
                                            "D|----15-------|" + Environment.NewLine +
                                            "A|-------------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine @>

        [<Test>]
        let ``Should draw c major to guitar fretboard on sixth string``() =
            let guitarChord = chordToGuitarClosedChord SixthString cMaj
            test <@ guitarChord |> tabify = "      CMaj   " + Environment.NewLine +
                                            "E|------------|" + Environment.NewLine +
                                            "B|------------|" + Environment.NewLine +
                                            "G|------------|" + Environment.NewLine +
                                            "D|----5-------|" + Environment.NewLine +
                                            "A|----7-------|" + Environment.NewLine +
                                            "E|----8-------|" + Environment.NewLine @>

        [<Test>]
        let ``Should draw C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord SixthString)
            test <@ guitarChord |> tabify = "      CMaj7   " + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine +
                                            "B|----8--------|" + Environment.NewLine +
                                            "G|----9--------|" + Environment.NewLine +
                                            "D|----9--------|" + Environment.NewLine +
                                            "A|-------------|" + Environment.NewLine +
                                            "E|----8--------|" + Environment.NewLine @>

        [<Test>]        
        let ``Should draw C major 7 drop 3 to guitar fretboard on fifth string closed``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord FifthString)
            test <@ guitarChord |> tabify = "      CMaj7   " + Environment.NewLine +
                                            "E|----3--------|" + Environment.NewLine +
                                            "B|----5--------|" + Environment.NewLine +
                                            "G|----4--------|" + Environment.NewLine +
                                            "D|-------------|" + Environment.NewLine +
                                            "A|----3--------|" + Environment.NewLine +
                                            "E|-------------|" + Environment.NewLine @>
                                                        
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
            
            test <@ tabifyAll guitarChords = 
                                "      CMaj7   DMin7   EMin7   FMaj7   " + Environment.NewLine +
                                "E|-------------------------------------|" + Environment.NewLine +
                                "B|----5-------6-------8-------10-------|" + Environment.NewLine +
                                "G|----4-------5-------7-------9--------|" + Environment.NewLine +
                                "D|----5-------7-------9-------10-------|" + Environment.NewLine +
                                "A|----3-------5-------7-------8--------|" + Environment.NewLine +
                                "E|-------------------------------------|" + Environment.NewLine @> 

        [<Test>]
        let ``Should draw shape of C major 7 drop 3 on sixth string``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop3
                |> chordToGuitarClosedChord SixthString)
            test <@ guitarChord |> shapify ="CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "8X998X" + Environment.NewLine @>


        [<Test>]
        let ``Should draw shape of C major 7 drop 2 on fifth string``() =
            let guitarChord = 
                (cIonian
                |> seventhsHarmonizer ScaleDgrees.I
                |> toDrop2
                |> chordToGuitarClosedChord FifthString)
            test <@ guitarChord |> shapify ="CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "X3545X" + Environment.NewLine @>

    module SpeechToMusicTests =
            open System
            open FParsec
            open NUnit.Framework
            open Swensen.Unquote
            open Vaughan.Domain
            open Vaughan.Notes
            open Vaughan.Chords
            open Vaughan.Chords
            open Vaughan.Scales
            open Vaughan.ScaleHarmonizer
            open Vaughan.SpeechToMusic
            open Vaughan.Guitar
            open Vaughan.GuitarTab
            open Vaughan.ScaleHarmonizer

            [<Test>]
            let ``Should parse textual representation of chord``() =
                test <@ (parseChord "C Major") = { Root=C; Quality=Major } @>
                test <@ (parseChord "C# Maj") = { Root=CSharp; Quality=Major } @>
                test <@ (parseChord "C minor") = { Root=C; Quality=Minor } @>
                test <@ (parseChord "Db min") = { Root=DFlat; Quality=Minor } @>
                test <@ (parseChord "Cmin") = { Root=C; Quality=Minor } @>
                test <@ (parseChord "Cm") = { Root=C; Quality=Minor } @>
                test <@ (parseChord "C augmented") = { Root=C; Quality=Augmented } @>
                test <@ (parseChord "C Aug") = { Root=C; Quality=Augmented } @>
                test <@ (parseChord "C diminished") = { Root=C; Quality=Diminished } @>
                test <@ (parseChord "C dim") = { Root=C; Quality=Diminished } @>
                test <@ (parseChord "C Major 7") = { Root=C; Quality=Major7 } @>
                test <@ (parseChord "C Maj 7") = { Root=C; Quality=Major7 } @>
                test <@ (parseChord "CMaj7") = { Root=C; Quality=Major7 } @>
                test <@ (parseChord "C minor 7") = { Root=C; Quality=Minor7 } @>
                test <@ (parseChord "C min 7") = { Root=C; Quality=Minor7 } @>
                test <@ (parseChord "Cmin7") = { Root=C; Quality=Minor7 } @>
                test <@ (parseChord "C augmented 7") = { Root=C; Quality=Augmented7 } @>
                test <@ (parseChord "C aug 7") = { Root=C; Quality=Augmented7 } @>
                test <@ (parseChord "C diminished 7") = { Root=C; Quality=Diminished7 } @>
                test <@ (parseChord "C dim 7") = { Root=C; Quality=Diminished7 } @>
                test <@ (parseChord "Caug7") = { Root=C; Quality=Augmented7 } @>
                test <@ (parseChord "Cdom7") = { Root=C; Quality=Dominant7 } @>
                test <@ (parseChord "C#7") = { Root=CSharp; Quality=Dominant7 } @>
                             
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

                test <@ createChord { Root=C; Quality=Major } = cMaj@>
                test <@ createChord { Root=D; Quality=Minor } = dMin@>
                test <@ createChord { Root=E; Quality=Minor } = eMin@>
                test <@ createChord { Root=F; Quality=Major } = fMaj@>
                test <@ createChord { Root=G; Quality=Major } = gMaj@>
                test <@ createChord { Root=A; Quality=Minor } = aMin@>
                test <@ createChord { Root=B; Quality=Diminished } = bDim@>
                
            [<Test>]
            let ``Should tabify chord from text``() =
                test <@ "A Major"
                        |> parseChord
                        |> createChord
                        |> chordToGuitarClosedChord SixthString
                        |> tabify = "      AMaj   " + Environment.NewLine+
                                    "E|------------|" + Environment.NewLine +
                                    "B|------------|" + Environment.NewLine +
                                    "G|------------|" + Environment.NewLine + 
                                    "D|----2-------|" + Environment.NewLine + 
                                    "A|----4-------|" + Environment.NewLine + 
                                    "E|----5-------|" + Environment.NewLine @>
            [<Test>]
            let ``Should tabify open chord from text``() =                 
                test <@ "C Major"
                        |> parseChord
                        |> createChord
                        |> chordToGuitarChord FifthString
                        |> tabify = "      CMaj   " + Environment.NewLine+
                                    "E|------------|" + Environment.NewLine +
                                    "B|------------|" + Environment.NewLine +
                                    "G|----0-------|" + Environment.NewLine + 
                                    "D|----2-------|" + Environment.NewLine + 
                                    "A|----3-------|" + Environment.NewLine + 
                                    "E|------------|" + Environment.NewLine @>