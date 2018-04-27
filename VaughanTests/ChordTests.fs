namespace VaughanTests
    module ChordTests =
        open Xunit
        open FsUnit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Notes
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

        [<Fact>]
        let ``Chord should return note names``() =
            noteNames cMaj7 |> should equal ["C"; "E"; "G"; "B"]

        [<Fact>]
        let ``Chord should return lowest note for bass``() =
            bass cDim7 |> should equal C

        [<Fact>]
        let ``Chord should return highest note for lead``() =
            lead cMin7 |> should equal BFlat

        [<Fact>]
        let ``Chord should be named after the root``() =
            (name cMin7b5) |> should startWith "C"

        [<Property>]
        let ``Chords should be named after the root`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            (name chord) |> should startWith (noteName root)

        [<Property>]
        let ``Major chords should be named after the quality`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality

            ( quality = Major || quality = Major7 || quality = Major9
            || quality = Major9Sharp11 || quality = Major11 || quality = Major13Sharp11)
                ==> lazy ((name chord).StartsWith((noteName root) + "Maj"))

        [<Property>]
        let ``Minor chords should be named after the quality`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality

            (quality = Minor || quality = Minor6 || quality = Minor6Add9 || quality = Minor7
             || quality = Minor7b5 || quality = Minor9 || quality = MinorMaj7 || quality = MinorMaj9)
                ==> lazy ((name chord).StartsWith((noteName root) + "Min"))

        [<Property>]
        let ``Dominant chords should be named after the quality`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality

            (quality = Dominant7 || quality = Dominant9 || quality = Dominant11 || quality = Dominant13
             || quality = Dominant7Flat5 || quality = Dominant7Flat5Flat9 || quality = Dominant7Flat5Sharp9
             || quality = Dominant7Flat9 || quality = Dominant7Sharp9)
                ==> lazy ((name chord).StartsWith((noteName root) + "7")
                        || (name chord).StartsWith((noteName root) + "9")
                        || (name chord).StartsWith((noteName root) + "11")
                        || (name chord).StartsWith((noteName root) + "13"))

        [<Fact>]
        let ``Chord should be named after the quality``() =
            name cMaj |> should startWith "CMaj"
            name cAug |> should startWith "CAug"
            name cMin |> should startWith "CMin"
            name cDim |> should startWith "CDim"
            name cMaj7 |> should startWith "CMaj7"
            name cAug7 |> should startWith "CAug7"
            name cMin7 |> should startWith "CMin7"
            name cDim7 |> should startWith "CDim7"
            name c6 |> should startWith "C6"

        [<Fact>]
        let ``Should create chord from root and function``() =
            (chord C Major).Notes |> should equal cMaj.Notes
            (chord C Augmented).Notes |> should equal cAug.Notes
            (chord C Minor).Notes |> should equal cMin.Notes
            (chord C Diminished).Notes |> should equal cDim.Notes
            (chord C Major7).Notes |> should equal cMaj7.Notes
            (chord C Augmented7).Notes |> should equal cAug7.Notes
            (chord C Minor7).Notes |> should equal cMin7.Notes
            (chord C Diminished7).Notes |> should equal cDim7.Notes
            (chord C Minor7b5).Notes |> should equal cMin7b5.Notes
            (chord C Major6).Notes |> should equal c6.Notes
            (chord C Major6Add9).Notes |> should equal c6add9.Notes
            (chord C Major6Flat5Add9).Notes |> should equal c6flat5add9.Notes
            (chord C Dominant7Flat5).Notes |> should equal c7flat5.Notes
            (chord C Dominant7Flat9).Notes |> should equal c7flat9.Notes
            (chord C Dominant7Sharp9).Notes |> should equal c7sharp9.Notes
            (chord C Dominant7Flat5Flat9).Notes |> should equal c7flat5flat9.Notes
            (chord C Dominant7Flat5Sharp9).Notes |> should equal c7flat5sharp9.Notes
            (chord C Dominant9).Notes |> should equal c9.Notes
            (chord C Dominant11).Notes |> should equal c11.Notes
            (chord C Dominant13).Notes |> should equal c13.Notes
            (chord C Major9).Notes |> should equal cMaj9.Notes
            (chord C Major11).Notes |> should equal cMaj11.Notes
            (chord C Major13).Notes |> should equal cMaj13.Notes
            (chord C Major9Sharp11).Notes |> should equal cMaj9Sharp11.Notes
            (chord C Major13Sharp11).Notes |> should equal cMaj13sharp11.Notes
            (chord C Minor6).Notes |> should equal cMin6.Notes
            (chord C Minor6Add9).Notes |> should equal cMin6add9.Notes
            (chord C MinorMaj7).Notes |> should equal cMinMaj7.Notes
            (chord C Minor9).Notes |> should equal cMin9.Notes
            (chord C MinorMaj9).Notes |> should equal cMinMaj9.Notes

        [<Fact>]
        let ``Should filter function from chord``() =
            (c9 |> skipFunction Fifth).Notes |> should equal [(C, Root); (E, Third); (BFlat, Seventh); (D, Ninth)]
            (c9 |> skipFunction Root).Notes |> should equal [(E, Third); (G, Fifth); (BFlat, Seventh); (D, Ninth)]
            (c9 |> skipFunction Seventh).Notes |> should equal [(C, Root); (E, Third); (G, Fifth); (D, Ninth)]

        [<Fact>]
        let ``Should invert chord for first inversion``() =
            (invert cMaj).Notes |> should equal [(E, Third); (G, Fifth); (C, Root)]
            (invert cAug).Notes |> should equal [(E, Third); (GSharp, Fifth); (C, Root)]
            (invert cMin).Notes |> should equal [(EFlat, Third); (G, Fifth); (C, Root)]
            (invert cDim).Notes |> should equal [(EFlat, Third); (GFlat, Fifth); (C, Root)]
            (invert cMaj7).Notes |> should equal [(E, Third); (G, Fifth); (B, Seventh); (C, Root)]
            (invert cAug7).Notes |> should equal [(E, Third); (GSharp, Fifth); (B, Seventh); (C, Root)]
            (invert cMin7).Notes |> should equal [(EFlat, Third); (G, Fifth); (BFlat, Seventh); (C, Root)]
            (invert cDim7).Notes |> should equal [(EFlat, Third); (GFlat, Fifth); (A, Seventh); (C, Root)]
            (invert cMin7b5).Notes |> should equal [(EFlat, Third); (GFlat, Fifth); (BFlat, Seventh); (C, Root)]

        [<Property>]
        let ``Should put third on bass for first inversion`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            snd (invert chord).Notes.Head = Third

        [<Fact>]
        let ``Should invert chord for second inversion``() =
            (cMaj |> invert |> invert).Notes |> should equal [(G, Fifth); (C, Root); (E, Third)]
            (cAug |> invert |> invert).Notes |> should equal [(GSharp, Fifth); (C, Root); (E, Third)]
            (cMin |> invert |> invert).Notes |> should equal [(G, Fifth); (C, Root); (EFlat, Third)]
            (cDim |> invert |> invert).Notes |> should equal [(GFlat, Fifth); (C, Root); (EFlat, Third)]
            (cMaj7 |> invert |> invert).Notes |> should equal [(G, Fifth); (B, Seventh); (C, Root); (E, Third)]
            (cAug7 |> invert |> invert).Notes |> should equal [(GSharp, Fifth); (B, Seventh); (C, Root); (E, Third)]
            (cMin7 |> invert |> invert).Notes |> should equal [(G, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]
            (cDim7 |> invert |> invert).Notes |> should equal [(GFlat, Fifth); (A, Seventh); (C, Root); (EFlat, Third)]
            (cMin7b5 |> invert |> invert).Notes |> should equal [(GFlat, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]

        [<Property>]
        let ``Should put fifth on bass for second inversion`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            snd (chord |> invert |> invert).Notes.Head = Fifth

        [<Fact>]
        let ``Should invert chord for third inversion``() =
            (cMaj7 |> invert |> invert |> invert).Notes |> should equal [(B, Seventh); (C, Root); (E, Third); (G, Fifth)]
            (cAug7 |> invert |> invert |> invert).Notes |> should equal [(B, Seventh); (C, Root); (E, Third); (GSharp, Fifth)]
            (cMin7 |> invert |> invert |> invert).Notes |> should equal [(BFlat, Seventh); (C, Root); (EFlat, Third); (G, Fifth)]
            (cDim7 |> invert |> invert |> invert).Notes |> should equal [(A, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]
            (cMin7b5 |> invert |> invert |> invert).Notes |> should equal [(BFlat, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]

        [<Property>]
        let ``Should put sixth or seventh on bass for third inversion`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality

            ( chord.Notes.Length > 3)
                ==> lazy (snd (chord |> invert |> invert |> invert).Notes.Head = Seventh
                            || snd (chord |> invert |> invert |> invert).Notes.Head = Sixth)

        [<Fact>]
        let ``Should loop inversions``() =
            cMaj7 |> invert |> invert |> invert |> invert |> should equal cMaj7
            cAug7 |> invert |> invert |> invert |> invert |> should equal cAug7

        [<Fact>]
        let ``Should transform chord to drop2``() =
            (cMaj7 |> toDrop2).Notes |> should equal [(C, Root); (G, Fifth); (B, Seventh); (E, Third);]

        [<Property>]
        let ``Drop 2 chords have third on lead and root on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop2Chord = chord |> toDrop2
            let lead = snd (drop2Chord.Notes |> List.last)
            let bass = snd (drop2Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (lead = Third && bass = Root)

        [<Fact>]
        let ``Should transform chord to drop3``() =
            (cMaj7 |> toDrop3).Notes |> should equal [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]

        [<Property>]
        let ``Drop 3 chords have fifth on lead`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop3Chord = chord |> toDrop3
            let lead = snd (drop3Chord.Notes |> List.last)
            let bass = snd (drop3Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (lead = Fifth && bass = Root)

        [<Fact>]
        let ``Should invert drop2``() =
            (cMaj7 |> toDrop2 |> invert).Notes |> should equal [(E, Third); (B, Seventh); (C, Root); (G, Fifth);]
            (cMaj7 |> toDrop2 |> invert |> invert).Notes |> should equal [(G, Fifth); (C, Root); (E, Third); (B, Seventh);]
            (cMaj7 |> toDrop2 |> invert |> invert |> invert ).Notes |> should equal [(B, Seventh); (E, Third); (G, Fifth); (C, Root); ]
            (cMaj7 |> toDrop2 |> invert |> invert |> invert |> invert).Notes |> should equal [(C, Root); (G, Fifth); (B, Seventh); (E, Third);]

        [<Property>]
        let ``First inversion drop 2 chords have fifth on lead and third on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop2Chord = chord |> toDrop2 |> invert
            let lead = snd (drop2Chord.Notes |> List.last)
            let bass = snd (drop2Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Third && lead = Fifth)

        [<Property>]
        let ``Second inversion drop 2 chords have seventh or sixth on lead and fifth on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop2Chord = chord |> toDrop2 |> invert |> invert
            let lead = snd (drop2Chord.Notes |> List.last)
            let bass = snd (drop2Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Fifth && (lead = Seventh || lead = Sixth))

        [<Property>]
        let ``Third inversion drop 2 chords have root on lead and seventh or sixth on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop2Chord = chord |> toDrop2 |> invert |> invert |> invert
            let lead = snd (drop2Chord.Notes |> List.last)
            let bass = snd (drop2Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy ((bass = Seventh || bass = Sixth) && lead = Root)

        [<Property>]
        let ``Fourth inversion drop 2 chords should have notes in same position as univerted chord`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop2Chord = chord |> toDrop2 |> invert |> invert |> invert |> invert
            let lead = snd (drop2Chord.Notes |> List.last)
            let bass = snd (drop2Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Root && lead = Third)

        [<Fact>]
        let ``Should invert drop3``() =
            (cMaj7 |> toDrop3 |> invert).Notes |> should equal [(E, Third); (C, Root); (G, Fifth); (B, Seventh)]
            (cMaj7 |> toDrop3 |> invert |> invert).Notes |> should equal [(G, Fifth); (E, Third); (B, Seventh); (C, Root);]
            (cMaj7 |> toDrop3 |> invert |> invert |> invert).Notes |> should equal [(B, Seventh); (G, Fifth); (C, Root); (E, Third);]
            (cMaj7 |> toDrop3 |> invert |> invert |> invert |> invert).Notes |> should equal [(C, Root); (B, Seventh); (E, Third); (G, Fifth)]

        [<Property>]
        let ``First inversion drop 3 chords have third on lead and seventh or sixth on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop3Chord = chord |> toDrop3 |> invert
            let lead = snd (drop3Chord.Notes |> List.last)
            let bass = snd (drop3Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Third && (lead = Seventh || lead = Sixth))

        [<Property>]
        let ``Second inversion drop 3 chords have seventh or sixth on lead and third on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop3Chord = chord |> toDrop3 |> invert |> invert
            let lead = snd (drop3Chord.Notes |> List.last)
            let bass = snd (drop3Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Fifth && lead = Root)

        [<Property>]
        let ``Third inversion drop 3 chords have third on lead and seventh or sixth on bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop3Chord = chord |> toDrop3 |> invert |> invert |> invert
            let lead = snd (drop3Chord.Notes |> List.last)
            let bass = snd (drop3Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy ((bass = Seventh || bass = Sixth) && lead = Third)

        [<Property>]
        let ``Fourth inversion drop 3 chords should have notes in same position as univerted chord`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let drop3Chord = chord |> toDrop3 |> invert |> invert |> invert |> invert
            let lead = snd (drop3Chord.Notes |> List.last)
            let bass = snd (drop3Chord.Notes.Head)

            (chord.Notes.Length = 4) ==> lazy (bass = Root && lead = Fifth)

        [<Fact>]
        let ``Should choose invertion that satisfies having a specific function as lead``() =
            (inversionForFunctionAsLead cMaj Third).Notes |> should equal (cMaj |> invert |> invert).Notes

        [<Property>]
        let ``Should choose invertion that satisfies having a root as lead`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let invertedChord = inversionForFunctionAsLead chord Root

            invertedChord.Notes |> List.last |> snd = Root

        [<Property>]
        let ``Should choose invertion that satisfies having a third as lead`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let invertedChord = inversionForFunctionAsLead chord Third

            invertedChord.Notes |> List.last |> snd = Third

        [<Property>]
        let ``Should choose invertion that satisfies having a fifth as lead`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let invertedChord = inversionForFunctionAsLead chord Fifth

            invertedChord.Notes |> List.last |> snd = Fifth

        [<Fact>]
        let ``Should choose invertion that satisfies having a specific function as bass``() =
            (inversionForFunctionAsBass cMaj Fifth).Notes |> should equal (cMaj |> invert |> invert).Notes

        [<Property>]
        let ``Should choose invertion that satisfies having a root as bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality |> invert
            let invertedChord = inversionForFunctionAsBass chord Root

            invertedChord.Notes |> List.head |> snd = Root

        [<Property>]
        let ``Should choose invertion that satisfies having a third as bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let invertedChord = inversionForFunctionAsBass chord Third

            invertedChord.Notes |> List.head |> snd = Third

        [<Property>]
        let ``Should choose invertion that satisfies having a fifth as bass`` (root :Note) (quality: ChordQuality) =
            let chord = chord root quality
            let invertedChord = inversionForFunctionAsBass chord Fifth

            invertedChord.Notes |> List.head |> snd = Fifth

        [<Property>]
        let ``It should choose invertion that satisfies having a lead that is closest to a provided note`` (root :Note) (quality: ChordQuality) (providedNote: Note) =
            let chord = chord root quality
            let invertedChord = invertionWithLeadClosestToNote chord providedNote

            let distancesToProvidedNote =
                invertedChord.Notes
                |> List.map (fun n -> measureAbsoluteSemitones (fst n) providedNote)

            (distancesToProvidedNote |> List.min) = (distancesToProvidedNote |> List.last)

        [<Property>]
        let ``It should choose invertion that satisfies having a bass that is closest to a provided note`` (root :Note) (quality: ChordQuality) (providedNote: Note) =
            let chord = chord root quality
            let invertedChord = invertionWithBassClosestToNote chord providedNote

            let distancesToProvidedNote =
                invertedChord.Notes
                |> List.map (fun n -> measureAbsoluteSemitones (fst n) providedNote)

            (distancesToProvidedNote |> List.min) = (distancesToProvidedNote |> List.head)

        [<Fact>]
        let ``It should return an empty list of fitting chords for an empty list of notes`` () =
            let fittingChords = chordsFitting [A; B]

            fittingChords |> should be Empty

        [<Property>]
        let ``It should return an empty list of fitting chords for list of notes with one note`` (note :Note) =
            let fittingChords = chordsFitting [note]

            fittingChords |> should be Empty

        [<Property>]
        let ``It should return an empty list of fitting chords for list of notes with two notes`` (note1 :Note)  (note2 :Note) =
            let fittingChords = chordsFitting [note1; note2]

            fittingChords |> should be Empty

        [<Property>]
        let ``It should return the fitting chord for list of notes from triad`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let chordNotes = chord.Notes |> List.map fst

            (chord.Notes.Length = 3)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from first inversion triad`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let invertedChord = chord |> invert
            let chordNotes = invertedChord.Notes |> List.map fst

            (chord.Notes.Length = 3)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from second inversion triad`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let invertedChord = chord |> invert |> invert
            let chordNotes = invertedChord.Notes |> List.map fst

            (chord.Notes.Length = 3)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from seventh chord`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let chordNotes = chord.Notes |> List.map fst

            (quality = Major7 || quality = Dominant7 || quality = Minor7 || quality = Minor7b5)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from seventh chord in first inversion`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let invertedChord = chord |> invert
            let chordNotes = invertedChord.Notes |> List.map fst

            (quality = Major7 || quality = Dominant7 || quality = Minor7 || quality = Minor7b5)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from seventh chord in second inversion`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let invertedChord = chord |> invert |> invert
            let chordNotes = invertedChord.Notes |> List.map fst

            (quality = Major7 || quality = Dominant7 || quality = Minor7 || quality = Minor7b5)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)

        [<Property>]
        let ``It should return the chord for list of notes from seventh chord in third inversion`` (root :Note) (quality :ChordQuality) =
            let chord = chord root quality
            let invertedChord = chord |> invert |> invert |> invert
            let chordNotes = invertedChord.Notes |> List.map fst

            (quality = Major7 || quality = Dominant7 || quality = Minor7 || quality = Minor7b5)
                ==> lazy ((chordsFitting chordNotes) |> List.contains chord)