namespace VaughanTests
    module ScalesHormonizerTests =
        open Xunit
        open FsUnit.Xunit
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open Vaughan.Notes
        open VaughanTests.DiatonicScalesArbitrary

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

        [<Fact>]
        let ``Should create triads for Ionian scale`` () =
            let cIonian = createScaleNotes Ionian C
            (triadsHarmonizer ScaleDegree.I cIonian).Notes |> should equal cMaj.Notes
            (triadsHarmonizer ScaleDegree.II cIonian).Notes |> should equal dMin.Notes
            (triadsHarmonizer ScaleDegree.III cIonian).Notes |> should equal eMin.Notes
            (triadsHarmonizer ScaleDegree.IV cIonian).Notes |> should equal fMaj.Notes
            (triadsHarmonizer ScaleDegree.V cIonian).Notes |> should equal gMaj.Notes
            (triadsHarmonizer ScaleDegree.VI cIonian).Notes |> should equal aMin.Notes
            (triadsHarmonizer ScaleDegree.VII cIonian).Notes |> should equal bDim.Notes

        [<Fact>]
        let ``Should create triads for Harmonic Minor scale`` () =
            let cMinor = createScaleNotes HarmonicMinor C
            (triadsHarmonizer ScaleDegree.I cMinor).Notes |> should equal cMin.Notes
            (triadsHarmonizer ScaleDegree.II cMinor).Notes |> should equal dDim.Notes
            (triadsHarmonizer ScaleDegree.III cMinor).Notes |> should equal eFlatAug.Notes
            (triadsHarmonizer ScaleDegree.IV cMinor).Notes |> should equal fMin.Notes
            (triadsHarmonizer ScaleDegree.V cMinor).Notes |> should equal gMaj.Notes
            (triadsHarmonizer ScaleDegree.VI cMinor).Notes |> should equal aFlatMaj.Notes
            (triadsHarmonizer ScaleDegree.VII cMinor).Notes |> should equal bDim.Notes

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should create triads for scale`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note)=
            let scale = createScaleNotes scaleType root

            (triadsHarmonizer scaleDegree scale).Notes
            |> List.pairwise
            |> List.map (fun e -> intervalBetween (fst (fst e)) (fst (snd e)))
            |> List.forall (
                fun e -> e = MajorThird || e = MinorThird || e = MajorSecond || e = PerfectFourth)

        [<Fact>]
        let ``Should create seventh chords for Ionian scale`` () =
            let cIonian = createScaleNotes Ionian C
            (seventhsHarmonizer ScaleDegree.I cIonian).Notes |> should equal cMaj7.Notes
            (seventhsHarmonizer ScaleDegree.II cIonian).Notes |> should equal dMin7.Notes
            (seventhsHarmonizer ScaleDegree.III cIonian).Notes |> should equal eMin7.Notes
            (seventhsHarmonizer ScaleDegree.IV cIonian).Notes |> should equal fMaj7.Notes
            (seventhsHarmonizer ScaleDegree.V cIonian).Notes |> should equal gDom7.Notes
            (seventhsHarmonizer ScaleDegree.VI cIonian).Notes |> should equal aMin7.Notes
            (seventhsHarmonizer ScaleDegree.VII cIonian).Notes |> should equal bMin7b5.Notes

        [<Fact>]
        let ``Should create seventh chords for Harmonic Minor scale`` () =
            let cMinor = createScaleNotes HarmonicMinor C
            (seventhsHarmonizer ScaleDegree.I cMinor).Notes |> should equal cMinMaj7.Notes
            (seventhsHarmonizer ScaleDegree.II cMinor).Notes |> should equal dMin7b5.Notes
            (seventhsHarmonizer ScaleDegree.III cMinor).Notes |> should equal eFlatAug7.Notes
            (seventhsHarmonizer ScaleDegree.IV cMinor).Notes |> should equal fMin7.Notes
            (seventhsHarmonizer ScaleDegree.V cMinor).Notes |> should equal gDom7.Notes
            (seventhsHarmonizer ScaleDegree.VI cMinor).Notes |> should equal aFlatMaj7.Notes
            (seventhsHarmonizer ScaleDegree.VII cMinor).Notes |> should equal bDim7.Notes

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should create seventh chords for scale`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) =
            let scale = createScaleNotes scaleType root

            (seventhsHarmonizer scaleDegree scale).Notes
            |> List.pairwise
            |> List.map (fun e -> intervalBetween (fst (fst e)) (fst (snd e)))
            |> List.forall (
                fun e -> e = MajorThird || e = MinorThird || e = MajorSecond || e = PerfectFourth)

        [<Fact>]
        let ``Should create ninth chords for Ionian scale`` () =
            let cIonian = createScaleNotes Ionian C
            (ninthsHarmonizer ScaleDegree.I cIonian).Notes |> should equal cMaj9.Notes
            (ninthsHarmonizer ScaleDegree.II cIonian).Notes |> should equal dMin9.Notes
            (ninthsHarmonizer ScaleDegree.III cIonian).Notes |> should equal eMin9.Notes
            (ninthsHarmonizer ScaleDegree.IV cIonian).Notes |> should equal fMaj9.Notes
            (ninthsHarmonizer ScaleDegree.V cIonian).Notes |> should equal gDom9.Notes
            (ninthsHarmonizer ScaleDegree.VI cIonian).Notes |> should equal aMin9.Notes
            (ninthsHarmonizer ScaleDegree.VII cIonian).Notes |> should equal bMin9b5.Notes