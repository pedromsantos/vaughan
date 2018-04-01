namespace VaughanTests
    module GuitarTests =
        open Xunit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open VaughanTests.DiatonicScalesArbitrary

        let cIonian = createScaleNotes Ionian C

        let cMaj = triadsHarmonizer ScaleDegree.I cIonian

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed triads to guitar fretboard`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            ((bassString <> FirstString || bassString = SecondString))
                ==> lazy (
                            let scale = createScaleNotes scaleType root
                            let chord = triadsHarmonizer scaleDegree scale
                            let guitarChord = createGuitarChord bassString chord
                            let maxFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.max
                            let minFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.min
                            maxFret - minFret < 6)

        [<Fact>]
        let ``Should map c major to guitar fretboard``() =
            (createGuitarChord SixthString cMaj).Frets |> should equal [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=7; Note=E};
                        {GuitarString=FourthString; Fret=5; Note=G};
                    ]

        [<Fact>]
        let ``Should map c major open to guitar fretboard on fifth string``() =
            (createGuitarChord FifthString (cMaj |> toOpen)).Frets |> should equal [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=0; Note=G};
                        {GuitarString=SecondString; Fret=1; Note=C};
                        {GuitarString=FirstString; Fret=0; Note=E};
                    ]

        [<Fact>]
        let ``Should map c major to guitar fretboard on fourth string``() =
            (createGuitarChord FourthString cMaj).Frets |> should equal [
                        {GuitarString=FourthString; Fret=10; Note=C};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]

        [<Fact>]
        let ``Should map c major to guitar fretboard on third string``() =
            (createGuitarChord ThirdString cMaj).Frets |> should equal [
                        {GuitarString=ThirdString; Fret=5; Note=C};
                        {GuitarString=SecondString; Fret=5; Note=E};
                        {GuitarString=FirstString; Fret=3; Note=G};
                    ]

        [<Fact>]
        let ``Should map F major to guitar fretboard on sixth string``() =
            let fMaj = triadsHarmonizer ScaleDegree.IV cIonian
            (createGuitarChord SixthString fMaj).Frets |> should equal [
                        {GuitarString=SixthString; Fret=13; Note=F};
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=10; Note=C};
                    ]

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed sevent chords to guitar fretboard`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            ((bassString = SixthString || bassString = FifthString || bassString = FourthString))
                ==> lazy (
                            let scale = createScaleNotes scaleType root
                            let chord = seventhsHarmonizer scaleDegree scale
                            let guitarChord = createGuitarChord bassString chord
                            let maxFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.max
                            let minFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.min
                            maxFret - minFret < 6)

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed seventh drop 2 chords to guitar fretboard`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            ((bassString = SixthString || bassString = FifthString || bassString = FourthString))
                ==> lazy (
                            let scale = createScaleNotes scaleType root
                            let chord = seventhsHarmonizer scaleDegree scale |> toDrop2
                            let guitarChord = createGuitarChord bassString chord
                            let maxFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.max
                            let minFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.min
                            maxFret - minFret < 6)

        [<Fact>]
        let ``Should map C major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let chord = seventhsHarmonizer ScaleDegree.I cIonian |> toDrop2
            (createGuitarChord FifthString chord).Frets |> should equal [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=5; Note=G};
                        {GuitarString=ThirdString; Fret=4; Note=B};
                        {GuitarString=SecondString; Fret=5; Note=E};
                    ]

        [<Fact>]
        let ``Should map A major 7 drop 2 to guitar fretboard on fifth string closed``() =
            let scale = createScaleNotes Ionian A
            let chord = seventhsHarmonizer ScaleDegree.I scale |> toDrop2
            (createGuitarChord FifthString chord).Frets |> should equal [
                        {GuitarString=FifthString; Fret=12; Note=A};
                        {GuitarString=FourthString; Fret=14; Note=E};
                        {GuitarString=ThirdString; Fret=13; Note=GSharp};
                        {GuitarString=SecondString; Fret=14; Note=CSharp};
                    ]

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed sevent drop 3 chords to guitar fretboard`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            (bassString = SixthString || bassString = FifthString)
                ==> lazy (
                            let scale = createScaleNotes scaleType root
                            let chord = seventhsHarmonizer scaleDegree scale |> toDrop3
                            let guitarChord = createGuitarChord bassString chord
                            let maxFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.max
                            let minFret = guitarChord.Frets |> List.map (fun f -> f.Fret) |> List.filter (fun f -> f <> -1) |> List.min
                            maxFret - minFret < 6)

        [<Fact>]
        let ``Should map C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            let chord = seventhsHarmonizer ScaleDegree.I cIonian |> toDrop3
            (createGuitarChord SixthString chord).Frets |> should equal [
                        {GuitarString=SixthString; Fret=8; Note=C};
                        {GuitarString=FifthString; Fret=(-1); Note=A};
                        {GuitarString=FourthString; Fret=9; Note=B};
                        {GuitarString=ThirdString; Fret=9; Note=E};
                        {GuitarString=SecondString; Fret=8; Note=G};
                    ]

        [<Fact>]
        let ``Should map C major9 ignoring 5th to guitar fretboard on fifth string closed``() =
            let chord = chord C Major9
                        |> skipFunction Fifth

            (createGuitarChord FifthString chord).Frets |> should equal [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=4; Note=B};
                        {GuitarString=SecondString; Fret=3; Note=D};
                    ]

        [<Fact>]
        let ``Should map C9 ignoring 5th to guitar fretboard on fifth string closed``() =
            let chord = chord C Dominant9
                        |> skipFunction Fifth

            (createGuitarChord FifthString chord).Frets |> should equal [
                        {GuitarString=FifthString; Fret=3; Note=C};
                        {GuitarString=FourthString; Fret=2; Note=E};
                        {GuitarString=ThirdString; Fret=3; Note=BFlat};
                        {GuitarString=SecondString; Fret=3; Note=D};
                    ]
                    
        [<Fact>]
        let ``Should map C major triad arpeggio to guitar fretboard on first position``() =
            cMaj
            |> (createGuitarArpeggio 1 4)
            |> should equal 
                            {
                                BaseChord = {
                                                Notes = [(C, Root); (E, Third); (G, Fifth)]; 
                                                ChordType = Closed; 
                                                Name = "CMaj";};
                                ArpeggioFrets = [
                                                    {GuitarString = FirstString; Fret = 3; Note = G;}; 
                                                    {GuitarString = SecondString; Fret = 1; Note = C;}; 
                                                    {GuitarString = ThirdString; Fret = -1; Note = G;}; 
                                                    {GuitarString = FourthString; Fret = 2; Note = E;};
                                                    {GuitarString = FifthString; Fret = 3; Note = C;}; 
                                                    {GuitarString = SixthString; Fret = 3; Note = G;}
                                                ];}
        
        [<Fact>]
        let ``Should create arpeggio melodic line for C major triad``() =
            cMaj
            |> (createGuitarArpeggio 1 4)
            |> createGuitarMelodicLineFromArpeggio
            |> should equal [[3]; [1]; [-1]; [2]; [3]; [3]]

        [<Fact>]
        let ``Should create arpeggio melodic line for C major 7 chord``() =
            cIonian
            |> seventhsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 2 5
            |> createGuitarMelodicLineFromArpeggio
            |> should equal [[3]; [5]; [4; 5]; [2; 5]; [2; 3]; [3]]

        [<Fact>]
        let ``Should create scale melodic line for C Ionian scale``() =
            createScale Ionian C
            |> createGuitarScale 2 6
            |> createGuitarMelodicLineFromScale
            |> should equal [[3; 5]; [3; 5; 6]; [2; 4; 5]; [2; 3; 5]; [2; 3; 5]; [3; 5]]

        [<Property>]
        let ``Should create scale melodic line for scale on second position`` (scaleType: ScaleType) (root: Note) () =
            let melodicLine = 
                            createScale scaleType root
                            |> createGuitarScale 2 6
                            |> createGuitarMelodicLineFromScale
            let maxFret = (melodicLine |> List.collect id |> List.max) 
            let minFret = (melodicLine |> List.collect id |> List.min)
            minFret > 1 && maxFret < 7

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed triad arpeggios to guitar fretboard first position`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) () =
            let scale = createScaleNotes scaleType root
            let chord = triadsHarmonizer scaleDegree scale
            let guitarArpeggio = createGuitarArpeggio 1 4 chord
            let unmutedFrets = guitarArpeggio.ArpeggioFrets |> List.filter (fun f -> f.Fret > -1)
            let maxFret = unmutedFrets |> List.map (fun f -> f.Fret) |> List.max
            let minFret = unmutedFrets |> List.map (fun f -> f.Fret) |> List.min
            minFret > 0 && maxFret < 5

    module GuitarTabTests =
        open Xunit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open System
        open Vaughan.Domain
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.GuitarTab
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open VaughanTests.DiatonicScalesArbitrary

        let cIonian = createScaleNotes Ionian C
        let cMaj = triadsHarmonizer ScaleDegree.I cIonian

        let createTriad (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) =
            let scale = createScaleNotes scaleType root
            let chord = triadsHarmonizer scaleDegree scale
            createGuitarChord bassString chord

        let createSeventhChord (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) =
            let scale = createScaleNotes scaleType root
            let chord = seventhsHarmonizer scaleDegree scale
            createGuitarChord bassString chord

        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed triad to guitar tab`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            (bassString <> SecondString && bassString <> FirstString)
                ==> lazy (
                            let guitarChord = createTriad scaleType scaleDegree root bassString
                            let frets = guitarChord.Frets
                            let tab = tabifyChord guitarChord
                            tab.Contains (string frets.[0].Fret)
                            && tab.Contains (string frets.[1].Fret)
                            && tab.Contains (string frets.[2].Fret))
                            
        [<Property(Arbitrary = [| typeof<DiatonicScales> |])>]
        let ``Should map diatonic closed seventh chord to guitar tab`` (scaleType: ScaleType) (scaleDegree: ScaleDegree) (root: Note) (bassString: GuitarString) () =
            (bassString <> ThirdString && bassString <> SecondString && bassString <> FirstString)
                ==> lazy (
                            let guitarChord = createSeventhChord scaleType scaleDegree root bassString
                            let frets = guitarChord.Frets
                            let tab = tabifyChord guitarChord
                            tab.Contains (string frets.[0].Fret)
                            && tab.Contains (string frets.[1].Fret)
                            && tab.Contains (string frets.[2].Fret)
                            && tab.Contains (string frets.[3].Fret))

        [<Fact>]
        let ``Should draw C major 7 drop 2 to guitar fretboard on fifth string closed ``() =
            cIonian
            |> seventhsHarmonizer ScaleDegree.I
            |> toDrop2
            |> createGuitarChord FifthString
            |> tabifyChord |> should equal ("      CMaj7   " + Environment.NewLine +
                                            "e||-------------||" + Environment.NewLine +
                                            "B||----5--------||" + Environment.NewLine +
                                            "G||----4--------||" + Environment.NewLine +
                                            "D||----5--------||" + Environment.NewLine +
                                            "A||----3--------||" + Environment.NewLine +
                                            "E||-------------||" + Environment.NewLine)
        
                                            
        [<Fact>]
        let ``Should draw C major arpeggio to guitar fretboard on open position ``() =
            cIonian
            |> triadsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 0 3
            |> tabifyArpeggio 
            |> should equal (
                             "e||---------------------0--3-||" + Environment.NewLine +
                             "B||------------------1-------||" + Environment.NewLine +
                             "G||---------------0----------||" + Environment.NewLine +
                             "D||------------2-------------||" + Environment.NewLine +
                             "A||---------3----------------||" + Environment.NewLine +
                             "E||---0--3-------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw C major arpeggio to guitar fretboard on first position ``() =
            cIonian
            |> triadsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 1 4
            |> tabifyArpeggio 
            |> should equal (
                             "e||------------------3-||" + Environment.NewLine +
                             "B||---------------1----||" + Environment.NewLine +
                             "G||-------------1------||" + Environment.NewLine +
                             "D||---------2----------||" + Environment.NewLine +
                             "A||------3-------------||" + Environment.NewLine +
                             "E||---3----------------||" + Environment.NewLine)
        
        [<Fact>]
        let ``Should draw C major arpeggio to guitar fretboard on second position ``() =
            cIonian
            |> triadsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 2 5
            |> tabifyArpeggio 
            |> should equal (
                             "e||---------------------3-||" + Environment.NewLine +
                             "B||------------------5----||" + Environment.NewLine +
                             "G||---------------5-------||" + Environment.NewLine +
                             "D||---------2--5----------||" + Environment.NewLine +
                             "A||------3----------------||" + Environment.NewLine +
                             "E||---3-------------------||" + Environment.NewLine)
        
        [<Fact>]
        let ``Should draw C major arpeggio to guitar fretboard on seventh position ``() =
            cIonian
            |> triadsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 7 10
            |> tabifyArpeggio 
            |> should equal (
                             "e||---------------------8-||" + Environment.NewLine +
                             "B||------------------8----||" + Environment.NewLine +
                             "G||---------------9-------||" + Environment.NewLine +
                             "D||------------10---------||" + Environment.NewLine +
                             "A||------7--10------------||" + Environment.NewLine +
                             "E||---8-------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw C major7 arpeggio to guitar fretboard on seventh position ``() =
            cIonian
            |> seventhsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 7 10
            |> tabifyArpeggio 
            |> should equal (
                             "e||---------------------------7--8-||" + Environment.NewLine +
                             "B||------------------------8-------||" + Environment.NewLine +
                             "G||---------------------9----------||" + Environment.NewLine +
                             "D||---------------9--10------------||" + Environment.NewLine +
                             "A||---------7--10------------------||" + Environment.NewLine +
                             "E||---7--8-------------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should tab C major triad arpeggio to guitar fretboard from ninth fret to twenty second fret ``() =
            cIonian
            |> triadsHarmonizer ScaleDegree.I
            |> createGuitarArpeggio 9 22
            |> tabifyArpeggio
            |> should equal (
                             "e||------------------------------------------------12--15--20-||" + Environment.NewLine +
                             "B||---------------------------------------13--17--20----------||" + Environment.NewLine +
                             "G||------------------------------9--12--17--------------------||" + Environment.NewLine +
                             "D||---------------------10--14--17----------------------------||" + Environment.NewLine +
                             "A||------------10--15--19-------------------------------------||" + Environment.NewLine +
                             "E||---12--15--20----------------------------------------------||" + Environment.NewLine)


        [<Fact>]
        let ``Should tab C ionian scale to guitar fretboard on second position ``() =
            createScale Ionian C
            |> createGuitarScale 2 6
            |> tabifyScale 
            |> should equal (
                             "e||---------------------------------------------3--5-||" + Environment.NewLine +
                             "B||------------------------------------3--5--6-------||" + Environment.NewLine +
                             "G||---------------------------2--4--5----------------||" + Environment.NewLine +
                             "D||------------------2--3--5-------------------------||" + Environment.NewLine +
                             "A||---------2--3--5----------------------------------||" + Environment.NewLine +
                             "E||---3--5-------------------------------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw A major 7 to guitar fretboard on fifth string closed ``() =
            createScaleNotes Ionian A
            |> seventhsHarmonizer ScaleDegree.I
            |> toDrop2
            |> createGuitarChord FifthString
            |> tabifyChord |> should equal ("      AMaj7   " + Environment.NewLine +
                                            "e||-------------||" + Environment.NewLine +
                                            "B||----14-------||" + Environment.NewLine +
                                            "G||----13-------||" + Environment.NewLine +
                                            "D||----14-------||" + Environment.NewLine +
                                            "A||----12-------||" + Environment.NewLine +
                                            "E||-------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw c major to guitar fretboard on sixth string``() =
            let guitarChord = createGuitarChord SixthString cMaj
            guitarChord |> tabifyChord |> should equal ("      CMaj   " + Environment.NewLine +
                                            "e||------------||" + Environment.NewLine +
                                            "B||------------||" + Environment.NewLine +
                                            "G||------------||" + Environment.NewLine +
                                            "D||----5-------||" + Environment.NewLine +
                                            "A||----7-------||" + Environment.NewLine +
                                            "E||----8-------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw C major 7 drop 3 to guitar fretboard on sixth string closed``() =
            cIonian
            |> seventhsHarmonizer ScaleDegree.I
            |> toDrop3
            |> createGuitarChord SixthString
            |> tabifyChord |> should equal ("      CMaj7   " + Environment.NewLine +
                                            "e||-------------||" + Environment.NewLine +
                                            "B||----8--------||" + Environment.NewLine +
                                            "G||----9--------||" + Environment.NewLine +
                                            "D||----9--------||" + Environment.NewLine +
                                            "A||-------------||" + Environment.NewLine +
                                            "E||----8--------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw C major 7 drop 3 to guitar fretboard on fifth string closed``() =
            cIonian
            |> seventhsHarmonizer ScaleDegree.I
            |> toDrop3
            |> createGuitarChord FifthString
            |> tabifyChord |> should equal ("      CMaj7   " + Environment.NewLine +
                                            "e||----3--------||" + Environment.NewLine +
                                            "B||----5--------||" + Environment.NewLine +
                                            "G||----4--------||" + Environment.NewLine +
                                            "D||-------------||" + Environment.NewLine +
                                            "A||----3--------||" + Environment.NewLine +
                                            "E||-------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should map C9 ignoring 5th to guitar fretboard on fifth string closed``() =
            (chord C Dominant9)
            |> skipFunction Fifth
            |> createGuitarChord FifthString
            |> tabifyChord |> should equal ("      C9   " + Environment.NewLine +
                                            "e||----------||" + Environment.NewLine +
                                            "B||----3-----||" + Environment.NewLine +
                                            "G||----3-----||" + Environment.NewLine +
                                            "D||----2-----||" + Environment.NewLine +
                                            "A||----3-----||" + Environment.NewLine +
                                            "E||----------||" + Environment.NewLine)

        [<Fact>]
        let ``Should tabify multiple chords``() =
            let cIonian = createScaleNotes Ionian C
            let cMaj7 = seventhsHarmonizer ScaleDegree.I cIonian
            let dMin7 = seventhsHarmonizer ScaleDegree.II cIonian
            let eMin7 = seventhsHarmonizer ScaleDegree.III cIonian
            let fMaj7 = seventhsHarmonizer ScaleDegree.IV cIonian

            let guitarChords =
                [cMaj7; dMin7; eMin7; fMaj7]
                |> List.map (
                    toDrop2 >> (createGuitarChord FifthString))

            tabifyAll guitarChords |> should equal
                                ("      CMaj7   DMin7   EMin7   FMaj7   " + Environment.NewLine +
                                "e||-------------------------------------||" + Environment.NewLine +
                                "B||----5-------6-------8-------10-------||" + Environment.NewLine +
                                "G||----4-------5-------7-------9--------||" + Environment.NewLine +
                                "D||----5-------7-------9-------10-------||" + Environment.NewLine +
                                "A||----3-------5-------7-------8--------||" + Environment.NewLine +
                                "E||-------------------------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should tabify multiple chained chords using operators``() =
            [(G=>Major)] /./ (C=>Major) /./ (A=>Minor) /./ (D=>Major)
            |> List.map (fun c -> createGuitarChord SixthString (toOpen c))
            |> tabifyAll |> should equal
                                ("      GMaj   CMaj   AMin   DMaj   " + Environment.NewLine +
                                "e||----3------0------0------2-------||" + Environment.NewLine +
                                "B||----0------1------1------3-------||" + Environment.NewLine +
                                "G||----0------0------2------2-------||" + Environment.NewLine +
                                "D||----0------2------2------0-------||" + Environment.NewLine +
                                "A||----2------3------0------0-------||" + Environment.NewLine +
                                "E||----3------0------0------2-------||" + Environment.NewLine)

        [<Fact>]
        let ``Should tabify multiple chained chords using only operators``() =
            [(!*(G=>Major) |~ SixthString);
             (!*(C=>Major) |~ FifthString);
             (!*(A=>Minor) |~ FifthString);
             (!*(D=>Major) |~ FourthString)]
            |> tabifyAll |> should equal
                                ("      GMaj   CMaj   AMin   DMaj   " + Environment.NewLine +
                                "e||----3------0------0------2-------||" + Environment.NewLine +
                                "B||----0------1------1------3-------||" + Environment.NewLine +
                                "G||----0------0------2------2-------||" + Environment.NewLine +
                                "D||----0------2------2------0-------||" + Environment.NewLine +
                                "A||----2------3------0--------------||" + Environment.NewLine +
                                "E||----3----------------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should draw shape of C major 7 drop 3 on sixth string``() =
            let guitarChord =
                (cIonian
                |> seventhsHarmonizer ScaleDegree.I
                |> toDrop3
                |> createGuitarChord SixthString)
            guitarChord |> shapify |> should equal ("CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "8X998X" + Environment.NewLine)


        [<Fact>]
        let ``Should draw shape of C major 7 drop 2 on fifth string``() =
            let guitarChord =
                (cIonian
                |> seventhsHarmonizer ScaleDegree.I
                |> toDrop2
                |> createGuitarChord FifthString)
            guitarChord |> shapify |> should equal ("CMaj7" + Environment.NewLine +
                                            "EADGBE" + Environment.NewLine +
                                            "X3545X" + Environment.NewLine)