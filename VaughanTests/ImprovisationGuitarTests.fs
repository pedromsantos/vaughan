namespace VaughanTests

    module ImprovisationGuitarTests =
        open System
        open Xunit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Scales
        open Vaughan.Chords
        open Vaughan.Guitar
        open Vaughan.GuitarTab
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open Vaughan.SpeechToMusic
        open Vaughan.ImprovisationGuitar

        [<Fact>]
        let ``Should create guitar arpeggios from chords`` () =
            [chord C Minor7; chord F Dominant7; chord BFlat Major7]
            |> createArpeggiosFromChords 2 5
            |> List.length 
            |> should equal 3

        [<Fact>]
        let ``Should create guitar scales from chords`` () =
            [chord C Minor7; chord F Dominant7; chord BFlat Major7]
            |> createScalesForChords 2 5
            |> List.length 
            |> should equal 3

        [<Fact>]
        let ``Should create guitar tab arpeggios from chords`` () =
            [chord C Minor7; chord F Dominant7; chord BFlat Major7]
            |> createTabArpeggiosFromChords 2 5
            |> List.length 
            |> should equal 3

        [<Fact>]
        let ``Should create guitar tab scales from chords`` () =
            [chord C Minor7; chord F Dominant7; chord BFlat Major7]
            |> createTabScalesFromChords 2 5
            |> List.length 
            |> should equal 3

        [<Fact>]
        let ``Should render tab for C arpeggio starting from root on position 2`` () =
            [StandardTunning; Start; Arpeggio(arpeggioFrom root (guitarArpeggio 2 5 (chord C Major))); End]
            |> renderTab
            |> should equal (
                             "e||-----------------3--||" + Environment.NewLine +
                             "B||--------------5-----||" + Environment.NewLine +
                             "G||-----------5--------||" + Environment.NewLine +
                             "D||-----2--5-----------||" + Environment.NewLine +
                             "A||--3-----------------||" + Environment.NewLine +
                             "E||--------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should render tab for G arpeggio starting from root on position 2`` () =
            [StandardTunning; Start; Arpeggio(arpeggioFrom root (guitarArpeggio 2 5 (chord G Major))); End]
            |> renderTab
            |> should equal (
                             "e||--------------------3--||" + Environment.NewLine +
                             "B||-----------------3-----||" + Environment.NewLine +
                             "G||--------------4--------||" + Environment.NewLine +
                             "D||-----------5-----------||" + Environment.NewLine +
                             "A||-----2--5--------------||" + Environment.NewLine +
                             "E||--3--------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should render tab for C Major 7 arpeggio enclosing the root on position 2`` () =
            [StandardTunning; Start; Notes(enclosedArpeggioFrom root (guitarArpeggio 2 5 (chord C Major7))); End]
            |> renderTab
            |> should equal (
                             "e||--------------------------3--||" + Environment.NewLine +
                             "B||-----------------------5-----||" + Environment.NewLine +
                             "G||-----------------4--5--------||" + Environment.NewLine +
                             "D||-----------2--5--------------||" + Environment.NewLine +
                             "A||--4--2--3--------------------||" + Environment.NewLine +
                             "E||-----------------------------||" + Environment.NewLine)
        
        [<Fact>]
        let ``Should render tab for C Major 7 arpeggio enclosing the third on position 2`` () =
            [StandardTunning; Start; Notes(enclosedArpeggioFrom third (guitarArpeggio 2 5 (chord C Major7))); End]
            |> renderTab
            |> should equal (
                             "e||--------------------3--||" + Environment.NewLine +
                             "B||-----------------5-----||" + Environment.NewLine +
                             "G||-----------4--5--------||" + Environment.NewLine +
                             "D||--3--1--2--------------||" + Environment.NewLine +
                             "A||-----------------------||" + Environment.NewLine +
                             "E||-----------------------||" + Environment.NewLine)

        [<Fact>]
        let ``Should render tab for C Major 7 arpeggio enclosing the root on position 5`` () =
            [StandardTunning; Start; Notes(enclosedArpeggioFrom root (guitarArpeggio 5 8 (chord C Minor7))); End]
            |> renderTab
            |> should equal (
                             "e||-----------------------------6--8--||" + Environment.NewLine +
                             "B||--------------------------8--------||" + Environment.NewLine +
                             "G||--------------------5--8-----------||" + Environment.NewLine +
                             "D||--------------5--8-----------------||" + Environment.NewLine +
                             "A||-----------6-----------------------||" + Environment.NewLine +
                             "E||--9--7--8--------------------------||" + Environment.NewLine)
        
        [<Fact>]
        let ``Should render tab for C Major 7 arpeggio enclosing the third on position 5`` () =
            [StandardTunning; Start; Notes(enclosedArpeggioFrom third (guitarArpeggio 5 8 (chord C Minor7))); End]
            |> renderTab
            |> should equal (
                             "e||--------------------------6--8--||" + Environment.NewLine +
                             "B||-----------------------8--------||" + Environment.NewLine +
                             "G||-----------------5--8-----------||" + Environment.NewLine +
                             "D||-----------5--8-----------------||" + Environment.NewLine +
                             "A||--7--5--6-----------------------||" + Environment.NewLine +
                             "E||--------------------------------||" + Environment.NewLine)