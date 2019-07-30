namespace VaughanTests

module ImprovisationGuitarTests =
    open System
    open Xunit
    open FsUnit.Xunit
    open FsCheck.Xunit
    open Vaughan.Domain
    open Vaughan.Scales
    open Vaughan.Chords
    open Vaughan.Guitar
    open Vaughan.GuitarTab
    open Vaughan.Infrastructure
    open Vaughan.ImprovisationGuitar

    [<Fact>]
    let ``Should create guitar arpeggios from chords``() =
        [ chord C Minor7
          chord F Dominant7
          chord BFlat Major7 ]
        |> createArpeggiosFromChords 2 5
        |> List.length
        |> should equal 3

    [<Fact>]
    let ``Should create guitar scales from chords``() =
        [ chord C Minor7
          chord F Dominant7
          chord BFlat Major7 ]
        |> createScalesForChords 2 5
        |> List.length
        |> should equal 3

    [<Fact>]
    let ``Should create guitar tab arpeggios from chords``() =
        [ chord C Minor7
          chord F Dominant7
          chord BFlat Major7 ]
        |> createTabArpeggiosFromChords 2 5
        |> List.length
        |> should equal 3

    [<Fact>]
    let ``Should create guitar tab scales from chords``() =
        [ chord C Minor7
          chord F Dominant7
          chord BFlat Major7 ]
        |> createTabScalesFromChords 2 5
        |> List.length
        |> should equal 3

    [<Property>]
    let ``Should create patterns for arpeggio`` (chordRoot : Note)
        (quality : ChordQuality) =
        let chord = chord chordRoot quality
        let arpeggio = guitarArpeggio 5 9 chord
        generateArpeggioPatterns root arpeggio
        |> List.length = factorial chord.Notes.Length

    [<Fact>]
    let ``Should render tab for C arpeggio starting from root on position 2``() =
        [ StandardTunning
          Start

          Arpeggio
              (ascendingArpeggioFrom root (guitarArpeggio 2 5 (chord C Major)))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------3--||" + Environment.NewLine
                + "B||--------------5-----||" + Environment.NewLine
                + "G||-----------5--------||" + Environment.NewLine
                + "D||-----2--5-----------||" + Environment.NewLine
                + "A||--3-----------------||" + Environment.NewLine
                + "E||--------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for G arpeggio starting from root on position 2``() =
        [ StandardTunning
          Start

          Arpeggio
              (ascendingArpeggioFrom root (guitarArpeggio 2 5 (chord G Major)))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------3--||" + Environment.NewLine
                + "B||-----------------3-----||" + Environment.NewLine
                + "G||--------------4--------||" + Environment.NewLine
                + "D||-----------5-----------||" + Environment.NewLine
                + "A||-----2--5--------------||" + Environment.NewLine
                + "E||--3--------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C major 7 arpeggio descending starting from root on position 2``() =
        [ StandardTunning
          Start

          ArbArpeggio
              (descendingArpeggioFrom root (guitarArpeggio 2 5 (chord C Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------||" + Environment.NewLine
                + "B||-----------------------||" + Environment.NewLine
                + "G||--5--4-----------------||" + Environment.NewLine
                + "D||--------5--2-----------||" + Environment.NewLine
                + "A||--------------3--2-----||" + Environment.NewLine
                + "E||--------------------3--||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 arpeggio enclosing the root on position 2``() =
        [ StandardTunning
          Start

          Notes
              (enclosedAscendingArpeggioFrom root 12
                   (guitarArpeggio 2 5 (chord C Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------3--||" + Environment.NewLine
                + "B||-----------------------5-----||" + Environment.NewLine
                + "G||-----------------4--5--------||" + Environment.NewLine
                + "D||-----------2--5--------------||" + Environment.NewLine
                + "A||--4--2--3--------------------||" + Environment.NewLine
                + "E||-----------------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 arpeggio enclosing the third on position 2``() =
        [ StandardTunning
          Start

          Notes
              (enclosedAscendingArpeggioFrom third 12
                   (guitarArpeggio 2 5 (chord C Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------3--||" + Environment.NewLine
                + "B||--------------------5-----||" + Environment.NewLine
                + "G||--------------4--5--------||" + Environment.NewLine
                + "D||--3--1--2--5--------------||" + Environment.NewLine
                + "A||--------------------------||" + Environment.NewLine
                + "E||--------------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 arpeggio enclosing the root on position 5``() =
        [ StandardTunning
          Start

          Notes
              (enclosedAscendingArpeggioFrom root 12
                   (guitarArpeggio 5 8 (chord C Minor7)))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------6--8--||" + Environment.NewLine
                + "B||--------------------------8--------||"
                + Environment.NewLine
                + "G||--------------------5--8-----------||"
                + Environment.NewLine
                + "D||--------------5--8-----------------||"
                + Environment.NewLine
                + "A||-----------6-----------------------||"
                + Environment.NewLine
                + "E||--9--7--8--------------------------||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 arpeggio enclosing the third on position 5``() =
        [ StandardTunning
          Start

          Notes
              (enclosedAscendingArpeggioFrom third 12
                   (guitarArpeggio 5 8 (chord C Minor7)))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------6--8--||" + Environment.NewLine
                + "B||-----------------------8--------||" + Environment.NewLine
                + "G||-----------------5--8-----------||" + Environment.NewLine
                + "D||-----------5--8-----------------||" + Environment.NewLine
                + "A||--7--5--6-----------------------||" + Environment.NewLine
                + "E||--------------------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 descending arpeggio enclosing the root on position 2``() =
        [ StandardTunning
          Start

          Notes
              (enclosedDescendingArpeggioFrom root 12
                   (guitarArpeggio 2 5 (chord C Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------||" + Environment.NewLine
                + "B||-----------------------------||" + Environment.NewLine
                + "G||--6--4--5--4-----------------||" + Environment.NewLine
                + "D||--------------5--2-----------||" + Environment.NewLine
                + "A||--------------------3--2-----||" + Environment.NewLine
                + "E||--------------------------3--||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 descending arpeggio enclosing the third on position 2``() =
        [ StandardTunning
          Start

          Notes
              (enclosedDescendingArpeggioFrom third 12
                   (guitarArpeggio 2 5 (chord C Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------------||" + Environment.NewLine
                + "B||--6--4--5-----------------------||" + Environment.NewLine
                + "G||-----------5--4-----------------||" + Environment.NewLine
                + "D||-----------------5--2-----------||" + Environment.NewLine
                + "A||-----------------------3--2-----||" + Environment.NewLine
                + "E||-----------------------------3--||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 asscending arpeggio aproched from above the root on position 5``() =
        [ StandardTunning
          Start
          Notes(ascEightsRootAbove (guitarArpeggio 5 8 (chord BFlat Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------||" + Environment.NewLine
                + "B||-----------------------6--||" + Environment.NewLine
                + "G||--------------------7-----||" + Environment.NewLine
                + "D||--------------7--8--------||" + Environment.NewLine
                + "A||--------5--8--------------||" + Environment.NewLine
                + "E||--7--6--------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 descending arpeggio aproched from below the root on position 5``() =
        [ StandardTunning
          Start
          Notes(descEightsRootBelow (guitarArpeggio 5 8 (chord BFlat Major7)))
          End ]
        |> renderTab
        |> should equal
               ("e||--5--6--5-----------------||" + Environment.NewLine
                + "B||-----------6--------------||" + Environment.NewLine
                + "G||--------------7-----------||" + Environment.NewLine
                + "D||-----------------8--7-----||" + Environment.NewLine
                + "A||-----------------------8--||" + Environment.NewLine
                + "E||--------------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 scale to seventh``() =
        [ StandardTunning
          Start
          Notes(createAscendingScaleSequence 5 8 7 C (createScale Ionian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------||" + Environment.NewLine
                + "B||-----------------------||" + Environment.NewLine
                + "G||--------------------5--||" + Environment.NewLine
                + "D||--------------5--7-----||" + Environment.NewLine
                + "A||-----5--7--8-----------||" + Environment.NewLine
                + "E||--8--------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Major 7 scale from seventh``() =
        [ StandardTunning
          Start
          Notes(createDescendingScaleSequence 5 8 7 C (createScale Ionian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------||" + Environment.NewLine
                + "B||-----------------------||" + Environment.NewLine
                + "G||--5--------------------||" + Environment.NewLine
                + "D||-----7--5--------------||" + Environment.NewLine
                + "A||-----------8--7--5-----||" + Environment.NewLine
                + "E||--------------------8--||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale to seventh``() =
        [ StandardTunning
          Start

          Notes
              (createAscendingScaleSequenceFromRootToSeventh 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------||" + Environment.NewLine
                + "B||-----------------------||" + Environment.NewLine
                + "G||-----------------------||" + Environment.NewLine
                + "D||--------------5--7--8--||" + Environment.NewLine
                + "A||-----5--7--8-----------||" + Environment.NewLine
                + "E||--8--------------------||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up from seventh``() =
        [ StandardTunning
          Start

          Notes
              (createDescendingScaleSequenceFromSeventhToRoot 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------||" + Environment.NewLine
                + "B||-----------------------||" + Environment.NewLine
                + "G||-----------------------||" + Environment.NewLine
                + "D||--8--7--5--------------||" + Environment.NewLine
                + "A||-----------8--7--5-----||" + Environment.NewLine
                + "E||--------------------8--||" + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale down and up from seventh``() =
        [ StandardTunning
          Start
          Notes(createScaleSequenceRootToSeventh 5 8 (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------||"
                + Environment.NewLine
                + "B||-----------------------------------------||"
                + Environment.NewLine
                + "G||-----------------------------------------||"
                + Environment.NewLine
                + "D||--------------5--7--8--7--5--------------||"
                + Environment.NewLine
                + "A||-----5--7--8-----------------8--7--5-----||"
                + Environment.NewLine
                + "E||--8-----------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up to seventh in thirds``() =
        [ StandardTunning
          Start

          Notes
              (createAscendingScaleSequenceRootToSeventhInThirds 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------------------------||"
                + Environment.NewLine
                + "B||--------------------------------------------||"
                + Environment.NewLine
                + "G||-----------------------------------5-----7--||"
                + Environment.NewLine
                + "D||-----------------5-----7--5--8--7-----8-----||"
                + Environment.NewLine
                + "A||-----7--5--8--7-----8-----------------------||"
                + Environment.NewLine
                + "E||--8-----------------------------------------||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale down from seventh in thirds``() =
        [ StandardTunning
          Start

          Notes
              (createDescendingScaleSequenceRootToSeventhInThirds 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||--------------------------------------------||"
                + Environment.NewLine
                + "B||--------------------------------------------||"
                + Environment.NewLine
                + "G||--7-----5-----------------------------------||"
                + Environment.NewLine
                + "D||-----8-----7--8--5--7-----5-----------------||"
                + Environment.NewLine
                + "A||-----------------------8-----7--8--5--7-----||"
                + Environment.NewLine
                + "E||-----------------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up and down to seventh in thirds``() =
        [ StandardTunning
          Start

          Notes
              (createScaleSequenceRootToSeventhInThirds 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------------------------||"
                + Environment.NewLine
                + "B||-----------------------------------------------------------------------------------||"
                + Environment.NewLine
                + "G||-----------------------------------5-----7-----5-----------------------------------||"
                + Environment.NewLine
                + "D||-----------------5-----7--5--8--7-----8-----8-----7--8--5--7-----5-----------------||"
                + Environment.NewLine
                + "A||-----7--5--8--7-----8-----------------------------------------8-----7--8--5--7-----||"
                + Environment.NewLine
                + "E||--8-----------------------------------------------------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up to seventh in triads``() =
        [ StandardTunning
          Start

          Notes
              (createAscendingScaleSequenceRootToSeventhInTriads 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------||"
                + Environment.NewLine
                + "B||-----------------------------------------------------5--------6--||"
                + Environment.NewLine
                + "G||-----------------------------------5--------7-----5--------7-----||"
                + Environment.NewLine
                + "D||--------5--------7-----5--8-----7-----5--8-----7--------8--------||"
                + Environment.NewLine
                + "A||-----7-----5--8-----7--------8-----------------------------------||"
                + Environment.NewLine
                + "E||--8--------------------------------------------------------------||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale down from seventh in triads``() =
        [ StandardTunning
          Start

          Notes
              (createDescendingScaleSequenceRootToSeventhInTriads 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------||"
                + Environment.NewLine
                + "B||--6--------5-----------------------------------------------------||"
                + Environment.NewLine
                + "G||-----7--------5-----7--------5-----------------------------------||"
                + Environment.NewLine
                + "D||--------8--------7-----8--5-----7-----8--5-----7--------5--------||"
                + Environment.NewLine
                + "A||-----------------------------------8--------7-----8--5-----7-----||"
                + Environment.NewLine
                + "E||--------------------------------------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up and down to seventh in triads``() =
        [ StandardTunning
          Start

          Notes
              (createScaleSequenceRootToSeventhInTriads 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------------------------------------------------------------------||"
                + Environment.NewLine
                + "B||-----------------------------------------------------5--------6--------5-----------------------------------------------------||"
                + Environment.NewLine
                + "G||-----------------------------------5--------7-----5--------7-----7--------5-----7--------5-----------------------------------||"
                + Environment.NewLine
                + "D||--------5--------7-----5--8-----7-----5--8-----7--------8-----------8--------7-----8--5-----7-----8--5-----7--------5--------||"
                + Environment.NewLine
                + "A||-----7-----5--8-----7--------8-----------------------------------------------------------------8--------7-----8--5-----7-----||"
                + Environment.NewLine
                + "E||--8-----------------------------------------------------------------------------------------------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up to seventh in chords``() =
        [ StandardTunning
          Start

          Notes
              (createAscendingScaleSequenceRootToSeventhInChords 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------------------------5--||"
                + Environment.NewLine
                + "B||-----------------------------------------------5-----------6--------5--8--------6-----||"
                + Environment.NewLine
                + "G||-----------------------5-----------7--------5-----------7--------5-----------7--------||"
                + Environment.NewLine
                + "D||--------5--8--------7--------5--8--------7--------5--8--------7-----------8-----------||"
                + Environment.NewLine
                + "A||-----7--------5--8--------7-----------8-----------------------------------------------||"
                + Environment.NewLine
                + "E||--8-----------------------------------------------------------------------------------||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale down to seventh in chords``() =
        [ StandardTunning
          Start

          Notes
              (createDescendingScaleSequenceRootToSeventhInChords 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||--5-----------------------------------------------------------------------------------||"
                + Environment.NewLine
                + "B||-----6--------8--5--------6-----------5-----------------------------------------------||"
                + Environment.NewLine
                + "G||--------7-----------5--------7-----------5--------7-----------5-----------------------||"
                + Environment.NewLine
                + "D||-----------8-----------7--------8--5--------7--------8--5--------7--------8--5--------||"
                + Environment.NewLine
                + "A||-----------------------------------------------8-----------7--------8--5--------7-----||"
                + Environment.NewLine
                + "E||-----------------------------------------------------------------------------------8--||"
                + Environment.NewLine)

    [<Fact>]
    let ``Should render tab for C Dominant 7 scale up and down to seventh in chords``() =
        [ StandardTunning
          Start

          Notes
              (createScaleSequenceRootToSeventhInChords 5 8
                   (createScale Mixolydian C))
          End ]
        |> renderTab
        |> should equal
               ("e||-----------------------------------------------------------------------------------5-----------------------------------------------------------------------------------||"
                + Environment.NewLine
                + "B||-----------------------------------------------5-----------6--------5--8--------6-----6--------8--5--------6-----------5-----------------------------------------------||"
                + Environment.NewLine
                + "G||-----------------------5-----------7--------5-----------7--------5-----------7-----------7-----------5--------7-----------5--------7-----------5-----------------------||"
                + Environment.NewLine
                + "D||--------5--8--------7--------5--8--------7--------5--8--------7-----------8-----------------8-----------7--------8--5--------7--------8--5--------7--------8--5--------||"
                + Environment.NewLine
                + "A||-----7--------5--8--------7-----------8-----------------------------------------------------------------------------------------8-----------7--------8--5--------7-----||"
                + Environment.NewLine
                + "E||--8-----------------------------------------------------------------------------------------------------------------------------------------------------------------8--||"
                + Environment.NewLine)
