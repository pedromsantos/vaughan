namespace VaughanTests

    module ImprovisationGuitarTests =
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