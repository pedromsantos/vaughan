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
        open Vaughan.ScaleHarmonizer
        open Vaughan.Scales
        open Vaughan.SpeechToMusic
        open Vaughan.ImprovisationGuitar

        // [<Fact>]
        // let ``Should create arpeggio melodic line from chords`` () =
        //     [chord C Minor7; chord F Dominant7; chord BFlat Major7]
        //     |> createArpeggioGuitarMelodicLineFromChords 2 5 
        //     |> List.length 
        //     |> should equal 3

        // [<Fact>]
        // let ``Should tabify arpeggios from chords`` () =
        //     [chord C Minor7; chord F Dominant7; chord BFlat Major7]
        //     |> tabifyArpeggiosFromChords 2 5
        //     |> List.head 
        //     |> should startWith "CMin7" 

        // [<Fact>]
        // let ``Should create scales melodic line from chords`` () =
        //     [chord C Minor7; chord F Dominant7; chord BFlat Major7]
        //     |> createScaleGuitarMelodicLineFromChords 2 5 
        //     |> List.length 
        //     |> should equal 3

        // [<Fact>]
        // let ``Should tabify scales from chords`` () =
        //     [chord C Minor7; chord F Dominant7; chord BFlat Major7]
        //     |> tabifyScalesFromChords 2 5
        //     |> List.head 
        //     |> should startWith "CMin7"

        // [<Fact>]
        // let ``Should create melodic line from notes`` () =
        //     createScaleNotes Ionian BFlat
        //     |> createGuitarMelodicLineFromNotes 2 5
        //     |> List.length 
        //     |> should equal 6