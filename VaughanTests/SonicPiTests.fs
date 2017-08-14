namespace VaughanTests
    module SonicPiTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Chords
        open Vaughan.SonicPi

        [<Test>]
        let ``Should generate SonicPi DSL for synth``() =
            Synth(Fm) |> toSonicPiScript =! "use_synth :fm"

        [<Test>]
        let ``Should generate SonicPi DSL for sleep``() =
            Sleep(1<s>) |> toSonicPiScript =! "sleep 1"

        [<Test>]
        let ``Should generate SonicPi DSL for play note``() =
            PlayNote(C, OneLine)|> toSonicPiScript =! "play 48"
        
        [<Test>]
        let ``Should generate SonicPi DSL for play chord``() =
            PlayChord(chord C Major, TwoLine) |> toSonicPiScript =! "play [60,64,67]"

        [<Test>]
        let ``Should generate SonicPi DSL for play chord major 7th``() =
            PlayChord(chord C Major7, TwoLine) |> toSonicPiScript =! "play [60,64,67,71]"

        [<Test>]
        let ``Should generate SonicPi DSL for fx empty block``() =
            Fx(Reverb,[]) |> toSonicPiScript =! "with_fx :reverb do\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for fx one instruction block``() =
            Fx(Reverb,[PlayNote(C, OneLine)]) 
            |> toSonicPiScript =! "with_fx :reverb do\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for fx two instructions block``() =
            Fx(Reverb,[PlayNote(C, OneLine); PlayNote(C, OneLine)]) 
            |> toSonicPiScript =! "with_fx :reverb do\nplay 48\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL with one instruction block``() =
            Statments([PlayNote(C, OneLine);]) 
            |> toSonicPiScript =! "play 48\n"

        [<Test>]
        let ``Should generate SonicPi DSL with two instructions block``() =
            Statments([Synth(Fm); PlayNote(C, OneLine);]) 
            |> toSonicPiScript =! "use_synth :fm\nplay 48\n"