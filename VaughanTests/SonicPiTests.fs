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
        let ``Should generate SonicPi DSL for use synth``() =
            UseSynth(Fm) |> toSonicPiScript =! "use_synth :fm"

        [<Test>]
        let ``Should generate SonicPi DSL for synth block withempty block``() =
            WithSynth(Fm, []) |> toSonicPiScript =! sprintf "with_synth :fm do\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for synth block with one instruction block``() =
            WithSynth(Fm, [PlayNote(C, OneLine, None)]) 
            |> toSonicPiScript =! "with_synth :fm do\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for synth block with two instructions block``() =
            WithSynth(Fm, [PlayNote(C, OneLine, None); PlayNote(C, OneLine, None)]) 
            |> toSonicPiScript =! "with_synth :fm do\nplay 48\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for sleep``() =
            Sleep(1<s>) |> toSonicPiScript =! "sleep 1"

        [<Test>]
        let ``Should generate SonicPi DSL for play note``() =
            PlayNote(C, OneLine, None)|> toSonicPiScript =! "play 48"
        
        [<Test>]
        let ``Should generate SonicPi DSL for play chord``() =
            PlayChord(chord C Major, TwoLine, None) |> toSonicPiScript =! "play [60,64,67]"

        [<Test>]
        let ``Should generate SonicPi DSL for play chord major 7th``() =
            PlayChord(chord C Major7, TwoLine, None) |> toSonicPiScript =! "play [60,64,67,71]"

        [<Test>]
        let ``Should generate SonicPi DSL for fx empty block``() =
            WithFx(Reverb,[]) |> toSonicPiScript =! "with_fx :reverb do\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for fx one instruction block``() =
            WithFx(Reverb,[PlayNote(C, OneLine, None)]) 
            |> toSonicPiScript =! "with_fx :reverb do\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL for fx two instructions block``() =
            WithFx(Reverb,[PlayNote(C, OneLine, None); PlayNote(C, OneLine, None)]) 
            |> toSonicPiScript =! "with_fx :reverb do\nplay 48\nplay 48\nend"

        [<Test>]
        let ``Should generate SonicPi DSL with one instruction block``() =
            Statments([PlayNote(C, OneLine, None);]) 
            |> toSonicPiScript =! "play 48\n"

        [<Test>]
        let ``Should generate SonicPi DSL with two instructions block``() =
            Statments([UseSynth(Fm); PlayNote(C, OneLine, None);]) 
            |> toSonicPiScript =! "use_synth :fm\nplay 48\n"