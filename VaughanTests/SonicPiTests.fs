namespace VaughanTests
    module SonicPiTests =
        open Xunit
        open FsUnit
        open FsUnit.Xunit
        open FsCheck
        open FsCheck.Xunit
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Chords
        open Vaughan.SonicPi

        [<Fact>]
        let ``Should generate SonicPi DSL for use synth``() =
            UseSynth(Fm) |> toSonicPiScript |> should equal "use_synth :fm"

        [<Fact>]
        let ``Should generate SonicPi DSL for synth block with empty block``() =
            WithSynth(Fm, []) |> toSonicPiScript |> should equal (sprintf "with_synth :fm do\nend")

        [<Fact>]
        let ``Should generate SonicPi DSL for synth block with one instruction block``() =
            WithSynth(Fm, [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_synth :fm do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for synth block with two instructions block``() =
            WithSynth(Fm, [PlayNote(C, OneLine, []); PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_synth :fm do\nplay 48\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for sleep``() =
            Rest(1<beat>) |> toSonicPiScript |> should equal "sleep 1"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note``() =
            PlayNote(C, OneLine, [])|> toSonicPiScript |> should equal "play 48" 
        
        [<Fact>]
        let ``Should generate SonicPi DSL for play chord``() =
            PlayChord(chord C Major, TwoLine, []) |> toSonicPiScript |> should equal "play [60,64,67]"

        [<Fact>]
        let ``Should generate SonicPi DSL for play chord major 7th``() =
            PlayChord(chord C Major7, TwoLine, []) |> toSonicPiScript |> should equal "play [60,64,67,71]"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx empty block``() =
            WithFx(Reverb,[], []) |> toSonicPiScript |> should equal "with_fx :reverb do\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx one instruction block``() =
            WithFx(Reverb, [], [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx two instructions block``() =
            WithFx(Reverb, [], [PlayNote(C, OneLine, []); PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb do\nplay 48\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL with one instruction block``() =
            Statments([PlayNote(C, OneLine, []);]) 
            |> toSonicPiScript |> should equal "play 48\n"

        [<Fact>]
        let ``Should generate SonicPi DSL with two instructions block``() =
            Statments([UseSynth(Fm); PlayNote(C, OneLine, []);]) 
            |> toSonicPiScript |> should equal "use_synth :fm\nplay 48\n"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with amplitude 2``() =
            PlayNote(C, OneLine, [Amplitude(2.0<loud>)])|> toSonicPiScript |> should equal "play 48,amp:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with panning rigth``() =
            PlayNote(C, OneLine, [Panning(1.0<pan>)])|> toSonicPiScript |> should equal "play 48,pan:1.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with release 2``() =
            PlayNote(C, OneLine, [Release(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,release:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with attack 2``() =
            PlayNote(C, OneLine, [Attack(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,attack:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with attack level 2``() =
            PlayNote(C, OneLine, [AttackLevel(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,attack_level:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with sustain 2``() =
            PlayNote(C, OneLine, [Sustain(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,sustain:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with sustain level 2``() =
            PlayNote(C, OneLine, [SustainLevel(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,sustain_level:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with decay 2``() =
            PlayNote(C, OneLine, [Decay(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,decay:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with decay level 2``() =
            PlayNote(C, OneLine, [DecayLevel(2.0<beat>)])|> toSonicPiScript |> should equal "play 48,decay_level:2.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for play note with amplitude and panning``() =
            PlayNote(C, OneLine, [Amplitude(2.0<loud>); Panning(1.0<pan>)])|> toSonicPiScript |> should equal "play 48,amp:2.00,pan:1.00"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx with amplitude``() =
            WithFx(Reverb, [Amp(1.0<loud>)], [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb,amp:1.00 do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx with pre amplitude``() =
            WithFx(Reverb, [PreAmp(1.0<loud>)], [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb,pre_amp:1.00 do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx with mix``() =
            WithFx(Reverb, [Mix(1.0)], [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb,mix:1.00 do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for fx with pre mix``() =
            WithFx(Reverb, [PreMix(1.0)], [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "with_fx :reverb,pre_mix:1.00 do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for play pattern timed``() =
            Arpeggio([C; E; G; B], OneLine, [0.5<beat>], [])
            |> toSonicPiScript |> should equal "play_pattern_timed [48,52,55,59],[0.50]" 

        [<Fact>]
        let ``Should generate SonicPi DSL for play pattern timed different times``() =
            Arpeggio([C; E; G; B], OneLine, [0.5<beat>; 0.75<beat>], [])
            |> toSonicPiScript |> should equal "play_pattern_timed [48,52,55,59],[0.50,0.75]" 

        [<Fact>]
        let ``Should generate SonicPi iteration DSL with empty block``() =
            Repeat(2, []) 
            |> toSonicPiScript |> should equal "2.times do\nend"

        [<Fact>]
        let ``Should generate SonicPi iteration DSL with non empty block``() =
            Repeat(2, [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal "2.times do\nplay 48\nend"

        [<Fact>]
        let ``Should generate SonicPi DSL for use bpm``() =
            UseBpm(60<bpm>) |> toSonicPiScript |> should equal "use_bpm 60"

        [<Fact>]
        let ``Should generate SonicPi DSL for live loop block with empty block``() =
            LiveLoop("foo", []) |> toSonicPiScript |> should equal (sprintf "live_loop :foo do\nend")

        [<Fact>]
        let ``Should generate SonicPi DSL for live loop block with non empty block``() =
            LiveLoop("foo", [PlayNote(C, OneLine, [])]) 
            |> toSonicPiScript |> should equal (sprintf "live_loop :foo do\nplay 48\nend")

        [<Fact>]
        let ``Should generate SonicPi DSL for play sample``() =
            PlaySample(LoopingSample Garzul, [])|> toSonicPiScript |> should equal "sample :loop_garzul"