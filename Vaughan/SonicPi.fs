namespace Vaughan

    module SonicPi =
        open System
        open System.Net
        open System.Net.Sockets
        open Rug.Osc
        open Domain
        open Notes
        open Chords

        let private RUN_COMMAND = "/run-code"
        let private STOP_COMMAND = "/stop-all-jobs"
        let private ID = "VAUGHAN_SONIC_PI_CLI"
        let sonicPiEndPoint = new IPEndPoint(IPAddress.Loopback, 4557) 

        let private synthToSonicPySynth = function
            | Beep -> ":beep"
            | BladeRunnerStyleStrings -> ":blade"
            | BrownNoise -> ":bnoise"
            | ChipBass -> ":chipbass"
            | ChipLead -> ":chiplead"
            | ChipNoise -> ":chipnoise"
            | ClipNoise -> ":cnoise"
            | DarkAmbience -> ":dark_ambience"
            | DetunedPulseWave -> ":dpulse"
            | DetunedSawWave -> ":dsaw"
            | DetunedTriangleWave -> ":dtri"
            | DullBell -> ":dull_bell"
            | Fm -> ":fm"
            | GreyNoise -> ":gnoise"
            | Growl -> ":growl"
            | Hollow -> ":hollow"
            | Hoover -> ":hoover"
            | ModulatedBeepWave -> ":mod_beep"
            | ModulatedDetunedSaWaves -> ":mod_dsaw"
            | ModulatedFm -> ":mod_fm"
            | ModulatedPulse -> ":mod_pulse"
            | ModulatedSawWave -> ":mod_saw"
            | ModulatedSineWave -> ":mod_sine"
            | ModulatedTriangleWave -> ":mod_tri"
            | Noise -> ":noise"
            | Piano -> ":piano"
            | Pluck -> ":pluck"
            | PinkNoise -> ":pnoise"
            | PretyBell -> ":pretty_bell"
            | TheProphet -> ":prophet"
            | PulseWave -> ":pulse"
            | SawWave -> ":saw"
            | SineWave -> ":sine"
            | SquareWave -> ":square"
            | PulseWaveWithSub -> ":subpulse"
            | SuperSaw -> ":supersaw"
            | TB303Emulation -> ":tb303"
            | TechSaws -> ":tech_saws"
            | TriangleWave -> ":tri"
            | Zawa -> ":zawa"

        let fxToSonicPiFx = function
            | BandPassFilter -> "bpf:" | BandEQFilter -> ":band_eq" | Bitcrusher -> ":bitcrusher" 
            | Compressor -> ":compressor" 
            | Distortion -> ":distortion" 
            | Echo -> ":echo" 
            | Flanger -> ":flanger" 
            | GVerb -> ":gverb" 
            | HighPassFilter -> ":hpf"
            | Krush -> ":krush" 
            | LevelAmplifier -> ":level"  | LowPassFilter -> ":lpf"
            | Mono -> ":mono"
            | NormalisedResonantLowPassFilter -> ":nlpf"  | NormalisedResonantHighPassFilter -> ":nrbpf" 
            | NormalisedHighPassFilter -> ":nhpf"  | NormalisedLowPassFilter -> ":nrlpf"  | Normaliser -> ":normaliser" 
            | NormalisedBandPassFilter -> ":nbpf"  | NormalisedResonantBandPassFilter -> ":nrhpf" 
            | Octaver -> ":octaver" 
            | PanSlicer -> ":panslicer"  | Pan -> ":pan"  | PitchShift -> ":pitch_shift" 
            | Reverb -> ":reverb"  | ResonantLowPassFilter -> ":rlpf"  | ResonantHighPassFilter -> ":rhpf" 
            | ResonantBandPassFilter -> ":rbpf"  | RingModulator -> ":ring_mod" 
            | Slicer -> ":slicer" 
            | TechnofromIXILang -> ":ixi_techno" 
            | Whammy -> ":whammy" 
            | Wobble -> ":wobble" 
            | Vowel -> ":vowel" 

        let playOptionToSonicPiPlayOption = function
            | Amplitude(v) -> "amp" 
            | Panning(v) -> "pan"
            | Release(v) -> "release"
            | Attack(v) -> "attack"
            | AttackLevel(v) -> "attack_level"
            | Sustain(v) -> "sustain"
            | SustainLevel(v) -> "sustain_level"
            | Decay(v) -> "decay"
            | DecayLevel(v) -> "decay_level"

        let sonicPiSend message =
            use udpClient = new UdpClient()
            let osc_message = OscMessage(RUN_COMMAND, ID, message).ToByteArray()
   
            udpClient.Connect(sonicPiEndPoint)
            udpClient.Send(osc_message, osc_message.Length) |> ignore
            udpClient.Close()

        let chordToSonicPi (chord:Chord) octave = 
            (notesMidiNumbers chord octave
            |> List.fold (fun acc n -> sprintf "%s%i," acc n) "").TrimEnd(',')

        let generateInnerStatments statments generator =
            (statments |> Seq.fold (fun acc st -> 
                sprintf "%s%s\n" acc (generator st)) 
                "")
        
        let playOptionValue = function
            | Amplitude(v) -> float(v)
            | Panning(v) -> float(v)
            | Release(v) -> float(v)
            | Attack(v) -> float(v)
            | AttackLevel(v) -> float(v)
            | Sustain(v) -> float(v)
            | SustainLevel(v) -> float(v)
            | Decay(v) -> float(v)
            | DecayLevel(v) -> float(v)

        let generatePlayOptionsStatments playOptions = 
            (playOptions |> List.fold (fun acc option -> 
                sprintf "%s,%s:%.2f" acc (playOptionToSonicPiPlayOption option) (playOptionValue option)) 
                "") 

        let rec toSonicPiScript = function
            | Sleep secs -> sprintf "sleep %i" secs 
            | UseSynth synth -> sprintf "use_synth %s" (synthToSonicPySynth synth)
            | PlayNote (note, octave, opts) -> sprintf "play %i%s" (midiNumber note octave) (generatePlayOptionsStatments opts)
            | PlayChord (chord, octave, opts) -> sprintf "play [%s]" (chordToSonicPi chord octave) 
            | WithFx (fx, sts) -> sprintf "with_fx %s do\n%send" (fxToSonicPiFx fx) (generateInnerStatments sts toSonicPiScript)
            | WithSynth (synth, sts) -> sprintf "with_synth %s do\n%send" (synthToSonicPySynth synth) (generateInnerStatments sts toSonicPiScript)
            | Statments sts -> generateInnerStatments sts toSonicPiScript