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
        let private sonicPiEndPoint = new IPEndPoint(IPAddress.Loopback, 4557) 

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

        let private fxToSonicPiFx = function
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

        let private playOptionToSonicPiPlayOption = function
            | Amplitude(v) -> "amp" 
            | Panning(v) -> "pan"
            | Release(v) -> "release"
            | Attack(v) -> "attack"
            | AttackLevel(v) -> "attack_level"
            | Sustain(v) -> "sustain"
            | SustainLevel(v) -> "sustain_level"
            | Decay(v) -> "decay"
            | DecayLevel(v) -> "decay_level"

        let private playOptionValue = function
            | Amplitude(v) -> float(v)
            | Panning(v) -> float(v)
            | Release(v) -> float(v)
            | Attack(v) -> float(v)
            | AttackLevel(v) -> float(v)
            | Sustain(v) -> float(v)
            | SustainLevel(v) -> float(v)
            | Decay(v) -> float(v)
            | DecayLevel(v) -> float(v)

        let private fxOptionToSonicPiFxOption = function
            | Amp(v) -> "amp" 
            | PreAmp(v) -> "pre_amp" 
            | Mix(v) -> "mix"
            | PreMix(v) -> "pre_mix" 

        let private fxOptionValue = function
            | Amp(v) -> float(v)
            | PreAmp(v) -> float(v)
            | Mix(v) -> float(v)
            | PreMix(v) -> float(v)

        let private chordToSonicPi (chord:Chord) octave = 
            (Chords.notesMidiNumbers chord octave
            |> List.fold (fun acc n -> sprintf "%s%i," acc n) "").TrimEnd(',')

        let private scaleToSonicPi (notes:ScaleNotes) octave = 
            (Notes.notesMidiNumbers notes octave
            |> List.fold (fun acc n -> sprintf "%s%i," acc n) "").TrimEnd(',')

        let private generateInnerStatments statments generator =
            (statments |> Seq.fold (fun acc st -> 
                sprintf "%s%s\n" acc (generator st)) 
                "") 

        let private generatePlayOptionsStatments playOptions = 
            (playOptions |> List.fold (fun acc option -> 
                sprintf "%s,%s:%.2f" acc (playOptionToSonicPiPlayOption option) (playOptionValue option)) 
                "") 

        let private generateFxOptionsStatments fxOptions = 
            (fxOptions |> List.fold (fun acc option -> 
                sprintf "%s,%s:%.2f" acc (fxOptionToSonicPiFxOption option) (fxOptionValue option)) 
                "")

        let private generateSonicPiList list = 
            (list |> List.fold (fun acc item -> 
                sprintf "%s%.2f," acc (float(item))) 
                "").TrimEnd(',')

        let rec toSonicPiScript = function
            | Rest beats -> sprintf "sleep %i"beats
            | UseSynth synth -> sprintf "use_synth %s" (synthToSonicPySynth synth)
            | UseBpm bpm -> sprintf "use_bpm %i" (int(bpm))
            | WithBpm (bpm, sts) -> sprintf "with_bpm %i do\n%send" (int(bpm)) (generateInnerStatments sts toSonicPiScript)
            | PlayNote (note, octave, opts) -> sprintf "play %i%s" (midiNumber note octave) (generatePlayOptionsStatments opts)
            | PlayChord (chord, octave, opts) -> sprintf "play [%s]%s" (chordToSonicPi chord octave) (generatePlayOptionsStatments opts)
            | Arpeggio (notes, octave, times, opts) -> sprintf "play_pattern_timed [%s],[%s]%s" (scaleToSonicPi notes octave) (generateSonicPiList times) (generatePlayOptionsStatments opts)
            | WithFx (fx, opts, sts) -> sprintf "with_fx %s%s do\n%send" (fxToSonicPiFx fx) (generateFxOptionsStatments opts) (generateInnerStatments sts toSonicPiScript)
            | WithSynth (synth, sts) -> sprintf "with_synth %s do\n%send" (synthToSonicPySynth synth) (generateInnerStatments sts toSonicPiScript)
            | Repeat (repeats, sts) -> sprintf "%i.times do\n%send" repeats (generateInnerStatments sts toSonicPiScript)
            | LiveLoop (name, sts) -> sprintf "live_loop :%s do\n%send" name (generateInnerStatments sts toSonicPiScript)
            | Statments sts -> generateInnerStatments sts toSonicPiScript

        let sonicPiSend message =
            use udpClient = new UdpClient()
            let osc_message = OscMessage(RUN_COMMAND, ID, message).ToByteArray()
   
            udpClient.Connect(sonicPiEndPoint)
            udpClient.Send(osc_message, osc_message.Length) |> ignore
            udpClient.Close()