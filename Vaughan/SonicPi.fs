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

        let sonicPiSend message =
            use udpClient = new UdpClient()
            let osc_message = OscMessage(RUN_COMMAND, ID, message).ToByteArray()
   
            udpClient.Connect(sonicPiEndPoint)
            udpClient.Send(osc_message, osc_message.Length) |> ignore
            udpClient.Close()

        let chordToSonicPi (chord:Chord) octave = 
            (notesMidiNumbers chord octave
            |> List.fold (fun acc n -> sprintf "%s%i," acc n) "").TrimEnd(',')

        let parseInnerStatments statments parser =
            (statments |> Seq.fold (fun acc st -> 
                sprintf "%s%s%s" acc (parser st) Environment.NewLine) 
                "")

        let rec toSonicPiScript = function
            | Sleep secs -> sprintf "sleep %i" secs 
            | Synth synth -> sprintf "use_synth %s" (synthToSonicPySynth synth)
            | PlayNote (note, octave) -> sprintf "play %i" (midiNumber note octave)
            | PlayChord (chord, octave) -> sprintf "play [%s]" (chordToSonicPi chord octave) 
            | Fx (fx, sts) -> sprintf "with_fx %s do\n%send" (fxToSonicPiFx fx) (parseInnerStatments sts toSonicPiScript)
            | Statments sts -> parseInnerStatments sts toSonicPiScript