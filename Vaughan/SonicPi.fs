namespace Vaughan

    module SonicPi =
        open System
        open System.Net
        open System.Net.Sockets
        open Rug.Osc
        open Domain
        open Notes

        let private RUN_COMMAND = "/run-code"
        let private STOP_COMMAND = "/stop-all-jobs"
        let private ID = "VAUGHAN_SONIC_PI_CLI"
        let sonicPiEndPoint = new IPEndPoint(IPAddress.Loopback, 4557) 

        let private synthToSonicPySynth synth =
            match synth with
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

        let sonicPiSend message =
            use udpClient = new UdpClient()
            let osc_message = OscMessage(RUN_COMMAND, ID, message).ToByteArray()
   
            udpClient.Connect(sonicPiEndPoint)
            udpClient.Send(osc_message, osc_message.Length) |> ignore
            udpClient.Close()

        let rec toSonicPiScript = function
            | Synth synth -> sprintf "use_synth %s" (synthToSonicPySynth synth)
            | PlayNote (note, octave) -> sprintf "play %i" (midiNumber note octave)
            | Statments s ->  s |> Seq.fold (fun acc elem -> 
                                             sprintf "%s%s%s" acc (toSonicPiScript elem) Environment.NewLine) 
                                             ""