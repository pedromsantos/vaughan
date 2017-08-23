#r "../packages/Rug.Osc/lib/Rug.Osc.dll"

#load "Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "SonicPi.fs"

open Vaughan.Domain
open Vaughan.Notes
open Vaughan.Chords
open Vaughan.SonicPi

Statments
    [
        UseBpm 120<bpm>;
        LiveLoop("Foo", 
                    [
                        PlaySample(LoopingSample Garzul, []);
                        UseSynth TheProphet;
                        PlayNote(C, Great, [Release(8.0<beat>)]);
                        Rest 8<beat>
                    ])
    ]
|> toSonicPiScript
|> sonicPiRun


sonicPiStop