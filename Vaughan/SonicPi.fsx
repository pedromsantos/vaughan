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

Statments[
        Synth(Fm); 
        Fx(Reverb, [
            PlayNote(C, OneLine);
            Sleep(1<s>);
            PlayChord(chord C Major, TwoLine)
            ]);
        ]
|> toSonicPiScript
|> sonicPiSend