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
    WithSynth(Fm, [
                WithFx(Reverb, [
                            PlayNote(C, OneLine, [Amplitude(5.0<loud>); Panning(-1.0<pan>)]);
                            Sleep(1<s>);
                            PlayChord(chord C Major, TwoLine, [Amplitude(1.0<loud>); Panning(1.0<pan>)])
                            ])
                ])]
|> toSonicPiScript
|> sonicPiSend