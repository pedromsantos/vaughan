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

WithSynth(Fm, [
            WithFx(Reverb, [Mix(0.5)], [
                                    Repeat(2, [
                                                PlayNote(C, OneLine, [
                                                                    Amplitude(0.5<loud>);
                                                                    Panning(0.0<pan>);
                                                                    Attack(2.0<beat>);
                                                                    Release(2.0<beat>)])
                                                PlayChord(chord C Major, TwoLine, [
                                                                                Amplitude(1.0<loud>); 
                                                                                Release(2.0<beat>); 
                                                                                Panning(1.0<pan>)]);
                                                Sleep(2<beat>);
                                                PlayPatternTimed([C; E; G; B], OneLine, [1.0<beat>;], [])
                                ])
                        ])
            ])
|> toSonicPiScript
|> sonicPiSend