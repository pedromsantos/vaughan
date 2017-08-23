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
        UseBpm(120<bpm>)
        WithSynth(Fm, [
                    WithFx(Reverb, [Mix(0.5)], [
                                            Repeat(2, [
                                                    PlayNote(C, OneLine, [
                                                                        Amplitude(0.5<loud>);
                                                                        Panning(0.0<pan>);
                                                                        Attack(2.0<beat>);
                                                                        Release(2.0<beat>)]);
                                                    PlayChord(chord C Major, TwoLine, [
                                                                                    Amplitude(1.0<loud>); 
                                                                                    Release(2.0<beat>); 
                                                                                    Panning(1.0<pan>)]);
                                                    Rest(2<beat>);
                                                    Arpeggio([C; E; G; B], OneLine, [1.0<beat>], [])
                                                    ])
                                                ])
                ])
        ]
|> toSonicPiScript
|> sonicPiSend

Statments[
        UseBpm(120<bpm>)
        LiveLoop("Foo", [
                    PlaySample(LoopingSample(Garzul), []);
                    UseSynth(TheProphet); 
                    PlayNote(C, Great, [Release(8.0<beat>)]);
                    Rest(8<beat>)
                    ])
        ]
|> toSonicPiScript
|> sonicPiSend