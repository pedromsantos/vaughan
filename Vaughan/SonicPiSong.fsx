#r "../packages/bespoke-osc-library/1.0.0/lib/Bespoke.Common.Osc.dll"

#load "Infrastructure.fs"
#load "Domain.fs"
#load "Notes.fs"
#load "Chords.fs"
#load "SonicPi.fs"

open Vaughan.Domain
open Vaughan.SonicPi

let section = Section(
                (4<beat>, Quarter),
                CMajor,
                [
                    [
                        1.0<beat>, [(D, OneLine, Quarter); (F, OneLine, Quarter); (A, OneLine, Quarter)];
                        2.0<beat>, [(D, OneLine, Quarter); (F, OneLine, Quarter); (A, OneLine, Quarter)];
                        3.0<beat>, [(D, OneLine, Quarter); (F, OneLine, Quarter); (A, OneLine, Quarter)];
                        4.0<beat>, [(D, OneLine, Quarter); (F, OneLine, Quarter); (A, OneLine, Quarter)]
                    ];
                    [
                        1.0<beat>, [(G, OneLine, Quarter); (B, OneLine, Quarter); (D, OneLine, Quarter)];
                        2.0<beat>, [(G, OneLine, Quarter); (B, OneLine, Quarter); (D, OneLine, Quarter)];
                        3.0<beat>, [(G, OneLine, Quarter); (B, OneLine, Quarter); (D, OneLine, Quarter)];
                        4.0<beat>, [(G, OneLine, Quarter); (B, OneLine, Quarter); (D, OneLine, Quarter)]
                    ];
                    [
                        1.0<beat>, [(C, OneLine, Quarter); (E, OneLine, Quarter); (G, OneLine, Quarter)];
                        2.0<beat>, [(C, OneLine, Quarter); (E, OneLine, Quarter); (G, OneLine, Quarter)];
                        3.0<beat>, [(C, OneLine, Quarter); (E, OneLine, Quarter); (G, OneLine, Quarter)];
                        4.0<beat>, [(C, OneLine, Quarter); (E, OneLine, Quarter); (G, OneLine, Quarter)]
                    ];
                ])

Statments[UseBpm 120<bpm>; WithSynth(Pluck, [section])]
|> toSonicPiScript
|> sonicPiRun
