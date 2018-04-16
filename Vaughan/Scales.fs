namespace Vaughan

    module Scales =
        open Notes
        open Chords

        type private ScalePattern = Interval list
        type private IScalePattern = ScaleType -> ScalePattern

        let private scalePattern:IScalePattern = function
            | Ionian -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorian -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Phrygian -> [Unisson; MinorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | Lydian -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Mixolydian -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Aolian -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | Locrian -> [Unisson; MinorSecond; MinorThird; PerfectFourth; DiminishedFifth; MinorSixth; MinorSeventh]
            | MajorPentatonic -> [Unisson; MajorSecond; MajorThird; PerfectFifth; MajorSixth]
            | MinorPentatonic -> [Unisson; MinorThird; PerfectFourth; PerfectFifth; MinorSeventh]
            | Blues -> [Unisson; MinorThird; PerfectFourth; DiminishedFifth; PerfectFifth; MinorSeventh]
            | HarmonicMinor -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MajorSeventh]
            | MelodicMinor -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MajorSeventh]
            | Dorianb2 -> [Unisson; MinorSecond; MinorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | NeapolitanMinor -> [Unisson; MinorSecond; MinorThird; PerfectFourth; PerfectFifth; MinorSixth; MajorSeventh]
            | LydianAugmented -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; AugmentedFifth; MajorSixth; MajorSeventh]
            | LydianDominant -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Mixolydianb6 -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | Bebop -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MajorSixth; MinorSeventh; MajorSeventh]
            | LocrianSharp2 -> [Unisson; MajorSecond; MinorThird; PerfectFourth; DiminishedFifth; MinorSixth; MinorSeventh]
            | AlteredDominant -> [Unisson; MinorSecond; AugmentedSecond; MajorThird; DiminishedFifth;  AugmentedFifth; MinorSeventh]
            | HalfWholeDiminished -> [Unisson; MinorSecond; MinorThird; MajorThird; AugmentedFourth;  PerfectFifth; MajorSixth; MinorSeventh]
            | WholeTone -> [Unisson; MajorSecond; MajorThird; DiminishedFifth; AugmentedFifth; MinorSeventh]
            | MajorSixthDiminishedScale -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; AugmentedFifth; MajorSixth; MajorSeventh]
            | MinorSixthDiminishedScale -> [Unisson; MajorSecond; MinorThird; PerfectFourth; PerfectFifth; AugmentedFifth; MajorSixth; MajorSeventh]
            | DominantDiminishedScale -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; AugmentedFifth; MinorSeventh; MajorSeventh]
            | Dominantb5DiminishedScale -> [Unisson; MajorSecond; MajorThird; PerfectFourth; DiminishedFifth; AugmentedFifth; MinorSeventh; MajorSeventh]

        let createScaleNotes:CreateScaleNotes = fun scale root ->
            scalePattern scale |> List.map (fun interval -> transpose root interval)

        let createScale (scale:ScaleType) (root:Note) =
            {Scale=scale; Notes=createScaleNotes scale root};

        let private createAllScalesFrom (root:Note) =
            [
                createScale Ionian root;
                createScale Dorian root;
                createScale Phrygian root;
                createScale Lydian root;
                createScale Mixolydian root;
                createScale Aolian root;
                createScale Locrian root;
                createScale MajorPentatonic root;
                createScale MinorPentatonic root;
                createScale Blues root;
                createScale HarmonicMinor root;
                createScale MelodicMinor root;
                createScale Dorianb2 root;
                createScale NeapolitanMinor root;
                createScale LydianAugmented root;
                createScale LydianDominant root;
                createScale Mixolydianb6 root;
                createScale Bebop root;
                createScale LocrianSharp2 root;
                createScale AlteredDominant root;
                createScale HalfWholeDiminished root;
                createScale WholeTone root;
                createScale MajorSixthDiminishedScale root;
                createScale MinorSixthDiminishedScale root;
                createScale DominantDiminishedScale root;
                createScale Dominantb5DiminishedScale root;
            ]

        let private scaleContainAllChordTones scale chordTones =
            ( scale |> List.filter (fun x ->
                                         (List.contains x chordTones)) |> List.sort) 
                                         = (chordTones |> List.sort)

        let scalesFitting (chord:Chord) =
            let chordTones = chord.Notes |> List.map fst

            [C; CSharp; DFlat; D; DSharp; EFlat; E; F; FSharp; GFlat; G; GSharp; AFlat; A; ASharp; BFlat; B]
            |> List.map (createAllScalesFrom 
                            >> (fun scls -> scls
                                            |> List.choose (fun scale -> 
                                                (if scaleContainAllChordTones scale.Notes chordTones 
                                                 then Some(scale) 
                                                 else None)))) 
            |> List.collect id