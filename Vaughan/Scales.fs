namespace Vaughan

    module Scales =
        open Domain
        open Notes

        type private ScalePattern = Interval list
        type private IScalePattern = Scale -> ScalePattern

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
            | LydianAugmented -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; AugmentedFifth; MajorSixth; MajorSeventh]
            | LydianDominant -> [Unisson; MajorSecond; MajorThird; AugmentedFourth; PerfectFifth; MajorSixth; MinorSeventh]
            | Mixolydianb6 -> [Unisson; MajorSecond; MajorThird; PerfectFourth; PerfectFifth; MinorSixth; MinorSeventh]
            | LocrianSharp2 -> [Unisson; MajorSecond; MinorThird; PerfectFourth; DiminishedFifth; MinorSixth; MinorSeventh]
            | AlteredDominant -> [Unisson; MinorSecond; AugmentedSecond; MajorThird; DiminishedFifth;  AugmentedFifth; MinorSeventh]
            | HalfWholeDiminished -> [Unisson; MinorSecond; MinorThird; MajorThird; AugmentedFourth;  PerfectFifth; MajorSixth; MinorSeventh]
            | WholeTone -> [Unisson; MajorSecond; MajorThird; DiminishedFifth; AugmentedFifth; MinorSeventh]

        let createScale:ICreateScale = fun scale root ->
            scalePattern scale |> List.map (fun interval -> transpose root interval)