namespace VaughanTests
    module ScaleTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Scales

        [<Test>]
        let ``Should have notes for scales``() =
            createScale Ionian C =! [ C; D; E; F; G; A; B ]
            createScale Dorian C =! [ C; D; EFlat; F; G; A; BFlat ]
            createScale Phrygian C =! [ C; DFlat; EFlat; F; G; AFlat; BFlat ]
            createScale Lydian C =! [ C; D; E; FSharp; G; A; B ]
            createScale Mixolydian C =! [ C; D; E; F; G; A; BFlat ]
            createScale Aolian C =! [ C; D; EFlat; F; G; AFlat; BFlat ]
            createScale Locrian C =! [ C; DFlat; EFlat; F; GFlat; AFlat; BFlat ]
            createScale MajorPentatonic C =! [ C; D; E; G; A;]
            createScale MinorPentatonic C =! [ C; EFlat; F; G; BFlat ]
            createScale Blues C =! [ C; EFlat; F; GFlat; G; BFlat ]
            createScale HarmonicMinor C =! [ C; D; EFlat; F; G; AFlat; B ]
            createScale MelodicMinor C =! [ C; D; EFlat; F; G; A; B ]
            createScale Dorianb2 C =! [ C; DFlat; EFlat; F; G; A; BFlat ]
            createScale LydianAugmented C =! [ C; D; E; FSharp; GSharp; A; B ]
            createScale LydianDominant C =! [ C; D; E; FSharp; G; A; BFlat ]
            createScale Mixolydianb6 C =! [ C; D; E; F; G; AFlat; BFlat ]
            createScale LocrianSharp2 C =! [ C; D; EFlat; F; GFlat; AFlat; BFlat ]
            createScale AlteredDominant C =! [ C; DFlat; DSharp; E; GFlat; GSharp; BFlat ]
            createScale HalfWholeDiminished C =! [ C; DFlat; EFlat; E; FSharp; G; A; BFlat ]
            createScale WholeTone C =! [ C; D; E; GFlat; GSharp; BFlat ]