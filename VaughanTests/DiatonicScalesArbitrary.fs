namespace VaughanTests
    module DiatonicScalesArbitrary =
        open FsCheck
        open Vaughan.Domain

        let isBlues = function Blues _ -> true | _ -> false
        let isMajorPentatonic = function MajorPentatonic _ -> true | _ -> false
        let isMinorPentatonic = function MinorPentatonic _ -> true | _ -> false
        let isWholeTone= function WholeTone _ -> true | _ -> false
        let isBebop= function Bebop _ -> true | _ -> false
        let isNeapolitanMinor = function NeapolitanMinor _ -> true | _ -> false
        let isSixthDiminishedScale = function MajorSixthDiminishedScale _ -> true | _ -> false
        let isMinorSixthDiminishedScale = function MinorSixthDiminishedScale _ -> true | _ -> false
        let isDominantDiminishedScale= function DominantDiminishedScale _ -> true | _ -> false
        let isDominantb5DiminishedScale = function Dominantb5DiminishedScale _ -> true | _ -> false
        let isHalfWholeDiminishedScale = function HalfWholeDiminished _ -> true | _ -> false
        let isLydianAugmented = function LydianAugmented _ -> true | _ -> false

        type DiatonicScales =
            static member DU () = Arb.Default.Derive () |> Arb.filter (not << isBlues) 
                                                        |> Arb.filter (not << isMajorPentatonic)
                                                        |> Arb.filter (not << isMinorPentatonic)
                                                        |> Arb.filter (not << isWholeTone)
                                                        |> Arb.filter (not << isBebop) 
                                                        |> Arb.filter (not << isNeapolitanMinor)
                                                        |> Arb.filter (not << isSixthDiminishedScale) 
                                                        |> Arb.filter (not << isMinorSixthDiminishedScale) 
                                                        |> Arb.filter (not << isDominantDiminishedScale)
                                                        |> Arb.filter (not << isDominantb5DiminishedScale)
                                                        |> Arb.filter (not << isHalfWholeDiminishedScale)
                                                        |> Arb.filter (not << isLydianAugmented)

