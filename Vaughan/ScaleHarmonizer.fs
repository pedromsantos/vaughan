namespace Vaughan

    module ScaleHarmonizer =
        open Infrastructure
        open Domain
        open Chords

        let private thirds (fromPosition:ScaleDegree) scale =
            let octave = 16

            scale
            |> circularSequenceFromList
            |> Seq.skip (int fromPosition)
            |> Seq.take octave
            |> filterOddIndexElements
            |> Seq.toList

        let private harmonizer forDegree scale =
            let thirdsList = scale |> thirds forDegree |> List.take 7
            {
                Notes = [(thirdsList.[0], Root);
                        (thirdsList.[1], Third);
                        (thirdsList.[2], Fifth);
                        (thirdsList.[3], Seventh);
                        (thirdsList.[4], Ninth);
                        (thirdsList.[5], Eleventh);
                        (thirdsList.[6], Thirteenth)];
                ChordType = Closed;
                Name =  ""
            }

        let private harmonizeScaleDegreeWithNotes forDegree scale notes =
            let complete = harmonizer forDegree scale
            let chord = {complete with Notes = complete.Notes |> List.take notes}
            {chord with Name = name chord}

        let private harmonize forDegree lastFunction scale =
            match lastFunction with
            | Seventh -> harmonizeScaleDegreeWithNotes forDegree scale 4
            | Ninth -> harmonizeScaleDegreeWithNotes forDegree scale 5
            | _ -> harmonizeScaleDegreeWithNotes forDegree scale 3

        let ninthsHarmonizer forDegree scale =
            harmonize forDegree Ninth scale

        let seventhsHarmonizer forDegree scale =
            harmonize forDegree Seventh scale

        let triadsHarmonizer forDegree scale =
            let complete = harmonize forDegree Fifth scale
            {complete with ChordType = Triad}