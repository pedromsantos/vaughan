namespace Vaughan

    module Keys =
        open Domain
        open Notes

        type private KeyAttributes = {Root:Note; Accidentals:int}
        type private IKeyAttributes = Key -> KeyAttributes

        let private keyFormula = function
            | AMajor -> {Root=A; Accidentals=3}
            | AFlatMajor -> {Root=AFlat; Accidentals=(-4)}
            | BMajor -> {Root=B; Accidentals=5}
            | BFlatMajor -> {Root=BFlat; Accidentals=(-2)}
            | CMajor -> {Root=C; Accidentals=0}
            | DMajor -> {Root=D; Accidentals=2}
            | DFlatMajor -> {Root=DFlat; Accidentals=(-5)}
            | EMajor -> {Root=E; Accidentals=4}
            | EFlatMajor -> {Root=EFlat; Accidentals=(-3)}
            | FMajor -> {Root=F; Accidentals=(-1)}
            | FSharpMajor -> {Root=FSharp; Accidentals=6}
            | GMajor -> {Root=G; Accidentals=1}
            | GFlatMajor -> {Root=GFlat; Accidentals=(-6)}
            | AMinor -> {Root=A; Accidentals=0}
            | BMinor -> {Root=B; Accidentals=2}
            | BFlatMinor -> {Root=BFlat; Accidentals=(-5)}
            | CMinor -> {Root=C; Accidentals=(-3)}
            | CSharpMinor -> {Root=CSharp; Accidentals=4}
            | DMinor -> {Root=D; Accidentals=(-1)}
            | EMinor -> {Root=E; Accidentals=1}
            | FMinor -> {Root=F; Accidentals=(-4)}
            | FSharpMinor -> {Root=FSharp; Accidentals=3}
            | GMinor -> {Root=G; Accidentals=(-2)}
            | GSharpMinor -> {Root=GSharp; Accidentals=5}
            | EFlatMinor -> {Root=EFlat; Accidentals=(-6)}

        let private root key =
            (keyFormula key).Root

        let private accidentals key =
            (keyFormula key).Accidentals

        let private flatKey fifths keyAccidents =
            (fifths |> List.rev |> List.skip -keyAccidents)
            @ (fifths
            |> List.rev
            |> List.take(-keyAccidents)
            |> List.map flat)

        let private sharpKey fifths keyAccidents =
            ((fifths |> List.skip keyAccidents) )
            @ (fifths
            |> List.take(keyAccidents)
            |> List.map sharp)

        let private rawKeyNotes key =
            let fifths = [F; C; G; D; A; E; B;]
            let keyAccidents = accidentals key

            if keyAccidents = 0 then
                fifths
            else
                if keyAccidents < 0 then
                    flatKey fifths keyAccidents
                else
                    sharpKey fifths keyAccidents

        let keyNotes:IKeyNotes = fun key ->
            (rawKeyNotes key
            |> List.sortBy pitch
            |> List.skipWhile (fun n -> n <> root key))
            @
            (rawKeyNotes key
            |> List.sortBy pitch
            |> List.takeWhile (fun n -> n <> root key))