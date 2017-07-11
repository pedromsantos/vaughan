namespace VaughanTests
    module KeyTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open FsUnit
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Keys

        [<Test>]
        let ``Should have notes for key``() =
            keyNotes CMajor |> should equal [ C; D; E; F; G; A; B ]
            keyNotes GMajor |> should equal [ G; A; B; C; D; E; FSharp ]
            keyNotes DMajor |> should equal [ D; E; FSharp; G; A; B; CSharp ]
            keyNotes AMajor |> should equal [ A; B; CSharp; D; E; FSharp; GSharp ]
            keyNotes EMajor |> should equal [ E; FSharp; GSharp; A; B; CSharp; DSharp ]
            keyNotes BMajor |> should equal [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ]
            keyNotes FSharpMajor |> should equal [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ]
            keyNotes DFlatMajor |> should equal [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ]
            keyNotes AFlatMajor |> should equal [ AFlat; BFlat; C; DFlat; EFlat; F; G ]
            keyNotes GFlatMajor |> should equal [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ]
            keyNotes EFlatMajor |> should equal [ EFlat; F; G; AFlat; BFlat; C; D ]
            keyNotes BFlatMajor |> should equal [ BFlat; C; D; EFlat; F; G; A ]
            keyNotes FMajor |> should equal [ F; G; A; BFlat; C; D; E ]

            keyNotes AMinor |> should equal [ A; B; C; D; E; F; G ]
            keyNotes EMinor |> should equal [ E; FSharp; G; A; B; C; D ]
            keyNotes BMinor |> should equal [ B; CSharp; D; E; FSharp; G; A ]
            keyNotes FSharpMinor |> should equal [ FSharp; GSharp; A; B; CSharp; D; E ]
            keyNotes CSharpMinor |> should equal [ CSharp; DSharp; E; FSharp; GSharp; A; B ]
            keyNotes GSharpMinor |> should equal [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ]
            keyNotes EFlatMinor |> should equal [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ]
            keyNotes BFlatMinor |> should equal [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ]
            keyNotes FMinor |> should equal [ F; G; AFlat; BFlat; C; DFlat; EFlat ]
            keyNotes CMinor |> should equal [ C; D; EFlat; F; G; AFlat; BFlat ]
            keyNotes GMinor |> should equal [ G; A; BFlat; C; D; EFlat; F ]
            keyNotes DMinor |> should equal [ D; E; F; G; A; BFlat; C ]

        [<Property>]
        let ``Major keys have formula R, W, W, H, W, W, W, H and Minor keys have formula R, W, H, W, W, H, W, W`` (key :Key) =
            let majorKeyformula =
                [Unisson; MajorSecond; MajorSecond; MinorSecond; MajorSecond; MajorSecond; MajorSecond; MinorSecond]
            let minorKeyformula =
                [Unisson; MajorSecond; MinorSecond; MajorSecond; MajorSecond; MinorSecond; MajorSecond; MajorSecond]

            let notesForKey = keyNotes key
            let notesForKeyWithOctave = notesForKey@[notesForKey.Head]
            let intervalsForKey =
                notesForKeyWithOctave
                |> List.mapi
                    (fun i n -> if i = 0 then Unisson else intervalBetween notesForKeyWithOctave.[i - 1] n)

            intervalsForKey = majorKeyformula || intervalsForKey = minorKeyformula