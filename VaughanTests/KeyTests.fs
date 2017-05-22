namespace VaughanTests
    module KeyTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open Swensen.Unquote
        open Vaughan.Domain
        open Vaughan.Notes
        open Vaughan.Keys

        [<Test>]
        let ``Should have notes for key``() =
            keyNotes CMajor =! [ C; D; E; F; G; A; B ]
            keyNotes GMajor =! [ G; A; B; C; D; E; FSharp ]
            keyNotes DMajor =! [ D; E; FSharp; G; A; B; CSharp ]
            keyNotes AMajor =! [ A; B; CSharp; D; E; FSharp; GSharp ]
            keyNotes EMajor =! [ E; FSharp; GSharp; A; B; CSharp; DSharp ]
            keyNotes BMajor =! [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ]
            keyNotes FSharpMajor =! [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ]
            keyNotes DFlatMajor =! [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ]
            keyNotes AFlatMajor =! [ AFlat; BFlat; C; DFlat; EFlat; F; G ]
            keyNotes GFlatMajor =! [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ]
            keyNotes EFlatMajor =! [ EFlat; F; G; AFlat; BFlat; C; D ]
            keyNotes BFlatMajor =! [ BFlat; C; D; EFlat; F; G; A ]
            keyNotes FMajor =! [ F; G; A; BFlat; C; D; E ]

            keyNotes AMinor =! [ A; B; C; D; E; F; G ]
            keyNotes EMinor =! [ E; FSharp; G; A; B; C; D ]
            keyNotes BMinor =! [ B; CSharp; D; E; FSharp; G; A ]
            keyNotes FSharpMinor =! [ FSharp; GSharp; A; B; CSharp; D; E ]
            keyNotes CSharpMinor =! [ CSharp; DSharp; E; FSharp; GSharp; A; B ]
            keyNotes GSharpMinor =! [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ]
            keyNotes EFlatMinor =! [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ]
            keyNotes BFlatMinor =! [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ]
            keyNotes FMinor =! [ F; G; AFlat; BFlat; C; DFlat; EFlat ]
            keyNotes CMinor =! [ C; D; EFlat; F; G; AFlat; BFlat ]
            keyNotes GMinor =! [ G; A; BFlat; C; D; EFlat; F ]
            keyNotes DMinor =! [ D; E; F; G; A; BFlat; C ]

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