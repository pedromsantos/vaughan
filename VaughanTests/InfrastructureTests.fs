namespace VaughanTests
    module InfrastructureTests =
        open NUnit.Framework
        open FsCheck
        open FsCheck.NUnit
        open Swensen.Unquote
        open Swensen.Unquote.Extensions
        open Vaughan.Infrastructure

        [<Test>]
        let ``Should rotate list``() =
            rotateByOne [] |> List.isEmpty =! true
            rotateByOne [1] =! [1]
            rotateByOne [1; 2] =! [2; 1]
            rotateByOne [1; 2; 3] =! [2; 3; 1]

        [<Property>]
        let ``Rotating list puts head as last`` (list :int list) =
            ( not (List.isEmpty list) )
                ==> lazy ((list |> rotateByOne |> List.last) = (list |> List.head))

        [<Test>]
        let ``Should swap first 2 elements in list``() =
            swapFirstTwo [1; 2; 3] =! [2; 1; 3]

        [<Property>]
        let ``Swapping first two elements in list twice undoes first swap`` (list :int list) =
            ( list |> List.length > 2 )
                ==> lazy ((list |> swapFirstTwo |> swapFirstTwo) = list)

        [<Test>]
        let ``Should swap second two elements in list``() =
            swapSecondTwo [1; 2; 3] =! [1; 3; 2]

        [<Property>]
        let ``Swapping second two elements in list twice undoes first swap`` (list :int list) =
            ( list |> List.length > 2 )
                ==> lazy ((list |> swapSecondTwo |> swapSecondTwo) = list)

        [<Test>]
        let ``Should filter odd index elements``() =
            filterOddIndexElements {1 .. 6} |> List.ofSeq =! ({1 .. 2 .. 6} |> List.ofSeq)

        [<Test>]
        let ``Should create circular sequence from list``() =
            circularSequenceFromList [1; 2; 3] |> Seq.take 3 |> Seq.last =! 3
            circularSequenceFromList [1; 2; 3] |> Seq.take 4 |> Seq.last =! 1
            circularSequenceFromList [1; 2; 3] |> Seq.take 5 |> Seq.last =! 2