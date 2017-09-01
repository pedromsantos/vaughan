module InfrastructureTests
    open Xunit
    open FsUnit
    open FsUnit.Xunit
    open FsCheck
    open FsCheck.Xunit
    open Vaughan.Infrastructure

    [<Fact>]
    let ``Should rotate list``() =
        rotateByOne [] |> List.isEmpty |> should equal true
        rotateByOne [1] |> should equal [1]
        rotateByOne [1; 2] |> should equal [2; 1]
        rotateByOne [1; 2; 3] |> should equal [2; 3; 1]

    [<Property>]
    let ``Rotating list puts head as last`` (list :int list) =
        ( not (List.isEmpty list) )
            ==> lazy ((list |> rotateByOne |> List.last) = (list |> List.head))

    [<Fact>]
    let ``Should swap first 2 elements in list``() =
        swapFirstTwo [1; 2; 3] |> should equal [2; 1; 3]

    [<Property>]
    let ``Swapping first two elements in list twice undoes first swap`` (list :int list) =
        ( list |> List.length > 2 )
            ==> lazy ((list |> swapFirstTwo |> swapFirstTwo) = list)

    [<Fact>]
    let ``Should swap second two elements in list``() =
        swapSecondTwo [1; 2; 3] = [1; 3; 2]

    [<Property>]
    let ``Swapping second two elements in list twice undoes first swap`` (list :int list) =
        ( list |> List.length > 2 )
            ==> lazy ((list |> swapSecondTwo |> swapSecondTwo) = list)

    [<Fact>]
    let ``Should filter odd index elements``() =
        filterOddIndexElements {1 .. 6} |> List.ofSeq = ({1 .. 2 .. 6} |> List.ofSeq)

    [<Fact>]
    let ``Should create circular sequence from list``() =
        circularSequenceFromList [1; 2; 3] |> Seq.take 3 |> Seq.last |> should equal 3
        circularSequenceFromList [1; 2; 3] |> Seq.take 4 |> Seq.last |> should equal 1
        circularSequenceFromList [1; 2; 3] |> Seq.take 5 |> Seq.last |> should equal 2