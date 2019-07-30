namespace Vaughan

module Infrastructure =
    let rotateByOne =
        function
        | [] -> []
        | f :: t -> t @ [ f ]

    let swapFirstTwo =
        function
        | [] -> []
        | f :: s :: r -> s :: f :: r
        | f -> f

    let swapSecondTwo =
        function
        | [] -> []
        | f :: s :: t :: r -> f :: t :: s :: r
        | f -> f

    let circularSequenceFromList (lst : 'a list) =
        let rec next() =
            seq {
                for element in lst do
                    yield element
                yield! next()
            }
        next()

    let private sequenceToIndexValueTupleSequence sequence =
        sequence |> Seq.mapi (fun i v -> i, v)

    let filterOddIndexElements sequence =
        sequence
        |> sequenceToIndexValueTupleSequence
        |> Seq.filter (fun (i, _) -> i % 2 = 0)
        |> Seq.map snd

    let cappedMinimum number cap =
        if number < cap then cap
        else number

    let minimumPositive number = cappedMinimum number 0

    let cappedMaximum number cap =
        if number > cap then cap
        else number

    let combineAll listOfLists =
        let prefix fs pfx = pfx :: fs
        let prefixWith pfxs fs = List.map (prefix fs) pfxs
        let prefixAll pfxs fs = List.collect (prefixWith pfxs) fs
        List.foldBack prefixAll listOfLists [ [] ]

    let commonElements mapFunction listOfLists =
        listOfLists
        |> Seq.map (mapFunction >> Set.ofList)
        |> Seq.reduce Set.intersect

    let rec combinations acc (lst : 'a list) =
        seq {
            match lst.Length, lst with
            | n, x :: xs ->
                if n > 0 then yield! combinations (x :: acc) xs
                if n >= 0 then yield! combinations acc xs
            | 0, [] -> yield acc
            | _, [] -> ()
        }

    let rec insertions x =
        function
        | [] -> [ [ x ] ]
        | (y :: ys) as l ->
            (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))

    let rec permutations =
        function
        | [] -> seq [ [] ]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)

    let factorial x =
        let rec tailRecursiveFactorial x acc =
            if x <= 1 then acc
            else tailRecursiveFactorial (x - 1) (acc * x)
        tailRecursiveFactorial x 1
