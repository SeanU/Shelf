[<AutoOpen>]
module Types
open FsCheck

type DbEntry = 
    | DbEntry of (int * string)
    static member op_Explicit(DbEntry(x)) = x

type UniqueKeys =
    | UniqueKeys of DbEntry list
    static member op_Explicit(UniqueKeys(l)) = l

type Generators =
    static member DbEntry () =
        let makeEntry x y =
            System.Console.WriteLine("Making dbEntry")
            DbEntry(x, y)
        let dbEntry = 
            let arbInt = Arb.Default.DontSizeInt32().Generator
                         |> Gen.map (fun (DontSize(i)) -> i)
            Gen.map2 makeEntry arbInt Arb.generate<string>
        {new Arbitrary<DbEntry>() with
            override x.Generator = dbEntry}

    static member UniqueKeys () =
        let getDistinctKeys l =
            Seq.groupBy (fun (DbEntry(k, _)) -> k) l
            |> Seq.map (fun (_, vals) -> Seq.pick Some vals)
            |> List.ofSeq

        let dbEntryList = 
            Generators.DbEntry().Generator
            |> Gen.listOf
            |> Gen.map getDistinctKeys
            |> Gen.map UniqueKeys
        let shrink (UniqueKeys(l)) =
            // Gets all combinations of the list of size [1 .. n], 
            // then removes them from the input list.
            let rec comb acc set size = seq {
                match size, set with
                    | 0, [] -> yield acc
                    | _, [] -> ()
                    | n, x::xs ->
                        if n > 0 then yield! comb (x :: acc) xs (n - 1)   // take current item
                        if n >= 0 then yield! comb acc xs n              // skip current item
                }
            [1 .. List.length l]
            |> Seq.collect (comb [] l)
            |> Seq.map Set.ofList
            |> Seq.map (fun excludes -> List.filter (excludes.Contains >> not) l)
            |> Seq.map UniqueKeys
        {new Arbitrary<UniqueKeys>() with
            override x.Generator = dbEntryList
            override x.Shrinker l = shrink l}


        