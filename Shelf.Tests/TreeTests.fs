namespace Shelf.Tests

open Shelf
open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit

[<Arbitrary(typeof<Generators>)>]
module TreeTests =

//    [<Fact>]
//    let ``Can get items from single node``() =
//        let tree = { Root = Leaf(Vec.ofArray [|1; 2; 3|], Vec.ofArray [|"one"; "two"; "three"|]); BranchFactor = 5 }
//
//        Tree.search 1 tree |> should equal (Some("one"))
//        Tree.search 2 tree |> should equal (Some("two"))
//        Tree.search 3 tree |> should equal (Some("three"))
//
//        Tree.search 0 tree |> should equal None
//        Tree.search 4 tree |> should equal None
//
//    [<Fact>]
//    let ``Can get items from simple hierarchy``() =
//        let left = Leaf(Vec.ofArray [|1; 2;|], Vec.ofArray [|"one"; "two"|])
//        let middle = Leaf(Vec.ofArray [|3; 4;|], Vec.ofArray [|"three"; "four"|])
//        let right = Leaf(Vec.ofArray [|5; 6;|], Vec.ofArray [|"five"; "six"|])
//        let tree = {
//            BranchFactor = 5;
//            Root = Internal(Vec.ofArray [|3; 5|], Vec.ofArray [|left; middle; right|])
//            }
//    
//        Tree.search 1 tree |> should equal (Some("one"))
//        Tree.search 2 tree |> should equal (Some("two"))
//        Tree.search 3 tree |> should equal (Some("three"))
//        Tree.search 4 tree |> should equal (Some("four"))
//        Tree.search 5 tree |> should equal (Some("five"))
//        Tree.search 6 tree |> should equal (Some("six"))
//
//        Tree.search 0 tree |> should equal None
//        Tree.search 7 tree |> should equal None

    let unpack = List.map (fun (DbEntry(x)) -> x)

    let buildTree order l =
        let tree = Tree.empty order
        List.iter (Tree.insert tree) (unpack l)
        tree

    let canRetrieveAllValues tree entries =
        unpack entries
        |> List.forall (fun (k, v) -> (Tree.search k tree) = Some(v))

    let allLeavesAtSameDepth tree =
        let depths = 
            let rec getDepths depth node = seq {
                match node with
                    | Internal(_, children) -> 
                        for child in children do
                            yield! getDepths (depth + 1) child
                    | Leaf(_) -> yield depth
            }
            getDepths 0 tree.Root
            |> List.ofSeq

        match List.length depths with
            | 0 -> failwith "How can this happen!?"
            | 1 -> true
            | _ -> Seq.pairwise depths |> Seq.forall (fun (a, b) -> a = b)

    let getAllNodes tree =
        let rec nodes' n = seq {
            yield n
            match n with
                | Internal (_, children) -> yield! children |> Seq.collect nodes'
                | Leaf(_) -> ()
        }
        nodes' tree.Root

    let rec getKeys node = seq {
        match node with
            | Internal (keys, children) ->
                yield! keys
                yield! Seq.collect getKeys children
            | Leaf (keys, _, _) ->
                yield! keys
    }

    let traverseLeaves tree =
        let rec traverse' node = seq {
            match node with
                | Leaf(_, _, next) ->
                    yield node
                    if (!next).IsSome then yield! traverse' (!next).Value
                | Internal(_) -> failwith "Tree is invalid - encountered internal node during leaf scan"
        } 
        traverse' tree.FirstLeaf

    let allKeysAreOrdered tree =
        let isOrdered lst = 
            lst 
            |> Seq.pairwise 
            |> Seq.forall (fun (a, b) -> a < b)
        let keysAreOrdered = function
            | Internal (keys, _) -> isOrdered keys
            | Leaf (keys, _, _)     -> isOrdered keys

        getAllNodes tree
        |> Seq.forall keysAreOrdered 

    let keysToRightAreAtLeastAsLarge tree =
        let checkKeys = function
            | Internal (keys, children) -> 
                Seq.zip keys (Seq.skip 1 children)  // zip key together with child to right
                |> Seq.forall (fun (key, child) -> getKeys child |> Seq.forall (key |> (<=) ) )
            | Leaf (_, _, _) -> true

        getAllNodes tree
        |> Seq.forall checkKeys

    let keysToLeftOfChildAreLesser tree =
        let checkKeys = function
            | Internal (keys, children) ->
                Seq.zip keys children  // zip key together with child to left
                |> Seq.forall (fun (key, child) -> getKeys child |> Seq.forall (key |> (>) ) )
            | Leaf (_, _, _) -> true

        getAllNodes tree
        |> Seq.forall checkKeys

    let nodesDoNotExceedBranchFactor tree =
        getAllNodes tree
        |> Seq.forall (
            function
                | Internal(keys, _) -> keys.Count <= tree.BranchFactor
                | Leaf(keys, _, _) -> keys.Count <= tree.BranchFactor)

    let nodesHaveCorrectNumOfChildren tree =
        getAllNodes tree
        |> Seq.forall (
            function
                | Internal(keys, children)  -> children.Count = keys.Count + 1
                | Leaf(keys, values, _)     -> values.Count = keys.Count)

    let leafNodesTraverseInSortOrder tree =
        traverseLeaves tree
        |> Seq.collect getKeys
        |> Seq.pairwise
        |> Seq.forall (fun (a, b) -> a < b)

    let leafTraversalCoversAllKeys tree entries =
        let treeKeys =
            traverseLeaves tree
            |> Seq.collect getKeys
            |> Seq.sort
        let entryKeys =
            entries
            |> Seq.map (fun (DbEntry(k, _)) -> k)
            |> Seq.sort
        Seq.zip treeKeys entryKeys
        |> Seq.forall (fun (a, b) -> a = b)
        && Seq.length treeKeys = Seq.length entryKeys
        
    [<Property(StartSize= 1, EndSize= 1000)>]
    let ``Check that tree is valid for various input`` (UniqueKeys(entries)) (Order(order)) =
        let tree = buildTree order entries

        "Can retrieve correct value for all keys"   @| (canRetrieveAllValues tree entries)  .&.
        "All leaves are at same depth"              @| (allLeavesAtSameDepth tree)          .&.
        "All keys are ordered within node"          @| (allKeysAreOrdered tree)             .&.
        "Keys of child to right >= key"             @| (keysToRightAreAtLeastAsLarge tree)  .&.
        "Keys of child to left are < key"           @| (keysToLeftOfChildAreLesser tree)    .&.
        "Nodes to not exceed B keys"                @| (nodesDoNotExceedBranchFactor tree)  .&.
        "Nodes have correct number of children"     @| (nodesHaveCorrectNumOfChildren tree) .&.
        "Leaf nodes traverse in sort order"         @| (leafNodesTraverseInSortOrder tree)  .&.
        "Leaf traversal covers all keys"            @| (leafTraversalCoversAllKeys tree entries)             

    

    [<Fact>]
    let specificTest() =
        let entries = [DbEntry (1545814853, "\x16\xC'\x1D"); DbEntry (-18641, "\x0")]
        let order = 5

        let tree = buildTree order entries

        leafNodesTraverseInSortOrder tree
        |> should equal true

