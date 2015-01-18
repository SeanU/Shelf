namespace Shelf

type Node<'k, 'v> when 'k : comparison and 'v : equality =
        // Within list, subree in tuple contains all keys less than key in tuple.
        // Bare subree is for anything greater than or equal to all keys
    | Internal of vec<'k> * vec<Node<'k, 'v>>  
    | Leaf of vec<'k> * vec<'v> * Option<Node<'k, 'v>> ref

type Tree<'k, 'v> when 'k : comparison and 'v : equality = {
    BranchFactor : int;
    mutable Root : Node<'k, 'v>;
    mutable FirstLeaf : Node<'k, 'v>
    }

module Tree =

    let private emptyNode () = Leaf(new vec<'k>(), new vec<'v>(), ref None)

    let empty order = 
        let firstNode = emptyNode ()
        { BranchFactor = order; Root = firstNode; FirstLeaf = firstNode }

    let private getSubtreeIndex key keys =
        match Vec.tryFindIndex (key |> (<)) keys with
            | Some(index)   -> index
            | None          -> keys.Count // always = pointers.Length - 1

    let private getSubtree key keys (pointers : vec<'v>) =
        pointers.[getSubtreeIndex key keys]

    let private getItemIndex key keys = Vec.tryFindIndex(key |> (<=)) keys 

    let private tryGetItem key keys (pointers : vec<'v>) =
        match getItemIndex key keys with
            | Some(index) when keys.[index] = key 
                            -> Some(pointers.[index])
            | Some(index)   -> None
            | None          -> None


    let search key tree = 
        let rec search' node = 
            match node with
                | Internal(keys, pointers)  -> search' (getSubtree key keys pointers)
                | Leaf(keys, values, _)     -> tryGetItem key keys values
        let result = search' tree.Root
        result

    let rec private getFirstKey = function
        | Internal(_, children) -> getFirstKey children.[0]
        | Leaf(keys, _, _)      -> keys.[0]

    let private split node = 
        match node with
            | Internal(keys, pointers) -> 
                let mid = keys.Count - (keys.Count / 2)
                let newSet = Internal(Vec.split mid keys, Vec.split mid pointers)
                Vec.removeLast keys  // Originally key for what's now left branch of newSet, so no longer used
                newSet
            | Leaf(keys, values, next) ->
                let mid = keys.Count - (keys.Count / 2)
                let newNode = Leaf(Vec.split mid keys, Vec.split mid values, ref !next)
                next := Some newNode
                newNode

    let private splitIfNeeded branchFactor node = 
        match node with
            | Internal(keys, _) when keys.Count > branchFactor ->
                Some(split node)
            | Leaf(keys, _, _) when keys.Count > branchFactor ->
                Some(split node)
            | _ -> None

    let private addChild branchFactor node splitNode =
        let key = getFirstKey splitNode
        match node with
            | Leaf(_) -> failwith "Cannot add child to leaf node"
            | Internal(keys, pointers) ->
                let index = getSubtreeIndex key keys
                Vec.insert index key keys
                Vec.insert (index + 1) splitNode pointers
        splitIfNeeded branchFactor node

    let private addEntry branchFactor node key value =
        match node with
            | Internal(_, _) -> failwith "Cannot add entry to internal node"
            | Leaf(keys, values, _) ->
                match getItemIndex key keys with
                    | Some(index) when keys.[index] = key -> // Update existing value
                        values.[index] <- value 
                    | Some(index)   ->                       // insert new value in middle
                        keys.Insert(index, key)
                        values.Insert(index, value)
                    | None ->                                // add new item to end
                        keys.Add(key)
                        values.Add(value)
        splitIfNeeded branchFactor node

    let insert tree (key, value) =
        let addChild' = addChild tree.BranchFactor
        let addEntry' = addEntry tree.BranchFactor
        let rec insert' node = 
            match node with
                | Internal(keys, pointers) ->
                    let index = getSubtreeIndex key keys
                    let subtree = pointers.[index]
                    match insert' subtree with
                        | None              -> None
                        | Some(splitNode)   -> addChild' node splitNode
                | Leaf(_) -> addEntry' node key value
        match insert' tree.Root with
            | None          -> ()
            | Some(newNode) ->
                tree.Root <- Internal(Vec.ofArray [| getFirstKey newNode |], Vec.ofArray [| tree.Root; newNode |])
            
                

