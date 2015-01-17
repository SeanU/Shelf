namespace Shelf

open System.Linq

type vec<'a> = ResizeArray<'a>

module Vec =
    let ofArray (arr : 'a array) = new vec<'a>(arr)

    let tryFindIndex pred (v : vec<'a>) =
        match v.FindIndex(new System.Predicate<'a>(pred)) with
            | -1    -> None
            |  x    -> Some(x)

    let insert index value (v : vec<'a>) = v.Insert(index, value)

    let split index (v : vec<'a>) =
        let newVec = new vec<'a>(v.Skip(index))
        v.RemoveRange(index, v.Count - index)
        newVec

    let removeLast (v : vec<'a>) = v.RemoveAt(v.Count - 1)