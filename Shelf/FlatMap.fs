namespace Shelf

open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.InteropServices

type kvp<'k, 'v> = KeyValuePair<'k, 'v>

type FlatMap<'k, 'v> when 'k : comparison and 'v : equality () =
    let contents = new ResizeArray<kvp<'k, 'v>>()

    let matchKey k (KeyValue(key, _)) = k = key

    let tryGetEntry key = Seq.tryFind (matchKey key) contents

    member d.TryGetItem key =
        match tryGetEntry key with
            | Some(entry)   -> Some(entry.Value)
            | None          -> None

    member d.TryFind pred = Seq.tryFind pred contents

//    let set key value =
//        let index = contents.FindIndex(fun e -> e.Key >= key)
//        if contents.[index].Key = key then
//            contents.[index].Value <- value
//        else
//            contents.Insert(index, { Key = key; Value = value })
//
//    let getEntry key = (contents :> IEnumerable<Entry<'k, 'v>>).Single(fun e -> e.Key = key)
//    let containsKey key = contents.Exists(fun e -> e.Key = key)
//    let keyvalue {Key = key; Value = value} = new KeyValuePair<'k, 'v>(key, value)
//
//    interface IDictionary<'k, 'v> with
//        member d.Add(key, value) = set key value
//        member d.Add(KeyValue(key, value)) = set key value
//        member d.ContainsKey(key) = containsKey key
//        member d.get_Count() = contents.Count
//        member d.get_Item(key) = (getEntry key).Value
//        member d.set_Item(key, value) = set key value
//        member d.get_Keys() = new ResizeArray<'k>(contents.Select(fun e-> e.Key)) :> ICollection<'k>
//        member d.get_Values() = new ResizeArray<'v>(contents.Select(fun e-> e.Value)) :> ICollection<'v>
//        member d.Remove(key) = contents.Remove(getEntry key)
//        member d.Remove(KeyValue(key, value)) = contents.Remove({Key = key; Value = value})
//        member d.TryGetValue(key, [<Out>] value) = 
//            let ok = containsKey key
//            if ok then value <- (getEntry key).Value
//            ok
//        member d.get_IsReadOnly() = false
//        member d.Clear() = contents.Clear()
//        member d.Contains(KeyValue(key, value)) = containsKey key && (getEntry key).Value.Equals(value) 
//        member d.CopyTo(arr, index) = 
//            let coll = new ResizeArray<KeyValuePair<'k, 'v>>(Seq.map keyvalue contents) :> ICollection<KeyValuePair<'k, 'v>>
//            coll.CopyTo(arr, index)
//        member d.GetEnumerator() = (Seq.map keyvalue contents).GetEnumerator()
//        member d.IEnumerable.GetEnumerator() = GetEnumerator()
                    
                    
