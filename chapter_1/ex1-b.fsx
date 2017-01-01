type Key = string
type Tree<'T> =
    | Leaf
    | Tree of Tree<'T> * Key * 'T * Tree<'T>

let empt = Leaf

let rec insert tree key value =
    match tree with
    | Leaf ->
        Tree(Leaf, key, value, Leaf)
    | Tree(l, k, v, r) ->
        if key < k
            then Tree((insert l key value), k, v, r)
        else if key > k
            then Tree(l, k, v, (insert r key value))
        else
            Tree(l, key, value, r)

let rec lookup targetKey tree =
    match tree with
    | Leaf -> None
    | Tree(l, k, v, r) ->
        if targetKey < k then
            lookup targetKey l
        else if targetKey > k then
            lookup targetKey r
        else
            Some v




