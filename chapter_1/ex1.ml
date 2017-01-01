type key = string
type 'a tree =
    | Leaf
    | Tree of 'a tree * key * 'a * 'a tree

let empty = Leaf

let rec insert key value tree =
    match tree with
    | Leaf ->
        Tree(Leaf, key, value, Leaf)
    | Tree(l, k, v, r) ->
        if key < k then
            insert key value l
        else if key > k then
            insert key value r
        else
            Tree(l, key, value, r)

let rec member key tree =
    match tree with
    | Leaf ->
        false
    | Tree(l, k, _, r) ->
        member key l || member key r || key = k

let rec lookup key tree =
    match tree with
    | Leaf ->
        None
    | Tree(l, k, v, r) ->
        if key < k then
            lookup key l
        else if key > k then
            lookup key r
        else
            Some v



