type Key = string
type Tree =
    | Leaf
    | Tree of Tree * Key * Tree

let empty = Leaf

let rec insert key tree =
    match tree with
    | Leaf ->
        Tree(Leaf, key, Leaf)
    | Tree(l, k, r) ->
        if key < k
            then Tree((insert key l), k, r)
        else if key > k
            then Tree(l, k, (insert key r))
        else
            Tree(l, key, r)

let rec isMember targetKey tree =
    match tree with
    | Leaf -> false
    | Tree(left, key, right) ->
        isMember key left || key = targetKey || isMember key right
