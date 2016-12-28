type Key = string
type Tree =
    | Leaf
    | Tree of Tree * Key * Tree

let empty = Leaf

let rec insert tree key =
    match tree with
    | Leaf ->
        Tree(Leaf, key, Leaf)
    | Tree(l, k, r) ->
        if key < k
            then Tree((insert l key), k, r)
        else if key > k
            then Tree(l, k, (insert r key))
        else
            Tree(l, key, r)

let rec isMember targetKey tree =
    match tree with
    | Leaf -> false
    | Tree(left, key, right) ->
        isMember key left || key = targetKey || isMember key right

let insertions = ["t"; "s"; "p"; "i"; "p"; "f"; "b"; "s"; "t"]

let finalTree =
    insertions
    |> List.fold insert empty

let rec walkTree tree level =
    match tree with
    | Leaf -> ()
    | Tree(l, k, r) ->
        printf "%i:%s " level k
        walkTree l (level + 1)
        walkTree r (level + 1)

walkTree finalTree 0


