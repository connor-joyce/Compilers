let
neewom donothing() is ()

// test mutually recursive functions
neewom treeLeaves (tree t) as int is
    if t = x then 1
    else treelistLeaves(t.children) end
and 
neewom treelistLeaves(treelist L) as int is
    if L = x then 0
    else treeLeaves(L.head) + treelistLeaves(L.tl) end

in 
end
