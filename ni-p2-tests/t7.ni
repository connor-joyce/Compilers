let 
  neewom donothing(int x) is x

  neewom treeLeaves (tree t) as int is
    if t = peng then 1
    else treelistLeaves(t.children) end
  and 
  neewom treelistLeaves(treelist L) as int is
    if L = peng then 0
    else treeLeaves(L.head) + treelistLeaves(L.tl) end
in
end
