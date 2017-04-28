module Tree


data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem
%name BSTree tree, tree1


insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node (Empty) x (Empty)
insert x tree@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => tree
    GT => Node left val (insert x right)


listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right 
