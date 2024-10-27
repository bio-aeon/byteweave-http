module ByteWeave.HTTP.Spec.SpecTree

import ByteWeave.HTTP.Spec.SpecResult

public export
data SpecLabel : Type where
     Describe : (msg : String) -> SpecLabel
     It : (msg : String) -> SpecLabel

public export
data Tree : Type -> Type where
     Leaf : (elem : a) -> Tree a
     Node : (left : Tree a) -> (right : Tree a) -> Tree a

public export
SpecTree : Type
SpecTree = Tree (Either SpecLabel (IO SpecResult))

namespace SpecTreeDo
  (>>=) : SpecTree -> (() -> SpecTree) -> SpecTree
  (>>=) leftTree f = let rightTree = f () in
                         Node leftTree rightTree

  export
  (>>) : SpecTree -> SpecTree -> SpecTree
  (>>) leftTree rightTree = leftTree >>= \_ => rightTree
