module ByteWeave.HTTP.Spec.Core

import ByteWeave.HTTP.Spec.SpecTree
import ByteWeave.HTTP.Spec.SpecResult

export
describe: (description : String) -> SpecTree -> SpecTree
describe descr tree = Node (Leaf $ Left $ Describe descr) tree

export
it : (description : String) -> SpecResult -> SpecTree
it descr spec = Node (Leaf $ Left $ It $ descr)
                     (Leaf $ Right $ pure spec)
