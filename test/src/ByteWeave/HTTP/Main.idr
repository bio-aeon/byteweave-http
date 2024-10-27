module ByteWeave.HTTP.Main

import ByteWeave.HTTP.Spec.Core
import ByteWeave.HTTP.Spec.Runner
import ByteWeave.HTTP.Spec.SpecTree
import ByteWeave.HTTP.ResponseSpec
import ByteWeave.HTTP.UriSpec

main : IO ()
main = spec $ do
  describe "ByteWeave.HTTP.ResponseSpec" ByteWeave.HTTP.ResponseSpec.specs
  describe "ByteWeave.HTTP.UriSpec" ByteWeave.HTTP.UriSpec.specs
