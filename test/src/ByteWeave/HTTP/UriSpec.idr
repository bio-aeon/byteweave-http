module ByteWeave.HTTP.UriSpec

import Data.String.Parser

import ByteWeave.HTTP.Uri
import ByteWeave.HTTP.Spec.Core
import ByteWeave.HTTP.Spec.SpecTree
import ByteWeave.HTTP.Spec.Expectations

export
specs : SpecTree
specs = do
  describe "uriParser" $ do
    it "parses url string correctly" $ do
      let parsed = fst <$> (parse uriParser "/path?a=b&c=d")
      let expected = Right (MkUri "/path" [("a", "b"), ("c", "d")])
      parsed `mustEqual` expected
