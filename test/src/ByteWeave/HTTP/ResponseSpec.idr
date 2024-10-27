module ByteWeave.HTTP.ResponseSpec

import ByteWeave.HTTP.HTTPVersion
import ByteWeave.HTTP.Response
import ByteWeave.HTTP.Spec.Core
import ByteWeave.HTTP.Spec.SpecTree
import ByteWeave.HTTP.Spec.Expectations

export
specs : SpecTree
specs = do
  describe "encodeResponse" $ do
    it "returns correct response string" $ do
      let response = MkResponse Ok (1 ** 1 ** HTTP11) [("Content-Length", "4")] ("test")
      let encoded = encodeResponse response
      let expected = "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest"
      encoded `mustEqual` expected
