module ByteWeave.HTTP.Spec.Expectations

import ByteWeave.HTTP.Spec.SpecResult

export
mustEqual : (Eq a, Show a) => (actual : a) -> (expected : a) -> SpecResult
mustEqual actual expected = if actual == expected then
                              Success
                            else
                              BinaryFailure actual expected "not equal"
