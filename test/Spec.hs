import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import V4.Spec

-- example test using HSpec & QuickCheck
main :: IO ()
main = hspec $ do
  -- V1Spec.spec
  -- V2Spec.spec
  -- V3Spec.spec
  V4.Spec.spec
  
