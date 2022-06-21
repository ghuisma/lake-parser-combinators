module V4.Spec (spec) where

import Test.Hspec

-- example test using HSpec & QuickCheck
spec :: Spec
spec = do
    describe "V4" $ do
        describe "Parser" $ do
            describe "Prim" $ do
                it "item" $ do
                    pending
                it "notFollowedBy" $ do
                    pending
                it "eof" $ do
                    pending
            describe "Lake" $ do
                it "emptyLake" $ do
                    pending
