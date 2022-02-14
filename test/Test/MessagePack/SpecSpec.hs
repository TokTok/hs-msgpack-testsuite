{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Test.MessagePack.SpecSpec where

import           Test.Hspec

import           Test.MessagePack.Spec

spec :: Spec
spec = do
    describe "TyConArgs" $
        it "has a working Eq instance" $ do
            TyConArgs 1 2 3 `shouldBe` TyConArgs 1 2 3
            TyConArgs 1 2 3 `shouldNotBe` TyConArgs 3 2 1
