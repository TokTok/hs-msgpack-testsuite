{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE Trustworthy         #-}
module Test.MessagePack.Spec where

import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen         as Gen
import           Test.QuickCheck.Instances   ()

import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Lazy.Char8  as L8
import qualified Data.HashMap.Strict         as HashMap
import           Data.Int                    (Int16, Int32, Int64, Int8)
import qualified Data.IntMap                 as IntMap
import qualified Data.Map                    as Map
import qualified Data.Maybe                  as Maybe
import           Data.MessagePack.Arbitrary  ()
import qualified Data.Text.Lazy              as LT
import qualified Data.Vector                 as V
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Unboxed         as VU
import           Data.Word                   (Word16, Word32, Word64, Word8)
import           GHC.Generics                (Generic)

import           Data.MessagePack.Types
import qualified Test.MessagePack.BytePacker as BytePacker
import           Test.MessagePack.BytePacker (BytePacker)


data Unit = Unit
  deriving (Eq, Show, Generic)

instance MessagePack Unit


data TyConArgs = TyConArgs Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack TyConArgs


data Record = Record
  { recordField1 :: Int
  , recordField2 :: Double
  , recordField3 :: String
  }
  deriving (Eq, Show, Generic)

instance MessagePack Record


data Foo
  = Foo1
  | Foo2 Int
  | Foo3 Int
  | Foo4 Int
  | Foo5 Int
  | Foo6 { unFoo3 :: Int }
  | Foo7 Int
  | Foo8 Int Int
  | Foo9 Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack Foo

instance Arbitrary Foo where
  arbitrary = Gen.oneof
    [ return Foo1
    , Foo2 <$> arbitrary
    , Foo3 <$> arbitrary
    , Foo4 <$> arbitrary
    , Foo5 <$> arbitrary
    , Foo6 <$> arbitrary
    , Foo7 <$> arbitrary
    , Foo8 <$> arbitrary <*> arbitrary
    , Foo9 <$> arbitrary <*> arbitrary <*> arbitrary
    ]


type UnpackResult a = Either DecodeError a

checkMessage :: Show a => UnpackResult a -> Expectation
checkMessage (Right res) =
  expectationFailure $ "unexpected success: " ++ show res
checkMessage (Left msgs) =
  show msgs `shouldContain` "invalid encoding for "


spec :: BytePacker p => p -> Spec
spec p = do
  describe "unpack" $ do
    it "does not throw exceptions on arbitrary data" $
      property $ \bs ->
        case unpack bs of
          Just "" -> return () :: IO ()
          _       -> return () :: IO ()

    it "does not allocate huge vectors up front" $ do
      -- Creates the beginning of a deeply nested array of 4 billion arrays containing again 4
      -- billion arrays containing 4 billion arrays, etc. 100 levels deep. This test ensures that
      -- the array construction is done lazily and we don't actually allocate 400GB of memory.
      let maxArray = [0xdd,0xff,0xff,0xff,0xff]
      (unpackEither (L.pack . concat $ maxArray : replicate 100 maxArray) :: UnpackResult [[Int]])
          `shouldSatisfy` isLeft
      (unpackEither (L.pack $ [0xdf,0xdf,0xdf,0xdf,0xdf,0xdf]) :: UnpackResult [[Int]])
          `shouldSatisfy` isLeft

  describe "pack" $
    it "handles very large arrays (32 bit array length)" $
      (take 13 $ L.unpack $ pack ([1,2,3,4,5,6] ++ replicate 90000 7 :: [Int])) `shouldBe` [0xdd,0x00,0x01,0x5f,0x96,1,2,3,4,5,6,7,7]

  describe "Assoc" $ do
    it "supports read/show" $
      property $ \(a :: Assoc [(Int, Int)]) ->
        read (show a) `shouldBe` a

    it "inherits ordering from its contained type" $
      property $ \(a :: Assoc Int) b ->
        (unAssoc a < unAssoc b) `shouldBe` (a < b)

  describe "failures" $
    it "should contain the same start of the failure message for all types" $ do
      checkMessage (unpackEither (pack $ ObjectInt (-1)) :: UnpackResult Foo)
      checkMessage (unpackEither (pack [ObjectInt (-1), ObjectInt 0]) :: UnpackResult Foo)
      checkMessage (unpackEither (pack $ ObjectArray V.empty) :: UnpackResult TyConArgs)
      checkMessage (unpackEither (pack $ ObjectArray V.empty) :: UnpackResult Record)
      checkMessage (unpackEither (pack [0 :: Int, 1, 2, 3]) :: UnpackResult Record)
      checkMessage (unpackEither (pack "") :: UnpackResult Unit)
      checkMessage (unpackEither (pack "") :: UnpackResult TyConArgs)
      checkMessage (unpackEither (pack "") :: UnpackResult Record)
      checkMessage (unpackEither (pack "") :: UnpackResult ())
      checkMessage (unpackEither (pack ()) :: UnpackResult Int)
      checkMessage (unpackEither (pack ()) :: UnpackResult Bool)
      checkMessage (unpackEither (pack ()) :: UnpackResult Float)
      checkMessage (unpackEither (pack ()) :: UnpackResult Double)
      checkMessage (unpackEither (pack ()) :: UnpackResult S.ByteString)
      checkMessage (unpackEither (pack ()) :: UnpackResult LT.Text)
      checkMessage (unpackEither (pack "") :: UnpackResult [String])
      checkMessage (unpackEither (pack ()) :: UnpackResult (V.Vector Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (VS.Vector Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (VU.Vector Int))
      checkMessage (unpackEither (pack "") :: UnpackResult (Assoc [(Int, Int)]))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int, Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int, Int, Int, Int, Int))
      checkMessage (unpackEither (pack ()) :: UnpackResult (Int, Int, Int, Int, Int, Int, Int, Int, Int))

  describe "type coercion" $ do
    it "bool<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` (Nothing :: Maybe Bool)

    it "int<-bool" $
      property $ \(a :: Bool) -> coerce a `shouldBe` (Nothing :: Maybe Int)

    it "float<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Float)
    it "float<-double" $
      property $ \(a :: Double) -> coerce a `shouldBe` Just (realToFrac a :: Float)
    it "float<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Float)

    it "double<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Double)
    it "double<-float" $
      property $ \(a :: Float) -> coerce a `shouldBe` Just (realToFrac a :: Double)
    it "double<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Double)

    it "bin<-string" $
      property $ \(a :: S.ByteString) -> coerce a `shouldBe` (Nothing :: Maybe String)

    it "string<-bin" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe S.ByteString)

  describe "Identity Properties" $ do
    let sizes = [0xf, 0x10, 0x1f, 0x20, 0xff, 0x100, 0xffff, 0x10000]

    it "unit encoding" $
      Unit `shouldBe` mid Unit

    it "map encodings" $ do
      let rt n = let a = IntMap.fromList [(x, -x) | x <- [0..n]] in a `shouldBe` mid a
      mapM_ rt sizes

    it "list encodings" $ do
      let rt n = let a = replicate n "hello" in a `shouldBe` mid a
      mapM_ rt sizes

    it "vector encodings" $ do
      let rt n = let a = V.fromList [0..n] in a `shouldBe` mid a
      mapM_ rt sizes

    it "storable-vector encodings" $ do
      let rt n = let a = VS.fromList [0..n] in a `shouldBe` mid a
      mapM_ rt sizes

    it "unboxed-vector encodings" $ do
      let rt n = let a = VU.fromList [0..n] in a `shouldBe` mid a
      mapM_ rt sizes

    it "string encodings" $ do
      let rt n = let a = replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "bytestring encodings" $ do
      let rt n = let a = S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "ext encodings" $ do
      let rt n = let a = ObjectExt 0 $ S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt [0..20]
      mapM_ rt sizes

    it "int encodings" $ do
      (-0x7fffffffffffffff) `shouldBe` intMid (-0x7fffffffffffffff)
      (-0x80000000) `shouldBe` intMid (-0x80000000)
      (-0x7fffffff) `shouldBe` intMid (-0x7fffffff)
      (-0x8000) `shouldBe` intMid (-0x8000)
      (-0x7fff) `shouldBe` intMid (-0x7fff)
      (-1) `shouldBe` intMid (-1)
      0 `shouldBe` intMid 0
      1 `shouldBe` intMid 1
      0x7fff `shouldBe` intMid 0x7fff
      0x8000 `shouldBe` intMid 0x8000
      0x7fffffff `shouldBe` intMid 0x7fffffff
      0x80000000 `shouldBe` intMid 0x80000000
      0x7fffffffffffffff `shouldBe` intMid 0x7fffffffffffffff

    it "int"    $ property $ \(a :: Int   ) -> a `shouldBe` mid a
    it "int8"   $ property $ \(a :: Int8  ) -> a `shouldBe` mid a
    it "int16"  $ property $ \(a :: Int16 ) -> a `shouldBe` mid a
    it "int32"  $ property $ \(a :: Int32 ) -> a `shouldBe` mid a
    it "int64"  $ property $ \(a :: Int64 ) -> a `shouldBe` mid a
    it "word"   $ property $ \(a :: Word  ) -> a `shouldBe` mid a
    it "word8"  $ property $ \(a :: Word8 ) -> a `shouldBe` mid a
    it "word16" $ property $ \(a :: Word16) -> a `shouldBe` mid a
    it "word32" $ property $ \(a :: Word32) -> a `shouldBe` mid a
    it "word64" $ property $ \(a :: Word64) -> a `shouldBe` mid a

    it "ext" $
      property $ \(n, a) -> ObjectExt n a `shouldBe` mid (ObjectExt n a)
    it "nil" $
      property $ \(a :: ()) -> a `shouldBe` mid a
    it "bool" $
      property $ \(a :: Bool) -> a `shouldBe` mid a
    it "float" $
      property $ \(a :: Float) -> a `shouldBe` mid a
    it "double" $
      property $ \(a :: Double) -> a `shouldBe` mid a
    it "string" $
      property $ \(a :: String) -> a `shouldBe` mid a
    it "bytestring" $
      property $ \(a :: S.ByteString) -> a `shouldBe` mid a
    it "lazy-bytestring" $
      property $ \(a :: L8.ByteString) -> a `shouldBe` mid a
    it "lazy-text" $
      property $ \(a :: LT.Text) -> a `shouldBe` mid a
    it "[int]" $
      property $ \(a :: [Int]) -> a `shouldBe` mid a
    it "vector int" $
      property $ \(a :: V.Vector Int) -> a `shouldBe` mid a
    it "storable-vector int" $
      property $ \(a :: VS.Vector Int) -> a `shouldBe` mid a
    it "unboxed-vector int" $
      property $ \(a :: VU.Vector Int) -> a `shouldBe` mid a
    it "[string]" $
      property $ \(a :: [String]) -> a `shouldBe` mid a
    it "(int, int)" $
      property $ \(a :: (Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int)" $
      property $ \(a :: (Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "[(int, double)]" $
      property $ \(a :: [(Int, Double)]) -> a `shouldBe` mid a
    it "[(string, string)]" $
      property $ \(a :: [(String, String)]) -> a `shouldBe` mid a
    it "Assoc [(string, int)]" $
      property $ \(a :: Assoc [(String, Int)]) -> a `shouldBe` mid a
    it "Map String Int" $
      property $ \(a :: Map.Map String Int) -> a `shouldBe` mid a
    it "IntMap Int" $
      property $ \(a :: IntMap.IntMap Int) -> a `shouldBe` mid a
    it "HashMap String Int" $
      property $ \(a :: HashMap.HashMap String Int) -> a `shouldBe` mid a

    it "generics" $
      property $ \(a :: Foo) -> a `shouldBe` mid a
    it "arbitrary message" $
      property $ \(a :: Object) -> a `shouldBe` mid a

  describe "encoding validation" $ do
    it "word64 2^64-1" $
      pack (0xffffffffffffffff :: Word64) `shouldBe` L.pack [0xCF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]

    it "decodes empty array as ()" $
      unpack (pack ([] :: [Int])) `shouldBe` Just ()

  where
    mid :: MessagePack a => a -> a
    mid = Maybe.fromJust . unpack . pack

    intMid :: Int64 -> Int64
    intMid = mid

    unpackEither :: MessagePack a => L8.ByteString -> Either DecodeError a
    unpackEither = BytePacker.unpackEither p

    isLeft Left{}  = True
    isLeft Right{} = False

    pack :: MessagePack a => a -> L8.ByteString
    pack = BytePacker.pack p

    unpack :: (MonadFail m, MessagePack a) => L8.ByteString -> m a
    unpack = BytePacker.unpack p

    coerce :: (MessagePack a, MessagePack b) => a -> Maybe b
    coerce = unpack . pack
