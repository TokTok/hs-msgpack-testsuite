{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
module Test.MessagePack.BytePacker
    ( BytePacker (..)
    , unpackEither, unpack
    ) where

import           Control.Monad.Validate             (Validate, runValidate)
import qualified Data.ByteString.Lazy               as LBS
import           Data.MessagePack.Types.Assoc       (Assoc (..))
import           Data.MessagePack.Types.Class       (Config, MessagePack (..),
                                                     defaultConfig)
import           Data.MessagePack.Types.DecodeError (DecodeError, decodeError,
                                                     errorMessages)
import           Data.MessagePack.Types.Generic     ()
import           Data.MessagePack.Types.Object      (Object (..))


class BytePacker p where
    -- | Pack a Haskell value to MessagePack binary.
    pack :: MessagePack a => p -> a -> LBS.ByteString
    -- | Unpack MessagePack binary to a Haskell value.
    --
    -- On failure, returns a list of error messages.
    unpackValidate :: MessagePack a => p -> LBS.ByteString -> Validate DecodeError a


unpackEither :: (BytePacker p, MessagePack a) => p -> LBS.ByteString -> Either DecodeError a
unpackEither p = runValidate . unpackValidate p


-- | Unpack MessagePack binary to a Haskell value. If it fails, it fails in the
-- Monad. In the Maybe monad, failure returns Nothing.
#if (MIN_VERSION_base(4,13,0))
unpack :: (BytePacker p, Monad m, MonadFail m, MessagePack a)
#else
unpack :: (BytePacker p, Monad m, MessagePack a)
#endif
       => p -> LBS.ByteString -> m a
unpack p = eitherToM . unpackEither p
  where
    eitherToM (Left msgs) = fail $ show msgs
    eitherToM (Right res) = return res
