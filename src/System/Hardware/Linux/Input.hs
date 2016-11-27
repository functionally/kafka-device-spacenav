{-|
Module      :  System.Hardware.Linux.Input
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Parse events from Linux /dev/input streams.  See \<<https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h>\> for details.
-}


{-# LANGUAGE RecordWildCards #-}


module System.Hardware.Linux.Input (
-- * Types
  InputEvent(..)
, byteLength
) where


import Data.Binary (Binary(..), encode)
import Data.Binary.Get (getWordhost, getWord16host, getWord32host)
import Data.Binary.Put (putWordhost, putWord16host, putWord32host)
import Data.Word (Word16, Word32)

import qualified Data.ByteString.Lazy as BS (length)


-- | Input events from Linux /dev/input streams.  See \<<https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h>\> for details.
data InputEvent =
  InputEvent
  {
    timeval :: (Word, Word) -- ^ The time stamp in seconds and microseconds.
  , typ     :: Word16       -- ^ The event type.
  , code    :: Word16       -- ^ The event code.
  , value   :: Word32       -- ^ The event value.
  }
  deriving (Eq, Ord, Read, Show)

instance Binary InputEvent where
  get =
    do
      timeval <- (,) <$> getWordhost <*> getWordhost
      typ     <- getWord16host
      code    <- getWord16host
      value   <- getWord32host
      return InputEvent{..}
  put InputEvent{..} =
    do
      putWordhost $ fst timeval
      putWordhost $ snd timeval
      putWord16host typ
      putWord16host code
      putWord32host value


-- | The number of bytes in an input event.
byteLength :: Integral a => a
byteLength =
  fromIntegral
    . BS.length
    . encode
    $ InputEvent (0, 0) 0 0 0
