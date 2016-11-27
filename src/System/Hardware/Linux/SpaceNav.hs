{-|
Module      :  System.Hardware.Linux.SpaceNav
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Interpret events from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module System.Hardware.Linux.SpaceNav (
-- * Types and sizes
  SpaceNav(..)
, byteLength
-- * Event handling
, interpretSpaceNav
, readSpaceNav
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary(..), decode)
import Data.Bits ((.&.), complement, shift)
import Data.ByteString.Lazy.Char8 as BS (ByteString, readFile, splitAt)
import Data.Serialize (Serialize)
import Data.Word (Word32)
import GHC.Generics (Generic)
import System.Hardware.Linux.Input (InputEvent(..), byteLength)


-- | SpaceNavigator data.
data SpaceNav =
    SpaceNavButton
    {
      timestamp :: Integer -- ^ The event timestamp, in POSIX picoseconds.
    , number    :: Int     -- ^ The button number.
    , pressed   :: Bool    -- ^ Whether the button is depressed.
    }
  | SpaceNavAnalog
    {
      timestamp :: Integer -- ^ The event timestamp, in POSIX picoseconds.
    , number    :: Int     -- ^ The axis number.
    , setting   :: Double  -- ^ The data value.
    }
  | SpaceNavNull
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON SpaceNav

instance ToJSON SpaceNav

instance Binary SpaceNav

instance Serialize SpaceNav


-- | Interpret SpaceNavigator event bytes on a Linux input deviceThis interpretation is based on \<<https://github.com/vrpn/vrpn/blob/master/vrpn_3DConnexion.h>\> and \<<https://github.com/vrpn/vrpn/blob/master/vrpn_3DConnexion.c>\>.
interpretSpaceNav :: ByteString -- ^ The bytes from /dev/input.
                  -> SpaceNav   -- ^ The corresponding SpaceNavigator data.
interpretSpaceNav x =
  let
    InputEvent{..} = decode x
    (seconds, microseconds) = timeval
    seconds' = fromIntegral seconds :: Integer
    microseconds' = fromIntegral microseconds :: Integer
    timestamp = (10^(6 :: Int) * seconds' + microseconds') * 10^(6 :: Int)
  in
    case typ of
      0x01 -> let
               number = fromIntegral $ code .&. 0x00ff
               pressed = value /= 0
             in
               SpaceNavButton{..}
      0x02 -> let
                number = fromIntegral code
                setting = fromIntegral (twosComplement value) / 400
              in
                SpaceNavAnalog{..}
      _    -> SpaceNavNull


-- | Decode a two's complement.
twosComplement :: Word32 -- ^ The two's complement.
               -> Int    -- ^ The corresponding integer.
twosComplement x =
  fromEnum (x' .&. complement mask) - fromEnum (x' .&. mask)
    where
      x' = fromEnum x :: Int
      mask = 1 `shift` 31


-- | Read a stream of SpaceNavigator data.
readSpaceNav :: FilePath      -- ^ The SpaceNavigator device, e.g., "\/dev\/input\/spacenav0".
             -> IO [SpaceNav] -- ^ Action to read the SpaceNavigator data.
readSpaceNav path =
  let
    chunks :: ByteString -> [ByteString]
    chunks x =
      let
        (y, ys) = BS.splitAt 8 x
      in
        y : chunks ys
  in
    map interpretSpaceNav
      . chunks 
      <$> BS.readFile path
