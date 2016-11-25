{-|
Module      :  System.Hardware.Linux.SpaceNav
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Interpret events from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>.

The interpretation of SpaceNavigator device output in this module is based on Jan Ciger's public-domain work \<<http://janoc.rd-h.com/files/software/linux/spacenav/spacenavig.c>\>, as discussed in \<<http://janoc.rd-h.com/archives/74>\>.
-}


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module System.Hardware.Linux.SpaceNav (
-- * Types and values
  SpaceNav(..)
, minValue
, maxValue
-- * Event handling
, interpretSpaceNav
, readSpaceNav
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Serialize (Serialize)
import Data.ByteString.Lazy.Char8 as BS (ByteString, length, readFile, splitAt, unpack)
import Data.Bits ((.&.), (.|.), complement, shift)
import Debug.Trace (trace)
import GHC.Generics (Generic)


-- | The minimum for 'value'.
minValue :: Int
minValue = - maxValue


-- | The maximum for 'value'.
maxValue :: Int
maxValue = 32767


-- | SpaceNavigator data.
data SpaceNav =
  SpaceNav
  {
    timestamp :: Int  -- ^ The event timestamp in milliseconds.
  , value     :: Int  -- ^ The data value.
  , number    :: Int  -- ^ The button or axis number.
  , button    :: Bool -- ^ Whether the button is being reported.
  , axis      :: Bool -- ^ Whether the axis is being reported.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON SpaceNav

instance ToJSON SpaceNav

instance Binary SpaceNav

instance Serialize SpaceNav


-- | Interpret SpaceNavigator event bytes on a Linux input device, specified in \<<https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h>\>.  This interpretation is based on Jan Ciger's public-domain work \<<http://janoc.rd-h.com/files/software/linux/spacenav/spacenavig.c>\>, as discussed in \<<http://janoc.rd-h.com/archives/74>\>.
interpretSpaceNav :: ByteString -- ^ The twenty-four bytes.
                  -> SpaceNav   -- ^ The corresponding SpaceNavigator data.
interpretSpaceNav x
  | BS.length x /= 24 = error "System.Hardware.Linux.SpaceNav.interpretSpaceNav: twenty-four bytes required."
  | otherwise         = let
                         [x0, x1, x2, x3, x4, x5, x6, x7] = fromEnum <$> drop 16 (unpack x)
                         timestamp = 0     -- FIXME
                         value     = 0     -- FIXME
                         typ       = 0     -- FIXME
                         number    = 0     -- FIXME
                         button    = False -- FIXME
                         axis      = False -- FIXME
                       in
                         trace (show ([x0, x1], [x2, x3], [x4, x5, x6, x7])) SpaceNav{..}


-- | Decode a two's complement.
twosComplement :: Int -- ^ The two's complement.
               -> Int -- ^ THe corresponding integer.
twosComplement x =
  fromEnum (x .&. complement mask) - fromEnum (x .&. mask)
    where
      mask = 0x8000


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
