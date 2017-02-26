{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Produce interpreted events for a Kafka topic from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\> on Linux.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.SpaceNav.Interpretation (
-- * Event handling
  spacenavLoop
) where


import Data.ByteString.Lazy.Char8 (hGet)
import Network.UI.Kafka (ExitAction, LoopAction)
import Network.UI.Kafka.Interpretation (Interpretation(..), interpretationLoop)
import System.Hardware.Linux.SpaceNav (SpaceNav(..), byteLength, interpretSpaceNav)
import System.IO (IOMode(ReadMode), hClose, openFile)


-- | Interpret events from a Linux SpaceNav.
spacenavLoop :: Interpretation FilePath Double -- ^ Instructions for interpretation.
             -> IO (ExitAction, LoopAction)    -- ^ Action to create the exit and loop actions.
spacenavLoop interpretation@TrackInterpretation{..} =
  do
    let
      analogHandler SpaceNavAnalog{..} = Just (number, setting)
      analogHandler _                  = Nothing
      buttonHandler SpaceNavButton{..} = Just (number, pressed)
      buttonHandler _                  = Nothing
    spacenav <- openFile device ReadMode
    (exit, loop) <-
      interpretationLoop analogHandler buttonHandler interpretation
        $ interpretSpaceNav <$> hGet spacenav byteLength
    return
      (
        do
          exit
          hClose spacenav
      , loop
      )
