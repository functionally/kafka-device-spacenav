{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Produce events for a Kafka topic from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\> on Linux.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.SpaceNav (
-- * Event handling
  spacenavLoop
) where


import Data.ByteString.Lazy.Char8 (hGet)
import Network.UI.Kafka (ExitAction, LoopAction, TopicConnection, Sensor, producerLoop)
import Network.UI.Kafka.Types (Button(..), Event(..), Toggle(..))
import System.Hardware.Linux.SpaceNav (SpaceNav(..), byteLength, interpretSpaceNav)
import System.IO (IOMode(ReadMode), hClose, openFile)


-- | Produce events for a Kafka topic from a Linux SpaceNav.
spacenavLoop :: FilePath                    -- ^ The path to the spacenav device, e.g. "\/dev\/input\/spacenav0".
             -> TopicConnection             -- ^ The Kafka topic name and connection information.
             -> Sensor                      -- ^ The name of the sensor producing events.
             -> IO (ExitAction, LoopAction) -- ^ Action to create the exit and loop actions.
spacenavLoop path topicConnection sensor =
  do
    spacenav <- openFile path ReadMode
    (exit, loop) <-
      producerLoop topicConnection sensor
        $ translate
        . interpretSpaceNav
        <$> hGet spacenav byteLength
    return
      (
        do
          exit
          hClose spacenav
      , loop
      )


-- | Translate a SpaceNavigator event on Linux into events for Kafka.
translate :: SpaceNav -- ^ The SpaceNavigator event.
          -> [Event]  -- ^ The corresponding events for Kafka.
translate SpaceNavButton{..} = [ButtonEvent (IndexButton number, if pressed then Down else Up)]
translate SpaceNavAnalog{..} = [AnalogEvent number setting]
translate SpaceNavNull       = []
