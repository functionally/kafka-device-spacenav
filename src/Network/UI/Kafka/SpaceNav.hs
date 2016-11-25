{-|
Module      :  Network.UI.Kafka.SpaceNav
Copyright   :  (c) 2016 Brian W Bush
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


import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 (hGet)
import Data.Maybe (catMaybes)
import Network.Kafka (KafkaAddress, KafkaClientId)
import Network.Kafka.Protocol (TopicName)
import Network.UI.Kafka (ExitAction, LoopAction, Sensor, producerLoop)
import Network.UI.Kafka.Types (Button(..), Event(..), Toggle(..))
import System.Hardware.Linux.SpaceNav (SpaceNav(..), interpretSpaceNav, maxValue)
import System.IO (IOMode(ReadMode), hClose, openFile)


-- | Produce events for a Kafka topic from a Linux SpaceNav.
spacenavLoop :: FilePath                    -- ^ The path to the spacenav device, e.g. "\/dev\/input\/spacenav0".
             -> KafkaClientId               -- ^ A Kafka client identifier for the producer.
             -> KafkaAddress                -- ^ The address of the Kafka broker.
             -> TopicName                   -- ^ The Kafka topic name.
             -> Sensor                      -- ^ The name of the sensor producing events.
             -> IO (ExitAction, LoopAction) -- ^ Action to create the exit and loop actions.
spacenavLoop path client address topic sensor =
  do
    spacenav <- openFile path ReadMode
    (exit, loop) <-
      producerLoop client address topic sensor
        $ translate
        . interpretSpaceNav
        <$> hGet spacenav 24
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
translate SpaceNav{..} =
  catMaybes
    [
      do
        guard button
        return
          $ ButtonEvent (IndexButton number, if value == 0 then Up else Down)
    , do
        guard axis
        return
          $ AnalogEvent number (fromIntegral value / fromIntegral maxValue)
    ]