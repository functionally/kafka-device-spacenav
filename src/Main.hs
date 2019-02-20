{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Production
Portability :  Linux

Produce events for a Kafka topic from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\> on Linux.
-}


module Main (
-- * Main entry
  main
) where


import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Network.UI.Kafka (TopicConnection(TopicConnection))
import System.Environment (getArgs)

import qualified Network.UI.Kafka.SpaceNav as Raw (spacenavLoop)
import qualified Network.UI.Kafka.SpaceNav.Interpretation as Interpreted (spacenavLoop)


-- The main action.
main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [device, client, host, port, topic, sensor] ->
        do
          putStrLn $ "Device:        " ++ device
          putStrLn $ "Kafka client:  " ++ client
          putStrLn $ "Kafka address: (" ++ host ++ "," ++ port ++ ")"
          putStrLn $ "Kafka topic:   " ++ topic
          putStrLn $ "Sensor name:   " ++ sensor
          (_, loop) <-
            Raw.spacenavLoop
              device
              (TopicConnection client (host, read port) topic)
              sensor
          result <- loop
          either print return result
      [_] ->
        do
          interpretation <- loadYamlSettingsArgs [] useEnv
          (_, loop) <- Interpreted.spacenavLoop interpretation
          either print return =<< loop
      _ ->
        do
          putStrLn "USAGE: kafka-device-spacenav device client host port topic sensor"
          putStrLn "or"
          putStrLn "USAGE: kafka-device-spacenav interpretation.yaml"
