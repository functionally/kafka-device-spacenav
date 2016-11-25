{-|
Module      :  Main
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Linux

Produce events for a Kafka topic from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\> on Linux.
-}


module Main (
-- * Main entry
  main
) where


import Data.String (IsString(fromString))
import Network.UI.Kafka.SpaceNav (spacenavLoop)
import System.Environment (getArgs)


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
            spacenavLoop
              device
              (fromString client)
              (fromString host, toEnum $ read port)
              (fromString topic)
              sensor
          result <- loop
          either print return result
      _ -> putStrLn "USAGE: kafka-device-spacenav device client host port topic sensor"
