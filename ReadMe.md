Linux joystick events via a Kafka message broker
================================================

This package contains functions for passing Linux device events from a [SpaceNavigator](http://www.3dconnexion.com/products/spacemouse/spacenavigator.html) to topics on a [Kafka message broker](https://kafka.apache.org/).


Clients
-------

The simple Kafka client that produces events from the joystick can be run, for example, as one of the following:

	cabal run kafka-device-spacenav -- /dev/input/spacenavigator spacenav-client localhost 9092 events spacenav
	cabal run kafka-device-spacenav -- sample.yaml

Also see https://hackage.haskell.org/package/kafka-device/.
