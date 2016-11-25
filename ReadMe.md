Linux joystick events via a Kafka message broker
================================================

This package contains functions for passing Linux device events from a [SpaceNavigator](http://www.3dconnexion.com/products/spacemouse/spacenavigator.html) to topics on a [Kafka message broker](https://kafka.apache.org/).


The interpretation of SpaceNavigator device output in this package is based on [Jan Ciger's public-domain work](http://janoc.rd-h.com/files/software/linux/spacenav/spacenavig.c), as discussed in http://janoc.rd-h.com/archives/74.

Clients
-------

The simple Kafka client that produces events from the joystick can be run, for example, as follows:

	cabal run kafka-device-spacenav -- /dev/input/spacenav0 spacenav-client localhost 9092 events spacenav

Also see https://hackage.haskell.org/package/kafka-device/.
