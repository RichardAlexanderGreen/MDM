This is version 0.3 of the Meter Data Management.

It uses ETS for the data mechanism.
ets:tab2file is used to persist the store between sesions.

Throughput is about 3-5 seconds per thousand meters.

The process will fail due to memory allocation problems somewhere above 50,000 meters
(simulator_load_test.script).