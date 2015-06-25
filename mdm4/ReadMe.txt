This is version 0.4 of the Meter Data Management case study.

This version replaces ETS with DETS as the data storage mechanism.

It will process up to about 50,000 meters (simulator_load_test.script)
in a 2 GB machine.

Above that point, the load test will tend to fail due to memory allocation problems.

The processing rate is about 4-5 seconds per thousand meters or 200-250 meters/second.