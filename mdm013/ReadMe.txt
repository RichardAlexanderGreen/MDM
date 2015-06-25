mdm013/ReadMe.txt

This version of the Meter Data Management case study
represents objects as records rather than processes.
It is an attempt to maximize throughput
rather than an attempt to emulate a realistic architecture.

Records are stored in ETS and then written to disk using table2file.

To reduce the memory required to hold load history, 
usage records are simply written to disk and then post-processed later.

The benchmark (see "test_scripts/load_test_result.txt")
simulates 24 "ticks" where a collection of one million meters
send one hour's usage to a collector.

The collector updated the meter's register value 
and writes the usage increment (interval value) to disk.
Then after the 24 hours are collected, 
the usage file is scanned to price the usage
according to a market-price (time-of-day) pricing scheme.