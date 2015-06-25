mdm2/src/ReadMe.txt
-------------------
This is the second version of the MDM design.
It is a ground-up redesign.

I have redesigned the system because the old design was becoming too transaction driven.
In order to learn Erlang and take advantage of Erlang's strengths
I want a system that is very event driven.

Accordingly, I have changed the design (architecture).
The view is guided by a test-driven-design.
The tests are defined by the simulators: sim_install.erl and sim_demand.erl.
For details, the reader should study the comments and tests in those modules.

The architecture assumes that the messenger application is running. 
The messenger provides the "service bus" for the system.

The architecture also assumes that the quad application is running.
The quad application provides data storage services.