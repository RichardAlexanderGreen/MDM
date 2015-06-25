mdm6/ReadMe.txt
===============
This is version 0.6 of MDM application architecture.
It differs from mdm4 in that it uses mnesia instead of dets as its storage mechanism.
It differs from mdm5 in that it makes *direct calls* between layers 
  instead of using gen_server:call/3 or gen_server:cast/2.

