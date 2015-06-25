 Copyright 2011 Richard Alexander Green
 Description: This version makes extensive use of the gen_event design patterns.
 Actors (Roles and Responsibilities):
 - Installer:
 - - Generate installation events for service / transformer / circuit.
 - Clock:
 - - Generate tick event.
 - Service:
 - - Handle tick: Send service_usage to transformer.
 - - Insert into service (PoD) usage history.
 - Transformer:
 - - Handle service_usage: Add into transformer (PoD) load history. Check for over-load.
 - - Handle tick: Send usage-sum resulting from prior period slot to the circuit.
 - Circuit:
 - - Handle usage-sum: Add into circuit (PoD) load history. Check for overload.
 - - Handle tick: Send usage-sum from prior slot to SystemMonitor.
 - SystemMonitor:
 - - Handle usage-sum: Add into system (PoD) load history. Check for overload? 
 - - Handle overload event: Just log for now. (Send to dashboard TBD ??)
 Message-ware:
 - An actor has a PID on the local node.
 - An actor sends a message to another actor using pattern: messenger:send( ActorID, Message ).
 - - Initial implementation is simply messenger:pid( ActorID ) ! Message 
 - - This version does not attempt to distribute processes across multiple nodes.
 - The messenger module maintains a mapping from ActorID to PID.
 - Actors are created by the simulator (in this case).
 - Actors self-report their PID to the messenger module.  -- messenger:register( ActorID, ProcessID ).
 - Messages can be broadcast using messenger:broadcast( Message ).
 - Actors may register their attributes -- messenger:attributes( ActorID, [ {Attribute,Value} ] ).
 - Messages can be broadcast to a group of actors according to their attributes -- messenger:broadcast( Message, [{A,V}]).
