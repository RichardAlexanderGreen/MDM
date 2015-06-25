I got confused while writing mdm8 -- So I am starting over.
The original goal for mdm8 was to keep things simple.
But I got confused by trying to incorporate an existing actor framework that depends on an include.
The problem was that the actor framework assumed that actors were like SOA services (singletons).
But the MDM simulation requires actors with many instances.
Goals for this version:
- New actor framework:
- - There may be many instances of an actor.
- - The actor module creates new instances.
- - The actor module provides utility functions to isolate interactions with the messenger.
- MAYBE NOT !!!!
- - Note that service / transformer / circuit actors ARE NOT HUM ACTORS -- THEY ARE NOT DRIVEN BY JOBS.
- - These modules simulate a coordinated network of SENSORS. SO MAYBE I SHOULD USE A DIFFERENT METAPHOR.
- - SENSORS ARE NOT DRIVEN BY REQUESTS FOR SERVICE. THEY ARE DRIVEN BY EXTERNAL NON-BUSINESS EVENTS.
- - THESE SENSORS ARE ALSO NOT DRIVEN BY CLIENT DIALOG GESTURES -- SO THE HUM SENSOR CONCEPTS DO NOT APPLY EXACTLY.
- - HUM assumes an asynchronous business service context.
- - The MDM simulation assumes a network of sensors. Such a network is more like a real-time control system, not Hum.
- - This raises the "Simple English" question: Can I express real-time programming in a natural-language notation?
- - See MDM_Ontology.txt -- Which attempts to do that.
- There is a messenger that stores and forwards messages.
- - The messenger can also forward broadcasts to multiple actors.
- - Actors register themselves with the messenger when they are created/initialized.
- - Actors de-register themselves when terminated. Assume a controlled shut-down.
- - MESSENGER WAS DESIGNED TO BE PART OF AN ACTOR FRAMEWORK -- PERHAPS IT IS OUT OF PLACE HERE!
- Persistence:
- - To reduce persistence overhead,
- - - the PoD (service, transformer, circuit) load histories will be forwarded to a logger periodically.
- - That logger will simply append data to a file.
- - The file can be post-processed (map-reduce style) for billing purposes. 
Keep it simple road-map.
- The initial version will assume everything running in one node.
- A later version will be re-factored so that actors may be on multiple nodes.
