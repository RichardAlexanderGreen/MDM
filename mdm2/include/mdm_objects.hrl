%% Copyright Richard Alexander Green 2011
%% Description: Version 02 of MDM in Erlang.
% - The architecture of this version of the MDM is designed to be more distributed
% and more testable (simulation friendly).
% Circuits were simply attributes in version 01.
% Circuits are active processes in this version.
% This provides an opportunity to structure the architecture for more concurrency and decentralization.
%
% Architecture:
%
% - Meters publish meter and service events. { notice meter service circuit event }
% - Network distributes events to subscribers. 
% - Network logs all notices received from meters (for replay and audit trail).
%
% - Each circuit process subscribes to events on its circuit topic.
% - Each circuit records service usage, service events for connected services.
% - Each circuit calculates transformer loads.
% - Each circuit will respond to queries about local services and devices.
%
% Production "Meter to Bill" Data flow:
%
% - The installer installs a certain number of meters per day.
% - As meters are installed, so are their circuits.
% - After a meter is installed, it starts publishing usage events on various topics and echoed on the circuit topic.
%
% - Usage, power, tamper, network, and configuration events are published on their respective topics.
% - Meter communications are invoked by tick events from their circuit.
% - The network stores (logs) and forwards the usage events to topic subscribers.
%
% - Each circuit subscribes to its own circuit topic.
% - The circuit calculates service usage month-to-date and stores it for fast look-up.
% - The circuit aggregates interval usage at the transformer and circuit level to create demand histories at those levels.
% - Fail-safe redundancy can be achieved by replicating the circuit process across hosts.
% - In addition, each meter has its own history and a replay can be requested when all else fails.
%
% - Each service has a next-bill date-time which triggers a billing event -- sent by the circuit to billing.
% - Billing increments the balance owed, sends a statement, and (when enabled) invokes auto-payment.
%
% Test / Simulation "Meter to Bill" Data flow:
%
% - The key differences between test mode and real mode are the clock and the network.
%
% - In production (real) mode, the clock tick is driven from actual time.
% - In simulation (test) mode, the clock tick is driven by the simulator.
%
% - In test mode, the network refuses subscribers that are not also in test mode.
% - In real mode, the network refuses subscribers that are not also in real mode.
%
% - In either mode, the installer is driven by work-order completion (events).
% - In test mode, the work-order completions are produced by the sim_installer.
%
% - In real mode, the meter events would be driven by c12.19 meters, c12.22 meter network, and a vendor-specific collection engine.
% - In test mode, the meter events are driven by the sim_demand module.
%
% Simulating Landscape:
% - Sim_Installer populates circuits with services and meters.
% - As soon as a service is installed, it is available to Sim_Demand.
%
% Simulating Actors:
% - Sim_Demand publishs meter and service events (simulating meter population)
% - Network operates in test mode. (*)
% - Circuits operate in test mode.
% - Outage operates in test mode.
% - RT-Bill operates in test mode.
% * - An actor in test mode can only send/receive using the test Network.
% * - In test mode, calls to the clock return simulated time rather than actual time.
% * - Actors must call clock:functions instead of calendar:functions and now().

% ------------------------------------------------------
% Business Entities in Metering Domain
% ------------------------------------------------------
%
% Asset
-record( asset, { oid, mfr, model, part_number, asset_attributes, asset_history } ). % The term mfr is short-hand for manufacturer
-record( asset_attributes, { asset_id, attribute_dict } ).  % attribute_dict is an orddict

% Device is an Asset  ( device oid == asset oid )
-record( device, { oid, device_events } ).
-record( device_events, { oid, device_events }).            % Device events are detected by the device

% Meter is a Device   ( meter oid == device oid).  (Device is an asset)
-record( meter, { oid
								, type_of_meter     % type of metering hardware
								, configuration     % name of meter configuration
								}).

%								, service }).       % Service where the meter is currently installed

-record( meter_installed_on_service, { meter, when_installed, service } ).  % {meter, when_installed } ==> service

% Note: The meter is the actor that witnesses the service usage and service events.
% The meter also experiences device events which affect its operational state.
% The meter also has asset events (asset_history) which affect inventory and asset management processes.

% Service is an asset. 
% Service is a sink.
% - It gets installed / inspected / maintained / removed; and it has attributes. 
% Service attributes include type, premise, coordinates.
-record( service, { oid, premise, coordinates, circuit, transformer
									, type = "Residential Electric"                      % default service.type
									, service_agreement = "Std Residential Rate"         % default service.service_agreement
									, service_events = []                                % default - Event history is empty
									} ).
-record( service_agreement, { oid, when_agreed, pricing, account } ).  % pricing ~ rate_plan ~ product_code (synonyms)

% Premise (as in "premises") -- examples: home | apartment | store | office-space | building 
% A premise has a street address.
% The term is used instead of site because people tend to associated site with a parcel of land or a building.
% Meters often measure the enegy delivered to a sub-unit of a building, such as an apartment or office space.
-record( premise, { oid, address, services } ).  % services = [ service ]

% Demand Sink -- absorbs system capacity (power, gas, water) at some level.
% This concept is primarily relevant for studying aggregated demand (summed across selected services)
% Sink is a supertype of service / circuit / transformer.

-record( sink, { oid, type, coordinates, load_profile, load_history }).


% Circuit -- is a demand sink with a collection of assets including services, transformers, devices.
-record( circuit, { oid, services, transformers, devices } ).


% ------------------------------------------------------
% Domain events
% ------------------------------------------------------
% From meter -- witnessing service usage and events
% Meter says: 
% - Service S  usage U.  -- Usage = { observation O for_period P }
% - Service S  event E.  -- Event = power_out | power_restored | load_side_voltage | tamper | switch_opened | switch_closed
% From meter -- witnessing device events
% - Meter M reports event E. -- Event = { firmware F staged } | { firmware F installed } | 
%                                     | { binding_to_relay R } | { configuration C set }
% From asset management -- witnessing asset events
% - Asset A installed on service S.
% - Asset A removed from service S.
% - Asset A withdrawn from inventory I.
% - Asset A deposited in inventory I.
% - Asset A moved from inventory I1 to inventory I2.
% - Asset A scrapped.
% - Asset A is of type T. (links to attributes in part catalog)
% - Define type T attributes [A]. (orddict with standard attributes of part)
% - Configure asset A with attributes-valus [V]. (orddict with configurable attribute values)
% ------------------------------------------------------
% Generic data types
% ------------------------------------------------------
-record( history, { oid, type_of_history, events }).   % events = [event]  in reverse chronological order
% type_of_history = asset_history | device_events | usage_history | service_events
% The oid identifies the thing with the history

-record( coordinates, { oid, latitude, longitude }).   % oid of the thing with the coordinates

-record( address, { oid, street_1, street_2, postal_code }).  % oid of the premise / contact

-record( city, { postal_code, city, state, nation }).         % third-normal-form -- hurrah!
% Note: This is the postal city, not necessarily the municipal (tax) jurisdiction. 

-record( account, { oid, type_of_account, party, current_balance, transactions } ).

-record( party, { oid, name, alpha_name, phones, contacts } ).

-record( time_series, { oid, time_values } ).  
% time_values = [ { time, value } ]  
% time = gregorian_seconds -- when value was observed (the end of an interval)
% gregorian_seconds -- see: calendar:gregorian_seconds_to_datetime/1
