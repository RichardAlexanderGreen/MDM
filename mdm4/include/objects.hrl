% Data Types 

-record( usage, { quantity, unit_of_measure, time_period } ).

-record( coordinates, { latitude, longitude }).

-record( pod_usage, { pod_ID, usage = #usage{} }).

-record( series, { series_type                    % A name indicating content / methodolgy of measurement
								 , interval_seconds = 3600        % = number of seconds in each interval 
								 , unit_of_measure = 'Watt-hour'  % 'Watt-hour'|'kiloWatt-hour'|'Watt-second'|'Watt'|'kiloWatt'
								 , curve = orddict:new()          % maps Gregorian-Seconds to a measurment quantity 
								 } ).                             % *** DO NOT MANIPULATE DIRECTLY - USE time_series module.

-record( pod_series, { pod_ID, series = #series{} }).


-record( time_period, { start_seconds, stop_seconds } ).

-record( load_profile, { owner                    % a PoD_ID or the name of some statistical sample
											 , interval_seconds = 3600  % number of seconds in the intervals
	                     , hours = orddict:new()    % map {day-of-week, hour-of-day} to a quantity (e.g. Watt-hour average)
											 }).                        % *** DO NOT MANIPULATE DIRECTLY - USE load_profile module.


% Entities - *** DO NOT MANIPULATE DIRECTLY - USE object_factory module to create / update  these records.

-record( service, { service_ID             % A service is a pod, a pod is a device, a device is an asset.
									, transformer_ID         % A service is connected to a transformer. 
									, site_ID                % A service is located on a site.
									, meter_ID               % A service is monitored by a meter.
									} ).
-record( meter, { meter_ID                 % A meter is a device, a device is an asset.
								, service_ID               % Meter is monitoring service.
								, configuration            % TODO -- A meter has a configuration 
								}).
-record( transformer, { transformer_ID     % A transformer is a pod, a pod is a device, a device is an asset.
											, circuit_ID         % A transformer is connected to a circuit.
											}).
-record( circuit, { circuit_ID             % A circuit is a PoD, a PoD is a device, a device is an asset.
									, next_billing_date      % A circuit has a next_billing_date (whole circuit is billed same day).
									}).
-record( pod,     { pod_ID                          % A PoD is a device, a device is an asset.
									, load_history = #series{}        % A PoD has a load_history ( a time-series of actual usage)
									, load_profile = #load_profile{}  % A PoD may have a load_profile ( hour of week predicted usage )
									} ).
-record( device, { device_ID               % A device is an asset.
								 , manufacturer
								 , model
								 , connections             % A device may be connected to other devices (e.g. circuits to circuits)
								 }).
-record( asset, { asset_ID                 % An asset is a supertype of a device | PoD | circuit | transformer | service.
								, coordinates              % An asset has a location.
								}).
-record( site, { site_ID                   % a.k.a. premise | premises | residence | place of business          
							 , street_address            % A site has a street address | delivery address (not a post office box)
							 , coordinates               % coo
							 }).

