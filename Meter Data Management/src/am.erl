%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(am).
-compile(export_all).

start() ->
		AssetsPID = spawn( fun() -> asset_db:start_link() end ),												
    spawn( fun() -> receiveLoop([]) end).   % Returns PID of am server

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
			{Pid, Response} ->
	    	Response
    end.
	    
receiveLoop(X) ->
    receive
			{asset, AssetID, has_attribute, Attribute, value, Value} ->
				see({asset, AssetID, has_attribute, Attribute, value, Value}),
				Db = whereis(asset_db),
				Db ! { remember_asset_attribute, AssetID, Attribute, Value}
				;
			{asset, AssetID, has_attribute_set, OrdDict } ->
				see({asset, AssetID, has_attribute_set, OrdDict }),
				Db = whereis(asset_db),
				Db ! { remember_multiple_attributes, AssetID, OrdDict }
				;
				
			%Catch patterns we have not implemented yet.		
			Any -> 
	   	 io:format("Not Implmented yet:~n~p~n",[Any]),
	   	 receiveLoop(X)
    end,
		receiveLoop(X).

see( Tuple ) ->
		io:format("See~n~p~n", [Tuple] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Use Cases / Messages:
%% Installation process informs AssetManager:
%%   Assets ! {asset, AssetID, has_attribute_set, OrdDict }.
%%   Assets ! {asset, AssetID, has_attribute, Attribute, value, Value}
%% Field-Service adds new service (points-of-delivery) to a circuit.
%%   Assets ! {asset, Service_ID, installed_at_site, SiteID, location, {location, Latitude, Longitude}, connected_to, Transformer_ID}
%% Field-Service attaches/removes a metering device to a service.
%%   Assets ! {asset, Device_ID, is_metering_service, Service_ID }
%%   Assets ! {asset, Device_ID, is_removed_from_service, Service_ID }
%% 
test_server() ->
		Me = start(),
		test_AssetHasAttributeValue(Me),
		test_AssetHasAttributeSet(Me),
		
		ok.

test_AssetHasAttributeValue(Assets) ->
		AssetID = "Meter-987654321",
		Attribute = "Configuration",
		Value = "Residential Standard",
		Assets ! {asset, AssetID, has_attribute, Attribute, value, Value},
		ok.

test_AssetHasAttributeSet(Assets) ->
		AssetID = "Meter-987654321",
		Attribute = "Configuration",
		Value = "Residential Standard",
		Dict0 = orddict:new(),
		Dict1 = orddict:append(Attribute, Value, Dict0),
		Dict2 = orddict:append("ServiceID", "Service-1234567", Dict1),
		Assets ! {asset, AssetID, has_attribute_set, Dict2 },
		ok.

		

		  
