%% Copyright 2011 Richard Alexander Green
%% Description: Access hour-of-week week-of-year profiles.
%% Created: Apr 25, 2011
-module(load_profile).

%%
%% Include files
%%

-define( TEST, true ).
-define( DEBUG, true ).


% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-include( "../include/objects.hrl").


%%
%% Exported Functions
%%
-export([ new/1, put_hour/4, put_day/3, map_profile_for_day_of_week_matching/2 ]).

%%
%% API Functions
%%

new( Owner ) -> new( Owner, IntervalSeconds = 3600 ).

new( Owner, IntervalSeconds ) ->
		#load_profile{ owner = Owner, interval_seconds = 60*60 }.

put_hour( Profile, 'Monday',   Hour, Usage ) -> put_hour( Profile, 1, Hour, Usage );
put_hour( Profile, 'Tuesday',  Hour, Usage ) -> put_hour( Profile, 2, Hour, Usage );
put_hour( Profile, 'Wednesday',Hour, Usage ) -> put_hour( Profile, 3, Hour, Usage );
put_hour( Profile, 'Thursday', Hour, Usage ) -> put_hour( Profile, 4, Hour, Usage );
put_hour( Profile, 'Friday',   Hour, Usage ) -> put_hour( Profile, 5, Hour, Usage );
put_hour( Profile, 'Saturday', Hour, Usage ) -> put_hour( Profile, 6, Hour, Usage );
put_hour( Profile, 'Sunday',   Hour, Usage ) -> put_hour( Profile, 7, Hour, Usage );


put_hour( Profile, DayOfWeek, Hour, Usage ) ->
		OldHours = Profile#load_profile.hours,
		NewHours = orddict:store( {DayOfWeek, Hour}, Usage, OldHours ),
		NewProfile = Profile#load_profile{ hours = NewHours },
		NewProfile.

put_day( Profile, DayOfWeek, UsageList ) ->
		HoursOfDay  = 
				    [ 0000, 0100, 0200, 0300, 0400, 0500, 0600, 0700
						, 0800, 0900, 1000, 1100, 1200, 1300, 1400, 1500
						, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300 ],
		NewProfile = map_hours( Profile, DayOfWeek, HoursOfDay, UsageList ),
		NewProfile.

map_profile_for_day_of_week_matching( ThisDate, FromLoadProfile ) ->
		DayOfWeek = calendar:day_of_the_week( ThisDate ),
		Hours =  FromLoadProfile#load_profile.hours,
		?assert( length( Hours ) > 0 ),
		%?debugVal( { DayOfWeek, Hours } ),
		DayProfile = [ Value || { { Day, _Hour }, Value } <- Hours, Day == DayOfWeek ],
		ExpectedLength = (24*60*60) div FromLoadProfile#load_profile.interval_seconds,
		?assertEqual( ExpectedLength, length( DayProfile) ),
		DayProfile.

	
%%
%% Local Functions
%%

map_hours( Profile, _DayOfWeek, [], [] ) ->
		?assert( Profile#load_profile.hours =/= [] ),
		Profile;
map_hours( Profile, DayOfWeek, HoursOfDay, UsageList ) ->
		[ Hour | RemainingHours ] = HoursOfDay,
		[ Usage | RemainingUsage ] = UsageList,
		NewProfile = put_hour( Profile, DayOfWeek, Hour, Usage ),
		map_hours( NewProfile, DayOfWeek, RemainingHours, RemainingUsage ).


% ============================== TESTS ================================

new_test() ->
		Owner = 'usage_test',
		Profile = new( Owner ),  % Owner is generally a PoD -- But it could be a generic type.
		%?debugVal( Profile),
		?assertEqual( #load_profile{ owner = usage_test, interval_seconds = 3600, hours = []}
		            , Profile ),
		ok.

hour_test() ->
		Owner = 'usage_test',
		Profile = new( Owner ),  % Owner is generally a PoD -- But it could be a generic type.
		DayOfWeek = 'Sunday',
		Hour = 00,
		Usage = 1234,  % TODO: Assume Watt-Hours for now
		HourResult = put_hour( Profile, DayOfWeek, Hour, Usage ),
		%?debugVal( HourResult ),
		?assertEqual( #load_profile{ owner= usage_test
															 , interval_seconds = 3600
															 , hours = [ { {7,0}, 1234 } ]  }
		            , HourResult ),
		ok.

day_test() ->
		Owner = 'usage_test',
		Profile = new( Owner ),  % Owner is generally a PoD -- But it could be a generic type.
		DayOfWeek = 'Sunday',
		Usage = [ 0000, 0100, 0200, 0300, 0400, 0500, 0600, 0700
						, 0800, 0900, 1000, 1111, 1200, 1300, 1400, 1500
						, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300 ],  % Not a realistic profile -- just for test
		DayResult = put_day( Profile, DayOfWeek, Usage ),
		%?debugVal( DayResult ),
		?assertEqual( #load_profile{ owner = usage_test
															 , interval_seconds = 3600
															 , hours =
                          [{{7,000},000},
                           {{7,100},100},
                           {{7,200},200},
                           {{7,300},300},
                           {{7,400},400},
                           {{7,500},500},
                           {{7,600},600},
                           {{7,700},700},
                           {{7,800},800},
													 {{7,900},900},
													 {{7,1000},1000},
                           {{7,1100},1111},  % <<< Make sure we don't mix the lists up
                           {{7,1200},1200},
                           {{7,1300},1300},
                           {{7,1400},1400},
                           {{7,1500},1500},
                           {{7,1600},1600},
                           {{7,1700},1700},
                           {{7,1800},1800},
													 {{7,1900},1900},
													 {{7,2000},2000},
                           {{7,2100},2100},
                           {{7,2200},2200},
                           {{7,2300},2300}
													]
									}
									, DayResult ),
		ok.

week_test() ->
		Owner = 'usage_test',
		Profile0 = new( Owner ),  % Owner is generally a PoD -- But it could be a generic type.
		Usage = [ 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700
						, 3800, 3900, 1000, 1111, 1200, 1300, 1400, 1500
						, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300 ],  % Not a realistic profile -- just for test
		Profile1 = put_day( Profile0, 'Sunday', Usage ),
    Profile2 = put_day( Profile1, 'Monday', Usage ),
		Profile3 = put_day( Profile2, 'Tuesday', Usage ),
    Profile4 = put_day( Profile3, 'Wednesday', Usage ),
		Profile5 = put_day( Profile4, 'Thursday', Usage ),
    Profile6 = put_day( Profile5, 'Friday', Usage ),
		Profile7 = put_day( Profile6, 'Saturday', Usage ),
		?assertEqual( 24*7, orddict:size( Profile7#load_profile.hours) ),
		Profile7.

map_profile_for_day_of_week_matching_test() ->
		Date = { 2011,12,31 },
		WeekProfile = week_test(),
		DayProfile = map_profile_for_day_of_week_matching( Date, WeekProfile ),
		?debugVal( DayProfile ),
		?assertEqual( 24, length( DayProfile ) ).

