% Event object shared by several modules
-record( event, {  notice_ID = 0, topic, from, notice}).

% Record structure used to pass subscriber attributes.
% module -- reference the module -- for gen_server:call/3 -- Module is assumed to be a server !
% topic  -- term() ==> Ignore topic values that don't match term() | all ==> Process all topics
-record( subscriber_attributes, { module, topic } ).

