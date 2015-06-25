% service / transformer / circuit / load_history include this declaration
-record( state, { id = ['undefined id']
	              , aggregator= ['undefined aggregator']
	              , history
	              , priorDateTime
	              , load_profile
	              }).  