{ application, messenger,                        % application's name
  [                                              % list application's attributes
    { description, "Messaging service - supports publish-subscribe and replay" },    % . description
    { vsn, "0.2.0" },                            % . version number
    { modules,                                   % . List ALL the run-time modules.
      [ messenger_app                              % . . module with application behavior
      , messenger_root_sup                          % . . module with supervisor behavior
      , messenger                                   % . . module with gen_server behavior
      , messenger_event_mgr                         % . . module with gen_event behavior
      , messenger_event_handler_sup                 % . . module with simple_one_for_one supervisor behavior
      , messenger_subscriber_proxy                  % . . module with gen_event behavior  
        ]                                        % . when list is long, maintain alphabetic order
      },
    { registered, [ messenger, messenger_event_mgr ] },     % . what process names will be registered?
    
    { applications, [ kernel, stdlib ] },        % . applications to start prior to this one
    { mod, { messenger_app, [[ {log_file, "messenger_log" } ]] }  }              % . start-up module (with application behavior)
    ]
  }. 