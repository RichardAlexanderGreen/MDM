{ application, messenger,                              % application's name
  [                                              % list application's attributes
    { description, "Meter Data Management" },    % . description
    { vsn, "0.2.0" },                            % . version number
    { modules,                                   % . List ALL the run-time modules.
      [ messenger_app,                                 % . . module with application behavior
        messenger_sup,                                 % . . module with supervisor behavior
        messenger, messenger_logger           % . . modules with gen_server behavior
        ]                                        % . when list is long, maintain alphabetic order
      },
    { registered, [ messenger, messenger_logger ] },  % . what process names will be registered?
    
    { applications, [ kernel, stdlib ] },        % . local applications to start prior to this one
    { mod, { messenger_app, [] }  }                    % . start-up module (with application behavior)
    ]
  }. 