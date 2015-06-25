{ application, mdm2,                             % application's name
  [                                              % list application's attributes
    { description, "Meter Data Management" },    % . description
    { vsn, "0.2.0" },                            % . version number
    { modules,                                   % . List ALL the run-time modules.
      [ mdm2_app,                                % . . module with application behavior
        mdm2_sup,                                % . . module with supervisor behavior
        asset_mgr                   % . . modules with gen_server behavior
        ]                                        % . when list is long, maintain alphabetic order
      },
    { registered, [ asset_mgr ] },  % . what process names will be registered?
    
    { applications, [ kernel, stdlib ] },        % . local applications to start prior to this one
    { mod, { mdm2_app, [] }  }                   % . start-up module (with application behavior)
    ]
  }. 