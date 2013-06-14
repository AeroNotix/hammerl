{application,hammerl,
             [{description,"Yet another blog engine."},
              {vsn,"1"},
              {registered,[]},
              {applications,[inets,cowboy,ranch,kernel,observer,runtime_tools,
                             stdlib,wx]},
              {mod,{hammerl_app,[]}},
              {env,[]},
              {modules,[blog,hammerl,hammerl_app,hammerl_sup,index]}]}.
