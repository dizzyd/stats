{application, stats,
 [{description, "Erlang Statistics Library"},
  {vsn, "1"},
  {modules, [ stats_sample,
              stats_histogram ]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, []}
]}.
