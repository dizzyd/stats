{application, stats,
 [{description, "Erlang Statistics Library"},
  {vsn, "1"},
  {modules, [ stats_sample,
              stats_histogram,
              stats_utils]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, []}
]}.
