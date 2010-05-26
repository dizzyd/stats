{application, stats,
 [{description, "Erlang Statistics Library"},
  {vsn, "3"},
  {modules, [ stats_sample,
              stats_histogram,
              stats_rv,
              stats_utils]},
  {registered, []},
  {applications, [kernel, 
                  stdlib, 
                  sasl]},
  {env, []}
]}.
