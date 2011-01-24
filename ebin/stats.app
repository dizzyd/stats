{application, stats,
 [{description, "Erlang Statistics Library"},
  {vsn, "1.0.0"},
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
