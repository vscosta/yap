set (PROGRAMS
  problog.yap 
  problog_lbdd.yap 
  problog_lfi.yap 
  dtproblog.yap 
  aproblog.yap 
  problog_learning.yap
  problog_lbfgs.yap
  problog_learning_lbdd.yap
  )

set (PROBLOG_PROGRAMS
  problog/extlists.yap 
  problog/flags.yap 
  problog/gflags.yap 
  problog/grounder.yap 
  problog/hash_table.yap 
  problog/intervals.yap 
  problog/logger.yap 
  problog/mc_DNF_sampling.yap 
  problog/os.yap 
  problog/print.yap 
  problog/sampling.yap 
  problog/tabling.yap 
  problog/timer.yap 
  problog/ptree.yap 
  problog/variable_elimination.yap 
  problog/print_learning.yap 
  problog/utils_learning.yap 
  problog/version_control.yap 
  problog/nestedtries.yap 
  problog/utils.yap 
  problog/lbdd.yap 
  problog/utils_lbdd.yap 
  problog/ad_converter.yap 
  problog/termhandling.yap 
  problog/completion.yap 
  problog/discrete.yap 
  problog/bdd.yap 
  problog/variables.yap
  )

set (PROBLOG_EXAMPLES
  problog_examples/alarm.pl 
  problog_examples/graph.pl 
  problog_examples/graph_tabled.pl 
  problog_examples/learn_graph.pl 
  problog_examples/office.pl 
  problog_examples/viralmarketing.pl 
  problog_examples/aProbLog_examples.pl 
  problog_examples/viralmarketing_tabled.pl
  )

set (LIBRARY_PL
 ${PROGRAMS}
 ${PROBLOG_PROGRAMS}
 )

install(FILES ${PROGRAMS}
  DESTINATION ${YAP_INSTALL_DATADIR}
  )

install(FILES ${PROBLOG_PROGRAMS}
  DESTINATION ${YAP_INSTALL_DATADIR}/problog
  )


