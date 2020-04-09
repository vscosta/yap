set (LIBRARY_PL
  INDEX.pl
  apply.yap
  apply_macros.yap
  arg.yap
  assoc.yap
  atts.yap
  autoloader.yap
  avl.yap
  bhash.yap
  charsio.yap
  clauses.yap
  coinduction.yap
  dbqueues.yap
  dbusage.yap
  dgraphs.yap
  exo_interval.yap
  expand_macros.yap
  gensym.yap
  hacks.yap
  heaps.yap
  lambda.pl
  lineutils.yap
  listing.yap
  lists.yap
  log2md.yap
  nb.yap
  ordsets.yap
  mapargs.yap
  maplist.yap
  maputils.yap
  matlab.yap
  matrix.yap
  prandom.yap
  queues.yap
  random.yap
  range.yap
  rbtrees.yap
  regexp.yap
  readutil.yap
  rltree.yap
  sockets.yap
  splay.yap
  stringutils.yap
  system.yap
  terms.yap
  tries.yap
  itries.yap
  timeout.yap
  trees.yap
  ugraphs.yap
  undgraphs.yap
  varnumbers.yap
  wdgraphs.yap
  wgraphs.yap
  wundgraphs.yap
  lam_mpi.yap
   ytest.yap
   c_alarms.yap
  flags.yap
  block_diagram.yap
  )


MY_add_subdirectory(dialect)
MY_add_subdirectory(clp)
MY_add_subdirectory(matlab)
MY_add_subdirectory(matrix)
MY_add_subdirectory(random)
MY_add_subdirectory(regex)
MY_add_subdirectory(rltree)
MY_add_subdirectory(system)
MY_add_subdirectory(tries)
MY_add_subdirectory(ytest)

add_to_group( LIBRARY_PL  pl_library)

install(FILES ${LIBRARY_PL} DESTINATION ${libpl})

if (ANDROID)
file( INSTALL  ${LIBRARY_PL} DESTINATION ${libpl} )
endif()