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
  bootlists.yap
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
  ypp.yap
   ytest.yap
   c_alarms.yap
  flags.yap
  block_diagram.yap
  )

add_subdirectory(clp)
add_subdirectory(dialect)
add_subdirectory(matlab)
add_subdirectory(matrix)
add_subdirectory(random)
add_subdirectory(regex)
add_subdirectory(rltree)
add_subdirectory(system)
add_subdirectory(tries)
add_subdirectory(ytest)

add_custom_target (library SOURCES ${LIBRARY_PL} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )


install(FILES ${LIBRARY_PL}
  DESTINATION ${libpl}
  )
