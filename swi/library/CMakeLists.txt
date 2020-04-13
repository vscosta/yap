set (LIBRARY_PL
INDEX.pl
  aggregate.pl
  autoloader.yap
  base64.pl
  broadcast.pl
  ctypes.pl
  date.pl
  debug.pl
  edit.pl
  error.pl
  main.pl
  menu.pl
  nb_set.pl
  occurs.yap
  operators.pl
  option.pl
  pairs.pl
  plunit.pl
  predicate_options.pl
  predopts.pl
  prolog_clause.pl
  prolog_colour.pl
  prolog_source.pl
  prolog_xref.pl
  pure_input.pl
  quasi_quotations.pl
  quintus.pl
  record.pl
  settings.pl
  shlib.pl
  thread_pool.pl
  unix.pl
  url.pl
  utf8.pl
  win_menu.pl
  www_browser.pl
  dcg/basics.pl
  )


install(FILES ${LIBRARY_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}      
  )

  add_to_dir (LIBRARY_PL YAP_INSTALL_DATADIR )
