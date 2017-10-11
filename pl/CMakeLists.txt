	set(PL_BOOT_SOURCES
absf.yap
arith.yap
arithpreds.yap
arrays.yap
atoms.yap
attributes.yap
boot.yap
bootlists.yap
callcount.yap
checker.yap
consult.yap
control.yap
corout.yap
dbload.yap
debug.yap
depth_bound.yap
dialect.yap
directives.yap
eam.yap
error.yap
errors.yap
eval.yap
flags.yap
grammar.yap
ground.yap
hacks.yap
init.yap
listing.yap
load_foreign.yap
messages.yap
meta.yap
modules.yap
newmod.yap
os.yap
pathconf.yap
preddecls.yap
preddyns.yap
preds.yap
profile.yap
protect.yap
qly.yap
save.yap
setof.yap
signals.yap
sort.yap
spy.yap
statistics.yap
strict_iso.yap
swi.yap
tabling.yap
threads.yap
udi.yap
undefined.yap
utils.yap
yapor.yap
  )

  add_to_group(  PL_BOOT_SOURCES   pl_boot_library )

  add_custom_target (STARTUP ALL SOURCES
    DEPENDS ${CMAKE_BINARY_DIR}/startup.yss              
    )

  if ( NOT CMAKE_CROSSCOMPILING )
    # Create a startup.yss on the top directory.
    add_custom_command (OUTPUT ${CMAKE_BINARY_DIR}/startup.yss
      COMMAND yap-bin -B
      VERBATIM
      WORKING_DIRECTORY ${CMAKE_TOP_BINARY_DIR}
      DEPENDS yap-bin  ${PL_SOURCES}
      )
  endif ( NOT CMAKE_CROSSCOMPILING )


install (FILES ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
  DESTINATION ${dlls}
  )



install (FILES ${PL_SOURCES}
  DESTINATION ${libpl}/pl
  )
