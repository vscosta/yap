	set(PL_SOURCES
absf.yap
arith.yap
arithpreds.yap
arrays.yap
atoms.yap
attributes.yap
boot.yap
bootlists.yap
bootutils.yap
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
pl
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
yio.yap
  )

if (ANDROID)
	file(INSTALL ${PL_SOURCES}  DESTINATION ${libpl}/pl)

else()
add_custom_target (${YAP_STARTUP} ALL SOURCES ${PL_SOURCES} 	${YAPOS_PL_SOURCES}  WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )

# Create a startup.yss on the top directory.
add_custom_command (TARGET ${YAP_STARTUP}
  COMMAND yap-bin -B
  VERBATIM
  WORKING_DIRECTORY ${CMAKE_TOP_BINARY_DIR}
  DEPENDS yap-bin  ${PL_SOURCES}
  )


install (FILES ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
  DESTINATION ${dlls}
  )


endif (ANDROID)


install (FILES ${PL_SOURCES}
  DESTINATION ${libpl}/pl
  )
