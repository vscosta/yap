set(PL_SOURCES
	absf.yap
	arith.yap
	arithpreds.yap
	arrays.yap
	attributes.yap
	atoms.yap
	boot.yap
    bootutils.yap
	callcount.yap
	checker.yap
	consult.yap
	control.yap
	corout.yap
	debug.yap
	dbload.yap
	depth_bound.yap
	dialect.yap
	directives.yap
	eam.yap
	eval.yap
	errors.yap
	flags.yap
	grammar.yap
	ground.yap
	hacks.yap
	init.yap
	listing.yap
	bootlists.yap
	load_foreign.yap
        messages.yap
        meta.yap
	modules.yap
	newmod.yap
	os.yap
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
	statistics.yap
	strict_iso.yap
	tabling.yap
	threads.yap
	udi.yap
	undefined.yap
	utils.yap
	swi.yap
	yapor.yap
	yio.yap
  )

if (NOT ANDROID)

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


endif (NOT ANDROID)


install (FILES ${PL_SOURCES}
  DESTINATION ${libpl}/pl
  )
