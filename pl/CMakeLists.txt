set(PL_BOOT_SOURCES
        absf.yap
        arith.yap
        arithpreds.yap
        arrays.yap
        atoms.yap
        attributes.yap
        boot.yap
        bootlists.yap
	bootutils.yap
	builtins.yap
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
        imports.yap
        init.yap
        listing.yap
        load_foreign.yap
        messages.yap
        meta.yap
	metadecls.yap
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
        top.yap
        udi.yap
        undefined.yap
        utils.yap
        yapor.yap
        ypp.yap
        )

add_to_group(PL_BOOT_SOURCES pl_boot_library)

if (ANDROID)
    add_custom_target(STARTUP
       DEPENDS ${PL_BOOT_SOURCES}
            )
    file (INSTALL  ${PL_BOOT_SOURCES} DESTINATION ${libpl}/pl)
elseif(CMAKE_CROSSCOMPILING)
    add_custom_target(STARTUP ALL SOURCES
            DEPENDS ${PL_BOOT_SOURCES}
            )
else ()
    add_custom_target(STARTUP ALL
            DEPENDS  ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
            )
    add_custom_command(OUTPUT ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
            COMMAND yap-bin -B
            VERBATIM
            DEPENDS ${PL_BOOT_SOURCES} yap-bin
            )

#    install(CODE "execute_process(COMMAND ./yap -B
#      WORKING_DIRECTORY ${CMAKE_TOP_BINARY_DIR})"
#            DEPENDS Py4YAP ${PL_BOOT_SOURCES} yap-bin )


    install(FILES ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
        DESTINATION ${YAP_INSTALL_DLLDIR}
        )
endif()


install(FILES ${PL_BOOT_SOURCES}
        DESTINATION ${libpl}/pll
        )
