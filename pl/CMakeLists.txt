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
        top.yap
        udi.yap
        undefined.yap
        utils.yap
        yapor.yap
        )

add_to_group(PL_BOOT_SOURCES pl_boot_library)

install(FILES ${PL_BOOT_SOURCES}
        DESTINATION ${libpl}/pl
        )
install(FILES ../library/ypp.yap
DESTINATION ${libpl}/library)
#         )
# if (ANDROID OR CMAKE_CROSSCOMPILING)
#     add_custom_target(STARTUP
#               )
# else()
    add_custom_target(STARTUP ALL
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/startup.yss)

        add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/startup.yss
        COMMAND yap-bin -B${CMAKE_CURRENT_SOURCE_DIR} --output-saved-state=${CMAKE_CURRENT_BINARY_DIR}/startup.yss
         DEPENDS ${PL_BOOT_SOURCES} yap-bin
            )
#else ()
    #add_custom_target(STARTUP ALL
    #        DEPENDS  ${CMAKE_TOP_BINARY_DIR}/${YAP_STARTUP}
    #        )

    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/startup.yss DESTINATION ${dlls} )


#endif()
