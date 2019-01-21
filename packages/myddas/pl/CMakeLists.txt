set(MYDDAS_YPP
        myddas.ypp
        myddas_assert_predicates.ypp
        myddas_top_level.ypp
        myddas_errors.ypp
        myddas_prolog2sql.ypp
        myddas_mysql.ypp
        myddas_util_predicates.ypp
        myddas_prolog2sql_optimizer.ypp)

set(MYDDAS_DRIVERS
        myddas_driver.ypp
        )
set(MYDDAS_DBMS sqlite3 postgres odbc)

if (ANDROID)
 set (MYDDAS_PL_OUTDIR ${YAP_APP_DIR}/src/generated/assets/Yap )
 else()
 set (MYDDAS_PL_OUTDIR ${CMAKE_CURRENT_BINARY_DIR})
 endif()
get_property(MYDDAS_FLAGS GLOBAL PROPERTY COMPILE_DEFINITIONS)
foreach (filename ${MYDDAS_YPP})
    get_filename_component(base ${filename} NAME_WE)
    set(base_abs ${MYDDAS_PL_OUTDIR}/${base})
    set(outfile ${base_abs}.yap)
    list(APPEND MYDDAS_YAP_FILES ${outfile})
    execute_process(
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
                   )
    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)
endforeach  ()
foreach (dbms ${MYDDAS_DBMS} )
    set(outfile ${MYDDAS_PL_OUTDIR}/myddas_${dbms}.yap)
    list(APPEND MYDDAS_YAP_FILES ${outfile})
  execute_process(
                COMMAND ${CMAKE_C_COMPILER} -D${dbms} -x c -E -P -w   ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp -o ${outfile}
                 )

    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)

endforeach()
    list(APPEND MYDDAS_YAP_FILES  ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/sqlitest.yap ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/chinook.db)

        install(FILES ${MYDDAS_YAP_FILES} DESTINATION ${YAP_INSTALL_DATADIR})
