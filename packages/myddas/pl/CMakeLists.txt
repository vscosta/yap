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

 set (MYDDAS_PL_OUTDIR ${YAP_APP_DIR}/src/generated/assets/Yap )
get_property(MYDDAS_FLAGS GLOBAL PROPERTY COMPILE_DEFINITIONS)
foreach (filename in ${MYDDAS_YPP})
    get_filename_component(base ${filename} NAME_WE)
    set(base_abs ${MYDDAS_PL_OUTDIR}/${base})
    set(outfile ${base_abs}.yap)
    execute_process(
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
                   )
    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)
endforeach  ()
foreach (dbms in ${MYDDAS_DBMS} )
    set(outfile ${MYDDAS_PL_OUTDIR}/myddas_${dbms}.yap)
  execute_process(
                COMMAND ${CMAKE_C_COMPILER} -D${dbms} -x c -E -P -w   ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp -o ${outfile}
                 )

    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)

endforeach()

        set( MYDDAS_YAP ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/sqlitest.yap ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/chinook.db)
#add_to_group(MYDDAS_YAP pl_library )
        file(INSTALL ${MYDDAS_YAP}
               DESTINATION ${MYDDAS_PL_OUTDIR} )
