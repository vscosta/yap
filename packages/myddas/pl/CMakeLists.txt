set(MYDDAS_YPP
        myddas_assert_predicates.ypp
        myddas_top_level.ypp
        myddas_errors.ypp
        myddas_prolog2sql.ypp
        myddas_util_predicates.ypp
        myddas_prolog2sql_optimizer.ypp)

get_property(MYDDAS_FLAGS GLOBAL PROPERTY COMPILE_DEFINITIONS)

if (ANDROID)
 set (MYDDAS_PL_OUTDIR ${CMAKE_SOURCE_DIR}/../yaplib/src/generated/assets/Yap/myddas )
 file(MAKE_DIRECTORY ${MYDDAS_PL_OUTDIR})
      file( COPY myddas.yap DESTINATION ${YAP_INSTALL_DATADIR} )
  else()
 set (MYDDAS_PL_OUTDIR ${CMAKE_CURRENT_BINARY_DIR})
endif()

set(MYDDAS_DRIVERS
        myddas_driver.ypp
        )
set(MYDDAS_DBMS sqlite3)
set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_SQLITE3=1)
    list(APPEND MYDDAS_DBMS postgres)
if (POSTGRES_FOUND)
    set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_POSTGRES=1)
    endif()
   list(APPEND MYDDAS_DBMS odbc)
if (ODBC_FOUND)
 set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_ODBC=1)
endif()
 list(APPEND MYDDAS_DBMS mysql)
if (MYSQL_FOUND)
   set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_MYSQL=1)
        endif()

set(base_abs ${MYDDAS_PL_OUTDIR}/myddas)
    set(base_abs ${MYDDAS_PL_OUTDIR}/myddas)
     set(outfile ${base_abs}.yap)
    add_custom_command(OUTPUT  ${MYDDAS_PL_OUTDIR}/myddas.yap
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/myddas.ypp -o ${outfile}
                 DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/myddas.ypp
              )
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)

foreach (filename ${MYDDAS_YPP})
    get_filename_component(base ${filename} NAME_WE)
    set(base_abs ${MYDDAS_PL_OUTDIR}/${base})
    set(outfile ${base_abs}.yap)
    list(APPEND MYDDAS_YAP_FILES ${outfile})
    add_custom_command(OUTPUT ${outfile}
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
                 DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
              )
    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)
endforeach  ()
foreach (dbms ${MYDDAS_DBMS} )
    set(outfile ${MYDDAS_PL_OUTDIR}/myddas_${dbms}.yap)
    list(APPEND MYDDAS_YAP_FILES ${outfile})
  add_custom_command(OUTPUT ${outfile}
    COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w   ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp -D${dbms}} -o ${outfile}
            DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp
            )

    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)


endforeach()
    list(APPEND MYDDAS_YAP_FILES  ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/sqlitest.yap ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/chinook.db)

        install(FILES ${MYDDAS_PL_OUTDIR}/myddas.yap DESTINATION ${YAP_INSTALL_DATADIR})
        install(FILES ${MYDDAS_YAP_FILES} DESTINATION ${YAP_INSTALL_DATADIR}/myddas)

add_custom_target(MYDDAS_PL
        ALL
        DEPENDS ${MYDDAS_YAP_FILES} ${MYDDAS_PL_OUTDIR}/myddas.yap)
