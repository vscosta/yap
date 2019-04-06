set(MYDDAS_YPP
        myddas_core.ypp
        myddas_assert_predicates.ypp
        myddas_top_level.ypp
        myddas_errors.ypp
        myddas_prolog2sql.ypp
        myddas_util_predicates.ypp
        myddas_prolog2sql_optimizer.ypp)
if (FOUND_MYSQL)
    list(APPEND MYDDAS_YPP myddas_assert_predicates.ypp)
endif()

get_property(MYDDAS_FLAGS GLOBAL PROPERTY COMPILE_DEFINITIONS)

set(MYDDAS_DRIVERS
        myddas_driver.ypp
        )
set(MYDDAS_DBMS sqlite3)
set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_SQLITE3=1)
if (POSTGRES_FOUND)
    list(APPEND MYDDAS_DBMS postgres)
    set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_POSTGRES=1)
    endif()
if (ODBC_FOUND)
 set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_ODBC=1)
   list(APPEND MYDDAS_DBMS odbc)
endif()
if (MYSQL_FOUND)
   set(MYDDAS_FLAGS ${MYDDAS_FLAGS};-DMYDDAS_MYSQL=1)
 list(APPEND MYDDAS_DBMS mysql)
        endif()


if (ANDROID)
 set (MYDDAS_PL_OUTDIR ${CMAKE_SOURCE_DIR}/../yaplib/src/generated/assets/Yap/myddas )
 file(MAKE_DIRECTORY ${MYDDAS_PL_OUTDIR})
      file( COPY myddas.yap DESTINATION ${YAP_INSTALL_DATADIR} )
  else()
 set (MYDDAS_PL_OUTDIR ${CMAKE_CURRENT_BINARY_DIR})
 endif()
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
                COMMAND ${CMAKE_C_COMPILER} -D${dbms} -x c -E -P -w   ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp -o ${outfile}
            DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/myddas_driver.ypp
            )

    set_source_files_properties(outfile PROPERTIES GENERATED TRUE)


endforeach()
    list(APPEND MYDDAS_YAP_FILES  ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/sqlitest.yap ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/chinook.db)

        install(FILES ${MYDDAS_YAP_FILES} DESTINATION ${YAP_INSTALL_DATADIR}/myddas)
    install(FILES myddas.yap DESTINATION ${YAP_INSTALL_DATADIR})

add_custom_target(MYDDAS_PL
        ALL
        DEPENDS ${MYDDAS_YAP_FILES})