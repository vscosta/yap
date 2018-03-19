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
        "myddas_driver.ypp"
        )


        if (ANDROID)
    set (MYDDAS_PREFIX ${libpl} )
else()
    set (MYDDAS_PREFIX ${CMAKE_CURRENT_BINARY_DIR} )
endif()

get_property(MYDDAS_FLAGS GLOBAL PROPERTY COMPILE_DEFINITIONS)

function(cpp_compile output filename)
    get_filename_component(base ${filename} NAME_WE)
    set(base_abs ${MYDDAS_PREFIX}/${base})
    set(outfile ${base_abs}.yap)
     set(${output} ${${output}} ${outfile} PARENT_SCOPE)
            IF (MSVC)
        add_custom_command(
                OUTPUT ${outfile}
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} /EP /P ${outfile} ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
                DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
    ELSE (MSVC)
        add_custom_command(
                OUTPUT ${outfile}
                COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
                DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
    ENDIF (MSVC)
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)
endfunction()

if (ANDROID)
set (MYDDAS_PL_OUTDIR ${YAP_APP_DIR}/src/generated/assets/Yap )
else()
set (MYDDAS_PL_OUTDIR ${CMAKE_CURRENT_BINARY_DIR} )
endif()

function(cpp_driver output dbms filename)
    set(outfile ${MYDDAS_PL_OUTDIR}/myddas_${dbms}.yap)
    set(${output} ${${output}} ${outfile} PARENT_SCOPE)
                   IF (MSVC)
		    add_custom_command(
                OUTPUT ${outfile}
                COMMAND ${CMAKE_C_COMPILER} -D${dbms} /EP /P ${outfile} ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
                DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
    else ()
        add_custom_command(
                OUTPUT ${outfile}
                COMMAND ${CMAKE_C_COMPILER} -D${dbms} -x c -E -P -w   ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
                DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
	    ENDIF (MSVC)
	    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)
endfunction()

set(MYDDAS_DBMS sqlite3 postgres odbc)

foreach (file ${MYDDAS_YPP})
    cpp_compile(MYDDAS_YAP ${file})
endforeach ()
foreach (driver ${MYDDAS_DBMS})
    cpp_driver(MYDDAS_YAP ${driver} myddas_driver.ypp)
endforeach ()

list (APPEND MYDDAS_YAP ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/test.yap ${CMAKE_CURRENT_SOURCE_DIR}/../sqlite3/chinook.db)
add_to_group(MYDDAS_YAP pl_library )

add_custom_target(plmyddas ALL DEPENDS  ${MYDDAS_YAP} )

    install(FILES ${MYDDAS_YAP}
     DESTINATION ${libpl}
        )
