set( MYDDAS_YPP
  myddas.ypp
  myddas_assert_predicates.ypp
  myddas_top_level.ypp
  myddas_errors.ypp
  myddas_prolog2sql.ypp
  myddas_mysql.ypp
  myddas_postgres.ypp
  myddas_sqlite3.ypp
  myddas_util_predicates.ypp
  myddas_prolog2sql_optimizer.ypp )

set (MYDDAS_YAP "")

function(cpp_compile output filename)
get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(outfile ${base_abs}.yap)
  set(${output} ${${output}} ${outfile} PARENT_SCOPE)
IF(MSVC)
 add_custom_command(
 OUTPUT ${outfile}
  COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS}   /EP /P ${outfile} ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
 else()
 add_custom_command(
 OUTPUT ${outfile}
  COMMAND ${CMAKE_C_COMPILER} ${MYDDAS_FLAGS} -x c -E -P -w ${CMAKE_CURRENT_SOURCE_DIR}/${filename} -o ${outfile}
DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${filename}")
ENDIF(MSVC)
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)
endfunction()


Add_custom_target (plmyddas ALL DEPENDS ${MYDDAS_YPP} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )

foreach(file ${MYDDAS_YPP})
   cpp_compile( MYDDAS_YAP ${file})
    #message("outfiles=${MYDDAS_YAP}\n")
  endforeach()


install(FILES ${MYDDAS_YAP}
  DESTINATION ${libpl}
)	
