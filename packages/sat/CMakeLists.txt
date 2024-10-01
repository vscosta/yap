set (MINISAT_SOURCES
#minisat-2.0.2/core/Main.cc
minisat-2.0.2/core/Solver.cc
#minisat-2.0.2/simp/Main.cc
minisat-2.0.2/simp/SimpSolver.cc
minisat-2.0.2/utils/Options.cc
minisat-2.0.2/utils/System.cc
  )

add_library( MINISAT
        ${MINISAT_SOURCES}
        minisat-interface.cc
        )

      target_link_libraries (MINISAT    libYap)
      
set_target_properties(MINISAT PROPERTIES CXX_STANDARD 11)
target_include_directories(MINISAT PRIVATE BEFORE minisat-2.0.2/core)
target_include_directories(MINISAT PRIVATE BEFORE minisat-2.0.2)




set_target_properties(MINISAT PROPERTIES COMPILE_FLAGS -DMINISAT=1)

set_target_properties(MINISAT PROPERTIES CXX_STANDARD 11)
set(solver minisat)
set (Solver Minisat)
configure_file(yap-interface.cc.cmake minisat-interface.cc)


find_library( CryptoMINISAT_LIBRARY cryptominisat5 )
  if (CryptoMINISAT_LIBRARY)
      set (CryptoMINISAT_FOUND On)
      set (CryptoMINISAT_ROOTDir get_filename_component(a_dir get_filename_component(a_dir  ${CryptoMINISAT_LIBRARY})))
      set(CryptoMINISAT_INCLUDE_DIRS ${CryptoMINISAT_ROOTDir}/include/cryptominisat5)


add_library( CRYPTOMINISAT
 cryptominisat-interface.cc #${CRYPTOMINISAT_SOURCES}
)

      target_link_libraries (CRYPTOMINISAT ${CryptoMINISAT_LIBRARY}  libYap)

target_include_directories(CRYPTOMINISAT PRIVATE BEFORE ${CryptoMINISAT_INCLUDE_DIRS})

target_compile_definitions(CRYPTOMINISAT PUBLIC CRYPTOMINISAT=1 register=)

set_target_properties(CRYPTOMINISAT PROPERTIES CXX_STANDARD 14)
set(solver cryptominisat)
set (Solver CMSat	)
configure_file(yap-interface.cc.cmake cryptominisat-interface.cc)
	install(TARGETS CRYPTOMINISAT
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR} )

endif()
				   
set(
  GLUCOSE_SOURCES
#  glucose-2.2/core/Main.cc
glucose-2.2/core/Solver.cc
# glucose-2.2/simp/Main.cc
  glucose-2.2/simp/SimpSolver.cc
glucose-2.2/utils/Options.cc
glucose-2.2/utils/System.cc
  )
add_library( GLUCOSE
  ${GLUCOSE_SOURCES}
        glucose-interface.cc
        )
      target_link_libraries (GLUCOSE   libYap)

set_target_properties(GLUCOSE PROPERTIES COMPILE_FLAGS -DGLUCOSE=1)
#set_target_properties(GLUCOSE PROPERTIES CXX_STANDARD 11)
target_include_directories(GLUCOSE PRIVATE glucose-2.2)
target_include_directories(GLUCOSE PRIVATE glucose-2.2/core)

set_target_properties(GLUCOSE PROPERTIES CXX_STANDARD 11)
set(solver glucose)
set (Solver Glucose)
configure_file(yap-interface.cc.cmake glucose-interface.cc)

set(
  GLUCOSE4_SOURCES
  #  glucose-4.2.1/core/Main.cc
glucose-4.2.1/core/Solver.cc
glucose-4.2.1/core/lcm.cc
# glucose-4.2.1/simp/Main.cc
glucose-4.2.1/simp/SimpSolver.cc
glucose-4.2.1/utils/Options.cc
glucose-4.2.1/utils/System.cc
  )
 add_library( GLUCOSE4
   ${GLUCOSE4_SOURCES}
   glucose4-interface.cc
   )
      target_link_libraries (GLUCOSE4   libYap)

 set_target_properties(GLUCOSE4 PROPERTIES COMPILE_FLAGS -DGLUCOSE4=1)
 set_target_properties(GLUCOSE4 PROPERTIES CXX_STANDARD 14)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4.2.1/parallel)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4.2.1)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4.2.1/core)


set_target_properties(GLUCOSE4 PROPERTIES CXX_STANDARD 11)
set(solver glucose4)
set (Solver Glucose)
configure_file(yap-interface.cc.cmake glucose4-interface.cc)

	install(TARGETS MINISAT GLUCOSE4 GLUCOSE
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR} 

    )

  INSTALL(FILES satsolver.pl cnf.pl DESTINATION ${YAP_INSTALL_DATADIR})
