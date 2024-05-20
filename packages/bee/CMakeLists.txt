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

      target_link_libraries (MINISAT   libYap)
      
set_target_properties(MINISAT PROPERTIES CXX_STANDARD 11)
target_include_directories(MINISAT PRIVATE BEFORE minisat-2.0.2/core)
target_include_directories(MINISAT PRIVATE BEFORE minisat-2.0.2)

set_target_properties(MINISAT PROPERTIES COMPILE_FLAGS -DMINISAT=1)

set_target_properties(MINISAT PROPERTIES CXX_STANDARD 11)
set(solver minisat)
set (Solver Minisat)
configure_file(yap-interface.cc.cmake minisat-interface.cc)

if (off)
  set(
    CRYPTOMINISAT_SOURCES
    cryptominisat-2.5.1/Solver/Clause.cpp
cryptominisat-2.5.1/Solver/ClauseCleaner.cpp
cryptominisat-2.5.1/Solver/FailedVarSearcher.cpp
cryptominisat-2.5.1/Solver/FindUndef.cpp
cryptominisat-2.5.1/Solver/Gaussian.cpp
cryptominisat-2.5.1/Solver/Logger.cpp
cryptominisat-2.5.1/Solver/MatrixFinder.cpp
cryptominisat-2.5.1/Solver/PackedRow.cpp
cryptominisat-2.5.1/Solver/PartFinder.cpp
cryptominisat-2.5.1/Solver/PartHandler.cpp
cryptominisat-2.5.1/Solver/RestartTypeChooser.cpp
cryptominisat-2.5.1/Solver/SmallPtr.cpp
cryptominisat-2.5.1/Solver/Solver.cpp
cryptominisat-2.5.1/Solver/StateSaver.cpp
cryptominisat-2.5.1/Solver/Subsumer.cpp
cryptominisat-2.5.1/Solver/VarReplacer.cpp
cryptominisat-2.5.1/Solver/XorFinder.cpp
cryptominisat-2.5.1/Solver/XorSubsumer.cpp

    )
add_library( CRYPTOMINISAT
        ${CRYPTOMINISAT_SOURCES}
        cryptominisat-interface.cc
        )
      target_link_libraries (CRYPTOMINISAT   libYap)

# target_include_directories(GLUCOSE4 PRIVATE glucose-4/parallel)
target_include_directories(CRYPTOMINISAT PRIVATE BEFORE cryptominisat-2.5.1/MTRand)
target_include_directories(CRYPTOMINISAT PRIVATE BEFORE cryptominisat-2.5.1/mtl)
target_include_directories(CRYPTOMINISAT PRIVATE BEFORE cryptominisat-2.5.1/Solver)

target_compile_definitions(CRYPTOMINISAT PUBLIC CRYPTOMINISAT=1 register=)

# set_target_properties(CRYPTOMINISAT PROPERTIES CXX_STANDARD 11)
set(solver cryptominisat)
set (Solver Cryptominisat)
configure_file(yap-interface.cc.cmake cryptominisat-interface.cc)

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
#  glucose-4/core/Main.cc
glucose-4/core/Solver.cc
# glucose-4/simp/Main.cc
glucose-4/simp/SimpSolver.cc
glucose-4/utils/Options.cc
glucose-4/utils/System.cc
  )
 add_library( GLUCOSE4
   ${GLUCOSE4_SOURCES}
        glucose4-interface.cc
   )

 set_target_properties(GLUCOSE4 PROPERTIES COMPILE_FLAGS -DGLUCOSE4=1)
 set_target_properties(GLUCOSE4 PROPERTIES CXX_STANDARD 11)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4/parallel)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4)
 target_include_directories(GLUCOSE4 PRIVATE glucose-4/core)


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
