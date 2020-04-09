
#cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)

  set ( MINISAT2_HEADERS
Alg.h
BasicHeap.h
BoxedVec.h
Heap.h
Map.h
Queue.h
Solver.h
SolverTypes.h
Sort.h
Vec.h
)

set ( MINISAT2_SOURCES
Solver.C
pl-minisat.C
)

 
  INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}
    )

    ADD_LIB(minisat2  ${MINISAT2_SOURCES} ${MINISAT2_HEADERS} )

    set_target_properties (minisat2 PROPERTIES OUTPUT_NAME pl-minisat)
    set_target_properties (minisat2 PROPERTIES PREFIX   "")

    
   if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(minisat2
      libYap
      )
  else()
    ADD_LIBRARY(minisat2 SHARED ${MINISAT2_SOURCES} )
  endif()

  #set_property(TARGET minisat2 PROPERTY CXX_STANDARD 11)
  #set_property(TARGET minisat2 PROPERTY CXX_STANDARD_REQUIRED ON)

  install (
    TARGETS minisat2
    RUNTIME DESTINATION ${CMAKE_INSTALL_BIINDIR}
    ARCHIVE  DESTINATION ${YAP_INSTALL_DLLDIR}
    LIBRARY  DESTINATION ${YAP_INSTALL_DLLDIR}
  )


