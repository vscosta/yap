if (WITH_MINISAT)

add_library( YAPMinisat minisat-interface.cc
        )
      target_link_libraries ( YAPMinisat minisat  libYap)
      
set_target_properties(MINISAT PROPERTIES CXX_STANDARD 11)

endif()

#cryptominisat5 support

if( WITH_CryptoMINISAT )

find_library( CryptoMINISAT_LIBRARY cryptominisat cryptominisat5 )
  if (CryptoMINISAT_LIBRARY)
      set (CryptoMINISAT_FOUND On)
endif()

target_link_libraries ( YAPCryptominisat cryptominisat   libYap)

set_target_properties(YAPCryptominisat PROPERTIES CXX_STANDARD 14)
set(solver cryptominisat)
set (Solver CMSat	)
configure_file(yap-interface.cc.cmake cryptominisat-interface.cc)
	instalal(TARGETS CRYPTOMINISAT
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR} )

endif()

if( WITH_Glucose4 )

find_library( Glucose4_LIBRARY glucose4 )
  if (Glucose4_LIBRARY)
      set (Glucose4_FOUND On)
endif()

if (Glucose4_FOUND)
add_library( YAPGlucose4
 glucose4-interface.cc 
)

target_link_libraries (glucose4   libYap)

target_include_directories(GLUCOSE4 PRIVATE BEFORE ${Glucose4_INCLUDE_DIRS})

target_compile_definitions(GLUCOSE4 PUBLIC GLUCOSE4=1)

set_target_properties(YAPCglucose4 PROPERTIES CXX_STANDARD 14)
set(solver glucose4)
set (Solver CMSat	)
configure_file(yap-interface.cc.cmake glucose4-interface.cc)
	instalal(TARGETS GLUCOSE4
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR} )

find_library( Glucose4_LIBRARY glucose4 glucose45 )
  if (Glucose4_LIBRARY)
      set (Glucose4_FOUND On)
endif()



endif()

  INSTALL(FILES satsolver.pl cnf.pl DESTINATION ${YAP_INSTALL_DATADIR})
endif()
