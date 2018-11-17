set (LIBRARY_PL
	xml_acquisition.pl
	xml_diagnosis.pl
	xml_driver.pl
	xml_generation.pl
	xml.iso.pl
	xml_pp.pl
	xml_utilities.pl
  )

set (XML_PL
	xml.pl
  )

set (LIBRARY_EX
	xml_example/bib.xml
	xml_example/books.xml
	xml_example/misc.pl
	xml_example/prices.xml
	xml_example/q1.xml
	xml_example/q2.xml
	xml_example/q3.xml
	xml_example/q4.xml
	xml_example/q5.xml
	xml_example/q6.xml
	xml_example/q7.xml
	xml_example/q8.xml
	xml_example/q9.xml
	xml_example/q10.xml
	xml_example/q11.xml
	xml_example/q12.xml
	xml_example/reviews.xml
	xml_example/xml_example.pl
  )

add_custom_target (xmlpl SOURCES ${XML_PL} ${LIBRARY_PL} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )


install(FILES ${LIBRARY_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}/xml
  )

install(FILES ${XML_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}
  )
