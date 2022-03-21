# do this after all install(...) commands so that all targets are finalized. 
# Essentially, the last thing included at the end of the top-level CMakeLists.txt

set(_fmt TGZ)
if(WIN32)
  set(_fmt ZIP)
else()
  set(_fmt RPM)
endif()
set(CPACK_GENERATOR ${_fmt})
set(CPACK_SOURCE_GENERATOR ${_fmt})
set (CPACK_PACKAGE_NAME YAP)
set (CPACK_PACKAGE_VENDOR CRACS/INESC-PT)

set (CPACK_PACKAGE_VERSION_MAJOR 7)
set (CPACK_PACKAGE_VERSION_MINOR 3)
set (CPACK_PACKAGE_VERSION_PATCH 0)

set (CPACK_PACKAGE_DESCRIPTION_SUMMARY "YAP Binary")

set (CPACK_PACKAGE_DESCRIPTION_FILE "README.md")
set(CPACK_PACKAGE_VENDOR "Your Company")
set(CPACK_PACKAGE_CONTACT "Your Name")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/Artistic")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
string(TOLOWER ${CMAKE_SYSTEM_NAME} _sys)
string(TOLOWER ${PROJECT_NAME} _project_lower)
set(CPACK_PACKAGE_FILE_NAME "${_project_lower}-${_sys}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${_project_lower}-${PROJECT_VERSION}")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
# not .gitignore as its regex syntax is distinct
file(READ ${CMAKE_CURRENT_LIST_DIR}/.cpack_ignore _cpack_ignore)
string(REGEX REPLACE "\n" ";" _cpack_ignore ${_cpack_ignore})
set(CPACK_SOURCE_IGNORE_FILES "${_cpack_ignore}")

install(FILES ${CPACK_RESOURCE_FILE_README} ${CPACK_RESOURCE_FILE_LICENSE}
  DESTINATION share/docs/${PROJECT_NAME})
