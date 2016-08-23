

#BREW install for Python3


if (APPLE)
foreach (i 3.6 3.5 3.4 3.3 3.2 3.1 3.0)
  set (PYTHON_INCLUDE_DIRS /usr/local/Frameworks/Python.framework/Versions/${i}/Headers)
  message("Trying Python ${i}")
  if (EXISTS ${PYTHON_INCLUDE_DIRS})
      set (PYTHON_EXECUTABLE /usr/local/bin/python${i} CACHE FILEPATH  "Path to a program" FORCE )
    set (PYTHON_INCLUDE_DIR /usr/local/Frameworks/Python.framework/Versions/${i}/include/python${i}m
    CACHE PATH "Path to a file." FORCE   )
    set (PYTHON_LIBRARY /usr/local/Frameworks/Python.framework/Versions/${i}/lib/libpython${i}.dylib
    CACHE FILEPATH  "Path to a library" FORCE )
    break()
  endif()
endforeach()
else()
set (Python_ADDITIONAL_VERSIONS 3.6 3.5 3.4 3.3 3.2 3.1 3.0 2.8 2.6 2.5)
endif()

find_package(PythonInterp)
find_package(PythonLibs)

 macro_log_feature (PYTHONLIBS_FOUND "Python"
   "Use Python System"
   "http://www.python.org" FALSE)
