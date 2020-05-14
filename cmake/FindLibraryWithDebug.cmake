# Supplies the macro FIND_LIBRARY_WITH_DEBUG()

MACRO(FIND_LIBRARY_WITH_DEBUG find_var libname paths)

  IF( "${paths}" STREQUAL "" )
    FIND_LIBRARY(${find_var}_RELEASE NAMES ${libname})
    FIND_LIBRARY(${find_var}_DEBUG NAMES ${libname}d ${libname}_d)
  ELSE( "${paths}" STREQUAL "" )
    FIND_LIBRARY(${find_var}_RELEASE NAMES ${libname} PATHS ${paths})
    FIND_LIBRARY(${find_var}_DEBUG NAMES ${libname}d ${libname}_d PATHS ${paths})
  ENDIF( "${paths}" STREQUAL "" )

  # release and debug found
  IF(${find_var}_RELEASE AND ${find_var}_DEBUG)
    SET(${find_var} optimized ${${find_var}_RELEASE} debug ${${find_var}_DEBUG} CACHE STRING "")
  ELSE(${find_var}_RELEASE AND ${find_var}_DEBUG)
     # release found
     IF(${find_var}_RELEASE)
      SET(${find_var} ${${find_var}_RELEASE})
    ELSE(${find_var}_RELEASE)
      # debug found
      IF(${find_var}_DEBUG)
        SET(${find_var} ${${find_var}_DEBUG})
      ENDIF(${find_var}_DEBUG)
    ENDIF(${find_var}_RELEASE)
  ENDIF(${find_var}_RELEASE AND ${find_var}_DEBUG)

  MARK_AS_ADVANCED(${find_var} ${find_var}_RELEASE ${find_var}_DEBUG)

ENDMACRO(FIND_LIBRARY_WITH_DEBUG)
