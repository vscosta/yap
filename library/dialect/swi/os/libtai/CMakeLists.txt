set(LIBYAPTAI_MAJOR_VERSION 0)
set(LIBYAPTAI_MINOR_VERSION 60)
set(LIBYAPTAI_PATCH_VERSION 0)
set(LIBYAPTAI_FULL_VERSION
  ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}.${LIBYAPTAI_PATCH_VERSION})

set(LIBYAPTAI_SOURCES
  tai_add.c
  tai_now.c
  tai_pack.c
  tai_sub.c
  tai_unpack.c
  taia_add.c
  taia_approx.c
  taia_fmtfrac.c
  taia_frac.c
  taia_half.c
  taia_less.c
  taia_now.c
  taia_pack.c
  taia_sub.c
  taia_tai.c
  taia_unpack.c
  caldate_fmt.c
  caldate_scan.c
  caldate_fmjd.c
  caldate_mjd.c
  caldate_norm.c
  caldate_ster.c
  leapsecs_read.c
  leapsecs_init.c
  leapsecs_add.c
  leapsecs_sub.c
  caltime_fmt.c
  caltime_scan.c
  caltime_tai.c
  caltime_utc.c
  )

# ugh
set (POSITION_INDEPENDENT_CODE TRUE)

add_library (libyaptai  OBJECT
  ${LIBYAPTAI_SOURCES})

set_target_properties(libyaptai
  PROPERTIES
  #  RPATH ${CMAKE_INSTALL_LIBDIR} VERSION ${LIBYAPTAI_FULL_VERSION}
  #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME yaptai
  )



