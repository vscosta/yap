

add_library (sys SHARED sys.c crypto/md5.c crypto/md5.h)

#
# this will support getting better cryptographic support,
# but right now Open SSL is not supported enough.
#
find_package (OpenSSL)
  macro_log_feature (OPENSSL_FOUND
      OpenSLL
      "Transport Layer Security (TLS) and Secure Sockets Layer (SSL) protocols"
      "https://www.openssl.org/")
if (OPENSSL_FOUND)
  include_directories (${OPENSSL_INCLUDE_DIR})
  target_link_libraries(sys ${OPENSSL_LIBRARIES})
  check_include_file( "openssl/ripemd.h" HAVE_OPENSSL_RIPEMD_H )
  check_include_file( "openssl/md5.h" HAVE_OPENSSL_MD5_H )
endif (OPENSSL_FOUND)

check_library_exists( crypt crypt "" HAVE_LIBCRYPT )
if (HAVE_LIBCRYPT)
  target_link_libraries(sys crypt)
endif (HAVE_LIBCRYPT)

target_link_libraries(sys libYap)
if (ANDROID)
target_link_libraries(sys android)
endif()

set_target_properties (sys PROPERTIES  PREFIX "")

install(TARGETS  sys
  LIBRARY DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )

configure_file ("sys_config.h.cmake" "sys_config.h" )

