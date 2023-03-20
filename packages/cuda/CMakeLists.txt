#CHECK: CUDA

set( CMAKE_CXX_STANDARD 14)
set(CMAKE_CUDA_ARCHITECTURES all)
include(CheckLanguage)
enable_language(CUDA)

  set (CUDA_SOURCES
src/CC_CSSTree.cu
src/bpreds.cu
src/dbio.cu
src/lista.cu
src/memory.cu
src/selectproyect.cu
src/treeb.cu
src/union2.cu
src/src/    )

  set (CXX_SOURCES
cpu/bpredscpu.cpp
cpu/joincpu.cpp
cpu/selectproyectcpu.cpp
cpu/unioncpu2.cpp
)

  set (C_SOURCES
src/creator2.c
cuda.c
)

  set (PL_SOURCES
    cuda.yap
    )

 add_library(YAPcuda   ${CUDA_SOURCES} ${C_SOURCES})

  target_link_libraries(YAPcuda  libYap
    ${CUDA_LIBRARIES} 
  )


  #set(BUILD_SHARED_LIBS OFF)

  set(CUDA_SEPARABLE_COMPILATION ON)

  #list(APPEND CUDA_NVCC_FLAGS -arch=sm_20)

  include_directories (${CUDA_TOOKIT_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/cpu
        )

  install(TARGETS  YAPcuda
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
    )

  install(FILES ${PL_SOURCES}
    DESTINATION ${YAP_INSTALL_DATADIR}
    )


 
