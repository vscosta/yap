#CHECK: CUDA
macro_optional_find_package (CUDA ON)
macro_log_feature (CUDA_FOUND "CUDA" 
  "CUDA GGPU Programming " 
  "http://www.r.org" FALSE)
if (CUDA_FOUND)
  #   CUDA_VERSION_MAJOR    -- The major version of cuda as reported by nvcc.
  #   CUDA_VERSION_MINOR    -- The minor version.
  #   CUDA_VERSION
  #   CUDA_VERSION_STRING   -- CUDA_VERSION_MAJOR.CUDA_VERSION_MINOR
  #
  #   CUDA_TOOLKIT_ROOT_DIR -- Path to the CUDA Toolkit (defined if not set).
  #   CUDA_SDK_ROOT_DIR     -- Path to the CUDA SDK.  Use this to find files in the
  #                            SDK.  This script will not directly support finding
  #                            specific libraries or headers, as that isn't
  #                            supported by NVIDIA.  If you want to change
  #                            libraries when the path changes see the
  #                            FindCUDA.cmake script for an example of how to clear
  #                            these variables.  There are also examples of how to
  #                            use the CUDA_SDK_ROOT_DIR to locate headers or
  #                            libraries, if you so choose (at your own risk).
  #   CUDA_INCLUDE_DIRS     -- Include directory for cuda headers.  Added automatically
  #                            for CUDA_ADD_EXECUTABLE and CUDA_ADD_LIBRARY.
  #   CUDA_LIBRARIES        -- Cuda RT library.
  #   CUDA_CUFFT_LIBRARIES  -- Device or emulation library for the Cuda FFT
  #                            implementation (alternative to:
  #                            CUDA_ADD_CUFFT_TO_TARGET macro)
  #   CUDA_CUBLAS_LIBRARIES -- Device or emulation library for the Cuda BLAS
  #                            implementation (alterative to:
  #                            CUDA_ADD_CUBLAS_TO_TARGET macro).
  #   CUDA_cupti_LIBRARY    -- CUDA Profiling Tools Interface library.
  #                            Only available for CUDA version 4.0+.
  #   CUDA_curand_LIBRARY   -- CUDA Random Number Generation library.
  #                            Only available for CUDA version 3.2+.
  #   CUDA_cusparse_LIBRARY -- CUDA Sparse Matrix library.
  #                            Only available for CUDA version 3.2+.
  #   CUDA_npp_LIBRARY      -- NVIDIA Performance Primitives lib.
  #                            Only available for CUDA version 4.0+.
  #   CUDA_nppc_LIBRARY     -- NVIDIA Performance Primitives lib (core).
  #                            Only available for CUDA version 5.5+.
  #   CUDA_nppi_LIBRARY     -- NVIDIA Performance Primitives lib (image processing).
  #                            Only available for CUDA version 5.5+.
  #   CUDA_npps_LIBRARY     -- NVIDIA Performance Primitives lib (signal processing).
  #                            Only available for CUDA version 5.5+.
  #   CUDA_nvcuvenc_LIBRARY -- CUDA Video Encoder library.
  #                            Only available for CUDA version 3.2+.
  #                            Windows only.
  #   CUDA_nvcuvid_LIBRARY  -- CUDA Video Decoder library.
  #                            Only available for CUDA version 3.2+.
  #                            Windows only.
endif (CUDA_FOUND)

