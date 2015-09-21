# File: Android.mk
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
NDK_TOOLCHAIN_VERSION := 4.8
LOCAL_SHARED_LIBRARIES := @abs_top_builddir@/libYap.a @abs_top_builddir@/yapi.o
LOCAL_MODULE    := yap_example
LOCAL_SRC_FILES := yap_wrap.cpp
LOCAL_C_INCLUDES :=  ../../../.. $(srcdir)/../../../../H $(srcdir)/../../../../include  $(srcdir)/../../../../os $(srcdir)/../../../../OPTYap $(srcdir)/../../../../BEAM $(srcdir)/../../../../CXX
LOCAL_CFLAGS := @DEFS@ -D_YAP_NOT_INSTALLED_=1
LOCAL_CPP_FEATURES := rtti
# LOCAL_ALLOW_UNDEFINED_SYMBOLS := true
include $(BUILD_SHARED_LIBRARY)
