import imp
import os
import ctypes
import glob
import os.path
import sys
import platform

global yap_lib_path
yap_lib_path = os.path.dirname(__file__)

if platform.system() == 'Windows':
    def load( dll ):
        dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
        dll = os.path.abspath(dll)
        ctypes.WinDLL(dll)
    load('libYap*')
else:
    def load( dll ):
        dll = os.path.join(yap_lib_path,dll)
        dll = os.path.abspath(dll)
        ctypes.CDLL(dll, mode=ctypes.RTLD_GLOBAL)
    if platform.system() == 'Apple':
         load('libYap.dylib')
         load('libPy4YAP.dylib'   )
