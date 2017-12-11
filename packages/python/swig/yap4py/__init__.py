import imp
import os
import ctypes
import glob
import os.path
import platform
import sys

global yap_lib_path
yap_lib_path = os.path.dirname(__file__)

if platform.system() == 'Windows':
    def load( dll ):
        dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
        dll = os.path.abspath(dll)
        ctypes.WinDLL(dll)
elseif  platform.system() == 'Apple':
    def load( dll ):
          dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
          dll = os.path.abspath(dll)
          ctypes.CDLL(dll)
    load('libYap.dylib')
    load('libPy4YAP.dylib')
else:
    def load( dll ):
          dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
          dll = os.path.abspath(dll)
          ctypes.CDLL(dll)
    load('libYap.so')
    load('libPy4YAP.so')
    