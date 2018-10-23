import imp
import os
import ctypes
import glob
import os.path
import sys

global yap_lib_path
yap_lib_path = os.path.dirname(__file__)

            
def load( dll ):
    dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
    dll = os.path.abspath(dll)
    ctypes.CDLL(dll, mode=ctypes.RTLD_GLOBAL)

load('libYap*')
