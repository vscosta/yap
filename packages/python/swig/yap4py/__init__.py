import imp
import os
import ctypes
import glob
import os.path
import platform
import sys

global yap_lib_path
yap_lib_path = "/home/vsc/anaconda3/conda-bld/yap4py_1522052049872/_h_env_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placehold_placeho/lib"

if platform.system() == 'Windows':
    def load( dll ):
        dll = glob.glob(os.path.join(yap_lib_path,dll))[0]
        dll = os.path.abspath(dll)
        ctypes.WinDLL(dll)
elif platform.system() == 'Darwin':
    def load( dll ):
        dll = glob.glob(os.path.join(os.path.dirname(__file__),dll))[0]
        dll = os.path.abspath(dll)
        ctypes.CDLL(dll)
        print('loaded ',dll)
        
    # try:
    #     load( '_yap*.so' )
    # except:
    #     load( '_yap*.dylib' )
else:
    def load( dll ):
        dll = glob.glob(os.path.join(os.path.dirname(__file__),dll))[0]
        dll = os.path.abspath(dll)
        ctypes.CDLL(dll)
    #load('_yap*.so')
