
import sysconfig
import os.path

v = sysconfig.get_python_version()
p = sysconfig.get_config_var('LIBPL')
l = sysconfig.get_config_var('LDLIBRARY')
print(os.path.join(p,l))
p = sysconfig.get_config_var('DESTDIR')
n = '../libpython3.6m.dylib'
l = os.path.join(p,n)
print(l)