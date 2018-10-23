from warnings import warn

warn("yap_ipython.utils.daemonize has moved to ipyparallel.apps.daemonize", stacklevel=2)
from ipyparallel.apps.daemonize import daemonize
