import sys
import warnings

from yap_ipython.utils.shimmodule import ShimWarning


def test_shim_warning():
    sys.modules.pop('yap_ipython.config', None)
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        import yap_ipython.config
    assert len(w) == 1
    assert issubclass(w[-1].category, ShimWarning)
