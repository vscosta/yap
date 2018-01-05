import nose.tools as nt

from yap_ipython.core.error import TryNext
from yap_ipython.lib.clipboard import ClipboardEmpty
from yap_ipython.testing.decorators import skip_if_no_x11

@skip_if_no_x11
def test_clipboard_get():
    # Smoketest for clipboard access - we can't easily guarantee that the
    # clipboard is accessible and has something on it, but this tries to
    # exercise the relevant code anyway.
    try:
        a = get_ipython().hooks.clipboard_get()
    except ClipboardEmpty:
        # Nothing in clipboard to get
        pass
    except TryNext:
        # No clipboard access API available
        pass
    else:
        nt.assert_is_instance(a, str)
