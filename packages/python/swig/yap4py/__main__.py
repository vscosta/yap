"""The main routine of the yap python project."""

import sys


def main(**args):
    """The main routine."""
    if args is None:
        args = sys.argv[1:]

if __name__ == "__main__":
    import yap4py.yapi
    main()
    yap4py.yapi.main()
