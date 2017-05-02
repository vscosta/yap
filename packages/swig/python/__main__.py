"""The main routine of the yap python project."""

import sys
import yapi


def main(args=None):
    """The main routine."""
    if args is None:
        args = sys.argv[1:]
    yap.yapi.live(args)
    
if __name__ == "__main__":
    main()
