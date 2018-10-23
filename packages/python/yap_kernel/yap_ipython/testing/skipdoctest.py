"""Decorators marks that a doctest should be skipped.

The yap_ipython.testing.decorators module triggers various extra imports, including
numpy and sympy if they're present. Since this decorator is used in core parts
of yap_ipython, it's in a separate module so that running yap_ipython doesn't trigger
those imports."""

# Copyright (C) yap_ipython Development Team
# Distributed under the terms of the Modified BSD License.


def skip_doctest(f):
    """Decorator - mark a function or method for skipping its doctest.

    This decorator allows you to mark a function whose docstring you wish to
    omit from testing, while preserving the docstring for introspection, help,
    etc."""
    f.skip_doctest = True
    return f
