"""Test embedding of yap_ipython"""

#-----------------------------------------------------------------------------
#  Copyright (C) 2013 The yap_ipython Development Team
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Imports
#-----------------------------------------------------------------------------

import os
import subprocess
import sys
import nose.tools as nt
from yap_ipython.utils.tempdir import NamedFileInTemporaryDirectory
from yap_ipython.testing.decorators import skip_win32

#-----------------------------------------------------------------------------
# Tests
#-----------------------------------------------------------------------------


_sample_embed = b"""
import yap_ipython

a = 3
b = 14
print(a, '.', b)

yap_ipython.embed()

print('bye!')
"""

_exit = b"exit\r"

def test_ipython_embed():
    """test that `yap_ipython.embed()` works"""
    with NamedFileInTemporaryDirectory('file_with_embed.py') as f:
        f.write(_sample_embed)
        f.flush()
        f.close() # otherwise msft won't be able to read the file

        # run `python file_with_embed.py`
        cmd = [sys.executable, f.name]
        env = os.environ.copy()
        env['IPY_TEST_SIMPLE_PROMPT'] = '1'

        p = subprocess.Popen(cmd, env=env, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate(_exit)
        std = out.decode('UTF-8')

        nt.assert_equal(p.returncode, 0)
        nt.assert_in('3 . 14', std)
        if os.name != 'nt':
            # TODO: Fix up our different stdout references, see issue gh-14
            nt.assert_in('yap_ipython', std)
        nt.assert_in('bye!', std)

@skip_win32
def test_nest_embed():
    """test that `yap_ipython.embed()` is nestable"""
    import pexpect
    ipy_prompt = r']:' #ansi color codes give problems matching beyond this
    env = os.environ.copy()
    env['IPY_TEST_SIMPLE_PROMPT'] = '1'


    child = pexpect.spawn(sys.executable, ['-m', 'yap_ipython', '--colors=nocolor'],
                          env=env)
    child.expect(ipy_prompt)
    child.sendline("import yap_ipython")
    child.expect(ipy_prompt)
    child.sendline("ip0 = get_ipython()")
    #enter first nested embed
    child.sendline("yap_ipython.embed()")
    #skip the banner until we get to a prompt
    try:
        prompted = -1
        while prompted != 0:
            prompted = child.expect([ipy_prompt, '\r\n'])
    except pexpect.TIMEOUT as e:
        print(e)
        #child.interact()
    child.sendline("embed1 = get_ipython()"); child.expect(ipy_prompt)
    child.sendline("print('true' if embed1 is not ip0 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline("print('true' if yap_ipython.get_ipython() is embed1 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    #enter second nested embed
    child.sendline("yap_ipython.embed()")
    #skip the banner until we get to a prompt
    try:
        prompted = -1
        while prompted != 0:
            prompted = child.expect([ipy_prompt, '\r\n'])
    except pexpect.TIMEOUT as e:
        print(e)
        #child.interact()
    child.sendline("embed2 = get_ipython()"); child.expect(ipy_prompt)
    child.sendline("print('true' if embed2 is not embed1 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline("print('true' if embed2 is yap_ipython.get_ipython() else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline('exit')
    #back at first embed
    child.expect(ipy_prompt)
    child.sendline("print('true' if get_ipython() is embed1 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline("print('true' if yap_ipython.get_ipython() is embed1 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline('exit')
    #back at launching scope
    child.expect(ipy_prompt)
    child.sendline("print('true' if get_ipython() is ip0 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline("print('true' if yap_ipython.get_ipython() is ip0 else 'false')")
    assert(child.expect(['true\r\n', 'false\r\n']) == 0)
    child.expect(ipy_prompt)
    child.sendline('exit')
    child.close()
