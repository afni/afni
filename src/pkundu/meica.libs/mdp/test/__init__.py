import os
SCRIPT="run_tests.py"
from mdp.configuration import _version_too_old

def test(filename=None, keyword=None, seed=None, options='', mod_loc=None,
         script_loc=None):
    """Run tests.

       filename -- only run tests in filename. If not set run all tests.
                   You do not need the full path, the relative path within the
                   test directory is enough.

       keyword  -- only run test items matching the given space separated
                   keywords.  precede a keyword with '-' to negate.
                   Terminate the expression with ':' to treat a match as
                   a signal to run all subsequent tests.

       seed     -- set random seed

       options  -- options to be passed to the underlaying py.test script (as a string) 

       mod_loc  -- don't use it, it's for internal usage

       script_loc  -- don't use it, it's for internal usage
    """
    if mod_loc is None:
        mod_loc = os.path.dirname(__file__)
    if script_loc is None:
        script_loc = os.path.dirname(__file__)
    if filename is None:
        loc = mod_loc
    else:
        loc = os.path.join(mod_loc, os.path.basename(filename))
    args = []
    if keyword is not None:
        args.extend(('-k', str(keyword)))
    if seed is not None:
        args.extend(('--seed', str(seed)))
    # add --assert=reiterp option to work around permissions problem
    # with __pycache__ directory when MDP is installed on a normal
    # user non-writable directory
    options = "--assert=reinterp "+options
    args.extend(options.split())
        
    args.append(loc)
    _worker = get_worker(script_loc)
    return _worker(args)

def subtest(script, args):
    # run the auto-generated script in a subprocess"
    import subprocess
    import sys
    subtest = subprocess.Popen([sys.executable,script]+args, stdout = sys.stdout,
                               stderr = sys.stderr)
    # wait for the subprocess to finish before returning the prompt
    subtest.wait()
    # ??? do we want to catch KeyboardInterrupt and send it to the
    # ??? subprocess?

def get_worker(loc):
    try:
        # use py.test module interface if it's installed
        import py.test
        # check that we have at least version 2.1.2
        try:
            py.test.__version__
        except AttributeError:
            raise ImportError
        if _version_too_old(py.test.__version__, (2,1,2)):
            raise ImportError
        else:
            return py.test.cmdline.main
    except ImportError:
        # try to locate the script
        script = os.path.join(loc, SCRIPT)
        if os.path.exists(script):
            return lambda args: subtest(script, args)
        else:
            raise Exception('Could not find self-contained py.test script in'
                            '"%s"'%script)
