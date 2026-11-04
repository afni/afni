#!/usr/bin/env python

"""Align EPI and anatomical datasets using the existing AFNI workflow."""

import sys

from afnipy import lib_aea_opts as LAO

# ===========================================================================

def main(argv=None):
    """Parse/check inputs, run alignment, and return (status,
processing object).

argv includes the program name and defaults to sys.argv.  Return None
for the object on help/version requests or option errors.  Preserve the
original CLI messages and exit status; load processing only after inputs
pass the option checks.

    """

    inobj = LAO.InOpts(argv=argv)
    inobj.version()

    rv = inobj.process_options()
    if rv > 0:
        return 0, None
    if rv < 0:
        return inobj.status, None

    if inobj.check_options():
        return inobj.status, None

    # later import here
    from afnipy import lib_aea_base as LAB

    mainobj = LAB.MainObj(user_inobj=inobj, args_orig=inobj.args_orig)
    return mainobj.status, mainobj

# ===========================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)
