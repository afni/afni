#!/usr/bin/env python

# translating AFNI's existing 1dplot technology to Python

author    = "PA Taylor (NIMH, NIH)"
#version   = "1.0"; date  = "Oct 24, 2018"
# + birth
#
#version   = "1.2345"; date  = "Nov 1, 2018"
# + [PT] now a working beta version, including boxplots
#
#version   = "1.3"; date  = "Feb 27, 2019"
# + [PT] Implement Reynoldian-Reschian solution to the problem of
#   getting an interactive backend session by default on Biowulf: if
#   DISPLAY isn't detected, the set our own MPLBACKEND value, in order
#   to avoid getting an interactive one like QtAgg that would crash.
#
version   = "1.4"; date  = "Mar 20, 2019"
# + [RR] Test directly against MPLBACKEND rather than DISPLAY, so the
#   default backend will be agg, unless the user specifies otherwise.
#   DISPLAY is inherited, even if children shells no longer have access
#   to it.
# =================================================================

import sys
import os

# [PT: Feb 27, 2019] For running on Biowulf (and potentially other
# places that would have QtAgg even though there is no DISPLAY).
# Good idea (as usual), Rick!
# [RR: Mar 20, 2019] DISPLAY is insufficient, so for now,
# require use of MPLBACKEND, one way or another.
try:
    os.environ['MPLBACKEND']
except:
    os.environ['MPLBACKEND'] = 'agg'

import lib_afni1D as LAD
import lib_apqc_io as laio
import lib_plot_1D as lpod



if __name__ == "__main__":

    iopts   = laio.parse_1dplot_args(sys.argv[1:])

    # after the inputs have been read in, use this to: arrange/combine
    # lists and labels; check that lengths of lists and numbers of
    # labels match; and sort out some other things, too, like the
    # color table.
    ok_arrs = lpod.populate_1dplot_arrays(iopts)

    bigfig  = lpod.populate_1dplot_fig(iopts)   
    ok_fig  = lpod.make_1dplot_figure(bigfig)
    sys.exit(0)
