#!/usr/bin/env python

# translating AFNI's existing 1dplot technology to Python

author    = "PA Taylor (NIMH, NIH)"
#version   = "1.0"; date  = "Oct 24, 2018"
# + birth
#
version   = "1.2345"; date  = "Nov 1, 2018"
# + [PT] now a working beta version, including boxplots
#
# =================================================================

import sys
import os
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
