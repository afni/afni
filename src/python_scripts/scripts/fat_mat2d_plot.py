#!/usr/bin/env python

# python3 status: compatible

# Plot *.netcc and *.grid files output by 3dNetCorr and 3dTrackID,
# respectively

#ver='0.1' ; date='June 3, 2020'
# + [PT] birth
#
#ver='1.0' ; date='June 3, 2020'
# [PT] initial debugging/running/testing done
#    + ready to roll out-- others can point out bugs!
#
ver='1.1' ; date='June 4, 2020'
# [PT] unnec line (from earlier testing)
#
##########################################################################

import sys, os, copy

from afnipy import afni_base      as ab
from afnipy import lib_mat2d_base as lm2b
from afnipy import lib_mat2d_plot as lm2p


# =============================================================================

if __name__ == "__main__" : 

    iopts = lm2p.parse_args_mat_plot(sys.argv)

    ab.IP("ver : {} (libver : {})"
          "".format( ver, lm2p.ver ))
    
    # create main object with matrix info
    M = lm2b.file_grid_netcc(iopts.input)

    # Make masterlist of parameters to plot
    if not(iopts.pars_list) :
        list_of_pars = copy.deepcopy(M.allmat_labs)
    else :
        list_of_pars = []
        for p in iopts.pars_list:
            if M.allmat_labs.__contains__(p) :
                list_of_pars.append(p)
            else:
                ab.WP("Parameter '{}' does not exist in {}; "
                      "continuing with others"
                      "".format(p, iopts.input))
    if not(list_of_pars) :
        ab.EP("No parameters to print, even though input file has these:\n"
              "{}".format(', '.join(M.allmat_labs)))

    for par in list_of_pars:
        # Make plot obj
        Mplot = lm2p.plot_mat2d(M.allmat[par])

        # Apply any user-chosen options to plot obj
        Mplot.apply_iopts(iopts)

        ab.IP("Plotting: {}".format(par))
        # Apply user opts for plotting
        Mplot.make_plot() 


    sys.exit(0)
