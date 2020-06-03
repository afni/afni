#!/usr/bin/env python

# python3 status: compatible

# Plot *.netcc and *.grid files output by 3dNetCorr and 3dTrackID,
# respectively

#ver='0.1' ; date='June 3, 2020'
# + [PT] birth
#
ver='1.0' ; date='June 3, 2020'
# [PT] initial debugging/running/testing done
#    + ready to roll out-- others can point out bugs!
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

    
    mat   = 'CC'
    
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
    # Make a mask from a magn dset, if need be
    did_copy_inps = iopts.copy_inps_to_wdir()

    # Make a mask from a magn dset, if need be
    if not(iopts.dset_mask) :
        did_mask_B0 = iopts.mask_B0()

    # Do the main work
    did_B0_corr = iopts.B0_corr()

    iopts.write_params()
    iopts.write_history()

    self_vars = vars( iopts ) 
    print("\n------------")
    print("++ epi_b0_correct.py finishes.")
    print("++ Text of commands :  {ocmds_fname}"
          "".format( **self_vars ))
    print("++ Text of params   :  {opars_fname}\n"
          "".format( **self_vars ))
    if iopts.do_qc_image :
        print("++ QC images        :  {outdir}/{outdir_qc}/*.png\n"
              "".format( **self_vars ))
    print("++ MASK dset output :  {outdir}/{odset_mask}{dext}"
          "".format( **self_vars ))
    print("++ WARP dset output :  {outdir}/{odset_warp}{dext}"
          "".format( **self_vars ))
    print("++ EPI  dset output :  {outdir}/{odset_epi}{dext}\n"
          "".format( **self_vars ))

    sys.exit(0)



