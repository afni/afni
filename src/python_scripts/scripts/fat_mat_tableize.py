#!/usr/bin/env python

# python3 status: compatible

# Stuff for taking 2d matrices and making tables for AFNI group
# analysis programs like 3dMVM, MBA, etc.
#
# In particular, these funcs are for interacting with 3dNetCorr and
# 3dTrackID outputs.

ver='0.0' ; date='June 8, 2020'
# + [PT] birth
#
##########################################################################

import sys, os, copy

from afnipy import afni_base          as ab
from afnipy import afni_util          as UTIL
from afnipy import lib_mat2d_base     as lm2b
from afnipy import lib_mat2d_tableize as lm2t
from afnipy import lib_csv            as LCSV


# =============================================================================

if __name__ == "__main__" : 

    iopts = lm2t.parse_args(sys.argv)

    ab.IP("ver : {} (libver : {})"
          "".format( ver, lm2t.ver ))
    

    # ---------- read in CSV, if input
    if iopts.in_csv :
        csv = LCSV.csv_data(iopts.in_csv)
        csv_ids = csv.get_table_col_by_idx(0) # [0] col is subj IDs
        # Now, csv.table has the data (all str); csv.header has the
        # col labels

    # ---------- read in matrix files
    # initialize main obj of matrices
    multi_mat = lm2b.multi_file_GoN()        

    if iopts.in_listfile :
        list_mat, list_ids = lm2t.read_in_listfile(iopts.in_listfile)

        for fname in list_mat:
            file_mat = lm2b.file_grid_netcc(fname)
            multi_mat.add_file(file_mat)   

    elif iopts.in_mat :
        list_ids = []    # this simplifies coding later

        for fname in iopts.in_mat:
            file_mat = lm2b.file_grid_netcc(fname)
            multi_mat.add_file(file_mat)

    else:
        # should not get here
        ab.EP("No listfile or list of matrices entered??")

    # ------------- match matfile to CSV (if latter is used)
    if csv :
        list_fnames = list(multi_mat.all_fname.values())  # list of vals
        dict_all_fname_inv = UTIL.invert_dict(multi_mat.all_fname) # dict

        # now, we have to make a dict whose keys are the indices
        # of the csv_ids, and whos values are the indices of the
        # filename dictionary of matrices
        dict_ids_fidx = {}

        if list_ids :
            # the matching was provided in a file by the user
            for ii in range(len(list_ids)):
                ii_id = list_ids[ii]
                ii_mat = list_mat[ii]
                dict_ids_fidx[csv_ids.index(ii_id)] \
                    = dict_all_fname_inv[ii_mat]
        else:
            # have to find our own matching from file names
            FULL_MATCH, da, db \
                = UTIL.match_listA_str_in_listB_str(csv_ids, 
                                                    list_fnames)
            for k in da:
                dict_ids_fidx[k] = dict_all_fname_inv[list_fnames[da[k]]]

        # At this point, the dict_ids_fidx.keys() are the indices in
        # list_ids, and dict_ids_fidx.values() are the index/keys in
        # multi_mat.all_fname

    ## else: well, we don't have to do anything else here if no
    ## csv were supplied













    sys.exit(0)

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
