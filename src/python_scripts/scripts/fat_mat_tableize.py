#!/usr/bin/env python

# python3 status: compatible

# Stuff for taking 2d matrices and making tables for AFNI group
# analysis programs like 3dMVM, MBA, etc.
#
# In particular, these funcs are for interacting with 3dNetCorr and
# 3dTrackID outputs.
#
# -----------------------------------------------------------------------
#ver='0.0' ; date='June 8, 2020'
# + [PT] birth
#
ver = '0.1' ; date = 'June 9, 2020'
# [PT] work toward storing mat info and choosing ROIs
#    + merge with CSV better
#    + new obj to store mat info (mat_info)
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

    # ------------------------- read in inputs -------------------------

    # read in MATRIX FILES (req in one of 2 forms)

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

    # this obj contains a lot of info summarizing the matrices
    mat_info = lm2t.mat_table_guide(multi_mat)

    # Couple last matrix-related things for later: list of mat
    # filenames, and inverse dictionary of fnames
    list_fnames = list(multi_mat.all_fname.values())
    dict_all_fname_inv = UTIL.invert_dict(multi_mat.all_fname) 

    # read in CSV (opt)

    if iopts.in_csv :
        csv = LCSV.csv_data(iopts.in_csv)
        csv_ids = csv.get_table_col_by_idx(0) # [0] col is subj IDs
        # Now, csv.table has the data (all str); csv.header has the
        # col labels


    # --------- match matfile to CSV subj IDs (if latter is used) -------

    if iopts.in_csv :
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
            # have to find our own matching from file names and subj IDs
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


    # -------------- select matrices (= pars) to include ---------------


    mat_info.set_pars_final( iopts.pars_list,
                             empty_one_ok=True, 
                             exit_on_empty=False )
    if not(mat_info.npar) :
        ab.EP("No parameters with which to build table?  Can't proceed.")


    # ------------------ select ROIs/elements to include ----------------

    # That is, here we need to make a mask for a matrix.  There are
    # many ways this can be done; for 3dMVM, we only took elements
    # that were nonzero across all subj, but we no longer need to be
    # that strict.  Additionally, users can select ROI-pairs or
    # col|row selectors for the mask.
    # 
    # For *.grid files, the 'NT' matrix of integers is the best one to
    # use if one needs to find 'nonzero elements across a group'.  
    # 
    # For *.netcc files, there really isn't one to choose, since
    # correlation values *can* be zero.
    # 
    # Note also that in general we will exclude diagonals, for both
    # *.grid and *.netcc; in the latter, they are trivial, but even in
    # the former where they have sooome meaning, they don't represent
    # between-ROI connections, and hence we typically ignore them.

    if 1 :    # !! put opt here later, based on iopt.*/input
        mat_info.remove_diags_from_mask()






    # ---------------------- build table ------------------------------











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
