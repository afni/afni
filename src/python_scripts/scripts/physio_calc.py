#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.
"""
    2022-2023 Peter Lauren, Paul Taylor
    peterdlauren@gmail.com
    
    TODO:
        - Variance in linear model used to assess quality.  (How it affects the
            variance in linear model. Ratio of variance with/without regressors)
            - Does it explain more of the variance in the data (which would be 
              a good thing) - (Permanent item)
        - Convolve RVT with some function using physiological regressors (Catie 
          Chang)
        - Get percentage of variance accounted for by cardio
        - Histogram of model
        - Remove large outliers in cardio
        - Duplicate current code over all slices
        - Per slice with cardio to deal with temporal offsets across slices
        - Try weird examples from physio dB
        - Options that might change do not have default
            
    DONE:
        - RVT without shifts (Single regressor for RVT.) 
        - Write alternative functions for
            - Finding peaks
            - Determining phase
            - Determining final output
        - Add options for Peter Lauren-written alternatives for
            - Finding peaks
            - Determining phase
            - Determining final output
        - Fix figure flashes in default mode
        - Offset for every slice relative to TR
            - afnipy/afni_util.py:slice_pattern_to_timing() gets the actual timing
        - Implement slice pattern.  Required option 
        - Allow dataset, as input, to determine TR, # slices, # time points and
            slice pattern.  Command line options could overwrite that. JSON file
        - EPI data set (Being used in afni_proc.py command)
        - Large smoothing to find peaks and troughs
        - Small smoothing to remove outliers
        - lags with RVT
"""

import sys, os
import copy

# part of AFNI imports (more below, if going beyond help viewing)
from afnipy import lib_physio_opts    as lpo

# ===========================================================================

def main():

    return 0















# ================================ main =====================================

if __name__ == "__main__":

    # ----------------------- proc cmd line opts ----------------------------

    # Leads to one of:
    # + a quick but OK exit (disp help, ver, opt list, etc.)
    # + getting a dict of checked opts for the program
    # + error exit :(
    args_orig = copy.deepcopy(sys.argv)
    args_dict = lpo.main_option_processing( sys.argv )

    # -------------------- if doing calc, import more -----------------------

    from afnipy import lib_physio_reading as lpr
    from afnipy import lib_physio_logs    as lpl
    from afnipy import lib_physio_funcs   as lpf
    from afnipy import lib_physio_regress as lpreg
    from afnipy import lib_physio_plot    as lpplt

    # --------------------- organize/check/combine ---------------------------

    # build the foundation objects: make main 'retro' object from
    # processing input options, checking to see if all necessary info
    # is present, and combining it as necessary
    retobj  = lpr.retro_obj( args_dict, args_orig=args_orig )
    verb    = retobj.verb

    # --------------------- make output directory ----------------------------

    ### !!! do more about checking for preexisting/overwrite
    if os.path.isdir(retobj.out_dir) :
        print("+* WARN: output directory exists already---just reusing here.")
    else:
        print("++ Making output directory:", retobj.out_dir)
        os.mkdir(retobj.out_dir)

    # save original command line opts (and the set of parsed opts) to
    # a log file in output dir
    tmp1 = lpl.save_cmd_orig(retobj)
    tmp2 = lpl.save_cmd_opts_parsed(retobj)

    # ---------------------- physio-MRI timing selection ---------------------

    # Set up timing selection matrices, for slicewise regressors
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpf.calc_timing_selection_phys( retobj, label=label, verb=verb )

    # Set up timing for RVT time series
    label = 'resp'
    if retobj.data[label] :
        lpf.calc_timing_selection_rvt( retobj, label=label, verb=verb )

    # ------------- Process any card/resp/etc. time series ------------------

    # Peak and trough estimation: now can also be loaded in from a previous run
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            # check if the peaks/troughs were loaded in already
            if not(retobj.count_load_proc(label)) :
                # do all peak/trough processing steps
                tmp3 = lpf.calc_time_series_peaks( retobj, label=label, 
                                                   verb=verb )
            # see if interactive mode refinement is on
            if retobj.data[label].do_interact :
                tmp4 = lpf.run_interactive_peaks( retobj, label=label, 
                                                  verb=verb )
            # make final peak/trough images
            tmp5 = lpf.make_final_image_peaks( retobj, label=label, 
                                               verb=verb )


    # save/write out peaks/troughs, if user asks
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpl.save_peaks_troughs_file_1D( retobj, label=label, verb=verb )


    # Phase estimation, which uses very diff methods for card and resp
    # processing.
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpf.calc_time_series_phases( retobj, label=label, verb=verb )

    # RVT time series estimation (prob just for resp)
    label = 'resp'
    if retobj.data[label] :
        lpf.calc_time_series_rvt( retobj, label=label, verb=verb )

    # ------------- Calculate regressors ------------------

    # Regressors, for all physio inputs
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpf.calc_regress_phys( retobj, label=label, verb=verb )

    ### Comment: after this step, here is an example of the physio
    ### regressors being stored:
    # retobj.data["resp"].regress_dict_phys["c2"][4][1]
    # -> for the 'resp' physio time series, "c2" means cos() with m=2, 
    #    and 4 means the [4]th slice, and [1] means the actual regression
    #    time series (the [0] in the last bracket would point to a label)

    # make a plot of the physio regressors
    lpplt.plot_regressors_phys(retobj)

    # Regressors, for RVT time series (plot is made within this func)
    label = 'resp'
    if retobj.data[label] :
        lpf.calc_regress_rvt( retobj, label=label, verb=verb )

    # ------------- Write out regressors ------------------

    # older style of output: everything is slicewise (not done by default)
    if retobj.do_out_slibase :
        lpreg.write_regressor_file(retobj)

    # newer, preferred style: separate slicewise and volumetric regressors
    lpreg.write_regressor_file_sli(retobj)
    lpreg.write_regressor_file_vol(retobj)

    # -------------------- log some of the results --------------------------

    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpl.make_ts_obj_review_log( retobj, label=label, verb=verb )

    print("++ DONE.  Goodbye.")
