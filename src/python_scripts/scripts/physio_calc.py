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
        - Get percentage of variance accounted for by cardio
        - Histogram of model
        - Remove large outliers in cardio
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
from afnipy import afni_base          as ab
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
    pcobj = lpr.pcalc_obj( args_dict, args_orig=args_orig )
    verb  = pcobj.verb

    # --------------------- make output directory ----------------------------

    ### !!! do more about checking for preexisting/overwrite
    if os.path.isdir(pcobj.out_dir) :
        ab.WP("output directory exists already---just reusing here.")
    else:
        ab.IP("Making output directory:\n{}".format(pcobj.out_dir))
        os.makedirs(pcobj.out_dir)

    # ... and make the supplementary subdirs for text and images
    if not(os.path.isdir(pcobj.extras_dir)) :
        os.makedirs(pcobj.extras_dir)
    if not(os.path.isdir(pcobj.images_dir)) :
        os.makedirs(pcobj.images_dir)

    # save original command line opts (and the set of parsed opts) to
    # a log file in output dir
    tmp1 = lpl.save_cmd_orig(pcobj)
    tmp2 = lpl.save_cmd_opts_parsed(pcobj)

    # ---------------------- physio-MRI timing selection ---------------------

    # Set up timing selection matrices, for slicewise regressors
    for label in lpf.PO_all_label:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            lpf.calc_timing_selection_phys( pcobj, label=label, verb=verb )

    # Set up timing for volume-based time series (RVT, HR, etc.)
    for label in ['card', 'resp']:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            lpf.calc_timing_selection_volbase( pcobj, label=label, verb=verb )

    # ------------- Process any card/resp/etc. time series ------------------

    # Peak and trough estimation: now can also be loaded in from a previous run
    for label in lpf.PO_all_label:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            # check if the peaks/troughs were loaded in already
            if not(pcobj.count_load_proc(label)) :
                # do all peak/trough processing steps
                is_fail = lpf.calc_time_series_peaks( pcobj, label=label, 
                                                      verb=verb )
                if is_fail :
                    ab.EP("peak/trough finding failure: {}".format(label))

            # see if interactive mode refinement is on
            if pcobj.data[label].do_interact :
                is_fail = lpf.run_interactive_peaks( pcobj, label=label, 
                                                     verb=verb )
                if is_fail :
                    ab.EP("peak/trough interactive failure: {}".format(label))

            # make final peak/trough images
            is_fail = lpf.make_final_image_peaks( pcobj, label=label, 
                                                  verb=verb )
            if is_fail :
                ab.WP("peak/trough final images failure: {}".format(label))


    # save/write out peaks/troughs, if user asks
    for label in lpf.PO_all_label:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            is_fail = lpl.save_peaks_troughs_file_1D( pcobj, label=label, 
                                                      verb=verb )
            if is_fail :
                ab.EP("Saving peaks/troughs failure: {}".format(label))


    # Phase estimation, which uses very diff methods for card and resp
    # processing.
    for label in lpf.PO_all_label:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            is_fail = lpf.calc_time_series_phases( pcobj, label=label, 
                                                   verb=verb )
            if is_fail :
                ab.EP("Phase estimation failure: {}".format(label))


    # RVT time series estimation (just for resp)
    label = 'resp'
    if pcobj.data[label] and pcobj.do_calc_rvt :
        is_fail = lpf.calc_time_series_rvt( pcobj, label=label, verb=verb )
        if is_fail :
            ab.EP("RVT estimation failure: {}".format(label))

    # HR time series estimation (just for card; and on EPI ts grid)
    label = 'card'
    if pcobj.data[label] and pcobj.do_calc_hr :
        is_fail = lpf.calc_time_series_hr( pcobj, label=label, verb=verb )
        if is_fail :
            ab.EP("HR estimation failure: {}".format(label))

    # ------------- Calculate regressors ------------------

    # Regressors, for all physio inputs
    for label in lpf.PO_all_label:
        if pcobj.data[label] and pcobj.do_calc_phys[label] :
            lpf.calc_regress_retroicor( pcobj, label=label, verb=verb )

    ### Comment: after this step, here is an example of the physio
    ### regressors being stored:
    # pcobj.data["resp"].regress_dict_regress["c2"][4][1]
    # -> for the 'resp' physio time series, "c2" means cos() with m=2, 
    #    and 4 means the [4]th slice, and [1] means the actual regression
    #    time series (the [0] in the last bracket would point to a label)

    # make a plot of the retroicor regressors
    tmp = lpplt.plot_regressors_retro(pcobj)

    # Resp-derived volbase regressors (plot is made within this func)
    label = 'resp'
    if pcobj.data[label] :
        # make RVT regressor 
        if pcobj.do_calc_rvt :
            is_fail = lpf.calc_regress_rvt( pcobj, label=label, verb=verb )
            if is_fail :
                ab.EP("RVT regressor estimation failure: {}".format(label))

        # make RVTRRF regressor (can only be done after RVT one is made)
        if pcobj.do_calc_rvtrrf :
            is_fail = lpf.calc_regress_rvtrrf( pcobj, label=label, verb=verb )
            if is_fail :
                ab.EP("RVTRRF estimation failure: {}".format(label))

    # Card-derived volbase regressors
    label = 'card'
    if pcobj.data[label] and pcobj.do_calc_hr :
        is_fail = lpf.calc_regress_hr( pcobj, label=label, verb=verb )
        if is_fail :
            ab.EP("HR regressor estimation failure: {}".format(label))

    # ------------- Write out regressors ------------------

    # optional (not recommended; testing only): older RetroTS.py format
    if pcobj.do_slibase_out :
        lpreg.write_regressor_file_OLD(pcobj)

    # modern output format, separate slice-based and volume-wise regressors
    is_fail = lpreg.write_regressor_file_sli(pcobj)
    if is_fail :
        ab.EP("Write slice regressor estimation failure: {}".format(label))
    is_fail = lpreg.write_regressor_file_vol(pcobj)
    if is_fail :
        ab.EP("Write volume regressor estimation failure: {}".format(label))

    # -------------------- log some of the results --------------------------

    for label in lpf.PO_all_label:
        if pcobj.data[label] :
            is_fail = lpl.make_ts_obj_review_log( pcobj, label=label, 
                                                  verb=verb )
            if is_fail :
                ab.EP("Log failure: {}".format(label))

    ab.IP("DONE.  Goodbye.")

