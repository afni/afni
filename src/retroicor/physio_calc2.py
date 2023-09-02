#!/usr/bin/env python

# this program runs the newer/test command line interface for retroicor.
"""
    2022-2023 Peter Lauren, Paul Taylor
    peterdlauren@gmail.com

    "retroicorTaylor" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    "retroicorLauren2" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with "retroicorLauren".  If not, see <http://www.gnu.org/licenses/>.
    
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

from afnipy import lib_physio_opts    as lpo
from afnipy import lib_physio_reading as lpr
from afnipy import lib_physio_logs    as lpl
from afnipy import lib_physio_funcs   as lpf
from afnipy import lib_physio_regress as lpreg

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

    # save original command line opts to a log file in output dir
    tmp = lpl.save_cmd_orig(retobj)

    # ---------------------- physio-MRI timing selection ---------------------

    # Set up timing selection matrices, for slicewise regressors
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpf.calc_timing_selection_phys( retobj, label=label, verb=verb )

    # Set up timing for RVT time series
    label = 'resp'
    if retobj.data[label] :
        lpf.calc_timing_selection_rvt( retobj, label=label, verb=verb )

    # -------------------- log some of the inputs ---------------------------
    lpl.make_cmd_logs(args_dict, retobj)

    # ------------- Process any card/resp/etc. time series ------------------

    # Peak and trough estimation
    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpf.calc_time_series_peaks( retobj, label=label, verb=verb )

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
    # ... so, after this, for example:
    ### retobj.data["resp"].regress_dict_phys["c2"][4][1]
    ### -> for the 'resp' physio time series, "c2" means cos() with m=2, 
    ###    and 4 means the [4]th slice, and [1] means the actual regression
    ###    time series (the [0] in the last bracket would point to a label)

    # Regressors, for RVT time series
    label = 'resp'
    if retobj.data[label] :
        lpf.calc_regress_rvt( retobj, label=label, verb=verb )

    # ------------- Write out regressors ------------------

    lpreg.write_regressor_file(retobj)

    # -------------------- log some of the results --------------------------

    for label in lpf.PO_all_label:
        if retobj.data[label] :
            lpl.make_ts_obj_review_log( retobj, label=label, verb=verb )









#    if retobj.data['card'] != None :
#        print("TEST, print card peaks")
#        print(retobj.data['card'].peaks)



#    physiologicalNoiseComponents = lrp.getPhysiologicalNoiseComponents(test_retro_obj)
#    if len(physiologicalNoiseComponents) == 0:
#        print('*** Error in retro_ts.  Failure to get physiological ' + \
#              'noise components')
#        sys.exit()
#
#    if RETO.outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
#        print('** ERROR outputting SliBase file')
#        sys.exit()


    print("++ DONE.  Goodbye.")
