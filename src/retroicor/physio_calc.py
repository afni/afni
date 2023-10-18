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
        - Lags.  Number of lags.  Direction and size
            - Add -oldRVT (current retroicorTaylor) option, and ability to flip 
               RVT direction
            - Add RVT_lags option with 3 paramaters:
                - (Relative (to onset of aqcuisition)) start time.  Beginning
                   of leftmost shift.  (Earliest time - currently -20 but is 
                   supposed) to be zero.  Currently beginnin of -20 but ends at 0
                   due to 4 intervals of 5 seconds
                - (Relative) end time
                - # RVTs
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
"""

import sys
import lib_retro_opts    as lro
import lib_retro_reading as lrr
import lib_retro_proc as lrp
import lib_retro_outObj as RETO
# import lib_retro_logs    as lpl


# ================================ main =====================================

if __name__ == "__main__":

    # Process the command line options. Leads to one of:
    # + a quick but OK exit (disp help, ver, opt list, etc.)
    # + getting a dict of checked opts for the program
    # + error exit :(
    args_dict = lro.main_option_processing( sys.argv )

    test_retro_obj = lrr.retro_obj(args_dict)
    
    physiologicalNoiseComponents = lrp.getPhysiologicalNoiseComponents(test_retro_obj)
    if len(physiologicalNoiseComponents) == 0:
        print('*** Error in retro_ts.  Failure to get physiological ' + \
              'noise components')
        sys.exit()

    if RETO.outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
        print('** ERROR outputting SliBase file')
        sys.exit()
        
    # Output logs
    # lpl.save_peaks_troughs_file_1D(test_retro_obj)

    print("++ DONE.  Goodbye.")
