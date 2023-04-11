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
        - Allow dataset, as input, to determine TR, # slices, # time points and
            slice pattern.  Command line options could overwrite that. JSON file
        - EPI data set (Being used in afniproc command)
        - alt-Z (alternating positive and megative z-direction)
        - Multiband (multiple slices at same point)
        - RVT without shifts
        - Variance in linear model used to assess quality (Convolve RVT with some 
            function using physiological regressors)
        - Get percentage of variance accounted for by cardio
        - Large smoothing to find peaks and troughs
        - Small smoothing to remove outliers
        - Histogram of model
        - Remove large outliers in cardio
        - Duplicate current code over all slices
        - Per slice with cardio to deal with temporal offsets across slices
        - Try weird examples from physio dB
        - Options that might change do not have default
        - Add options for Peter Lauren-written alternatives for
            - Findging peaks
            - Determining phase
            - Determining final output
        - Write alternative functions for
            - Finding peaks
            - Determining phase
            - Determining final output
            
    DONE:
        - Offset for every slice relative to TR
            - afnipy/afni_util.py:slice_pattern_to_timing() gets the actual timing
        - Implement slice pattern.  Required option 
"""

import sys
import lib_retro_opts    as lro
import lib_retro_reading as lrr
import lib_retro_proc as lrp
import lib_retro_outObj as RETO


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

    # if RET.outputInNimlFormat(physiologicalNoiseComponents, test_retro_obj):
    #     print('** ERROR outputting SliBase file')
    #     sys.exit()
    
    # if len(physiologicalNoiseComponents['resp_phases']) > 0 and\
    #     (test_retro_obj.save_graph_level > 1 or 
    #      test_retro_obj.show_graph_level > 1):
    #     status = RET.show_rvt_peak(physiologicalNoiseComponents, test_retro_obj)
    #     if status == 1:
    #         print('*** Error in retro_ts')
    #         print('Failure to show RVT peak')
    #         sys.exit()

    print("++ DONE.  Goodbye.")
