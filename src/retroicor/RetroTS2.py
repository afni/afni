#!/usr/bin/env python

# python3 status: compatible

# coding=utf-8
__author__ = "Joshua Zosky" # Modified a bit by gianfranco 

"""
    Copyright 2015 Joshua Zosky
    joshua.e.zosky@gmail.com

    This file is part of "RetroTS".
    "RetroTS" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    "RetroTS" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with "RetroTS".  If not, see <http://www.gnu.org/licenses/>.
"""

import gzip
import json
from numpy import zeros, size, savetxt, column_stack, shape, array
import retroicor
from retroicor import phase_estimator
from retroicor import rvt_from_peakfinder
from retroicor import peak_finder
from retroicor import readRawInputData
from lib_RetroTS.Show_RVT_Peak import show_rvt_peak

def setup_exceptionhook():
    """
    Overloads default sys.excepthook with our exceptionhook handler.
    If interactive, our exceptionhook handler will invoke pdb.post_mortem;
    if not interactive, then invokes default handler.
    """

    def _pdb_excepthook(type, value, tb):
        if sys.stdin.isatty() and sys.stdout.isatty() and sys.stderr.isatty():
            import traceback
            import pdb

            traceback.print_exception(type, value, tb)
            # print()
            pdb.post_mortem(tb)
        else:
            print("We cannot setup exception hook since not in interactive mode")

    sys.excepthook = _pdb_excepthook
    
def getSliceOffsets(offsetDict):
    slice_offset = offsetDict["slice_offset"]
    
    # Determining slice_offset based upon slice_order, volume_tr,
    #  and number_of_slices.
    tt = 0.0  # Default float value to start iterations
    dtt = float(offsetDict["volume_tr"]) / float(
        offsetDict["number_of_slices"]
    )  # Increments for iteration
    # init slice_offsets, unless Custom order
    # (noted by Jogi Ho on board   27 Dec 2017 [rickr])
    if (
        (offsetDict["slice_order"] not in ["Custom", "custom"])
        or len(slice_offset) != offsetDict["number_of_slices"]
    ):
        slice_offsets = [0] * offsetDict[
            "number_of_slices"
        ]  # Initial value for slice_offset
    slice_file_list = (
        []
    )  # List for using external file for slice_offset values/
    # Indicates if using external file in last loop
    if offsetDict["slice_order"][0:3] == "alt":  # Alternating?
        for i in range(0, offsetDict["number_of_slices"], 2):
            slice_offsets[i] = tt
            tt += dtt
        for i in range(1, offsetDict["number_of_slices"], 2):
            slice_offsets[i] = tt
            tt += dtt
    elif offsetDict["slice_order"][0:3] == "seq":  # Sequential?
        for i in range(0, offsetDict["number_of_slices"]):
            slice_offsets[i] = tt
            tt += dtt
    elif offsetDict["slice_order"] in ["Custom", "custom"] \
        and type(slice_offset) == str:

        # If slice_order is custom, parse from slice_offset string.
        # Allow simple or pythonic array form.   1 Dec 2020 [rickr]
        try:
           offlist = eval(slice_offset)
           # noff = len(offlist)
        except:
           try:
              offlist = [float(v) for v in slice_offset.split()]
           except:
              print("** failed to apply custom slice timing from: %s" \
                    % slice_offset)
              return
        if len(offlist) != offsetDict["number_of_slices"]:
           print("** error: slice_offset len = %d, but %d slices" \
              % (len(offlist), offsetDict["number_of_slices"]))
           return

        # success, report and apply
        print("applying custom slice timing, min = %g, max = %g" \
              % (min(offlist), max(offlist)))
        slice_offset = offlist
        slice_offsets = offlist

    else:  # Open external file specified in argument line,
        # fill SliceFileList with values, then load into slice_offset
        with open(offsetDict["slice_order"], "r") as f:
            for i in f.readlines():
                # read times, in seconds
                slice_file_list.append(float(i))

            # Check that slice acquisition times match the number of slices
            if len(slice_file_list) != offsetDict["number_of_slices"]:
                print("Could not read enough slice offsets from file")
                print("File should have as many offsets as number_of_slices")
                quit()
            slice_offsets = slice_file_list
    if (
        offsetDict["slice_order"][3] == "-" and slice_file_list == []
    ):  # Check for a minus to indicate
        #  a reversed offset list
        slice_offsets.reverse()
    if (
        offsetDict["quiet"] != 1
    ):  # Show the slice timing (P.S. Printing is very time consuming in python)
        print("Slice timing: %s" % slice_offsets)
        
    return slice_offsets

def getInputFileParameters(respiration_info, cardiac_info, phys_file,\
                        phys_json_arg, respiration_out, cardiac_out, rvt_out):
    
    # Handle file inputs
    # BIDS = Brain Imaging Data Structure
    if (((phys_file is not None) and (respiration_info["respiration_file"] is not None))
        or ((phys_file is not None) and (cardiac_info["cardiac_file"] is not None))):
        raise ValueError('You should not pass a BIDS style phsyio file'
                         ' and respiration or cardiac files.')
    # Get the peaks for respiration_info and cardiac_info
    # init dicts, may need -cardiac_out 0, for example   [16 Nov 2021 rickr]
    if phys_file:
        # Use json reader to read file data into phys_meta
        with open(phys_json_arg, 'rt') as h:
            phys_meta = json.load(h)
        # phys_ending is last element following a period
        phys_ending = phys_file.split(".")[-1]
        
        # Choose file opening function on the basis of whether file is gzippped
        if phys_ending == 'gz':
            opener = gzip.open 
        else:
            opener = open
            
        # Read Columns field of JSON file
        phys_dat = {k:[] for k in phys_meta['Columns']}
        
        # Append tab delimited phys_file to phys_dat
        with opener(phys_file, 'rt') as h:
            for pl in h.readlines():
                pls = pl.split("\t")
                for k,v in zip(phys_meta['Columns'], pls):
                    phys_dat[k].append(float(v))
                    
        # Read columns field from JSON data
        print('Read columns field from JSON data')
        for k in phys_meta['Columns']:
            phys_dat[k] = array(phys_dat[k])
            
            # Read respiratory component
            if k.lower() == 'respiratory' or k.lower() == 'respiration':
                # create peaks only if asked for    25 May 2021 [rickr]
                if respiration_out or rvt_out:
                   if not respiration_info["phys_fs"]:
                       respiration_info['phys_fs'] = phys_meta['SamplingFrequency']
                   # respiration_peak, error = peak_finder(respiration_info,
                   #                                       v=phys_dat[k])
                   respiration_file = None
                   phys_resp_dat = phys_dat[k]
            
            # Read cardiac component
            elif k.lower() == 'cardiac':
                # create peaks only if asked for    25 May 2021 [rickr]
                if cardiac_out != 0:
                   if not respiration_info["phys_fs"]:
                       cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                   # cardiac_peak, error = peak_finder(cardiac_info,v=phys_dat[k])
                   cardiac_file = None
                   phys_cardiac_dat = phys_dat[k]
            else:
                print("** warning phys data contains column '%s', but\n" \
                      "   RetroTS only handles cardiac or respiratory data" % k)
    else:   # Not a JSON file
        if respiration_info["respiration_file"]:
            # respiration_peak, error = peak_finder(respiration_info, respiration_info["respiration_file"])
            respiration_file = respiration_info["respiration_file"]
            phys_resp_dat = None
        if cardiac_info["cardiac_file"]:
            # cardiac_peak, error = peak_finder(cardiac_info, cardiac_info["cardiac_file"])
            cardiac_file = cardiac_info["cardiac_file"]
            phys_cardiac_dat = None
            
    return respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat
    
    
def getPeaks(respiration_info, cardiac_info, phys_file, phys_json_arg, respiration_out, cardiac_out, rvt_out):
    
    # Handle file inputs
    # BIDS = Brain Imaging Data Structure
    if (((phys_file is not None) and (respiration_info["respiration_file"] is not None))
        or ((phys_file is not None) and (cardiac_info["cardiac_file"] is not None))):
        raise ValueError('You should not pass a BIDS style phsyio file'
                         ' and respiration or cardiac files.')
    # Get the peaks for respiration_info and cardiac_info
    # init dicts, may need -cardiac_out 0, for example   [16 Nov 2021 rickr]
    if phys_file:
        if phys_json_arg is None:
            # Remove .gz extension and give tab-separated file a .json extension
            phys_json = phys_file.replace(".gz", "").replace(".tsv", ".json")
        # Use json reader to read file data into phys_meta
        with open(phys_json_arg, 'rt') as h:
            phys_meta = json.load(h)
        # phys_ending is last element following a period
        phys_ending = phys_file.split(".")[-1]
        
        # Choose file opening function on the basis of whether file is gzippped
        if phys_ending == 'gz':
            opener = gzip.open 
        else:
            opener = open
            
        # Read Columns field of JSON file
        phys_dat = {k:[] for k in phys_meta['Columns']}
        
        # Append tab delimited phys_file to phys_dat
        with opener(phys_file, 'rt') as h:
            for pl in h.readlines():
                pls = pl.split("\t")
                for k,v in zip(phys_meta['Columns'], pls):
                    phys_dat[k].append(float(v))
                    
        # Read columns field from JSON data
        print('Read columns field from JSON data')
        for k in phys_meta['Columns']:
            phys_dat[k] = array(phys_dat[k])
            
            # Read respiratory component
            if k.lower() == 'respiratory' or k.lower() == 'respiration':
                # create peaks only if asked for    25 May 2021 [rickr]
                if respiration_out or rvt_out:
                   if not respiration_info["phys_fs"]:
                       respiration_info['phys_fs'] = phys_meta['SamplingFrequency']
                   # respiration_peak, error = peak_finder(respiration_info,
                   #                                       v=phys_dat[k])
                   respiration_file = None
                   var_v = phys_dat[k]
                   phys_resp_dat = phys_dat[k]
                else:
                   # user opted out
                   respiration_peak = {}    # No respiratory component
            
            # Read cardiac component
            elif k.lower() == 'cardiac':
                # create peaks only if asked for    25 May 2021 [rickr]
                if cardiac_out != 0:
                   if not respiration_info["phys_fs"]:
                       cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                   # cardiac_peak, error = peak_finder(cardiac_info,v=phys_dat[k])
                   cardiac_file = None
                   var_v = phys_dat[k]
                   phys_cardiac_dat = phys_dat[k]
                else:
                   # user opted out
                   cardiac_peak = {}    # No cardiac component
            else:
                print("** warning phys data contains column '%s', but\n" \
                      "   RetroTS only handles cardiac or respiratory data" % k)
    else:   # Not a JSON file
        if respiration_info["respiration_file"]:
            # respiration_peak, error = peak_finder(respiration_info, respiration_info["respiration_file"])
            respiration_file = respiration_info["respiration_file"]
            var_v = None
            phys_resp_dat = None
        else:
            respiration_peak = {}
        if cardiac_info["cardiac_file"]:
            # cardiac_peak, error = peak_finder(cardiac_info, cardiac_info["cardiac_file"])
            cardiac_file = cardiac_info["cardiac_file"]
            var_v = None
            phys_cardiac_dat = None
        else:
            cardiac_peak = {}

    respiration_peak = {}   
    v_np = readRawInputData(respiration_info, respiration_file, phys_dat)
    respiration_peak, error = peak_finder(respiration_info, v_np)
    if error:
        print("Died in respiratory PeakFinder")
        return
    cardiac_peak = {}
    v_np = readRawInputData(cardiac_info, cardiac_file, phys_dat)
    cardiac_peak, error = peak_finder(cardiac_info, v_np)
    if error:
        print("Died in cardiac PeakFinder")
        return

    respiration_info.update(respiration_peak)    
    cardiac_info.update(cardiac_peak)
    
    return respiration_info, cardiac_info, respiration_peak, cardiac_peak


def retro_ts(
    respiration_file=None,
    cardiac_file=None,
    phys_fs=None,
    number_of_slices=None,
    volume_tr=None,
    prefix="Output_File_Name",
    slice_offset=0,
    slice_major=1,
    rvt_shifts=list(range(0, 21, 5)),
    respiration_cutoff_frequency=3,
    cardiac_cutoff_frequency=3,
    interpolation_style="linear",
    fir_order=40,
    quiet=1,
    demo=0,
    rvt_out=1,
    cardiac_out=1,
    respiration_out=1,
    slice_order="alt+z",
    show_graphs=0,
    zero_phase_offset=0,
    legacy_transform=0,
    phys_file=None,
    phys_json=None,
    retroicor_algorithm=False
):

    if not slice_offset:
        slice_offset = zeros((1, number_of_slices))
     
    # Update slice offsets.  Note that this is done before teh data is read
    offsetDict = dict()
    offsetDict["slice_offset"] = slice_offset
    offsetDict["volume_tr"] = volume_tr
    offsetDict["number_of_slices"] = number_of_slices
    offsetDict["slice_order"] = slice_order
    offsetDict["quiet"] = quiet
    slice_offset = getSliceOffsets(offsetDict)

    # Create information dictionary for each type of signal
    # Note that this is done by reading the relevant input parameters
    respiration_info = dict()
    respiration_info["respiration_file"] = respiration_file
    respiration_info["phys_fs"] = phys_fs
    respiration_info["number_of_slices"] = number_of_slices
    respiration_info["volume_tr"] = volume_tr
    respiration_info["slice_offset"] = slice_offset
    respiration_info["rvt_shifts"] = rvt_shifts
    respiration_info["interpolation_style"] = interpolation_style
    respiration_info["legacy_transform"] = legacy_transform
    cardiac_info = dict()
    cardiac_info["phys_fs"] = phys_fs
    cardiac_info["cardiac_file"] = cardiac_file
    cardiac_info["number_of_slices"] = number_of_slices
    cardiac_info["volume_tr"] = volume_tr
    cardiac_info["slice_offset"] = slice_offset
    cardiac_info["rvt_shifts"] = rvt_shifts
    cardiac_info["interpolation_style"] = interpolation_style
    cardiac_info["frequency_cutoff"] = cardiac_cutoff_frequency
    cardiac_info["fir_order"] = fir_order
    cardiac_info["zero_phase_offset"] = zero_phase_offset
    cardiac_info["legacy_transform"] = legacy_transform
    
    # Get input file parameters
    respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat =\
        getInputFileParameters(respiration_info, cardiac_info, phys_file,\
                            phys_json, respiration_out, cardiac_out, rvt_out)
        

    # Read in raw data, and find peaks for respiration   
    respiration_peak = {}
    v_np = readRawInputData(respiration_info, respiration_file, phys_resp_dat)
    respiration_peak, error = peak_finder(respiration_info, v_np)
    if error:
        print("Died in respiratory PeakFinder")
        return

    # Read in raw data, and find peaks for cardiac    
    cardiac_peak = {}
    v_np = readRawInputData(cardiac_info, cardiac_file, phys_cardiac_dat)
    cardiac_peak, error = peak_finder(cardiac_info, v_np)
    if error:
        print("Died in cardiac PeakFinder")
        return

    respiration_info.update(respiration_peak)    
    cardiac_info.update(cardiac_peak)
       
    # Get the phase
    respiration_info["amp_phase"] = 1 # Amplitude-based phase for respiration
    if respiration_peak:
        print("Estimating phase for respiration_info")
        respiration_phased, rvt = phase_estimator(
            respiration_info["amp_phase"], respiration_info
        )
    else:
        respiration_phased = {}
    
    cardiac_info["amp_phase"] = 0   # Time-based phase for cardiac signal
    if cardiac_peak:
        cardiac_phased, tmp = phase_estimator(cardiac_info["amp_phase"], cardiac_info)
    else:
        cardiac_phased = {}

    if retroicor_algorithm:
        parameters=dict()
        parameters['-c'] = cardiac_file
        parameters['-r'] = respiration_file
        parameters['-s'] = number_of_slices
        parameters['-TR'] = volume_tr
        parameters['-Nt'] = 220
        parameters['-Sr'] = phys_fs
        parameters['-abt'] = 0
        parameters['-aby'] = 0
        fourierSeries = retroicor.getFourierSeries(parameters)
        fsDims = shape(fourierSeries)
        numTimePts = int(fsDims[0]/number_of_slices)
        numTimePts = min(numTimePts,220)
        print('***WARNING: Only ', numTimePts, ' timepoints.  Should be 220')
        inc = 0
        for t in range(0,numTimePts):
            for s in range(0,number_of_slices):
                for i in range(0,4):
                    respiration_phased[t][i][s] = fourierSeries[inc][i]
                    cardiac_phased[t][i][s] = fourierSeries[inc][i+4]
                inc += 1

    # Show some results
    if show_graphs:
        if respiration_info:
            print("Showing RVT Peaks for R\n")
            show_rvt_peak(respiration_info, 1)

    # also generate files as 3dREMLfit likes them
    n_n = 0
    n_r_v = 0
    n_r_p = 0
    n_e = 0
    if "time_series_time" in respiration_info:
        n_n = len(respiration_info["time_series_time"])
        n_r_p = size(respiration_info["phase_slice_reg"], 1)
        n_r_v = size(respiration_info["rvtrs_slc"], 0)

    if "time_series_time" in cardiac_info:  # must have cardiac_info
        n_n = len(
            cardiac_phased["time_series_time"]
        )  # ok to overwrite len(respiration_info.tst), should be same.
        n_e = size(cardiac_phased, 1)

    if (
        cardiac_out == 0
        and respiration_out == 0
        and rvt_out == 0
    ):
        print(
            "Options cardiac_out, respiration_out, and RVT_out all 0.\nNo output required.\n"
        )
        return

    temp_y_axis = number_of_slices * (
        (rvt_out) * int(n_r_v)
        + (respiration_out) * int(n_r_p)
        + (cardiac_out) * int(n_e)
    )
    reml_out = zeros((n_n, temp_y_axis))
    cnt = 0
    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = "'
        % (size(reml_out, 1), size(reml_out, 0))
    )
    tail = '"\n>'
    tailclose = "</RetroTSout>"

    label = head

    reml_out = []
    if slice_major == 0:  # old approach, not handy for 3dREMLfit
        # RVT
        if rvt_out != 0:
            for j in range(0, size(respiration_info["rvtrs_slc"], 2)):
                for i in range(0, number_of_slices):
                    cnt += 1
                    reml_out[:, cnt] = respiration_info["rvtrs_slc"][
                        :, j
                    ]  # same for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
        # Resp
        if respiration_out != 0:
            for j in range(0, size(respiration_info, 2)):
                for i in range(0, number_of_slices):
                    cnt += 1
                    reml_out[:, cnt] = respiration_info[
                        :, j, i
                    ]
                    label = "%s s%d.Resp%d ;" % (label, i, j)
        # Card
        if cardiac_out != 0:
            for j in range(0, size(cardiac_info, 2)):
                for i in range(0, number_of_slices):
                    cnt += 1
                    reml_out[:, cnt] = cardiac_info[
                        :, j, i
                    ]
                    label = "%s s%d.Card%d ;" % (label, i, j)
        fid = open(("%s.retrots.1D", prefix), "w")
    else:
        for i in range(0, number_of_slices):
            if rvt_out != 0:
                # RVT
                for j in range(0, shape(rvt)[0]):
                    cnt += 1
                    reml_out.append(
                        rvt[j,:]
                    )  # same regressor for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
            if respiration_out != 0:
                # Resp
                for j in range(0, shape(respiration_phased)[1]):
                    cnt += 1
                    reml_out.append(
                        respiration_phased[:, j, i]
                    )
                    label = "%s s%d.Resp%d ;" % (label, i, j)
            if cardiac_out != 0:
                # Card
                for j in range(0, shape(cardiac_phased)[1]):
                    cnt += 1
                    reml_out.append(
                        cardiac_phased[:, j, i]
                    )
                    label = "%s s%d.Card%d ;" % (label, i, j)
        fid = open(("%s.slibase.1D" % prefix), "w")
        
        print('Output file ', ("%s.slibase.1D" % prefix))

    # remove very last ';'
    label = label[1:-2]

    savetxt(
        "%s.slibase.1D" % prefix,
        column_stack(reml_out),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    
    return 0


if __name__ == "__main__":

    import sys

    opt_dict = {
        "-help": """
This function creates slice-based regressors for regressing out components of
    heart rate, respiration and respiration volume per time.

Windows Example:
C:\\afni\\python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20 -v 2

Mac/Linux Example:
/usr/afni/python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20 -v 2

Input
================================================================================
    RetroTS.py can be run with independent respiration and cardiac data files
    (Method 1), or with a BIDS formatted physio file and json (Method 2).

    Method 1:
    ---------
    :param -r: (respiration_file) Respiration data file
    :param -c: (cardiac_file) Cardiac data file
    :param -p: (phys_fs) Physiological signal sampling frequency in Hz.
    :param -n: (number_of_slices) Number of slices
    :param -v: (volume_tr) Volume TR in seconds
    Note:   These parameters are the only single-letter parameters, as they are
            mandatory and frequently typed. The following optional parameters
            must be fully spelled out.

    Method 2:
    ---------
    :param -phys_file: BIDS formatted physio file in tab separated format. May
            be gzipped.
    :param -phys_json: BIDS formatted physio metadata json file. If not specified
            the json corresponding to the phys_file will be loaded.
    :param -n: (number_of_slices) Number of slices
    :param -v: (volume_tr) Volume TR in seconds


    Optional:
    ---------
    :param -retroicor: Use the retroicor algorithm to estimate the cardiac and 
           respiratory phases
    ============================================================================
    :param -prefix: Prefix of output file
    ============================================================================
    :param -rvt_shifts: Vector of shifts in seconds of RVT signal.
            (default is [0:5:20])
    :param -rvt_out: Flag for writing RVT regressors
            (default is 1)
    ============================================================================
    :param -respiration_cutoff_frequency: Cut off frequency in Hz for
            respiratory lowpass filter
            (default 3 Hz)
    :param -cardiac_cutoff_frequency: Cut off frequency in Hz for
            cardiac lowpass filter
            (default 3 Hz)
    :param -cardiac_out: Flag for writing Cardiac regressors
            (default is 1)
    :param -respiration_out: Flag for writing Respiratory regressors
            (default is 1)
    ============================================================================
    :param -interpolation_style: Resampling kernel.
            (default is 'linear', see help interp1 for more options)
    :param -fir_order: Order of FIR filter.
            (default is 40)
    ============================================================================
    :param -quiet: Show talkative progress as the program runs
            (default is 1)
    :param -demo: Run demonstration of RetroTS
            (default is 0)
    :param -show_graphs:
            (default is unset; set with any parameter to view)
    :param -debug Drop into pdb upon an exception
            (default is False)
    ============================================================================
    :param -slice_offset: Vector of slice acquisition time offsets in seconds.
            (default is equivalent of alt+z)
    :param -slice_major: ? (default is 1)
    :param -slice_order: Slice timing information in seconds. The default is
           alt+z. See 3dTshift help for more info.
               alt+z    = alternating in the plus direction
               alt-z    = alternating in the minus direction
               seq+z    = sequential in the plus direction
               seq-z    = sequential in the minus direction
               custom   = allows the program to use the values stored in the
                            -slice_offset list
               filename = read temporal offsets from 'filename', including file
                            extension; e.g. slice_file.dat
                            (expecting a 1D / text file containing the times for
                            each slice in seconds)

            For example, the following 4 commands would produce identical
            output, based on 10 slices using a (non-default) alt-z slice order:

               RetroTS.py -c ECG.1D -r Resp.1D             \\
                          -v 2 -p 50 -n 10 -prefix fred    \\
                          -slice_order alt-z

               set offlist = "[1.8, 0.8, 1.6, 0.6, 1.4, 0.4, 1.2, 0.2, 1.0, 0]"
               RetroTS.py -c ECG.1D -r Resp.1D             \\
                          -v 2 -p 50 -n 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               set offlist = "1.8  0.8  1.6  0.6  1.4  0.4  1.2  0.2  1.0  0"
               RetroTS.py -c ECG.1D -r Resp.1D             \\
                          -v 2 -p 50 -n 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               # put those same offsets into a text file (vertically)
               echo $offlist | tr ' ' '\\n' > slice_offsets.txt
               RetroTS.py -c ECG.1D -r Resp.1D             \\
                          -v 2 -p 50 -n 10 -prefix fred    \\
                          -slice_order slice_offsets.txt


    ============================================================================
    :param -zero_phase_offset:
    ============================================================================
    :param legacy_transform: Important-this will specify whether you use the
           original Matlab code's version (1) or the potentially bug-corrected
           version (0) for the final phase correction in
           lib_RetroTS/RVT_from_PeakFinder.py
           (default is 0)

Output:
================================================================================
    Files saved to same folder based on selection for "-respiration_out" and
    "-cardiac_out". If these options are enabled, than the data will be written
    to a single output file based on the filename assigned to the
    option "-prefix".

    Example:
    C:\\afni\\python RetroTS.py -r resp_file.dat -c card_file.dat -p 50 -n 20
        -v 2 -prefix subject12_regressors -respiration_out 1 -cardiac_out 1

        Output:
        The file "subject12_regressors.slibase.1D" will be saved to current
        directory, including respiratory regressors and cardiac regressors.

        """,
        "-r": None,
        "-c": None,
        "-p": None,
        "-n": None,
        "-v": None,
        "-prefix": "Output_File_Name",
        "-slice_offset": 0,
        "-slice_major": 1,
        "-rvt_shifts": list(range(0, 21, 5)),
        "-respiration_cutoff_frequency": 3,
        "-cardiac_cutoff_frequency": 3,
        "-interpolation_style": "linear",
        "-fir_order": 40,
        "-quiet": 1,
        "-demo": 0,
        "-debug": False,
        "-rvt_out": 1,
        "-cardiac_out": 1,
        "-respiration_out": 1,
        "-slice_order": "alt+z",
        "-show_graphs": 0,
        "-zero_phase_offset": 0,
        "-legacy_transform": 0,
        "-phys_file":None,
        "-phys_json":None,
        "-retroicor": False
    }

    if len(sys.argv) < 2:
        print(
            "You need to provide parameters. If you need help, rerun the"
            'program using the "-help" argument:'
            '\n"python RetroTS.py -help"'
        )
        sys.exit() 
    else:
        opts = sys.argv[1:]
        temp_opt = None
        for opt in opts:
            if opt in opt_dict:
                if opt == "-help":
                    print(opt_dict[opt])
                    quit()
                elif opt == "-debug":
                    setup_exceptionhook()

            elif temp_opt in opt_dict:
                opt_dict[temp_opt] = opt
            else:
                print("No such command '%s', try:" % opt)
                for key in list(opt_dict.keys()):
                    print("%s" % key)
                quit()
            temp_opt = opt
    if opt_dict["-p"]:
        opt_dict["-p"] = float(opt_dict["-p"])
    
    if (opt_dict["-n"] == None):
        print('WARNING: Number of slices not given.')
        
    # change phys_fs and volume_tr to float     6 Mar 2017 [rickr]
    retro_ts(
        respiration_file=opt_dict["-r"],
        cardiac_file=opt_dict["-c"],
        phys_fs=opt_dict["-p"],
        number_of_slices=int(opt_dict["-n"]),
        volume_tr=float(opt_dict["-v"]),
        prefix=opt_dict["-prefix"],
        slice_offset=opt_dict["-slice_offset"],
        slice_major=opt_dict["-slice_major"],
        rvt_shifts=opt_dict["-rvt_shifts"],
        respiration_cutoff_frequency=opt_dict["-respiration_cutoff_frequency"],
        cardiac_cutoff_frequency=opt_dict["-cardiac_cutoff_frequency"],
        interpolation_style=opt_dict["-interpolation_style"],
        fir_order=opt_dict["-fir_order"],
        quiet=opt_dict["-quiet"],
        demo=opt_dict["-demo"],
        rvt_out= (int(opt_dict["-rvt_out"]) ),
        cardiac_out= (int(opt_dict["-cardiac_out"])),
        respiration_out= (int(opt_dict["-respiration_out"])),
        slice_order=opt_dict["-slice_order"],
        show_graphs=opt_dict["-show_graphs"],
        zero_phase_offset=opt_dict["-zero_phase_offset"],
        legacy_transform=opt_dict["-legacy_transform"],
        phys_file=opt_dict["-phys_file"],
        phys_json=opt_dict["-phys_json"],
        retroicor_algorithm=opt_dict["-retroicor"]
    )
