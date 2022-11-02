#!/usr/bin/env python

# python3 status: compatible

# coding=utf-8
__author__ = "Joshua Zosky and Peter Lauren" # Modified a bit by gianfranco 

"""
    Copyright 2015 Joshua Zosky
    Copyright 2022 Peter Lauren
    joshua.e.zosky@gmail.com
    peterdlauren@gmail.com

    "RetroTS2" is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    "RetroTS2" is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with "RetroTS".  If not, see <http://www.gnu.org/licenses/>.
    
    TODO:
        - Add docstring to each function
        - Make demo scripts
        - Align names of variables
        - Add plot font size as command line option
        - quiet switch?
        - Add to box:
            - Samples (input files)
            - Scripts that run samples with options we want
            - Physio measure files
        - Offset for every slice relative to TR
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
            - Findging peaks
            - Determining phase
            - Determining final output
        
"""

roadmap = """

Major blocks of calculation:

- calculate A, B
  PL      getCoefA(),
  JZ/ZSS  old_getCoefA()

- estimate phase
  PL      PhaseEstimator()
  JZ/ZSS  get_phase_calc()

- check time series lengths

- find gaps

"""

import sys
import gzip
import json
from numpy import zeros, size, savetxt, column_stack, shape, array
import lib_retroicor
from sept_retroicor import phase_estimator
from sept_retroicor import peak_finder
from lib_retroicor import readRawInputData
from sept_retroicor import show_rvt_peak
from sept_retroicor import determineCardiacPhases,determineRespiratoryPhases,compareLaurenAndZoskyPhases
import os

from datetime import datetime
now     = datetime.now() # current date and time
now_str = now.strftime("retro_%Y-%m-%d-%H-%M-%S")

def setup_exceptionhook():
    """
    NAME
        setup_exceptionhook 
            Overloads default sys.excepthook with our exceptionhook handler.
            If interactive, our exceptionhook handler will invoke pdb.post_mortem;
            if not interactive, then invokes default handler.
            
    TYPE
        void
    SYNOPSIS
      setup_exceptionhook()
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
       """
       
    def _pdb_excepthook(type, value, tb):
        """
        NAME
            _pdb_excepthook
                Sets up exception hook (if in interactive mode)
                
        TYPE
            void
            
        SYNOPSIS
            _pdb_excepthook(type, value, tb)
            
        ARGUMENTS
            type: Exception type being handled (a subclass of BaseException)
            
            value:   Exception instance
            
            tb:   Traceback object
       AUTHOR
           Joshua Zosky (Documentation by Peter Lauren)
        """

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
    """
    NAME
        getSliceOffsets 
            Return phase offsets among slices
    TYPE
        <class 'list'>
    SYNOPSIS
       getSliceOffsets(offsetDict)
    ARGUMENTS
        offsetDict:   Dictionary with the following fields.
        
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
        
            slice_order:   Order of slices (alt+z, alt-z, etc).  Default is "alt+z".
        
            quiet:   0 if show graphs. 1 if do not show graphs
            
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """
        
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
                sys.exit(1)
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
    """
    NAME
        getInputFileParameters 
            Returns the local respiriation file name  (if JSON file absent, None otherwise), 
            respiration file data (if JSON file present, None otherwise), the 
            local cardiac file name  (if JSON file absent, None otherwise), 
            cardiac file data (if JSON file present, None otherwise).
    TYPE
        <class 'str'>, <class 'numpy.ndarray'>, <class 'str'>, <class 'numpy.ndarray'>
    SYNOPSIS
       getInputFileParameters(respiration_info, cardiac_info, phys_file,
       phys_json_arg, respiration_out, cardiac_out, rvt_out)
    ARGUMENTS
        respiration_info:   Dictionary with the following fields.
        
            respiration_file:  Name of ASCII file with respiratory time series
            
            phys_fs:   Physiological signal sampling frequency in Hz.
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            frequency_cutoff:   Cutoff frequency for smoothing RVT
            
            fir_order:   Order of Finite Impulse Response (FIR) filter
            
            zero_phase_offset:Phase offset added to the location of each peak.
            Default is 0.0
            
            legacy_transform:   Important-this will specify whether you use the 
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py  (default is 0)
            
        cardiac_info:   Dictionary with the following fields.
            
            phys_fs:   Physiological signal sampling frequency in Hz.
        
            cardiac_file:  Name of ASCII file with cardiac time series
            
            number_of_slices:   Number of slices
            
            volume_tr:   Volume repetition time (TR) which defines the length of time 
            between the acquisition of consecutive frames/volumes; in seconds
            
            slice_offset:   Vector of slice acquisition time offsets in seconds.
            
            rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
            
            interpolation_style:   Resampling kernel.
            
            frequency_cutoff:   Cutoff frequency for smoothing RVT
            
            fir_order:   Order of Finite Impulse Response (FIR) filter
            
            zero_phase_offset:Phase offset added to the location of each peak.
            Default is 0.0
            
            legacy_transform:   Important-this will specify whether you use the 
            original Matlab code's version (1) or the potentially bug-corrected
            version (0) for the final phase correction in
            lib_RetroTS/RVT_from_PeakFinder.py  (default is 0)
            
        phys_file: BIDS formatted physio file in tab separated format. May
        be gzipped.
                
        phys_json_arg: File metadata in JSON format
        
        respiration_out:  Whether to have respiratory output
        
        cardiac_out:  Whether to have cardiac output
        
        rvt_out:  Whether to have RVT output
            
    AUTHOR
       Joshua Zosky (Documentation by Peter Lauren)
    """
            
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
                    
        # Process StartTime is in JSON file
        if ('StartTime' in phys_meta and "StartTime" not in respiration_info):
            startTime = float(phys_meta["StartTime"])
            if (startTime > 0):
                print('***** WARNING: JSON file gives positive start time which is not currently handled')
                print('    Start time must be <= 0')
            else:
                respiration_info["StartTime"] = startTime            
                cardiac_info["StartTime"] = startTime            
                    
        print('phys_meta = ', phys_meta)
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
                   respiration_file = None
                   phys_resp_dat = phys_dat[k]
            
            # Read cardiac component
            elif k.lower() == 'cardiac':
                # create peaks only if asked for    25 May 2021 [rickr]
                if cardiac_out != 0:
                   if not respiration_info["phys_fs"]:
                       cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                   cardiac_file = None
                   phys_cardiac_dat = phys_dat[k]
            else:
                print("** warning phys data contains column '%s', but\n" \
                      "   RetroTS only handles cardiac or respiratory data" % k)
    else:   # Not a JSON file
        if respiration_info["respiration_file"]:
            respiration_file = respiration_info["respiration_file"]
            phys_resp_dat = None
        if cardiac_info["cardiac_file"]:
            cardiac_file = cardiac_info["cardiac_file"]
            phys_cardiac_dat = None
            
    return respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat
    

def retro_ts(
    respiration_file=None,
    cardiac_file=None,
    phys_fs=None,
    number_of_slices=None,
    volume_tr=None,
    OutDir=now_str,
    prefix="Output_File_Name",
    slice_offset=0,
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
    show_graphs=1,
    zero_phase_offset=0,
    legacy_transform=0,
    phys_file=None,
    phys_json=None,
    retroicor_algorithm=False,
    args=None
):
    """
    NAME
        retro_ts
            Main function for RetroTS2
        
    TYPE
        <class 'int'>
        
    SYNOPSIS
        retro_ts(
            respiration_file=None,
            cardiac_file=None,
            phys_fs=None,
            number_of_slices=None,
            volume_tr=None,
            OutDir=now_str,
            prefix="Output_File_Name",
            slice_offset=0,
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
            show_graphs=1,
            zero_phase_offset=0,
            legacy_transform=0,
            phys_file=None,
            phys_json=None,
            retroicor_algorithm=False,
            args=None)
        
    ARGUMENTS
        respiration_file:   String giving name of ASCII file with respiratory time series
        
        cardiac_file:   String giving name of ASCII file with cardiac time series
        
        phys_fs:   Physiological signal sampling frequency in Hz.
        
        number_of_slices:   Number of slices.
        
        volume_tr:   Volume repetition time (TR) which defines the length of time 
        between the acquisition of consecutive frames/volumes; in seconds
        
        OutDir:   String giving name of directory to create for output files.
        Default is "retro_" follwed by the current date and time.
        
        prefix:   Prefix for output filename.
        
        slice_offset:   Vector of slice acquisition time offsets in seconds.
        
        rvt_shifts:   Vector of shifts (in seconds) of RVT signal.
        
        respiration_cutoff_frequency: Cut off frequency in Hz for respiratory lowpass filter
        
        cardiac_cutoff_frequency: : Cut off frequency in Hz for cardiac lowpass 
        filter (default 3 Hz)
        
        interpolation_style:   Resampling kernel. 
        
        fir_order:   Order of Finite Impulse Response (FIR) filter
        
        quiet:   0 if show graphs. 1 if do not show graphs
        
        demodemo:   Whether running in demo mode.  (Show graphs and pause between graphs.)
        
        rvt_out: Flag for writing RVT regressors (default is 1)
        
        cardiac_out: Flag for writing Cardiac regressors (default is 1)
        
        respiration_out: Flag for writing Respiratory regressors (default is 1)
        
        slice_order: Slice timing information in seconds. The default is
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
            
        show_graphs:   Whether to show graphs
        
        zero_phase_offset:Phase offset added to the location of each peak.
        Default is 0.0
        
        legacy_transform:   Important-this will specify whether you use the
        original Matlab code's version (1) or the potentially bug-corrected
        version (0) for the final phase correction in
        lib_RetroTS/RVT_from_PeakFinder.py
        (default is 0)
        
        phys_file: BIDS formatted physio file in tab separated format. May
        be gzipped.
                       
        phys_json_arg: File metadata in JSON format
        
        retroicor_algorithm: Whether to use Peter Lauren's implementation of 
        the Glover paper'
        
        args: Command line arguments supplied by user (String)

    AUTHOR
       Joshua Zosky and Peter Lauren
    """

    # Make output directory
    path = os.path.join(os.getcwd(), OutDir)
    os.mkdir(path)
    
    # Output args to file in new directory
    fid = open(("%s/arguments.txt"% (OutDir)), "w")
    fid.write(" ".join(args))
    fid.write("\n")
    fid.close()
    
    # Set output directory for lib_retroicor
    lib_retroicor.setOutputDirectory(OutDir)

    if not slice_offset:
        slice_offset = zeros((1, number_of_slices))
     
    # Update slice offsets.  Note that this is done before teh data is read
    print('Update slice offsets.  Note that this is done before teh data is read')
    offsetDict = dict()
    offsetDict["slice_offset"] = slice_offset
    offsetDict["volume_tr"] = volume_tr
    offsetDict["number_of_slices"] = number_of_slices
    offsetDict["slice_order"] = slice_order
    offsetDict["quiet"] = quiet
    slice_offset = getSliceOffsets(offsetDict)

    # Create information dictionary for each type of signal
    # Note that this is done by reading the relevant input parameters
    print('Create information dictionary for each type of signal')
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
    print('Get input file parameters')

    respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat =\
        getInputFileParameters(respiration_info, cardiac_info, phys_file,\
                            phys_json, respiration_out, cardiac_out, rvt_out)        
        
    # Read in raw data, and find peaks for respiration measurements
    print('Read in raw data, and find peaks for respiration')
    v_np = readRawInputData(respiration_info, respiration_file, phys_resp_dat)
    rawRespiratorData = v_np

    # Find respiratory peaks
    respiration_info['respcard'] = "Respiratory"
    respiration_peak, error = peak_finder(respiration_info, v_np)
    if error:
        print("Died in respiratory PeakFinder")
        return

    # Read in raw data, and find peaks for cardiac measurements    
    v_np = readRawInputData(cardiac_info, cardiac_file, phys_cardiac_dat)
    rawCardiacData = v_np

    # Find cardiac peaks
    cardiac_info['respcard'] = "Cardiac"
    cardiac_peak, error = peak_finder(cardiac_info, v_np)
    if error:
        print("Died in cardiac PeakFinder")
        return

    # Update respiratory and cardiac info with peak info
    respiration_info.update(respiration_peak)    
    cardiac_info.update(cardiac_peak)
       
    # Get the phase
    respiration_info["amp_phase"] = 1 # Amplitude-based phase for respiration
    respiration_info["zero_phase_offset"] = zero_phase_offset
    if respiration_peak:
        print("Estimating phase for respiration_info")
        phasee, rvt = phase_estimator(
            respiration_info["amp_phase"], respiration_info
        )
        respiration_phased = phasee["phase_slice_reg"]
    else:
        respiration_phased = {}    
    cardiac_info["amp_phase"] = 1   # Time-based phase for cardiac signal
    cardiac_info["zero_phase_offset"] = zero_phase_offset
    if cardiac_peak:
        phasee, tmp = phase_estimator(cardiac_info["amp_phase"], cardiac_info)
        cardiac_phased = phasee["phase_slice_reg"]
    else:
        cardiac_phased = {}

    if retroicor_algorithm: # Peter Lauren's implementation from 2000 Glover paper
        parameters=dict()
        parameters['-cardFile'] = cardiac_file
        parameters['-respFile'] = respiration_file
        parameters['-numSlices'] = number_of_slices
        parameters['-TR'] = volume_tr
        parameters['-Nt'] = 220
        parameters['-Sr'] = phys_fs
        parameters['-abt'] = 0
        parameters['-aby'] = 0
        fourierSeries = lib_retroicor.getFourierSeries(parameters)
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
        
    # R&D: Compare new algorithm for finding phases with old peaks
    # rawData = rawCardiacData
    # newCardiacPhases = determineCardiacPhases((cardiac_info["tp_trace"]*phys_fs).astype(int), 
    #                                           len(v_np), phys_fs, rawCardiacData)
    # parameters = dict()
    # parameters["-respFile"] = respiration_info["respiration_file"]
    # parameters["-phys_fs"] = respiration_info["phys_fs"]
    # newRespiratoryPhases = determineRespiratoryPhases(parameters,\
    #    [round(elem*respiration_info["phys_fs"]) for elem in respiration_info["tp_trace"]],\
    #    [round(elem*respiration_info["phys_fs"]) for elem in respiration_info["tn_trace"]],
    #    phys_fs, rawRespiratorData)
    # sliceNumber = 0
    # compareLaurenAndZoskyPhases(cardiac_info, respiration_info, cardiac_phased, respiration_phased,\
    #                             newCardiacPhases, newRespiratoryPhases, sliceNumber)

    # Show some results
    if show_graphs:
        if respiration_info:
            print("Showing RVT Peaks for R\n")
            respiration_info['v_name'] = 'Respiratory RVT'
            respiration_info["time_series_time"] = phasee["time_series_time"]
            respiration_info["phase_slice"] = phasee["phase_slice"]
            show_rvt_peak(respiration_info, 1)

    # also generate files as 3dREMLfit likes them
    n_n = 0
    n_r_v = 0
    n_r_p = 0
    n_e = 0
    if "time_series_time" in respiration_info:
        n_n = len(respiration_info["time_series_time"])
        n_r_p = size(respiration_phased, 1)
        n_r_v = size(phasee["rvtrs_slc"], 0)

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

    # Make output vector
    reml_out = []
    for i in range(0, number_of_slices):
        if rvt_out != 0:
            # RVT
            for j in range(0, shape(rvt)[0]):
                cnt += 1
                reml_out.append(
                    rvt[j,:]
                )  # same regressor for each slice
                label = " %s s%d.RVT%d " % (label, i, j)
        if respiration_out != 0:
            # Resp
            for j in range(0, shape(respiration_phased)[1]):
                cnt += 1
                reml_out.append(
                    respiration_phased[:, j, i]
                )
                label = " %s s%d.Resp%d " % (label, i, j)
        if cardiac_out != 0:
            # Card
            for j in range(0, shape(cardiac_phased)[1]):
                cnt += 1
                reml_out.append(
                    cardiac_phased[:, j, i]
                )
                label = " %s s%d.Card%d " % (label, i, j)
    fid = open(("%s/%s.slibase.1D"% (OutDir , prefix)), "w")
    
    print('Output file ', ("%s/%s.slibase.1D"% (OutDir , prefix)))

    # remove very last '\t'
    label = label[1:-2]

    savetxt(
        "%s/%s.slibase.1D" % (OutDir , prefix),
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
C:\\afni\\python RetroTS.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20 -volume_tr 2

Mac/Linux Example:
/usr/afni/python RetroTS.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20 -volume_tr 2

Input
================================================================================
    RetroTS.py can be run with independent respiration and cardiac data files
    (Method 1), or with a BIDS formatted physio file and json (Method 2).

    Method 1:
    ---------
    respFile: (respiration_file) Respiration data file
    caesFile: (cardiac_file) Cardiac data file
    freq: (phys_fs) Physiological signal sampling frequency in Hz.
    numSlices: (number_of_slices) Number of slices
    volume_tr: (volume_tr) Volume repetition time (TR) which defines the length of time 
    between the acquisition of consecutive frames/volumes; in seconds
    Note:   These parameters are the only single-letter parameters, as they are
            mandatory and frequently typed. The following optional parameters
            must be fully spelled out.

    Method 2:
    ---------
    phys_file: BIDS formatted physio file in tab separated format. May
            be gzipped.
    phys_json: BIDS formatted physio metadata json file. If not specified
            the json corresponding to the phys_file will be loaded.
    numSlices: (number_of_slices) Number of slices
    v: (volume_tr) Volume TR in seconds


    Optional:
    ---------
    retroicor: Use the retroicor algorithm to estimate the cardiac and 
           respiratory phases
    ============================================================================
    OutDir: Output directory
    ============================================================================
    prefix: Prefix of output file
    ============================================================================
    rvt_shifts: Vector of shifts in seconds of RVT signal.
            (default is [0:5:20])
    rvt_out: Flag for writing RVT regressors
            (default is 1)
    ============================================================================
    respiration_cutoff_frequency: Cut off frequency in Hz for
            respiratory lowpass filter
            (default 3 Hz)
    cardiac_cutoff_frequency: Cut off frequency in Hz for
            cardiac lowpass filter
            (default 3 Hz)
    cardiac_out: Flag for writing Cardiac regressors
            (default is 1)
    respiration_out: Flag for writing Respiratory regressors
            (default is 1)
    ============================================================================
    interpolation_style: Resampling kernel.
            (default is 'linear', see help interp1 for more options)
    fir_order: Order of FIR filter.
            (default is 40)
    ============================================================================
    quiet: Show talkative progress as the program runs
            (default is 1)
    demo: Run demonstration of RetroTS
            (default is 0)
    show_graphs:
            (default is unset; set with any parameter to view)
    debug Drop into pdb upon an exception
            (default is False)
    ============================================================================
    slice_offset: Vector of slice acquisition time offsets in seconds.
            (default is equivalent of alt+z)
    slice_order: Slice timing information in seconds. The default is
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

               RetroTS.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order alt-z

               set offlist = "[1.8, 0.8, 1.6, 0.6, 1.4, 0.4, 1.2, 0.2, 1.0, 0]"
               RetroTS.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               set offlist = "1.8  0.8  1.6  0.6  1.4  0.4  1.2  0.2  1.0  0"
               RetroTS.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               # put those same offsets into a text file (vertically)
               echo $offlist | tr ' ' '\\n' > slice_offsets.txt
               RetroTS.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order slice_offsets.txt


    ============================================================================
    zero_phase_offset:
    ============================================================================
    legacy_transform: Important-this will specify whether you use the
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
    C:\\afni\\python RetroTS.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20
        -volume_tr 2 -prefix subject12_regressors -respiration_out 1 -cardiac_out 1

        Output:
        The file "subject12_regressors.slibase.1D" will be saved to current
        directory, including respiratory regressors and cardiac regressors.

        """,
        "-respFile": None,
        "-cardFile": None,
        "-freq": None,
        "-numSlices": None,
        "-volume_tr": None,
        "-OutDir": now_str,
        "-prefix": "Output_File_Name",
        "-slice_offset": 0,
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
        "-show_graphs": 1,
        "-zero_phase_offset": 0,
        "-legacy_transform": 0,
        "-phys_file":None,
        "-phys_json":None,
        "-retroicor_algorithm": False
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
                    sys.exit(0) 
                elif opt == "-debug":
                    setup_exceptionhook()

            elif temp_opt in opt_dict:
                opt_dict[temp_opt] = opt
            else:
                print("No such command '%s', try:" % opt)
                for key in list(opt_dict.keys()):
                    print("%s" % key)
                sys.exit(1)
            temp_opt = opt
    if opt_dict["-freq"]:
        opt_dict["-freq"] = float(opt_dict["-freq"])
    
    if (opt_dict["-numSlices"] == None):
        print('WARNING: Number of slices not given.')
        
    # change phys_fs and volume_tr to float     6 Mar 2017 [rickr]
    retro_ts(
        respiration_file=opt_dict["-respFile"],
        cardiac_file=opt_dict["-cardFile"],
        phys_fs=opt_dict["-freq"],
        number_of_slices=int(opt_dict["-numSlices"]),
        volume_tr=float(opt_dict["-volume_tr"]),
        OutDir=opt_dict["-OutDir"],
        prefix=opt_dict["-prefix"],
        slice_offset=opt_dict["-slice_offset"],
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
        retroicor_algorithm=opt_dict["-retroicor_algorithm"],
        args = sys.argv[1:]
    )
