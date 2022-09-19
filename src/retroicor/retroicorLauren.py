#!/usr/bin/env python

# python3 status: compatible

# coding=utf-8
__author__ = "Peter Lauren" # Modified a bit by gianfranco 

"""
    Copyright 2022 Peter Lauren
    peterdlauren@gmail.com

    "retroicorLauren" is free software: you can redistribute it and/or modify
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
        - Make demo scripts
        - Add to box:
            - Samples (input files)
            - Scripts that run samples with options we want
            - Physio measure files
        - Simplify reading function
        - Align names of variables
        - Add plot font size as command line option
        - quiet switch?
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
import numpy as np
import lib_retroicor
from lib_retroicor import getPhysiologicalNoiseComponents, getInputFileParameters
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
    

def retro_ts(
    respiration_file=None,
    cardiac_file=None,
    phys_fs=None,
    number_of_slices=None,
    volume_tr=None,
    OutDir=now_str,
    prefix="Output_File_Name",
    slice_offset=0,
    fir_order=40,
    quiet=1,
    demo=0,
    rvt_out=0,
    cardiac_out=1,
    respiration_out=1,
    slice_order="alt+z",
    show_graphs=1,
    zero_phase_offset=0,
    phys_file=None,
    phys_json=None,
    abt=False,
    aby=False,
    niml = False,
    args=None
):
    """
    NAME
        retro_ts
            Main function for retroicorLauren2
        
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
            fir_order=40,
            quiet=1,
            demo=0,
            rvt_out=0,
            cardiac_out=1,
            respiration_out=1,
            slice_order="alt+z",
            show_graphs=1,
            zero_phase_offset=0,
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
        
        fir_order:   Order of Finite Impulse Response (FIR) filter
        
        quiet:   0 if show graphs. 1 if do not show graphs
        
        demodemo:   Whether running in demo mode.  (Show graphs and pause between graphs.)
        
        rvt_out: Flag for writing RVT regressors (default is 0)
        
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
        
        phys_file: BIDS formatted physio file in tab separated format. May
        be gzipped.
                       
        phys_json_arg: File metadata in JSON format
        
        retroicor_algorithm: Whether to use Peter Lauren's implementation of 
        the Glover paper'
        
        args: Command line arguments supplied by user (String)

    AUTHOR
       Peter Lauren
    """
    
    if not phys_fs:
        print('Error: Sampling frequency in Hz (phys_fs) required')
        return 1

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
        slice_offset = np.zeros((1, number_of_slices))
     
    # Update slice offsets.  Note that this is done before the data is read
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
    cardiac_info = dict()
    cardiac_info["phys_fs"] = phys_fs
    cardiac_info["cardiac_file"] = cardiac_file
       
    # Get input file parameters
    print('Get input file parameters')

    respiration_file, phys_resp_dat, cardiac_file, phys_cardiac_dat =\
        getInputFileParameters(respiration_info, cardiac_info, phys_file,\
                            phys_json, respiration_out, cardiac_out, rvt_out)        
        
    parameters = dict()
    parameters['-cardFile'] = cardiac_file
    parameters['-respFile'] = respiration_file
    parameters['-s'] = number_of_slices
    parameters['-TR'] = volume_tr
    parameters['-phys_fs'] = phys_fs
    parameters['-abt'] = abt
    parameters['-aby'] = aby
    parameters['-niml'] = niml
    parameters['phys_resp_dat'] = phys_resp_dat
    parameters['phys_cardiac_dat'] = phys_cardiac_dat
    if cardiac_info['phys_fs']: parameters['phys_fs'] = cardiac_info['phys_fs']
    else: parameters['phys_fs'] = respiration_info['phys_fs']
    physiologicalNoiseComponents = getPhysiologicalNoiseComponents(parameters)
    if parameters['-niml']:
        return 0
    
    outputFileName = path + "/" + prefix + "FourierSeries.csv"
    physiologicalNoiseComponents.to_csv(outputFileName)

    # PLot first 200 rows of dataframe
    # colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen','red','magenta']
    # physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (parameters['-abt']): print(repr(physiologicalNoiseComponents))
    
    return 0


if __name__ == "__main__":

    import sys

    opt_dict = {
        "-help": """
This function creates slice-based regressors for regressing out components of
    heart rate, respiration and respiration volume per time.

Windows Example:
C:\\afni\\python retroicorLauren.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20 -volume_tr 2

Mac/Linux Example:
/usr/afni/python retroicorLauren.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20 -volume_tr 2

Input
================================================================================
    retroicorLauren.py can be run with independent respiration and cardiac data files
    (Method 1), or with a BIDS formatted physio file and json (Method 2).

    Method 1:
    ---------
    respFile: (respiration_file) Respiration data file
    caesFile: (cardiac_file) Cardiac data file
    freq: (phys_fs) Physiological signal sampling frequency in Hz.
    numSlices: (number_of_slices) Number of slices
    volume_tr: (volume_tr) Volume repetition time (TR) which defines the length of time 
    between the acquisition of consecutive frames/volumes; in seconds

    Method 2:
    ---------
    phys_file: BIDS formatted physio file in tab separated format. May
            be gzipped.
    phys_json: BIDS formatted physio metadata json file. If not specified
            the json corresponding to the phys_file will be loaded.
    numSlices: (number_of_slices) Number of slices
    volume_tr: Volume TR in seconds

    Optional:
    ---------
    abt 0|1                  : Output a and b coefficients to terminal (Default = false)
    aby 0|1                  : Output time series based on a,b coefficients (Default = false) 
    niml 0|1                 : Output in NIML format instead of CSV format (Default = false) 
    ============================================================================
    OutDir: Output directory
    ============================================================================
    prefix: Prefix of output file
    ============================================================================
    cardiac_out: Flag for writing Cardiac regressors
            (default is 1)
    respiration_out: Flag for writing Respiratory regressors
            (default is 1)
    rvt_out: Flag for writing RVT regressors
            (default is 0)
    ============================================================================
    quiet: Show talkative progress as the program runs
            (default is 1)
    demo: Run demonstration of retroicorLauren
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

               retroicorLauren.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order alt-z

               set offlist = "[1.8, 0.8, 1.6, 0.6, 1.4, 0.4, 1.2, 0.2, 1.0, 0]"
               retroicorLauren.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               set offlist = "1.8  0.8  1.6  0.6  1.4  0.4  1.2  0.2  1.0  0"
               retroicorLauren.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order custom              \\
                          -slice_offset "$offlist"

               # put those same offsets into a text file (vertically)
               echo $offlist | tr ' ' '\\n' > slice_offsets.txt
               retroicorLauren.py -cardFile ECG.1D -respFile Resp.1D             \\
                          -volume_tr 2 -freq 50 -numSlices 10 -prefix fred    \\
                          -slice_order slice_offsets.txt


    ============================================================================
    zero_phase_offset:

Output:
================================================================================
    The output data will be written
    to a single output file based on the file root-name assigned to the
    option "-prefix".

    Example:
    C:\\afni\\python retroicorLauren.py -respFile resp_file.dat -cardFile card_file.dat -freq 50 -numSlices 20
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
        "-fir_order": 40,
        "-quiet": 1,
        "-demo": 0,
        "-debug": False,
        "-rvt_out": 0,
        "-cardiac_out": 1,
        "-respiration_out": 1,
        "-slice_order": "alt+z",
        "-show_graphs": 1,
        "-zero_phase_offset": 0,
        "-phys_file":None,
        "-phys_json":None,
        "-abt": False,
        "-aby": False,
        "-niml": False
    }

    if len(sys.argv) < 2:
        print(
            "You need to provide parameters. If you need help, rerun the"
            'program using the "-help" argument:'
            '\n"python retroicorLauren.py -help"'
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
                print("No such option key: '%s', try:" % opt)
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
        fir_order=opt_dict["-fir_order"],
        quiet=opt_dict["-quiet"],
        demo=opt_dict["-demo"],
        rvt_out= (int(opt_dict["-rvt_out"]) ),
        cardiac_out= (int(opt_dict["-cardiac_out"])),
        respiration_out= (int(opt_dict["-respiration_out"])),
        slice_order=opt_dict["-slice_order"],
        show_graphs=opt_dict["-show_graphs"],
        zero_phase_offset=opt_dict["-zero_phase_offset"],
        phys_file=opt_dict["-phys_file"],
        phys_json=opt_dict["-phys_json"],
        abt=opt_dict["-abt"],
        aby=opt_dict["-aby"],
        niml=opt_dict["-niml"],
        args = sys.argv[1:]
    )
