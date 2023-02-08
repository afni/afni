#!/usr/bin/env python

# python3 status: compatible

# coding=utf-8
__author__ = "Peter Lauren" 

"""
    2022 Peter Lauren
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
        - Offset for every slice relative to TR
            - afnipy/afni_util.py:slice_pattern_to_timing() gets the actual timing
        - Implement slice pattern.  Required option 
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
            - Findging peaks
            - Determining phase
            - Determining final output
        - See what quiet option does and determine if it is still necessary,
            given that verbose is available.
    
    DONE:
        - Test start time via TR shift
        - Align names of variables
        - Add plot font size as command line option
        - Make demo scripts
        - Add to box:
            - Samples (input files)
            - Scripts that run samples with options we want
            - Physio measure files
        
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
import numpy         as np
import lib_retroicor as RET
import os
from   datetime      import datetime

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
            
        ARGUMENTS
            type: (dType = sys.exc_type) Exception type being handled (a 
                  subclass of BaseException)
            
            value: (dType = sys.exc_value) Exception instance
            
            tb: (dType = sys.exc_traceback) Traceback object
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
    ARGUMENTS
        offsetDict:   Dictionary with the following fields.
        
            number_of_slices:   (dType = int) Number of slices
            
            volume_tr:   (dType = float) Volume repetition time (TR) which 
            defines the length of time between the acquisition of consecutive 
            frames/volumes; in seconds
            
            num_time_pts:  (dType = int) Number of time points in the output
            
            slice_offset:   (2D array dType = numpy.float64) Vector of slice 
                            acquisition time offsets in seconds.
        
            slice_pattern:   (dType = str) Pettern of slices 
                           (alt+z, alt-z, etc).  Default is "alt+z".
        
            quiet:   (dType = int) 0 if show graphs. 1 if do not show graphs
            
    AUTHOR
       Joshua Zosky (Documentation and comments by Peter Lauren)
    """
        
    slice_offset = offsetDict["slice_offset"]
    
    # Determining slice_offset based upon slice_pattern, volume_tr,
    #  and number_of_slices.
    tt = 0.0  # Default float value to start iterations
    dtt = float(offsetDict["volume_tr"]) / float(
        offsetDict["number_of_slices"]
    )  # Increments for iteration
    # init slice_offsets, unless Custom order
    # (noted by Jogi Ho on board   27 Dec 2017 [rickr])
    if (
        (offsetDict["slice_pattern"] not in ["Custom", "custom"])
        or len(slice_offset) != offsetDict["number_of_slices"]
    ):
        slice_offsets = [0] * offsetDict[
            "number_of_slices"
        ]  # Initial value for slice_offset
    slice_file_list = (
        []
    )  # List for using external file for slice_offset values/
    # Indicates if using external file in last loop
    if offsetDict["slice_pattern"][0:3] == "alt":  # Alternating?
        for i in range(0, offsetDict["number_of_slices"], 2):
            slice_offsets[i] = tt
            tt += dtt
        for i in range(1, offsetDict["number_of_slices"], 2):
            slice_offsets[i] = tt
            tt += dtt
    elif offsetDict["slice_pattern"][0:3] == "seq":  # Sequential?
        for i in range(0, offsetDict["number_of_slices"]):
            slice_offsets[i] = tt
            tt += dtt
    elif offsetDict["slice_pattern"] in ["Custom", "custom"] \
        and type(slice_offset) == str:

        # If slice_pattern is custom, parse from slice_offset string.
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
        with open(offsetDict["slice_pattern"], "r") as f:
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
        offsetDict["slice_pattern"][3] == "-" and slice_file_list == []
    ):  # Check for a minus to indicate
        #  a reversed offset list
        slice_offsets.reverse()
    if (
        offsetDict["quiet"] != 1
    ):  # Show the slice timing (P.S. Printing is very time consuming in python)
        print("Slice timing: %s" % slice_offsets)
        
    return slice_offsets
    

def retro_ts(
    resp_file         = None,
    card_file         = None,
    phys_fs           = None,
    number_of_slices  = None,
    volume_tr         = None,
    start_time        = 0,
    num_time_pts      = None,
    OutDir            = now_str,
    prefix            = None,
    slice_offset      = 0,
    fir_order         = 40,
    quiet             = 1,
    demo              = 0,
    dev               = False,
    verbose           = False,
    show_graphs       = 0,
    save_graphs       = 1,
    font_size         = 10,
    rvt_out           = 0,
    card_out          = 1,
    resp_out          = 1,
    slice_pattern       = "alt+z",
    zero_phase_offset = 0,
    phys_file         = None,
    phys_json         = None,
    abt               = False,
    aby               = False,
    niml              =  False,
    args              = None
):
    """
    NAME
        retro_ts
            Main function for retroicorLauren2
        
    TYPE
        <class 'int'>
        
    ARGUMENTS
        resp_file: (dType = str) String giving name of ASCII file with 
                   respiratory time series
        
        card_file: (dType = str) String giving name of ASCII file with 
                      cardiac time series
        
        phys_fs: (dType = float) Physiological signal sampling frequency in Hz.
        
        number_of_slices: (dType = int) Number of slices.
        
        volume_tr: (dType = float) Volume repetition time (TR) which defines the 
                   length of time between the acquisition of consecutive 
                   frames/volumes; in seconds
                   
        start_time: Start time in secomds.  (Must be negative)
        
        num_time_pts: (dType = int) Number of time points in the output
        
        OutDir: (dType = str) String giving name of directory to create for 
                output files.  Default is "retro_" follwed by the current date 
                and time.
        
        prefix: (dType = str) Prefix for output filename.  Default = None
        
        slice_offset: (dType = int) Vector of slice acquisition time offsets in 
                      seconds.
        
        fir_order: (dType = int) Order of Finite Impulse Response (FIR) filter
        
        quiet: (dType = int) 0 if show graphs. 1 if do not show graphs
        
        demo: (dType = int) Whether running in demo mode.  (Show graphs and 
                            pause between graphs.)
        
        dev: (dType = bool) Whether running in dev(elopment) mode.   (Show 
                            graphs and pause between graphs.)
        
        verbose: (dType = bool) Whether runnung in verbose mode.  Save graphs, 
                                of each filtering step, to disk.
        
        rvt_out: (dType = int) Flag for writing RVT regressors (default is 0)
        
        card_out: (dType = int) Flag for writing Cardiac regressors (default is 1)
        
        resp_out: (dType = int) Flag for writing Respiratory regressors (default is 1)
        
        slice_pattern: (dType = str) Slice timing information in seconds. The 
                     default is alt+z. See 3dTshift help for more info.
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
            
        show_graphs: (dType = int) Whether to show graphs
        
        save_graphs: (dType = int) Whether to save graphs
        
        font_size:    (dType = int) Font size to use with graphs
        
        zero_phase_offset: (dType = int) Phase offset added to the location of 
                           each peak. Default is 0.0
        
        phys_file: (dType = NoneType) BIDS formatted physio file in tab separated 
        format. May be gzipped.
                       
        phys_json: (dType = NoneType) File metadata in JSON format
        
        args: (dType = list) Command line arguments supplied by user (String)

    AUTHOR
       Peter Lauren
    """
    
    if not phys_fs and not phys_json:
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
    RET.setOutputDirectory(OutDir)

    if not slice_offset:
        slice_offset = np.zeros((1, number_of_slices))
     
    # Update slice offsets.  Note that this is done before the data is read
    print('Update slice offsets.  '
          'Note that this is done before the data is read')
    offsetDict = dict()
    offsetDict["slice_offset"] = slice_offset
    offsetDict["volume_tr"] = volume_tr
    offsetDict["num_time_pts"] = int(num_time_pts)
    offsetDict["number_of_slices"] = number_of_slices
    offsetDict["slice_pattern"] = slice_pattern
    offsetDict["quiet"] = quiet
    offsetDict["dev"] = dev
    offsetDict["verbose"] = verbose
    slice_offset = getSliceOffsets(offsetDict)

    # Create information dictionary for each type of signal
    # Note that this is done by reading the relevant input parameters
    print('Create information dictionary for each type of signal')
    resp_info = dict()
    resp_info["resp_file"] = resp_file
    resp_info["phys_fs"] = phys_fs
    cardiac_info = dict()
    cardiac_info["phys_fs"] = phys_fs
    cardiac_info["card_file"] = card_file
       
    # Get input file parameters
    print('Get input file parameters')

    resp_file, phys_resp_dat, card_file, phys_card_dat =\
        RET.getInputFileParameters(resp_info, cardiac_info, phys_file, \
                                   phys_json, resp_out, card_out, rvt_out) 
        
    # Set paremeters
    parameters = dict()
    parameters['cardFile']      = card_file
    parameters['respFile']      = resp_file
    parameters['s']             = number_of_slices
    parameters['TR']            = volume_tr
    parameters['StartTime']     = start_time
    parameters['num_time_pts']  = int(num_time_pts)
    parameters['phys_fs']       = phys_fs
    parameters['abt']           = abt
    parameters['aby']           = aby
    parameters['niml']          = niml
    parameters['phys_resp_dat'] = phys_resp_dat
    parameters['phys_card_dat'] = phys_card_dat
    parameters['dev']           = dev
    parameters['verbose']       = verbose
    parameters['rvt_out']       = rvt_out
    parameters['slice_offset']  = slice_offset

    if prefix: parameters['prefix'] = prefix
    elif  phys_json: parameters['prefix'] = getPrefix(phys_json)
    elif  parameters['cardFile']: 
        parameters['prefix'] = getPrefix(parameters['cardFile'])
    elif  parameters['cardFile']: 
        parameters['prefix'] = getPrefix(parameters['respFile'])
    else: 
        print('Error: Could not determine output file prefix')
        return 1

    if cardiac_info['phys_fs']: parameters['phys_fs'] = cardiac_info['phys_fs']
    else: parameters['phys_fs'] = resp_info['phys_fs']    

    if not parameters['phys_fs']:
        print('Error: Sampling frequency in Hz (phys_fs) required')
        return 1

    parameters['show_graphs'] = show_graphs
    parameters['save_graphs'] = save_graphs
    parameters['font_size']   = font_size

    physiologicalNoiseComponents = RET.getPhysiologicalNoiseComponents(parameters)
    if len(physiologicalNoiseComponents) == 0:
        print('*** Error in retro_ts.  Failure to get physiological noise\
              components')
        return 1
    if parameters['niml']:
        return 0
    parameters['OutDir'] = OutDir
    RET.ouputInNimlFormat(physiologicalNoiseComponents, parameters)
    
    if len(physiologicalNoiseComponents['resp_phases']) > 0 and\
        (parameters['save_graphs'] or parameters['show_graphs']):
        status = RET.show_rvt_peak(physiologicalNoiseComponents, parameters)
        if status == 1:
            print('*** Error in retro_ts')
            print('Failure to show RVT peak')
            return 1
    
    # outputFileName = path + "/" + prefix + "FourierSeries.csv"
    # physiologicalNoiseComponents.to_csv(outputFileName)

    # PLot first 200 rows of dataframe
    # colors = ['blue','cyan','blueviolet','cadetblue', 'olive','yellowgreen',
    # 'red','magenta']
    # physiologicalNoiseComponents.head(200).plot(color=colors)
    
    # Send output to terminal
    if (parameters['abt']): print(repr(physiologicalNoiseComponents))
    
    return 0

def getPrefix(fileName):
    
    if fileName.find('/') >= 0: 
        return fileName.split('/',1)[-1].split('.', 1)[0] # Linux search path
    if fileName.find(':') >= 0: 
        return fileName.split(':',1)[-1].split('.', 1)[0] # Mac search path
    
    # No search path given
    return fileName.split('.', 1)[0]


if __name__ == "__main__":

    import sys

    opt_dict = {
        "-help": """

OVERVIEW ~1~

This function creates slice-based regressors for regressing out components of
heart rate, resp and respiration volume per time.


USAGE ~1~

    retroicorLauren.py can be run with independent respiration and cardiac data 
    files (Method 1), or with a BIDS formatted physio file and json (Method 2).

    Method 1:
    ---------
    -resp_file RFILE   : Respiration data file
    -card_file CFILE   : Cardiac data file
    -freq      PHYS_FS : Physiological signal sampling frequency (in Hz)
    -num_slices NS     : Integer number of slices
    -volume_tr  TR     : MRI volume's repetition time (TR), which defines 
                         the time interval between consecutive volumes (in s)
    -Nt         NT     : Integer number of time points to have in the output
                         (should likely match MRI time series length)

    Method 2:
    ---------
    -phys_file  PFILE  : BIDS-formatted physio file in tab-separated format. 
                         May be gzipped
    -phys_json  PJSON  : BIDS-formatted physio metadata JSON file. If not 
                         specified, the JSON corresponding to the 
                         '-phys_file ..' will be loaded
    -num_slices NS     : Integer number of slices
    -volume_tr  TR     : MRI volume's repetition time (TR), which defines 
                         the time interval between consecutive volumes (in s)
    -Nt         NT     : Integer number of time points to have in the output
                         (should likely match MRI time series length)

    Optional:
    ---------
    -abt        ABT    : Output a and b coefficients to terminal (ABT = 1) or 
                         don't (ABT = 0, the default)
    -aby        ABY    : Output the time series based on a,b coefficients 
                         (ABY = 1) or don't (ABY = 0, the default)
    -niml       NIML   : Output in NIML format (if NIML = 1) instead of 
                         CSV format (if NIML = 0, the default)
        
    -out_dir    ODIR   : Output directory

    -prefix     PREF   : Prefix of output file

    -card_out   CCC    : Write out cardiac regressors (CCC = 1, the default) 
                         or not (CCC = 0)
    -resp_out   RRR    : Write out respiratory regressors (RRR = 1, the 
                         default) or not (RRR = 0)
    -rvt_out    RVT    : Write out RVT regressors (RVT = 1, the default) 
                         or not (RVT = 0)

    -show_graphs SHOWG : Integer value for one of the following behaviors:
                           0 - Do not show graphs
                           1 - Show end results (cardiac peaks, respiratory 
                               peaks and final RVT)
                           2 - Show intermediate and end results (band-pass 
                               filter, cardiac peaks, respiratory peaks and 
                               final RVT)
                         (def: SHOWG = 0)

    -save_graphs SAVEG : Integer value for one of the following behaviors:
                           0 - Do not save graphs
                           1 - Save end results (cardiac peaks, respiratory 
                               peaks and final RVT)
                           2 - Save intermediate and end results (band-pass 
                               filter, cardiac peaks, respiratory peaks and 
                               final RVT)
                         (def: SAVEG = 1)

    -font_size   FS    : Font size used for graphics (def: FS = 10)

   -slice_offset SLOFF : Vector of slice acquisition time offsets (in sec)
                         (default is equivalent of alt+z)

   -slice_pattern SLPAT : Slice timing pattern code (def: SLPAT = alt+z). The
                          following codes are allowed:
                            alt+z    = alternating in the plus direction
                            alt-z    = alternating in the minus direction
                            seq+z    = sequential in the plus direction
                            seq-z    = sequential in the minus direction
                            custom   = allows the program to use the
                                       values stored in the '-slice_offset ..'
                                       list
                            filename = read temporal offsets from 'filename', 
                                       including file extension; e.g. 
                                       "slice_file.dat" (expecting a 1D text 
                                       file containing the times for each slice,
                                       in seconds)
                          See 3dTshift help for more info.

    -zero_phase_offset ZPO : ***does something, needs description***

    -demo       DDD    : Run demonstration of this program (DDD = 1) 
                         or not (DDD = 0, the default)

    -dev        DEV    : Run development mode for this program (DDD = 1) 
                         or not (DDD = 0, the default)

    -verbose           : Integer values to control verbosity level
                         (default is 0)

    -quiet      QQQ    : Show talkative progress as the program runs (QQQ = 1,
                         the default) or not (QQQ = 0)

    -debug       DBG   : Drop into pdb upon an exception (DBG = 1) or not
                         (DBG = 0, default)


EXAMPLES ~1~

  The following 4 commands would produce identical output, based on 10
  slices using a (non-default) alt-z slice pattern:

    1) 
    retroicorLauren.py                                            \\
        -card_file      ECG.1D                                    \\
        -resp_file      Resp.1D                                   \\
        -volume_tr      2                                         \\
        -freq           50                                        \\
        -num_slices     10                                        \\
        -prefix         fred                                      \\
        -slice_pattern  alt-z                                     \\
        -Nt             220

    2) Same as Ex 1, with 'list format' of slice offset list (that
       happens to correspond to 'alt+z' pattern)
    set offlist = "[1.8, 0.8, 1.6, 0.6, 1.4, 0.4, 1.2, 0.2, 1.0, 0]"
    retroicorLauren.py                                            \\
        -card_file      ECG.1D                                    \\
        -resp_file      Resp.1D                                   \\
        -volume_tr      2                                         \\
        -freq           50                                        \\
        -num_slices     10                                        \\
        -prefix         fred                                      \\
        -slice_pattern  custom                                    \\
        -slice_offset   "$offlist"                                \\
        -Nt             220

    3) Same as Ex 1, with 'text format' of slice offset list (that
       happens to correspond to 'alt+z' pattern)
    set offlist = "1.8  0.8  1.6  0.6  1.4  0.4  1.2  0.2  1.0  0"
    retroicorLauren.py                                            \\
        -card_file      ECG.1D                                    \\
        -resp_file      Resp.1D                                   \\
        -volume_tr      2                                         \\
        -freq           50                                        \\
        -num_slices     10                                        \\
        -prefix         fred                                      \\
        -slice_pattern  custom                                    \\
        -slice_offset   "$offlist"                                \\
        -Nt             220

    4) Same as Ex 1, with '1D file format' of slice offset list (that
       happens to correspond to 'alt+z' pattern)
    # put those same offsets into a text file (vertically)
    echo $offlist | tr ' ' '\\n' > slice_offsets.txt
    retroicorLauren.py                                            \\
        -card_file      ECG.1D                                    \\
        -resp_file      Resp.1D                                   \\
        -volume_tr      2                                         \\
        -freq           50                                        \\
        -num_slices     10                                        \\
        -prefix         fred                                      \\
        -slice_pattern  slice_offsets.txt                         \\
        -Nt             220

OUTPUT FORMAT ~1~

The output data will be written to a single output file based on the
file root-name assigned to the option "-prefix ..".

For example, from Ex. 1 above, the file "fred_regressors.slibase.1D"
would be saved to current working directory, including respiratory
regressors and cardiac regressors.

***describe more about each file output (including saved images)***


        """,
        "-resp_file"         : None,
        "-card_file"         : None,
        "-freq"              : None,
        "-num_slices"        : None,
        "-volume_tr"         : None,
        "-start_time"        : 0,
        "-num_time_pts"      : None,
        "-out_dir"           : now_str,
        "-prefix"            : None,
        "-slice_offset"      : 0,
        "-fir_order"         : 40,
        "-quiet"             : 1,
        "-demo"              : 0,
        "-dev"               : False,
        "-verbose"           : False,
        "-debug"             : False,
        "-rvt_out"           : 0,
        "-card_out"          : 1,
        "-resp_out"          : 1,
        "-slice_pattern"       : "alt+z",
        "-show_graphs"       : 0,
        "-save_graphs"       : 1,
        "-font_size"         : 10,
        "-zero_phase_offset" : 0,
        "-phys_file"         : None,
        "-phys_json"         : None,
        "-abt"               : False,
        "-aby"               : False,
        "-niml"              : False
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
                elif opt == "-verbose":
                    opt_dict["-verbose"] = True
                elif opt == "-dev":
                    opt_dict["-dev"] = True

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
    
    if (opt_dict["-num_slices"] == None):
        print('WARNING: Number of slices not given.')
        
    # change phys_fs and volume_tr to float     6 Mar 2017 [rickr]
    return_status = retro_ts(
        resp_file         = opt_dict["-resp_file"],
        card_file         = opt_dict["-card_file"],
        phys_fs           = opt_dict["-freq"],
        number_of_slices  = int(opt_dict["-num_slices"]),
        volume_tr         = float(opt_dict["-volume_tr"]),
        start_time        = float(opt_dict["-start_time"]),
        num_time_pts      = int(opt_dict["-num_time_pts"]),
        OutDir            = opt_dict["-out_dir"],
        prefix            = opt_dict["-prefix"],
        slice_offset      = opt_dict["-slice_offset"],
        fir_order         = opt_dict["-fir_order"],
        quiet             = opt_dict["-quiet"],
        demo              = opt_dict["-demo"],
        dev               = opt_dict["-dev"],
        verbose           = opt_dict["-verbose"],
        rvt_out           = int(opt_dict["-rvt_out"]),
        card_out          = int(opt_dict["-card_out"]),
        resp_out          = int(opt_dict["-resp_out"]),
        slice_pattern       = opt_dict["-slice_pattern"],
        show_graphs       = int(opt_dict["-show_graphs"]),
        save_graphs       = int(opt_dict["-save_graphs"]),
        font_size         = int(opt_dict["-font_size"]),
        zero_phase_offset = opt_dict["-zero_phase_offset"],
        phys_file         = opt_dict["-phys_file"],
        phys_json         = opt_dict["-phys_json"],
        abt               = opt_dict["-abt"],
        aby               = opt_dict["-aby"],
        niml              = opt_dict["-niml"],
        args              = sys.argv[1:]
    )
    
    if return_status == 0:
        print('Program completed successfully')
    else:
        print('Program failed')
        
