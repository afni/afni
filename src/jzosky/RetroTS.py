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
from lib_RetroTS.PeakFinder import peak_finder
from lib_RetroTS.PhaseEstimator import phase_estimator
from lib_RetroTS.RVT_from_PeakFinder import rvt_from_peakfinder
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
):
    """

    :param respiration_file:
    :param cardiac_file:
    :param phys_fs:
    :param number_of_slices:
    :param volume_tr:
    :param prefix:
    :param slice_offset:
    :param slice_major:
    :param rvt_shifts:
    :param respiration_cutoff_frequency:
    :param cardiac_cutoff_frequency:
    :param interpolation_style: kind : str or int, optional
        Specifies the kind of interpolation as a string:
            "linear", "nearest", "zero", 'slinear', "quadratic", "cubic",
            Where 'slinear', "quadratic" and "cubic" refer to a spline
            interpolation of first, second or third order
        Or as an integer specifying the order of the spline interpolator to
        use. Default is "linear".
    :param fir_order:
    :param quiet:
    :param demo:
    :param rvt_out:
    :param cardiac_out:
    :param respiration_out:
    :param slice_order:
    :param show_graphs:
    :param legacy_transform: Important-this will specify whether you use the original Matlab code's version or the
        potentially bug-corrected version for the final phase correction in lib_RetroTS/RVT_from_PeakFinder.py
    :param phys_file: IDS formatted physio file in tab separated format. May
            be gzipped.
    :param phys_json: BIDS formatted physio metadata json file. If not specified
            the json corresponding to the phys_file will be loaded.
    :return:
    """
    if not slice_offset:
        slice_offset = zeros((1, number_of_slices))
    main_info = {
        "respiration_file": respiration_file,
        "cardiac_file": cardiac_file,
        "phys_fs": phys_fs,
        "number_of_slices": number_of_slices,
        "volume_tr": volume_tr,
        "prefix": prefix,
        "slice_offset": slice_offset,
        "slice_major": slice_major,
        "rvt_shifts": rvt_shifts,
        "respiration_cutoff_frequency": respiration_cutoff_frequency,
        "cardiac_cutoff_frequency": cardiac_cutoff_frequency,
        "interpolation_style": interpolation_style,  # replacement for 'ResamKernel' variable name
        "fir_order": fir_order,
        "quiet": quiet,
        "demo": demo,
        "rvt_out": rvt_out,
        "cardiac_out": cardiac_out,
        "respiration_out": respiration_out,
        "slice_order": slice_order,
        "show_graphs": show_graphs,
        "zero_phase_offset": zero_phase_offset,
        "legacy_transform": legacy_transform,
        "phys_file": phys_file,
        "phsio_json": phys_json,
    }
    # Determining main_info['slice_offset'] based upon main_info['slice_order'], main_info['volume_tr'],
    #  and main_info['number_of_slices'].
    tt = 0.0  # Default float value to start iterations
    dtt = float(main_info["volume_tr"]) / float(
        main_info["number_of_slices"]
    )  # Increments for iteration
    # init slice_offsets, unless Custom order
    # (noted by Jogi Ho on board   27 Dec 2017 [rickr])
    if (
        (main_info["slice_order"] not in ["Custom", "custom"])
        or len(main_info["slice_offset"]) != main_info["number_of_slices"]
    ):
        main_info["slice_offset"] = [0] * main_info[
            "number_of_slices"
        ]  # Initial value for main_info['slice_offset']
    slice_file_list = (
        []
    )  # List for using external file for main_info['slice_offset'] values/
    # Indicates if using external file in last loop
    if main_info["slice_order"][0:3] == "alt":  # Alternating?
        for i in range(0, main_info["number_of_slices"], 2):
            main_info["slice_offset"][i] = tt
            tt += dtt
        for i in range(1, main_info["number_of_slices"], 2):
            main_info["slice_offset"][i] = tt
            tt += dtt
    elif main_info["slice_order"][0:3] == "seq":  # Sequential?
        for i in range(0, main_info["number_of_slices"]):
            main_info["slice_offset"][i] = tt
            tt += dtt
    elif main_info["slice_order"] in ["Custom", "custom"] \
        and type(slice_offset) == str:

        # If slice_order is custom, parse from slice_offset string.
        # Allow simple or pythonic array form.   1 Dec 2020 [rickr]
        try:
           offlist = eval(slice_offset)
           noff = len(offlist)
        except:
           try:
              offlist = [float(v) for v in slice_offset.split()]
           except:
              print("** failed to apply custom slice timing from: %s" \
                    % slice_offset)
              return
        if len(offlist) != main_info["number_of_slices"]:
           print("** error: slice_offset len = %d, but %d slices" \
              % (len(offlist), main_info["number_of_slices"]))
           return

        # success, report and apply
        print("applying custom slice timing, min = %g, max = %g" \
              % (min(offlist), max(offlist)))
        slice_offset = offlist
        main_info["slice_offset"] = offlist

    else:  # Open external file specified in argument line,
        # fill SliceFileList with values, then load into main_info['slice_offset']
        with open(main_info["slice_order"], "r") as f:
            for i in f.readlines():
                # read times, in seconds
                slice_file_list.append(float(i))

            # Check that slice acquisition times match the number of slices
            if len(slice_file_list) != main_info["number_of_slices"]:
                print("Could not read enough slice offsets from file")
                print("File should have as many offsets as number_of_slices")
                quit()
            main_info["slice_offset"] = slice_file_list
    if (
        main_info["slice_order"][3] == "-" and slice_file_list == []
    ):  # Check for a minus to indicate
        #  a reversed offset list
        main_info["slice_offset"].reverse()
    if (
        main_info["quiet"] != 1
    ):  # Show the slice timing (P.S. Printing is very time consuming in python)
        print("Slice timing: %s" % main_info["slice_offset"])

    # Create information copy for each type of signal
    respiration_info = dict(main_info)
    respiration_info["frequency_cutoff"] = main_info["respiration_cutoff_frequency"]
    # Amplitude-based phase for respiration
    respiration_info["amp_phase"] = 1
    # respiration_info['as_percover'] = 50  # Percent overlap of windows for fft
    # respiration_info['as_windwidth'] = 0  # Window width in seconds for fft, 0 for full window
    # respiration_info['as_fftwin'] = 0     # 1 == hamming window. 0 == no windowing
    cardiac_info = dict(main_info)
    cardiac_info["frequency_cutoff"] = main_info["cardiac_cutoff_frequency"]
    # Time-based phase for cardiac signal
    cardiac_info["amp_phase"] = 0

    # Handle file inputs
    if (((phys_file is not None) and (respiration_file is not None))
        or ((phys_file is not None) and (cardiac_file is not None))):
        raise ValueError('You should not pass a BIDS style phsyio file'
                         ' and respiration or cardiac files.')
    # Get the peaks for respiration_info and cardiac_info
    if phys_file:
        if phys_json is None:
            phys_json = phys_file.replace(".gz", "").replace(".tsv", ".json")
        with open(phys_json, 'rt') as h:
            phys_meta = json.load(h)
        phys_ending = phys_file.split(".")[-1]
        if phys_ending == 'gz':
            opener = gzip.open
        else:
            opener = open
        phys_dat = {k:[] for k in phys_meta['Columns']}
        with opener(phys_file, 'rt') as h:
            for pl in h.readlines():
                pls = pl.split("\t")
                for k,v in zip(phys_meta['Columns'], pls):
                    phys_dat[k].append(float(v))
        for k in phys_meta['Columns']:
            phys_dat[k] = array(phys_dat[k])
            if k.lower() == 'respiratory':
                if not main_info['phys_fs']:
                    respiration_info['phys_fs'] = phys_meta['SamplingFrequency']
                respiration_peak, error = peak_finder(respiration_info, v=phys_dat[k])
                if error:
                    print("Died in respiratory PeakFinder")
                    return
            elif k.lower() == 'cardiac':
                if not main_info['phys_fs']:
                    cardiac_info['phys_fs'] = phys_meta['SamplingFrequency']
                cardiac_peak, error = peak_finder(cardiac_info, v=phys_dat[k])
                if error:
                    print("Died in cardiac PeakFinder")
                    return
            else:
                Warning('phys data contains a {s} column, but RetroTS will'
                        ' only handle cardiac or reipiratory data.')
    else:
        if respiration_file:
            respiration_peak, error = peak_finder(respiration_info, respiration_file)
            if error:
                print("Died in respiratory PeakFinder")
                return
        else:
            respiration_peak = {}
        if cardiac_file:
            cardiac_peak, error = peak_finder(cardiac_info, cardiac_file)
            if error:
                print("Died in cardiac PeakFinder")
                return
        else:
            cardiac_peak = {}

    main_info["resp_peak"] = respiration_peak
    main_info["card_peak"] = cardiac_peak
    respiration_info.update(respiration_peak)
    cardiac_info.update(cardiac_peak)

    # Get the phase
    if respiration_peak:
        print("Estimating phase for respiration_info")
        respiration_phased = phase_estimator(
            respiration_info["amp_phase"], respiration_info
        )
    else:
        respiration_phased = {}
    if cardiac_peak:
        print("Estimating phase for cardiac_info")
        print(cardiac_info["v"])
        cardiac_phased = phase_estimator(cardiac_info["amp_phase"], cardiac_info)
    else:
        cardiac_phased = {}

    respiration_info.update(respiration_phased)
    cardiac_info.update(cardiac_phased)

    if respiration_phased:
        print("Computing RVT from peaks")
        print(respiration_info["p_trace_r"])
        rvt = rvt_from_peakfinder(respiration_phased)
        respiration_info.update(rvt)

    # Show some results
    if show_graphs:
        if respiration_info:
            print("Showing RVT Peaks for R\n")
            show_rvt_peak(respiration_info, 1)

    """
    # Not sure if this code is necessary, 'if 0' is never run in MATLAB
    # Show RVT graphs goes here, currently not important though

    if 0:
        # Write retroicor regressors
        for i in range(0, opt['main_info['number_of_slices']']):
            fname = '%s.RetroCard.slc%02d.1D' % (opt['Prefix'], i)
            #wryte3(cardiac_phased['phase_slice_reg'][:,:,i], fname, 1);
            savetxt(fname, cardiac_phased['phase_slice_reg'][:,:,i], fmt="%12.6G")
            fname = sprintf('%s.RetroResp.slc%02d.1D', Opt.Prefix, i);
            # wryte3(respiration_info.phase_slice_reg(:,:,i), fname, 1);
            savetxt(fname, respiration_phased['phase_slice_reg'][:,:,i], fmt="%12.6G")

        # And write the RVT puppy, plus or minus a few seconds delay
        fname = '%s.RetroRVT.1D' % opt['Prefix']
        # wryte3(respiration_info.rvtrs_slc, fname, 1);
        savetxt(fname, rvt['rvtrs_slc'], fmt="%12.6G")
    """
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
        n_e = size(cardiac_phased["phase_slice_reg"], 1)

    if (
        main_info["cardiac_out"] == 0
        and main_info["respiration_out"] == 0
        and main_info["rvt_out"] == 0
    ):
        print(
            "Options cardiac_out, respiration_out, and RVT_out all 0.\nNo output required.\n"
        )
        return

    temp_y_axis = main_info["number_of_slices"] * (
        (main_info["rvt_out"]) * int(n_r_v)
        + (main_info["respiration_out"]) * int(n_r_p)
        + (main_info["cardiac_out"]) * int(n_e)
    )
    main_info["reml_out"] = zeros((n_n, temp_y_axis))
    cnt = 0
    head = (
        "<RetroTSout\n"
        'ni_type = "%d*double"\n'
        'ni_dimen = "%d"\n'
        'ColumnLabels = "'
        % (size(main_info["reml_out"], 1), size(main_info["reml_out"], 0))
    )
    tail = '"\n>'
    tailclose = "</RetroTSout>"

    label = head

    main_info["reml_out"] = []
    if main_info["slice_major"] == 0:  # old approach, not handy for 3dREMLfit
        # RVT
        if main_info["rvt_out"] != 0:
            for j in range(0, size(respiration_info["rvtrs_slc"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = respiration_info["rvtrs_slc"][
                        :, j
                    ]  # same for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
        # Resp
        if main_info["respiration_out"] != 0:
            for j in range(0, size(respiration_info["phase_slice_reg"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = respiration_info["phase_slice_reg"][
                        :, j, i
                    ]
                    label = "%s s%d.Resp%d ;" % (label, i, j)
        # Card
        if main_info["Card_out"] != 0:
            for j in range(0, size(cardiac_info["phase_slice_reg"], 2)):
                for i in range(0, main_info["number_of_slices"]):
                    cnt += 1
                    main_info["reml_out"][:, cnt] = cardiac_info["phase_slice_reg"][
                        :, j, i
                    ]
                    label = "%s s%d.Card%d ;" % (label, i, j)
        fid = open(("%s.retrots.1D", main_info["prefix"]), "w")
    else:
        for i in range(0, main_info["number_of_slices"]):
            if main_info["rvt_out"] != 0:
                # RVT
                for j in range(0, shape(respiration_info["rvtrs_slc"])[0]):
                    cnt += 1
                    main_info["reml_out"].append(
                        respiration_info["rvtrs_slc"][j]
                    )  # same regressor for each slice
                    label = "%s s%d.RVT%d ;" % (label, i, j)
            if main_info["respiration_out"] != 0:
                # Resp
                for j in range(0, shape(respiration_info["phase_slice_reg"])[1]):
                    cnt += 1
                    main_info["reml_out"].append(
                        respiration_info["phase_slice_reg"][:, j, i]
                    )
                    label = "%s s%d.Resp%d ;" % (label, i, j)
            if main_info["cardiac_out"] != 0:
                # Card
                for j in range(0, shape(cardiac_info["phase_slice_reg"])[1]):
                    cnt += 1
                    main_info["reml_out"].append(
                        cardiac_info["phase_slice_reg"][:, j, i]
                    )
                    label = "%s s%d.Card%d ;" % (label, i, j)
        fid = open(("%s.slibase.1D" % main_info["prefix"]), "w")

    # remove very last ';'
    label = label[1:-2]

    savetxt(
        "%s.slibase.1D" % main_info["prefix"],
        column_stack(main_info["reml_out"]),
        fmt="%.4f",
        delimiter=" ",
        newline="\n",
        header=("%s%s" % (label, tail)),
        footer=("%s" % tailclose),
    )
    """
    fid.write('%s', label)
    fid.write('%s ', tail)
    for i in range(0, len(main_info['reml_out'])):
        fid.write('%s ', main_info['reml_out'][i, :])
        fprintf(fid, '\n ')
    fprintf(fid, '%s', tailclose)
    fclose(fid)
    """
    main_info["error"] = 0

    return main_info


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
        "-phys_json":None
    }

    if len(sys.argv) < 2:
        print(
            "You need to provide parameters. If you need help, rerun the"
            'program using the "-help" argument:'
            '\n"python RetroTS.py -help"'
        )
        quit()
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
        phys_json=opt_dict["-phys_json"]
    )
