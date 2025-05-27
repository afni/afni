#!/usr/bin/env python

# create some testing time series, for both card and resp

# at the moment, this isn't done in a finalized way with an object
# class, which would likely simplify things...

# note that for some tests, some parts of calcs might fail/give weird
# results. For example, when cardiac rate is perfectly constant, the
# HRCRF calc will give all zeros.  That is OK, this is just testing,
# not real data.

import sys, json
import numpy             as np
import matplotlib.pyplot as plt

# ===========================================================================

np.random.seed(7)

# ===========================================================================


if __name__ == "__main__" :

    oscript = "run_physio_testing.tcsh"

    otxt = """#!/bin/tcsh

set here    = $PWD
set dir_dat = .

"""

    # ------------------------------------------------------------
    exnum = "001a"
    print("++ Ex. {}, which is constant across time".format(exnum))
    
    fbase = "example_physio_" + exnum
    ftsv  = fbase + ".tsv"
    fjson = fbase + ".json"

    # sampling info
    samp_freq = 50.0                  # Hz
    samp_rate = 1./samp_freq          # s
    start     = 0.0                   # s
    T         = 200.0                 # s
    N         = int(T * samp_freq)
    t         = np.arange(start, start+T, samp_rate)

    # card 
    fcard = 0.5
    xc = np.sin(2*np.pi*fcard*t)

    # resp
    fresp = 0.2
    xr = np.sin(2*np.pi*fresp*t)

    if 0 :
        plt.plot(t, xc)
        plt.plot(t, xr)
        plt.show()

    # write JSON output
    jdict = {
        "Columns" : ["respiratory", "cardiac"],
        "SamplingFrequency" : samp_freq,
        "StartTime" : start,
    }
    ojson = json.dumps( jdict, indent=4 )
    fff = open( fjson, "w" )
    fff.write( ojson )
    fff.close()

    # write TSV data output
    fff = open( ftsv, "w" )
    for ii in range(N):
        sss = "{:7.4f} \t{:7.4f}\n".format(xr[ii], xc[ii])
        fff.write( sss )
    fff.close()

    otxt+= """
echo "++ run Ex. {exnum}"

set odir = odir_{exnnn}
\mkdir -p ${{odir}}

physio_calc.py                                                               \\
    -phys_file           {ftsv} \\
    -phys_json           {fjson} \\
    -dset_nslice         1                                                   \\
    -dset_tr             2.0                                                 \\
    -dset_nt             100                                                 \\
    -dset_slice_pattern  alt+z                                               \\
    -out_dir             ${{odir}}  \\
    -prefix              out_{exnum}  \\
    -img_verb            3                                                   \\
    -verb                6                                                   \\
    -save_proc_peaks                                                         \\
    -save_proc_troughs                                                       \\
    -prefilt_max_freq    50                                                  \\
    -prefilt_mode        median                                              \\
    -img_figsize         8.5 10                                              \\
    -img_line_time       60                                                  \\
    -img_fig_line        10                                                  \\
    -do_extend_bp_resp                                                       \\
    -volbase_types_resp  rvt rvtrrf                                          \\
    -volbase_types_card  hrcrf                                               \\
    |& tee log_{exnum}.txt

""".format(exnum=exnum, ftsv=ftsv, fjson=fjson, 
           exnnn="".join([x for x in exnum if x.isnumeric()]), # num exnum
           )

    # ------------------------------------------------------------
    exnum = "001b"
    print("++ Ex. {} (same as 001a, but lower samp freq)".format(exnum))

    fbase = "example_physio_" + exnum
    ftsv  = fbase + ".tsv"
    fjson = fbase + ".json"

    # sampling info
    samp_freq = 25.0                  # Hz
    samp_rate = 1./samp_freq          # s
    start     = 0.0                   # s
    T         = 200.0                 # s
    N         = int(T * samp_freq)
    t         = np.arange(start, start+T, samp_rate)

    # card 
    fcard = 0.5
    xc = np.sin(2*np.pi*fcard*t)

    # resp
    fresp = 0.2
    xr = np.sin(2*np.pi*fresp*t)

    if 0 :
        plt.plot(t, xc)
        plt.plot(t, xr)
        plt.show()

    # write JSON output
    jdict = {
        "Columns" : ["respiratory", "cardiac"],
        "SamplingFrequency" : samp_freq,
        "StartTime" : start,
    }
    ojson = json.dumps( jdict, indent=4 )
    fff = open( fjson, "w" )
    fff.write( ojson )
    fff.close()

    # write TSV data output
    fff = open( ftsv, "w" )
    for ii in range(N):
        sss = "{:7.4f} \t{:7.4f}\n".format(xr[ii], xc[ii])
        fff.write( sss )
    fff.close()

    otxt+= """
echo "++ run Ex. {exnum}"

set odir = odir_{exnnn}
\mkdir -p ${{odir}}

physio_calc.py                                                               \\
    -phys_file           {ftsv} \\
    -phys_json           {fjson} \\
    -dset_nslice         1                                                   \\
    -dset_tr             2.0                                                 \\
    -dset_nt             100                                                 \\
    -dset_slice_pattern  alt+z                                               \\
    -out_dir             ${{odir}}                                           \\
    -prefix              out_{exnum}                                         \\
    -img_verb            3                                                   \\
    -verb                3                                                   \\
    -save_proc_peaks                                                         \\
    -save_proc_troughs                                                       \\
    -prefilt_max_freq    50                                                  \\
    -prefilt_mode        median                                              \\
    -img_figsize         8.5 10                                              \\
    -img_line_time       60                                                  \\
    -img_fig_line        10                                                  \\
    -do_extend_bp_resp                                                       \\
    -volbase_types_resp  rvt rvtrrf                                          \\
    -volbase_types_card  hrcrf                                               \\
    |& tee log_{exnum}.txt

""".format(exnum=exnum, ftsv=ftsv, fjson=fjson, 
           exnnn="".join([x for x in exnum if x.isnumeric()]), # num exnum
           )


    # ------------------------------------------------------------
    exnum = "002a"
    print("++ Ex. {}, which is nonconstant across time".format(exnum))
    
    fbase = "example_physio_" + exnum
    ftsv  = fbase + ".tsv"
    fjson = fbase + ".json"

    # sampling info
    samp_freq = 50.0                  # Hz
    samp_rate = 1./samp_freq          # s
    start     = 0.0                   # s
    T         = 200.0                 # s
    N         = int(T * samp_freq)
    t         = np.arange(start, start+T, samp_rate)

    # card 
    fcard = 0.5
    # perturb freq
    #fcard_pert = 0.01*(np.random.random(N)-0.5)*fcard + fcard
    fcard_pert = np.linspace(-0.1, 0.1, N)*fcard + fcard
    xc = np.sin(2*np.pi*fcard_pert*t)

    # resp
    fresp = 0.2
    # perturb freq
    #fresp_pert = 0.01*(np.random.random(N)-0.5)*fresp + fresp
    fresp_pert = np.linspace(-0.1, 0.1, N)*fresp + fresp
    xr = np.sin(2*np.pi*fresp_pert*t)

    if 0 :
        plt.plot(t, xc)
        plt.plot(t, xr)
        plt.show()

    # write JSON output
    jdict = {
        "Columns" : ["respiratory", "cardiac"],
        "SamplingFrequency" : samp_freq,
        "StartTime" : start,
    }
    ojson = json.dumps( jdict, indent=4 )
    fff = open( fjson, "w" )
    fff.write( ojson )
    fff.close()

    # write TSV data output
    fff = open( ftsv, "w" )
    for ii in range(N):
        sss = "{:7.4f} \t{:7.4f}\n".format(xr[ii], xc[ii])
        fff.write( sss )
    fff.close()

    otxt+= """
echo "++ run Ex. {exnum}"

set odir = odir_{exnnn}
\mkdir -p ${{odir}}

physio_calc.py                                                               \\
    -phys_file           {ftsv} \\
    -phys_json           {fjson} \\
    -dset_nslice         1                                                   \\
    -dset_tr             2.0                                                 \\
    -dset_nt             100                                                 \\
    -dset_slice_pattern  alt+z                                               \\
    -out_dir             ${{odir}}  \\
    -prefix              out_{exnum}  \\
    -img_verb            3                                                   \\
    -verb                6                                                   \\
    -save_proc_peaks                                                         \\
    -save_proc_troughs                                                       \\
    -prefilt_max_freq    50                                                  \\
    -prefilt_mode        median                                              \\
    -img_figsize         8.5 10                                              \\
    -img_line_time       60                                                  \\
    -img_fig_line        10                                                  \\
    -do_extend_bp_resp                                                       \\
    -volbase_types_resp  rvt rvtrrf                                          \\
    -volbase_types_card  hrcrf                                               \\
    |& tee log_{exnum}.txt

""".format(exnum=exnum, ftsv=ftsv, fjson=fjson, 
           exnnn="".join([x for x in exnum if x.isnumeric()]), # num exnum
           )




    # =========================================================

    fff = open( oscript, "w" )
    fff.write( otxt )
    fff.close()




