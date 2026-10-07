#!/usr/bin/env python

# Read in and parse options for physio_calc.py
# 
# ==========================================================================

# Version/default settings are stored separately, in the same style as
# the other arch_* programs.  Use those definitions directly here, rather
# than creating a second alias layer in this option-processing library.

import sys
import os
import copy
import json
import math
import subprocess as SP
from platform import python_version_tuple

from afnipy import afni_base          as ab
from afnipy import afni_util          as UTIL
from afnipy import option_list        as OL
from afnipy import lib_physio_defs    as DEF

# ==========================================================================



# ===========================================================================
# PART_01: default parameter settings
#
# Defaults and static option-definition structures live in
# lib_physio_defs.py and are referenced directly through DEF throughout
# this option-processing library.

# --------------------------------------------------------------------------
# sundry other items

verb = 0

dent = '\n' + 5*' '

g_help_dict = {
    **DEF.DOPTS,
    'ddashline'          : '='*76,
    'ver'                : DEF.version,
    'AJM_str'            : DEF.AJM_str,
    'tikm'               : DEF.TEXT_interact_key_mouse,
    'all_prefilt_mode'   : ', '.join(DEF.all_prefilt_mode),
    'all_volbase_resp'   : DEF.all_volbase_resp,
    'all_volbase_card'   : DEF.all_volbase_card,
    'DEF_rvt_shift_list' : DEF.DEF_rvt_shift_list,
}

g_help_string = """
Overview ~1~

This program creates slice-based regressors for regressing out
components of estimated cardiac and respiratory signals, as well as
the respiration volume per time (RVT), RVTRRF and HRCRF.

Much of the calculations are based on the following papers about
estimating physiological regressors to be applied in FMRI analysis:

  Glover GH, Li TQ, Ress D (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magn Reson Med 44(1):162-7.

  Birn RM, Diamond JB, Smith MA, Bandettini PA (2006). Separating
  respiratory-variation-related fluctuations from
  neuronal-activity-related fluctuations in fMRI. Neuroimage
  31(4):1536-48.

  Birn RM, Smith MA, Jones TB, Bandettini PA (2008). The respiration
  response function: the temporal dynamics of fMRI signal fluctuations
  related to changes in respiration. Neuroimage 40(2):644-654.

  Chang C, Glover GH (2009). Relationship between respiration,
  end-tidal CO2, and BOLD signals in resting-state fMRI. Neuroimage
  47(4):1381-1393.

This code has been informed by earlier programs that estimated
RETROICOR and RVT regressors, namely 3dretroicor by Fred Tam,
RetroTS.m by Ziad Saad and RetroTS.py by J Zosky.  That being said, the
current code's implementation was written separately, to understand
the underlying processes and algorithms afresh, to modularize several
pieces, to refactorize others, and to produce more QC outputs and logs
of information.  Several steps in the eventual regressor estimation
depend on processes like peak- (and trough-) finding and outlier
rejection, which can be reasonably implemented in many ways.  We do
not expect exact matching of outcomes between this and the previous
versions.

Below, we use the following abbreviations a lot:
* "resp" refers to respiratory (breathing) input and results
* "card" refers to cardiac (heart rate) input and results

{ddashline}

Options ~1~

The options are organized by category here. The general expectation is
for the user to use one or more options from the "Main ..."
categories, and the remaining options are available for additional
control, if needed. For example, the '-do_fix_* ..' options might be
very useful if there are weird features in the input physio data.

Note that some prefiltering and other processing parameters are _on_
by default, since they seem generally useful. But they could be turned
off, if preferable.


* Main input, physio datasets and parameters: at least one card or resp
  time series must be provided, along with timing information:

  -resp_file RF       :Path to one respiration data file

  -card_file CF       :Path to one cardiac data file

  -phys_file PF       :BIDS-formatted physio file in tab-separated format.
                       May be gzipped

  -phys_json PJ       :BIDS-formatted physio metadata JSON file. This is
                       required whenever -phys_file is used

  -freq F             :Physiological signal sampling frequency (in Hz)

  -start_time ST      :The start time for the physio time series, relative to
                       the initial MRI volume (in s) 
                       (def: {start_time})


* Main EPI/FMRI dataset info, which can be provided in various ways
  (easiest: provide an EPI dataset via '-dset_epi ..'):

  -dset_epi DE        :Accompanying EPI/FMRI dset to which the physio
                       regressors will be applied, for obtaining the
                       volumetric parameters (namely, dset_tr, dset_nslice,
                       dset_nt)

  -dset_tr TR         :FMRI dataset's repetition time (TR), which defines the
                       time interval between consecutive volumes (in s)

  -dset_nt NT         :Integer number of time points to have in the output
                       (should likely match FMRI dataset's number of volumes)

  -dset_nslice NS     :Integer number of slices in FMRI dataset

  -dset_slice_times DST  :Slice time values (space separated list of numbers)

  -dset_slice_pattern SP  :Slice timing pattern code. 
                       Use '-disp_all_slice_patterns Yes' to see all allowed
                       patterns. Alternatively, one can enter the filename of
                       a file containing a single column of slice times.
                       (def: {dset_slice_pattern})


* Main output options like directory and file naming:

  -out_dir OD         :Output directory name (can include path)

  -prefix P           :Prefix of output filenames, without path
                       (def: {prefix})


* Main choice of output regressor types, related control options, and formats:

  -regress_types_resp RTR
                      :Provide a list of one or more types of regressors
                       derived from the input respiratory physio data. This is
                       done by listing one or more codes from among the
                       following list: {all_volbase_resp} 
                       (def: {regress_types_resp})

  -regress_types_card RTC
                      :Provide a list of one or more types of regressors
                       derived from the input cardiac physio data. This is
                       done by listing one or more codes from among the
                       following list:   {all_volbase_card} 
                       (def: {regress_types_card})

  -rvt_shift_list RSL :Provide one or more values to specify how many and what
                       kinds of shifted copies of RVT are output as
                       regressors. Units are seconds, and including 0 may be
                       useful. Shifts could also be entered via
                       '-rvt_shift_linspace ..' 
                       (def: {DEF_rvt_shift_list})

  -rvt_shift_linspace A B N
                      :Alternative to '-rvt_shift_list ..'. Provide three
                       space-separated values (start stop N) used to determine
                       how many and what kinds of shifted copies of RVT are
                       output as regressors, according to the Python-Numpy
                       function linspace(start, stop, N). Both start and stop
                       (units of seconds) can be negative, zero or positive.
                       Including 0 may be useful.  Example params: 0 4 5,
                       which lead to shifts of 0, 1, 2, 3 and 4 sec 
                       (def: {rvt_shift_linspace}, use '-rvt_shift_list')

  -do_slibase_out DSO: Output the older style of physio output from the
                       RetroTS.py days, namely where all regressors are output
                       in a single slice-based regressor file, *slibase.1D;
                       NB: this is _not_ recommended, and only existing for 
                       comparisons to older formats.
                       Allowed values are: Yes, 1, No, 0
                       (def: '{do_slibase_out}')


* Prefiltering: these options control input physio time series pre-processing
  (NB: some of these are now ON by default, because they seem generally
  useful, but they could be turned off):

  -do_fix_nan DFN     :Fix (= replace with interpolation) any NaN values in
                       the physio time series.
                       Allowed values are: Yes, 1, No, 0 
                       (def: '{do_fix_nan}')

  -do_fix_null DFN    :Fix (= replace with interpolation) any null or missing
                       values in the physio time series.
                       Allowed values are: Yes, 1, No, 0 
                       (def: '{do_fix_null}')

  -do_fix_outliers DFO  :Fix (= replace with interpolation) any outliers in
                       the physio time series. 
                       Allowed values are: Yes, 1, No, 0
                       (def: '{do_fix_outliers}')

  -extra_fix_list EFL: List of one or more values that will also be considered
                       'bad' if they appear in the physio time series, and
                       replaced with interpolated values

  -remove_val_list RVL  :List of one or more values that will removed (not
                       interpolated: the time series will be shorter, if any
                       are found) if they appear in the physio time series;
                       this is necessary with some manufacturers' outputs, see
                       "Notes of input peculiarities," below

  -prefilt_max_freq PMF  :Allow for downsampling of the input physio time
                       series, by providing a maximum sampling frequency
                       (in Hz). This is applied just after badness checks.
                       Values <=0 mean that no downsampling will occur 
                       (def: {prefilt_max_freq})

  -prefilt_mode PM    :Filter input physio time series (after badness checks),
                       likely aiming at reducing noise; can be combined
                       usefully with prefilt_max_freq. Allowed modes:
                       {all_prefilt_mode} 
                       (def: {prefilt_mode})

  -prefilt_win_card PWC  :Window size (in s) for card time series, if
                       prefiltering input physio time series with
                       '-prefilt_mode ..'; value must be >0
                       (def: {prefilt_win_card}, only used with prefiltering)

  -prefilt_win_resp PWR  :Window size (in s) for resp time series, if
                       prefiltering input physio time series with
                       '-prefilt_mode ..'; value must be >0 
                       (def: {prefilt_win_resp}, only used with prefiltering)


* Processing parameters during some intermediate filtering stages:

  -min_bpm_resp MNR   :Set the minimum breaths per minute for respiratory proc
                       (def: {min_bpm_resp})

  -max_bpm_resp MXR   :Set the maximum breaths per minute for respiratory proc
                       (def: {max_bpm_resp})

  -min_bpm_card MNC   :Set the minimum beats per minute for cardiac proc
                       (def: {min_bpm_card})

  -max_bpm_card MXC   :Set the maximum beats per minute for cardiac proc
                       (def:{max_bpm_card})


* User interaction: users can fix/check/update peak and trough estimates:

  -do_interact DI     :Enter into interactive mode as the last stage of
                       peak/trough estimation for the physio time series.
                       Allowed values are: Yes, 1, No, 0
                       (def: '{do_interact}')


* QC images properties, which can be adjusted if needed/desired:

  -img_verb IV        :Verbosity level for saving QC images during processing,
                       by choosing one integer: 0 - Do not save graphs 1 -
                       Save end results (card and resp peaks, final RVT) 2 -
                       Save end results and intermediate steps (bandpassing,
                       peak refinement, etc.) 
                       (def: {img_verb})

  -img_figsize W H    :Figure dimensions used for QC images 
                       (def: depends on length of physio time series)

  -img_fontsize FS    :Font size used for QC images
                       (def: {img_fontsize})

  -img_line_time LT   :Maximum time duration per line in the QC images, in
                       units of sec
                       (def: {img_line_time})

  -img_fig_line NL    :Maximum number of lines per fig in the QC images
                       (def: {img_fig_line})

  -img_dot_freq DF    :Maximum number of dots per line in the QC images (to
                       save filesize and plot time), in units of dots per sec
                       (def: {img_dot_freq})

  -img_bp_max_f MF    :Maximum frequency in the bandpass QC images (i.e.,
                       upper value of x-axis), in units of Hz
                       (def: {img_bp_max_f})


* Save current peaks to a separate file:

  -save_proc_peaks    :Write out the final set of peaks indices to a text file
                       called PREFIX_LABEL_proc_peaks_00.1D ('LABEL' is
                       'card', 'resp', etc.), which is a single column of the
                       integer values
                       (def: don't write them out)

  -save_proc_troughs  :Write out the final set of trough indices to a text
                       file called PREFIX_LABEL_proc_troughs_00.1D ('LABEL' is
                       'card', 'resp', etc.), which is a single column of the
                       integer values. The file is only output for LABEL 
                       types where troughs were estimated (e.g., resp).
                       (def: don't write them out)


* Load/read in previous peaks and troughs to use:

  -load_proc_peaks_resp LPR
                      :Load in a file of resp data peaks that have been saved
                       via '-save_proc_peaks'. This file is a single column of
                       integer values, which are indices of the peak locations
                       in the processed time series

  -load_proc_troughs_resp LTR
                      :Load in a file of resp data troughs that have been
                       saved via '-save_proc_troughs'. This file is a single
                       column of integer values, which are indices of the
                       trough locations in the processed time series

  -load_proc_peaks_card LPC
                      :Load in a file of card data peaks that have been saved
                       via '-save_proc_peaks'. This file is a single column of
                       integer values, which are indices of the peak locations
                       in the processed time series


* Sundry displays of help or further text display:

  -verb V             :Integer values to control verbosity level
                       (def: {verb})

  -disp_all_slice_patterns DSP
                      :Display all allowed slice pattern names?
                       Allowed values are: Yes, 1, No, 0
                       (def: '{disp_all_slice_patterns}')

  -disp_all_opts DAO  :Display all options for this program?
                       Allowed values are: Yes, 1, No, 0 
                       (def: '{disp_all_opts}')

  -ver                :Display program version number

  -help               :Display help text in terminal

  -hview              :Display help text in a text editor (AFNI functionality)

{ddashline}

Notes on usage ~1~

Physio data inputs ~2~

  At least one of the following sets of input option sets (shown one per
  line) must be used to provide input physio data (i.e., card, resp or 
  both time series):

    -card_file CF
    -resp_file RF
    -card_file CF  -resp_file RF
    -phys_file PF  -phys_json PJ

  If the sampling frequency (units: Hz) of the physio data is not
  provided by -phys_json, then it must be provided with this opt:

    -freq F

  Additionally, the starting time of the physio data relative to the
  start of the EPI data will be assumed to be 0.0 unless another value
  is provided by the user (units: sec; any value provided by the
  user/file should be <=0); this info can be provided either via the
  -phys_json file, or by this opt:

    -start_time ST

  The following table lists the relevant sampling and starting
  parameters that could be provided from _either_ a command line
  option (ARG/OPT) or a '-phys_json ..' file's keys (JSON KEY):

{AJM_str}
  It is possible that these items could be provided by *both* the JSON
  file and the command line opt (e.g., due to JSON heterogeneity
  across a study).  In such events, this program checks to make sure
  any dually-provided values are consistent to within EPS VAL.

EPI/FMRI inputs and information ~2~

  EPI-related information is required to build regressors: TR, number
  of slices, number of time points, and slice timing info.  It is
  easiest to provide these items by just providing the EPI/FMRI dset
  directly with:

    -dset_epi DE

  But users could also provide that info separately, with:

    -dset_tr           TR
    -dset_nslice       NS
    -dset_nt           NT
    -dset_slice_times  DST    _or_   -dset_slice_pattern  SP

{ddashline}

Prefiltering options for the physio time series ~1~

  Many physio time series contain noisy spikes or occasional blips.
  Since most physio processing algorithms rely on peak-/trough-finding,
  such spikes can be highly problematic. The effects of these can be
  reduced during processing with some "prefiltering".  At present, this
  includes using a moving median filter along the time series, to try to
  remove spiky things that are likely nonphysiological. This can be
  implemented by using this opt+arg:

      -prefilt_mode median

  An additional decision to make then becomes what width of filter to
  apply. That is, over how many points should the median be
  calculated?  The choice should balance being large enough to be
  stable with being small enough to not remove useful features (like
  real peaks, troughs or other time series changes). Users can specify
  a time interval for the card and resp data separately, because each
  has a different expected time scale of variability (and experimental
  design can affect this choice, as well).  So, the user can use:

      -prefilt_win_card  PWC
      -prefilt_win_resp  PWR

  ... providing PW? values in units of seconds. Because this seems
  generally useful to use, at present prefiltering is _on_ by default
  (with default parameters listed above in the options section). To
  turn these off, one could use:  '-prefilt_mode none'.

  Finally, physio time series are acquired with a variety of sampling
  frequencies.  These can easily range from 50 Hz to 2000 Hz (or more).
  That means 50 (or 2000) point estimates per second---which is a lot
  for most applications.  Consider that typical FMRI sampling intervals
  are TR = 1-2 sec or so, meaning that they have 0.5 or 1 point estimates
  per sec.  Additionally, many (human) cardiac cycles are roughly of
  order 1 per sec or so, and (human) respiration is at a much slower
  rate.  All this is to say, having a highly sampled physio time series
  can be unnecessary for most practical applications and analyses.  We
  can reduce computational cost and processing time by downsampling it
  near the beginning of processing. This would be done by specifying a
  max sampling frequency MAX_F for the input data, to downsample to (or 
  near to), via: 

      -prefilt_max_freq  PMF

  In general, at least for human applications, it seems hard to see
  why one would need more than 50 physio measures per
  second. Therefore, this sampling-related prefiltering is also on by
  default. To turn this off, one could use: '-prefilt_max_freq -1'.

  NB: The above prefiltering actions are all be applied _after_ any
  steps performed with '-do_fix_* ..' options, which help to find (and
  hopefully remove) other kinds of problematic, non-phyiosological
  features in the input time series.
 
{ddashline}

User interaction for peak/trough editing ~1~

  This program includes functionality whereby the user can directly
  edit the peaks and troughs that have estimated. This includes
  adding, deleting or moving the points around, with the built-in
  constraint of keeping the points on the displayed physio time series
  line. It's actually kind of fun.

  To enter interactive mode during the runtime of the program, add this
  option:

    -do_interact Yes

  Then, at some stage during the processing, a Matplotlib panel will pop
  up, showing estimated troughs and/or peaks, which the user can edit if
  desired. Upon closing the pop-up panel, the final locations of
  peaks/troughs are kept and used for the remainder of the code's run.

  {tikm}

  For more on the Matplotlib panel navigation keypresses and tips, see:
  https://matplotlib.org/3.2.2/users/navigation_toolbar.html

  At present, there is no "undo" functionality. If you accidentally
  delete a point, you can add one back, or vice versa.

{ddashline}

Reload peaks/troughs from earlier physio_calc.py run ~1~

  It is possible to save estimated peak and trough values to a text file
  with this program, using:

     -save_proc_peaks
     -save_proc_troughs

  respectively.  These options tell the program to write *.1D files
  that contain the integer indices of the peaks or troughts within the
  processed time series. The output files are automatically named in
  the output directory.

  It is also possible to re-load those text files of integer indices
  back into the program, which might be useful when further editing of
  peaks/troughs is necessary, for example, via '-do_interact Yes'.

  To do this, you should basically run the same physio_calc.py command
  you initially ran to create the time points (same inputs, same
  '-prefilt_* ..' opts, etc.)  but perhaps with different output
  directory and/or prefix, and add the one or more of the following
  options:

     -load_proc_peaks_resp    LPR
     -load_proc_troughs_resp  LTR
     -load_proc_peaks_card    LPC

  Each of these takes a single argument, which is the appropriate file
  name to read in.

  **Note 1: it is important to keep all the same processing options
    from the original command even when reading in already-generated
    peaks and troughs. This is because prefiltering and start_time
    options can affect how the read-in indices are interpreted. It is
    important to maintain consistency. To facilitate recalling the
    earlier options, there should be a 'PREFIX_pc_cmd.tcsh' file that is
    saved among the outputs of a given physio_calc.py run.

  **Note 2: while reusing the same processing options is advised when
    loading in earlier outputs to use, it might help reduce confusion
    between those prior physio_calc.py outputs and the new results by
    changing the '-out_dir ..' and '-prefix ..'.

{ddashline}

Notes on scanner-related peculiarities ~1~

  With Siemens physiological monitoring, values of 5000, 5003 and 6000
  can be used as trigger events to mark the beginning or end of
  something, like the beginning of a TR.  Based on the Siemens Matlab
  programs, the encoded meanings are:

      5000 = cardiac pulse on
      5003 = cardiac pulse off
      6000 = cardiac pulse off
      6002 = phys recording on
      6003 = phys recording off

  Moreover, it appears that these numbers are *inserted* into the
  series, in which case, the specified 500? and 600? values should be
  *removed* rather than replaced by an interpolation of the two adjacent
  values.  To do this, you can use something like the following option
  syntax:

      -remove_val_list 5000 5003 6000 6002 6003

{ddashline}

Outputs overview ~1~

  The following are possible outputs to running this program. The
  number of images created varies based on user-controlled options. The
  *resp* files are only output if respiratory signal information were
  input, and similarly for *card* files with cardiac input.


Outputs in: OUT_DIR/ ~2~

  The main output files are the following text files, which contain
  regressors that can be provided to afni_proc.py for FMRI processing:

    PREFIX_physio_regress_slice.1D 
      :slice-based regressor file, which can be made up of any of the
       following card and/or resp regressors: retro.  This can be
       provided to afni_proc.py via '-ricor_regs ..'.

    PREFIX_physio_regress_volume.1D 
      :volume-based regressor file, which can be made up of any of the
       following card and/or resp regressors: rvt, rvtrrf, hrcrf.
       This can be provided to afni_proc.py via '-********** ..'.

  The following subdirectories contain useful supplementary information:

    PREFIX_physio_images/     
      :subdir holding QC images (see below)

    PREFIX_physio_extras/
      :subdir holding additional text files of interest (see below)


Outputs in: OUT_DIR/PREFIX_physio_extras/ ~2~

  Supplementary text files that may be of user. These include recording
  input options, as well as QC summaries of peak/trough properties.

    PREFIX_resp_review.txt
      :summary statistics and info for resp proc

    PREFIX_card_review.txt
      :summary statistics and info for card proc

    PREFIX_pc_cmd.tcsh
      :log/copy of the command used

    PREFIX_info.json 
      :reference dictionary of all command inputs after interpreting
       user options and integrating default values

  The following text files are only output when using the
  '-save_proc_peaks' and/or '-save_proc_troughs' option(s):

    PREFIX_card_peaks_00.1D
      :1D column file of peak indices for card data corresponding to
       card*final_peaks*svg image.

    PREFIX_resp_peaks_00.1D
      :1D column file of peak indices for resp data corresponding to
       resp*final_peaks*svg image.

    PREFIX_resp_troughs_00.1D
      :1D column file of trough indices for resp data, corresponding
       to resp*final_peaks*svg image.


Outputs in: OUT_DIR/PREFIX_physio_images/ ~2~

  QC images related to finding peaks and troughs, phase estimation,
  and regressor creation.  The number of files here will vary based on
  input data, regressors created, and verbosity of intermediate
  processing.  The main output QC images are:

    PREFIX_the_regressors_*.svg
      :QC images of all regressors estimated by physio_calc.py

    PREFIX_card_10_final*peaks*.svg
    PREFIX_resp_10_final*peaks*.svg
      :QC images of final peak estimation for card data processing.
       Colorbands highlight longer (red) and shorter (blue) intervals,
       compared to median (white). For more details, see 'How to
       interpret coloration...', below.

  The following intermediate QC images are only output with '-img_verb 2'
  or higher:

    PREFIX_card_0*.svg
    PREFIX_resp_0*.svg      
      :QC images of intermediate peak estimation for card and resp
       data processing

    PREFIX_card_bandpass*.svg
    PREFIX_resp_bandapss*.svg
      :QC images of intermediate peak/trough estimation during an
       initial bandpass stage; includes image of Fourier-transform
       spectrum, as well as bandpassed time series

    PREFIX_card_20_*.svg
    PREFIX_resp_20_*.svg
      :QC images of intermediate stages in either RVT- or CRF-based
       estimations

{ddashline}

How to interpret coloration in *final_peaks* images ~1~

  The QC images contain images that are supposed to be helpful in
  interpreting the data.  Here are some notes on various aspects.

  When viewing physio time series, the interval that overlaps the FMRI
  dataset in time has a white background, while any parts that do not
  have a light gray background.  Essentially, only the overlap regions
  should affect regressor estimation---the parts in gray are useful to
  have as realistic boundary conditions, though.

  Peaks are always shown as downward pointing triangles, and troughs are
  upward pointing triangles.

  When viewing "final" peak and trough images, there will be color bands
  made of red/white/blue rectangles shown in the subplots.  These
  highlight the relative duration of a given interpeak interval (top
  band in the subplot) and/or intertrough interval (bottom intervals),
  relative to their median values across the entire time series.
  Namely:

     white : interval matches median
     blue  : interval is shorter than median (darker blue -> much shorter)
     red   : interval is longer than median (darker red -> much longer)

  The more intense colors mean that the interval is further than the median,
  counting in standard deviations of the interpeak or intertrough intervals.  
  This coloration is meant to help point out variability across time: this
  might reflect natural variability of the physio time series, or possibly
  draw attention to a QC issue like an out-of-place or missing extremum 
  (which could be edited in "interactive mode").

A note on previous physio estimation with RetroTS.py ~1~

  Note that the older RetroTS.py program for deriving physio-based
  regressors in AFNI output only a single slice-based file, the
  "*slibase.1D" file.  This contained even the non-slicewise defined
  regressors, simply entered in a slicewise format.  But the slicewise
  regression must be done before any other processing, rather than as
  part of the main regress block processing.  So, the present program
  outputs separate files for slice-based and volume-wise regressors, so
  that as many as possible volumetric regressors can be applied more
  appropriately in the regress block stage.

  *If* you would like the older format of all-physio-regressors-in-a-single-
  slicewise-file, you can add an option here for that:

     -do_slibase_out Yes 

  ... but this is not recommended and primarily exists just for testing
  purposes.  If you do want the older *_slibase.1D file output, it
  should _not_ be simultaneously included with the other
  *physio_regress*.1D files estimated here.

{ddashline}

Examples ~1~

  1. Basic input example, only cardiac data
    
    physio_calc.py                                                     \\
        -card_file           physiopy/test000c                         \\
        -freq                400                                       \\
        -dset_epi            DSET_MRI                                  \\
        -out_dir             OUT_DIR                                   \\
        -prefix              PREFIX

  2. Input of file+json pair; no EPI data input, so those parameters are
     provided separately; some badness checks are also added:
    
    physio_calc.py                                                     \\
        -phys_file           physiopy/test003c.tsv.gz                  \\
        -phys_json           physiopy/test003c.json                    \\
        -dset_tr             2.2                                       \\
        -dset_nt             34                                        \\
        -dset_nslice         34                                        \\
        -dset_slice_pattern  alt+z                                     \\
        -do_fix_nan          Yes                                       \\
        -extra_fix_list      5000                                      \\
        -out_dir             OUT_DIR                                   \\
        -prefix              PREFIX

  3. Another example, with both card and resp input; the types of regressors
     to estimate are also specified:

    physio_calc.py                                                     \\
        -card_file           sub-005_ses-01_task-rest_physio-ECG.txt   \\
        -resp_file           sub-005_ses-01_task-rest_physio-Resp.txt  \\
        -freq                50                                        \\
        -dset_tr             2.2                                       \\
        -dset_nt             220                                       \\
        -dset_nslice         33                                        \\
        -dset_slice_pattern  alt+z                                     \\
        -regress_types_resp  retro rvt                                 \\
        -regress_types_card  retro hrcrf                               \\
        -out_dir             OUT_DIR                                   \\
        -prefix              PREFIX
    
{ddashline}

written by: Peter Lauren, Richard Reynolds, Daniel Glen, and Paul 
            Taylor (SSCC, NIMH, NIH, USA)

additional input: Josh Dean and Dan Handwerker (SFIM, NIMH, NIH, USA)

{ddashline}
""".format(**g_help_dict)


# ========================================================================== 
# PART_02: helper functions

def parser_to_dict(inopts, argv, verb=0):
    """Convert AFNI OptionList parsing to dictionaries of key (=opt) and
value pairs.  There is an intermediate dictionary of volumetric-related
items that is output first, because parsing it is complicated.  That will
later get merged with the main args_dict.

This preserves the dictionary interface used by the rest of physio_calc.py;
only the front-end option parser has changed.

Parameters
----------
inopts : InOpts
    object that defines and parses the valid AFNI-style options
argv : list
    full command line, including program name
verb : int
    verbosity level whilst working

Returns
-------
args_dict : dict
    dictionary whose keys are option names and values are the user-entered
    values (which might still need separate interpreting later).  Empty
    on parsing failure.
vol_dict : dict
    secondary dictionary of volume-related option values

    """

    is_bad = inopts.process_options(argv)
    if is_bad :
        return {}, {}

    args_dict = copy.deepcopy(inopts.args_dict)
    args_dict['argv'] = copy.deepcopy(argv)

    # pop out the volumetric-related items, to parse separately, and
    # return later
    vol_dict = {}
    for key in DEF.vol_key_list:
        if key in args_dict :
            val = args_dict.pop(key)
            vol_dict[key] = copy.deepcopy(val)

    return args_dict, vol_dict

def compare_keys_in_two_dicts(A, B, nameA=None, nameB=None):
    """Compare sets of keys between two dictionaries A and B (which could
have names nameA and nameB, when referring to them in output text).

Parameters
----------
A : dict
    some dictionary
B : dict
    some dictionary
nameA : str
    optional name for referring to dict A when reporting
nameB : str
    optional name for referring to dict B when reporting

Returns
-------
DIFF_KEYS : int
    integer encoding whether there is a difference in the set of keys
    in A and B:
      0 -> no difference
      1 -> difference

"""

    DIFF_KEYS = 0

    if not(nameA) :    nameA = 'A'
    if not(nameB) :    nameB = 'B'

    # simple count
    na = len(A)
    nb = len(B)

    if na != nb :
        DIFF_KEYS = 1
        msg = "number of keys in {} '{}' ".format(nameA, na)
        msg+= "and in {} '{}' do not match.\n".format(nameB, nb)
        msg+= "This is a programming/dev issue."
        ab.EP1(msg)

    # detailed check per opt class
    setA = set(A.keys())
    setB  = set(B.keys())

    missA = list(setB.difference(setA))
    missB = list(setA.difference(setB))

    if len(missA) :
        DIFF_KEYS = 1
        missA.sort()
        str_missA = ', '.join(missA)
        msg = "keys in {} that are missing in {}:\n".format(nameB, nameA)
        msg+= "{}".format(str_missA)
        ab.EP1(msg)

    if len(missB) :
        DIFF_KEYS = 1
        missB.sort()
        str_missB = ', '.join(missB)
        msg = "keys in {} that are missing in {}:\n".format(nameA, nameB)
        msg+= "{}".format(str_missB)
        ab.EP1(msg)

    return DIFF_KEYS

def check_simple_opts_to_exit(args_dict, inopts):
    """Check for simple options, after which to exit, such as help/hview,
ver, disp all slice patterns, etc.  The inopts object is an included
arg because it has the help info to display, if called for.

Parameters
----------
args_dict : dict
    a dictionary of input options (=keys) and their values
inopts : InOpts
    object from parsing program options

Returns
-------
int : int
    return 1 on the first instance of a simple opt being found, else
    return 0

    """

    # if nothing or help opt, show help
    if args_dict['help'] :
        inopts.print_help()
        return 1

    # hview functionality
    if args_dict['hview'] :
        prog = inopts.prog 
        acmd = 'apsearch -view_prog_help {}'.format( prog )
        check_info = SP.Popen(acmd, shell=True, 
                              stdout=SP.PIPE, stderr=SP.PIPE,
                              close_fds=True)
        so, se = check_info.communicate() 

        if se :
            ab.WP("no hview?")
            # act like simple help disp
            inopts.print_help()

        return 1

    # display program version
    if args_dict['ver'] :
        print(DEF.version, flush=True)
        return 1

    # slice patterns, from list somewhere
    if args_dict['disp_all_slice_patterns'] :
        lll = UTIL.g_valid_slice_patterns
        lll.sort()
        print("{}".format('\n'.join(lll)))
        return 1

    # all opts for this program, via DEF list
    if args_dict['disp_all_opts'] :
        tmp = disp_keys_sorted(DEF.DOPTS, pref='-')
        if not(tmp) :
            return 1

    # getting here means a mistake happened
    return 0

def disp_keys_sorted(D, pref=''):
    """Display a list of sorted keys from dict D, one per line.  Can
include a prefix (left-concatenated string) for each.

Parameters
----------
D : dict
    some dictionary
pref : str
    some string to be left-concatenated to each key

Returns
-------
SF : int
    return 0 if successful
    """

    if type(D) != dict :
        ab.EP("input D must be dict")
    
    all_key = [pref+str(x) for x in get_keys_sorted(D)]
    print('{}'.format('\n'.join(all_key)))

    return 0

def get_keys_sorted(D):
    """Return a list of sorted keys from dict D.

Parameters
----------
D : dict
    some dictionary

Returns
-------
L : list
    a sorted list (of keys from D)

"""

    if type(D) != dict :
        ab.EP("input D must be dict")

    L = list(D.keys())
    L.sort()
    return L


def read_slice_pattern_file(fname, verb=0):
    """Read in a text file fname that contains a slice timing pattern.
That pattern must be either a single row or column of (floating point)
numbers.

Parameters
----------
fname : str
    filename of slice timing info to be read in

Returns
-------
all_sli : list (of floats)
    a list of floats, the slice times

"""

    BAD_RETURN = []

    if not(os.path.isfile(fname)) :
        ab.EP1("{} is not a file (to read for slice timing)".format(fname))
        return BAD_RETURN

    try:
        fff = open(fname, 'r')
        X   = fff.readlines()
        fff.close()
    except:
        ab.EP1("opening {} (to read for slice timing)".format(fname))
        return BAD_RETURN

    # get list of floats, and length of each row when reading
    N = 0
    all_sli = []
    all_len = []
    for ii in range(len(X)):
        row = X[ii]
        rlist = row.split()
        if rlist :
            N+= 1
            try:
                # use extend so all_sli stays 1D
                all_sli.extend([float(rr) for rr in rlist])
                all_len.append(len(rlist))
            except:
                msg = "badness in float conversion within "
                msg+= "slice timing file {}\n".format(fname)
                msg+= "Bad line {} is: '{}'".format(ii+1, row)
                ab.EP1(msg)
                return BAD_RETURN
    
    if not(N) :
        ab.EP1("no data in slice timing file {}?".format(fname))
        return BAD_RETURN

    M = max(all_len)  # (max) number of cols

    if verb :
        ab.IP("Slice timing file {} has {} rows and {} columns"
              "".format(fname, N, M))

    if not(N==1 or M==1) :
        msg = "dset_slice_pattern file {} is not Nx1 or 1xN.\n".format(fname)
        msg+= "Its dims of data are: nrow={}, max_ncol={}".format(N, M)
        ab.EP1(msg)
        return BAD_RETURN

    # finally, after more work than we thought...
    return all_sli

def read_json_to_dict(fname):
    """Read in a text file fname that is supposed to be a JSON file and
output a dictionary.

Parameters
----------
fname : str
    JSON filename

Returns
-------
jdict : dict
    dictionary form of the JSON

"""
    
    BAD_RETURN = {}

    if not(os.path.isfile(fname)) :
        ab.EP1("cannot read file: {}".format(fname))
        return BAD_RETURN

    with open(fname, 'rt') as fff:
        jdict = json.load(fff)

    return jdict

def read_dset_epi_to_dict(fname, verb=None):
    """Extra properties from what should be a valid EPI dset and output a
dictionary.

Parameters
----------
fname : str
    EPI dset filename

Returns
-------
epi_dict : dict
    dictionary of necessary EPI info

"""
    
    BAD_RETURN = {}

    if not(os.path.isfile(fname)) :
        ab.EP1("cannot read file: {}".format(fname))
        return BAD_RETURN

    # initialize, and store dset name
    epi_dict = {}
    epi_dict['dset_epi'] = fname

    # get simple dset info, which should/must exist
    cmd = '''3dinfo -n4 -tr {}'''.format(fname)
    com = ab.shell_com(cmd, capture=1, save_hist=0)
    stat = com.run()
    lll = com.so[0].split()
    try:
        nk = int(lll[2])
        nv = int(lll[3])
        tr = float(lll[4])
        
        epi_dict['dset_nslice']  = int(lll[2])
        epi_dict['dset_nt']      = int(lll[3])
        epi_dict['dset_tr']      = float(lll[4])
    except:
        ab.WP("problem extracting info from dset_epi")
        return BAD_RETURN 

    # try getting timing info from this dset, which might not exist
    cmd  = '''3dinfo -slice_timing {}'''.format(fname)
    com  = ab.shell_com(cmd, capture=1, save_hist=0)
    stat = com.run()
    lll  = [float(x) for x in com.so[0].strip().split('|')]
    
    nslice = len(lll)
    if nk != nslice :
        msg = "number of dset_slice_times in header ({}) ".format(nslice)
        msg+= "does not match slice count in k-direction ({})".format(nk)
        ab.EP(msg)

    epi_dict['dset_slice_times'] = copy.deepcopy(lll)

    return epi_dict

def reconcile_phys_json_with_args(jdict, args_dict, verb=None):
    """Go through the jdict created from the command-line given phys_json,
and pull out any pieces of information like sampling freq, etc. that
should be added to the args_dict (dict of all command line opts
used). These pieces of info can get added to the args_dict, but they
also have to be checked against possible conflict from command line opts.

Matched partners include:
{AJM_str}

The jdict itself gets added to the output args_dict2 as a value for
a new key 'phys_json_dict', for possible later use.

Parameters
----------
jdict : dict
    a dictionary from the phys_json input from the user
args_dict : dict
    the args_dict of input opts.

Returns
-------
BAD_RECON : int
    integer signifying bad reconiliation of files (> 1) or a
    non-problematic one (= 0)
args_dict2 : dict
    copy of input args_dict, that may be augmented with other info.

"""

    BAD_RETURN = 1, {}

    if not(jdict) :    return BAD_RETURN

    args_dict2 = copy.deepcopy(args_dict)

    # add known items that might be present
    for aj_match in DEF.ALL_AJ_MATCH:
        aname   = aj_match[0]
        jname   = aj_match[1]
        eps_val = aj_match[2]
        if jname in jdict :
            val_json = jdict[jname]
            val_args = args_dict2[aname]
            if val_args != None :
                if abs(val_json - val_args) > eps_val :
                    msg = "inconsistent JSON '{}' ".format(jname)
                    msg+= "= {} and ".format(val_json)
                    msg+= "input arg '{}' = {}".format(aname, val_args)
                    ab.EP1(msg)
                    return BAD_RETURN
                else:
                    msg = "Reconciled: input info provided in two ways, "
                    msg+= "which is OK because they are consistent (at "
                    msg+= "eps={}):\n".format(eps_val)
                    msg+= "   JSON '{}' = {} and ".format(jname, val_json)
                    msg+= "input arg '{}' = {}".format(aname, val_args)
                    ab.IP(msg)
            else:
                args_dict2[aname] = val_json

    # and add in this JSON to the args dict, so it is only ever read
    # in once
    args_dict2['phys_json_dict'] = copy.deepcopy(jdict)

    return 0, args_dict2

# ... and needed with the above to insert a variable into the docstring
reconcile_phys_json_with_args.__doc__ = \
    reconcile_phys_json_with_args.__doc__.format(AJM_str=DEF.AJM_str)

def interpret_rvt_shift_linspace_opts(A, B, C):
    """Three numbers are used to determine the shifts for RVT when
processing.  These get interpreted as Numpy's linspace(A, B, C).  Verify
that any entered set (which might come from the user) works fine.

Parameters
----------
A : float
    start of range
B : float
    end of range (inclusive)
C : int
    number of steps in range

Returns
-------
is_fail : bool
    False if everything is OK;  True otherwise
shift_list : list
    1D list of (floating point) shift values

"""

    try :
        shift_list = imitation_linspace_mini(A, B, C)
    except:
        return True, [] 
    
    return False, shift_list

def imitation_linspace_mini(A,B,C):
    """Do simple linspace-like calcs, so we can remove numpy
dependency. This returns a list, not a numpy array, though

Parameters
----------
A : float
    start of range
B : float
    end of range (inclusive)
C : int
    number of steps in range

Returns
-------
L : list
    list of (float) values

"""

    if not(C > 0) :
        raise ValueError("C must be positive")
    if C == 1 :
        return [A]

    # at this point, we know C>1 ...

    denom = C - 1
    delta = (B - A)/denom

    L = [A+delta*ii for ii in range(C)]

    return L


# ========================================================================== 
# PART_03: setup AFNI-style help and arguments/options



# ========================================================================== 
# setup AFNI-style option parser

# ============================ the args/opts ===============================

class InOpts:
    """Object for storing and parsing physio_calc.py command line inputs."""

    def __init__(self, prog=None):

        self.status     = 0
        self.prog       = prog if prog else os.path.basename(sys.argv[0])
        self.valid_opts = OL.OptionList('valid opts')
        self.user_opts  = None
        self.args_dict  = copy.deepcopy(DEF.DOPTS)

        # Keep descriptions and simple parsing types together with each option.
        self.odict      = {}
        self.opt_kind   = {}
        self.opt_npar   = {}

        self.init_options()

    def add_opt(self, opt, npar, kind, helpstr):
        """Add one option, and retain its help/parsing metadata."""

        name = '-' + opt
        self.valid_opts.add_opt(name, npar, [], helpstr=helpstr)
        self.odict[opt] = helpstr
        self.opt_kind[opt] = kind
        self.opt_npar[opt] = npar

    def init_options(self):
        """Prepare the set of all options."""

        self.add_opt('resp_file', 1, 'str',
                     'respiration input file')

        self.add_opt('card_file', 1, 'str',
                     'cardiac input file')

        self.add_opt('phys_file', 1, 'str',
                     'BIDS physio input file')

        self.add_opt('phys_json', 1, 'str',
                     'BIDS physio JSON file')

        self.add_opt('freq', 1, 'float',
                     'physio sampling frequency')

        self.add_opt('start_time', 1, 'float',
                     'physio start time relative to MRI')

        self.add_opt('out_dir', 1, 'str',
                     'output directory')

        self.add_opt('prefix', 1, 'str',
                     'output filename prefix')

        self.add_opt('dset_epi', 1, 'str',
                     'EPI dataset for MRI timing info')

        self.add_opt('dset_tr', 1, 'float',
                     'MRI repetition time')

        self.add_opt('dset_nt', 1, 'int',
                     'MRI number of time points')

        self.add_opt('dset_nslice', 1, 'int',
                     'MRI number of slices')

        self.add_opt('dset_slice_times', -1, 'list',
                     'MRI slice timing values')

        self.add_opt('dset_slice_pattern', 1, 'str',
                     'MRI slice timing pattern')

        self.add_opt('prefilt_max_freq', 1, 'float',
                     'maximum physio sampling frequency')

        self.add_opt('prefilt_mode', 1, 'str',
                     'physio prefiltering mode')

        self.add_opt('prefilt_win_card', 1, 'float',
                     'cardiac prefilter window')

        self.add_opt('prefilt_win_resp', 1, 'float',
                     'respiration prefilter window')

        self.add_opt('do_fix_nan', 1, 'yesno',
                     'interpolate NaN values')

        self.add_opt('do_fix_null', 1, 'yesno',
                     'interpolate null values')

        self.add_opt('do_fix_outliers', 1, 'yesno',
                     'interpolate outlier values')

        self.add_opt('extra_fix_list', -1, 'list',
                     'additional values to interpolate')

        self.add_opt('remove_val_list', -1, 'list',
                     'values to remove from physio data')

        self.add_opt('do_interact', 1, 'yesno',
                     'enable interactive peak/trough editing')

        self.add_opt('do_slibase_out', 1, 'yesno',
                     'write old-style slibase regressors')

        self.add_opt('regress_types_resp', -1, 'list',
                     'respiration regressor types')

        self.add_opt('regress_types_card', -1, 'list',
                     'cardiac regressor types')

        self.add_opt('rvt_shift_list', -1, 'list',
                     'RVT shift values')

        self.add_opt('rvt_shift_linspace', 3, 'list',
                     'RVT shift linspace parameters')

        self.add_opt('min_bpm_resp', 1, 'float',
                     'minimum respiration rate')

        self.add_opt('max_bpm_resp', 1, 'float',
                     'maximum respiration rate')

        self.add_opt('min_bpm_card', 1, 'float',
                     'minimum cardiac rate')

        self.add_opt('max_bpm_card', 1, 'float',
                     'maximum cardiac rate')

        self.add_opt('img_verb', 1, 'int',
                     'QC image verbosity')

        self.add_opt('img_figsize', 2, 'list',
                     'QC image dimensions')

        self.add_opt('img_fontsize', 1, 'float',
                     'QC image font size')

        self.add_opt('img_line_time', 1, 'float',
                     'QC image time per line')

        self.add_opt('img_fig_line', 1, 'int',
                     'QC image lines per figure')

        self.add_opt('img_dot_freq', 1, 'float',
                     'QC image point density')

        self.add_opt('img_bp_max_f', 1, 'float',
                     'QC bandpass maximum frequency')

        self.add_opt('save_proc_peaks', 0, 'flag',
                     'save final peak indices')

        self.add_opt('save_proc_troughs', 0, 'flag',
                     'save final trough indices')

        self.add_opt('load_proc_peaks_resp', 1, 'str',
                     'load respiration peak indices')

        self.add_opt('load_proc_troughs_resp', 1, 'str',
                     'load respiration trough indices')

        self.add_opt('load_proc_peaks_card', 1, 'str',
                     'load cardiac peak indices')

        self.add_opt('verb', 1, 'int',
                     'verbosity level')

        self.add_opt('disp_all_slice_patterns', 1, 'yesno',
                     'show valid slice timing patterns')

        self.add_opt('disp_all_opts', 1, 'yesno',
                     'show valid options')

        self.add_opt('ver', 0, 'flag',
                     'show program version')

        self.add_opt('help', 0, 'flag',
                     'show full help')

        self.add_opt('hview', 0, 'flag',
                     'view full help in editor')

        # programming check: are all opts and defaults accounted for?
        have_diff_keys = compare_keys_in_two_dicts(
            self.odict, DEF.DOPTS, nameA='odict', nameB='DEF.DOPTS')
        if have_diff_keys :
            ab.EP("exiting because of opt name setup failure")

        return 0

    def print_help(self):
        """Display the full program help text."""

        print(g_help_string)

    def process_options(self, argv):
        """Read command line options and populate args_dict."""

        self.valid_opts.check_special_opts(argv)

        self.user_opts = OL.read_options(argv, self.valid_opts)
        uopts = self.user_opts
        if not(uopts) :
            return -1

        err_base = "Problem interpreting use of opt: "

        for opt in uopts.olist:
            key  = opt.name.lstrip('-')
            kind = self.opt_kind[key]

            if kind == 'flag' :
                self.args_dict[key] = True
                continue

            if kind == 'str' :
                val, err = uopts.get_string_opt('', opt=opt)
            elif kind == 'yesno' :
                val, err = uopts.get_string_opt('', opt=opt)
            elif kind == 'int' :
                val, err = uopts.get_type_opt(int, '', opt=opt)
            elif kind == 'float' :
                val, err = uopts.get_type_opt(float, '', opt=opt)
            elif kind == 'list' :
                val, err = uopts.get_string_list('', opt=opt)
                if val is not None :
                    # Preserve the historical parser_to_dict behavior: the
                    # downstream interpretation code receives a whitespace-
                    # separated string for user-entered multi-value options.
                    val = ' '.join(val)
            else:
                ab.EP1("Unknown parser type for option: {}".format(opt.name))
                return -1

            if val is None or err :
                ab.EP(err_base + opt.name)
                return -1

            self.args_dict[key] = val

        # As in the archimedes_* option processing, keep Yes/1 and No/0
        # user-facing values for -do_* and -disp_* options, but convert them
        # to Python bools before the rest of physio_calc.py uses args_dict.
        for key in self.args_dict :
            if key.startswith('do_') or key.startswith('disp_') :
                val = UTIL.convert_to_bool_yn10(self.args_dict[key])
                if val is None :
                    ab.EP("option '-{}' requires one of: Yes, 1, No, 0"
                          "".format(key))
                    return -1
                self.args_dict[key] = val

        return 0

# =========================================================================
# PART_04: process opts slightly, checking if all required ones are
# present (~things with None for def), and also note that some pieces
# of info can come from the phys_json, so open and interpret that (and
# check against conflicts!)

def check_required_args(args_dict):
    """A primary check of the user provided arguments (as well as the
potential update of some).  

This first checks whether everything that *should* have been provided
actually *was* provided, namely for each opt that has a default None
value (as well as some strings that could have been set to '').  In
many cases, a failure here will lead to a direct exit.

This also might read in a JSON of info and use that to populate
args_dict items, updating them.  It will also then save that JSON to a
new field, phys_json_dict.

This function might likely grow over time as more options get added or
usage changes.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
args_dict2 : dict
    A potentially updated args_dict (if no updates happen, then just
    a copy of the original).

    """

    verb = args_dict['verb']

    args_dict2 = copy.deepcopy(args_dict)

    if not(args_dict2['card_file'] or args_dict2['resp_file']) and \
       not(args_dict2['phys_file'] and args_dict2['phys_json']) :
        msg = "no physio inputs provided. Allowed physio inputs:\n"
        msg+= "A) '-card_file ..', '-resp_file ..' or both.\n"
        msg+= "B) '-phys_file ..' and '-phys_json ..'."
        ab.EP(msg)

    # Do not allow the two physio input styles to be mixed.  Individual
    # card/resp files are one input mode; phys_file + phys_json is another.
    have_solo_phys = bool(args_dict2['card_file'] or
                          args_dict2['resp_file'])
    have_bids_phys = bool(args_dict2['phys_file'] or
                          args_dict2['phys_json'])
    if have_solo_phys and have_bids_phys :
        msg = "cannot mix physio input modes:\n"
        msg+= "A) '-card_file ..' and/or '-resp_file ..'\n"
        msg+= "B) '-phys_file ..' with '-phys_json ..'"
        ab.EP(msg)

    # for any filename that was provided, check if it actually exists
    # (dset_slice_pattern possible filename checked below)
    all_fopt = [ 'card_file', 'resp_file', 'phys_file', 'phys_json',
                 'dset_epi', 'load_proc_peaks_card', 
                 'load_proc_peaks_resp', 'load_proc_troughs_resp' ]
    for fopt in all_fopt:
        if args_dict2[fopt] != None :
            if not(os.path.isfile(args_dict2[fopt])) :
                ab.EP("no {} '{}'".format(fopt, args_dict2[fopt]))

    # deal with json for a couple facets: getting args_dict2 info, and
    # making sure there are no inconsistencies (in case both JSON and opt
    # provide the same info)
    if args_dict2['phys_json'] :
        jdict = read_json_to_dict(args_dict2['phys_json'])
        if not(jdict) :
            ab.EP("JSON unreadable or empty")

        # jdict info can get added to args_dict; also want to make sure it
        # does not conflict, if items were entered with other opts
        check_fail, args_dict2 = reconcile_phys_json_with_args(jdict, 
                                                               args_dict2)
        if check_fail :
            ab.EP("issue using the JSON")

     # different ways to provide volumetric EPI info, and ONE must be used
    if not( args_dict2['dset_tr'] ) :
        ab.EP("must provide '-dset_tr ..' information")

    if not(args_dict2['dset_nslice']) :
        ab.EP("must provide '-dset_nslice ..' information")

    if not(args_dict2['dset_nt']) :
        ab.EP("must provide '-dset_nt ..' information")

    if not(args_dict2['freq']) :
        ab.EP("must provide '-freq ..' information")

    if not(args_dict2['prefix']) :
        ab.EP("must provide '-prefix ..' information")

    if not(args_dict2['out_dir']) :
        ab.EP("must provide '-out_dir ..' information")

    if not(args_dict2['dset_slice_times']) :
        ab.EP("must provide slice timing info in some way")

    return args_dict2

def interpret_vol_info(vol_dict, verb=1):
    """This function takes a dictionary of all command line-entered items
that are/might be related to MRI acquisition, and will: 1)
expand/calculate any info (like slice times); 2) check info for
conflicts; 3) reduce info down (= reconcile items) where it is OK to do so.

The output of this function can be merged into the main arg_dicts.

Parameters
----------
vol_dict : dict
    The dictionary of volume-related arguments and option values,
    before any parsing

Returns
-------
vol_dict2 : dict
    A potentially updated vol_dict (if no updates happen, then just
    a copy of the original).  But likely this will have been parsed in 
    important ways

    """

    BAD_RETURN = {}

    # initialize what will be output dictionary
    vol_dict2 = {}

    # first see if a dset_epi has been entered, which might have a lot
    # of important information (and add to output dict); any/all EPI
    # info *might* be in here (at least at the time of writing)
    if 'dset_epi' in vol_dict and vol_dict['dset_epi'] :
        vol_dict2 = read_dset_epi_to_dict(vol_dict['dset_epi'], verb=verb)
        if not(vol_dict2) :
            ab.EP("dset_epi unreadable or problematic")
    else:
        vol_dict2['dset_epi'] = None

    # Make sure all expected volume-info keys exist, even if they were
    # not supplied via -dset_epi or other command line options.
    # This allows downstream reconciliation/checking to handle missing
    # values explicitly rather than producing a KeyError.
    for key in DEF.vol_key_list :
        if key not in vol_dict2 :
            vol_dict2[key] = None

    # then check scalar values about volume properties from simple
    # command line opts; try to reconcile or add each (and add to
    # output dict)
    ndiff, nmerge = compare_dict_nums_with_merge(vol_dict2, vol_dict, 
                                                 L=DEF.ALL_EPIM_MATCH, 
                                                 do_merge=True, verb=1)
    if ndiff :
        ab.EP("inconsistent dset_epi and command line info")

    # next/finally, check about slice timing specifically, which might
    # use existing scalar values (from dset or cmd line, which would
    # be in vol_dict2 now) 
    if vol_dict['dset_slice_times'] and vol_dict['dset_slice_pattern'] :
        msg = "must use only one of either dset_slice_times or "
        msg+= "dset_slice_pattern"
        ab.EP(msg)

    if vol_dict['dset_slice_times'] :
        # the input cmd line string has not been split yet; interpret
        # this single string to be a list of floats, and replace it in
        # the dict

        L = vol_dict['dset_slice_times'].split()
        try:
            # replace single string of slice times with list of
            # numerical values
            dset_slice_times = [float(ll) for ll in L]
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        except:
            ab.EP("interpreting dset_slice_times from cmd line")

    elif vol_dict['dset_slice_pattern'] :
        # if pattern, check if it is allowed; elif it is a file, check
        # if it exists *and* use it to fill in
        # vol_dict['dset_slice_times']; else, whine.  Use any supplementary
        # info from the output dict, but edit vol_dict slice times in
        # place

        pat = vol_dict['dset_slice_pattern']
        if pat in UTIL.g_valid_slice_patterns :
            ab.IP("Slice pattern from cmd line: '{}'".format(pat))
            # check with vol info in vol_dict2 (not in vol_dict) bc
            # vol_dict2 should be the merged superset of info
            dset_slice_times = UTIL.slice_pattern_to_timing(pat, 
                                                       vol_dict2['dset_nslice'],
                                                       vol_dict2['dset_tr'])
            if not(dset_slice_times) :
                ab.EP("could not convert slice pattern to timing")
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        elif os.path.isfile(pat) :
            ab.IP("Found dset_slice_pattern '{}' exists as a file".format(pat))
            dset_slice_times = read_slice_pattern_file(pat, verb=verb)
            if not(dset_slice_times) :
                ab.EP("translate slice pattern file to timing")
            vol_dict['dset_slice_times'] = copy.deepcopy(dset_slice_times)
        else:
            msg = "could not match provided dset_slice_pattern "
            msg+= "'{}' as either a recognized pattern or file".format(pat)
            ab.EP(msg)

    # ... and now that we might have explicit slice times in vol_dict,
    # reconcile any vol['dset_slice_times'] with vol_dict2['dset_slice_times']
    if 'dset_slice_times' in vol_dict and \
       vol_dict['dset_slice_times'] != None :
        if 'dset_slice_times' in vol_dict2 and \
           vol_dict2['dset_slice_times'] != None :
            # try to reconcile
            ndiff = compare_list_items( vol_dict['dset_slice_times'],
                                        vol_dict2['dset_slice_times'],
                                        eps=DEF.EPS_TH )
            if ndiff :
                ab.EP("inconsistent slice times entered")
        else:
            # nothing to reconcile, just copy over
            vol_dict2['dset_slice_times'] = \
                copy.deepcopy(vol_dict['dset_slice_times'])
    else:
        # I believe these cases hold
        if 'dset_slice_times' in vol_dict2 and \
           vol_dict2['dset_slice_times'] != None :
            pass
        else:
            # this is a boring one, which will probably lead to an
            # error exit in a downstream check
            vol_dict2['dset_slice_times'] = None

    # If both quantities are available, the number of explicit slice
    # times must match the number of slices.
    if vol_dict2['dset_slice_times'] is not None and \
       vol_dict2['dset_nslice'] is not None :

        ntimes = len(vol_dict2['dset_slice_times'])
        nslice = vol_dict2['dset_nslice']

        if ntimes != nslice :
            msg = "number of slice timing values ({}) ".format(ntimes)
            msg+= "does not match dset_nslice ({})".format(nslice)
            ab.EP(msg)

    # Slice timing values should all be in the half-open interval: [0, TR)
    if vol_dict2['dset_slice_times'] is not None and \
       vol_dict2['dset_tr'] is not None :

        sli_times = vol_dict2['dset_slice_times']
        tr        = vol_dict2['dset_tr']

        for ii, stime in enumerate(sli_times) :
            if not(math.isfinite(stime)) or stime < 0.0 or stime >= tr :
                msg = "slice timing value [{}] = {} ".format(ii, stime)
                msg+= "is outside the allowed range [0, TR), "
                msg+= "where TR = {}".format(tr)
                ab.EP(msg)

    # copy this over just for informational purposes
    if 'dset_slice_pattern' in vol_dict :
        vol_dict2['dset_slice_pattern'] = \
            copy.deepcopy(vol_dict['dset_slice_pattern'])

    return vol_dict2


def compare_dict_nums_with_merge(A, B, L=[], do_merge=True, verb=1):
    """Let A and B be dictionaries, each of arbitrary size.  Let L be a
list of lists, where each sublist contains the name of a possible key
(which would have a numerical value) and a tolerance for differencing
its value between A and B.  This function goes through L and 1) sees
if that element exists in B; 2) if yes, sees if it also exists in A;
3) if yes, sees if they are the same to within allowed tolerance and
(if do_merge==True) else if no, adds that value to B.

Parameters
----------
A : dict
    dict of arbitrary size, which main contain numerical elements
    listed in L
B : dict
    dict of arbitrary size, which main contain numerical elements
    listed in L
L: list
    list of 2-element sublists, each of which contains the str name of
    a parameter to search for in A and B, and a numerical value eps
    representing the tolerance for checking the element's difference
do_merge : bool
    if True, add any found key-value pair in B whose key is in L to A,
    if the tolerance allows or if it isn't already in A; otherwise, don't
    try merging

Returns
-------
ndiff : int
    number of different elements
nmerge : int
    number of merged elements (may not be useful, but heck, just return it)
    """

    ndiff  = 0
    nmerge = 0

    for row in L:
        ele = row[0]
        eps = row[1]
        if ele in B and B[ele] != None :
            if ele in A and A[ele] != None :
                valA = A[ele]
                valB = B[ele]
                if abs(A[ele] - B[ele]) > eps :
                    if verb :
                        msg = "Difference in dictionary elements:\n"
                        msg+= "eps = {}\n".format(eps)
                        msg+= "A[{}] = {}\n".format(ele, A[ele])
                        msg+= "B[{}] = {}".format(ele, B[ele])
                        ab.WP(msg)
                    ndiff+= 1
                    # ... and cannot merge
                else:
                    if verb :
                        msg = "Reconciled dictionary elements:\n"
                        msg+= "eps = {}\n".format(eps)
                        msg+= "A[{}] = {}\n".format(ele, A[ele])
                        msg+= "B[{}] = {}".format(ele, B[ele])
                        ab.IP(msg)
                    # ... and no need to merge
            else:
                if do_merge :
                    A[ele] = B[ele]
                    nmerge+= 1

    return ndiff, nmerge


def compare_list_items(A, B, eps=0.0, verb=1):
    """Let A and B be 1D lists of numerical values, each of length N.
This function goes through and compares elements of the same index,
checking whether values are equal within a tolerance of eps.  Output
the number of different elements (so, returning 0 means they are the
same).

Parameters
----------
A : list
    1D list of numerical-valued elements
B : list
    1D list of numerical-valued elements
eps: float
    tolerance of elementwise differences

Returns
-------
ndiff : int
    number of different elements, to within tolerance eps

    """

    N = len(A)
    if len(B) != N :
        msg = "unequal length lists:\n"
        msg+= "len(A) = {}\n".format(N)
        msg+= "len(B) = {}".format(len(B))
        ab.EP(msg)

    ndiff = 0
    for ii in range(N):
        if abs(A[ii] - B[ii]) > eps :
            ndiff+= 1
            if verb :
                msg = "Difference in list elements:\n"
                msg+= "A[{}] = {}\n".format(ii, A[ii])
                msg+= "B[{}] = {}".format(ii, B[ii])
                ab.WP(msg)

    return ndiff

def check_multiple_rvt_shift_opts(args_dict):
    """Can only use at most one '-rvt_shift*' opt.  Simplest to check for
that at one time.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
is_bad : int
    Value is 1 if badness from arg usage, 0 otherwise.

"""

    is_bad = 0
    count  = 0
    lopt   = []

    # go through and check, building a list
    for opt in DEF.all_rvt_opt :
        if args_dict[opt] :
            lopt.append('-' + opt)
            count+= 1

    # bad if more than one opt was used
    if count > 1 :
        msg = "more than one '-rvt_shift_*' opt was used:\n"
        msg+= "{}\n".format(' '.join(lopt))
        msg+= "... but at most only one can be."
        ab.EP1(msg)
        is_bad = 1

    return is_bad

def interpret_args(args_dict):

    """Interpret the user provided arguments (and potentially update
some).  This also checks that entered values are valid, and in some
cases a failure here will lead to a direct exit.

This function might likely grow over time as more options get added or
usage changes.

Parameters
----------
args_dict : dict
    The dictionary of arguments and option values.

Returns
-------
args_dict2 : dict
    A potentially updated args_dict (if no updates happen, then just
    a copy of the original).

    """

    verb = args_dict['verb']

    args_dict2 = copy.deepcopy(args_dict)


    if args_dict2['out_dir'] :
        # remove any rightward '/'.  Maybe also check for pre-existing
        # out_dir?
        args_dict2['out_dir'] = args_dict2['out_dir'].rstrip('/')

    if args_dict2['start_time'] == None :
        ab.IP("No start time provided; will assume it is 0.0.")
        args_dict2['start_time'] = 0.0
    elif not(math.isfinite(args_dict2['start_time'])) or \
         args_dict2['start_time'] > 0.0 :
        msg = "start_time must be <= 0.0, "
        msg+= "not: {}".format(args_dict2['start_time'])
        ab.EP(msg)

    if args_dict2['extra_fix_list'] :
        # Interpret string to be list of ints or floats. NB: written
        # as floats, but these are OK for equality checks in this case
        IS_BAD = 0

        L = args_dict2['extra_fix_list'].split()
        try:
            efl = [float(ll) for ll in L]
            args_dict2['extra_fix_list'] = copy.deepcopy(efl)
        except:
            ab.EP1("interpreting extra_fix_list")
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)

    if args_dict2['remove_val_list'] :
        # Interpret string to be list of ints or floats. NB: written
        # as floats, but these are OK for equality checks in this case
        IS_BAD = 0

        L = args_dict2['remove_val_list'].split()
        try:
            lll = [float(ll) for ll in L]
            args_dict2['remove_val_list'] = copy.deepcopy(lll)
        except:
            ab.EP1("interpreting remove_val_list")
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)

    # for card inputs, which volume-based regressors will be created? 
    # There will always be at least one value in this list
    if args_dict2['regress_types_card'] :
        IS_BAD = 0

        # defaults, which don't change if 'NONE' is in the list here
        args_dict2['do_calc_retro-card'] = False
        args_dict2['do_calc_hr']         = False
        args_dict2['do_calc_hrcrf']      = False
        args_dict2['do_out_retro-card']  = False
        args_dict2['do_out_hr']          = False
        args_dict2['do_out_hrcrf']       = False

        L = args_dict2['regress_types_card'].split()

        # check for non-allowed item
        for val in L :
            if val not in DEF.list_volbase_card :
                msg = "unrecognized type in '-regress_types_card ..' args:\n"
                msg+= "{}\n".format(val)
                msg+= "Valid types: {}".format(DEF.all_volbase_card)
                ab.EP1(msg)
                IS_BAD = 1

        if 'NONE' in L :
            if len(L) > 1 :
                msg = "with '-regress_types_card ..' args:\n"
                msg+= "'{}'\n".format(args_dict2['regress_types_card'])
                msg+= "Cannot mix 'NONE' with other types"
                ab.EP1(msg)
                IS_BAD = 1

            #  NB: if here, no need to change def switch values above

        if 'retro' in L :
            args_dict2['do_calc_retro-card'] = True
            args_dict2['do_out_retro-card']  = True

        if 'hrcrf' in L :
            # retro and hr calc needed here
            args_dict2['do_calc_retro-card'] = True
            args_dict2['do_calc_hr']         = True
            args_dict2['do_calc_hrcrf']      = True
            args_dict2['do_out_hrcrf']       = True

        if IS_BAD :  sys.exit(1)

    # for resp inputs, which volume-based regressors will be created? 
    # There will always be at least one value in this list
    # NB: check this BEFORE the RVT considerations are parsed; it will 
    # control lots of switches for calculations and outputs 
    if args_dict2['regress_types_resp'] :
        IS_BAD = 0

        # defaults, which don't change if 'NONE' is in the list here
        args_dict2['do_calc_retro-resp'] = False
        args_dict2['do_calc_rvt']        = False
        args_dict2['do_calc_rvtrrf']     = False
        args_dict2['do_out_retro-resp']  = False
        args_dict2['do_out_rvt']         = False
        args_dict2['do_out_rvtrrf']      = False

        L = args_dict2['regress_types_resp'].split()

        # check for non-allowed item
        for val in L :
            if val not in DEF.list_volbase_resp :
                msg = "unrecognized type in '-regress_types_resp ..' args:\n"
                msg+= "{}\n".format(val)
                msg+= "Valid types: {}".format(DEF.all_volbase_resp)

                ab.EP1(msg)
                IS_BAD = 1

        if 'NONE' in L :
            if len(L) > 1 :
                msg = "with '-regress_types_resp ..' args:\n"
                msg+= "'{}'\n".format(args_dict2['regress_types_resp'])
                msg+= "Cannot mix 'NONE' with other types"
                ab.EP1(msg)
                IS_BAD = 1

            #  NB: if here, no need to change def switch values above

        if 'retro' in L :
            args_dict2['do_calc_retro-resp'] = True
            args_dict2['do_out_retro-resp']  = True

        if 'rvt' in L :
            # retro calc needed here
            args_dict2['do_calc_retro-resp'] = True
            args_dict2['do_calc_rvt']        = True
            args_dict2['do_out_rvt']         = True

        if 'rvtrrf' in L :
            # retro and RVT calc needed here
            args_dict2['do_calc_retro-resp'] = True
            args_dict2['do_calc_rvt']        = True
            args_dict2['do_calc_rvtrrf']     = True
            args_dict2['do_out_rvtrrf']      = True

        if IS_BAD :  sys.exit(1)

    # RVT considerations: several branches here; first check if >1 opt
    # was used, which is bad; then check for any other opts.  When
    # this full conditional is complete, we should have our shift
    # list, one way or another
    if check_multiple_rvt_shift_opts(args_dict2) :
        sys.exit(1)
    elif args_dict2['rvt_shift_list'] != None :
        # RVT branch A: direct list of shifts from user to make into array

        IS_BAD = 0

        if not(args_dict2['do_out_rvt']) :
            msg = "RVT calcs were turned off in opt proc; "
            msg+= "you cannot then use -rvt_shift_list"
            ab.EP1(msg)
            IS_BAD = 1

        L = args_dict2['rvt_shift_list'].split()

        try:
            # make list of floats
            shift_list = [float(ll) for ll in L]
            # and copy list of shifts
            args_dict2['rvt_shift_list'] = copy.deepcopy(shift_list) 
        except:
            msg = "interpreting '-rvt_shift_list ..' args: "
            msg+= "'{}'".format(args_dict2['rvt_shift_list'])
            ab.EP1(msg)
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)
    elif args_dict2['rvt_shift_linspace'] :
        # RVT branch B: linspace pars from user, list of ints or floats

        IS_BAD = 0

        if not(args_dict2['do_out_rvt']) :
            msg = "RVT calcs were turned off in opt proc; "
            msg+= "you cannot then use -rvt_shift_linspace"
            ab.EP1(msg)
            IS_BAD = 1

        # make sure -rvt_shift_list had 3 entries
        L = args_dict2['rvt_shift_linspace'].split()
        if len(L) != 3 :
            ab.EP1("'-rvt_shift_linspace ..' takes exactly 3 values.")
            IS_BAD = 1

        try:
            # first 2 numbers can be int or float, but last must be int
            lll     = [float(ll) for ll in L]
            # ... and check about lossyness in converting last val to int
            if int(lll[-1]) != lll[-1] :
                msg = "the 3rd number via '-rvt_shift_linspace ..' "
                msg+= "must be an int: {}".format(L)
                ab.EP1(msg)
                IS_BAD = 1

            lll[-1] = int(lll[-1])

            # These 3 values get interpreted as (start, stop, N);
            # verify that this is a legit expression
            is_fail, all_shift = \
                interpret_rvt_shift_linspace_opts(lll[0], lll[1], lll[2])
            IS_BAD+= is_fail

            # copy original params in place
            args_dict2['rvt_shift_linspace'] = copy.deepcopy(lll) 
            # and copy arr of shifts
            args_dict2['rvt_shift_list'] = copy.deepcopy(all_shift) 
        except:
            msg = "interpreting '-rvt_shift_linspace ..' args: "
            msg+= "'{}'".format(args_dict2['rvt_shift_linspace'])
            ab.EP1(msg)
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)
    else:
        # RVT branch D: use default shifts
        L   = DEF.DEF_rvt_shift_list.split()
        args_dict2['rvt_shift_list'] = [float(ll) for ll in L]

    if args_dict2['img_figsize'] :
        # Interpret string to be list of floats.
        IS_BAD = 0

        L = args_dict2['img_figsize'].split()
        try:
            aaa = [float(ll) for ll in L]
            args_dict2['img_figsize'] = copy.deepcopy(aaa)
        except:
            ab.EP1("interpreting img_figsize")
            IS_BAD = 1

        if IS_BAD :  sys.exit(1)

    if '/' in args_dict2['prefix'] :
        msg = "Cannot have path information in '-prefix ..'\n"
        msg+= "Use '-out_dir ..' for path info instead"
        ab.EP(msg)

    if args_dict2['prefilt_mode'] :
        # there are only certain allowed values
        IS_BAD = 0
        if args_dict2['prefilt_mode'] not in DEF.all_prefilt_mode :
           IS_BAD = 1
        if IS_BAD :  sys.exit(1)

    # when loading in previous resp peaks/troughs, must use *both*
    if int(bool(args_dict2['load_proc_peaks_resp'])) + \
       int(bool(args_dict2['load_proc_troughs_resp'])) == 1 :
        msg = "If you load in previously processed resp peaks or\n"
        msg+= "troughs, you must load in *both* files via:\n"
        msg+= "-load_proc_peaks_resp ..\n"
        msg+= "-load_proc_troughs_resp .."
        ab.EP(msg)

    # check many numerical inputs for being >=0 or >0; probably leave this
    # one as last in this function
    IS_BAD = 0
    for quant in DEF.all_quant_ge_zero:
        if args_dict2[quant] == None :
            msg = "Must provide a value for '{}' via options".format(quant)
            ab.EP1(msg)
            IS_BAD+= 1
        elif not(math.isfinite(args_dict2[quant])) or args_dict2[quant] < 0 :
            msg = "Provided '{}' value ".format(quant)
            msg+= "({}) not allowed to be <0".format(args_dict2[quant])
            ab.EP1(msg)
            IS_BAD+= 1
    for quant in DEF.all_quant_gt_zero:
        if args_dict2[quant] == None :
            msg = "Must provide a value for '{}' via options.".format(quant)
            ab.EP1(msg)
            IS_BAD+= 1
        elif not(math.isfinite(args_dict2[quant])) or args_dict2[quant] <= 0 :
            msg = "Provided '{}' value ".format(quant)
            msg+= "({}) not allowed to be <=0".format(args_dict2[quant])
            ab.EP1(msg)
            IS_BAD+= 1
    if IS_BAD :
        sys.exit(4)

    # successful navigation
    return args_dict2

def add_info_to_dict(A, B):
    """Simply loop over keys in B and add them to A. Assume nothing
overlaps (might add checks for that later...).

Parameters
----------
A : dict
    The base dictionary to which new items get entered
B : dict
    The source dictionary from which new items are obtained

Returns
-------
C : dict
    The new dict of values (copy of A with B added)
"""

    C = copy.deepcopy(A)
    for key in B.keys() :
        C[key] = B[key]

    return C


def main_option_processing(argv):
    """This is the main function for running the physio processing
program.  It executes the first round of option processing: 
1) Make sure that all necessary inputs are present and accounted for.
2) Check all entered files exist.
3) Check many of the option values for validity (e.g., being >0, if
   appropriate).

This does not do the main physio processing itself, just is the first
step/gateway for doing so.

In some 'simple option' use cases, running this program simply
displays terminal text (or opens a text editor for displaying the
help) and then quits.  Otherwise, it returns a dictionary of checked
argument values.

Typically call this from another main program like:
    args_dict = lib_retro_opts.main_option_processing(sys.argv)

Parameters
----------
argv : list
    The list of strings that defines the command line input.

Returns
-------
args_dict : dict
    Dictionary of arg values from the command line. *Or* nothing might
    be returned, and some text is simply displayed (depending on the
    options used).

    """

    # ---------------------------------------------------------------------
    # case of 0 opt used: just show help and quit

    # We do this check separately, because in this case, each item in
    # args_dict is *not* a list containing what we want, but just is that
    # thing itself.  That confuses the means for checking it, so treat
    # that differently.

    if len(argv) == 1 :
        inopts = InOpts(prog=os.path.basename(argv[0]))
        inopts.print_help()
        sys.exit(0)

    # ---------------------------------------------------------------------
    # case of >=1 opt being used: parse!

    # get all opts and values as a main dict and secondary/temporary
    # dict of volume-related items, separately
    inopts = InOpts(prog=os.path.basename(argv[0]))
    args_dict, vol_dict = parser_to_dict(inopts, argv)
    if not args_dict:
        # process_options has already reported the invalid option/value.
        # Do not pass its empty error result to the simple-option checks.
        sys.exit(1)

    # check for simple-simple cases with a quick exit: ver, help, etc.
    have_simple_opt = check_simple_opts_to_exit(args_dict, inopts)
    if have_simple_opt :
        sys.exit(0)

    # parse/merge the volumetric dict items, which can have some
    # tangled relations.
    vol_dict  = interpret_vol_info(vol_dict, verb=args_dict['verb'])
    args_dict = add_info_to_dict(args_dict, vol_dict)

    # real work to be done now: check that all required inputs were
    # provided, and do a bit of verification of some of their
    # attributes (e.g., that files exist, values that should be >=0
    # are, etc.)
    args_dict = check_required_args(args_dict)
    args_dict = interpret_args(args_dict)

    return args_dict


# ================================ main =====================================

if __name__ == "__main__":

    args_dict = main_option_processing(sys.argv)
    ab.IP("DONE.  Goodbye.")

    sys.exit(0)
