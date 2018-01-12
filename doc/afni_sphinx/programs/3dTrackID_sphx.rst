.. contents:: 
    :depth: 4 

*********
3dTrackID
*********

.. code-block:: none

    
      FACTID-based tractography code, from Taylor, Cho, Lin and Biswal (2012),
      and part of FATCAT (Taylor & Saad, 2013) in AFNI. Version 2.1 (Jan. 2014),
      written by PA Taylor and ZS Saad.
    
      Estimate locations of WM associated with target ROIs, particularly between
      pairs of GM in a network;  can process several networks in a given run.
    
      Now does both single tract propagation per voxel (as per DTI) and 
      multi-directional tracking (as in HARDI-type models). Many extra files can
      be loaded in for getting quantitative stats in WM-ROIs, mostly done via
      search from entered prefixes. Many more switches and options are available
      to the user to control the tracking (yay!).
      Track display capabilities in SUMA have been boosted and continue to rise
      quickly (all courtesy of ZS Saad).
    
    ****************************************************************************
    + NOTE that this program runs in three separate modes, each with its own
       subset of commandline options and outputs:
       $ 3dTrackID -mode {DET | MINIP | PROB} ... 
       where     DET   -> deterministic tracking,
                 MINIP -> mini-probabilistic tracking,
                 PROB  -> (full) probabilistic tracking.
       So, for example, DET and MINIP produce pretty track-image output,
       while PROB only provides volumes; MINIP and PROB make use of
       tensor uncertainty to produce more robust results than DET; all
       produce quantitative statistical output of WM-ROIs; etc. In some cases,
       using a combination of all three might even be variously useful in a
       particular study.
    ****************************************************************************
      For DTI, this program reads in tensor-related data from, e.g., 3dDWItoDTI,
      and also uses results from 3dDWUncert for uncertainty measures when
      necessary.
    
      For HARDI, this program reads in the direction vectors and WM-proxy map 
      (such as the diffusion anisotropy coefficient, GFA) created by any source-
      right now, there's no HARDI modeler in AFNI. Currently known sources which
      are reasonably straightforward to use include DSI-Studio (Yeh et al.,
      2010) and Diffusion Toolkit (Wang et al., 2007). An example script of
      outputting Qball model data as NIFTI output from the former software is
      included in the FATCAT demo set.
    
      ...And on that note, it is highly recommended for users to check out the
      FATCAT demo set, which can be downloaded and unwrapped simply from the
      commandline:
      $ @Install_FATCAT_Demo
      In that demo are data, a number of scripts, and more detailed descriptions
      for using 3dTrackID, as well as other programs in the FATCAT litter.
      Recommended to always check that one has the most uptodate version.
    
    ****************************************************************************
    
    + INPUT NOTES:
      NETWORK MAPS, for any '-mode' of track, given as a single- or multi-brik
       file via '-netrois':
       Each target ROI is defined by the set of voxels with a given integer >0.
       Target ROI labels do not have to be purely consecutive.
    
      Note on vocabulary, dual usage of 'ROI': an (input) network is made up of
       *target ROIs*, between/among which one wants to find WM connections; so,
       3dTrackID outputs locations and stats on those calculated *WM-ROIs*.
    
    ****************************************************************************
    
    + OUTPUTS, all named using '-prefix INPREF'; somewhat dependent on tracking
               mode being utilized ('-mode {DET | MINIP | PROB}').
               Because multiple networks can be input simultaneously as a multi-
               brik '-netrois ROIS' file, the output prefix will also have a
               numerical designation of its network, matching to the brik of
               the ROIS file: thus, INPREF_000* goes with ROIS[0], INPREF_001*
               with ROIS[1] (if present), etc. This applies with all types of
               output files, now described:
      1) *INDIMAP*  BRIK files (output in ALL modes).
         For each network with N_ROI target ROIs, this is a N_ROI+1 brik file.
         0th brick contains the number of tracts per voxel which passed through
         at least one target ROI in that network (and in '-mode PROB', this
         number has been thresholded-- see 'alg_Thresh_Frac' below).
         If the target ROIs are consecutively labelled from 1 to N_ROI, then:
           Each i-th brick (i running from 1 to N_ROI) contains the voxels
           through which tracks hitting that i-th target passed; the value of
           each voxel is the number of tracks passing through that location.
         Else, then:
           Each i-th brick contains the voxels through which the tracks
           hitting the j-th target passed (where j may or may not equal i; the
           value of j is recorded in the brick label:  OR_roi_'j').  The target
           ROI connectivity is recorded increasing order of 'j'.
         For single-ROI inputs (such as a single wholebrain ROI), only the
           [0] brick is output (because [1] would be redundant).
      2) *PAIRMAP*  BRIK files (output in ALL modes).
         (-> This has altered slightly at the end of June, 2014! No longer using
         2^i notation-- made simpler for reading, assuming individual connection
         information for calculations was likely obtained more easily with 
         '-dump_rois {AFNI | BOTH | AFNI_MAP}...)
         For each network with N_ROI target ROIs, this is a N_ROI+1 brik file.
         0th brick contains a binary mask of voxels through which passed a
         supra-threshold number of tracks (more than 0 for '-mode {DET | MINIP}'
         and more than the user-defined threshold for '-mode PROB') between any
         pair of target ROIs in that network (by default, these tracks have been
         trimmed to only run between ROIs, cutting off parts than dangle outside
         of the connection).
         If the target ROIs are consecutively labelled from 1 to N_ROI, then:
           Each i-th brick (i running from 1 to N_ROI) contains the voxels
           through which tracks hitting that i-th target AND any other target
           passed; voxels connecting i- and j-th target ROIs have value j, and
           the values are summed if a given voxel is in multiple WM ROIs (i.e.,
           for a voxel connecting both target ROIs 2 and 1 as well as 2 and 4,
           then the value there in brick [2] would be 1 + 4 = 5).
         Else, then:
           Each i-th brick contains the voxels through which the tracks
           hitting the j-th target AND any other target passed (where j may or
           may not equal i; the value of j is recorded in the brick label: 
           AND_roi_'j'). The same voxel labelling and summing rules described
           above also apply here.
         For single-ROI inputs (such as a single wholebrain ROI), no PAIRMAP
           file is output (because it would necessarily be empty).
      3) *.grid  ASCII-text file (output in ALL modes).
         Simple text file of output stats of WM-ROIs. It outputs the means and
         standard deviations of parameter quantities (such as FA, MD, L1, etc.)
         as well as counts of tracks and volumes of WM-ROIs. Each matrix is
         square, with dimension N_ROI by N_ROI. Like the locations in a standard
         correlation matrix, each element reflects associativity with target
         ROIs.  A value at element (1,3) is the same as that at (3,1) and tells
         about the property of a WM-ROI connecting target ROIs 1 and 3 (consider
         upper left corner as (1,1)); diagonal elements provide info of tracks
         through (at minimum) that single target ROI-- like OR logic connection.
         Format of *.grid file is:
         Line 1:  number of ROIs in network (padded with #-signs)
         Line 2:  number of output matrices of stats info (padded with #-signs)
         Line 3:  list of N_ROI labels for that network
         Lines following: first line, label of a property (padded with #), and 
                          then N_ROI lines of the N_ROI-by-N_ROI matrix of that
                          property;
                          /repeat/
         The first *five* matrices are currently (this may change over time):
             NT  = number of tracks in that WM-ROI
             fNT = fractional number of tracks in that WM-ROI, defined as NT
                   divided by total number of tracts found (may not be relevant)
             PV  = physical volume of tracks, in mm^3
             fNV = fractional volume of tracks compared to masked (internally or
                   '-mask'edly) total volume; would perhaps be useful if said
                   mask represents the whole brain volume well.
             NV  = number of voxels in that WM-ROI.
             BL  = average length (in mm) of a bundle of tracts.
             sBL = stdev of the length (in mm) of a bundle of tracts.
         Then, there can be a great variety in the remaining matrices, depending
         on whether one is in DTI or HARDI mode and how many scalar parameter
         files get input (max is 10). For each scalar file there are two
         matrices: first a label (e.g., 'FA') and then an N_ROI-by-N_ROI matrix
         of the means of that parameter in each WM-ROI; then a label (here,
         would be 'sFA') and then an N_ROI-by-N_ROI matrix of the standard
         deviations of that parameter in each WM-ROI.
      4) *niml.tract  NIML/SUMA-esque file (output in '-mode {DET | MINIP}')
         File for viewing track-like output in SUMA, with, e.g.:
         $ suma -tract FILE.niml.tract
      5) *niml.dset  NIML/SUMA-esque file (output in '-mode {DET | MINIP}')
         File accompanying the *.niml.tract file-- also for use in SUMA, for
         including GRID-file like information with the tract info.
         $ suma -tract FILE.niml.tract -gdset FILE.niml.dset
      6) *.trk TrackVis-esque file (output in '-mode {DET | MINIP}')
         File for viewing track-like output in TrackVis (separate install from
         AFNI/SUMA); things mainly done via GUI interface.
    
    ****************************************************************************
    
    + LABELTABLE LABELLING (Sept 2014).
     The ability to use label tables in tracking result output has been
         included. 
     Default behavior will be to *construct* a labeltable from zero-padded ints
         in the '-netrois' file which define target ROIs.  Thus, the ROI of '3's
         will be given a label '003'.  This will be used in INDIMAP and PAIRMAP
         brick labels (which is useful if the targets are not consecutively
         numbered from 1), PAIRMAP connections in bricks >0, and output 
         *.niml.tract files. The PAIRMAP labeltable will be created and output
         as 'PREFIX_PAIRMAP.niml.lt', and will be useful for the user in (some-
         what efficiently) resolving multiple tracts passing through voxels.
         These labels are also used in the naming of '-dump_rois AFNI' output.
     At the moment, in a given PAIRMAP brick of index >0, labels can only 
         describe up to two connections through a given voxel.  In brick 1, if 
         voxel is intersected by tracts connection ROIs 1 and 3 as well as ROIs
         1 and 6, then the label there would be '003<->006'; if another voxel
         in that brick had those connections as well as one between ROIs 1 and 
         4, then the label might be '_M_<->003<->006', or '_M_<->003<->004', or
         any two of the connections plus the leading '_M_' that stands for 
         'multiple others' (NB: which two are shown is not controlled, but I 
         figured it was better to show at least some, rather than just the 
         less informative '_M_' alone).  In all of these things, the PAIRMAP
         map is a useful, fairly efficient guide-check, but the overlaps are
         difficult to represent fully and efficiently, given the possibly
         complexity of patterns.  For more definite, unique, and scriptable
         information of where estimated WM connections are, use the 
         '-dump_rois AFNI' or '-dump_rois AFNI_MAP' option.
     If the '-netrois' input has a labeltable, then this program will program
         will read it in, use it in PAIRMAP and INDIMAP bricklabels, PAIRMAP
         subbricks with index >0, *niml.tract outputs and, by default, in the
         naming of '-dump_rois AFNI' output.  The examples and descriptions
         directly above still hold, but in cases where the ROI number has an
         explicit label, then the former is replaced by the latter's string.
         In cases where an input label table does not cover all ROI values, 
         there is no need to panic-- the explicit input labels will be used
         wherever possible, and the zero-padded numbers will be used for the 
         remaining cases.  Thus, one might see PAIRMAP labels such as:
         '003<->Right-Amygdala', '_M_<->ctx-lh-insula<->006', etc.
    
    ****************************************************************************
    
    + RUNNING AND COMMANDLINE OPTIONS: pick a MODEL and a MODE.
     There are now two types of models, DTI and HARDI, that can be tracked.
         In HARDI, one may have multiple directions per voxel along which tracts
         may propagate; in DTI, there can be only one. Each MODEL has some
         required, and some optional, inputs.
     Additionally, tracking is run in one of three modes, as described near the
         top of this document, '-mode {DET | MINIP | PROB}', for deterministic
         mini-probabilistic, or full probabilistic tracking, respectively.
         Each MODE has some required, and some optional, inputs. Some options
         find work in multiple modes.
     To run '3dTrackID', one needs to have both a model and a mode in mind (and
         in data...).  Below is a table to show the various options available
         for the user to perform tracking. The required options for a given
         model or mode are marked with a single asterisk (*); the options under
         the /ALL/ column are necessary in any mode. Thus, to run deterministic
         tracking with DTI data, one *NEEDS* to select, at a minimum:
             '-mode DET', '-netrois', '-prefix', '-logic';
         and then there is a choice of loading DTI data, with either:
             '-dti_in' or '-dti_list',
         and then one can also use '-dti_extra', '-mask', '-alg_Nseed_Y',
         et al. from the /ALL/ and DET colums; one canNOT specify '-unc_min_FA'
         here -> the option is in an unmatched mode column.
         Exact usages of each option, plus formats for any arguments, are listed
         below. Default values for optional arguments are also described.
    
             +-----------------------------------------------------------------+
             |          COMMAND OPTIONS FOR TRACKING MODES AND MODELS          |
             +-----------------------------------------------------------------+
             |     /ALL/         |     DET     |    MINIP    |      PROB       |
    +--------+-------------------+-------------+-------------+-----------------+
             |{dti_in, dti_list}*|             |             |                 |
       DTI   | dti_extra         |             |             |                 |
             | dti_search_NO     |             |             |                 |
    +-~or~---+-------------------+-------------+-------------+-----------------+
             | hardi_gfa*        |             |             |                 |
      HARDI  | hardi_dirs*       |             |             |                 |
             | hardi_pars        |             |             |                 |
    ==~and~==+===================+=============+=============+=================+
             | mode*             |             |             |                 |
     OPTIONS | netrois*          |             |             |                 |
             | prefix*           |             |             |                 |
             | mask              |             |             |                 |
             | thru_mask         |             |             |                 |
             | targ_surf_stop    |             |             |                 |
             | targ_surf_twixt   |             |             |                 |
             |                   | logic*      | logic*      |                 |
             |                   |             | mini_num*   |                 |
             |                   |             | uncert*     | uncert*         |
             |                   |             | unc_min_FA  | unc_min_FA      |
             |                   |             | unc_min_V   | unc_min_V       |
             | algopt            |             |             |                 |
             | alg_Thresh_FA     |             |             |                 |
             | alg_Thresh_ANG    |             |             |                 |
             | alg_Thresh_Len    |             |             |                 |
             |                   | alg_Nseed_X | alg_Nseed_X |                 |
             |                   | alg_Nseed_Y | alg_Nseed_Y |                 |
             |                   | alg_Nseed_Z | alg_Nseed_Z |                 |
             |                   |             |             | alg_Thresh_Frac |
             |                   |             |             | alg_Nseed_Vox   |
             |                   |             |             | alg_Nmonte      |
             | uncut_at_rois     |             |             |                 |
             | do_trk_out        |             |             |                 |
             | dump_rois         |             |             |                 |
             | dump_no_labtab    |             |             |                 |
             | dump_lab_consec   |             |             |                 |
             | posteriori        |             |             |                 |
             | rec_orig          |             |             |                 |
             | tract_out_mode    |             |             |                 |
             | write_opts        |             |             |                 |
             | write_rois        |             |             |                 |
             | pair_out_power    |             |             |                 |
    +--------+-------------------+-------------+-------------+-----------------+
    *above, asterisked options are REQUIRED for running the given '-mode'.
     With DTI data, one must use either '-dti_in' *or* '-dti_list' for input.
    
     FOR MODEL DTI:
        -dti_in  INPREF :basename of DTI volumes output by, e.g., 3dDWItoDT.
                         NB- following volumes are *required* to be present:
                         INPREF_FA, INPREF_MD, INPREF_L1,
                         INPREF_V1, INPREF_V2, INPREF_V3,
                         and (now) INPREF_RD (**now output by 3dTrackID**).
                         Additionally, the program will search for all other
                         scalar (=single brik) files with name INPREF* and will
                         load these in as additional quantities for WM-ROI
                         stats; this could be useful if, for example, you have
                         PD or anatomical measures and want mean/stdev values
                         in the WM-ROIs (to turn this feature off, see below,
                         'dti_search_NO'); all the INPREF* files must be in same
                         DWI space.
                         Sidenote: including/omitting a '_' at the end of INPREF
                         makes no difference in the hunt for files.
        -dti_extra SET  :if you want to use a non-FA derived definition for the
                         WM skeleton in which tracts run, you can input one, and
                         then the threshold in the -algopt file (or, via the
                         '-alg_Thresh_FA' option) will be applied to 
                         thresholding this SET; similarly for the minimum
                         uncertainty by default will be set to 0.015 times the
                         max value of SET, or can be set with '-unc_min_FA'.
                         If the SET name is formatted as INPREF*, then it will
                         probably be included twice in stats, but that's not the
                         worst thing. In grid files, name of this quantity will
                         be 'XF' (stands for 'extra file').
        -dti_search_NO  :turn off the feature to search for more scalar (=single
                         brik) files with INPREF*, for including stats in output
                         GRID file. Will only go for FA, MD, L1 and RD scalars
                         with INPREF.
        -dti_list FILE  :an alternative way to specify DTI input files, where
                         FILE is a NIML-formatted text file that lists the
                         explicit/specific files for DTI input.  This option is
                         used in place of '-dti_in' and '-dti_extra' for loading
                         data sets of FA, MD, L1, etc.  An 'extra' set (XF) can
                         be loaded in the file, as well as supplementary scalar
                         data sets for extra WM-ROI statistics.
                         See below for a 'DTI LIST FILE EXAMPLE'.
     FOR MODEL HARDI:
        -hardi_gfa GFA  :single brik data set with generalized FA (GFA) info.
                         In reality, it doesn't *have* to be a literal GFA, esp.
                         if you are using some HARDI variety that doesn't have
                         a specific GFA value-- in such a case, use whatever
                         could be thresholded as your proxy for WM.
                         The default threshold is still 0.2, so you will likely
                         need to set a new one in the '-algopt ALG_FILE' file or
                         from the commandline with '-alg_Thresh_FA', which does
                         apply to the GFA in the HARDI case as well.
                         Stats in GRID file are output under name 'GFA'.
       -hardi_dirs DIRS :For tracking if X>1 propagation directions per voxel
                         are given, for example if HARDI data is input. DIRS
                         would then be a file with 3*X briks of (x,y,z) ordered,
                         unit magnitude vector components;  i.e., brik [0]
                         contains V1_x, [1] V1_y, [2] V1_z, [3] V2_x, etc.
                         (NB: even if X=1, this option works, but that would
                         seem to take the HAR out of HARDI...)
       -hardi_pars PREF :search for scalar (=single brik) files of naming
                         format PREF*.  These will be read in for WM-ROI stats
                         output in the GRID file.  For example, if there are
                         some files PREF_PD.nii.gz, PREF_CAT.nii.gz and
                         PREF_DOG.nii.gz, they will be labelled in the GRID file
                         as 'PD', 'CAT' and 'DOG' (that '_' will be cut out).
     MODEL-INDEPENDENT OPTIONS:
        -mode  MODUS    :this necessary option is used to define whether one is
                         performing deterministic, mini-probabilistic or full-
                         probabilistic tractography, by selecting one of three
                         respective modes:  DET, MINIP, or PROB.
        -netrois ROIS   :mask(s) of target ROIs- single file can have multiple
                         briks, one per network. The target ROIs through which
                         tracks will be kept should have index values >0. It is
                         also possible to define anti-targets (exclusionary
                         regions) which stop a propagating track... in its 
                         tracks. These are defined per network (i.e., per brik)
                         by voxels with values <0.
        -prefix  PREFIX :output file name part.
        -mask   MASK    :can include a brainmask within which to calculate 
                         things. Otherwise, data should be masked already.
        -thru_mask TM   :optional extra restrictor mask, through which paths are
                         (strictly) required to pass in order to be included
                         when passing through or connecting targets. It doesn't
                         discriminate based on target ROI number, so it's
                         probably mostly useful in examining specific pairwise
                         connections. It is also not like one of the target
                         '-netrois' in that no statistics are calculated for it.
                         Must be same number of briks as '-netrois' set.
        -targ_surf_stop :make the final tracts and tracked regions stop at the
                         outer surface of the target ROIs, rather than being
                         able to journey arbitrarily far into them (latter being
                         the default behavior.  Might be useful when you want
                         meaningful distances *between* targets.  Tracts stop
                         after going *into* the outer layer of a target.
                         This can be applied to DET, MINIP, or PROB modes.
                         NB: this only affects the connections between pairs
                         of targets (= AND-logic, off-diagonal elements in
                         output matrices), not the single-target tracts
                         (= OR-logic, on-diagonal elements in output
                         matrices); see also a related option, below.
       -targ_surf_twixt :quite similar to '-targ_surf_stop', above, but the
                         tracts stop *before* entering the target surfaces, so
                         that they are only between (or betwixt) the targets.
                         Again, only affects tracts between pairs of targets.
    
        -logic {OR|AND} :when in one of '-mode {DET | MINIP}', one will look for
                         either OR- or AND-logic connections among target ROIs
                         per network (multiple networks can be entered as
                         separate briks in '-netrois ROIS'): i.e., one keeps
                         either any track going through at least one network ROI
                         or only those tracks which join a pair of ROIs.
                         When using AND here, default behavior is to only keep
                         voxels in tracks between the ROIs they connect (i.e.,
                         cut off track bits which run beyond ROIs).
        -mini_num NUM   :will run a small number NUM of whole brain Monte Carlo
                         iterations perturbing relevant tensor values in accord
                         with their uncertainty values (hence, the need for also
                         using `-uncert' with this option). This might be useful
                         for giving a flavor of a broader range of connections
                         while still seeing estimated tracks themselves. NB: if
                         NUM is large, you might be *big* output track files;
                         e.g., perhaps try NUM = 5 or 9 or so to start.
                         Requires '-mode MINIP' in commandline.
        -bundle_thr V   :the number of tracts for a given connection is called
                         a bundle. For '-mode {DET | MINIP}', one can choose to
                         NOT output tracts, matrix info, etc. for any bundle
                         with fewer than V tracts. This might be useful to weed
                         out ugly/false tracts (default: V=1).
        -uncert U_FILE  :when in one of '-mode {MINIP | PROB}', uncertainty
                         values for eigenvector and WM skeleton (FA, GFA, etc.)
                         maps are necessary.
                         When using DTI ('-dti_*'), then use the 6-brik file
                         from 3dDWUncert; format of the file given below.
                         When using HARDI ('-hardi_*') with up to X directions
                         per voxel, one needs U_FILE to have X+1 briks, where
                         U_FILE[0] is the uncertainty for the GFAfile, and the
                         other briks are ordered for directions given with
                         '-hardi_dirs'.
                         Whatever the values in the U_FILE, this program asserts
                         a minimum uncertainty of stdevs, with defaults:
                         for FA it is 0.015, and for GFA or -dti_extra sets it
                         is 0.015 times the max value present (set with option
                         '-unc_min_FA');
                         for each eigenvector or dir, it is 0.06rad (~3.4deg)
                         (set with option '-unc_min_V')
       -unc_min_FA VAL1 :when using '-uncert', one can control the minimum
                         stdev for perturbing FA (in '-dti_in'), or the EXTRA-
                         file also in DTI ('-dti_extra'), or GFA (in '-hardi_*).
                         Default value is: 0.015 for FA, and 0.015 times the max
                         value in the EXTRA-file or in the GFA file.
        -unc_min_V VAL2 :when using '-uncert', one can control the minimum
                         stdev for perturbing eigen-/direction-vectors.
                         In DTI, this is for tipping e_1 separately toward e2
                         and e3, and in HARDI, this is for defining a single
                         degree of freedom uncertainty cone. Default values are
                         0.06rad (~3.4deg) for any eigenvector/direction. User
                         assigns values in degrees.
    
       -algopt A_FILE   :simple ASCII file with six numbers defining tracking 
                         parameter quantities (see list below); note the
                         differences whether running in '-mode {DET | MINIP}'
                         or in '-mode PROB': the first three parameters in each
                         mode are the same, but the next three differ.
                         The file can be in the more understandable html-type
                         format with labels per quantity, or just as a column
                         of the numbers, necessarily in the correct order.
                         NB: each quantity can also be changed individually
                         using a commandline option (see immediately following).
                         If A_FILE ends with '.niml.opts' (such as would be
                         produced using the '-write_opts' option), then it is
                         expected that it is in nice labelled NIML format;
                         otherwise, the file should just be a column of numbers
                         in the right order. Examples of A_FILEs are given at
                         the end of the option section.
      -alg_Thresh_FA  A :set threshold for DTI FA map, '-dti_extra' FILE, or 
                         HARDI GFA map (default = 0.2).
      -alg_Thresh_ANG B :set max angle (in deg) for turning when going to a new
                         voxel during propagation (default = 60).
      -alg_Thresh_Len C :min physical length (in mm) of tracts to keep
                         (default = 20).
      -alg_Nseed_X    D :Number of seeds per vox in x-direc (default = 2).
      -alg_Nseed_Y    E :Number of seeds per vox in y-direc (default = 2).
      -alg_Nseed_Z    F :Number of seeds per vox in z-direc (default = 2).
               +-------> NB: in summation, for example the alg_Nseed_* options
                            for '-mode {DET | MINIP} place 2x2x2=8 seed points,
                            equally spread in a 3D cube, in each voxel when
                            tracking.
     -alg_Thresh_Frac G :value for thresholding how many tracks must pass
                         through a voxel for a given connection before it is
                         included in the final WM-ROI of that connection.
                         It is a decimal value <=1, which will multiply the
                         number of 'starting seeds' per voxel, Nseed_Vox*Nmonte
                         (see just below for those). (efault = 0.001; for higher
                         specificity, a value of 0.01-0.05 could be used).
      -alg_Nseed_Vox  H :number of seeds per voxel per Monte Carlo iteration;
                         seeds will be placed randomly (default = 5).
      -alg_Nmonte     I :number of Monte Carlo iterations (default = 1000).
               +-------> NB: in summation, the preceding three options for the
                            '-mode PROB' will mean that 'I' Monte Carlo
                            iterations will be run, each time using 'H' track
                            seeds per relevant voxel, and that a voxel will
                            need 'G*H*I' tracks of a given connection through
                            it to be included in that WM-ROI. Default example:
                            1000 iterations with 5 seeds/voxel, and therefore
                            a candidate voxel needs at least 0.001*5*1000 = 5
                            tracks/connection.
    
        -extra_tr_par   :run three extra track parameter scalings for each
                         target pair, output in the *.grid file. The NT value
                         of each connection is scaled in the following manners
                         for each subsequent matrix label:
                            NTpTarVol  :div. by average target volume;
                            NTpTarSA   :div. by average target surface area;
                            NTpTarSAFA :div. by average target surface area
                                        bordering suprathreshold FA (or equi-
                                        valent WM proxy definition).
                         NB: the volume and surface area numbers are given in
                         terms of voxel counts and not using physical units
                         (consistent: NT values themselves are just numbers.)
        -uncut_at_rois  :when looking for pairwise connections, keep entire
                         length of any track passing through multiple targets,
                         even when part ~overshoots a target (i.e., it's not
                         between them).  When using OR tracking, this is
                         automatically applied.  For probabilistic tracking, not
                         recommended to use (are untrimmed ends meaningful?).
                         The default behavior is to trim the tracts that AND-
                         wise connect targets to only include sections that are
                         between the targets, and not parts that run beyond one.
                         (Not sure why one would want to use this option, to be
                         honest; see '-targ_surf_stop' for really useful tract
                         control.)
        -dump_rois TYPE :can output individual masks of ROI connections.
                         Options for TYPE are: {DUMP | AFNI | BOTH | AFNI_MAP}.
                         Using DUMP gives a set of 4-column ASCII files, each 
                         formatted like a 3dmaskdump data set; it can be recon-
                         stituted using 3dUndump. Using AFNI gives a set of
                         BRIK/HEAD (byte) files in a directory called PREFIX; 
                         using AFNI_MAP is like using AFNI, but it gives non-
                         binarized *maps* of ROI connections.
                         Using BOTH produces AFNI and DUMP formats of outputs.
        -dump_no_labtab :if the ROIS file has a label table, the default is to
                         use it in naming a '-dump_rois' output (if being used);
                         using this switch turn that off-- output file names 
                         will be the same as if no label table were present.
       -dump_lab_consec :if using `-dump_rois', then DON'T apply the numerical
                         labels of the original ROIs input to the output names.
                         This would only matter if input ROI labels aren't 
                         consecutive and starting with one (e.g., if instead 
                         they were 1,2,3,5,8,..).
              --->   This is the opposite  from previous default behavior, where
                         the option '-lab_orig_rois' was used to switch away
                         from consecutive-izing the labels in the output.
        -posteriori     :switch to have a bunch of individual files output, with
                         the value in each being the number of tracks per voxel
                         for that pair; works with '-dump_rois {AFNI | BOTH }',
                         where you get track-path maps instead of masks; makes
                         threshold for number of tracks between ROIs to keep to
                         be one automatically, regardless of setting in algopt.
        -rec_orig       :record dataset origin in the header of the *.trk file.
                         As of Sept. 2012, TrackVis doesn't use this info so it
                         wasn't included, but if you might want to map your 
                         *.trk file later, then use the switch as the datasets's
                         Origin is necessary info for the mapping (the default
                         image in TrackVis might not pop up in the center of the
                         viewing window, though, just be aware). NB: including
                         the origin might become default at some point in time.
        -do_trk_out     :Switch ON outputting *.trk files, which are mainly to
                         be viewed in TrackVis (Wang et al., 2007). 
                         (Feb, 2015): Default is to NOT output *.trk files.
        -nifti          :output the PAIRMAP, INDIMAP, and any '-dump_rois' in
                         *.nii.gz format (default is BRIK/HEAD).
      -no_indipair_out  :Switch off outputting *INDIMAP* and *PAIRMAP* volumes.
                         This is probably just if you want to save file space;
                         also, for connectome-y studies with many (>100) target
                         regions, the output INDI and PAIR maps can be quite
                         large and/or difficult to write out. In some cases, it
                         might be better to just use '-dump_rois AFNI' instead.
                         Default is to output the INDI and PAIR map files.
        -write_rois     :write out a file (PREFIX.roi.labs) of all the ROI 
                         (re-)labels, for example if the input ROIs aren't
                         simply consecutive and starting from 1. File has 3cols:
                           Input_ROI   Condensed_form_ROI   Power_of_2_label
        -write_opts     :write out all the option values into PREFIX.niml.opts.
        -pair_out_power :switch to affect output of *PAIRMAP* output files. 
                         Now, the default format is to output the >0 bricks with
                         tracks labelled by the target integers themselves.
                         This is not a unique labelling system, but it *is* far
                         easier to view and understand what's going on than
                         using a purely unique system based on using powers of
                         two of the ROIs (with integer summation for overlaps).
                         Using the switch '-pair_out_power' will change the
                         output of bricks [1] and higher to contain
                         information on connections stored as powers of two, so
                         that there is a unique decomposition in terms of
                         overlapped connections. However, it's *far* easier to
                         use '-dump_rois {AFNI | BOTH }' to get individual mask
                         files of the ROIs clearly (as well as annoying to need
                         to calculate powers of two simply to visualize the
                         connections.
                     -----> when considering this option, see the 'LABELTABLE'
                            description up above for how the labels work, with
                            or without an explicit table being entered.
        -verb VERB      :verbosity level, default is 0.
    
    ****************************************************************************
    
    + ALGOPT FILE EXAMPLES (note that different MODES have some different opts):
      For '-mode {DET | MINIP}, the nicely readable NIML format of algopt file
      would have a file name ending '.niml.opts' and contain something like the:
      following seven lines:
          <TRACK_opts
             Thresh_FA="0.2"
             Thresh_ANG="60.000000"
             Thresh_Len="20.000000"
             Nseed_X="2"
             Nseed_Y="2"
             Nseed_Z="2" />
      The values above are actually all default values, and such a file would be
      output using the '-write_opts' flag. For the same modes, one could get
      the same result using a plain column of numbers, whose meaning is defined
      by their order, contained in a file NOT ending in .niml.opts, such as 
      exemplified in the next six lines:
             0.2
             60
             20
             2
             2
             2
      For '-mode PROB', the nice NIML format algopt file would contain something
      like the next seven lines (again requiring the file name to end in
      '.niml.opts'):
          <TRACK_opts
             Thresh_FA="0.2"
             Thresh_ANG="60.0"
             Thresh_Len="20.0"
             Thresh_Frac="0.001"
             Nseed_Vox="5"
             Nmonte="1000" />
      Again, those represent the default values, and could be given as a plain
      column of numbers, in that order.
    
    * * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * **
    
    + DTI LIST FILE EXAMPLE:
         Consider, for example, if you hadn't used the '-sep_dsets' option when
         outputting all the tensor information from 3dDWItoDT.  Then one could
         specify the DTI inputs for this program with a file called, e.g., 
         FILE_DTI_IN.niml.opts (the name *must* end with '.niml.opts'):
           <DTIFILE_opts
             dti_V1="SINGLEDT+orig[9..11]"
             dti_V2="SINGLEDT+orig[12..14]"
             dti_V3="SINGLEDT+orig[15..17]"
             dti_FA="SINGLEDT+orig[18]"
             dti_MD="SINGLEDT+orig[19]"
             dti_L1="SINGLEDT+orig[6]"
             dti_RD="SINGLEDT+orig[20]" />
         This represents the *minimum* set of input files needed when running
         3dTrackID.  (Oct. 2016: RD now output by 3dDWItoDT, and not calc'ed 
         internally by 3dTrackID.)
         One could also input extra data: an 'extra file' (XF) to take the place
         of an FA map for determining where tracks can propagate; and up to four
         other data sets (P1, P2, P3 and P4, standing for 'plus one' etc.) for
         calculating mean/stdev properties in the obtained WM-ROIs:
           <DTIFILE_opts
             dti_V1="SINGLEDT+orig[9..11]"
             dti_V2="SINGLEDT+orig[12..14]"
             dti_V3="SINGLEDT+orig[15..17]"
             dti_XF="Segmented_WM.nii.gz"
             dti_FA="SINGLEDT+orig[18]"
             dti_MD="SINGLEDT+orig[19]"
             dti_L1="SINGLEDT+orig[6]"
             dti_RD="SINGLEDT+orig[20]"
             dti_P1="SINGLEDT+orig[7]"
             dti_P2="SINGLEDT+orig[8]"
             dti_P3="T1_map.nii.gz"
             dti_P4="PD_map.nii.gz" />
    
    ****************************************************************************
    
    + EXAMPLES:
       Here are just a few scenarios-- please see the Demo data set for *maaany*
       more, as well as for fuller descriptions.  To obtain the Demo, type the
       following into a commandline:
       $ @Install_FATCAT_demo
       This will also unzip the archive, which contains required data (so it's
       pretty big, currently >200MB), a README.txt file, and several premade
       scripts that are ~heavily commented.
    
       A) Deterministic whole-brain tracking; set of targets is just the volume
          mask. This can be useful for diagnostic purposes, sanity check for
          gradients+data, for interactively selecting interesting subsets later,
          etc. This uses most of the default algopts, but sets a higher minimum
          length for keeping tracks:
          $ 3dTrackID -mode DET                \
                      -dti_in DTI/DT           \
                      -netrois mask_DWI+orig   \
                      -logic OR                \
                      -alg_Thresh_Len 30       \
                      -prefix DTI/o.WB
    
       B) Mini-probabilistic tracking through a multi-brik network file using a
          DTI model and AND-logic. Instead of using the thresholded FA map to
          guide tracking, an extra data set (e.g., a mapped anatomical
          segmentation image) is input as the WM proxy; as such, what used to be
          a threshold for adult parenchyma FA is now changed to an appropriate
          value for the segmentation percentages; and this would most likely
          also assume that 3dDWUncert had been used to calculate tensor value
          uncertainties:
          $ 3dTrackID -mode MINIP                      \
                      -dti_in DTI/DT                   \
                      -dti_extra T1_WM_in_DWI.nii.gz   \
                      -netrois ROI_ICMAP_GMI+orig      \
                      -logic AND                       \
                      -mini_num 7                      \
                      -uncert DTI/o.UNCERT_UNC+orig.   \
                      -alg_Thresh_FA 0.95              \
                      -prefix DTI/o.MP_AND_WM 
    
       C) Full probabilistic tracking through a multi-brik network file using
          HARDI-Qball reconstruction. The designated GFA file is used to guide
          the tracking, with an appropriate threshold set and a smaller minimum
          uncertainty of that GFA value (from this and example B, note how
          generically the '-alg_Thresh_FA' functions, always setting a value for
          the WM proxy map, whether it be literally FA, GFA or the dti_extra
          file). Since HARDI-value uncertainty isn't yet calculable in AFNI,
          brain-wide uniform values were assigned to the GFA and directions:
          $ 3dTrackID -mode PROB                       \
                      -hardi_gfa QBALL/GFA.nii.gz      \
                      -hardi_dirs QBALL/dirs.nii.gz    \
                      -netrois ROI_ICMAP_GMI+orig      \
                      -uncert QBALL/UNIFORM_UNC+orig.  \
                      -mask mask_DWI+orig              \
                      -alg_Thresh_FA 0.04              \
                      -unc_min_FA 0.003                \
                      -prefix QBALL/o.PR_QB 
    
    ****************************************************************************
    
      If you use this program, please reference the workhorse FACTID
      tractography algorithm:
        Taylor PA, Cho K-H, Lin C-P, Biswal BB (2012). Improving DTI
        Tractography by including Diagonal Tract Propagation. PLoS ONE
        7(9): e43415. 
      and the introductory/description paper for FATCAT:
        Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And
        Tractographic Connectivity Analysis Toolbox. Brain Connectivity.
