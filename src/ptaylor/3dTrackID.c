/* 
   Probabilistic(+deterministic) tracking, first draft at
   AFNIfication, March 2012.  using FACTID code, from Taylor,
   Kuan-Hung, Lin and Biswal (2012)

	Sept 2012: fixing some memory stuff.

	Nov 2012: ability to use ROI labels which are non-consecutive ints.

	Dec 2012: 
	+ allow non-FA map to define `WM' for tracks,
	+ can include brainmask, if wanted.
	+ allow both nifti and afni inputs to be read 

	Jan 2013:
	+ 'NOT' masks
	+ minimum uncert

	Jan 2013: 
	+ output ascii format 'dumps' to be turned into masks by 3dUndump,
	or individ brain mask dumps, or both 

   Feb 2013:
   + allow for output of unthresholded values for ROI maps

   Mar 2013:
   + output *.grid file now has row of ROI labels in it
   + floor instead of ceil to calc NmNsThr

   Aug 2013:
   + merge with det tracking to have networks here, and single prog: @)
   + mini prob tracking for det case
   + better visualization tools, a la Ziad Saad
   + groundwork for adding SUMA+matrix capabilities, new IO: @)))
   + will start voxelwise track info

   Sep 2013:
   + include better NOT mask behavior-- simpler; instead of separate mask,
   have negative (-1) ROIs in same brik with positive ones

   Jan 2014:
   + firstly, migrated HERE.
   + multidirectional tracking: HARDI!
   + nb: *coorded* now have 0th brick as FA info, and then vec1, vec2, etc.
   + lots of interior changes: searching for names/globbing, allow more inputs
   + allow commandline opts for algopts; got rid of M and bval in algopt- unused
   + change output numbering to start from 0.
   + Instatract stuff better, ZSS work.
   + '-cut_at_rois' now default behavior; added '-uncut_at_rois' switch
   + can switch off *.trk format

   Feb 2014:
   + couple option name switches, and how TROSS name is saved
   
   Mar 2014:
   + '-dti_list' input option added at user request.

   May/June 2014:
   + structified call to tracking, for ZSS
   + redoing what reaching arrMax does: throw an error, but don't stop
   + more stats in grid: frac and phys volumes included
   + extra through mask ('-thru_mask')
   + some nicer output-- file name prefix at end, count miniprob

   Aug 2014:
   + add option for output of PAIRMAP to not be 2^X for connections
   + add option for some scaling of tracks by ROI vols and surf areas

   Aug 2014 (again):
   + using UHTs to store information-- major mem savings
   + prob a bit faster now, as well.  No change in results

   Sept 2014: 
   + use label table of netrois, if exists

   Jan 2015:
   + '-nifti' switch for NIFTI output

   Feb 2015:
   + option to threshold minip results by bundle size
   + trk output no longer default
   + *neeed* logic in DET|MINIP modes

   Oct/Nov 2016:
   + major streamlining of internal code (DTI globbing)
   + newer functions to clean up functionality
   
   Nov 2016:
   + new distance calculation: output lengths of bundles
   + new targ_surf_stop opt: stop tracts at surface of nets
   + new targ_surf_twist opt: stop tracts *before*
     surface of nets
   + *lots* of new internal streamlining as part 
     of this process

*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <debugtrace.h>
#include <mrilib.h>
#include <3ddata.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include "Fat_Labels.h"
#include "DoTrackit.h"
#include "TrackIO.h"
#include "FuncTrac.h"
#include "readglob.h"

#include "suma_suma.h"


//#define MAX_PARAMS (11) // for *.trk output
//#define N_dti_scal (3) 
//#define N_dti_scal_tot (6) // 1+2+3, matches with N_dti_scal
//#define N_dti_vect (3) 
//#define N_plus_dtifile (4) // num of allowed additional param files in NIMLver
//#define PreCalcParLabs (5) // we define a few things initially


int GetSizesOfTargets( int ***ROI_SIZES,
                       int **MAPROI, int *Dim, 
                       int ***mskd, 
                       int ***INDEX2, 
                       float **coorded, float minFA,
                       int N_nets, int *NROI,
                       int **INV_LABELS);


int RunTrackingMaestro( int comline, TRACK_RUN_PARAMS opts,
                        int argc, char *argv[]);



void usage_TrackID(int detail) 
{
	printf("\n"
"  FACTID-based tractography code, from Taylor, Cho, Lin and Biswal (2012),\n"
"  and part of FATCAT (Taylor & Saad, 2013) in AFNI. Version 2.1 (Jan. 2014),\n"
"  written by PA Taylor and ZS Saad.\n"
"\n"
"  Estimate locations of WM associated with target ROIs, particularly between\n"
"  pairs of GM in a network;  can process several networks in a given run.\n"
"\n"
"  Now does both single tract propagation per voxel (as per DTI) and \n"
"  multi-directional tracking (as in HARDI-type models). Many extra files can\n"
"  be loaded in for getting quantitative stats in WM-ROIs, mostly done via\n"
"  search from entered prefixes. Many more switches and options are available\n"
"  to the user to control the tracking (yay!).\n"
"  Track display capabilities in SUMA have been boosted and continue to rise\n"
"  quickly (all courtesy of ZS Saad).\n"
"\n"
"****************************************************************************\n"
"+ NOTE that this program runs in three separate modes, each with its own\n"
"   subset of commandline options and outputs:\n"
"   $ 3dTrackID -mode {DET | MINIP | PROB} ... \n"
"   where     DET   -> deterministic tracking,\n"
"             MINIP -> mini-probabilistic tracking,\n"
"             PROB  -> (full) probabilistic tracking.\n"
"   So, for example, DET and MINIP produce pretty track-image output,\n"
"   while PROB only provides volumes; MINIP and PROB make use of\n"
"   tensor uncertainty to produce more robust results than DET; all\n"
"   produce quantitative statistical output of WM-ROIs; etc. In some cases,\n"
"   using a combination of all three might even be variously useful in a\n"
"   particular study.\n"
"****************************************************************************\n"
"  For DTI, this program reads in tensor-related data from, e.g., 3dDWItoDTI,\n"
"  and also uses results from 3dDWUncert for uncertainty measures when\n"
"  necessary.\n"
"\n"
"  For HARDI, this program reads in the direction vectors and WM-proxy map \n"
"  (such as the diffusion anisotropy coefficient, GFA) created by any source-\n"
"  right now, there's no HARDI modeler in AFNI. Currently known sources which\n"
"  are reasonably straightforward to use include DSI-Studio (Yeh et al.,\n"
"  2010) and Diffusion Toolkit (Wang et al., 2007). An example script of\n"
"  outputting Qball model data as NIFTI output from the former software is\n"
"  included in the FATCAT demo set.\n"
"\n"
"  ...And on that note, it is highly recommended for users to check out the\n"
"  FATCAT demo set, which can be downloaded and unwrapped simply from the\n"
"  commandline:\n"
"  $ @Install_FATCAT_Demo\n"
"  In that demo are data, a number of scripts, and more detailed descriptions\n"
"  for using 3dTrackID, as well as other programs in the FATCAT litter.\n"
"  Recommended to always check that one has the most uptodate version.\n"
"\n"
"****************************************************************************\n"
"\n"
"+ INPUT NOTES:\n"
"  NETWORK MAPS, for any '-mode' of track, given as a single- or multi-brik\n"
"   file via '-netrois':\n"
"   Each target ROI is defined by the set of voxels with a given integer >0.\n"
"   Target ROI labels do not have to be purely consecutive.\n"
"\n"
"  Note on vocabulary, dual usage of 'ROI': an (input) network is made up of\n"
"   *target ROIs*, between/among which one wants to find WM connections; so,\n"
"   3dTrackID outputs locations and stats on those calculated *WM-ROIs*.\n"
"\n"
"****************************************************************************\n"
"\n"
"+ OUTPUTS, all named using '-prefix INPREF'; somewhat dependent on tracking\n"
"           mode being utilized ('-mode {DET | MINIP | PROB}').\n"
"           Because multiple networks can be input simultaneously as a multi-\n"
"           brik '-netrois ROIS' file, the output prefix will also have a\n"
"           numerical designation of its network, matching to the brik of\n"
"           the ROIS file: thus, INPREF_000* goes with ROIS[0], INPREF_001*\n"
"           with ROIS[1] (if present), etc. This applies with all types of\n"
"           output files, now described:\n"
"  1) *INDIMAP*  BRIK files (output in ALL modes).\n"
"     For each network with N_ROI target ROIs, this is a N_ROI+1 brik file.\n"
"     0th brick contains the number of tracts per voxel which passed through\n"
"     at least one target ROI in that network (and in '-mode PROB', this\n"
"     number has been thresholded-- see 'alg_Thresh_Frac' below).\n"
"     If the target ROIs are consecutively labelled from 1 to N_ROI, then:\n"
"       Each i-th brick (i running from 1 to N_ROI) contains the voxels\n"
"       through which tracks hitting that i-th target passed; the value of\n"
"       each voxel is the number of tracks passing through that location.\n"
"     Else, then:\n"
"       Each i-th brick contains the voxels through which the tracks\n"
"       hitting the j-th target passed (where j may or may not equal i; the\n"
"       value of j is recorded in the brick label:  OR_roi_'j').  The target\n"
"       ROI connectivity is recorded increasing order of 'j'.\n"
"     For single-ROI inputs (such as a single wholebrain ROI), only the\n"
"       [0] brick is output (because [1] would be redundant).\n"
"  2) *PAIRMAP*  BRIK files (output in ALL modes).\n"
"     (-> This has altered slightly at the end of June, 2014! No longer using\n"
"     2^i notation-- made simpler for reading, assuming individual connection\n"
"     information for calculations was likely obtained more easily with \n"
"     '-dump_rois {AFNI | BOTH | AFNI_MAP}...)\n"
"     For each network with N_ROI target ROIs, this is a N_ROI+1 brik file.\n"
"     0th brick contains a binary mask of voxels through which passed a\n"
"     supra-threshold number of tracks (more than 0 for '-mode {DET | MINIP}'\n"
"     and more than the user-defined threshold for '-mode PROB') between any\n"
"     pair of target ROIs in that network (by default, these tracks have been\n"
"     trimmed to only run between ROIs, cutting off parts than dangle outside\n"
"     of the connection).\n"
"     If the target ROIs are consecutively labelled from 1 to N_ROI, then:\n"
"       Each i-th brick (i running from 1 to N_ROI) contains the voxels\n"
"       through which tracks hitting that i-th target AND any other target\n"
"       passed; voxels connecting i- and j-th target ROIs have value j, and\n"
"       the values are summed if a given voxel is in multiple WM ROIs (i.e.,\n"
"       for a voxel connecting both target ROIs 2 and 1 as well as 2 and 4,\n"
"       then the value there in brick [2] would be 1 + 4 = 5).\n"
"     Else, then:\n"
"       Each i-th brick contains the voxels through which the tracks\n"
"       hitting the j-th target AND any other target passed (where j may or\n"
"       may not equal i; the value of j is recorded in the brick label: \n"
"       AND_roi_'j'). The same voxel labelling and summing rules described\n"
"       above also apply here.\n"
"     For single-ROI inputs (such as a single wholebrain ROI), no PAIRMAP\n"
"       file is output (because it would necessarily be empty).\n"
"  3) *.grid  ASCII-text file (output in ALL modes).\n"
"     Simple text file of output stats of WM-ROIs. It outputs the means and\n"
"     standard deviations of parameter quantities (such as FA, MD, L1, etc.)\n"
"     as well as counts of tracks and volumes of WM-ROIs. Each matrix is\n"
"     square, with dimension N_ROI by N_ROI. Like the locations in a standard\n"
"     correlation matrix, each element reflects associativity with target\n"
"     ROIs.  A value at element (1,3) is the same as that at (3,1) and tells\n"
"     about the property of a WM-ROI connecting target ROIs 1 and 3 (consider\n"
"     upper left corner as (1,1)); diagonal elements provide info of tracks\n"
"     through (at minimum) that single target ROI-- like OR logic connection.\n"
"     Format of *.grid file is:\n"
"     Line 1:  number of ROIs in network (padded with #-signs)\n"
"     Line 2:  number of output matrices of stats info (padded with #-signs)\n"
"     Line 3:  list of N_ROI labels for that network\n"
"     Lines following: first line, label of a property (padded with #), and \n"
"                      then N_ROI lines of the N_ROI-by-N_ROI matrix of that\n"
"                      property;\n"
"                      /repeat/\n"
"     The first *five* matrices are currently (this may change over time):\n"
"         NT  = number of tracks in that WM-ROI\n"
"         fNT = fractional number of tracks in that WM-ROI, defined as NT\n"
"               divided by total number of tracts found (may not be relevant)\n"
"         PV  = physical volume of tracks, in mm^3\n"
"         fNV = fractional volume of tracks compared to masked (internally or\n"
"               '-mask'edly) total volume; would perhaps be useful if said\n"
"               mask represents the whole brain volume well.\n"
"         NV  = number of voxels in that WM-ROI.\n"
"         BL  = average length (in mm) of a bundle of tracts.\n"
"         sBL = stdev of the length (in mm) of a bundle of tracts.\n"
"     Then, there can be a great variety in the remaining matrices, depending\n"
"     on whether one is in DTI or HARDI mode and how many scalar parameter\n"
"     files get input (max is 10). For each scalar file there are two\n"
"     matrices: first a label (e.g., 'FA') and then an N_ROI-by-N_ROI matrix\n"
"     of the means of that parameter in each WM-ROI; then a label (here,\n"
"     would be 'sFA') and then an N_ROI-by-N_ROI matrix of the standard\n"
"     deviations of that parameter in each WM-ROI.\n"
"  4) *niml.tract  NIML/SUMA-esque file (output in '-mode {DET | MINIP}')\n"
"     File for viewing track-like output in SUMA, with, e.g.:\n"
"     $ suma -tract FILE.niml.tract\n"
"  5) *niml.dset  NIML/SUMA-esque file (output in '-mode {DET | MINIP}')\n"
"     File accompanying the *.niml.tract file-- also for use in SUMA, for\n"
"     including GRID-file like information with the tract info.\n"
"     $ suma -tract FILE.niml.tract -gdset FILE.niml.dset\n"
"  6) *.trk TrackVis-esque file (output in '-mode {DET | MINIP}')\n"
"     File for viewing track-like output in TrackVis (separate install from\n"
"     AFNI/SUMA); things mainly done via GUI interface.\n"
"\n"
"****************************************************************************\n"
"\n"
"+ LABELTABLE LABELLING (Sept 2014).\n"
" The ability to use label tables in tracking result output has been\n"
"     included. \n"
" Default behavior will be to *construct* a labeltable from zero-padded ints\n"
"     in the '-netrois' file which define target ROIs.  Thus, the ROI of '3's\n"
"     will be given a label '003'.  This will be used in INDIMAP and PAIRMAP\n"
"     brick labels (which is useful if the targets are not consecutively\n"
"     numbered from 1), PAIRMAP connections in bricks >0, and output \n"
"     *.niml.tract files. The PAIRMAP labeltable will be created and output\n"
"     as 'PREFIX_PAIRMAP.niml.lt', and will be useful for the user in (some-\n"
"     what efficiently) resolving multiple tracts passing through voxels.\n"
"     These labels are also used in the naming of '-dump_rois AFNI' output.\n"
" At the moment, in a given PAIRMAP brick of index >0, labels can only \n"
"     describe up to two connections through a given voxel.  In brick 1, if \n"
"     voxel is intersected by tracts connection ROIs 1 and 3 as well as ROIs\n"
"     1 and 6, then the label there would be '003<->006'; if another voxel\n"
"     in that brick had those connections as well as one between ROIs 1 and \n"
"     4, then the label might be '_M_<->003<->006', or '_M_<->003<->004', or\n"
"     any two of the connections plus the leading '_M_' that stands for \n"
"     'multiple others' (NB: which two are shown is not controlled, but I \n"
"     figured it was better to show at least some, rather than just the \n"
"     less informative '_M_' alone).  In all of these things, the PAIRMAP\n"
"     map is a useful, fairly efficient guide-check, but the overlaps are\n"
"     difficult to represent fully and efficiently, given the possibly\n"
"     complexity of patterns.  For more definite, unique, and scriptable\n"
"     information of where estimated WM connections are, use the \n"
"     '-dump_rois AFNI' or '-dump_rois AFNI_MAP' option.\n"
" If the '-netrois' input has a labeltable, then this program will program\n"
"     will read it in, use it in PAIRMAP and INDIMAP bricklabels, PAIRMAP\n"
"     subbricks with index >0, *niml.tract outputs and, by default, in the\n"
"     naming of '-dump_rois AFNI' output.  The examples and descriptions\n"
"     directly above still hold, but in cases where the ROI number has an\n"
"     explicit label, then the former is replaced by the latter's string.\n"
"     In cases where an input label table does not cover all ROI values, \n"
"     there is no need to panic-- the explicit input labels will be used\n"
"     wherever possible, and the zero-padded numbers will be used for the \n"
"     remaining cases.  Thus, one might see PAIRMAP labels such as:\n"
"     '003<->Right-Amygdala', '_M_<->ctx-lh-insula<->006', etc.\n"
"\n"
"****************************************************************************\n"
"\n"
"+ RUNNING AND COMMANDLINE OPTIONS: pick a MODEL and a MODE.\n"
" There are now two types of models, DTI and HARDI, that can be tracked.\n"
"     In HARDI, one may have multiple directions per voxel along which tracts\n"
"     may propagate; in DTI, there can be only one. Each MODEL has some\n"
"     required, and some optional, inputs.\n"
" Additionally, tracking is run in one of three modes, as described near the\n"
"     top of this document, '-mode {DET | MINIP | PROB}', for deterministic\n"
"     mini-probabilistic, or full probabilistic tracking, respectively.\n"
"     Each MODE has some required, and some optional, inputs. Some options\n"
"     find work in multiple modes.\n"
" To run '3dTrackID', one needs to have both a model and a mode in mind (and\n"
"     in data...).  Below is a table to show the various options available\n"
"     for the user to perform tracking. The required options for a given\n"
"     model or mode are marked with a single asterisk (*); the options under\n"
"     the /ALL/ column are necessary in any mode. Thus, to run deterministic\n"
"     tracking with DTI data, one *NEEDS* to select, at a minimum:\n"
"         '-mode DET', '-netrois', '-prefix', '-logic';\n"
"     and then there is a choice of loading DTI data, with either:\n"
"         '-dti_in' or '-dti_list',\n"
"     and then one can also use '-dti_extra', '-mask', '-alg_Nseed_Y',\n"
"     et al. from the /ALL/ and DET colums; one canNOT specify '-unc_min_FA'\n"
"     here -> the option is in an unmatched mode column.\n"
"     Exact usages of each option, plus formats for any arguments, are listed\n"
"     below. Default values for optional arguments are also described.\n"
"\n"
"         +-----------------------------------------------------------------+\n"
"         |          COMMAND OPTIONS FOR TRACKING MODES AND MODELS          |\n"
"         +-----------------------------------------------------------------+\n"
"         |     /ALL/         |     DET     |    MINIP    |      PROB       |\n"
"+--------+-------------------+-------------+-------------+-----------------+\n"
"         |{dti_in, dti_list}*|             |             |                 |\n"
"   DTI   | dti_extra         |             |             |                 |\n"
"         | dti_search_NO     |             |             |                 |\n"
"+-~or~---+-------------------+-------------+-------------+-----------------+\n"
"         | hardi_gfa*        |             |             |                 |\n"
"  HARDI  | hardi_dirs*       |             |             |                 |\n"
"         | hardi_pars        |             |             |                 |\n"
"==~and~==+===================+=============+=============+=================+\n"
"         | mode*             |             |             |                 |\n"
" OPTIONS | netrois*          |             |             |                 |\n"
"         | prefix*           |             |             |                 |\n"
"         | mask              |             |             |                 |\n"
"         | thru_mask         |             |             |                 |\n"
"         | targ_surf_stop    |             |             |                 |\n"
"         | targ_surf_twixt   |             |             |                 |\n"
"         |                   | logic*      | logic*      |                 |\n"
"         |                   |             | mini_num*   |                 |\n"
"         |                   |             | uncert*     | uncert*         |\n"
"         |                   |             | unc_min_FA  | unc_min_FA      |\n"
"         |                   |             | unc_min_V   | unc_min_V       |\n"
"         | algopt            |             |             |                 |\n"
"         | alg_Thresh_FA     |             |             |                 |\n"
"         | alg_Thresh_ANG    |             |             |                 |\n"
"         | alg_Thresh_Len    |             |             |                 |\n"
"         |                   | alg_Nseed_X | alg_Nseed_X |                 |\n"
"         |                   | alg_Nseed_Y | alg_Nseed_Y |                 |\n"
"         |                   | alg_Nseed_Z | alg_Nseed_Z |                 |\n"
"         |                   |             |             | alg_Thresh_Frac |\n"
"         |                   |             |             | alg_Nseed_Vox   |\n"
"         |                   |             |             | alg_Nmonte      |\n"
"         | uncut_at_rois     |             |             |                 |\n"
"         | do_trk_out        |             |             |                 |\n"
"         | dump_rois         |             |             |                 |\n"
"         | dump_no_labtab    |             |             |                 |\n"
"         | dump_lab_consec   |             |             |                 |\n"
"         | posteriori        |             |             |                 |\n"
"         | rec_orig          |             |             |                 |\n"
"         | tract_out_mode    |             |             |                 |\n"
"         | write_opts        |             |             |                 |\n"
"         | write_rois        |             |             |                 |\n"
"         | pair_out_power    |             |             |                 |\n"
"+--------+-------------------+-------------+-------------+-----------------+\n"
"*above, asterisked options are REQUIRED for running the given '-mode'.\n"
" With DTI data, one must use either '-dti_in' *or* '-dti_list' for input.\n"
"\n"
" FOR MODEL DTI:\n"
"    -dti_in  INPREF :basename of DTI volumes output by, e.g., 3dDWItoDT.\n"
"                     NB- following volumes are *required* to be present:\n"
"                     INPREF_FA, INPREF_MD, INPREF_L1,\n" 
"                     INPREF_V1, INPREF_V2, INPREF_V3,\n"
"                     and (now) INPREF_RD (**now output by 3dTrackID**).\n"
"                     Additionally, the program will search for all other\n"
"                     scalar (=single brik) files with name INPREF* and will\n"
"                     load these in as additional quantities for WM-ROI\n"
"                     stats; this could be useful if, for example, you have\n"
"                     PD or anatomical measures and want mean/stdev values\n"
"                     in the WM-ROIs (to turn this feature off, see below,\n"
"                     'dti_search_NO'); all the INPREF* files must be in same\n"
"                     DWI space.\n"
"                     Sidenote: including/omitting a '_' at the end of INPREF\n"
"                     makes no difference in the hunt for files.\n"
"    -dti_extra SET  :if you want to use a non-FA derived definition for the\n"
"                     WM skeleton in which tracts run, you can input one, and\n"
"                     then the threshold in the -algopt file (or, via the\n"
"                     '-alg_Thresh_FA' option) will be applied to \n"
"                     thresholding this SET; similarly for the minimum\n"
"                     uncertainty by default will be set to 0.015 times the\n"
"                     max value of SET, or can be set with '-unc_min_FA'.\n"
"                     If the SET name is formatted as INPREF*, then it will\n"
"                     probably be included twice in stats, but that's not the\n"
"                     worst thing. In grid files, name of this quantity will\n"
"                     be 'XF' (stands for 'extra file').\n"
"    -dti_search_NO  :turn off the feature to search for more scalar (=single\n"
"                     brik) files with INPREF*, for including stats in output\n"
"                     GRID file. Will only go for FA, MD, L1 and RD scalars\n"
"                     with INPREF.\n"
"    -dti_list FILE  :an alternative way to specify DTI input files, where\n"
"                     FILE is a NIML-formatted text file that lists the\n"
"                     explicit/specific files for DTI input.  This option is\n"
"                     used in place of '-dti_in' and '-dti_extra' for loading\n"
"                     data sets of FA, MD, L1, etc.  An 'extra' set (XF) can\n"
"                     be loaded in the file, as well as supplementary scalar\n"
"                     data sets for extra WM-ROI statistics.\n"
"                     See below for a 'DTI LIST FILE EXAMPLE'.\n"
" FOR MODEL HARDI:\n"
"    -hardi_gfa GFA  :single brik data set with generalized FA (GFA) info.\n" 
"                     In reality, it doesn't *have* to be a literal GFA, esp.\n"
"                     if you are using some HARDI variety that doesn't have\n"
"                     a specific GFA value-- in such a case, use whatever\n"
"                     could be thresholded as your proxy for WM.\n"
"                     The default threshold is still 0.2, so you will likely\n"
"                     need to set a new one in the '-algopt ALG_FILE' file or\n"
"                     from the commandline with '-alg_Thresh_FA', which does\n"
"                     apply to the GFA in the HARDI case as well.\n"
"                     Stats in GRID file are output under name 'GFA'.\n"
"   -hardi_dirs DIRS :For tracking if X>1 propagation directions per voxel\n" 
"                     are given, for example if HARDI data is input. DIRS\n"
"                     would then be a file with 3*X briks of (x,y,z) ordered,\n"
"                     unit magnitude vector components;  i.e., brik [0]\n"
"                     contains V1_x, [1] V1_y, [2] V1_z, [3] V2_x, etc.\n"
"                     (NB: even if X=1, this option works, but that would\n"
"                     seem to take the HAR out of HARDI...)\n"
"   -hardi_pars PREF :search for scalar (=single brik) files of naming\n"
"                     format PREF*.  These will be read in for WM-ROI stats\n"
"                     output in the GRID file.  For example, if there are\n"
"                     some files PREF_PD.nii.gz, PREF_CAT.nii.gz and\n"
"                     PREF_DOG.nii.gz, they will be labelled in the GRID file\n"
"                     as 'PD', 'CAT' and 'DOG' (that '_' will be cut out).\n"
" MODEL-INDEPENDENT OPTIONS:\n"
"    -mode  MODUS    :this necessary option is used to define whether one is\n"
"                     performing deterministic, mini-probabilistic or full-\n"
"                     probabilistic tractography, by selecting one of three\n"
"                     respective modes:  DET, MINIP, or PROB.\n"
"    -netrois ROIS   :mask(s) of target ROIs- single file can have multiple\n"
"                     briks, one per network. The target ROIs through which\n"
"                     tracks will be kept should have index values >0. It is\n"
"                     also possible to define anti-targets (exclusionary\n"
"                     regions) which stop a propagating track... in its \n"
"                     tracks. These are defined per network (i.e., per brik)\n"
"                     by voxels with values <0.\n"
"    -prefix  PREFIX :output file name part.\n"
"    -mask   MASK    :can include a brainmask within which to calculate \n"
"                     things. Otherwise, data should be masked already.\n"
"    -thru_mask TM   :optional extra restrictor mask, through which paths are\n"
"                     (strictly) required to pass in order to be included\n"
"                     when passing through or connecting targets. It doesn't\n"
"                     discriminate based on target ROI number, so it's\n"
"                     probably mostly useful in examining specific pairwise\n"
"                     connections. It is also not like one of the target\n"
"                     '-netrois' in that no statistics are calculated for it.\n"
"                     Must be same number of briks as '-netrois' set.\n"
"    -targ_surf_stop :make the final tracts and tracked regions stop at the\n"
"                     outer surface of the target ROIs, rather than being\n"
"                     able to journey arbitrarily far into them (latter being\n"
"                     the default behavior.  Might be useful when you want\n"
"                     meaningful distances *between* targets.  Tracts stop\n"
"                     after going *into* the outer layer of a target.\n"
"                     This can be applied to DET, MINIP, or PROB modes.\n"
"                     NB: this only affects the connections between pairs\n"
"                     of targets (= AND-logic, off-diagonal elements in\n"
"                     output matrices), not the single-target tracts\n"
"                     (= OR-logic, on-diagonal elements in output\n"
"                     matrices); see also a related option, below.\n"
"   -targ_surf_twixt :quite similar to '-targ_surf_stop', above, but the\n"
"                     tracts stop *before* entering the target surfaces, so\n"
"                     that they are only between (or betwixt) the targets.\n"
"                     Again, only affects tracts between pairs of targets.\n"
"\n"
"    -logic {OR|AND} :when in one of '-mode {DET | MINIP}', one will look for\n"
"                     either OR- or AND-logic connections among target ROIs\n"
"                     per network (multiple networks can be entered as\n"
"                     separate briks in '-netrois ROIS'): i.e., one keeps\n"
"                     either any track going through at least one network ROI\n"
"                     or only those tracks which join a pair of ROIs.\n"
"                     When using AND here, default behavior is to only keep\n"
"                     voxels in tracks between the ROIs they connect (i.e.,\n"
"                     cut off track bits which run beyond ROIs).\n"
"    -mini_num NUM   :will run a small number NUM of whole brain Monte Carlo\n"
"                     iterations perturbing relevant tensor values in accord\n"
"                     with their uncertainty values (hence, the need for also\n"
"                     using `-uncert' with this option). This might be useful\n"
"                     for giving a flavor of a broader range of connections\n"
"                     while still seeing estimated tracks themselves. NB: if\n"
"                     NUM is large, you might be *big* output track files;\n"
"                     e.g., perhaps try NUM = 5 or 9 or so to start.\n"
"                     Requires '-mode MINIP' in commandline.\n"
"    -bundle_thr V   :the number of tracts for a given connection is called\n"
"                     a bundle. For '-mode {DET | MINIP}', one can choose to\n"
"                     NOT output tracts, matrix info, etc. for any bundle\n"
"                     with fewer than V tracts. This might be useful to weed\n"
"                     out ugly/false tracts (default: V=1).\n"
"    -uncert U_FILE  :when in one of '-mode {MINIP | PROB}', uncertainty\n"
"                     values for eigenvector and WM skeleton (FA, GFA, etc.)\n"
"                     maps are necessary.\n"
"                     When using DTI ('-dti_*'), then use the 6-brik file\n"
"                     from 3dDWUncert; format of the file given below.\n"
"                     When using HARDI ('-hardi_*') with up to X directions\n"
"                     per voxel, one needs U_FILE to have X+1 briks, where\n"
"                     U_FILE[0] is the uncertainty for the GFAfile, and the\n"
"                     other briks are ordered for directions given with\n"
"                     '-hardi_dirs'.\n"
"                     Whatever the values in the U_FILE, this program asserts\n"
"                     a minimum uncertainty of stdevs, with defaults:\n"
"                     for FA it is 0.015, and for GFA or -dti_extra sets it\n"
"                     is 0.015 times the max value present (set with option\n"
"                     '-unc_min_FA');\n"
"                     for each eigenvector or dir, it is 0.06rad (~3.4deg)\n"
"                     (set with option '-unc_min_V')\n"
"   -unc_min_FA VAL1 :when using '-uncert', one can control the minimum\n"
"                     stdev for perturbing FA (in '-dti_in'), or the EXTRA-\n"
"                     file also in DTI ('-dti_extra'), or GFA (in '-hardi_*).\n"
"                     Default value is: 0.015 for FA, and 0.015 times the max\n"
"                     value in the EXTRA-file or in the GFA file.\n"
"    -unc_min_V VAL2 :when using '-uncert', one can control the minimum\n"
"                     stdev for perturbing eigen-/direction-vectors.\n"
"                     In DTI, this is for tipping e_1 separately toward e2\n"
"                     and e3, and in HARDI, this is for defining a single\n"
"                     degree of freedom uncertainty cone. Default values are\n"
"                     0.06rad (~3.4deg) for any eigenvector/direction. User\n"
"                     assigns values in degrees.\n"
"\n"
"   -algopt A_FILE   :simple ASCII file with six numbers defining tracking \n"
"                     parameter quantities (see list below); note the\n"
"                     differences whether running in '-mode {DET | MINIP}'\n"
"                     or in '-mode PROB': the first three parameters in each\n"
"                     mode are the same, but the next three differ.\n"
"                     The file can be in the more understandable html-type\n"
"                     format with labels per quantity, or just as a column\n"
"                     of the numbers, necessarily in the correct order.\n"
"                     NB: each quantity can also be changed individually\n"
"                     using a commandline option (see immediately following).\n"
"                     If A_FILE ends with '.niml.opts' (such as would be\n"
"                     produced using the '-write_opts' option), then it is\n"
"                     expected that it is in nice labelled NIML format;\n"
"                     otherwise, the file should just be a column of numbers\n"
"                     in the right order. Examples of A_FILEs are given at\n"
"                     the end of the option section.\n"
"  -alg_Thresh_FA  A :set threshold for DTI FA map, '-dti_extra' FILE, or \n"
"                     HARDI GFA map (default = 0.2).\n"
"  -alg_Thresh_ANG B :set max angle (in deg) for turning when going to a new\n"
"                     voxel during propagation (default = 60).\n"
"  -alg_Thresh_Len C :min physical length (in mm) of tracts to keep\n"
"                     (default = 20).\n"
"  -alg_Nseed_X    D :Number of seeds per vox in x-direc (default = 2).\n"
"  -alg_Nseed_Y    E :Number of seeds per vox in y-direc (default = 2).\n"
"  -alg_Nseed_Z    F :Number of seeds per vox in z-direc (default = 2).\n"
"           +-------> NB: in summation, for example the alg_Nseed_* options\n"
"                        for '-mode {DET | MINIP} place 2x2x2=8 seed points,\n"
"                        equally spread in a 3D cube, in each voxel when\n"
"                        tracking.\n"
" -alg_Thresh_Frac G :value for thresholding how many tracks must pass\n"
"                     through a voxel for a given connection before it is\n"
"                     included in the final WM-ROI of that connection.\n"
"                     It is a decimal value <=1, which will multiply the\n"
"                     number of 'starting seeds' per voxel, Nseed_Vox*Nmonte\n"
"                     (see just below for those). (efault = 0.001; for higher\n"
"                     specificity, a value of 0.01-0.05 could be used).\n"
"  -alg_Nseed_Vox  H :number of seeds per voxel per Monte Carlo iteration;\n"
"                     seeds will be placed randomly (default = 5).\n"
"  -alg_Nmonte     I :number of Monte Carlo iterations (default = 1000).\n"
"           +-------> NB: in summation, the preceding three options for the\n"
"                        '-mode PROB' will mean that 'I' Monte Carlo\n"
"                        iterations will be run, each time using 'H' track\n"
"                        seeds per relevant voxel, and that a voxel will\n"
"                        need 'G*H*I' tracks of a given connection through\n"
"                        it to be included in that WM-ROI. Default example:\n"
"                        1000 iterations with 5 seeds/voxel, and therefore\n"
"                        a candidate voxel needs at least 0.001*5*1000 = 5\n"
"                        tracks/connection.\n"
"\n"
"    -extra_tr_par   :run three extra track parameter scalings for each\n"
"                     target pair, output in the *.grid file. The NT value\n"
"                     of each connection is scaled in the following manners\n"
"                     for each subsequent matrix label:\n"
"                        NTpTarVol  :div. by average target volume;\n"
"                        NTpTarSA   :div. by average target surface area;\n"
"                        NTpTarSAFA :div. by average target surface area\n"
"                                    bordering suprathreshold FA (or equi-\n"
"                                    valent WM proxy definition).\n"
"                     NB: the volume and surface area numbers are given in\n"
"                     terms of voxel counts and not using physical units\n"
"                     (consistent: NT values themselves are just numbers.)\n"
"    -uncut_at_rois  :when looking for pairwise connections, keep entire\n"
"                     length of any track passing through multiple targets,\n"
"                     even when part ~overshoots a target (i.e., it's not\n"
"                     between them).  When using OR tracking, this is\n"
"                     automatically applied.  For probabilistic tracking, not\n"
"                     recommended to use (are untrimmed ends meaningful\?).\n"
"                     The default behavior is to trim the tracts that AND-\n"
"                     wise connect targets to only include sections that are\n"
"                     between the targets, and not parts that run beyond one.\n"
"                     (Not sure why one would want to use this option, to be\n"
"                     honest; see '-targ_surf_stop' for really useful tract\n"
"                     control.)\n"
"    -dump_rois TYPE :can output individual masks of ROI connections.\n"
"                     Options for TYPE are: {DUMP | AFNI | BOTH | AFNI_MAP}.\n"
"                     Using DUMP gives a set of 4-column ASCII files, each \n"
"                     formatted like a 3dmaskdump data set; it can be recon-\n"
"                     stituted using 3dUndump. Using AFNI gives a set of\n"
"                     BRIK/HEAD (byte) files in a directory called PREFIX; \n"
"                     using AFNI_MAP is like using AFNI, but it gives non-\n"
"                     binarized *maps* of ROI connections.\n"
"                     Using BOTH produces AFNI and DUMP formats of outputs.\n"
"    -dump_no_labtab :if the ROIS file has a label table, the default is to\n"
"                     use it in naming a '-dump_rois' output (if being used);\n"
"                     using this switch turn that off-- output file names \n"
"                     will be the same as if no label table were present.\n"
"   -dump_lab_consec :if using `-dump_rois', then DON'T apply the numerical\n"
"                     labels of the original ROIs input to the output names.\n"
"                     This would only matter if input ROI labels aren't \n"
"                     consecutive and starting with one (e.g., if instead \n"
"                     they were 1,2,3,5,8,..).\n"
"          --->   This is the opposite  from previous default behavior, where\n"
"                     the option '-lab_orig_rois' was used to switch away\n"
"                     from consecutive-izing the labels in the output.\n"
"    -posteriori     :switch to have a bunch of individual files output, with\n"
"                     the value in each being the number of tracks per voxel\n"
"                     for that pair; works with '-dump_rois {AFNI | BOTH }',\n"
"                     where you get track-path maps instead of masks; makes\n"
"                     threshold for number of tracks between ROIs to keep to\n"
"                     be one automatically, regardless of setting in algopt.\n"
"    -rec_orig       :record dataset origin in the header of the *.trk file.\n"
"                     As of Sept. 2012, TrackVis doesn't use this info so it\n"
"                     wasn't included, but if you might want to map your \n"
"                     *.trk file later, then use the switch as the datasets's\n"
"                     Origin is necessary info for the mapping (the default\n"
"                     image in TrackVis might not pop up in the center of the\n"
"                     viewing window, though, just be aware). NB: including\n"
"                     the origin might become default at some point in time.\n"
"    -do_trk_out     :Switch ON outputting *.trk files, which are mainly to\n"
"                     be viewed in TrackVis (Wang et al., 2007). \n"
"                     (Feb, 2015): Default is to NOT output *.trk files.\n"
"    -nifti          :output the PAIRMAP, INDIMAP, and any '-dump_rois' in\n"
"                     *.nii.gz format (default is BRIK/HEAD).\n"
"  -no_indipair_out  :Switch off outputting *INDIMAP* and *PAIRMAP* volumes.\n"
"                     This is probably just if you want to save file space;\n"
"                     also, for connectome-y studies with many (>100) target\n"
"                     regions, the output INDI and PAIR maps can be quite\n"
"                     large and/or difficult to write out. In some cases, it\n"
"                     might be better to just use '-dump_rois AFNI' instead.\n"
"                     Default is to output the INDI and PAIR map files.\n"
"    -write_rois     :write out a file (PREFIX.roi.labs) of all the ROI \n" 
"                     (re-)labels, for example if the input ROIs aren't\n"
"                     simply consecutive and starting from 1. File has 3cols:\n"
"                       Input_ROI   Condensed_form_ROI   Power_of_2_label\n"
"    -write_opts     :write out all the option values into PREFIX.niml.opts.\n" 
"    -pair_out_power :switch to affect output of *PAIRMAP* output files. \n"
"                     Now, the default format is to output the >0 bricks with\n"
"                     tracks labelled by the target integers themselves.\n"
"                     This is not a unique labelling system, but it *is* far\n"
"                     easier to view and understand what's going on than\n"
"                     using a purely unique system based on using powers of\n"
"                     two of the ROIs (with integer summation for overlaps).\n" 
"                     Using the switch '-pair_out_power' will change the\n"
"                     output of bricks [1] and higher to contain\n"
"                     information on connections stored as powers of two, so\n"
"                     that there is a unique decomposition in terms of\n"
"                     overlapped connections. However, it's *far* easier to\n"
"                     use '-dump_rois {AFNI | BOTH }' to get individual mask\n"
"                     files of the ROIs clearly (as well as annoying to need\n"
"                     to calculate powers of two simply to visualize the\n"
"                     connections.\n"
"                 -----> when considering this option, see the 'LABELTABLE'\n"
"                        description up above for how the labels work, with\n"
"                        or without an explicit table being entered.\n"
"    -verb VERB      :verbosity level, default is 0.\n"
"\n"
"****************************************************************************\n"
"\n"
"+ ALGOPT FILE EXAMPLES (note that different MODES have some different opts):\n"
"  For '-mode {DET | MINIP}, the nicely readable NIML format of algopt file\n"
"  would have a file name ending '.niml.opts' and contain something like the:\n"
"  following seven lines:\n"
"      <TRACK_opts\n"
"         Thresh_FA=\"0.2\"\n"
"         Thresh_ANG=\"60.000000\"\n"
"         Thresh_Len=\"20.000000\"\n"
"         Nseed_X=\"2\"\n"
"         Nseed_Y=\"2\"\n"
"         Nseed_Z=\"2\" />\n"
"  The values above are actually all default values, and such a file would be\n"
"  output using the '-write_opts' flag. For the same modes, one could get\n"
"  the same result using a plain column of numbers, whose meaning is defined\n"
"  by their order, contained in a file NOT ending in .niml.opts, such as \n"
"  exemplified in the next six lines:\n"
"         0.2\n"
"         60\n"
"         20\n"
"         2\n"
"         2\n"
"         2\n"
"  For '-mode PROB', the nice NIML format algopt file would contain something\n"
"  like the next seven lines (again requiring the file name to end in\n"
"  '.niml.opts'):\n"
"      <TRACK_opts\n"
"         Thresh_FA=\"0.2\"\n"
"         Thresh_ANG=\"60.0\"\n"
"         Thresh_Len=\"20.0\"\n"
"         Thresh_Frac=\"0.001\"\n"
"         Nseed_Vox=\"5\"\n"
"         Nmonte=\"1000\" />\n"
"  Again, those represent the default values, and could be given as a plain\n"
"  column of numbers, in that order.\n"
"\n"
"* * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * ** * **\n"
"\n"
"+ DTI LIST FILE EXAMPLE:\n"
"     Consider, for example, if you hadn't used the '-sep_dsets' option when\n"
"     outputting all the tensor information from 3dDWItoDT.  Then one could\n"
"     specify the DTI inputs for this program with a file called, e.g., \n"
"     FILE_DTI_IN.niml.opts (the name *must* end with '.niml.opts'):\n"
"       <DTIFILE_opts\n"
"         dti_V1=\"SINGLEDT+orig[9..11]\"\n"
"         dti_V2=\"SINGLEDT+orig[12..14]\"\n"
"         dti_V3=\"SINGLEDT+orig[15..17]\"\n"
"         dti_FA=\"SINGLEDT+orig[18]\"\n"
"         dti_MD=\"SINGLEDT+orig[19]\"\n"
"         dti_L1=\"SINGLEDT+orig[6]\"\n"
"         dti_RD=\"SINGLEDT+orig[20]\" />\n"
"     This represents the *minimum* set of input files needed when running\n"
"     3dTrackID.  (Oct. 2016: RD now output by 3dDWItoDT, and not calc'ed \n"
"     internally by 3dTrackID.)\n"
"     One could also input extra data: an 'extra file' (XF) to take the place\n"
"     of an FA map for determining where tracks can propagate; and up to four\n"
"     other data sets (P1, P2, P3 and P4, standing for 'plus one' etc.) for\n"
"     calculating mean/stdev properties in the obtained WM-ROIs:\n"
"       <DTIFILE_opts\n"
"         dti_V1=\"SINGLEDT+orig[9..11]\"\n"
"         dti_V2=\"SINGLEDT+orig[12..14]\"\n"
"         dti_V3=\"SINGLEDT+orig[15..17]\"\n"
"         dti_XF=\"Segmented_WM.nii.gz\"\n"
"         dti_FA=\"SINGLEDT+orig[18]\"\n"
"         dti_MD=\"SINGLEDT+orig[19]\"\n"
"         dti_L1=\"SINGLEDT+orig[6]\"\n"
"         dti_RD=\"SINGLEDT+orig[20]\"\n"
"         dti_P1=\"SINGLEDT+orig[7]\"\n"
"         dti_P2=\"SINGLEDT+orig[8]\"\n"
"         dti_P3=\"T1_map.nii.gz\"\n"
"         dti_P4=\"PD_map.nii.gz\" />\n"
"\n"
"****************************************************************************\n"
"\n"
"+ EXAMPLES:\n"
"   Here are just a few scenarios-- please see the Demo data set for *maaany*\n"
"   more, as well as for fuller descriptions.  To obtain the Demo, type the\n"
"   following into a commandline:\n"
"   $ @Install_FATCAT_demo\n"
"   This will also unzip the archive, which contains required data (so it's\n"
"   pretty big, currently >200MB), a README.txt file, and several premade\n"
"   scripts that are ~heavily commented.\n"
"\n"
"   A) Deterministic whole-brain tracking; set of targets is just the volume\n"
"      mask. This can be useful for diagnostic purposes, sanity check for\n"
"      gradients+data, for interactively selecting interesting subsets later,\n"
"      etc. This uses most of the default algopts, but sets a higher minimum\n"
"      length for keeping tracks:\n"
"      $ 3dTrackID -mode DET                \\\n"
"                  -dti_in DTI/DT           \\\n"
"                  -netrois mask_DWI+orig   \\\n"
"                  -logic OR                \\\n"
"                  -alg_Thresh_Len 30       \\\n"
"                  -prefix DTI/o.WB\n"
"\n"
"   B) Mini-probabilistic tracking through a multi-brik network file using a\n"
"      DTI model and AND-logic. Instead of using the thresholded FA map to\n"
"      guide tracking, an extra data set (e.g., a mapped anatomical\n"
"      segmentation image) is input as the WM proxy; as such, what used to be\n"
"      a threshold for adult parenchyma FA is now changed to an appropriate\n"
"      value for the segmentation percentages; and this would most likely\n"
"      also assume that 3dDWUncert had been used to calculate tensor value\n"
"      uncertainties:\n"
"      $ 3dTrackID -mode MINIP                      \\\n"
"                  -dti_in DTI/DT                   \\\n"
"                  -dti_extra T1_WM_in_DWI.nii.gz   \\\n"
"                  -netrois ROI_ICMAP_GMI+orig      \\\n"
"                  -logic AND                       \\\n"
"                  -mini_num 7                      \\\n"
"                  -uncert DTI/o.UNCERT_UNC+orig.   \\\n"
"                  -alg_Thresh_FA 0.95              \\\n"
"                  -prefix DTI/o.MP_AND_WM \n"
"\n"
"   C) Full probabilistic tracking through a multi-brik network file using\n"
"      HARDI-Qball reconstruction. The designated GFA file is used to guide\n"
"      the tracking, with an appropriate threshold set and a smaller minimum\n"
"      uncertainty of that GFA value (from this and example B, note how\n"
"      generically the '-alg_Thresh_FA' functions, always setting a value for\n"
"      the WM proxy map, whether it be literally FA, GFA or the dti_extra\n"
"      file). Since HARDI-value uncertainty isn't yet calculable in AFNI,\n"
"      brain-wide uniform values were assigned to the GFA and directions:\n"
"      $ 3dTrackID -mode PROB                       \\\n"
"                  -hardi_gfa QBALL/GFA.nii.gz      \\\n"
"                  -hardi_dirs QBALL/dirs.nii.gz    \\\n"
"                  -netrois ROI_ICMAP_GMI+orig      \\\n"
"                  -uncert QBALL/UNIFORM_UNC+orig.  \\\n"
"                  -mask mask_DWI+orig              \\\n"
"                  -alg_Thresh_FA 0.04              \\\n"
"                  -unc_min_FA 0.003                \\\n"
"                  -prefix QBALL/o.PR_QB \n"
"\n"
"****************************************************************************\n"
"\n"
"  If you use this program, please reference the workhorse FACTID\n"
"  tractography algorithm:\n"
"    Taylor PA, Cho K-H, Lin C-P, Biswal BB (2012). Improving DTI\n"
"    Tractography by including Diagonal Tract Propagation. PLoS ONE\n"
"    7(9): e43415. \n"
"  and the introductory/description paper for FATCAT:\n"
"    Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And\n"
"    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.\n\n");
	return;
}
		


int main(int argc, char *argv[]) 
{

   TRACK_RUN_PARAMS InOpts;
   int i;
   int iarg;
   
   // fill option struct with defaults
   InOpts = SetInitialTrackingDefaults();

	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************
  
	//	INFO_message("version: CHI");
	mainENTRY("3dTrackID"); machdep(); 
  
	// scan args 
	if (argc == 1) { usage_TrackID(1); exit(0); }
	iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_TrackID(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}

		if( strcmp(argv[iarg],"-verb") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-verb'") ;
			set_tract_verb(atoi(argv[iarg]));
			iarg++ ; continue ;
		}
	 
		if( strcmp(argv[iarg],"-write_opts") == 0) {
			InOpts.dump_opts=1;
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-write_rois") == 0) {
			InOpts.ROIS_OUT=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-pair_out_power") == 0) {
			InOpts.PAIRPOWER=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-pair_out_int") == 0) {
         WARNING_message("(Harmless note:) the option '-pair_out_int' will"
                         " not be used anymore,\n"
                         "\t as it describes default behavior of PAIRMAP"
                         " file output;\n"
                         "\t see '-pair_out_power' for the ugly but unique"
                         " file labelling output.");
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-extra_tr_par") == 0) {
         // scale NT grids by: 
         // 1) target vol, 
         // 2) target SA, 
         // 3) and target SA near WM.
			InOpts.EXTRA_TR_PAR = N_XTR_PAR_LABS; 
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-dump_lab_consec") == 0) {
			InOpts.DUMP_ORIG_LABS=0;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-nifti") == 0) {
			InOpts.NIFTI_OUT=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-netrois") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-netrois'") ;

         InOpts.NAMEIN_netrois = strdup(argv[iarg]); 
			
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-uncert") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-uncert'") ;

         InOpts.NAMEIN_uncert = strdup(argv[iarg]); 

			iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-unc_min_FA") == 0) {
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-unc_min_FA'") ;

         InOpts.NAMEIN_uncFA = strdup(argv[iarg]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-unc_min_V") == 0) {		
         if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-unc_min_V'") ;

         InOpts.NAMEIN_uncEI = strdup(argv[iarg]);

         iarg++ ; continue ;
      }

		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			InOpts.prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(InOpts.prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
			iarg++ ; continue ;
		}
      
		if( strcmp(argv[iarg],"-dti_in") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-in_dti'");
         InOpts.dti_inpref = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-dti_search_NO") == 0) {
			InOpts.NO_NONDTI_SEARCH = 1;
			iarg++ ; continue ;
		}

      // tracts are chopped at targ surface bounds
		if( strcmp(argv[iarg],"-targ_surf_stop") == 0) {
			InOpts.TARG_SURF_STOP = 1;
			iarg++ ; continue ;
		}
      // ... or prevent from penetrating bounds
		if( strcmp(argv[iarg],"-targ_surf_twixt") == 0) {
			InOpts.TARG_SURF_STOP = 1;
			InOpts.TARG_SURF_TWIXT = 1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-dti_list") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-dti_list'");
         InOpts.dti_listname = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-algopt") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-algopt'");
         InOpts.algopt_file_name = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}

      // can also set options at commandline
      if( strcmp(argv[iarg],"-alg_Thresh_FA") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Thresh_FA'");

         InOpts.MinFA = atof(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Thresh_ANG") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Thresh_Ang'");
         InOpts.MaxAngDeg = atof(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Thresh_Len") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Thresh_Len'");
         InOpts.MinL = atof(argv[iarg]);
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Thresh_Frac") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Thresh_Frac'");
         InOpts.NmNsFr = atof(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Nseed_Vox") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Nseed_Vox'");
         InOpts.Nseed = atoi(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Nmonte") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Nmonte'");
         InOpts.Nmonte = atoi(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Nseed_X") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Nseed_X'");
         InOpts.SeedPerV[0] = atoi(argv[iarg]);

         iarg++ ; continue ;
      }
		if( strcmp(argv[iarg],"-alg_Nseed_Y") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Nseed_Y'");
         InOpts.SeedPerV[1] = atoi(argv[iarg]);

         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-alg_Nseed_Z") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-alg_Nseed_Z'");
         InOpts.SeedPerV[2] = atoi(argv[iarg]);

         iarg++ ; continue ;
      }

      // Not in main code for now
      if( strcmp(argv[iarg],"-choose_seed") == 0) {
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-choose_seed'");
         InOpts.CHOOSE_SEED = atoi(argv[iarg]);
         
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-no_trk_out") == 0) {
         WARNING_message("The switch '-no_trk_out' is no longer used."
                         " As of Feb, 2015, TrackVis-style *.trk"
                         "\n files are no longer created by default."
                         " To output them, use '-do_trk_out'." );
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-do_trk_out") == 0) {
         InOpts.OUTPUT_TRK = 1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-no_indipair_out") == 0) {
         InOpts.OUT_INDIPAIR = 0;
         iarg++ ; continue ;
      }

		if( strcmp(argv[iarg],"-dti_extra") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-dti_extra'");

         InOpts.in_EXTRA = strdup(argv[iarg]) ;
         
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-mask") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-mask'");
			
         InOpts.NAMEIN_mask = strdup( argv[iarg]);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-thru_mask") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-thru_mask'");
			
         InOpts.NAMEIN_thru = strdup( argv[iarg]);
			
			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-uncut_at_rois") == 0) {
			InOpts.ONLY_BT=0;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-posteriori") == 0) {
         InOpts.POST=1;
         iarg++ ; continue ;
		} 

      if( strcmp(argv[iarg],"-mode") == 0) { // @)
         iarg++ ; 
         if( iarg >= argc ) 
            ERROR_exit("Need argument after '-mode'");

         InOpts.NAMEIN_mode = strdup(argv[iarg]);

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-logic") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-logic'");

         INFO_message("ROI logic type is: %s",argv[iarg]);
         if( strcmp(argv[iarg],"AND") == 0 ) 
            InOpts.LOG_TYPE = 1;
         else if( strcmp(argv[iarg],"OR") == 0 )
            InOpts.LOG_TYPE = 0;
         else 
            ERROR_exit("Illegal after '-logic': need 'OR' or 'AND'");

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-hardi_dirs") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-hardi_dirs'");
         InOpts.hardi_dir = strdup(argv[iarg]) ;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-hardi_gfa") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-hardi_gfa'");
         InOpts.hardi_gfa = strdup(argv[iarg]) ;

         iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-hardi_pars") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-hardi_pars'");
         InOpts.hardi_pars = strdup(argv[iarg]) ;
         
         iarg++ ; continue ;
		}

      // can do a mini monte carlo if you want; for example, use 1
      // seed per ROI and then see more locs to tracks possibly; prob
      // don't want to make tooo large for visualization stuff.
      if( strcmp(argv[iarg],"-mini_num") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mini_num'");
        
         InOpts.MINI_PROB_NM = atoi(argv[iarg]);
         
         iarg++ ; continue ;
		}

      // Feb,2015
      if( strcmp(argv[iarg],"-bundle_thr") == 0) { // @)
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-bundle_thr'");
        
         InOpts.THRESH_BUNDS = atoi(argv[iarg]);
         
         iarg++ ; continue ;
		}


      /*    TURNED OFF FOR MOMENT, WORK IN PROGRESS
            if( strcmp(argv[iarg],"-jump_les") == 0) { // @)) prop check
            JUMPLES=1;
            iarg++ ; continue ;
            } */
      
		if( strcmp(argv[iarg],"-rec_orig") == 0) { // @)
			InOpts.RECORD_ORIG=1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-tract_out_mode") == 0) { // @)
         if( ++iarg >= argc ) 
            ERROR_exit("Need argument after '-tract_out_mode'") ;

         InOpts.NAMEIN_outmode = strdup(argv[iarg]);

         iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-dump_rois") == 0) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-dump_rois'");

			if( strcmp(argv[iarg],"DUMP") == 0 ) 
				InOpts.DUMP_TYPE = 1;
			else if( strcmp(argv[iarg],"AFNI") == 0 ) 
				InOpts.DUMP_TYPE = 2;
         else if( strcmp(argv[iarg],"AFNI_MAP") == 0 ) 
				InOpts.DUMP_TYPE = 4;
			else if( strcmp(argv[iarg],"BOTH") == 0 )
				InOpts.DUMP_TYPE = 3;
			else 
				ERROR_exit("Illegal after '-dump_rois': need 'DUMP',"
                       " 'AFNI' or 'BOTH' or the new 'AFNI_MAP'.");

			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-dump_no_labtab") == 0) {
         // if NETROIS has a label table, default is to use it in
         // naming a dump_rois output;  this can turn that off.
			InOpts.DUMP_with_LABELS=0;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-cut_at_rois") == 0) {
         ERROR_exit("No longer used! Default behavior now to cut_at_rois;"
                    " turn *off* with '-uncut_at_rois', if desired");
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-det_net") == 0) {
         ERROR_exit("No longer used! use '-logic {AND | OR}', instead.");
			iarg++ ; continue ;
		}

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}

	if (iarg < 5) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}

   // DONE FILLING, now call
   i = RunTrackingMaestro(1, InOpts, argc, argv);

   if( !i )
      ERROR_message("Error in run?");

   exit(0);
}


// BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB


int RunTrackingMaestro( int comline, TRACK_RUN_PARAMS opts,
                        int argc, char *argv[])
{

   int i,j,k,m,n,ii,jj,kk,mm,gg,nn,hh,bb,cc,rr,ss,uu,vv,ll,lll;
	int nvox_rois=0;
	int nvox_unc=0;
	THD_3dim_dataset *insetUC = NULL;
   THD_3dim_dataset **insetPARS=NULL; // DEF_DTI: [0] X, [1] FA, [2] MD, [3] L1
   THD_3dim_dataset **insetVECS=NULL; // DEF_DTI: [0] V1, [1] V2, [2] V3
   MRI_IMAGE *temp_vec=NULL;

   // default for DTI, but easily changed for HARDI
   int PARS_BOT = 1; // default is to start at brik 1 for pars, with FA there.
   int PARS_TOP = 0; // number of bricks.
   int PARS_N = 0;
   int Noutmat = 0; // number of output matrices in GRID file
   char **ParLab=NULL; 
   int DEF_DTI = 1; // default mode, mainly for # of params
   char **wild_names=NULL; //[N_DTI_MAX_PARS][32]; // get names
   int pref_offset = 0;
   char temp_name[32];
   int hardi_pref_len=0;
   int FOUND_DTI;
   int tot_FOUND_DTI=0;
   int TR_MODE = -1; // -1=ERR; 0=DET; 1=MINIP; 2=PROB;
   char *list_modes[3] = {"DET", "MINIP", "PROB"}; // match with PARLABS later

	THD_3dim_dataset *ROI_set=NULL; 

   char ***gdset_roi_names=NULL;
   SUMA_DSET *gset=NULL;
   float ***flat_matr=NULL;
   float *xyz=NULL;
   char OUT_gdset[300];
   NI_group *GDSET_netngrlink=NULL;
   char *NAME_gdset=NULL;

   THD_3dim_dataset *thrumask=NULL;
   int doesnt_pass;
	int KEEP_GOING=1;

	THD_3dim_dataset *MASK=NULL;
	int HAVE_MASK=0;
	int ***mskd=NULL; // define mask of where time series are nonzero
   int ni=0;
   time_t t_start;

   //char *DTI_SCAL_INS[N_dti_scal] = {"FA", "MD", "L1"}; // match PARLABS later
   //char *DTI_VECT_INS[N_dti_vect] = {"V1", "V2", "V3"};
   //char *DTI_PLUS_INS[N_plus_dtifile] = {"P1", "P2", "P3", "P4"};

	int EXTRAFILE=0; // switch for whether other file is input as WM map

	char OUT_grid[300];
	char OUT_map[300];
	char OUT_mask[300];
	char OUT_rois[300];
	FILE *fin4, *fout1;

	float MaxAng=0.;
	int NmNsThr=0;
	int ArrMax=0;
	float unc_minfa_std=0.015, unc_minei_std=0.06; // min uncert stds.
   int USER_UNC_FA=0, USER_UNC_EI=0; // whether user has input explicit vals

	int Nvox=-1;   // tot number vox
	int Ndata=-1;
   float VoxVol=0;
	int *Dim=NULL; //[3]={0,0,0}; // dim in each dir
	float **LocSeed=NULL; // fractional locations within voxel, 
	float Ledge[3]; // voxel edge lengths

	int **Tforw=NULL, **Tback=NULL;
	int **Ttot=NULL;
	float **flTforw=NULL, **flTback=NULL;
	float **coorded=NULL;
	float **copy_coorded=NULL; // copy will have perturbed info;
	float **UNC=NULL; // for DTI now: just combined pieces, not bias+stdev
	int ***INDEX=NULL,***INDEX2=NULL;
	int **MAPROI=NULL; // store what ROIs are where, per data voxel
	int ***NETROI=NULL; // store connection info
	int len_forw, len_back; // int count of num of squares through
	float phys_forw[1], phys_back[1];
	int idx,idx3;

	int in[3]; // to pass to trackit
	float physin[3]; // also for trackit, physical loc, 
	int totlen; 
	float totlen_phys, phys_l;
	int Numtract; 

   int READS_in;
	float READS_fl;
	int onoff[3];
	int BreakAddCont=0;

	char dset_or[4] = "RAI";
	char *voxel_order=NULL;//[4]="---";
	THD_3dim_dataset *dsetn=NULL;
	int *TV_switch=NULL;//[3] = {0,0,0};

	int **Prob_grid=NULL; // NROIx(NROI+1)/2 grid (NROI is num of ROIs);
	float ***Prob_grid_L=NULL; // for bundle lengths; Nov,2016
	float ***Param_grid=NULL; // same size as Prob_Grid (extra dim for
                             // ROI stats)
	int *list_rois=NULL, *temp_list=NULL;
	int MAXNROI=0;
	int N_nets=0;

	float w2,w3,thetval;
	float tempv[3];
	float tempvmagn;
	float testang;

	int *NROI=NULL,*INVROI=NULL;
	int **ROI_LABELS=NULL, **INV_LABELS=NULL; // allow labels to be non-consec
	NI_element *nel=NULL;
	char *postfix[4]={"+orig.HEAD\0",".nii.gz\0",".nii\0","+tlrc.HEAD\0"};
	int  FOUND = -1;

   // @) this block of values for detnet options
   int DETNET = 0; // switch on detnet
   TAYLOR_NETWORK **tnet=NULL;
   TAYLOR_BUNDLE ***tb=NULL;  // @))), for new IO
   int *N_bund=NULL; // @))) number of possible tri+diag bund/net
	TAYLOR_TRACT **tt=NULL;  
	char *mode = "NI_fast_binary";
   int *id=NULL;
	float **flTtot=NULL;
   int start_loc=0;
   int trL=0;
   char **prefix_det=NULL;
	char OUT_bin[300];
   FILE **fout0=NULL;
   tv_io_header headerDTI = {.id_string = "TRACK\0", 
                             .origin = {0,0,0},   
                             .n_scalars = 3,   // eventually can adjust; 
                             .scal_n[0] = "FA",
                             .scal_n[1] = "MD",
                             .scal_n[2] = "L1",
                             .n_properties = 0,
                             .vox_to_ras = {{0.,0.,0.,0.},{0.,0.,0.,0.},
                                            {0.,0.,0.,0.},{0.,0.,0.,0.}},
                             // reset this later based on actual data set
                             .voxel_order = "RAI\0", 
                             .invert_x = 0,
                             .invert_y = 0,
                             .invert_z = 0,
                             .swap_xy = 0,
                             .swap_yz = 0,
                             .swap_zx = 0,
                             .n_count = 0,
                             .version = 2,
                             .hdr_size = 1000};
   tv_io_header headerHAR = {.id_string = "TRACK\0", 
                             .origin = {0,0,0},   
                             .n_scalars = 1, // niml has all data anyways...
                             //.scal_n[0] = "FA",
                             //.scal_n[1] = "MD",
                             //.scal_n[2] = "L1",
                             .n_properties = 0,
                             .vox_to_ras = {{0.,0.,0.,0.},{0.,0.,0.,0.},
                                            {0.,0.,0.,0.},{0.,0.,0.,0.}},
                             // reset this later based on actual data set
                             .voxel_order = "RAI\0", 
                             .invert_x = 0,
                             .invert_y = 0,
                             .invert_z = 0,
                             .swap_xy = 0,
                             .swap_yz = 0,
                             .swap_zx = 0,
                             .n_count = 0,
                             .version = 2,
                             .hdr_size = 1000};
   
	float Orig[3] = {0.0,0.0,0.0};

   int vA,vA0,vB,vB0,vB1; // will be indices along tracks in NOT-ROI checking
   int TESTCOUNT=0;

   int N_HAR = 0;
   int N_uncert = 0;
   short *DirPerVox=NULL; // can have diff num of dir/vox
   int N_HARPARS=0;

   // @)) jump lesion searching stuff, to be used prob only with DETNET
   int JUMPLES=0; // switch to turn on prop check.
   float ***TROUT=NULL;

   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2; // "-wild_files_noAext_noAview"
   int wild_orig_name = 1; // "-wild_files_orig_name"

   char tprefix[THD_MAX_PREFIX];
   int nfiles;

   int ***ROI_SIZES=NULL;

   Dtable *roi_dtable=NULL;
   char *LabTabStr=NULL;
	char ***ROI_STR_LABELS=NULL;
   char EleNameStr[128];

	// for random number generation
	const gsl_rng_type * T=NULL;
	gsl_rng *r=NULL;
	long seed1, seed2;   
  

   // * * * * * * * * * * * * * * Load general stuff in * * * * * * * * * *
   // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   // Use random seeds, unless special repeat option is on
   if( opts.CHOOSE_SEED )
      seed1 = seed2 = opts.CHOOSE_SEED;
   else {
      seed1 = time(NULL);
      seed2 = time(NULL) ;
   }
      
   srand(seed1);
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);
	gsl_rng_set (r, seed2);
   
   // from '-mode'
   if( opts.NAMEIN_mode ) {
      INFO_message("Tracking mode: %s",opts.NAMEIN_mode);
      if( strcmp(opts.NAMEIN_mode,list_modes[0]) == 0 ) { // det
         DETNET = 1;
         TR_MODE = 0;
      }
      else if( strcmp(opts.NAMEIN_mode,list_modes[1]) == 0 ) { // minip
         DETNET = 1;
         TR_MODE = 1;
      }
      else if( strcmp(opts.NAMEIN_mode,list_modes[2]) == 0 ) { //prob
         DETNET = 0;
         TR_MODE = 2;
      }
      else 
         ERROR_exit("Illegal after '-mode': need '%s' or '%s' or '%s'",
                    list_modes[0], list_modes[1], list_modes[2]);
   }
   else {
      ERROR_message("Failed to load '-mode' option correctly.");
      exit(3);
   }
   
   if( opts.hardi_dir ||  opts.hardi_gfa || opts.hardi_pars )
      DEF_DTI = 0;

   if( opts.hardi_gfa )
      PARS_BOT = 0; 

   // from '-netrois'
   if( opts.NAMEIN_netrois ) {
      ROI_set = THD_open_dataset( opts.NAMEIN_netrois );
      if( ROI_set == NULL ) 
         ERROR_exit("Can't open netrois dataset '%s'", opts.NAMEIN_netrois);
      DSET_load(ROI_set) ; CHECK_LOAD_ERROR(ROI_set) ;
      nvox_rois = DSET_NVOX(ROI_set) ;
      N_nets = DSET_NVALS(ROI_set) ;
      
      NROI = (int *)calloc(N_nets, sizeof(int)); 
      INVROI = (int *)calloc(N_nets, sizeof(int)); 
      
      if( (NROI == NULL) || (INVROI == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(122);
      }
      
      for( i=0 ; i<N_nets ; i++) 
         INVROI[i] = (int) THD_subbrick_max(ROI_set, i, 1);
      
      ROI_LABELS = calloc( N_nets,sizeof(ROI_LABELS));  
      for(i=0 ; i<N_nets ; i++) 
         ROI_LABELS[i] = calloc(INVROI[i]+1,sizeof(int)); 
      
      INV_LABELS = calloc( N_nets,sizeof(INV_LABELS));  
      for(i=0 ; i<N_nets ; i++) 
         INV_LABELS[i] = calloc(INVROI[i]+1,sizeof(int)); 

      if( (ROI_LABELS == NULL) || (ROI_LABELS == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
			
      // after this, ROI_LABELS will have network labels store compactly
      // and NROI will have the actual number of ROI per netw, not just 
      // the max label value
      // INV_LABELS has compact index labels at input label locations,
      // hence is inverse of ROI_LABELS
      // ** This function allows us to switch to talk about with i-th index
      // label is used, using NROI to define sizes of things
      bb = ViveLeRoi(ROI_set, ROI_LABELS, INV_LABELS, NROI, INVROI);
      if( bb != 1)
         ERROR_exit("Problem loading/assigning ROI labels");
			
      for( i=0 ; i<N_nets ; i++) {
         if( NROI[i]>MAXNROI )
            MAXNROI = NROI[i];
      }

      fprintf(stdout,"\n");
      for( bb=0 ; bb<N_nets ; bb++)
         INFO_message("Number of ROIs in netw[%d] = %d",bb,NROI[bb]); 

      ROI_STR_LABELS = (char ***) calloc( N_nets, sizeof(char **) );
      for ( i=0 ; i<N_nets ; i++ ) 
         ROI_STR_LABELS[i] = (char **) calloc( NROI[i]+1, sizeof(char *) );
      for ( i=0 ; i<N_nets ; i++ ) 
         for ( j=0 ; j<NROI[i]+1 ; j++ ) 
            ROI_STR_LABELS[i][j] = (char *) calloc( 100 , sizeof(char) );
      if(  (ROI_STR_LABELS == NULL)) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }

      // Sept 2014:  Labeltable stuff
      if ((ROI_set->Label_Dtable = DSET_Label_Dtable(ROI_set))) {
         if ((LabTabStr = Dtable_to_nimlstring( DSET_Label_Dtable(ROI_set),
                                                "VALUE_LABEL_DTABLE"))) {
            //fprintf(stdout,"%s", LabTabStr);
            if (!(roi_dtable = Dtable_from_nimlstring(LabTabStr))) {
               ERROR_exit("Could not parse labeltable.");
            }
         } 
         else {
            INFO_message("No label table from '-netrois'.");
         }
      }

      bb = Make_ROI_Output_Labels( ROI_STR_LABELS,
                                   ROI_LABELS, 
                                   N_nets,
                                   NROI,
                                   roi_dtable, 
                                   opts.DUMP_with_LABELS);
   }
   else {
      ERROR_message("Failed to load '-netrois' options.");
      exit(2);
   }
   
   // from '-uncert'
   if( opts.NAMEIN_uncert ) {
      insetUC = THD_open_dataset( opts.NAMEIN_uncert );
      if( insetUC == NULL ) 
         ERROR_exit("Can't open uncert dataset '%s'",
                    opts.NAMEIN_uncert) ;
      DSET_load(insetUC) ; CHECK_LOAD_ERROR(insetUC);
      nvox_unc = DSET_NVOX(insetUC);
      N_uncert = DSET_NVALS(insetUC); // needed if HARDI input now
   }

   if( opts.NAMEIN_uncFA ){
      unc_minfa_std = atof(opts.NAMEIN_uncFA);
      USER_UNC_FA = 1;
   }

   if( opts.NAMEIN_uncEI ){
      unc_minei_std = atof(opts.NAMEIN_uncEI);
      unc_minei_std*= CONV;
      USER_UNC_EI = 1;
   }

   // for naming stuff later
   if( opts.dti_inpref )
      hardi_pref_len = strlen(opts.dti_inpref); // assume an underscore
   else if( opts.hardi_pars )
      hardi_pref_len = strlen(opts.hardi_pars);
   
   if( opts.NAMEIN_mask ){
      MASK = THD_open_dataset(opts.NAMEIN_mask) ;
      if( (MASK == NULL ))
         ERROR_exit("Can't open time series dataset '%s'.",opts.NAMEIN_mask);
      
      DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
      HAVE_MASK=1; // kind of needless switch...
   }

   if( opts.in_EXTRA ){
      EXTRAFILE = 1; // switch on
      PARS_BOT = 0;
   }

   if( opts.NAMEIN_outmode ) {
      if (strcmp(opts.NAMEIN_outmode, "NI_fast_binary") &&
          strcmp(opts.NAMEIN_outmode, "NI_fast_text") &&
          strcmp(opts.NAMEIN_outmode, "NI_slow_binary") &&
          strcmp(opts.NAMEIN_outmode, "NI_slow_text") ) {
         ERROR_message("Bad value (%s) for -tract_out_mode",
                       opts.NAMEIN_outmode);
         exit(1);
      }  
      mode = opts.NAMEIN_outmode;//argv[iarg];
   }

   // * * * * * * * * * * * * * * check HARDI vs DTI * * * * * * * * * * * *
   
   if( opts.algopt_file_name ) {

      if( (TR_MODE == 0) || (TR_MODE == 1) ) { // DET or MINIP
         if (!(nel = ReadTractAlgOpts_M(opts.algopt_file_name))) {
				ERROR_message("Failed to read options in %s\n",
                          opts.algopt_file_name);
				exit(19);
			}
         
			if (NI_getTractAlgOpts_M(nel, &(opts.MinFA),&(opts.MaxAngDeg),
                                  &(opts.MinL),
                                  opts.SeedPerV)) {
				ERROR_message("Failed to get options");
				exit(1);
			}
			NI_free_element(nel); nel=NULL;

         if( (opts.MinFA<0.001) && 
             (opts.MaxAngDeg<0.001) && 
             (opts.MinL<0.001) ) 
            ERROR_exit("All of alg_MinFA, alg_MaxAngDeg and alg_MinL"
                       " are <0.001.  This looks suspicious.  Is your\n"
                       "'-algopt' file %s written in the niml.opts"
                       " format (i.e., with nice labels per parameter)\n"
                       "but it doesn't end with .niml.opts, so it's being"
                       " treated like the old school, plain-numbers-in-a-\n"
                       "column format, and thus causing an error here?",
                       opts.algopt_file_name);

         if( (opts.SeedPerV[0] < 1) || 
             (opts.SeedPerV[1] < 1) || 
             (opts.SeedPerV[2] < 1) ) {
            WARNING_message("One of the dimensions of seeds was <1\n"
                            "Perhaps you input a probabilistic algopt file?\n"
                            "Since you are chose the deterministic switch, I\n"
                            "am going to use 1 seed per vox.");
            opts.SeedPerV[0] = opts.SeedPerV[1] = opts.SeedPerV[2] = 1;
         }

      }
      else { // PROB
			if (!(nel = ReadProbTractAlgOpts_M(opts.algopt_file_name))) {
				ERROR_message("Failed to read options in %s\n",
                          opts.algopt_file_name);
				exit(19);
			}

			if (NI_getProbTractAlgOpts_M(nel, &(opts.MinFA),&(opts.MaxAngDeg),
                                      &(opts.MinL),
                                      &(opts.NmNsFr),&(opts.Nseed),
                                      &(opts.Nmonte))) {
				ERROR_message("Failed to get options");
				exit(1);
			}
			NI_free_element(nel); nel=NULL;
      }
   }

   if (DEF_DTI && !(opts.dti_inpref || opts.dti_listname) ){
      ERROR_exit("Looks like DTI, but -dti_in was NULL.\n") ;
		exit(5);
   }

   if ( opts.dti_inpref && opts.dti_listname ) {
      ERROR_exit("Pick using either '-dti_listname' *or* '-dti_in'.\n") ;
		exit(5);
   }

   if ( !DEF_DTI && ( !opts.hardi_dir || !opts.hardi_gfa ) ) {
      ERROR_exit("Looks like HARDI, but one of -hardi_* was NULL.\n") ;
		exit(5);
   }

   if ( (opts.dti_inpref  || opts.dti_listname) && 
        ( opts.hardi_dir || opts.hardi_pars || opts.hardi_gfa )) {
      ERROR_exit("Looks like a combo of -dti_* and -hardi_* was used.\n") ;
		exit(5);
   }

   if ( EXTRAFILE && !DEF_DTI ) {
      ERROR_exit("ERROR: no extra file allowed with HARDI.\t"
                 "Did you mean to use '-hardi_pars'?\n") ;
		exit(6);
   }

   if( TR_MODE<0) // minip
      ERROR_exit("!what, no mode? Need to select from: '-mode {%s | %s | %s }'",
                 list_modes[0],list_modes[1],list_modes[2]);

   if( TR_MODE==1) // minip
      if( opts.MINI_PROB_NM<1 )
         ERROR_exit("in '-mode %s', one needs '-mini_num D' with D>0.\n",
                    list_modes[TR_MODE]);
      else
         INFO_message("Running with %d mini-prob iterations",
                      opts.MINI_PROB_NM);

   if( (TR_MODE==1) || (TR_MODE==2) ) // minip
      if(!insetUC)
         ERROR_exit("ERROR: in '-mode %s', one needs '-uncert FILE' as well.\n",
                    list_modes[TR_MODE]);
   
   if( (TR_MODE==0) && insetUC )
      ERROR_exit("ERROR: in '-mode %s', one doesn't use '-uncert FILE'.\n"
                 "\t--> Do you mean to be using '-mode %s' or '-mode %s', "
                 "which require uncertainty?\n",
                 list_modes[TR_MODE],list_modes[1],list_modes[2]);

   if( (TR_MODE==2) && opts.MINI_PROB_NM )
      ERROR_exit("ERROR: in '-mode %s', don't assign '-mini_num' values.\n",
                 list_modes[TR_MODE]);

   if( (TR_MODE==2) && (opts.LOG_TYPE>=0) )
      ERROR_exit("ERROR: in '-mode %s', one doesn't assign '-logic' values.\n",
                 list_modes[TR_MODE]);

   // ** * ** * ** * ** * figure out size of vec and scal files * ** * ** *

   // for both DTI/HARDI
   wild_names = (char **)calloc(N_DTI_MAX_PARS, sizeof(char *)); 
   for (j=0; j<N_DTI_MAX_PARS; ++j) 
      wild_names[j] = (char *)calloc(32, sizeof(char));
   if( (wild_names == NULL) ){
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(123);
   }
   
   if(DEF_DTI) { // much reworked and cleaned up (believe it or not), Oct2016

      insetVECS = (THD_3dim_dataset **)calloc(N_DTI_VECT,
                                              sizeof(THD_3dim_dataset *));
      // allocate up to max, and free extras after
      insetPARS = (THD_3dim_dataset **)calloc(N_DTI_MAX_PARS,
                                              sizeof(THD_3dim_dataset *));

      if( (insetPARS == NULL) || (insetVECS == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
      
      // START of listfile set of inputs:
      if( opts.dti_listname ) {
         
         // get both scalars (all) and vecs
         list_for_DTI( opts.dti_listname,
                       insetPARS,
                       insetVECS,
                       &EXTRAFILE, &PARS_TOP,
                       wild_names,
                       1); 
         if ( EXTRAFILE )
            PARS_BOT = 0;
         
      } // done with list-only stuff
      else { // GLOB for file names, because we don't know
             // endings/types
         
         // vecs
         i = glob_for_DTI_vec( opts.dti_inpref,
                               insetVECS,
                               hardi_pref_len);
         // scals
         i = glob_for_DTI_trac( opts.dti_inpref,
                                insetPARS,
                                wild_names,
                                hardi_pref_len,
                                &PARS_TOP,
                                opts.NO_NONDTI_SEARCH );

         if( opts.in_EXTRA ) {
            EXTRAFILE = 1;
            PARS_BOT = 0;

            insetPARS[0] = THD_open_dataset(opts.in_EXTRA);
            if( (insetPARS[0] == NULL ) )
               ERROR_exit("Can't open 'extra' listed dataset '%s': ",
                          opts.in_EXTRA);

            DSET_load(insetPARS[0]); 
            CHECK_LOAD_ERROR(insetPARS[0]);
            fprintf(stderr,"\tFound 'extra' file '%s' to be labeled '%s'\n",
                    opts.in_EXTRA, DTI_XTRA_LABS[0]);
            snprintf(wild_names[0],31,"%s", DTI_XTRA_LABS[0]); 
         }

      } // end of DTI glob-only stuff

      // clean up unused
      for( i=PARS_TOP ; i<N_DTI_MAX_PARS ; i++ ) 
         if( insetPARS[i] ) {
            DSET_delete(insetPARS[i]);
            insetPARS[i] = NULL;
            free(insetPARS[i]);
         }
         
      PARS_N = PARS_TOP - 1 + EXTRAFILE;
      // number of output matrices in GRID file: first two parts
      // are simple counts of things, and the third part is '2x'
      // because of having both mean and stdev for those scalars
      Noutmat = N_DEF_PAR_LABS + opts.EXTRA_TR_PAR + 
         2*(PARS_N); 
      // ParLab has length Noutmat
      // param_grid has 2*PARS_N+1 = Noutmat-2 blocks???????? pt,Oct,2016
      
      if( EXTRAFILE ) {
         if( !USER_UNC_FA )
            unc_minfa_std*= THD_subbrick_max( insetPARS[0] , 0, 1);
      }

      INFO_message("Done with scalar search, found: %d parameters\n"
                   "\t--> so will have %d output data matrices.",
                   PARS_N, Noutmat);

      
   } // end of DTI
   else { // HARDI section: not updated in the big Nov,2016 stuff.  Perhaps later.
      
      wsort = 0;
      nsort = 0;

      // GLOB for file names, because we don't know endings/types, nor
      // number for HARDI
      if(opts.hardi_pars) {
         sprintf(tprefix,"%s*",opts.hardi_pars);

         // this island of coding, globbing and sorting due to ZSS;
         // see apsearch.c program for original.
         wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
         fprintf(stderr,"\n++ SEARCHING for HARDI parts with: %s",wild_list);
         fprintf(stdout,"\n++ SCALAR FINDINGS:\n\t");
         MCW_wildcards(wild_list, &nglob, &wglob ); 
         if(nglob)
            if( !(wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                                     &nsort, &isrt)) ) {
               ERROR_message("Failed to sort -hard_pars");
               SUMA_ifree(wild_list);
               MCW_free_wildcards( nglob , wglob ) ;
               exit(1);
            }
      }
      else
         INFO_message("Not searching for additional files,"
                      " no '-hardi_pars' given.");
      
      // we know we have GFA, so enter that
      PARS_N = PARS_TOP = nsort + 1; // GFA + number of files         
      insetPARS = (THD_3dim_dataset **)calloc(PARS_N,
                                              sizeof(THD_3dim_dataset *));
      if(  (insetPARS == NULL)  ){
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }

      insetPARS[0] = THD_open_dataset( opts.hardi_gfa );
      if( insetPARS[0] == NULL ) 
         ERROR_exit("Can't open dataset '%s'", opts.hardi_gfa);
      DSET_load(insetPARS[0]); 
      CHECK_LOAD_ERROR(insetPARS[0]);
      snprintf(wild_names[0],31,"GFA");
      fprintf(stdout,"\t '%s' ",wild_names[0]);

      // set uncert of this param to be ~2% of max, unless user
      // expresses value
      if( !USER_UNC_FA )
         unc_minfa_std*= THD_subbrick_max( insetPARS[0] , 0, 1);

      if (opts.hardi_pars) {

         jj = 1;
         for( ii=1 ; ii<PARS_TOP ; ii++ ) {
            
            dsetn = THD_open_dataset(wglob[isrt[ii-1]]);
            if( DSET_NVALS(dsetn) > 1 ) {// check if it's a vec or scal
               DSET_delete(dsetn);
               dsetn=NULL;
               PARS_N--;
            }
            else {
               insetPARS[jj] = dsetn;
               
               // in case of underscore between pref and var name
               pref_offset = 0;
               if( *(wsort[ii-1]+hardi_pref_len) == '_')
                  pref_offset = 1;
               
               snprintf(wild_names[jj],31,"%s", 
                        wsort[ii-1]+hardi_pref_len+pref_offset);
               //INFO_message("FOUND another scalar '%s'  (from '%s')",
               // wild_names[jj],wsort[ii-1]);
               fprintf(stdout," '%s' ",wild_names[jj]);
               dsetn=NULL;
               if( insetPARS[jj] == NULL ) 
                  ERROR_exit("Can't open dataset '%s'", wglob[isrt[ii-1]]);
               DSET_load(insetPARS[jj]); 
               CHECK_LOAD_ERROR(insetPARS[jj]) ;
               jj++;
            }
            if( jj == N_DTI_MAX_PARS ) {
               INFO_message("Have reached max number of allowed"
                            " input scalars (%d).\n"
                            "\tWill just go with the ones gotten now.\n",
                            N_DTI_MAX_PARS) ;
               PARS_N = N_DTI_MAX_PARS; // pars_top reset below
               break;
            }
         }

         // free any unfilled ones, reduce stored count
         for( i=jj ; i<PARS_TOP ; i++ ) {
            //               free(insetPARS[i]); 
            insetPARS[i]=NULL;
         }

         PARS_TOP = PARS_N;
         
         if (isrt) free(isrt); isrt = NULL;
         for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
         free(wsort); wsort = NULL;
         SUMA_ifree(wild_list);
         MCW_free_wildcards( nglob , wglob ) ;

      }

      fprintf(stdout,"\n");
      
      // number of output matrs in GRID
      Noutmat = N_DEF_PAR_LABS + opts.EXTRA_TR_PAR + 2*PARS_N; 
      // ParLab has length Noutmat
      // param_grid has 2*PARS_N+1 = Noutmat-2 blocks
      INFO_message("Done with scalar search, found: %d parameters\n"
                   "\t--> so will have %d output data matrices.",
                   PARS_N, Noutmat);
            

      //sprintf(headerHAR.scal_n[0],"%s", wild_names[0]);
            
      // need to find out max number of vectors per voxel
      // single file
      dsetn = THD_open_dataset(opts.hardi_dir);   
      if( dsetn == NULL ) 
         ERROR_exit("Can't open dataset '%s'", opts.hardi_dir);
      DSET_load(dsetn) ; CHECK_LOAD_ERROR(dsetn) ;
      
      N_HAR = DSET_NVALS(dsetn);
      if( N_HAR % 3 != 0 )
         ERROR_exit("Number of bricks in '-hardi_dir' set must"
                    " be a multiple of 3,"
                    " (which %d isn't).", N_HAR );
      else
         N_HAR/= 3;
     
      insetVECS = (THD_3dim_dataset **)calloc(N_HAR,
                                              sizeof(THD_3dim_dataset *) );
      if( (insetVECS == NULL) ){
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
      
      for( i=0 ; i<N_HAR ; i++ ) {
         insetVECS[i] = EDIT_empty_copy(insetPARS[PARS_BOT] ) ; // single brick
         EDIT_add_bricklist(insetVECS[i],                // make it 3D vector.
                            2, NULL , NULL , NULL );
         EDIT_dset_items(insetVECS[i],              // should be float anyways
                         ADN_datum_all , MRI_float , 
                         ADN_none ) ;
         
         for( j=0 ; j<3 ; j++) {
            temp_vec =  THD_extract_float_brick(3*i+j, dsetn) ; 
            if( MRI_FLOAT_PTR(temp_vec) == NULL) {
               fprintf(stderr, "\n\n Copy failure for temp_vec.\n\n");
               exit(124);
            }
            EDIT_substitute_brick(insetVECS[i], j, 
                                  MRI_float, 
                                  MRI_FLOAT_PTR(temp_vec) );
            mri_clear_and_free(temp_vec);
         }
      }
      INFO_message("FOUND vector '%s', with %d subbricks\n"
                   "\t--> at most %d direction(s) per voxel.",
                   opts.hardi_dir,3*N_HAR,N_HAR);

      DSET_delete(dsetn);
      free(dsetn);
   } // end of HARDI-only section

   // ---------------- labels for all (DTI or HARDI) -------------------------

   ParLab = (char **)calloc(Noutmat, sizeof(char *)); 
   for (j=0; j<Noutmat; ++j) 
      ParLab[j] = (char *)calloc(32, sizeof(char));
   if( (ParLab == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(121);
   }
         
   // start loading up param labels: auto-calc'ed ones first
   for ( j=0; j<N_DEF_PAR_LABS; j++ ) 
      ParLab[j] = strdup(DEF_PAR_LABS[j]);
   if ( opts.EXTRA_TR_PAR ) 
      for ( j=0; j<N_XTR_PAR_LABS; j++ ) 
         ParLab[N_DEF_PAR_LABS + j] = strdup(XTR_PAR_LABS[j]);
      
   for( i=PARS_BOT ; i<PARS_TOP ; i++ ){
      // 'i-PARS_BOT' goes from 0 to number-of-data-scalars-minus-1; 
      // j: taking into account offsets and spacing for mean '%s'
      // and stdev 's%s'
      j = N_DEF_PAR_LABS + opts.EXTRA_TR_PAR + 2*(i-PARS_BOT);
      // mean
      snprintf(ParLab[j], 31, "%s", wild_names[i]);
      // stdev
      snprintf(ParLab[j+1], 31, "s%s", wild_names[i]);
   }

   // ----------------- get basic info, and TV header stuff ----------------

   if ( insetPARS[PARS_BOT] ) { // should have PARS_BOT = 0 for HARDI.
      Nvox = DSET_NVOX(insetPARS[PARS_BOT]) ;
         
      Dim = (int *)calloc(3, sizeof(int)); 
      Dim[0] = DSET_NX(insetPARS[PARS_BOT]); 
      Dim[1] = DSET_NY(insetPARS[PARS_BOT]); 
      Dim[2] = DSET_NZ(insetPARS[PARS_BOT]); 
      Ledge[0] = fabs(DSET_DX(insetPARS[PARS_BOT])); 
      Ledge[1] = fabs(DSET_DY(insetPARS[PARS_BOT])); 
      Ledge[2] = fabs(DSET_DZ(insetPARS[PARS_BOT])); 
      Orig[0] = DSET_XORG(insetPARS[PARS_BOT]); 
      Orig[1] = DSET_YORG(insetPARS[PARS_BOT]); 
      Orig[2] = DSET_ZORG(insetPARS[PARS_BOT]);
      VoxVol = DSET_VOXVOL(insetPARS[PARS_BOT]);

      // this stores the original data file orientation for later use,
      // as well since we convert everything to RAI temporarily, as
      // described below
      voxel_order = (char *)calloc(4, sizeof(char)); 
      TV_switch = (int *)calloc(3, sizeof(int)); 
      voxel_order[0]=ORIENT_typestr[insetPARS[PARS_BOT]->daxes->xxorient][0];
      voxel_order[1]=ORIENT_typestr[insetPARS[PARS_BOT]->daxes->yyorient][0];
      voxel_order[2]=ORIENT_typestr[insetPARS[PARS_BOT]->daxes->zzorient][0];
      voxel_order[3]='\0';
      
      if( DEF_DTI ){
         headerDTI.voxel_order[0] =
            ORIENT_typestr[insetPARS[PARS_BOT]->daxes->xxorient][0];
         headerDTI.voxel_order[1] =
            ORIENT_typestr[insetPARS[PARS_BOT]->daxes->yyorient][0];
         headerDTI.voxel_order[2] =
            ORIENT_typestr[insetPARS[PARS_BOT]->daxes->zzorient][0];
            
         for( i=0 ; i<3 ; i++) {
            headerDTI.dim[i] = Dim[i];
            headerDTI.voxel_size[i] = Ledge[i];
            // will want this when outputting file later for TrackVis.
            TV_switch[i] = !(dset_or[i]==voxel_order[i]);
         }  
      }
      else{ // HARDI
         sprintf(headerHAR.scal_n[0],"%s", wild_names[0]);

         headerHAR.voxel_order[0] =
            ORIENT_typestr[insetPARS[0]->daxes->xxorient][0]; 
         headerHAR.voxel_order[1] =
            ORIENT_typestr[insetPARS[0]->daxes->yyorient][0];
         headerHAR.voxel_order[2] =
            ORIENT_typestr[insetPARS[0]->daxes->zzorient][0];
            
         for( i=0 ; i<3 ; i++) {
            headerHAR.dim[i] = Dim[i];
            headerHAR.voxel_size[i] = Ledge[i];
            // will want this when outputting file later for TrackVis.
            TV_switch[i] = !(dset_or[i]==voxel_order[i]);
         }  
            
            
      }
   }

   // ---------------------------

   // check tot num vox match (as proxy for dims...)
   if( (Nvox != nvox_rois) || ( (Nvox != nvox_unc) && !DETNET) ) {
      if(nvox_unc ==0)
         INFO_message("Are you doing something probabilistic, and\n"
                      " did you forget to use the '-uncert' option?\n"
                      "If not, use '-logic {AND | OR}' for"
                      " deterministic tracking.");
      ERROR_exit("Input datasets do not match in number of voxels!\n"
                 "\t'-netrois' volume has %d vox, \n"
                 "\t'-uncert' volume has %d vox, \n"
                 "\t'-inset' volume has %d vox, \n",nvox_rois, nvox_unc, Nvox);
   }
      
   // Nov,2016
   if( ( ( voxel_order[0] != 'R' ) && ( voxel_order[0] != 'L' ) ) ||
       ( ( voxel_order[1] != 'A' ) && ( voxel_order[1] != 'P' ) ) ||
       ( ( voxel_order[2] != 'S' ) && ( voxel_order[2] != 'I' ) ) ) 
      ERROR_exit("Data must have oriention with:\n"
                 "\tindex [0] either 'R' or 'L',\n"
                 "\tindex [1] either 'P' or 'A', and\n"
                 "\tindex [2] either 'I' or 'S'.\n"
                 "\tLife is hard.");

   // all input files must have the same orientation
   for( j=(PARS_BOT+1); j<PARS_TOP; ++j ) 
      i = CompareSetOrients(insetPARS[PARS_BOT], insetPARS[j]);
   if( DEF_DTI )
      for( j=0; j<3; ++j ) 
         i = CompareSetOrients(insetPARS[PARS_BOT], insetVECS[j]);
   else
      for( j=0; j<N_HAR; ++j ) 
         i = CompareSetOrients(insetPARS[PARS_BOT], insetVECS[j]);
   
   if(EXTRAFILE)
      if( !((Dim[0] == DSET_NX(insetPARS[0])) && 
            (Dim[1] == DSET_NY(insetPARS[0])) &&
            (Dim[2] == DSET_NZ(insetPARS[0]))))
         ERROR_exit("Dimensions of extra set '%s' don't match those of"
                    " the DTI prop (prefix '%s').",
                    opts.in_EXTRA, opts.dti_inpref);

   if( ( N_HAR>0 ) && ( N_uncert>0 ) && (N_HAR+1 != N_uncert ) )
      ERROR_exit("Number of `-hardi_dir's is %d, while the number of\n" 
                 " uncertainty values for the directions appears to be %d.\n"
                 " These must match.", N_HAR, N_uncert);
                 

   if(nvox_unc && DETNET && ( opts.MINI_PROB_NM == 0 ) ){ // @)
      ERROR_message("Don't -detnet AND -uncert together without -mini_num.\n");
      exit(1);
   }

   //  if(!nvox_unc && ( MINI_PROB_NM ) ){ // @)
   // ERROR_message("If using -mini_num, then you need -uncert as well.\n");
   // exit(1);
   //}

   if( DETNET && (opts.LOG_TYPE==-1) ) {
      ERROR_message("You forgot to provide a type of logic: '-logic {AND|OR}.\n");
      exit(1);
   }

   if(DETNET && (opts.LOG_TYPE==0) && opts.ONLY_BT){
      INFO_message("With '-logic OR', the '-cut_at_rois' option will"
                   " be automically turned off (-> have '-uncut_at_rois').");
      opts.ONLY_BT=0;
   }

   if(opts.ROIS_OUT) {
      for( k=0 ; k<N_nets ; k++ ) { // each netw gets own file
         sprintf(OUT_rois,"%s_%03d.roi.labs",opts.prefix,k);
         if( (fout1 = fopen(OUT_rois, "w")) == NULL) {
            fprintf(stderr, "Error opening file %s.",OUT_rois);
            exit(19);
         }
         for( i=1 ; i<=NROI[k] ; i++ ) {
            fprintf(fout1,"%d\t\t%d\t\t%d\n",ROI_LABELS[k][i],i,
                    (int) pow(2,i));
         }
         fclose(fout1);
      }
   }

   flat_matr = (float ***) calloc( N_nets, sizeof(float **) );
   for ( i = 0 ; i < N_nets ; i++ ) 
      flat_matr[i] = (float **) calloc( Noutmat, sizeof(float *) );
   for ( i = 0 ; i < N_nets ; i++ ) 
      for ( j = 0 ; j < Noutmat ; j++ ) 
         flat_matr[i][j] = (float *) calloc( NROI[i]*NROI[i], sizeof(float));
      
   gdset_roi_names = (char ***)calloc(N_nets, sizeof(char **));
   for (i=0; i< N_nets ; i++ ) {
      gdset_roi_names[i] = (char **)calloc(NROI[i], sizeof(char *));
      for (j=0; j<NROI[i]; ++j) {
         gdset_roi_names[i][j] = (char *)calloc(32, sizeof(char));
         //snprintf(gdset_roi_names[i][j],31,"N%03d:R%d", i, ROI_LABELS[i][j]);
         //if( OLD_LABEL )
         //  snprintf(gdset_roi_names[i][j],31,"N%03d:R%d", i, 
         //          ROI_LABELS[i][j]);
         //else{
         snprintf(gdset_roi_names[i][j],31,"%s",
                  ROI_STR_LABELS[i][j+1]);
         //fprintf(stderr," %s ",
         //       ROI_STR_LABELS[i][j+1]);
         //}
      }
   }
   
   if(  (flat_matr == NULL) || ( gdset_roi_names == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(14);
   }


   // convert to cos of rad value for comparisons, instead of using acos()
   MaxAng = cos(CONV*opts.MaxAngDeg); 
   
   // for temp storage array, just a multiple of longest dimension!
   // essentially, a buffer size per tract we're making
   if(Dim[0] > Dim[1])
      ArrMax = Dim[0] * 4;
   else
      ArrMax = Dim[1] * 4;
   if(4*Dim[2] > ArrMax)
      ArrMax = Dim[2] * 4;
  
   // switch to add header-- option for now, added Sept. 2012
   // for use with map_TrackID to map tracks to different space
   if(opts.RECORD_ORIG) { // @)
      for( i=0 ; i<3 ; i++) {
         if(N_HAR)
            headerHAR.origin[i] = Orig[i];
         else
            headerDTI.origin[i] = Orig[i];
      }
   }

   // @) IF DOING JUST DETNET TRACK
   // adjust what some params mean
   if (DETNET){ 

      if( N_nets > FOPEN_MAX)
         ERROR_message("You have more networks (%d) than the allowable number\n"
                       "of open stream (%d) allowed for your computer.\n"
                       " You should divide up the networks below this max,\n"
                       "or spring for a better computer.\n",N_nets,FOPEN_MAX);

      if(opts.MINI_PROB_NM)
         opts.Nmonte = opts.MINI_PROB_NM; // only doing 1 it
      else
         opts.Nmonte = 1;

      opts.Nseed = opts.SeedPerV[0]*opts.SeedPerV[1]*opts.SeedPerV[2];
     
      LocSeed = calloc(opts.Nseed,sizeof(LocSeed)); 
      for(i=0 ; i<opts.Nseed ; i++) 
         LocSeed[i] = calloc(3,sizeof(float)); 
     
      if( (LocSeed == NULL) ){
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
     
      // regularly spaced seeds
      i = 0; 
      for( ii=0 ; ii<opts.SeedPerV[0] ; ii++ ) 
         for( jj=0 ; jj<opts.SeedPerV[1] ; jj++ ) 
            for( kk=0 ; kk<opts.SeedPerV[2] ; kk++ ) {
               LocSeed[i][0] = (0.5 + (float) ii)/opts.SeedPerV[0];
               LocSeed[i][1] = (0.5 + (float) jj)/opts.SeedPerV[1];
               LocSeed[i][2] = (0.5 + (float) kk)/opts.SeedPerV[2];
               i++;
            }
     
      NmNsThr = 1; // thresh of 1 for stats stuff

      N_bund = (int *)calloc(N_nets, sizeof(int)); 
      if( N_bund == NULL) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
      if(opts.LOG_TYPE) //just for AND logic
         for( i=0 ; i<N_nets ; i++)
            N_bund[i] = FlatUHT_Len(NROI[i]);
      else
         for( i=0 ; i<N_nets ; i++)
            N_bund[i] = FlatUHT_Len(NROI[i]);
     
      tb = (TAYLOR_BUNDLE ***) calloc( N_nets, sizeof(TAYLOR_BUNDLE **) );
      for ( i = 0 ; i < N_nets ; i++ ) // halftri+diag notation!!
         tb[i] = (TAYLOR_BUNDLE **) calloc( N_bund[i], sizeof(TAYLOR_BUNDLE *));
      tt = (TAYLOR_TRACT **)calloc(N_nets, sizeof(TAYLOR_TRACT *)); 
      tnet = (TAYLOR_NETWORK **)calloc(N_nets, sizeof(TAYLOR_NETWORK *));
      id = (int *)calloc(N_nets, sizeof(int)); 
     

      prefix_det = calloc( N_nets,sizeof(prefix_det));  
      for(i=0 ; i<N_nets ; i++) 
         prefix_det[i] = calloc( 300,sizeof(char)); 

      if( opts.OUTPUT_TRK)
         fout0 = (FILE **)calloc(N_nets, sizeof(FILE)); 

      if(  (prefix_det == NULL) 
           || (tb == NULL) || (tt == NULL) || (id == NULL) 
           || (tnet == NULL)
           || ( opts.OUTPUT_TRK && (fout0 == NULL)) ) { 
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
   }
   else {

      // Process the options a little 
      LocSeed = calloc(opts.Nseed,sizeof(LocSeed)); 
      for(i=0 ; i<opts.Nseed ; i++) 
         LocSeed[i] = calloc(3,sizeof(float)); 
     
      // initial value in this case 
      for( k=0 ; k<opts.Nseed ; k++ ) 
         for( j=0 ; j<3 ; j++ ) 
            LocSeed[k][j] = rand()*1.0/RAND_MAX; 
     
      // will take stats on voxels with number of tracts >=NmNsThr
      NmNsThr =  (int) floor(opts.NmNsFr*opts.Nseed*opts.Nmonte); 
      // lower bound is 1, and also force to be 1 if posteriori is chosen
      if( (NmNsThr<1) || opts.POST ) 
         NmNsThr=1;
   }

   if (opts.dump_opts) {
      if (DETNET)
         nel = NI_setTractAlgOpts_M(NULL, &(opts.MinFA), &(opts.MaxAngDeg),
                                    &(opts.MinL), 
                                    (opts.SeedPerV));
      else
         nel = NI_setProbTractAlgOpts_M(NULL, &(opts.MinFA), &(opts.MaxAngDeg),
                                        &(opts.MinL), 
                                        &(opts.NmNsFr),&(opts.Nseed),
                                        &(opts.Nmonte) );

      WriteTractAlgOpts(opts.prefix, nel);
      NI_free_element(nel); nel=NULL;
   }


   if(opts.POST){
      if(opts.DUMP_TYPE==1) {
         INFO_message("You asked for '-dump_rois DUMP', but also chose\n"
                      "\t'-posteriori', so you will get binary mask DUMP,\n"
                      "\tas well as AFNI files of numbers of tracks/voxel.");
         opts.DUMP_TYPE=3;
      }
      if(opts.DUMP_TYPE== -1){
         INFO_message("You did NOT ask for individual dump of ROIs by using\n"
                      "\t'-dump_rois {option}' but then you DID choose\n"
                      "\t'-posteriori', so you will get a set of AFNI files\n"
                      "having numbers of tracks/voxel.");
         opts.DUMP_TYPE=2;
      }
   }

   // have all be RAI for processing here
   if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
      
      for( i=PARS_BOT ; i<PARS_TOP ; i++){
         //fprintf(stderr,"\nswitching par: %d",i);
         dsetn = r_new_resam_dset( insetPARS[i], NULL, 0.0, 0.0, 0.0,
                                   dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(insetPARS[i]); 
         insetPARS[i]=dsetn;
         dsetn=NULL;
      }

      if( DEF_DTI )
         ii = 3;
      else
         ii = N_HAR;
      
      for( i=0 ; i<ii ; i++) { 
         dsetn = r_new_resam_dset(insetVECS[i], NULL, 0.0, 0.0, 0.0,
                                  dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(insetVECS[i]); 
         insetVECS[i]=dsetn;
         dsetn=NULL;
      }
      
      dsetn = r_new_resam_dset( ROI_set, NULL, 0.0, 0.0, 0.0,
                                dset_or, RESAM_NN_TYPE, NULL, 1, 0);
      DSET_delete(ROI_set); 
      ROI_set=dsetn;
      dsetn=NULL;
      
      if( N_uncert>0 ){
         dsetn = r_new_resam_dset( insetUC, NULL, 0.0, 0.0, 0.0,
                                   dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(insetUC); 
         insetUC=dsetn;
         dsetn=NULL;
      }

      if(HAVE_MASK) {
         dsetn = r_new_resam_dset(MASK, NULL, 0.0, 0.0, 0.0,
                                  dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(MASK); 
         MASK=dsetn;
         dsetn=NULL;
      }
   }
  
   // ****************************************************************
   // ****************************************************************
   //                    make arrays for tracking
   // ****************************************************************
   // ****************************************************************
  
   // will hold indices of all voxels with actual data, i.e., takes in
   // (i,j,k) coor and gives index, for efficiency of storage because
   // lots of zeros in (Dimx, Dimy,Dimz grid).
   // INDEX:  for afni THD_* things
   // INDEX2: for coor*, NETROI
   INDEX = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      INDEX[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         INDEX[i][j] = (int *) calloc( Dim[2] , sizeof(int) );
   INDEX2 = (int ***) calloc( Dim[0] , sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      INDEX2[i] = (int **) calloc( Dim[1] , sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         INDEX2[i][j] = (int *) calloc( Dim[2], sizeof(int) );
  
   mskd = (int ***) calloc( Dim[0], sizeof(int **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );
	
   if( (INDEX == NULL) || (INDEX2 == NULL) || (mskd == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(121);
   }

   Ndata = 0;
   idx = 0;
   // determine how many voxels of actual data there are
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            INDEX[i][j][k] = idx;
            if( ( (HAVE_MASK==0) && 
                  (THD_get_voxel(insetPARS[PARS_BOT],idx,0)>EPS_V) ) ||
                ( HAVE_MASK && (THD_get_voxel(MASK,idx,0)>0) ) )
               {
                  mskd[i][j][k]=1;
                  Ndata+=1;
                  INDEX2[i][j][k] = Ndata;
               }
            else
               INDEX2[i][j][k] = 0; 
            idx+=1;
         }
   // now, Ndata is number of voxels of actual data, will be size of grids.
	
   if( Ndata<1) {
      fprintf(stderr, "\n\n Too few data voxels-- wrong scale of things?\n\n");
      exit(1221);
   }
   //INFO_message("Ndata: %d.  Nvox: %d",Ndata,Nvox);

   DirPerVox = ( short *)calloc((Ndata+1), sizeof(short)); 
   if( (DirPerVox == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(123);
   }

   if(N_HAR){ // so switch from initialized defaults

      j = Setup_Ndir_per_vox( N_HAR, Dim, mskd, INDEX, INDEX2,
                              insetVECS, DirPerVox);
      
      if(N_uncert) {
         UNC = calloc( (Ndata+1),sizeof(UNC)); 
         for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
            UNC[i] = calloc(DirPerVox[i]+1, sizeof(float)); 
      }
   }
   else{
      j = Setup_Ndir_per_vox( 1, Dim, mskd, INDEX, INDEX2,
                              insetVECS, DirPerVox);

      if(N_uncert) {
         // store final delta e_{12}, delta e_{12}, bias_FA and std_FA
         UNC = calloc( (Ndata+1),sizeof(UNC)); 
         for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
            UNC[i] = calloc(4, sizeof(float));  
      }
   }

   if( (N_uncert>0) && (UNC == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(122);
   }

   if( !DETNET )
      INFO_message("Effective Monte iterations: %d."
                   " Fraction threshold set: %.5f\n"
                   "\t--> Ntrack voxel threshold: %d.",
                   opts.Nseed*opts.Nmonte,opts.NmNsFr,NmNsThr);

   // 3 comp of V1 and FA for each data voxel
   // ragged for HARDI to save mem
   coorded = calloc( (Ndata+1),sizeof(coorded)); // to have all ind be >=1
   for(i=0 ; i<=Ndata ; i++) 
      coorded[i] = calloc(3*DirPerVox[i]+1, sizeof(float)); 
   // copy for perturb
   copy_coorded = calloc( (Ndata+1),sizeof(copy_coorded)); 
   for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
      copy_coorded[i] = calloc(3*DirPerVox[i]+1, sizeof(float)); 

   MAPROI = calloc( (Ndata+1),sizeof(MAPROI)); // to have all ind be >=1
   for(i=0 ; i<=Ndata ; i++) 
      MAPROI[i] = calloc(N_nets,sizeof(int)); 

   NETROI = (int ***) calloc( (Ndata+1), sizeof(int **) );
   for ( i = 0 ; i<=Ndata ; i++ ) 
      NETROI[i] = (int **) calloc( N_nets, sizeof(int *) );
   for ( i=0 ; i<=Ndata ; i++ ) 
      for ( j=0 ; j<N_nets ; j++ ) // jth net has NROI[j] rois
         NETROI[i][j] = (int *) calloc( FlatUHT_Len(NROI[j]), sizeof(int) );
   
   
   if( (coorded == NULL) || (copy_coorded == NULL) 
       || (NETROI == NULL) || (MAPROI == NULL)) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(122);
   }

   Prob_grid = (int **) calloc( N_nets, sizeof(int *));
   for ( i = 0 ; i < N_nets ; i++ ) 
      Prob_grid[i] = (int *) calloc( FlatUHT_Len(NROI[i]), sizeof(int));
   
   Prob_grid_L = (float ***) calloc( N_nets, sizeof(float **));
   for ( i = 0 ; i < N_nets ; i++ ) 
      Prob_grid_L[i] = (float **) calloc( FlatUHT_Len(NROI[i]), sizeof(float *));
   for ( i = 0 ; i < N_nets ; i++ ) 
      for ( j = 0 ; j < FlatUHT_Len(NROI[i]) ; j++ ) 
         Prob_grid_L[i][j] = (float *) calloc( 2, sizeof(float));

   Param_grid = (float ***) calloc( N_nets, sizeof(float **));
   for ( i = 0 ; i < N_nets ; i++ ) 
      Param_grid[i] = (float **) calloc( FlatUHT_Len(NROI[i]), sizeof(float *));
   for ( i = 0 ; i < N_nets ; i++ ) 
      for ( j = 0 ; j < FlatUHT_Len(NROI[i]) ; j++ ) 
         Param_grid[i][j] = (float *) calloc( Noutmat-2, sizeof(float));

   temp_list = ( int *)calloc((MAXNROI+1), sizeof( int)); 
   list_rois = ( int *)calloc((MAXNROI+1), sizeof( int)); 

   Tforw = calloc(ArrMax,sizeof(Tforw)); 
   for(i=0 ; i<ArrMax ; i++) 
      Tforw[i] = calloc(3,sizeof(int)); 
   Ttot = calloc(2*ArrMax,sizeof(Ttot)); 
   for(i=0 ; i<2*ArrMax ; i++) 
      Ttot[i] = calloc(3,sizeof(int)); 
   Tback = calloc(ArrMax,sizeof(Tback)); 
   for(i=0 ; i<ArrMax ; i++) 
      Tback[i] = calloc(3,sizeof(int)); 
   // temp storage whilst tracking, physical loc
   flTforw = calloc(ArrMax,sizeof(flTforw)); 
   for(i=0 ; i<ArrMax ; i++) 
      flTforw[i] = calloc(3,sizeof(int)); 
   flTback = calloc(ArrMax,sizeof(flTback)); 
   for(i=0 ; i<ArrMax ; i++) 
      flTback[i] = calloc(3,sizeof(int)); 
   // Nov,2016: calc for all tracking situations, to know lengths for
   // all
   flTtot = calloc(2*ArrMax,sizeof(flTtot)); 
   for(i=0 ; i<2*ArrMax ; i++) 
      flTtot[i] = calloc(3,sizeof(float)); 

   if(  (flTback == NULL) || (Tforw == NULL) || (Tback == NULL) 
        || (flTforw == NULL) || (temp_list == NULL) || (Ttot == NULL) 
        || (list_rois == NULL) || (Param_grid == NULL)
        || (Prob_grid == NULL) || (Prob_grid_L == NULL) 
        || (flTtot == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(12);
   }

   // slightly stricter now, because TrackItP includes extra endpoint
   // on each half tract tested
   ArrMax-= 1;

   // @))
   if(JUMPLES) {

      // right now, this contains space for:
      //     + a growing param,
      //     + and a counter of tracks per vox
      TROUT = (float ***) calloc( (Ndata+1), sizeof(float **) );
      for ( i = 0 ; i<=Ndata ; i++ ) 
         TROUT[i] = (float **) calloc( N_nets, sizeof(float *) );
      for ( i=0 ; i<=Ndata ; i++ ) 
         for ( j=0 ; j<N_nets ; j++ ) 
            TROUT[i][j] = (float *) calloc( 2 , sizeof(float) );
     
      if(  (TROUT == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(14);
      }
     
   }

   j = Setup_Labels_Indices_Unc_M_both( Dim, mskd, INDEX, 
                                        INDEX2, UNC,
                                        coorded, copy_coorded, 
                                        insetPARS[PARS_BOT],
                                        DirPerVox, N_HAR,
                                        insetVECS,
                                        insetUC,
                                        unc_minei_std, unc_minfa_std,
                                        N_nets, NROI,
                                        ROI_set, MAPROI, 
                                        INV_LABELS, NETROI);

   // can free uncert dsets
   if(nvox_unc){
      DSET_delete(insetUC);
      free(insetUC);
   }
   

   // Aug 2014
   if( opts.EXTRA_TR_PAR ) {
      
      INFO_message("Running three extra track parameter scalings for each"
                   " target pair:\n"
                   "\tNTpTarVol  :div. by average target volume;\n"
                   "\tNTpTarSA   :div. by average target surface area;\n"
                   "\tNTpTarSAFA :div. by average target surface area\n"
                   "\t            bordering suprathreshold FA (or equi-\n"
                   "\t            valent WM proxy definition).");

      // [Nnets][Nrois+1][2 -> Vol and Surf Area and SA near highFA]
      ROI_SIZES = (int ***) calloc( N_nets, sizeof(int **) );
      for ( i=0 ; i<N_nets ; i++ ) 
         ROI_SIZES[i] = (int **) calloc( INVROI[i]+1, sizeof(int *) );
      for ( i=0 ; i<N_nets ; i++ ) 
         for ( j=0 ; j<INVROI[i]+1 ; j++ ) 
            ROI_SIZES[i][j] = (int *) calloc( 3 , sizeof(int) );
      
      if(  (ROI_SIZES == NULL) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(14);
      }

      i = GetSizesOfTargets( ROI_SIZES,
                             MAPROI, Dim, 
                             mskd,
                             INDEX2,
                             coorded, opts.MinFA,
                             N_nets, NROI,
                             INV_LABELS );
      /*
        fprintf(stderr, "\n\nTEST ROI\n\n");
        fprintf(stderr, "VOL\n");
        for ( i=0 ; i<N_nets ; i++ ) {
        for ( j=1 ; j<INVROI[i]+1 ; j++ ) 
        fprintf(stderr, "%7d ",ROI_SIZES[i][j][0]);
        fprintf(stderr, "\n");
        }
        fprintf(stderr, "\n\nSA\n");
        for ( i=0 ; i<N_nets ; i++ ) {
        for ( j=1 ; j<INVROI[i]+1 ; j++ ) 
        fprintf(stderr, "%7d ",ROI_SIZES[i][j][1]);
        fprintf(stderr, "\n");
        }
        fprintf(stderr, "\n\nSAFA\n");
        for ( i=0 ; i<N_nets ; i++ ) {
        for ( j=1 ; j<INVROI[i]+1 ; j++ ) 
        fprintf(stderr, "%7d ",ROI_SIZES[i][j][2]);
        fprintf(stderr, "\n");
        }
      */


   }

   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************

   Numtract = 0;

   if(DETNET){ // @)

      // @))) start bundle tags
      // counter here: halftri+diag notation!!!
      for( hh=0 ; hh<N_nets ; hh++) 
         for( i=0 ; i<N_bund[hh] ; i++)
            tb[hh][i] = AppCreateBundle(NULL, 0, NULL); // start bundle
      
      if( opts.OUTPUT_TRK )
         for( hh=0 ; hh<N_nets ; hh++) {
            sprintf(OUT_bin,"%s_%03d.trk",opts.prefix,hh); // match brick num
            if( (fout0[hh] = fopen(OUT_bin, "w")) == NULL) {
               fprintf(stderr, "Error opening file %s.",OUT_bin);
               exit(16);
            }
            // all outputs have same header
            if(N_HAR)
               fwrite(&headerHAR,sizeof(tv_io_header),1,fout0[hh]); 
            else
               fwrite(&headerDTI,sizeof(tv_io_header),1,fout0[hh]); 
         }
      
      if (get_tract_verb()) {
         INFO_message("Begin tracking...");
      }
   }

   if(opts.TARG_SURF_STOP) 
      INFO_message("Surf-to-surf track mode ON.");
   if(opts.TARG_SURF_TWIXT) 
      INFO_message("\t... and 'twixt only.");

   if(opts.NAMEIN_thru) {
      thrumask = THD_open_dataset(opts.NAMEIN_thru);
      if( thrumask == NULL ) 
         ERROR_exit("Can't open dataset '%s': for extra set.",
                    opts.NAMEIN_thru);
      DSET_load(thrumask) ; CHECK_LOAD_ERROR(thrumask) ;
      if( !(Dim[0] == (int) DSET_NX(thrumask) && 
            Dim[1] == (int) DSET_NY(thrumask) && 
            Dim[2] == (int) DSET_NZ(thrumask) && 
            N_nets == (int) DSET_NVALS(thrumask) ) ) 
         ERROR_exit("The thru_mask '%s' doesn't match up fully with the data.\n"
                    "\tCheck the number of bricks, spatial dimensions and "
                    "the origin.",
                    opts.NAMEIN_thru);      

      if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
         
         dsetn = r_new_resam_dset( thrumask, NULL, 0.0, 0.0, 0.0,
                                   dset_or, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(thrumask); 
         thrumask=dsetn;
         dsetn=NULL;
      }
   }

   if(TR_MODE ==2) {
      ni = (int) opts.Nmonte / 10.;
      if (ni < 2)
         ni = opts.Nmonte;
   }
   else if(TR_MODE==1)
      ni = 1;
   if( ni<opts.Nmonte )
      fprintf(stderr,"++ Tracking progress count: start ...\n");
   t_start = time(NULL);

   for (gg=0 ; gg<opts.Nmonte ; gg++) {
      
      if( gg>0) {// first time through is no change
         // relative location of each seed within voxel for this iter
         for( k=0 ; k<opts.Nseed ; k++ ) // @) had just minorly rearr. here
            for( j=0 ; j<3 ; j++ ) 
               LocSeed[k][j] = rand()*1.0/RAND_MAX;

         if(N_HAR)
            j = HARDI_Perturb( Dim, mskd, INDEX, INDEX2,
                               UNC, coorded, copy_coorded, 
                               r, DirPerVox);
         else
            j = DTI_Perturb_M( Dim, mskd, INDEX, INDEX2,
                               UNC, coorded, copy_coorded, 
                               r, 
                               insetVECS);

      }

      // this is where we start the tracking for a given data set
      // start of Monte Carlo loop
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) 
               if( copy_coorded[INDEX2[i][j][k]][0]>=opts.MinFA ) 
                  for( kk=0 ; kk<opts.Nseed ; kk++ ) 
                     for( ll=0 ; ll<DirPerVox[INDEX2[i][j][k]] ; ll++ ) {

                        in[0] = i;
                        in[1] = j;
                        in[2] = k;

                        for( jj=0 ; jj<3 ; jj++ ) 
                           physin[jj] = ((float) in[jj]+LocSeed[kk][jj])*Ledge[jj];

                        len_forw = TrackItP_NEW_M( N_HAR, DirPerVox,
                                                   ll, copy_coorded, 
                                                   in, physin, Ledge, Dim, 
                                                   opts.MinFA, MaxAng, 
                                                   ArrMax, Tforw, 
                                                   flTforw, 1, phys_forw,INDEX2);

                        in[0] = i; // reset, because it's changed in TrackIt func
                        in[1] = j;
                        in[2] = k;

                        for( jj=0 ; jj<3 ; jj++ ) 
                           physin[jj] = ((float) in[jj]+LocSeed[kk][jj])*Ledge[jj];

                        len_back = TrackItP_NEW_M( N_HAR, DirPerVox,
                                                   ll, copy_coorded,
                                                   in, physin, Ledge, Dim, 
                                                   opts.MinFA, MaxAng, 
                                                   ArrMax, Tback, 
                                                   flTback, -1, phys_back,INDEX2);
                        // b/c of overlap of starts; includes 2ends
                        totlen = len_forw+len_back-1; 
                        totlen_phys = phys_forw[0] + phys_back[0];

                        if( totlen_phys >= opts.MinL ) {
                           Numtract += 1; //keeping tally of tot num of tracts

                           // glue together for simpler notation later
                           for( n=0 ; n<len_back ; n++) { // all of this
                              rr = len_back-n-1; // read in backward
                              for(m=0;m<3;m++)
                                 Ttot[rr][m] = Tback[n][m];
         
                           }
                           for( n=1 ; n<len_forw ; n++) { // skip first->overlap
                              rr = n+len_back-1; // put after
                              for(m=0;m<3;m++)
                                 Ttot[rr][m] = Tforw[n][m];
                           }

                           vB0 = ( Ttot[totlen-1][0]<0 ) ? totlen-2 : totlen-1;
                           //printf("%d\t%f\t\t%d\t%d\n",totlen,totlen_phys,
                           //Ttot[0][0],Ttot[totlen-1][0]);

                           // Nov,2016: calc lengths for all!
                           //if(DETNET){ // for now, a sep mirroring option to above
                              // glue together for simpler notation later
                              for( n=0 ; n<len_back ; n++) { // all of this
                                 rr = len_back-n-1; // read in backward
                                 for(m=0;m<3;m++)
                                    flTtot[rr][m] = flTback[n][m];
                              }
                              for( n=1 ; n<len_forw ; n++) { // skip first->overlap
                                 rr = n+len_back-1; // put after
                                 for(m=0;m<3;m++)
                                    flTtot[rr][m] = flTforw[n][m];
                              }
                              //}

                           // at this point now, both 0- and trL-th index 
                           // have `test index'
                           // locations in them; these are not `tract' locs
                           //  themselves, but
                           // voxels to check in terms of connections.
                           // if the locations are `bad'-- either tract went
                           // back on itself,
                           // or tried to walk out of brain data set array,
                           // then a -1 is
                           // stored there, so we check against that before
                           // looking at that
                           // index.
                        
                           for( hh=0 ; hh<N_nets ; hh++) { 

                              // checking for NOT masks, which map MAPROI[][]=-1
                              // if running through a not mask, split tract up
                              // walk through once, all the way, per network
                           
                              // initialize for this network.
                              // first, check if initial ends are bad--
                              // would have negative value from TrackItP only if 
                              // some badness had happened.

                              vB = ( Ttot[0][0]<0 ) ? -1 : -2;
                              vB1 = 0; 
                              // go until hitting a NOT or penult vox
                              while( vB+vB1 < vB0  ){ 
                                 // clear list of ROIs
                                 for( n=0 ; n<=MAXNROI ; n++)
                                    list_rois[n] = temp_list[n] = 0;
             
                                 // literal start and finishes
                                 vA0 = vB+2; // for `checking' purposes
                                 // this starts at either 1 or where we ended+2
                                 vA = ( vA0==0 ) ? 1 : vA0 ; // for writing
                                 for( n=vA0 ; n<=vB0 ; n++) {
                                    rr=INDEX2[Ttot[n][0]][Ttot[n][1]][Ttot[n][2]];
                                    if( MAPROI[rr][hh]>0 )
                                       list_rois[MAPROI[rr][hh]]=1;
                                    else if( MAPROI[rr][hh]<0 ) 
                                       break;
                                 }

                                 // keep track of where'd we gotten, 
                                 // as `last good voxel'
                                 // whether we made it all the way
                                 // through track, or ran
                                 // into a NOTvox; max vB here should be vB0 still.
                                 if( (n==totlen) ) {
                                    vB = totlen-2;
                                    vB1 = 1; // check extra one
                                 }
                                 else {
                                    vB = n-1;
                                    vB1 = 0;
                                 }

                                 // now, for this track, record 
                                 // first, write shorter list of which
                                 // ones were hit
                                 m = 0;
                                 for( n=1 ; n<=NROI[hh] ; n++)
                                    if(list_rois[n]>0 ) {
                                       // values stored are 1...NROI -> 0...NROI-1
                                       // keep track of which was hit
                                       temp_list[m] = n-1; 
                                       m = m+1;
                                    }
                              
                                 if ( opts.NAMEIN_thru ) 
                                    if( m>0 ) {
                                       doesnt_pass = 1;
                                       for( mm=vA ; mm<=vB ; mm++) { 
                                          rr = INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];
                                          if ( THD_get_voxel(thrumask,rr,hh) ) {
                                             doesnt_pass = 0;
                                             break;
                                          }
                                       }
                                       if( doesnt_pass )
                                          m=0; // and this will not go into next section!
                                    }
                              

                                 // let's keep track of where tracts connecting 
                                 // regions go.
                                 // we'll keep stats on individ ROI tracks
                                 if( m>0) {

                                    //if( (opts.ONLY_BT==0) || (m==1) ) { // uncut tracts
                                    if( (opts.LOG_TYPE==0) || (m==1) ) { // OR tracts
                                       // Nov,2016: not changing this
                                       // if{} part for surf_stop
                                       // estimates for tract length.
                                       // Would have to chop tracts, etc.
                                       // Might consider doing so in
                                       // future, if need arises.  So,
                                       // length on diagonals would be
                                       // full length of any tract.

                                       trL = vB-vA+1; // counter of len of flTtot

                                       if(trL>0) {

                                          bb = 0;
                                          idx3 = MatrInd_to_FlatUHT(temp_list[bb],temp_list[bb],NROI[hh]);
                                          phys_l = 0.;
                                          for( mm=vA ; mm<=vB ; mm++) {
                                       
                                             if( mm>vA) // phys len: Nov,2016
                                                phys_l+= sqrt( pow(flTtot[mm][0]-flTtot[mm-1][0],2) +
                                                               pow(flTtot[mm][1]-flTtot[mm-1][1],2) +
                                                               pow(flTtot[mm][2]-flTtot[mm-1][2],2));

                                             rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];

                                             // Don't loop-> we know m==1 here! Nov,2016
                                             NETROI[rr][hh][idx3]+=1;
                                             if(NETROI[rr][hh][idx3]==NmNsThr) 
                                                ss=ScoreTrackGrid_M(Param_grid,
                                                                    INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
                                                                    hh, idx3,
                                                                    insetPARS, PARS_BOT, PARS_TOP);

                                          }
                                          // store here in case doing PROB, not DET/MINI
                                          Prob_grid[hh][idx3]+= 1;
                                          Prob_grid_L[hh][idx3][0]+= phys_l;        // bundle L -> mean
                                          Prob_grid_L[hh][idx3][1]+= phys_l*phys_l; // bundle L -> stdev

                                          if(DETNET && opts.LOG_TYPE==0) { // OR logic
                                             // @) if created, it will be kept; data set is just for orient/orig
                                             tt[hh] = Create_Tract_NEW(0,trL-1, flTtot+vA, id[hh], 
                                                                       insetPARS[PARS_BOT]); 
                                             ++id[hh]; 
                                             //tb[hh][0] = AppCreateBundle(tb[hh][0], 1, tt[hh]); 
                                             tb[hh][idx3] = AppCreateBundle(tb[hh][idx3], 1, tt[hh]); 
                                             tt[hh] = Free_Tracts(tt[hh], 1);

                                             if( opts.OUTPUT_TRK )
                                                ss = SimpleWriteDetNetTr_M(N_HAR, fout0[hh], INDEX, 
                                                                           insetPARS, PARS_BOT, PARS_TOP,
                                                                           flTtot+vA, Ttot+vA, trL,
                                                                           TV_switch, Dim, Ledge);
                                          }
                                       }
                                    } // end of 'if or-logic or m==1'
                                    else{
                                       // first do diagonal/individual ones,
                                       // because now we
                                       // have options for the pairwise connectors

                                       // DIAGONAL
                                       phys_l = 0.;
                                       for( mm=vA ; mm<=vB ; mm++) {
                                          rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];

                                          if( mm>vA) // phys len: Nov,2016
                                             phys_l+= sqrt( pow(flTtot[mm][0]-flTtot[mm-1][0],2) +
                                                            pow(flTtot[mm][1]-flTtot[mm-1][1],2) +
                                                            pow(flTtot[mm][2]-flTtot[mm-1][2],2));

                                          for( bb=0 ; bb<m ; bb++) {
                                             idx3 = MatrInd_to_FlatUHT(temp_list[bb],temp_list[bb],NROI[hh]);
                                             NETROI[rr][hh][idx3]+=1;
                                             if(NETROI[rr][hh][idx3]==NmNsThr)
                                                ss=ScoreTrackGrid_M(Param_grid,
                                                                    INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
                                                                    hh, idx3,
                                                                    insetPARS, PARS_BOT, PARS_TOP);
                                          }
                                       }                                    

                                       for( bb=0 ; bb<m ; bb++) { // and add length info here
                                          idx3 = MatrInd_to_FlatUHT(temp_list[bb],temp_list[bb],NROI[hh]);
                                          Prob_grid[hh][idx3]+= 1;
                                          Prob_grid_L[hh][idx3][0]+= phys_l;        // bundle L -> mean
                                          Prob_grid_L[hh][idx3][1]+= phys_l*phys_l; // bundle L -> stdev
                                       }

                                       // CONNECTORS: walk through mult times
                                       // just do unique connectors (we know that m>=2 here...)
                                       for( bb=0 ; bb<m ; bb++)
                                          for( cc=bb+1 ; cc<m ; cc++) {

                                             if( !opts.ONLY_BT ) {
                                                start_loc = vA0;
                                                trL = vB+vB1 - vA0 + 1;
                                             }
                                             else{
                                                // 2 switches for finding ROI, and 1 for current 'FIND'
                                                onoff[0]=0; onoff[1]=0; onoff[2]=0;
                                                BreakAddCont=0;

                                                start_loc = vA0; // default start
                                                trL = 0; //counter of len of flTtot
                                                // now walk through each vox, keep testing and
                                                // evaluating at each step
                                                // Nov,2016: change to not just add voxels to WM_ROI
                                                // at each step of the way, but instead to record 
                                                // final start/stop
                                                for( mm=vA0 ; mm<=vB+vB1 ; mm++) { 
                                                   rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];
                                             
                                                   if( MAPROI[rr][hh]==temp_list[bb]+1 ) { // hit 1
                                                      onoff[0]=1;
                                                      onoff[2]=1;
                                                   }
                                                   else if( MAPROI[rr][hh]==temp_list[cc]+1 ){ // hit 2
                                                      onoff[1]=1;
                                                      onoff[2]=1;
                                                   }
                                                   else {// a miss, could be either in b/t or outside
                                                      onoff[2]=0;
                                                   }
                                                   switch(onoff[0]+onoff[1]) {
                                                   case 2:
                                                      // for default "trimmed", two opts:
                                                      if(onoff[2])
                                                         BreakAddCont=1; // still in last ROI
                                                      else{
                                                         BreakAddCont=-1; // done
                                                      }
                                                      // for surf-to-surf, trump last  
                                                      // choice and stop at 
                                                      // this moment, the first time the
                                                      // second ROI gets hit
                                                      if( opts.TARG_SURF_STOP ) {
                                                         BreakAddCont=-1; // done
                                                         trL++; // and include this vox
                                                         // will remove later if 
                                                         // *TWIXT mode is on.
                                                      }
                                                      break;
                                                   case 1:
                                                      BreakAddCont=1; // in 1st or in b/t
                                                      break;
                                                   default:
                                                      BreakAddCont=0; // just keep walking
                                                      break;
                                                   }
									
                                                   // are in b/t, and not at edge; 
                                                   // and can't include 0th track element
                                                   if( (BreakAddCont==1) && mm && (mm<totlen-1) ) {

                                                      // catch this on the first one
                                                      if( trL==0 )
                                                         start_loc=mm;
                                                      else if( opts.TARG_SURF_STOP )
                                                         // for surf-to-surf, reset at any point
                                                         // where we are still in first
                                                         // region
                                                         if( onoff[2] ) {
                                                            start_loc=mm;
                                                            trL = 0; // reset counter
                                                         }
                                                      trL++;
                                                   }
                                                   else if(BreakAddCont==-1) {// done
                                                      break;
                                                   }
                                                   else {// unnec cond...
                                                      continue;
                                                   }
                                                } // end of mm

                                                if( opts.TARG_SURF_TWIXT ) {
                                                   // to be *between* regions only;
                                                   // we only need to pull back *one* on
                                                   // each side of the target connections
                                                   start_loc+=1;
                                                   trL-=2;
                                                }
                                             }

                                             if (trL>0) {

                                                phys_l = 0.;
                                                // now put tract into action
                                                idx3 = MatrInd_to_FlatUHT(temp_list[bb],temp_list[cc],NROI[hh]);
                                                for( mm=start_loc ; mm<(start_loc+trL) ; mm++) {

                                                   if( mm>start_loc ) // phys len: Nov,2016
                                                      phys_l+= sqrt( pow(flTtot[mm][0]-flTtot[mm-1][0],2) +
                                                                     pow(flTtot[mm][1]-flTtot[mm-1][1],2) +
                                                                     pow(flTtot[mm][2]-flTtot[mm-1][2],2));

                                                   // get both sides of param_grid, b/c just testing one,
                                                   // and param_grid is symm
                                                   rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];

                                                   NETROI[rr][hh][idx3]+=1;
                                                   if(NETROI[rr][hh][idx3]==NmNsThr) {
                                                      ss=ScoreTrackGrid_M( Param_grid,
                                                                           INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
                                                                           hh, idx3,
                                                                           insetPARS, PARS_BOT, PARS_TOP);
                                                   }
                                                }

                                                Prob_grid[hh][idx3]+= 1;
                                                Prob_grid_L[hh][idx3][0]+= phys_l;        // bundle L -> mean 
                                                Prob_grid_L[hh][idx3][1]+= phys_l*phys_l; // bundle L -> stdev

                                                if(DETNET && opts.LOG_TYPE==1) {
                                                   // @) if created, it will be kept
                                                   tt[hh] = Create_Tract_NEW(0, trL-1, flTtot+start_loc, id[hh], 
                                                                             insetPARS[PARS_BOT]); ++id[hh]; 

                                                   lll = MatrInd_to_FlatUHT(temp_list[bb],temp_list[cc],NROI[hh]);
                                                   // AUGCHECK:  think about temp_list ordering...
                                                   //lll = temp_list[cc]+temp_list[bb]*NROI[hh]; // sq matr coor
                                                   //lll -= (temp_list[bb]*(temp_list[bb]+1))/2; // fix for tridiag.
                                                   //if(lll>N_bund[hh])
                                                   //fprintf(stderr, "  tb[%d][%d]=%p\n",hh, lll, tb[hh][lll]);
                                                   tb[hh][lll] = AppCreateBundle(tb[hh][lll], 1, tt[hh]); 
                                                   tt[hh] = Free_Tracts(tt[hh], 1);

                                                   if( opts.OUTPUT_TRK )
                                                      ss = SimpleWriteDetNetTr_M(N_HAR, fout0[hh], INDEX, 
                                                                                 insetPARS, PARS_BOT, PARS_TOP,
                                                                                 flTtot+start_loc, Ttot+start_loc, trL,
                                                                                 TV_switch, Dim, Ledge);

                                                }
                                             } // end of trL>0
                                          } // end of cc
                                    } // end of an else
                                 }// end of 'if m>0'
                              }
                           }
                        }
                     }

      if( TR_MODE == 2 ) {
         if (gg && (gg % ni == 0) ) 
            fprintf(stderr,"\t%s %3.0f%% %s -> %.2f min\n",
                    "[", gg *10./ni,"]", (float) difftime(time(NULL),t_start)/60.);
      }
      else if( TR_MODE ==1 )
         fprintf(stderr,"\t%s %2d/%-2d %s -> %.2f min\n",
                 "[", gg+1,opts.Nmonte,"]", (float) difftime(time(NULL),t_start)/60.);
      
      
   } // end of Monte Carlo loop

   if( TR_MODE == 2 )
      if( ni<opts.Nmonte )
         fprintf(stderr,"\t%s %3.0f%% %s -> %.2f min\n",
                 "[", 100.,"]", (float) difftime( time(NULL) ,t_start)/60.);
      else if( TR_MODE ==1 )
         fprintf(stderr,"\t%s %2d/%-2d %s -> %.2f min\n",
                 "[", opts.Nmonte,opts.Nmonte,"]", 
                 (float) difftime(time(NULL), t_start)/60.);

   if(DETNET){
      INFO_message("Done tracking, tidying up outputs...");
      for( i=0 ; i<N_nets ; i++){
         if( opts.OUTPUT_TRK )
            fclose(fout0[i]); // !important to do...

         m = 0; // count num of tracts per net

         // Feb,2015: threshold by bundle size; default
         // N_tract/bundle=1, but user can increase
         // 
         for (j=0; j<NROI[i]; j++) 
            for (k=j; k<NROI[i]; k++) {
               jj = j*NROI[i] + k;
               kk = j + k*NROI[i];
               lll = MatrInd_to_FlatUHT(j,k,NROI[i]); // the 1-index location

               if ( tb[i][lll]->N_tracts >= opts.THRESH_BUNDS ) { //THRESH_BUNDS
                  snprintf( EleNameStr, 128, "%s<->%s",
                            ROI_STR_LABELS[i][j+1], ROI_STR_LABELS[i][k+1]);

                  m+= tb[i][lll]->N_tracts;

                  if (get_tract_verb()) {
                     INFO_message("  From tracking, net[%d],"
                                  " bund[%d] has %d tracks.",
                                  i,lll+1, tb[i][lll]->N_tracts);
                     Show_Taylor_Bundle(tb[i][lll], NULL, 3);
                  }
                  
                  tnet[i] = AppAddBundleToNetwork(tnet[i], &(tb[i][lll]), 
                                                  jj,kk, insetPARS[PARS_BOT], 
                                                  EleNameStr); 
               }
               else if( tb[i][lll]->N_tracts ) { // if it's nonzero, but subthr
                  INFO_message("   Removed net[%d]'s bundle '%s<->%s' "
                               "(only %d tracts).",
                               i, ROI_STR_LABELS[i][j+1], 
                               ROI_STR_LABELS[i][k+1], 
                               tb[i][lll]->N_tracts);

                  rr = ByeByeBundle( j,
                                     k,
                                     i,
                                     Prob_grid,
                                     Prob_grid_L,
                                     Param_grid,
                                     Noutmat-2, // len of Param_grid 3rd dim
                                     NETROI,
                                     Ndata+1, // len of NETROI in 1st dim
                                     NROI );
               }

               Free_Bundle(tb[i][lll]); // free some here... and more below
            }
         
         INFO_message("From tracking, net[%d] has %d tracks.", i, m);
      }

      for( i=0 ; i<N_nets ; i++){
         sprintf(prefix_det[i],"%s_%03d",opts.prefix,i); 
         
         if (!Write_Network(tnet[i],prefix_det[i],mode)) 
            ERROR_message("Failed to write the network.");
     
         tnet[i] = Free_Network(tnet[i]);
      }
   }

   // **************************************************************
   // **************************************************************
   //                    Some outputs
   // **************************************************************
   // **************************************************************
	
   if(Numtract > 0 ) {

      // apply threshold with all output stats.
      // threshold determined by:  having more than 1 voxel in the WM-ROI
      // (assuming that preeetty much always there will be either ==0 or >>1)
      // calc mean and stdevs of different Param_grid entries
      for( k=0 ; k<N_nets ; k++) 
         for( i=0 ; i<FlatUHT_Len(NROI[k]) ; i++ ) 
            if(Param_grid[k][i][0]>1.5) { // need at least 2 vox per ROI
               for( m=0 ; m<PARS_N ; m++) {
                  // means
                  Param_grid[k][i][2*m+1]/= Param_grid[k][i][0];
                  // stdevs
                  Param_grid[k][i][2*(m+1)]-= 
                     Param_grid[k][i][0]*pow(Param_grid[k][i][2*m+1],2);
                  Param_grid[k][i][2*(m+1)]/= Param_grid[k][i][0]-1;
                  Param_grid[k][i][2*(m+1)] = sqrt(Param_grid[k][i][2*(m+1)]);
               }
               Prob_grid_L[k][i][0]/= Prob_grid[k][i]; // div by num of tracts
               if( Prob_grid[k][i] > 1.5 ) {// need at least 2 tr per bun
                  Prob_grid_L[k][i][1]-= Prob_grid[k][i] * pow(Prob_grid_L[k][i][0],2);
                  Prob_grid_L[k][i][1]/= Prob_grid[k][i] - 1;
                  Prob_grid_L[k][i][1] = sqrt(Prob_grid_L[k][i][1]);
               }
               else{
                  INFO_message("Network [%d]:\t [%d]th WM bundle "
                               "has only one tract!", k, i);
                  Prob_grid_L[k][i][1] = 0.;
               }
            }
            else {
               for( m=0 ; m<PARS_N ; m++) {
                  Param_grid[k][i][2*m+1]=0.;
                  Param_grid[k][i][2*(m+1)]=0.;
               }
               Prob_grid[k][i] = 0;
               Prob_grid_L[k][i][0] = 0;
               Prob_grid_L[k][i][1] = 0;
            }

      for( k=0 ; k<N_nets ; k++) { // each netw gets own file
			
         // print out prob grid
         sprintf(OUT_grid,"%s_%03d.grid",opts.prefix,k); // matches brick number
         if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
            fprintf(stderr, "Error opening file %s.",OUT_grid);
            exit(19);
         }
    
         fprintf(fout1,"# %d  # Number of network ROIs\n",NROI[k]); // N_ROIs
         fprintf(fout1,
                 "# %d  # Number of grid matrices\n"
                 ,Noutmat); // Num of matrices

         // Sept 2014:  label_table stuff
         //if( roi_dtable ) {
         fprintf(fout1, "# WITH_ROI_LABELS\n");
         for( i=1 ; i<NROI[k] ; i++ ) 
            fprintf(fout1," %10s \t",ROI_STR_LABELS[k][i]); 
         fprintf(fout1,"  %10s\n",ROI_STR_LABELS[k][i]);
         //}
         
         for( i=1 ; i<NROI[k] ; i++ ) // labels of ROIs
            fprintf(fout1," %10d \t",ROI_LABELS[k][i]); // at =NROI, -> \n
         fprintf(fout1,"  %10d\n",ROI_LABELS[k][i]);

         fprintf(fout1,"# %s\n", ParLab[0]); // NT 
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) // b/c we put '\n' after last one.
               fprintf(fout1,"%12d\t",
                       Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]);
            fprintf(fout1,"%12d\n",
                    Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]);
         }

         fprintf(fout1,"# %s\n",ParLab[1]); // fNT = frac num of tracts
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*1.0/Numtract);
            fprintf(fout1,"%e\n",
                    Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*1.0/Numtract);
         }

         fprintf(fout1,"# %s\n",ParLab[2]); // PV = phys vol
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Param_grid[k][MatrInd_to_FlatUHT(i,j,
                                                        NROI[k])][0]*VoxVol);
            fprintf(fout1,"%e\n",
                    Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]*VoxVol);
         }

         fprintf(fout1,"# %s\n",ParLab[3]); // fNV = frac num of vox
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]/Ndata);
            fprintf(fout1,"%e\n",
                    Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]/Ndata);
         }

         fprintf(fout1,"# %s\n",ParLab[4]); // NV = num of vox
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]);
            fprintf(fout1,"%e\n",
                    Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]);
         }

         fprintf(fout1,"# %s\n",ParLab[5]); // BL = mean bundle lengths
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Prob_grid_L[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]);
            fprintf(fout1,"%e\n",
                    Prob_grid_L[k][MatrInd_to_FlatUHT(i,j,NROI[k])][0]);
         }

         fprintf(fout1,"# %s\n",ParLab[6]); // sBL = stdev bundle lengths
         for( i=0 ; i<NROI[k] ; i++ ) {
            for( j=0 ; j<NROI[k]-1 ; j++ ) 
               fprintf(fout1,"%e\t",
                       Prob_grid_L[k][MatrInd_to_FlatUHT(i,j,NROI[k])][1]);
            fprintf(fout1,"%e\n",
                    Prob_grid_L[k][MatrInd_to_FlatUHT(i,j,NROI[k])][1]);
         }

         if (opts.EXTRA_TR_PAR) {
            // NTpTarVol = NT scale by ave target vol
            fprintf(fout1,"# %s\n",ParLab[N_DEF_PAR_LABS+0]); 
            for( i=0 ; i<NROI[k] ; i++ ) {
               for( j=0 ; j<NROI[k]-1 ; j++ ) 
                  fprintf(fout1,"%e\t",
                          Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                          2.0/(ROI_SIZES[k][i+1][0] + ROI_SIZES[k][j+1][0]));
               fprintf(fout1,"%e\n",
                       Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                       2.0/(ROI_SIZES[k][i+1][0] + ROI_SIZES[k][j+1][0]));
            }
            
            // NTpTarSA = NT scale by ave target SA
            fprintf(fout1,"# %s\n",ParLab[N_DEF_PAR_LABS+1]); 
            for( i=0 ; i<NROI[k] ; i++ ) {
               for( j=0 ; j<NROI[k]-1 ; j++ ) 
                  fprintf(fout1,"%e\t",
                          Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                          2.0/(ROI_SIZES[k][i+1][1] + ROI_SIZES[k][j+1][1]));
               fprintf(fout1,"%e\n",
                       Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                       2.0/(ROI_SIZES[k][i+1][1] + ROI_SIZES[k][j+1][1]));
            }

            // NTpTarSAFA = NT scale by ave target SAFA
            fprintf(fout1,"# %s\n",ParLab[N_DEF_PAR_LABS+2]); 
            for( i=0 ; i<NROI[k] ; i++ ) {
               for( j=0 ; j<NROI[k]-1 ; j++ ) 
                  fprintf(fout1,"%e\t",
                          Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                          2.0/(ROI_SIZES[k][i+1][2] + ROI_SIZES[k][j+1][2]));
               fprintf(fout1,"%e\n",
                       Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]*
                       2.0/(ROI_SIZES[k][i+1][2] + ROI_SIZES[k][j+1][2]));
            }

         } // done extra par

         for( m=1 ; m<Noutmat-N_DEF_PAR_LABS-opts.EXTRA_TR_PAR +1; m++) {
            fprintf(fout1,"# %s\n",ParLab[N_DEF_PAR_LABS + 
                                          opts.EXTRA_TR_PAR + m - 1]);    
            for( i=0 ; i<NROI[k] ; i++ ) {
               for( j=0 ; j<NROI[k]-1 ; j++ ) 
                  fprintf(fout1,"%e\t",
                          Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][m]);
               fprintf(fout1,"%e\n",
                       Param_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])][m]);
            }
         }

         fclose(fout1);

         // @#$ recapitulate *.grid files
         // NB: flatmatr is flat *full* matr; *_grids are flat *UHT* matr
         if( DEF_DTI) {
            for( i=0 ; i<NROI[k] ; i++ ) 
               for( j=0 ; j<NROI[k] ; j++ ) { 
                  idx3 = MatrInd_to_FlatUHT(i,j,NROI[k]);
                  flat_matr[k][0][i*NROI[k]+j] = Prob_grid[k][idx3];
                  flat_matr[k][1][i*NROI[k]+j] = Prob_grid[k][idx3]*1.0/Numtract;
                  flat_matr[k][2][i*NROI[k]+j] = Param_grid[k][idx3][0]*VoxVol;
                  flat_matr[k][3][i*NROI[k]+j] = Param_grid[k][idx3][0]*Ndata;
                  flat_matr[k][4][i*NROI[k]+j] = Param_grid[k][idx3][0]; 
                  flat_matr[k][5][i*NROI[k]+j] = Prob_grid_L[k][idx3][0]; // Nov,2016
                  flat_matr[k][6][i*NROI[k]+j] = Prob_grid_L[k][idx3][1]; 

                  if(opts.EXTRA_TR_PAR){
                     flat_matr[k][N_DEF_PAR_LABS+0][i*NROI[k]+j] = Prob_grid[k][idx3]*
                        2.0/(ROI_SIZES[k][i+1][0] + ROI_SIZES[k][j+1][0]);
                     flat_matr[k][N_DEF_PAR_LABS+1][i*NROI[k]+j] = Prob_grid[k][idx3]*
                        2.0/(ROI_SIZES[k][i+1][1] + ROI_SIZES[k][j+1][1]);
                     flat_matr[k][N_DEF_PAR_LABS+2][i*NROI[k]+j] = Prob_grid[k][idx3]*
                        2.0/(ROI_SIZES[k][i+1][2] + ROI_SIZES[k][j+1][2]);
                  }
                  
                  for( m=1 ; m<Noutmat-N_DEF_PAR_LABS-opts.EXTRA_TR_PAR+1 ;m++) 
                     flat_matr[k][N_DEF_PAR_LABS+opts.EXTRA_TR_PAR+m-1]
                        [i*NROI[k]+j] = Param_grid[k][idx3][m];
               }
            gset = SUMA_FloatVec_to_GDSET(flat_matr[k], Noutmat, 
                                          NROI[k]*NROI[k], 
                                          "full", ParLab, 
                                          NULL, NULL, NULL);

            if( xyz = THD_roi_cmass(ROI_set, k, ROI_LABELS[k]+1, NROI[k]) ) {
               if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
                                                  xyz, NULL, NULL, 
                                                  gdset_roi_names[k],
                                                  NULL, NULL,
                                                  NROI[k]))) { // jan. 2014
                  ERROR_message("Failed to add node list");
                  exit(1);  
               }
               free(xyz);
            } 
            else {
               ERROR_message("Failed in THD_roi_cmass"); exit(1);
            }

            sprintf(OUT_gdset,"%s_%03d",opts.prefix,k);
            GDSET_netngrlink = 
               Network_link(SUMA_FnameGet( OUT_gdset, "f",NULL));
            NI_add_to_group(gset->ngr, GDSET_netngrlink);
            NAME_gdset = SUMA_WriteDset_ns( OUT_gdset,
                                            gset, SUMA_ASCII_NIML, 1, 0);
            if (!NAME_gdset && !SUMA_IS_DSET_STDXXX_FORMAT(SUMA_ASCII_NIML)) { 
               ERROR_message("Failed to write dataset."); exit(1); 
            } else {
               if (NAME_gdset) SUMA_free(NAME_gdset); NAME_gdset = NULL;      
            }
            SUMA_FreeDset(gset);
            gset=NULL;

         }
      }

      // in order to threshold the `overall' (`0th') map;
      // individual ones already have
      // been above, just by how they were created
      for( i=0 ; i<=Ndata ; i++ ) 		
         for( j=0 ; j<N_nets ; j++) {
            hh = NROI[j]*(NROI[j]+1);
            hh/= 2;
            for( k=0 ; k<hh ; k++ ) 
               if( NETROI[i][j][k] < NmNsThr)
                  NETROI[i][j][k]=0;
         }

      // output AFNI files mapping WM		
      INFO_message("Writing output (%s, same as your input): %s ...",
                   voxel_order,opts.prefix);

      // some output messaging
      for( k=0 ; k<N_nets ; k++) {
         hh = 0;
         for( i=0 ; i<NROI[k] ; i++ ) 
            for( j=i+1 ; j<NROI[k] ; j++ ) // Don't include diags
               if(Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]>0){
                  hh+=1;
               }
         if( NROI[k]>1 )
            INFO_message("Number of pairwise connections (bundles) in netw[%d] = %d",
                         k,hh); 
      }

      if (opts.OUT_INDIPAIR)  // Nov. 2014
         i = WriteBasicProbFiles(N_nets, Ndata, Nvox, opts.prefix, 
                                 insetPARS[PARS_BOT], TV_switch, voxel_order,
                                 NROI, NETROI, mskd, INDEX2, Dim,
                                 dsetn, argc, argv,
                                 ROI_STR_LABELS, opts.DUMP_with_LABELS,
                                 roi_dtable,
                                 ROI_LABELS, opts.PAIRPOWER,
                                 opts.NIFTI_OUT);
      else
         INFO_message("Not writing out *INDI* and *PAIR* maps."); 

      if(opts.DUMP_TYPE>=0)
         // Prob_grid just for counting number of outputs finally
         i = WriteIndivProbFiles(N_nets, Ndata, Nvox, Prob_grid,
                                 opts.prefix, insetPARS[PARS_BOT],
                                 TV_switch, voxel_order,NROI,
                                 NETROI, mskd, INDEX2, Dim,
                                 dsetn, argc, argv,
                                 Param_grid, opts.DUMP_TYPE,
                                 opts.DUMP_ORIG_LABS, ROI_LABELS, opts.POST,
                                 ROI_STR_LABELS, opts.DUMP_with_LABELS,
                                 opts.NIFTI_OUT);
		
      //INFO_message("Brainwide total number of tracts found = %d",Numtract);
   }
   else{
      INFO_message(" No Tracts Found!!!");
    
      sprintf(OUT_map,"%s.pmap",opts.prefix);
      if( (fout1 = fopen(OUT_map, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",OUT_map);
         exit(19);
      }
    
      fprintf(fout1,"0!\n");    
      fclose(fout1);

      sprintf(OUT_grid,"%s.grid",opts.prefix);
      if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",OUT_grid);
         exit(19);
      }

      fprintf(fout1,"0!\n");
      fclose(fout1);
   }

   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   if(LabTabStr)
      free(LabTabStr); 
   if(roi_dtable)
      free(roi_dtable);

   if( opts.EXTRA_TR_PAR ) {
      for ( i=0 ; i<N_nets ; i++ ) 
         for ( j=0 ; j<INVROI[i]+1 ; j++ ) 
            free(ROI_SIZES[i][j]);
      for ( i=0 ; i<N_nets ; i++ ) 
         free(ROI_SIZES[i]);
      free(ROI_SIZES);
   }

   for ( i=0 ; i<N_nets ; i++ ) 
      for ( j=0 ; j<NROI[i]+1 ; j++ ) 
         free(ROI_STR_LABELS[i][j]);
   for ( i=0 ; i<N_nets ; i++ ) 
      free(ROI_STR_LABELS[i]);
   free(ROI_STR_LABELS);

   for( i=PARS_BOT ; i<PARS_TOP ; i++){
      DSET_delete(insetPARS[i]);
      free(insetPARS[i]);
   }
   free(insetPARS);

   if(DEF_DTI)
      ii = 3;
   else
      ii = N_HAR;
   for( i=0 ; i<ii ; i++){
      DSET_delete(insetVECS[i]);
      free(insetVECS[i]);
   }
   free(insetVECS);

   for( i=0 ; i<Noutmat ; i++)  
      free(ParLab[i]);
   free(ParLab);

   for( i=0 ; i<N_DTI_MAX_PARS ; i++)  
      free(wild_names[i]);
   free(wild_names);

   for ( i = 0 ; i < N_nets ; i++ ) {
      for (j = 0; j < NROI[i]; ++j) 
         free(gdset_roi_names[i][j]);
      free(gdset_roi_names[i]);
   }
   free(gdset_roi_names);
   
   for ( i = 0 ; i < N_nets ; i++ ) 
      for ( j = 0 ; j < Noutmat ; j++ ) 
         free(flat_matr[i][j]);
   for ( i = 0 ; i < N_nets ; i++ ) 
      free(flat_matr[i]);
   free(flat_matr);

   DSET_delete(ROI_set);

   free(ROI_set);
   /*	free(opts.prefix);
      free(opts.dti_inpref);
      free(NAMEIN_netrois);
      if ( NAMEIN_uncert ) 
      free(NAMEIN_uncert);
      if ( NAMEIN_mask ) 
      free(NAMEIN_mask);
      if( in_EXTRA )
      free( in_EXTRA );
      if( opts.algopt_file_name ) 
      free( opts.algopt_file_name );
      if ( NAMEIN_mode ) 
      free(NAMEIN_mode);
      if ( NAMEIN_uncEI )
      free( NAMEIN_uncEI );
      if ( NAMEIN_uncFA )
      free( NAMEIN_uncFA );
      if ( NAMEIN_outmode ) 
      free(NAMEIN_outmode);*/

   free(DirPerVox);
   if(HAVE_MASK) {
      DSET_delete(MASK);
      free(MASK);
   }

   for( i=0 ; i<2*ArrMax ; i++) 
      free(Ttot[i]);
   free(Ttot);

   for( i=0 ; i<ArrMax ; i++) {
      free(Tforw[i]);
      free(Tback[i]);
      free(flTforw[i]);
      free(flTback[i]);
   }
   free(Tforw);
   free(Tback);
   free(flTforw);
   free(flTback);

   for( i=0 ; i<2*ArrMax ; i++) {
      free(flTtot[i]);
   }
   free(flTtot);
   
   if(DETNET){ // @) freeing
      // Feb,2015: this pointer should already have been freed above!
      //for( i=0 ; i<N_nets ; i++) 
      // for ( j=0 ; j<N_bund[i] ; j++ ) // halftri+diag notation!!!
      //    free(tb[i][j]); 
      for( i=0 ; i<N_nets ; i++) {
         free(tb[i]);
         free(tt[i]);
         free(tnet[i]);
      }
      free(id);
      free(tb);
      for( i=0 ; i<N_nets ; i++) {
         free(prefix_det[i]); 
      }
      free(prefix_det);
      free(N_bund);

   }

   if(JUMPLES){
      for( k=0 ; k<=Ndata ; k++) 
         for( m=0 ; m<N_nets ; m++) 
            free(TROUT[k][m]);
      for( k=0 ; k<=Ndata ; k++) 
         free(TROUT[k]);
      free(TROUT);

   }

   for( i=0 ; i<opts.Nseed ; i++) 
      free(LocSeed[i]);
   free(LocSeed);
  
   for( k=0 ; k<=Ndata ; k++) 
      for( m=0 ; m<N_nets ; m++) {
         free(NETROI[k][m]);
      }
   for( k=0 ; k<=Ndata ; k++) {
      free(coorded[k]);
      free(copy_coorded[k]);
      free(NETROI[k]);
      free(MAPROI[k]);
      if(N_uncert)
         free(UNC[k]);
   }
   free(coorded);
   free(copy_coorded);
   free(NETROI);
   free(MAPROI);
   if(N_uncert)
      free(UNC);

   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) {
         free(INDEX[i][j]);
         free(mskd[i][j]);
      }
   for( i=0 ; i<Dim[0] ; i++) {
      free(INDEX[i]);
      free(mskd[i]);
   }
   free(INDEX);
   free(mskd);

   /*if(THRU_MASK>0){
     for( k=0 ; k<=Ndata ; k++) 
     free(thrumask[k]);
     free(thrumask);
     }*/
   if(opts.NAMEIN_thru) {
      DSET_delete(thrumask);
      free(thrumask);
   }

   for( i=0 ; i<Dim[0] ; i++) 
      for( j=0 ; j<Dim[1] ; j++) 
         free(INDEX2[i][j]);
   for( i=0 ; i<Dim[0] ; i++) 
      free(INDEX2[i]);
   free(INDEX2);
	
   for( i=0 ; i<N_nets ; i++) 
      for( j=0 ; j<FlatUHT_Len(NROI[i]) ; j++) { 
         free(Param_grid[i][j]);
         free(Prob_grid_L[i][j]);
      }
   for( i=0 ; i<N_nets ; i++) {
      free(Prob_grid[i]);
      free(Param_grid[i]);
      free(Prob_grid_L[i]);
   }
   free(Prob_grid);
   free(Param_grid);
    free(Prob_grid_L);

   free(temp_list);
   free(list_rois);

   for( i=0 ; i<N_nets ; i++) {
      free(ROI_LABELS[i]);
      free(INV_LABELS[i]);
   }
   free(ROI_LABELS);
   free(INV_LABELS);

   free(NROI);
   free(INVROI);
   gsl_rng_free(r); // free also
   free(Dim);
   free(TV_switch);
   free(voxel_order);

   return 1;
}





// ORIGINAL GDSET_TOY example, moved from it's place among other
// command line input options

/*  if ( strcmp(argv[iarg],"-gdset_toy") == 0 ) {
    float x[3]={-40, 38, 4.7};
    float y[3]={-27, -34, 14.4};
    float z[3]={26, 41, 55};
    // alternate coordinate specification 
    float xyz[9]={ x[0], y[0], z[0], x[1], y[1], z[1], x[2], y[2], z[2]};
    float **mv=NULL;
    int i,j,k=0;
    char **labs, *NameOut;
    SUMA_DSET *gset=NULL;
    TAYLOR_TRACT *tt=NULL;
    TAYLOR_BUNDLE *tb=NULL;
    TAYLOR_NETWORK *net=NULL;
    NI_group *netngrlink=NULL, *netngr=NULL;
         
    int onexyz = 1; // set to 1 to specify coords by xyz triplets 
    //  in demo. 0 to specify x,y, and z as separate
    // vectors 

    INFO_message("A demonstration for creating/writing graph dset\n"
    "The vast majority of this content is for creating\n"
    "dummy data.\n"
    "Non dummy sections are marked with -->\n");

    // Create 2 matrices and some matrix labels 
    mv = (float **)calloc(2, sizeof(float*));
    for (i=0; i<2; ++i) mv[i] = (float *)calloc(9, sizeof(float));
    labs = (char **)calloc(2, sizeof(char*));
    for (i=0; i<2; ++i) labs[i]=calloc(64, sizeof(char));

    for (i=0; i<2; ++i) {
    for (j=0; j<9; ++j) {
    mv[i][j] = k++;
    }
    sprintf(labs[i],"Label Of Matrix %d", i);
    }
         
    // --> Create a graph dataset out of these two matrices 
    gset = SUMA_FloatVec_to_GDSET(mv, 2, 9, "full", labs, 
    NULL, NULL, NULL);
         
    // --> Now add the XYZ of each graph point, two ways are possible
    if (onexyz) { // for when your ROI centroid coords 
    // and in one vector of XYZ triplets 
    if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
    xyz, NULL, NULL,NULL, NULL, NULL, 3))) { // jan. 2014
    ERROR_message("Failed to add node list");
    exit(1);  
    }                                     
    } else {
    if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
    x, y, z, NULL, NULL, 
    NULL, 3))) { // jan. 2014
    ERROR_message("Failed to add node list");
    exit(1);     
    }
    }

    // Create some dummy bundles representing edge 1-2=5, or 2-1=7
    tb = NULL; net = NULL; tt = NULL;
    tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
    tt->id=77; tt->N_pts3=12; 
    tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
    tt->pts[0]=x[1]; tt->pts[1]=y[1]; tt->pts[2]=z[1];
    tt->pts[3]=22;   tt->pts[4]=36;   tt->pts[5]=40;
    tt->pts[6]=22;   tt->pts[7]=33;   tt->pts[8]=49;
    tt->pts[9]=x[2]; tt->pts[10]=y[2];tt->pts[11]=z[2];
    tb = AppCreateBundle(tb, 1, tt);
    tt = Free_Tracts(tt, 1);
    // put another track in 
    tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
    tt->id=78; tt->N_pts3=12; 
    tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
    tt->pts[0]=x[1]; tt->pts[1]=y[1]; tt->pts[2]=z[1];
    tt->pts[3]=23;   tt->pts[4]=35;   tt->pts[5]=42;
    tt->pts[6]=20;   tt->pts[7]=32;   tt->pts[8]=51;
    tt->pts[9]=x[2]; tt->pts[10]=y[2];tt->pts[11]=z[2];
    tb = AppCreateBundle(tb, 1, tt);
    tt = Free_Tracts(tt, 1);
    // add it to network 
    net = AppAddBundleToNetwork(net, &tb, 5, 7, NULL);
    // make another one for edge 0-1=1 and 1-0=3
    tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
    tt->id=77; tt->N_pts3=15; 
    tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
    tt->pts[0]=x[0]; tt->pts[1]=y[0];  tt->pts[2]=z[0];
    tt->pts[3]=5;    tt->pts[4]=12;    tt->pts[5]=17;
    tt->pts[6]=16;   tt->pts[7]=13;    tt->pts[8]=12;
    tt->pts[9]=20;   tt->pts[10]=16;   tt->pts[11]=16;
    tt->pts[12]=x[1];tt->pts[13]=y[1]; tt->pts[14]=z[1];
    tb = AppCreateBundle(tb, 1, tt);
    tt = Free_Tracts(tt, 1);
    // add bundle to network 
    net = AppAddBundleToNetwork(net, &tb, 1, 3, NULL);

    // --> Now put network in graph dset 
    netngr = Network_2_NIgr(net, 1);
    NI_add_to_group(gset->ngr, netngr);

    // --> Write the graph dataset 
    NameOut = SUMA_WriteDset_ns ("toy", gset, SUMA_ASCII_NIML, 1, 0);
    if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(SUMA_ASCII_NIML)) { 
    ERROR_message("Failed to write dataset."); exit(1); 
    } else {
    if (NameOut) SUMA_free(NameOut); NameOut = NULL;      
    }
         
    // --> Now alternately you can leave the network outside of the 
    //     graph dataset and just put a link element to it           
    NI_remove_from_group(gset->ngr, netngr);
    netngrlink = Network_link("toy.network");
    NI_add_to_group(gset->ngr, netngrlink);
    NameOut = SUMA_WriteDset_ns ("toy.link",
    gset, SUMA_ASCII_NIML, 1, 0);
    if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(SUMA_ASCII_NIML)) { 
    ERROR_message("Failed to write dataset."); exit(1); 
    } else {
    if (NameOut) SUMA_free(NameOut); NameOut = NULL;      
    }
    // And of course you need to write the tract 
    Write_NI_Network(netngr, "toy.network", NI_TEXT_MODE);
    // free netngr since it is no longer tucked into dset 
    NI_free_element(netngr); netngr = NULL;
               
    // cleanup for good manners 
    for(i=0;i<2; ++i) free(mv[i]); free(mv);
    for(i=0;i<2; ++i) free(labs[i]); free(labs);
    Free_Network(net); net = NULL;
         
    INFO_message(
    "All done. Demo graph dset is called toy.niml.dset."
    "graph dset with external network spec is toy.link.niml.dset\n"
    "Try:\n"
    "       suma -gdset toy.link.niml.dset\n\n"
    "* Open a new controller (ctrl+n), switch states ('>') to\n"
    "see graph dset in complementary forms.\n"
    "* Open 'Surface Controller' to colorize graph data, etc.\n"
    "Interacting with such data is possible, but not \n"
    "yet documented. This will come soon.\n"
    );   
    exit(0);
    }*/
      



int GetSizesOfTargets( int ***ROI_SIZES,
                       int **MAPROI, int *Dim, 
                       int ***mskd, 
                       int ***INDEX2, 
                       float **coorded, float minFA,
                       int N_nets, int *NROI,
                       int **INV_LABELS) {
   
   int i,j,k,idx,m;
   int ii,jj,kk;
   int val,gotbnd,gotbndsa;

   // for each vox ...
   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) 
            // ... which is in the masked region ...
            if(mskd[i][j][k]) {
               idx = INDEX2[i][j][k];
               // ... check each network for an ROI label ...
               for( m=0 ; m<N_nets ; m++ ) {
                  // ... and if found ...
                  val = MAPROI[idx][m];
                  
                  if( val > 0 ) {
                     // ... then the vol increases ...
                     ROI_SIZES[m][ val ][0]+=1; 
                     
                     gotbnd = 1;
                     gotbndsa = 1;
                     // ... and we check if it's a boundary ...
                     for( ii=i-1 ; ii<=i+1 ; ii++ )
                        for( jj=j-1 ; jj<=j+1 ; jj++ )
                           for( kk=k-1 ; kk<=k+1 ; kk++ )
                              // (inside FOV)
                              if( (ii>=0) && (ii<Dim[0]) && 
                                  (jj>=0) && (jj<Dim[1]) &&
                                  (kk>=0) && (kk<Dim[2])) {
                                 // ... like this, ...
                                 if( MAPROI[INDEX2[ii][jj][kk]][m] != val ) {
                                    if (gotbnd) {// only get one
                                       ROI_SIZES[m][ val ][1]+=1;
                                       gotbnd = 0;
                                    }
                                    // ... and even whether it is
                                    // useful enough to be near
                                    // high-FA material
                                    if ( coorded[INDEX2[ii][jj][kk]][0]>minFA ) {
                                       if (gotbndsa) {// only get one
                                          ROI_SIZES[m][ val ][2]+=1;
                                          gotbndsa = 0;
                                          break; // well, break at least 1/3 loops
                                       }
                                    }
                                 }
                              }
                  }
               }
            }

   RETURN(1);
}
   
