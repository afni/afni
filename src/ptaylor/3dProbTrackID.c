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
*/



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     
#include <3ddata.h>     
#include <time.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <DoTrackit.h>
#include <TrackIO.h>

#include "suma_suma.h"

void usage_ProbTrackID(int detail) 
{
	printf(
"  \n"
"  FACTID-based tractography code, from Taylor, Cho, Lin and Biswal (2012),\n"
"  part of FATCAT (Taylor & Saad, 2013) in AFNI.\n"
"  \n"
"  Estimate locations of WM associated with GM ROIs, particularly between\n"
"    pairs of GM in a network;  can process several networks in a given run.\n"
"\n"
"  Reads in DT-related data from, e.g., 3dDWItoDTI, and\n"
"    also uses results from 3dDWUncert for uncertainty measures.\n"
"\n\n"
"  + OUTPUTS, named using prefix; done per network of N_ROI ROIs, and \n"
"    having (N_ROI+1) subbricks:\n"
"    1) MAP/MASK file, AFNI format with \n"
"         0th brick containing the number of tracts being a mask of all\n"
"         voxels through which more than the threshold number of tracts\n"
"         passed through, traversing individual and all combinatorial pairs\n"
"         of ROIs; re. threshold, see `NmNsFr' in input description.\n"
"         i-th brick (i running from 1 to N_ROI) containing the voxels\n"
"         comprising WM ROIs.  Voxels connecting i-th and j-th GM ROIs have\n"
"         value 2^j, the values are summed if a given voxel is in multiple\n"
"         WM ROIs; voxels in tracts which just past through i-th WM ROI\n"
"         have value 2^i.  (NB: this kind of assumes that not more than\n"
"         ~15 GM ROIs will form a given network.)\n"
"    2) GRID file, simple statistics in matrix format (N_ROI by N_ROI per \n"
"         network).  In file, first there is the N_ROI in that network, and\n"
"         then a row of integers containing the network's ROI labels, and\n"
"         the following N_ROI by N_ROI matrices (all have diagonal entries,\n"
"         where diagonal is for tracts just through (i-1)th ROI alone):\n"
"         1- Numbers of tracts connecting ROIs (pairwise AND logic)\n"
"         2- Ratio of numbers of tracts connecting ROIS to total number\n"
"            of tracts found through all iters (prob not too meaningful)\n"
"         3- Mean of FA value in WM ROIs connecting GM-ROIs \n"
"         4- Associated stdev of FA value in WM ROIs connecting two GM-ROIs\n"
"         5- Mean of MD value in WM ROIs connecting GM-ROIs \n"
"         6- Associated stdev of MD value in WM ROIs connecting two GM-ROIs\n"
"         7- Mean of RD value in WM ROIs connecting GM-ROIs \n"
"         8- Associated stdev of RD value in WM ROIs connecting two GM-ROIs\n"
"         9- Mean of L1 value in WM ROIs connecting GM-ROIs \n"
"        10- Associated stdev of L1 value in WM ROIs connecting two GM-ROIs\n"
"        11- Numbers of voxels in WM ROI connecting GM-ROIs \n"
"\n\n"
"  + TRACTOGRAPHY OPTIONS via {-algopt [file of tractog options}: \n"
"     1) Probabilistic mode (default):\n"
"        MinFA    :FA cutoff value, lower bound\n"
"        MaxAng   :max angle (in deg) to propagate between vox\n"
"        MinL     :min physical length (in mm) of tracts to keep\n"
"        NmNsFr   :fractional threshold for number of tracts which must\n"
"                  pass through vox to be included in WM ROI (e.g., 0.05).\n"
"                  It is the frac of Nseed*Nmonte to provide threshold.\n"
"        Nseed    :number of seeds per vox, will be placed randomly\n"
"        Nmonte   :number of Monte Carlo iterations\n"
"        M        :number of grads used to scan (not used yet)\n"
"        bval     :non-b=0 value (mm^2/s), e.g., 1000 (not used yet)\n"
"\n"
"     2) Deterministic mode (`-det_net', below):\n"
"        MinFA       :FA cutoff value, lower bound\n"
"        MaxAng      :max angle (in deg) to propagate between vox\n"
"        MinL        :min physical length (in mm) of tracts to keep\n"
"        SeedPerV[0] :Number of seeds per vox in x direc\n"
"        SeedPerV[1] :Number of seeds per vox in y direc\n"
"        SeedPerV[2] :Number of seeds per vox in z direc\n"
"        M           :number of grads used to scan (not used yet)\n"
"        bval        :non-b=0 value (mm^2/s), e.g., 1000 (not used yet)\n"
"\n\n"
"  + RUNNING, need to provide:\n"
"    -netrois ROIS   :mask(s) of ROIs- can be many subbricks! The target\n"
"                     ROIs through which you want tracks to pass should have\n"
"                     index values >0, and now it is possible to have\n"
"                     NOT-ROIs, or anti-targets, which stop a propagating\n"
"                     track... in its tracks -- these are defined per network\n"
"                     (i.e., per subbrick) by voxels with <0.\n"
"    -prefix  PREFIX :output file name part\n"
"    -input   INPREF :basename of DTI volumes output by, e.g., 3dDWItoDT.\n"
"                     NB- following volumes are currently expected:\n"
"                     INPREF_V1, INPREF_MD, INPREF_L1, INPREF_FA,\n" 
"                     INPREF_V2 and INPREF_V3\n"
"    -algopt  ALGO   :ASCII file with eight numbers defining parameter \n"
"                     quantities just above (see list above); note the\n"
"                     slight difference whether running in 1) the default\n"
"                     probabilistic mode, or 2) the deterministic mode.\n"
"\n"
"    -uncert  U_FILE :uncertainty values [6 subbricks], obtainable from\n"
"                     3dDWUncert. Necessary in default probabilistic mode, or\n"
"                     if using `-det_net' + `-mini_prob', described below.\n"
"                     Note that whatever the values in the U_FILE, this\n"
"                     program asserts a minimum uncertainty of stdevs:\n"
"                     for FA, it is 0.015;\n"
"                     for delta e_i, it is 0.06rad (~3.4deg).\n"
"\n"
"    -det_net {OR|AND}:perform *deterministic* tractography, thus the ALGO\n"
"                     file should contain the relevant information described\n"
"                     for case 2 up above.  One can still enter network(s) of\n"
"                     ROIs, one per brik, choosing to keep tracks found with\n"
"                     either OR or AND logic: i.e., keeping either any track\n"
"                     found to go through at least one network ROI, or only\n"
"                     those tracks which join a pair of ROIs.\n"
"                     When using AND here, the `cut_at_rois' option will be\n"
"                     applied automatically (see below).\n"
"    -rec_orig       :record dataset origin in the header of the *.trk file.\n"
"                     As of Sept. 2012, TrackVis doesn't use this info so it\n"
"                     wasn't included, but if you might want to map your \n"
"                     *.trk file later, then use the switch as the datasets's\n"
"                     Origin is necessary info for the mapping (the default\n"
"                     image in TrackVis might not pop up in the center of the\n"
"                     viewing window, though, just be aware). NB: including\n"
"                     the origin might become default at some point in time.\n"
"    -mini_prob NUM  :will run a small number NUM of whole brain Monte Carlo\n"
"                     iterations perturbing relevant tensor values in accord\n"
"                     with their uncertainty values (hence, the need for also\n"
"                     using `-uncert' with this option). This might be useful\n"
"                     for giving a flavor of a broader range of connections\n"
"                     while still seeing estimated tracks themselves. NB: if\n"
"                     NUM is large, you might be *big* output track files.\n"
"\n"
"    -mask   MASK    :can include a brainmask within which to calculate \n"
"                     things. Otherwise, data should be masked already.\n"
"    -cut_at_rois    :for pairwise track connections, only keep voxels in\n"
"                     tracks between the ROIs they connect (i.e., cut off\n"
"                     tracks which run beyond ROIs). Even with this switch\n"
"                     on, for individual ROI stats and total tracking maps,\n"
"                     all voxels are kept.\n"
"    -extra_set SET  :if you want to use a non-FA derived definition for the\n"
"                     WM skeleton in which tracts run, you can input one, and\n"
"                     then the threshold in the ALGO file refers to values\n"
"                     in this map. (Then the uncertainty in FA measures does\n"
"                     not really matter during the Monte Carlo part.)\n"
"    -not_mask  !NB! :THIS OPTION REMOVED in favor of allowing NOT-regions\n"
"                     to be defined more easily in the `-netrois' input briks\n"
"                     themselves, by voxels having to have a `-1' value. \n"
"                     Surely much simpler?\n"
"\n"
"    -dump_rois TYPE :can output individual masks of ROI connections.\n"
"                     Options for TYPE are: {DUMP | AFNI | BOTH}. Using DUMP\n"
"                     gives a set of 4-column ASCII files, each formatted\n"
"                     like a 3dmaskdump data set; it can be reconstituted\n"
"                     using 3dUndump. Using AFNI gives a set of BRIK/HEAD\n"
"                     (byte) files in a directory called PREFIX; using BOTH\n"
"                     produces both formats of outputs.\n"
"    -lab_orig_rois  :if using `-dump_rois', then apply numerical labels of\n"
"                     original ROIs input to the output names.  This would\n"
"                     only matter if input ROI labels aren't consecutive and\n"
"                     starting one (e.g., if instead they were 1,2,3,5,8,..).\n"
"    -posteriori     :switch to have a bunch of individual files output, with\n"
"                     the value in each being the number of tracks per voxel\n"
"                     for that pair; works with '-dump_rois {AFNI | BOTH }',\n"
"                     where you get track-path maps instead of masks; makes\n"
"                     threshold for number of tracks between ROIs to keep to\n"
"                     be one automatically, regardless of setting in algopt.\n"
"\n"
"    -write_rois     :write out a file (PREFIX.roi.labs) of all the ROI \n" 
"                     (re-)labels, for example if the input ROIs aren't\n"
"                     simply consecutive and starting from 1. File has 3cols:\n"
"                       Input_ROI   Condensed_form_ROI   Power_of_2_label\n"
"    -write_opts     :write out all the option values into PREFIX.niml.opts.\n" 
"    -verb VERB      :verbosity level, default is 0.\n"
"\n\n"
"  + EXAMPLE:\n"
"      Download and install this archive:\n"
"         curl -O http://..../FACTID_draft.tar.gz \n"
"      tar xvzf FACTID_draft.tar.gz\n"
"      (requires output of 3dDWUncert program's sample command)\n"
"      3dProbTrackID \\\n"
"        -netrois TEST_FILES/DTI/masks_together+orig.BRIK \\\n"
"        -uncert TEST_FILES/DTI/o.UNCERT_UNC+orig.BRIK \\\n"
"        -prefix TEST_FILES/DTI/o.PROBTR \\\n"
"        -input TEST_FILES/DTI/DT \\\n"
"        -algopt TEST_FILES/ALGOPTS_PROB.dat\n"
"\n\n"
"  +NB: The integer labels of ROIs no longer have to be purely\n"
"       consecutive; this allows a bit more flexibility.  But in \n"
"       order to have the labels not get *too* huge or have blank \n"
"       output briks, the ROI labels get mapped as follows: \n"
"       Say you have 3 ROIs but the labels are {1,2,24}, for whatever \n"
"       reason; the output labels will condense this to treat it LIKE \n"
"       {1,2,3}-- thus, the labels stay as small as possible (which is a \n"
"       a legit concern since labels are 2^{ROI #}).\n"
"\n\n"
"  If you use this program, please reference the tractography algorithm:\n"
"    Taylor PA, Cho K-H, Lin C-P, Biswal BB (2012). Improving DTI\n"
"    Tractography by including Diagonal Tract Propagation. PLoS ONE\n"
"    7(9): e43415. \n"
"    and the introductory/description paper for the FATCAT toolbox:\n"
"    Taylor PA, Saad ZS (2013). FATCAT: (An Efficient) Functional And\n"
"    Tractographic Connectivity Analysis Toolbox. Brain Connectivity.\n\n");
	return;
}
		
		

int main(int argc, char *argv[]) {
  int i,j,k,m,n,ii,jj,kk,mm,gg,nn,hh,bb,cc,rr,ss,uu,vv;
	int iarg;
	int nmask1=0;
	int nmask2=0;
	THD_3dim_dataset *insetV1 = NULL,*insetV2 = NULL,*insetV3 = NULL;
	THD_3dim_dataset *insetL1 = NULL;
	THD_3dim_dataset *insetFA = NULL,*insetMD = NULL,*insetUC = NULL;
	THD_3dim_dataset *mset1=NULL; 

   // @#$ 
   char ***gdset_roi_names=NULL;
   int DEF_DTI = 1; // default mode, mainly for # of params
   int Npars = 3; // size of final paramgrid index 2+
   int Noutmat = 11; // number of output matrices in GRID file
   char **ParLab=NULL; // default: {"tNTR","fNTR","mFA","sFA","mMD","sMD","mRD","sRD","mL1","sL1","tNV"}
   SUMA_DSET *gset=NULL;
   float ***flat_matr=NULL;
   float *xyz=NULL;
   char OUT_gdset[300];
   NI_group *GDSET_netngrlink=NULL;
   char *NAME_gdset;

	THD_3dim_dataset *insetEXTRA=NULL; 
	char *in_EXTRA="name";//[300];
	int EXTRAFILE=0; // switch for whether other file is input as WM map

	short **antimask=NULL;
	int NOTMASK=0; // switch for whether other file is input as WM map
	int ALLorONE=0; // switch for whether NOTMASK==N_nets
	int NM=0; // switched off, unless mask puts one on
	int KEEP_GOING=1;
   int POST=0; // switch about having no min threshold,and dumping ROIs

	THD_3dim_dataset *MASK=NULL;
	char in_mask[300];
	int HAVE_MASK=0;
	int ***mskd; // define mask of where time series are nonzero

	char *prefix="tracky" ;
	char in_FA[300];
	char in_V1[300];
	char in_V2[300];
	char in_V3[300];
	char in_MD[300];
	char in_L1[300];

	char OUT_grid[300];
	char OUT_map[300];
	char OUT_mask[300];
	char OUT_rois[300];
	FILE *fin4, *fout1;

	// FACT algopts
	float MinFA=0.2,MaxAngDeg=45.0,MinL=20.0,NmNsFr=0.05;
	float MaxAng;
	int NmNsThr=0;
	int ArrMax=0;
	int ROIS_OUT=0;
	float unc_minfa_std=0.015, unc_minei_std=0.06; // min uncert stds.
	
	int Nvox=-1;   // tot number vox
	int Ndata=-1;
	int *Dim=NULL; //[3]={0,0,0}; // dim in each dir
	int Nseed=5,M=30,bval=1000;
	float **LocSeed=NULL; // fractional locations within voxel, 
	float Ledge[3]; // voxel edge lengths

	int **Tforw=NULL, **Tback=NULL;
	int **Ttot=NULL;
	float **flTforw=NULL, **flTback=NULL;
	float **coorded=NULL;
	float **copy_coorded=NULL; // copy will have perturbed info;
	float **UNC=NULL;
	int ***INDEX=NULL,***INDEX2=NULL;
	int **MAPROI=NULL; // store what ROIs are where, per data voxel
	int ****NETROI=NULL; // store connection info
	int len_forw, len_back; // int count of num of squares through
	float phys_forw[1], phys_back[1];
	int idx,idx2;

	int DUMP_TYPE=-1; // switch about whether to dump ascii/afni/both
	int DUMP_ORIG_LABS=0;

	int in[3]; // to pass to trackit
	float physin[3]; // also for trackit, physical loc, 
	int totlen; 
	float totlen_phys;
	int Numtract; 

   int READS_in;
	float READS_fl;
	// int end[2][3]; // don't need anymore ! @@##
	// int test_ind[2];
	int onoff[3];
	int BreakAddCont=0;

	char dset_or[4] = "RAI";
	char *voxel_order=NULL;//[4]="---";
	THD_3dim_dataset *dsetn=NULL;
	int *TV_switch=NULL;//[3] = {0,0,0};

	int ***Prob_grid=NULL;// NROIxNROI grid (NROI is num of ROIs)
	float ****Param_grid=NULL;// same size as Prob_Grid (extra dim for ROI stats)
	int *list_rois=NULL, *temp_list=NULL;
	int MAXNROI=0;
	int Nmonte=250;
	int N_nets=0;
	float w2,w3,thetval;
	float tempv[3];
	float tempvmagn;
	float testang;

	int *NROI=NULL,*INVROI=NULL;
	int **ROI_LABELS=NULL, **INV_LABELS=NULL; // allow labels to be non-consec
	long seed;   
	const gsl_rng_type * T=NULL;
	gsl_rng *r=NULL;
	NI_element *nel=NULL;
	int dump_opts=0;
	int ONLY_BT = 0; // don't cut off tracks at ROI endpts
	char *postfix[4]={"+orig.HEAD\0",".nii.gz\0",".nii\0","+tlrc.HEAD\0"};
	int  FOUND = -1;

   // @) this block of values for detnet options
   int DETNET = 0; // switch on detnet
	int SeedPerV[3]={2,2,2}; // default detnet seedopts
   TAYLOR_NETWORK **tnet=NULL;
   TAYLOR_BUNDLE ***tb=NULL;  // @))), for new IO
   int *N_bund=NULL; // @))) number of possible tri+diag bund/net
   int **b_tags=NULL; // @))) bundle tags per network
	TAYLOR_TRACT **tt=NULL;  
	char *mode = "NI_fast_binary";
   int *id=NULL;
	float **flTtot=NULL, **cutTot=NULL;
   int **cutTotI=NULL;
   int trL=0;
   char **prefix_det=NULL;
	char OUT_bin[300];
   FILE **fout0=NULL;
	tv_io_header header1 = {.id_string = "TRACK\0", 
				.origin = {0,0,0},   
				.n_scalars = 3,
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
	int RECORD_ORIG = 0; 
	float Orig[3] = {0.0,0.0,0.0};
   int LOG_TYPE=0; // default is OR logic
   int MINI_PROB_NM=1; // default: no mini monte carloing for det_net

   int vA,vA0,vB,vB0,vB1; // will be indices along tracks in NOT-ROI checking
   int TESTCOUNT=0;

   // @)) jump lesion searching stuff, to be used prob only with DETNET
   int JUMPLES=0; // switch to turn on prop check.
   float ***TROUT=NULL;

	// for random number generation
	srand(time(0));
	seed = time(NULL) ;
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc (T);
	gsl_rng_set (r, seed);
  
	mainENTRY("3dProbTrackID"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************
  
	//	INFO_message("version: UPSILON");

	// scan args 
	if (argc == 1) { usage_ProbTrackID(1); exit(0); }
	iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_ProbTrackID(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}

		if( strcmp(argv[iarg],"-verb") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-verb'") ;
			set_tract_verb(atoi(argv[iarg]));
			iarg++ ; continue ;
		}
	 
		if( strcmp(argv[iarg],"-write_opts") == 0) {
			dump_opts=1;
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-write_rois") == 0) {
			ROIS_OUT=1;
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-lab_orig_rois") == 0) {
			DUMP_ORIG_LABS=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-netrois") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-netrois'") ;
			mset1 = THD_open_dataset( argv[iarg] ) ;
			if( mset1 == NULL ) 
				ERROR_exit("Can't open netrois dataset '%s'", argv[iarg]) ;
			DSET_load(mset1) ; CHECK_LOAD_ERROR(mset1) ;
			nmask1 = DSET_NVOX(mset1) ;
			N_nets = DSET_NVALS(mset1) ;
      
			NROI = (int *)calloc(N_nets, sizeof(int)); 
			INVROI = (int *)calloc(N_nets, sizeof(int)); 

			if( (NROI == NULL) || (INVROI == NULL) ) {
				fprintf(stderr, "\n\n MemAlloc failure.\n\n");
				exit(122);
			}
      
			for( i=0 ; i<N_nets ; i++) {
				// NROI[i] = (int) THD_subbrick_max(mset1, i, 1);
				INVROI[i] = (int) THD_subbrick_max(mset1, i, 1);
				//if( NROI[i]>MAXNROI )
				//	MAXNROI = NROI[i];
			}
			
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
			bb = ViveLeRoi(mset1, ROI_LABELS, INV_LABELS, NROI, INVROI);
			if( bb != 1)
				ERROR_exit("Problem loading/assigning ROI labels");
			
			for( i=0 ; i<N_nets ; i++) {
				if( NROI[i]>MAXNROI )
					MAXNROI = NROI[i];
			}

			for( bb=0 ; bb<N_nets ; bb++)
				INFO_message("Number of ROIs in netw[%d]=%d",bb+1,NROI[bb]);
      
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-uncert") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-uncert'") ;
			insetUC = THD_open_dataset( argv[iarg] ) ;
			if( insetUC == NULL ) 
				ERROR_exit("Can't open uncert dataset '%s'",
							  argv[iarg]) ;
			DSET_load(insetUC) ; CHECK_LOAD_ERROR(insetUC);
			nmask2 = DSET_NVOX(insetUC);
			iarg++ ; continue ;
      }
    
		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
			iarg++ ; continue ;
		}
      
		if( strcmp(argv[iarg],"-input") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-input'");
			//sprintf(in_FA,"%s_FA+orig", argv[iarg]); 
			
			for( i=0 ; i<4 ; i++) {
				sprintf(in_FA,"%s_FA%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_FA)) {
					FOUND = i;
					break;
				}
			}
			insetFA = THD_open_dataset(in_FA) ;//argv[iarg] ) ;
			if( insetFA == NULL ) 
				ERROR_exit("Can't open dataset '%s':FA",in_FA);
			DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
			Nvox = DSET_NVOX(insetFA) ;

			Dim = (int *)calloc(3, sizeof(int)); 
			Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); 
			Dim[2] = DSET_NZ(insetFA); 
			Ledge[0] = fabs(DSET_DX(insetFA)); Ledge[1] = fabs(DSET_DY(insetFA)); 
			Ledge[2] = fabs(DSET_DZ(insetFA)); 
			Orig[0] = DSET_XORG(insetFA); Orig[1] = DSET_YORG(insetFA); // @)
			Orig[2] = DSET_ZORG(insetFA);

			// check tot num vox match (as proxy for dims...)
			if( (Nvox != nmask1) || ( (Nvox != nmask2) && !DETNET) )
				ERROR_exit("Input dataset does not match both mask volumes!");
        
			// this stores the original data file orientation for later use,
			// as well since we convert everything to RAI temporarily, as
			// described below
			voxel_order = (char *)calloc(4, sizeof(char)); 
			TV_switch = (int *)calloc(3, sizeof(int)); 
			voxel_order[0]=ORIENT_typestr[insetFA->daxes->xxorient][0];
			voxel_order[1]=ORIENT_typestr[insetFA->daxes->yyorient][0];
			voxel_order[2]=ORIENT_typestr[insetFA->daxes->zzorient][0];
			voxel_order[3]='\0';

			header1.voxel_order[0]=ORIENT_typestr[insetFA->daxes->xxorient][0]; // @)
			header1.voxel_order[1]=ORIENT_typestr[insetFA->daxes->yyorient][0];
			header1.voxel_order[2]=ORIENT_typestr[insetFA->daxes->zzorient][0];

			for( i=0 ; i<3 ; i++) {
           header1.dim[i] = Dim[i];
           header1.voxel_size[i] = Ledge[i];
           // will want this when outputting file later for TrackVis.
           TV_switch[i] = !(dset_or[i]==voxel_order[i]);
			}

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V1,"%s_V1%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V1)) {
					FOUND = i;
					break;
				}
			}
			insetV1 = THD_open_dataset(in_V1);
			if( insetV1 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V1",in_V1);
			DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;

			
			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V2,"%s_V2%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V2)) {
					FOUND = i;
					break;
				}
			}
			insetV2 = THD_open_dataset(in_V2);
			if( insetV2 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V2",in_V2);
			DSET_load(insetV2) ; CHECK_LOAD_ERROR(insetV2) ;

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_V3,"%s_V3%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_V3)) {
					FOUND = i;
					break;
				}
			}
			insetV3 = THD_open_dataset(in_V3);
			if( insetV3 == NULL ) 
				ERROR_exit("Can't open dataset '%s':V3",in_V3);
			DSET_load(insetV3) ; CHECK_LOAD_ERROR(insetV3) ;

			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_MD,"%s_MD%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_MD)) {
					FOUND = i;
					break;
				}
			}
			insetMD = THD_open_dataset(in_MD);
			if( insetMD == NULL ) 
				ERROR_exit("Can't open dataset '%s':MD",in_MD);
			DSET_load(insetMD) ; CHECK_LOAD_ERROR(insetMD) ;
			
			FOUND = -1;
			for( i=0 ; i<4 ; i++) {
				sprintf(in_L1,"%s_L1%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_L1)) {
					FOUND = i;
					break;
				}
			}
			insetL1 = THD_open_dataset(in_L1);
			if( insetL1 == NULL ) 
				ERROR_exit("Can't open dataset '%s':L1",in_L1);
			DSET_load(insetL1) ; CHECK_LOAD_ERROR(insetL1) ;

			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-algopt") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-algopt'");
      
			if (!(nel = ReadProbTractAlgOpts(argv[iarg]))) {
				ERROR_message("Failed to read options in %s\n", argv[iarg]);
				exit(19);
			}
			if (NI_getProbTractAlgOpts(nel, &MinFA, &MaxAngDeg, &MinL, 
												&NmNsFr,&Nseed,&Nmonte,&M,&bval)) {
				ERROR_message("Failed to get options");
				exit(1);
			}
			NI_free_element(nel); nel=NULL;
				
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-extra_set") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-extra_set'");
			EXTRAFILE = 1; // switch on
			in_EXTRA = argv[iarg];
			
			insetEXTRA = THD_open_dataset(in_EXTRA);
			if( (insetEXTRA == NULL ) )
				ERROR_exit("Can't open dataset '%s': for extra set.",in_EXTRA);
			DSET_load(insetEXTRA) ; CHECK_LOAD_ERROR(insetEXTRA) ;
			
			if( !((Dim[0] == DSET_NX(insetEXTRA)) && 
					(Dim[1] == DSET_NY(insetEXTRA)) &&
					(Dim[2] == DSET_NZ(insetEXTRA))))
				ERROR_exit("Dimensions of extra set '%s' don't match those of the DTI prop ones ('%s', etc.).",in_EXTRA, in_FA);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-mask") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-mask'");
			HAVE_MASK=1;
			
			sprintf(in_mask,"%s", argv[iarg]); 
			MASK = THD_open_dataset(in_mask) ;
			if( (MASK == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_mask);
			
			DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-cut_at_rois") == 0) {
			ONLY_BT=1;
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-posteriori") == 0) {
        POST=1;
        iarg++ ; continue ;
		} 

      if( strcmp(argv[iarg],"-det_net") == 0) { // @)
        iarg++ ; if( iarg >= argc ) 
                   ERROR_exit("Need argument after '-det_net'");

        INFO_message("ROI logic type is: %s",argv[iarg]);
        if( strcmp(argv[iarg],"AND") == 0 ) 
          LOG_TYPE = 1;
        else if( strcmp(argv[iarg],"OR") == 0 ) 
          LOG_TYPE = 0;
        else 
          ERROR_exit("Illegal after '-det_net': need 'OR' or 'AND'");

        DETNET=1;
        iarg++ ; continue ;
		}

      // can do a mini monte carlo if you want; for example, use 1
      // seed per ROI and then see more locs to tracks possibly; prob
      // don't want to make tooo large for visualization stuff.
      if( strcmp(argv[iarg],"-mini_prob") == 0) { // @)
        iarg++ ; if( iarg >= argc ) 
                   ERROR_exit("Need argument after '-mini_prob'");
        
        MINI_PROB_NM = atoi(argv[iarg]);

        iarg++ ; continue ;
		}

      /*    TURNED OFF FOR MOMENT, WORK IN PROGRESS
            if( strcmp(argv[iarg],"-jump_les") == 0) { // @)) prop check
            JUMPLES=1;
            iarg++ ; continue ;
            } */
      
      
      
		if( strcmp(argv[iarg],"-rec_orig") == 0) { // @)
			RECORD_ORIG=1;
			iarg++ ; continue ;
		}
      if( strcmp(argv[iarg],"-tract_out_mode") == 0) { // @)
        if( ++iarg >= argc ) 
          ERROR_exit("Need argument after '-tract_out_mode'") ;
        if (strcmp(argv[iarg], "NI_fast_binary") &&
            strcmp(argv[iarg], "NI_fast_text") &&
            strcmp(argv[iarg], "NI_slow_binary") &&
            strcmp(argv[iarg], "NI_slow_text") ) {
          ERROR_message("Bad value (%s) for -tract_out_mode",argv[iarg]);
				exit(1);
        }  
        mode = argv[iarg];
        iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-dump_rois") == 0) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-dump_rois'");

			if( strcmp(argv[iarg],"DUMP") == 0 ) 
				DUMP_TYPE = 1;
			else if( strcmp(argv[iarg],"AFNI") == 0 ) 
				DUMP_TYPE = 2;
			else if( strcmp(argv[iarg],"BOTH") == 0 )
				DUMP_TYPE = 3;
			else 
				ERROR_exit( "Illegal after '-dump_rois': "
                        "need 'DUMP', 'AFNI' or 'BOTH'.");

			iarg++ ; continue ;
		}

      if ( strcmp(argv[iarg],"-gdset_toy") == 0 ) {
         float x[3]={-40, 38, 4.7};
         float y[3]={-27, -34, 14.4};
         float z[3]={26, 41, 55};
            /* alternate coordinate specification */
         float xyz[9]={ x[0], y[0], z[0], x[1], y[1], z[1], x[2], y[2], z[2]};
         float **mv=NULL;
         int i,j,k=0;
         char **labs, *NameOut;
         SUMA_DSET *gset=NULL;
         TAYLOR_TRACT *tt=NULL;
         TAYLOR_BUNDLE *tb=NULL;
         TAYLOR_NETWORK *net=NULL;
         NI_group *netngrlink=NULL, *netngr=NULL;
         
         int onexyz = 1; /* set to 1 to specify coords by xyz triplets 
                            in demo. 0 to specify x,y, and z as separate
                            vectors */

         INFO_message("A demonstration for creating/writing graph dset\n"
                      "The vast majority of this content is for creating\n"
                      "dummy data.\n"
                      "Non dummy sections are marked with -->\n");

         /* Create 2 matrices and some matrix labels */
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
         
         /* --> Create a graph dataset out of these two matrices */
               gset = SUMA_FloatVec_to_GDSET(mv, 2, 9, "full", labs, 
                                             NULL, NULL, NULL);
         
         /* --> Now add the XYZ of each graph point, two ways are possible*/
               if (onexyz) { /* for when your ROI centroid coords 
                                and in one vector of XYZ triplets */
                  if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
                                                  xyz, NULL, NULL, NULL, 3))) {
                     ERROR_message("Failed to add node list");
                     exit(1);  
                  }                                     
               } else {
                  if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
                                                  x, y, z, NULL, 3))) {
                     ERROR_message("Failed to add node list");
                     exit(1);     
                  }
               }

         /* Create some dummy bundles representing edge 1-2=5, or 2-1=7*/
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
               /* put another track in */
               tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
               tt->id=78; tt->N_pts3=12; 
               tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
               tt->pts[0]=x[1]; tt->pts[1]=y[1]; tt->pts[2]=z[1];
               tt->pts[3]=23;   tt->pts[4]=35;   tt->pts[5]=42;
               tt->pts[6]=20;   tt->pts[7]=32;   tt->pts[8]=51;
               tt->pts[9]=x[2]; tt->pts[10]=y[2];tt->pts[11]=z[2];
               tb = AppCreateBundle(tb, 1, tt);
               tt = Free_Tracts(tt, 1);
               /* add it to network */
               net = AppAddBundleToNetwork(net, &tb, 5, 7, NULL);
               /* make another one for edge 0-1=1 and 1-0=3*/
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
               /* add bundle to network */
               net = AppAddBundleToNetwork(net, &tb, 1, 3, NULL);

         /* --> Now put network in graph dset */
               netngr = Network_2_NIgr(net, 1);
               NI_add_to_group(gset->ngr, netngr);

         /* --> Write the graph dataset */
               NameOut = SUMA_WriteDset_ns ("toy", gset, SUMA_ASCII_NIML, 1, 0);
               if (!NameOut && !SUMA_IS_DSET_STDXXX_FORMAT(SUMA_ASCII_NIML)) { 
                  ERROR_message("Failed to write dataset."); exit(1); 
               } else {
                  if (NameOut) SUMA_free(NameOut); NameOut = NULL;      
               }
         
         /* --> Now alternately you can leave the network outside of the 
                graph dataset and just put a link element to it           */
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
               /* And of course you need to write the tract */
               Write_NI_Network(netngr, "toy.network", NI_TEXT_MODE);
               /* free netngr since it is no longer tucked into dset */
               NI_free_element(netngr); netngr = NULL;
               
         /* cleanup for good manners */
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
      }
      
		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}

	if (iarg < 5) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}

   if(nmask2 && DETNET && (MINI_PROB_NM == 1) ){ // @)
     ERROR_message("Don't do -detnet AND -uncert together without -mini_prob.\n");
     exit(1);
   }

   if(!nmask2 && (MINI_PROB_NM != 1) ){ // @)
     ERROR_message("If using -mini_prob, then you need -uncert as well.\n");
     exit(1);
   }

   if(DETNET && LOG_TYPE && !ONLY_BT){
     INFO_message("With `-det_net AND', the `-cut_at_rois' option will be automically applied.");
     ONLY_BT=1;
   }

	if (dump_opts) {
		nel = NI_setProbTractAlgOpts(NULL, &MinFA, &MaxAngDeg, &MinL, 
											  &NmNsFr,&Nseed,&Nmonte, &M, &bval);
		WriteTractAlgOpts(prefix, nel);
		NI_free_element(nel); nel=NULL;
	}

	if(ROIS_OUT) {
		for( k=0 ; k<N_nets ; k++) { // each netw gets own file
			sprintf(OUT_rois,"%s_%03d.roi.labs",prefix,k+1);
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

   // @#$
   ParLab = (char **)calloc(Noutmat, sizeof(char *));
   if( (ParLab == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(121);
   }
   if(DEF_DTI && (Noutmat==11) ) { // default for DTI 
      ParLab[0] = strdup("tNT");
      ParLab[1] = strdup("fNT");
      ParLab[2] = strdup("mFA");
      ParLab[3] = strdup("sFA");
      ParLab[4] = strdup("mMD");
      ParLab[5] = strdup("sMD");
      ParLab[6] = strdup("mRD");
      ParLab[7] = strdup("sRD");
      ParLab[8] = strdup("mL1");
      ParLab[9] = strdup("sL1");
      ParLab[10] = strdup("tNV");

      flat_matr = (float ***) calloc( N_nets, sizeof(float **) );
      for ( i = 0 ; i < N_nets ; i++ ) 
         flat_matr[i] = (float **) calloc( Noutmat, sizeof(float *) );
      for ( i = 0 ; i < N_nets ; i++ ) 
         for ( j = 0 ; j < Noutmat ; j++ ) 
            flat_matr[i][j] = (float *) calloc( NROI[i]*NROI[i], sizeof(float));

	}

   gdset_roi_names = (char ***)calloc(N_nets, sizeof(char **));
	for (i=0; i< N_nets ; i++ ) {
      gdset_roi_names[i] = (char **)calloc(NROI[i], sizeof(char *));
      for (j=0; j<NROI[i]; ++j) {
         gdset_roi_names[i][j] = (char *)calloc(32, sizeof(char));
         snprintf(gdset_roi_names[i][j],31,"N%03d:R%d", i+1, ROI_LABELS[i][j]);
      }
   }
   
   // convert to cos of rad value for comparisons, instead of using acos()
	MaxAng = cos(CONV*MaxAngDeg); 

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
	if(RECORD_ORIG) { // @)
		for( i=0 ; i<3 ; i++)
			header1.origin[i] = Orig[i];
	}

   // @) IF DOING JUST DETNET TRACK
   // adjust what some params mean
   if (DETNET){ 

     if( N_nets > FOPEN_MAX)
       ERROR_message("You have more networks (%d) than the allowable number of open stream (%d)\n"
                    "allowed for your computer. You should divide up the networks below this max,\n"
                    "or spring for a better computer.\n",N_nets,FOPEN_MAX);
     //printf("FOPEN_MAX=%d\n",FOPEN_MAX);


     Nmonte = MINI_PROB_NM; // only doing 1 it

     if( (NmNsFr >= 1) && (Nseed >= 1) && (Nmonte >= 1) ) {
       SeedPerV[0] = (int) NmNsFr;
       SeedPerV[1] = (int) Nseed;
       SeedPerV[2] = (int) Nmonte;
     } 
     else // just have 2x2x2
       INFO_message("Perhaps you are using a *probabilistic* algopt file.\n  Since you are chose the deterministic switch,\n  going to use 8 evenly spaced seeds per vox.");
     
     Nseed = SeedPerV[0]*SeedPerV[1]*SeedPerV[2];
     
     LocSeed = calloc(Nseed,sizeof(LocSeed)); 
     for(i=0 ; i<Nseed ; i++) 
       LocSeed[i] = calloc(3,sizeof(float)); 
     
     if( (LocSeed == NULL) ){
       fprintf(stderr, "\n\n MemAlloc failure.\n\n");
       exit(123);
     }
     
     // regularly spaced seeds
     i = 0; 
     for( ii=0 ; ii<SeedPerV[0] ; ii++ ) 
       for( jj=0 ; jj<SeedPerV[1] ; jj++ ) 
         for( kk=0 ; kk<SeedPerV[2] ; kk++ ) {
           LocSeed[i][0] = (0.5 + (float) ii)/SeedPerV[0];
           LocSeed[i][1] = (0.5 + (float) jj)/SeedPerV[1];
           LocSeed[i][2] = (0.5 + (float) kk)/SeedPerV[2];
           i++;
         }
     
     NmNsThr = 1; // thresh of 1 for stats stuff

     // @) @)))
     //PT: This next line was commented out, but if you
     //    choose to reuse it, sizeof() should be for (TAYLOR_BUNDLE *)
     //tb = (TAYLOR_BUNDLE **)calloc(N_nets, sizeof(TAYLOR_BUNDLE)); 
     N_bund = (int *)calloc(N_nets, sizeof(int)); 
     if( N_bund == NULL) {
       fprintf(stderr, "\n\n MemAlloc failure.\n\n");
       exit(123);
     }
     if(LOG_TYPE) //just for AND logic
       for( i=0 ; i<N_nets ; i++)
         N_bund[i] = (NROI[i]*(NROI[i]+1))/2;
     else
       for( i=0 ; i<N_nets ; i++)
         N_bund[i] = (NROI[i]*(NROI[i]+1))/2;
     
     tb = (TAYLOR_BUNDLE ***) calloc( N_nets, sizeof(TAYLOR_BUNDLE **) );
     for ( i = 0 ; i < N_nets ; i++ ) // halftri+diag notation!!!
       tb[i] = (TAYLOR_BUNDLE **) calloc( N_bund[i], sizeof(TAYLOR_BUNDLE *));
     tt = (TAYLOR_TRACT **)calloc(N_nets, sizeof(TAYLOR_TRACT *)); 
     tnet = (TAYLOR_NETWORK **)calloc(N_nets, sizeof(TAYLOR_NETWORK *));
     id = (int *)calloc(N_nets, sizeof(int)); 
     
     b_tags = calloc(N_nets,sizeof(b_tags)); 
     for(i=0 ; i<N_nets ; i++) 
       b_tags[i] = calloc(N_bund[i],sizeof(int)); 

     flTtot = calloc(2*ArrMax,sizeof(flTtot)); 
     for(i=0 ; i<2*ArrMax ; i++) 
       flTtot[i] = calloc(3,sizeof(float)); 
     cutTot = calloc(2*ArrMax,sizeof(cutTot)); 
     for(i=0 ; i<2*ArrMax ; i++) 
       cutTot[i] = calloc(3,sizeof(float)); 
     cutTotI = calloc(2*ArrMax,sizeof(cutTotI)); 
     for(i=0 ; i<2*ArrMax ; i++) 
       cutTotI[i] = calloc(3,sizeof(float)); 

     prefix_det = calloc( N_nets,sizeof(prefix_det));  
     for(i=0 ; i<N_nets ; i++) 
       prefix_det[i] = calloc( 300,sizeof(char)); 

     fout0 = (FILE **)calloc(N_nets, sizeof(FILE)); 

     if(  (prefix_det == NULL) || (flTtot == NULL) || (cutTot == NULL) 
          || (tb == NULL) || (tt == NULL) || (id == NULL) 
          || (b_tags == NULL) || (tnet == NULL)
          || (fout0 == NULL) || (cutTotI == NULL) ){
       fprintf(stderr, "\n\n MemAlloc failure.\n\n");
       exit(123);
     }

     for( i=0 ; i<N_nets ; i++)
       if(tnet[i])
         fprintf(stderr,"\nHEY!!!! %d",i);

   }
   else {
     // Process the options a little 
     LocSeed = calloc(Nseed,sizeof(LocSeed)); 
     for(i=0 ; i<Nseed ; i++) 
       LocSeed[i] = calloc(3,sizeof(float)); 
     
     // initial value in this case 
     for( k=0 ; k<Nseed ; k++ ) 
       for( j=0 ; j<3 ; j++ ) 
         LocSeed[k][j] = rand()*1.0/RAND_MAX;
     
     // will take stats on voxels with number of tracts >=NmNsThr
     NmNsThr =  (int) floor(NmNsFr*Nseed*Nmonte); 
     // lower bound is 1, and also force to be 1 if posteriori is chosen
     if( (NmNsThr<1) || POST ) 
       NmNsThr=1;
     INFO_message("Effective Monte Iters:%d. Voxel threshold:%d.",
                  Nseed*Nmonte,NmNsThr);
   }



   if(POST){
     if(DUMP_TYPE==1) {
       INFO_message("You asked for '-dump_rois DUMP', but also chose\n"
                    "\t'-posteriori', so you will get binary mask DUMP,\n"
                    "\tas well as AFNI files of numbers of tracks/voxel.");
       DUMP_TYPE=3;
     }
     if(DUMP_TYPE== -1){
       INFO_message("You did NOT ask for individual dump of ROIs by using\n"
                    "\t'-dump_rois {option}' but then you DID choose\n"
                    "\t'-posteriori', so you will get a set of AFNI files\n"
                    "having numbers of tracks/voxel.");
       DUMP_TYPE=2;
     }
   }
   

	// have all be RAI for processing here
	if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
		dsetn = r_new_resam_dset(insetFA, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetFA); 
		insetFA=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(insetMD, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetMD); 
		insetMD=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(insetV1, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetV1); 
		insetV1=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(insetV2, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetV2); 
		insetV2=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(insetV3, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetV3); 
		insetV3=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(insetL1, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(insetL1); 
		insetL1=dsetn;
		dsetn=NULL;
    
		dsetn = r_new_resam_dset(mset1, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(mset1); 
		mset1=dsetn;
		dsetn=NULL;
    
      if(!DETNET || !(MINI_PROB_NM==1) ){
        dsetn = r_new_resam_dset(insetUC, NULL, 0.0, 0.0, 0.0,
                                 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
        DSET_delete(insetUC); 
        insetUC=dsetn;
        dsetn=NULL;
      }

		if(EXTRAFILE) {
			dsetn = r_new_resam_dset(insetEXTRA, NULL, 0.0, 0.0, 0.0,
											 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
			DSET_delete(insetEXTRA); 
			insetEXTRA=dsetn;
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
				if( ((HAVE_MASK==0) && (THD_get_voxel(insetL1,idx,0)>EPS_V) ) ||
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

	// 3 comp of V1 and FA for each data voxel
	coorded = calloc( (Ndata+1),sizeof(coorded)); // to have all ind be >=1
	for(i=0 ; i<=Ndata ; i++) 
		coorded[i] = calloc(4,sizeof(float)); 
	// copy for perturb
	copy_coorded = calloc( (Ndata+1),sizeof(copy_coorded)); 
	for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
		copy_coorded[i] = calloc(4,sizeof(float)); 
	UNC = calloc( (Ndata+1),sizeof(UNC)); 
	for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
		UNC[i] = calloc(6,sizeof(float)); 

	MAPROI = calloc( (Ndata+1),sizeof(MAPROI)); // to have all ind be >=1
	for(i=0 ; i<=Ndata ; i++) 
		MAPROI[i] = calloc(N_nets,sizeof(int)); 

	NETROI = (int ****) calloc( (Ndata+1), sizeof(int ***) );
	for ( i = 0 ; i<=Ndata ; i++ ) 
		NETROI[i] = (int ***) calloc( N_nets, sizeof(int **) );
	for ( i=0 ; i<=Ndata ; i++ ) 
		for ( j=0 ; j<N_nets ; j++ ) // jth net has NROI[j] rois
			NETROI[i][j] = (int **) calloc( NROI[j], sizeof(int *) );
	for ( i=0 ; i<=Ndata ; i++ ) 
		for ( j=0 ; j<N_nets ; j++ ) 
			for ( k=0 ; k<NROI[j] ; k++ ) 
				NETROI[i][j][k] = (int *) calloc( NROI[j], sizeof(int) );

	if( (coorded == NULL) || (copy_coorded == NULL) || (UNC == NULL) 
		 || (NETROI == NULL) || (MAPROI == NULL)) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
  
	if(NOTMASK>0){
		antimask = calloc( (Ndata+1),sizeof(antimask)); 
		for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
			antimask[i] = calloc(NOTMASK,sizeof(short));
 
		if( (antimask == NULL) ) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(127);
		}
	}

	Prob_grid = (int ***) calloc( N_nets, sizeof(int **));
	for ( i = 0 ; i < N_nets ; i++ ) 
		Prob_grid[i] = (int **) calloc( (NROI[i]), sizeof(int *));
	for ( i = 0 ; i < N_nets ; i++ ) 
		for ( j = 0 ; j < (NROI[i]) ; j++ ) 
			Prob_grid[i][j] = (int *) calloc( (NROI[i]), sizeof(int));

	Param_grid = (float ****) calloc( N_nets, sizeof(float ***));
	for ( i = 0 ; i < N_nets ; i++ ) 
		Param_grid[i] = (float ***) calloc( (NROI[i]), sizeof(float **));
	for ( i = 0 ; i < N_nets ; i++ ) 
		for ( j = 0 ; j < (NROI[i]) ; j++ ) 
			Param_grid[i][j] = (float **) calloc( (NROI[i]), sizeof(float *));
	for ( i = 0 ; i < N_nets ; i++ ) 
		for ( j = 0 ; j < (NROI[i]) ; j++ ) 
			for ( k = 0 ; k < (NROI[i]) ; k++ )// mu and std of FA,MD,RD,L1; count
				Param_grid[i][j][k] = (float *) calloc( 9, sizeof(float));

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
	if(  (flTback == NULL) || (Tforw == NULL) || (Tback == NULL) 
		  || (flTforw == NULL) || (temp_list == NULL) || (Ttot == NULL) 
		  || (list_rois == NULL) || (Param_grid == NULL)
		  || (Prob_grid == NULL) ) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(12);
	}

   // slightly stricter now, because TrackItP includes extra endpoint
   // on each half tract tested
   ArrMax-= 1;

   // @))
   if(JUMPLES){

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
   
	// set up eigvecs in 3D coord sys,
	// mark off where ROIs are and keep index handy
	// also make uncert matrix, with min values of del ang and del FA
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) 
				if(mskd[i][j][k]) {
					idx = INDEX2[i][j][k];
					for( m=0 ; m<3 ; m++ ) 
						coorded[idx][m]=copy_coorded[idx][m]=
							THD_get_voxel(insetV1,INDEX[i][j][k],m);
					coorded[idx][3]=copy_coorded[idx][3]=
						THD_get_voxel(insetFA,INDEX[i][j][k],0); 
     
					// all from data set
					for( m=0 ; m<6 ; m++ ) 
						UNC[idx][m] = THD_get_voxel(insetUC,INDEX[i][j][k],m);
					// then check min stds.
					if (UNC[idx][1]<unc_minei_std)
						UNC[idx][1] = unc_minei_std;
					if (UNC[idx][3]<unc_minei_std)
						UNC[idx][3] = unc_minei_std;
					if (UNC[idx][5]<unc_minfa_std)
						UNC[idx][5] = unc_minfa_std;
					
					// apparently, some |V1| != 1... gotta fix
					tempvmagn = sqrt(copy_coorded[idx][0]*copy_coorded[idx][0]+
										  copy_coorded[idx][1]*copy_coorded[idx][1]+
										  copy_coorded[idx][2]*copy_coorded[idx][2]);
					if(tempvmagn<0.99) 
						for( m=0 ; m<3 ; m++ ) {
							copy_coorded[idx][m]/=tempvmagn;
							coorded[idx][m]/=tempvmagn;
						}
     
					for( m=0 ; m<N_nets ; m++ ) {
						// allow indentification by index --
						// now, since we allow non-consecutive ROI labels,
						// just use the compacted list numbers via INV_LABELS
						if( THD_get_voxel(mset1, INDEX[i][j][k], m)>0.5 )
							MAPROI[idx][m] = INV_LABELS[m][(int) THD_get_voxel(mset1,INDEX[i][j][k],m)];
						else if( THD_get_voxel(mset1, INDEX[i][j][k], m)<-0.5 )   // silly, is zero anyways... can use this to set to neg mask
							MAPROI[idx][m] = -1;
       
						// counter for number of kept tracks passing through
						for( mm=0 ; mm<NROI[m] ; mm++ )
							for( rr=0 ; rr<NROI[m] ; rr++ )
								NETROI[idx][m][mm][rr] = 0;
					}
				}
  
	
	// can free uncert dsets
   if(nmask2){
     DSET_delete(insetUC);
     free(insetUC);
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
     for( hh=0 ; hh<N_nets ; hh++) {
       for( i=0 ; i<N_bund[hh] ; i++)
         tb[hh][i] = AppCreateBundle(NULL, 0, NULL); // start bundle
       
       sprintf(OUT_bin,"%s_%03d.trk",prefix,hh+1);
       if( (fout0[hh] = fopen(OUT_bin, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.",OUT_bin);
         exit(16);
       }
       // all outputs have same header
       fwrite(&header1,sizeof(tv_io_header),1,fout0[hh]); 
     }
     
     if (get_tract_verb()) {
       INFO_message("Begin tracking...");
     }
   }
   
	for (gg=0 ; gg<Nmonte ; gg++) {
     
     
     if( gg>0) {// first time through is no change
        // relative location of each seed within voxel for this iter
        for( k=0 ; k<Nseed ; k++ ) // @) had just minorly rearr. here
          for( j=0 ; j<3 ; j++ ) 
            LocSeed[k][j] = rand()*1.0/RAND_MAX;


        for( k=0 ; k<Dim[2] ; k++ ) 
          for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
              // only do region in brain mask
						if(mskd[i][j][k]) {
							idx = INDEX2[i][j][k];
							
							// these are weights determined by rotation angle,
							// (prob. determined by jackknifing with 3dDWUncert)
							// each tips in the +/- direc toward/away from each evec 
							// by averaging and that's why tan of angle is taken
							//@@@
							thetval = pow(UNC[idx][0],2) + pow(UNC[idx][1],2); 
							testang = gsl_ran_gaussian_ziggurat(r,1.0)*sqrt(thetval);
							w2 = tan(testang); 

							thetval = pow(UNC[idx][2],2) + pow(UNC[idx][3],2);
							testang = gsl_ran_gaussian_ziggurat(r,1.0)*sqrt(thetval);
							w3 = tan(testang);
							for( m=0 ; m<3 ; m++)
								tempv[m] = coorded[idx][m] + 
									w2*THD_get_voxel(insetV2,INDEX[i][j][k],m) + 
									w3*THD_get_voxel(insetV3,INDEX[i][j][k],m);
							tempvmagn = sqrt(tempv[0]*tempv[0]+
												  tempv[1]*tempv[1]+tempv[2]*tempv[2]);
              
							for( m=0 ; m<3 ; m++)
								tempv[m]/= tempvmagn;
							for( m=0 ; m<3 ; m++)
								copy_coorded[idx][m] = tempv[m];
              
							copy_coorded[idx][3] = 
								THD_get_voxel(insetFA,INDEX[i][j][k],0) + UNC[idx][4] +
								( UNC[idx][5] * gsl_ran_gaussian_ziggurat(r,1.0) );
							
							if(copy_coorded[idx][3] <0)
								copy_coorded[idx][3] =0.;
							if(copy_coorded[idx][3] >1)
								copy_coorded[idx][3] =1.;
						}
						else
							for( m=0 ; m<4 ; m++)
								copy_coorded[idx][m] = 0;
					}

      }

		// this is where we start the tracking for a given data set
		// start of Monte Carlo loop
		for( k=0 ; k<Dim[2] ; k++ ) 
        for( j=0 ; j<Dim[1] ; j++ ) 
          for( i=0 ; i<Dim[0] ; i++ ) 
            if( (EXTRAFILE==0 && (copy_coorded[INDEX2[i][j][k]][3]>=MinFA))
						 || (EXTRAFILE && 
							  (THD_get_voxel(insetEXTRA, INDEX[i][j][k], 0)>=MinFA)) )
						for( kk=0 ; kk<Nseed ; kk++ ) {
	
	in[0] = i;
	in[1] = j;
	in[2] = k;

	for( jj=0 ; jj<3 ; jj++ ) 
		physin[jj] = ((float) in[jj]+LocSeed[kk][jj])*Ledge[jj];

	len_forw = TrackItP_NEW(copy_coorded, in, physin, Ledge, Dim, 
                           MinFA, MaxAng, ArrMax, Tforw, 
                           flTforw, 1, phys_forw,INDEX2);

	in[0] = i; // reset, because it's changed in TrackIt func
	in[1] = j;
	in[2] = k;

	for( jj=0 ; jj<3 ; jj++ ) 
		physin[jj] = ((float) in[jj]+LocSeed[kk][jj])*Ledge[jj];

	len_back = TrackItP_NEW(copy_coorded, in, physin, Ledge, Dim, 
                           MinFA, MaxAng, ArrMax, Tback, 
                           flTback, -1, phys_back,INDEX2);

	totlen = len_forw+len_back-1; // b/c of overlap of starts; includes 2ends
	totlen_phys = phys_forw[0] + phys_back[0];

	if( totlen_phys >= MinL ) {
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
      //printf("%d\t%f\t\t%d\t%d\n",totlen,totlen_phys,Ttot[0][0],Ttot[totlen-1][0]);


      if(DETNET){ // @) for now, have sep mirroring option to above
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
      }
      // at this point now, both 0- and trL-th index have `test index'
      // locations in them; these are not `tract' locs themselves, but
      // voxels to check in terms of connections.
      // if the locations are `bad'-- either tract went back on itself,
      // or tried to walk out of brain data set array, then a -1 is
      // stored there, so we check against that before looking at that
      // index.
      
		for( hh=0 ; hh<N_nets ; hh++) { ////////////// ##@@ start of new work
         // allow checking for NOT masks, which map MAPROI[][]=-1
         // if running through a not mask, split tract up
         // walk through once, all the way, per network

         // initialize for this network.
         // first, check if initial ends are bad--
         // would have negative value from TrackItP only if 
         // some badness had happened.

         vB = ( Ttot[0][0]<0 ) ? -1 : -2;
         vB1 = 0; 

         while( vB+vB1 < vB0  ){ // go until hitting a NOT or penult vox
            // clear list of ROIs
            for( n=0 ; n<=MAXNROI ; n++)
               list_rois[n] = temp_list[n] = 0;
            
            // literal start and finishes
            vA0 = vB+2; // for `checking' purposes
            // this starts at either 1 or where we ended+2
            vA = ( vA0==0 ) ? 1 : vA0 ; // for writing/out purposes
            for( n=vA0 ; n<=vB0 ; n++) {
               rr = INDEX2[Ttot[n][0]][Ttot[n][1]][Ttot[n][2]];
               if( MAPROI[rr][hh]>0 )
                  list_rois[MAPROI[rr][hh]]=1;
               else if( MAPROI[rr][hh]<0 ) 
                  break;
            }

            // keep track of where'd we gotten, as `last good voxel'
            // whether we made it all the way through track, or ran
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
				// first, write shorter list of which ones were hit
				m = 0;
				for( n=1 ; n<=NROI[hh] ; n++)
					if(list_rois[n]>0 ) {
						// values stored are 1...NROI -> 0...NROI-1
						// keep track of which was hit
						temp_list[m] = n-1; 
						m = m+1;
					}

				// let's keep track of where tracts connecting 
				// regions go.
				// we'll keep stats on individ ROI tracks
				if( m>0) {
					if( (ONLY_BT==0) || (m==1) ) {

                  trL = 0; // @) counter of len of flTtot
                  for( mm=vA ; mm<=vB ; mm++) { // @@@
                                       
                     // @) keep both float loc and ind
                     if(DETNET){
                        for( uu=0 ; uu<3 ; uu++) {
                           cutTot[trL][uu] = flTtot[mm][uu];
                           cutTotI[trL][uu] = Ttot[mm][uu];
                        }
                        trL++;
                     }
                    
                     rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];// @@@
                     for( bb=0 ; bb<m ; bb++)
                        for( cc=0 ; cc<m ; cc++) { // only individual, or keep all
                           NETROI[rr][hh][temp_list[bb]][temp_list[cc]]+=1;
                           if(NETROI[rr][hh][temp_list[bb]][temp_list[cc]]==NmNsThr) 
                              ss=ScoreTrackGrid(Param_grid,
                                                INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
                                                hh, temp_list[cc],temp_list[bb], 
                                                insetFA,insetMD,insetL1);
                        }
                     //	}
                  }// @@@
                 
                  if(DETNET && (trL>0) && !LOG_TYPE){
                     // @) if created, it will be kept
                     tt[hh] = Create_Tract_NEW(0,trL-1, cutTot,id[hh], insetFA); ++id[hh]; 
                     tb[hh][0] = AppCreateBundle(tb[hh][0], 1, tt[hh]); 
                     tt[hh] = Free_Tracts(tt[hh], 1);
                   
                     ss = SimpleWriteDetNetTr(fout0[hh], INDEX, insetFA,
                                              insetMD, insetL1,
                                              cutTot, cutTotI, trL,
                                              TV_switch, Dim, Ledge);
                  }
                 
					} // end of 'if only_bt or m==1'
					else{
						// first do diagonal/individual ones, because now we
						// have options for the pairwise connectors

						// DIAGONAL
						for( mm=vA ; mm<=vB ; mm++) {
                        rr = INDEX2[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]];
                        for( bb=0 ; bb<m ; bb++) {
                           vv = temp_list[bb];
                           NETROI[rr][hh][vv][vv]+=1;
                           if(NETROI[rr][hh][vv][vv]==NmNsThr)
                              ss=ScoreTrackGrid(Param_grid,
                                                INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
                                                hh, vv,vv, 
                                                insetFA,insetMD,insetL1);
                        }
                        //}
						}
                  
						// CONNECTORS: walk through mult times
						
						// just do unique connectors (we know that m>=2 here...)
						for( bb=0 ; bb<m ; bb++)
							for( cc=bb+1 ; cc<m ; cc++) {
								// 2 switches for finding ROI, and 1 for current 'FIND'
								onoff[0]=0; onoff[1]=0; onoff[2]=0;
								BreakAddCont=0;
                        
                        trL = 0; //counter of len of flTtot
								// now walk through each vox, keep testing and
								// evaluating at each step
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
										if(onoff[2])
											BreakAddCont=1; // still in last ROI
										else{
											BreakAddCont=-1; // done
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

                              // @) keep both float loc and ind
                              if(DETNET){
                                 for( uu=0 ; uu<3 ; uu++) {
                                    cutTot[trL][uu] = flTtot[mm][uu];
                                    cutTotI[trL][uu] = Ttot[mm][uu];
                                 }
                                 trL++;
                              }

										// get both sides of param_grid, b/c just testing one,
										// and param_grid is symm
										NETROI[rr][hh][temp_list[bb]][temp_list[cc]]+=1;
										NETROI[rr][hh][temp_list[cc]][temp_list[bb]]+=1;
										if(NETROI[rr][hh][temp_list[bb]][temp_list[cc]]==NmNsThr) {
											ss=ScoreTrackGrid(Param_grid,
																	INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
																	hh, temp_list[cc],temp_list[bb], 
																	insetFA,insetMD,insetL1);
											ss=ScoreTrackGrid(Param_grid,
																	INDEX[Ttot[mm][0]][Ttot[mm][1]][Ttot[mm][2]],
																	hh, temp_list[bb],temp_list[cc], 
																	insetFA,insetMD,insetL1);
										}
									}
									else if(BreakAddCont==-1) {// done
										break;
									}
									else {// unnec cond...
										continue;
									}
									//}
								} // end of mm

                        if(DETNET && (trL>0) && LOG_TYPE){
                          int lll;
                          // @) if created, it will be kept
                          tt[hh] = Create_Tract_NEW(0,trL-1, cutTot,id[hh], insetFA); ++id[hh]; 
                          lll = temp_list[cc]+temp_list[bb]*NROI[hh]; // sq matr coor
                          lll -= (temp_list[bb]*(temp_list[bb]+1))/2; // fix for tridiag.
                          //if(lll>N_bund[hh])
                          //fprintf(stderr, "  tb[%d][%d]=%p\n",hh, lll, tb[hh][lll]);
                          tb[hh][lll] = AppCreateBundle(tb[hh][lll], 1, tt[hh]); 
                          tt[hh] = Free_Tracts(tt[hh], 1);
                          
                          ss = SimpleWriteDetNetTr(fout0[hh], INDEX, insetFA,
                                                   insetMD, insetL1,
                                                   cutTot, cutTotI, trL,
                                                   TV_switch, Dim, Ledge);
                        }
                        
							} // end of cc
					} // end of an else
				
			
               //will just have be symm
               // this will fill in UHT part of matrix 
               // store as values in range 1...NROI
               for( mm=0 ; mm<m ; mm++)
                  for( nn=0 ; nn<m ; nn++) { 
                     Prob_grid[hh][ temp_list[mm] ][ temp_list[nn] ]+= 1;
                  }
            }// end of 'if m>0'
			}
		}
	}
}
	} // end of Monte Carlo loop
	
   if(DETNET){

     for( i=0 ; i<N_nets ; i++){
       fclose(fout0[i]); // !important to do...
       k=0;
       for( j=0 ; j<N_bund[i] ; j++)
         k+= tb[i][j]->N_tracts;
       INFO_message("Done tracking, net[%d] has %d tracks.",i+1,k);
       }

     if (get_tract_verb()) {
       for( i=0 ; i<N_nets ; i++)
         for( j=0 ; j<N_bund[i] ; j++){
           INFO_message("Done tracking, net[%d], bund[%d] has %d tracks.",
                        i+1,j+1, tb[i][j]->N_tracts);
           Show_Taylor_Bundle(tb[i][j], NULL, 3);
         }
     }
     
     // @#$
     //     for( i=0 ; i<N_nets ; i++){
     //for (j=0; j<N_bund[i]; j++) {
     //  tnet[i] = AppAddBundleToNetwork(tnet[i], &(tb[i][j]), 
     //                                  b_tags[i][j], -1, insetFA); 
     for( i=0 ; i<N_nets ; i++){
        ii = 0;
        for (j=0; j<NROI[i]; j++) 
           for (k=j; k<NROI[i]; k++) {
              jj = j*NROI[i] + k;
              kk = j + k*NROI[i];
              tnet[i] = AppAddBundleToNetwork(tnet[i], &(tb[i][ii]), 
                                              jj,kk, insetFA); 
              ii+=1;
        }
     }
     
     for( i=0 ; i<N_nets ; i++){
       sprintf(prefix_det[i],"%s_%03d",prefix,i+1); 
       
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
			for( i=0 ; i<NROI[k] ; i++ ) 
				for( j=0 ; j<NROI[k] ; j++ ) 
					if(Param_grid[k][i][j][8]>1.5) // need at least 2 vox per ROI
						for( m=0 ; m<4 ; m++) {
							// means
							Param_grid[k][i][j][2*m]/= Param_grid[k][i][j][8];
							// stdevs
							Param_grid[k][i][j][2*m+1]-= 
								Param_grid[k][i][j][8]*pow(Param_grid[k][i][j][2*m],2);
							Param_grid[k][i][j][2*m+1]/= Param_grid[k][i][j][8]-1;
							Param_grid[k][i][j][2*m+1] = sqrt(Param_grid[k][i][j][2*m+1]);
						}
					else {
						for( m=0 ; m<4 ; m++) {
							Param_grid[k][i][j][2*m]=0.;
							Param_grid[k][i][j][2*m+1]=0.;
						}
						Prob_grid[k][i][j]= 0.;
					}
		
		for( k=0 ; k<N_nets ; k++) { // each netw gets own file
			
			// print out prob grid
			sprintf(OUT_grid,"%s_%03d.grid",prefix,k+1);
			if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
				fprintf(stderr, "Error opening file %s.",OUT_grid);
				exit(19);
			}
    
			fprintf(fout1,"%d\n\n",NROI[k]); // Num of ROIs
         for( i=1 ; i<NROI[k] ; i++ ) // labels of ROIs
           fprintf(fout1,"%d\t",ROI_LABELS[k][i]); // because at =NROI, -> \n
         fprintf(fout1,"%d\n\n",ROI_LABELS[k][i]);
			for( i=0 ; i<NROI[k] ; i++ ) {
				for( j=0 ; j<NROI[k]-1 ; j++ ) // b/c we put '\n' after last one.
					fprintf(fout1,"%d\t",Prob_grid[k][i][j]);
				fprintf(fout1,"%d\n",Prob_grid[k][i][j]);
			}
			fprintf(fout1,"\n");    
      
			for( i=0 ; i<NROI[k] ; i++ ) {
				for( j=0 ; j<NROI[k]-1 ; j++ ) 
					fprintf(fout1,"%e\t",Prob_grid[k][i][j]*1.0/Numtract);
				fprintf(fout1,"%e\n",Prob_grid[k][i][j]*1.0/Numtract);
			}
			fprintf(fout1,"\n");    

			for( m=0 ; m<9 ; m++) {
				for( i=0 ; i<NROI[k] ; i++ ) {
					for( j=0 ; j<NROI[k]-1 ; j++ ) 
						fprintf(fout1,"%e\t",Param_grid[k][i][j][m]);
					fprintf(fout1,"%e\n",Param_grid[k][i][j][m]);
				}
				fprintf(fout1,"\n");    
			}
			fclose(fout1);

         // @#$ recapitulate *.grid files
         if( DEF_DTI) {
            for( i=0 ; i<NROI[k] ; i++ ) 
               for( j=0 ; j<NROI[k] ; j++ ) {
                  flat_matr[k][0][i*NROI[k]+j] = Prob_grid[k][i][j];
                  flat_matr[k][1][i*NROI[k]+j] = Prob_grid[k][i][j]*1.0/Numtract;
                  for( m=0 ; m<9 ; m++) 
                     flat_matr[k][2+m][i*NROI[k]+j] = Param_grid[k][i][j][m];
               }
            gset = SUMA_FloatVec_to_GDSET(flat_matr[k], Noutmat, NROI[k]*NROI[k], 
                                          "full", ParLab, 
                                          NULL, NULL, NULL);

            if( xyz = THD_roi_cmass(mset1, k, ROI_LABELS[k]+1, NROI[k]) ) {
               if (!(SUMA_AddGDsetNodeListElement(gset, NULL,
                                                  xyz, NULL, NULL, 
                                               gdset_roi_names[k], NROI[k]))) {
                  ERROR_message("Failed to add node list");
                  exit(1);  
               }
               free(xyz);
            } 
            else {
               ERROR_message("Failed in THD_roi_cmass"); exit(1);
            }

            sprintf(OUT_gdset,"%s_%03d",prefix,k+1);
            GDSET_netngrlink = 
               Network_link(SUMA_FnameGet(OUT_gdset, "f",NULL));
            NI_add_to_group(gset->ngr, GDSET_netngrlink);
            NAME_gdset = SUMA_WriteDset_ns (OUT_gdset,
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
		for( k=0 ; k<N_nets ; k++)
			for( i=0 ; i<=Ndata ; i++ ) 
				for( j=0 ; j<NROI[k] ; j++ ) 
					for( hh=0 ; hh<NROI[k] ; hh++ ) 
						if( NETROI[i][k][j][hh] < NmNsThr)
							NETROI[i][k][j][hh]=0;
		
		
		// output AFNI files mapping WM		
		INFO_message("Writing output. NB: it will be %s.",voxel_order);
		i = WriteBasicProbFiles(N_nets, Ndata, Nvox, prefix, 
										insetFA,TV_switch,voxel_order,
										NROI, NETROI,mskd,INDEX2,Dim,
										dsetn,argc,argv,ROI_LABELS);
		if(DUMP_TYPE>=0)
			i = WriteIndivProbFiles(N_nets,Ndata,Nvox,Prob_grid,
											prefix,insetFA,
											TV_switch,voxel_order,NROI,
											NETROI,mskd,INDEX2,Dim,
											dsetn,argc,argv,
											Param_grid,DUMP_TYPE,
											DUMP_ORIG_LABS,ROI_LABELS,POST);
		
	

      //INFO_message("Brainwide total number of tracts found = %d",Numtract);
	}
	else{
		INFO_message(" No Tracts Found!!!");
    
		sprintf(OUT_map,"%s.pmap",prefix);
		if( (fout1 = fopen(OUT_map, "w")) == NULL) {
			fprintf(stderr, "Error opening file %s.",OUT_map);
			exit(19);
		}
    
		fprintf(fout1,"0!\n");    
		fclose(fout1);

		sprintf(OUT_grid,"%s.grid",prefix);
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
	
   for( i=0 ; i<Noutmat ; i++)  free(ParLab[i]);
   free(ParLab);
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
   
	DSET_delete(insetFA);
	DSET_delete(insetMD);
	DSET_delete(insetL1);
	DSET_delete(insetV1);
	DSET_delete(insetV2);
	DSET_delete(insetV3);
	DSET_delete(mset1);
  	DSET_delete(insetEXTRA);

	free(prefix);
	free(insetV1);
	free(insetV2);
	free(insetV3);
	free(insetL1);
	free(insetFA);
  	free(insetMD);
	free(mset1);
  	free(insetEXTRA);

   DSET_delete(MASK);
   free(MASK);

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

   if(DETNET){ // @) freeing
     for( i=0 ; i<2*ArrMax ; i++) {
       free(flTtot[i]);
       free(cutTot[i]);
       free(cutTotI[i]);
     }
     free(flTtot);
     free(cutTot);
     free(cutTotI);
     
     for( i=0 ; i<N_nets ; i++) 
       for ( j=0 ; j<N_bund[i] ; j++ ) // halftri+diag notation!!!
         free(tb[i][j]);
     for( i=0 ; i<N_nets ; i++) {
       free(tb[i]);
       free(tt[i]);
       free(tnet[i]);
       free(b_tags[i]);
       //free(fout0[i]);
     }
     free(id);
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



	for( i=0 ; i<Nseed ; i++) 
		free(LocSeed[i]);
	free(LocSeed);
  
	for( k=0 ; k<=Ndata ; k++) 
		for( m=0 ; m<N_nets ; m++) 
			for( i=0 ; i<NROI[m] ; i++) 
				free(NETROI[k][m][i]);
	for( k=0 ; k<=Ndata ; k++) 
		for( m=0 ; m<N_nets ; m++) {
			free(NETROI[k][m]);
		}
	for( k=0 ; k<=Ndata ; k++) {
		free(coorded[k]);
		free(copy_coorded[k]);
		free(NETROI[k]);
		free(MAPROI[k]);
		free(UNC[k]);
	}
	free(coorded);
	free(copy_coorded);
	free(NETROI);
	free(MAPROI);
	free(UNC);

	if(NOTMASK>0){
		for( k=0 ; k<=Ndata ; k++) 
			free(antimask[k]);
		free(antimask);
	}

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

	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			free(INDEX2[i][j]);
	for( i=0 ; i<Dim[0] ; i++) 
		free(INDEX2[i]);
	free(INDEX2);
	
	for( i=0 ; i<N_nets ; i++) 
		for( j=0 ; j<NROI[i] ; j++) 
			for( k=0 ; k<NROI[i] ; k++) 
            free(Param_grid[i][j][k]);
	for( i=0 ; i<N_nets ; i++) 
		for( j=0 ; j<NROI[i] ; j++) { 
			free(Prob_grid[i][j]);
			free(Param_grid[i][j]);
		}
	for( i=0 ; i<N_nets ; i++) {
		free(Prob_grid[i]);
		free(Param_grid[i]);
	}
	free(Prob_grid);
	free(Param_grid);
 
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

	return 0;
}


