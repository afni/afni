/* 
   Turn ROIs into prep for tractography, written by PA Taylor
   (Oct/Nov, 2012).

	Main goal: take maps from from ICA, correlation, et al. and
	threshold to make GM-ROIs; then that latter and make into ROIs for
	tractography (most likely prob. tractography to define WM-ROIs)

	Inflation/detection for voxels sharing face/edge, but not for only
	vertex.

	Dec. 2012: 
	     fixed bug about thresholds,
        include maskability,
	     rename outputs, `*_WM' -> `*_GMI'.

	Jan. 2013:
	     csf_skel option

   Sept. 2013:
        allow negative ROIs to be used in refset

   Apr. 2014:
        can redefine how tight the definition of neighbors is
        also put in HOT_POINTS option:  threshold based on vol to get
        max values.
        --> bug fixed version; forgot a logical test in IF statement 

    Aug/Sept 2014: 
        new option for pre-expansion first, for example if WM region
        is input (expand to boundary, stop, and expand in GM by a
        certain number of voxels).

    Nov 2014:
        changed default neighborhood definition for AFNI consistency
        -> now it's facewise ONLY as default

    Dec 2014:
        new option for delineating a peak set of N connected voxels
        within an ROI

    Jan 2015:
        added nifti output switch

    Jan 2015b:
        minor bug fix when tiny number of input suprathreshold voxels

    April 2015:
        minor bug fix: outsets needed to have brickfac nulled, for when
        byte and short insets were used (-> had been causing error and 
        no output)

    Jul 2015:
        minor bug fix for negative-including refsets

    Aug 2015:
        minor bug fix: don't crash if no GM ROIs survive

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <3ddata.h>    
#include <gsl/gsl_rng.h>
#include <Fat_Labels.h>
#include "DoTrackit.h"
#include "roiing.h"


void usage_ROIMaker(int detail) 
{
	printf(
"\n"
"  ROIMaker, written by PA Taylor (Nov, 2012), part of FATCAT (Taylor & Saad,\n"
"  2013) in AFNI.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  THE GENERAL PURPOSE of this code is to create a labelled set of ROIs from\n"
"  input data. It was predominantly written with a view of aiding the process\n"
"  of combining functional and tractographic/structural data. Thus, one might\n"
"  input a brain map (or several, as subbricks) of functional parameters \n"
"  (e.g., correlation coefficients or ICA maps of Z-scores), set a value \n"
"  threshold and/or a cluster-volume threshold, and this program will find\n"
"  distinct ROIs in the data and return a map of them, each labelled with\n"
"  an integer. One can also provide a reference map so that, for example, in\n"
"  group studies, each subject would have the same number label for a given\n"
"  region (i.e., the L motor cortex is always labelled with a `2'). In order\n"
"  to be prepared for tractographic application, one can also enlarge the\n"
"  gray matter ROIs so that they intersect with neighboring white matter.\n"
"  One can either specify a number of voxels with which to pad each ROI, \n"
"  and/or input a white matter skeleton (such as could be defined from a \n"
"  segmented T1 image or an FA map) and use this as an additional guide for\n"
"  inflating the GM ROIs.  The output of this program can be used directly\n"  
"  for guiding tractography, such as with 3dTrackID.\n"
"\n"
"  If an input dataset ('-inset INSET') already contains integer delineation,\n"
"  such as using a parcellation method, then you can preserve these integers\n"
"  *even if the ROIs are contiguous* by using the same set as the reference\n"
"  set (-> '-refset INSET', as well).  Otherwise, contiguous blobs defined\n"
"  will likely be given a single integer value in the program.\n"
"\n"
"  Labeltable functionality is now available.  If an input '-refset REFSET'\n"
"  has a labeltable attached, it will also be attached to the output GM and\n"
"  inflated GMI datasets by default (if you don't want to do this, you can\n"
"  use the '-dump_no_labtab' to turn off this functionality).  If either no\n"
"  REFSET is input or it doesn't have a labeltable, one will be made from\n"
"  zeropadding the GM and GMI map integer values-- this may not add a lot of\n"
"  information, but it might make for more useful output.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  OUTPUTS:\n"
"   + `GM' map of ROIs  :based on value- and volume-thresholding, would\n"
"                        correspond most closely to gray matter regions of\n" 
"                        activation. The values of each voxel are an integer,\n"
"                        distinct per ROI.\n"
"   + `GMI' map of ROIs :map of inflated GM ROIs, based on GM map, with the \n"
"                        ROIs inflated either by a user-designed number of\n" 
"                        voxels, or also possibly including information of\n"
"                        the WM skeleton (so that inflation is halted after\n"
"                        encountering WM). The values of each voxel are the\n"
"                        same integers as in the GM map.\n"
"\n"
"  + RUNNING, need to provide:\n"
"     -inset    INSET  :3D volume(s) of values, esp. of functionally-derived\n"
"                       quantities like correlation values or ICA Z-scores.\n"
"     -thresh   MINTHR :threshold for values in INSET, used to great ROI\n"
"                       islands from the 3D volume's sea of values.\n"
"     -prefix   PREFIX :prefix of output name, with output files being:\n"
"                       PREFIX_GM* and PREFIX_GMI* (see `Outputs', above).\n"
"    and can provide: \n"
"     -refset   REFSET :3D (or multi-subbrick) volume containing integer \n"
"                       values with which to label specific GM ROIs after\n"
"                       thresholding.  This can be useful to assist in having\n"
"                       similar ROIs across a group labelled with the same \n"
"                       integer in the output GM and GMI maps.\n"
"                       If an INSET ROI has no corresponding REFSET label,\n"
"                       then the former is marked with an integer greater \n"
"                       than the max refset label. If an INSET ROI overlaps\n"
"                       with multiple REFSET ROIs, then the former is split\n"
"                       amongst the latter-- overlap regions get labelled \n"
"                       first, and then REFSET labels grow to cover the INSET\n"
"                       ROI in question.  NB: it is possible to utilize\n"
"                       negative-valued ROIs (voxels =-1) to represent NOT-\n"
"                       regions for tracking, for example.\n"
"     -volthr   MINVOL :integer number representing minimum size a cluster of\n"
"                       voxels must have in order to remain a GM ROI after \n"
"                       the values have been thresholded.  Number might be\n"
"                       estimated with 3dAlphaSim, or otherwise, to reduce\n"
"                       number of `noisy' clusters.\n"
"     -only_some_top N :after '-volthr' but before any ref-matching or\n"
"                       inflating, one can restrict each found region\n"
"                       to keep only N voxels with the highest inset values.\n"
"                       (If an ROI has <N voxels, then all would be kept.)\n"
"                       This option can result in unconnected pieces.\n"
"     -only_conn_top N :similar-ish to preceding option, but instead of just\n"
"                       selecting only N max voxels, do the following\n"
"                       algorithm: start the ROI with the peak voxel; search\n"
"                       the ROI's neighbors for the highest value; add that\n"
"                       voxel to the ROI; continue until either the ROI has \n"
"                       reached N voxels or whole region has been  added.\n"
"                       The returned ROI is contiguous and 'locally' maximal\n"
"                       but not necessarily globally so within the original\n"
"                       volume.\n"
"     -inflate  N_INFL :number of voxels which with to pad each found ROI in\n"
"                       order to turn GM ROIs into inflated (GMI) ROIs.\n"
"                       ROIs won't overlap with each other, and a WM skeleton\n"
"                       can also be input to keep ROIs from expanding through\n"
"                       a large amount of WM ~artificially (see below).\n"
"     -trim_off_wm     :switch to trim the INSET to exclude voxels in WM,\n"
"                       by excluding those which overlap an input WM\n"
"                       skeleton, SKEL (see `-wm_skel', below; to trim off\n"
"                       CSF, see separate `-csf_skel').  NB: trimming is done\n"
"                       before volume thresholding the ROIs, so fewer ROIs\n"
"                       might pass, or some input regions might be split\n"
"                       apart creating a greater number of regions.\n"
"     -wm_skel  SKEL   :3D volume containing info of WM, as might be defined\n"
"                       from an FA map or anatomical segmentation.  Can be\n"
"                       to guide ROI inflation with `-skel_stop'.\n"
"     -skel_thr THR    :if the skeleton is not a mask, one can put in a \n"
"                       threshold value for it, such as having THR=0.2 if \n"
"                       SKEL were a FA map.\n"
"     -skel_stop       :switch to stop inflation at locations which are \n"
"                       already on WM skeleton (default: off; and need\n"
"                       `-wm_skel' to be able to use).\n"
"     -csf_skel CSF_SK :similar to SKEL, a 3D volume containing info of CSF.\n"
"                       NB: however, with CSF_SK, info must just be a binary\n"
"                       mask already, and it will only be applied in trimming\n"
"                       procedure (no affect on inflation); if input, INSET\n"
"                       is automatically trimmed of CSF, independent of\n" 
"                       using `-trim_off_wm'.  Again, trimming done before\n"
"                       volume thresholding, so may decrease/separate regions\n"
"                       (though, that may be useful/more physiological).\n"
"     -mask   MASK     :can include a mask within which to apply threshold.\n"
"                       Otherwise, data should be masked already. Guess this\n"
"                       would be useful if the MINTHR were a negative value.\n"
"                       It's also useful to ensure that the output *_GMI*\n"
"                       ROI masks stay within the brain-- this probably won't\n"
"                       often matter too much.\n"
"                       For an N-brick inset, one can input an N- or 1-brick\n"
"                       mask.\n"
"    -neigh_face_only  : **DEPRECATED SWITCH** -> it's now default behavior\n"
"                       to have facewise-only neighbors, in order to be\n"
"                       consistent with the default usage of the clusterize\n"
"                       function in the AFNI window.\n"
"    -neigh_face_edge  :can loosen the definition of neighbors, so that\n"
"                       voxels can share a face or an edge in order to be\n"
"                       grouped into same ROI (AFNI default is that neighbors\n"
"                       share at least one edge).\n"
"    -neigh_upto_vert  :can loosen the definition of neighbors, so that\n"
"                       voxels can be grouped into the same ROI if they share\n"
"                       at least one vertex (see above for default).\n"
"    -nifti            :switch to output *.nii.gz GM and GMI files\n"
"                       (default format is BRIK/HEAD).\n"
"\n"
"  -preinfl_inset PSET :as a possible use, one might want to start with a WM\n"
"                       ROI, inflate it to find the nearest GM, then expand\n"
"                       that GM, and subtract away the WM+CSF parts. Requires\n"
"                       use of a '-wm_skel' and '-skel_stop', and replaces\n"
"                       using '-inset'.\n"
"                       The size of initial expansion through WM is entered\n"
"                       using the option below; then WM+CSF is subtracted.\n"
"                       The *_GM+orig* set is returned. In the *_GMI+orig*\n"
"                       set, the number of voxels expanded in GM is set using\n"
"                       the '-inflate' value (WM+CSF is subtracted again\n"
"                       before output).\n"
"  -preinfl_inflate PN :number of voxels for initial inflation of PSET.\n"
"\n"
"  -dump_no_labtab     :switch for turning off labeltable attachment to the\n"
"                       output GM and GMI files (from either from a '-refset\n"
"                       REFSET' or from automatic generation from integer\n"
"                       labels.\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"      3dROIMaker                     \\\n"
"         -inset CORR_VALUES+orig.    \\\n"
"         -thresh 0.6                 \\\n"
"         -prefix ROI_MAP             \\\n"
"         -volthr 100                 \\\n"
"         -inflate 2                  \\\n"
"         -wm_skel WM_T1+orig.        \\\n"
"         -skel_stop \n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  If you use this program, please reference the introductory/description\n"
"  paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"____________________________________________________________________________\n"
);
	return;
}


int main(int argc, char *argv[]) {
	int i,j,k,m,ii,jj,kk,mm,bb,n,aaa;
	int X,Y,Z;
	int iarg=0;
	THD_3dim_dataset *inset=NULL;
	char in_name[300];
	char inpre_name[300];
	float THR=0.;
	char *prefix="NAME_ROIMaker";
	THD_3dim_dataset *insetREF=NULL;
	THD_3dim_dataset *insetSKEL=NULL;
	THD_3dim_dataset *insetCSF_SKEL=NULL;
	THD_3dim_dataset *outsetGM=NULL, *outsetGMI=NULL;
   THD_3dim_dataset *outsetGM_tmp=NULL; // jul,2015: fix mismatch with rescal

   // default setting: neighbor shares face ONLY
   int NEIGHBOR_LIMIT = 2; 

	THD_3dim_dataset *MASK=NULL;
	char in_mask[300];
	int HAVE_MASK=0;
   int HAVEPREFIX=0;

	char in_REF[300];
	char prefix_GM[300];
	char prefix_GMI[300];
	char voxel_order[4]="---";


	int HAVEREF = 0; // switch if an external ref file for ROI labels is present
   int HAVE_PREINF = 0, HAVE_IN = 0; 
	int HAVESKEL = 0; // switch if an external ref file for ROI labels is present
	int HAVE_CSFSKEL = 0; // similar to above...
	int SKEL_STOP=0; // switch if inflation will be stopped in WM skeleton
	int TRIM_OFF_WM=0; // switch if WM_skel will be used to trim init inp map
	int idx = 0;
	int VOLTHR = 0; // switch about thresholding GM ROI volume size
	int val=0,Nvox=0;
	int max_nroi=0, index1=0;

	int ****DATA=NULL;
	float ****inpmap=NULL;
	short int ***SKEL=NULL;
	short int ***CSF_SKEL=NULL;
	int *N_thr=NULL, *relab_vox=NULL; // num of ROI vox per brik, pre-thr
	short int **temp_arr=NULL,**temp_arr2=NULL;
   short int **temp_arr_tmp=NULL; // jul,2015
	int **ROI_LABELS_pre=NULL;
	int **N_refvox_R=NULL;
	float SKEL_THR=0.5;// default, such as if input SKEL is a mask

	int *Dim=NULL; 
   // num of rois per brik, and inv labels
	int *NROI_IN=NULL,*NROI_IN_b=NULL,*INVROI_IN=NULL;
   // allow labels to be non-consecutive.
	int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL; 
	int *NROI_REF=NULL,*NROI_REF_b=NULL,*INVROI_REF=NULL;
	int ****OLAP_RI=NULL; // info of overlap WRT ref and T inset
	int **N_olap_RI=NULL,**N_olap_IR=NULL; // nums of rois per olap
	int *EXTRA_LAB=NULL; // count inROIs unmatched to any refROI
	int INFL_NUM=0; // number of vox to inflate ROIs by
	int PREINFL_NUM=0; // number of vox to inflate ROIs by
   short int ***invSKEL=NULL;

	// stuff for part 2: allow labels to be non-consecutive.
	int **ROI_LABELS_GM=NULL, **INV_LABELS_GM=NULL; 
	int *NROI_GM=NULL,*INVROI_GM=NULL;
	int ***COUNT_GM=NULL;

   int HOT_POINTS=0;
   int HOT_CONN=0;
   int NIFTI_OUT=0;
   int N_nonzer = 0;

   int *RESCALES=NULL; // will be used if negative values in refset mask
   short int **temp_ref=NULL; // for holding rescaled values
   THD_3dim_dataset *set_REFSCAL=NULL;
   float dum1[1]={0},dum2[1]={0};

   // Nov 2014: labeltables
   Dtable *roi_dtable=NULL;
   char *LabTabStr=NULL;
	char ***ROI_STR_LABELS=NULL;
   char EleNameStr[128];
   int DUMP_with_LABELS = 1;
	int MAXNROI=0;
	char prefix_dtable[300];
	char prefix_dtableGM[300];
   char *Dtable_str=NULL;
   char *Dtable_strGM=NULL;
   Dtable *new_dt=NULL;
   char mini[50];
   FILE *fout1=NULL;

	mainENTRY("3dROIMaker"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

	//INFO_message("version: OMICRON");
	Dim = (int *)calloc(4,sizeof(int));

	// scan args
	if (argc == 1) { usage_ROIMaker(1); exit(0); }
	iarg = 1; 
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_ROIMaker(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
			 
		if( strcmp(argv[iarg],"-inset") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-inset'");

			sprintf(in_name,"%s", argv[iarg]); 
         HAVE_IN = 1;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-thresh") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-thresh'");
			//INFO_message("Size of threshold is: %s",argv[iarg]);
			THR = atof(argv[iarg]);
			
			iarg++ ; continue ;
		}
		
		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]);

			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");

         HAVEPREFIX = 1;
			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-refset") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-refset'");
			
			sprintf(in_REF,"%s", argv[iarg]); 
			insetREF = THD_open_dataset(in_REF) ;
			if( insetREF == NULL )
				ERROR_exit("Can't open time series dataset '%s'.",in_REF);
			
			DSET_load(insetREF); CHECK_LOAD_ERROR(insetREF);
			HAVEREF = DSET_NVALS(insetREF);

			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-dump_no_labtab") == 0) {
         // if REFSET has a label table, default is to use it in
         // naming a dump_rois output;  this can turn that off.
			DUMP_with_LABELS=0;
			iarg++ ; continue ;
		}



      if( strcmp(argv[iarg],"-preinfl_inset") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-preinfl_inset'");
			
			sprintf(inpre_name,"%s", argv[iarg]); 
         HAVE_PREINF=1;

			iarg++ ; continue ;
		}      

		// can determine size of expansion based on this
		if( strcmp(argv[iarg],"-preinfl_inflate") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-preinfl_inflate'");
			PREINFL_NUM = atoi(argv[iarg]);
			
			if(PREINFL_NUM<=0)
				ERROR_exit("Size of inflation layer for ROIs=%d: must be >0!",
							  PREINFL_NUM);
			
			iarg++ ; continue ;
		}

		// can have vol thr.
		if( strcmp(argv[iarg],"-volthr") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-volthr'");
			VOLTHR = atoi(argv[iarg]);
			
			if(VOLTHR<=0)
				ERROR_exit("Volume threshold for size of ROIs=%d: must be >0!",
							  VOLTHR);
			
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-only_some_top") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-only_some_top'");
			HOT_POINTS = atoi(argv[iarg]);
			
			if(HOT_POINTS<=0)
				ERROR_exit("The number of (high-value) voxels to keep per ROI=%d: "
                       "must be >0!",
							  HOT_POINTS);
			
			iarg++ ; continue ;
		}

      if( strcmp(argv[iarg],"-only_conn_top") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-only_conn_top'");
			HOT_CONN = atoi(argv[iarg]);
			
			if(HOT_CONN<=0)
				ERROR_exit("The number of (high-value) voxels to keep per ROI=%d: "
                       "must be >0!",
							  HOT_CONN);
			
			iarg++ ; continue ;
		}


		// can determine size of expansion based on this
		if( strcmp(argv[iarg],"-inflate") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-inflate'");
			INFL_NUM = atoi(argv[iarg]);
			
			if(INFL_NUM<=0)
				ERROR_exit("Size of inflation layer for ROIs=%d: must be >0!",
							  INFL_NUM);
			
			iarg++ ; continue ;
		}

		// use this to also to determine size of expansion
		if( strcmp(argv[iarg],"-wm_skel") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-wm_skel'");
			
			insetSKEL = THD_open_dataset(argv[iarg]) ;
			if( insetSKEL == NULL )
				ERROR_exit("Can't open time series dataset '%s'.",argv[iarg]);
			
			DSET_load(insetSKEL); CHECK_LOAD_ERROR(insetSKEL);
			HAVESKEL = 1;// DSET_NVALS(insetSKEL);

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-skel_thr") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-skel_thr'");
			SKEL_THR = atof(argv[iarg]);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-nifti") == 0) {
			NIFTI_OUT=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-trim_off_wm") == 0) {
			TRIM_OFF_WM=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-skel_stop") == 0) {
			SKEL_STOP=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-csf_skel") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-csf_skel'");
			
			insetCSF_SKEL = THD_open_dataset(argv[iarg]) ;
			if( insetCSF_SKEL == NULL )
				ERROR_exit("Can't open time series dataset '%s'.",argv[iarg]);
			
			DSET_load(insetCSF_SKEL); CHECK_LOAD_ERROR(insetCSF_SKEL);
			HAVE_CSFSKEL = 1;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-neigh_face_only") == 0) {
			// NEIGHBOR_LIMIT=2;
         WARNING_message("This option no longer exists -> "
                         "in fact, it is default behavior.");

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-neigh_face_edge") == 0) {
			NEIGHBOR_LIMIT=3;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-neigh_upto_vert") == 0) {
			NEIGHBOR_LIMIT=4;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-mask") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-mask'");
			// HAVE_MASK=1;

			sprintf(in_mask,"%s", argv[iarg]); 
			MASK = THD_open_dataset(in_mask) ;
			if( MASK == NULL )
				ERROR_exit("Can't open time series dataset '%s'.",in_mask);

			DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
			HAVE_MASK = DSET_NVALS(MASK);

			iarg++ ; continue ;
		}

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}

	if (iarg < 3) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}

   if( HAVE_IN && HAVE_PREINF ) 
      ERROR_exit("Can't have both '-inset' and '-preinfl_inset' together.");
   else if( HAVE_IN || HAVE_PREINF ) {

      if( HAVE_IN )
         inset = THD_open_dataset(in_name);
      else
         inset = THD_open_dataset(inpre_name);

      if( inset == NULL )
         ERROR_exit("Can't open time series dataset '%s'.",in_name);
      DSET_load(inset); CHECK_LOAD_ERROR(inset);
      
      Dim[0] = DSET_NX(inset); Dim[1] = DSET_NY(inset); 
      Dim[2] = DSET_NZ(inset); Dim[3]= DSET_NVALS(inset); 
      Nvox = DSET_NVOX(inset) ;
      voxel_order[0]=ORIENT_typestr[inset->daxes->xxorient][0];
      voxel_order[1]=ORIENT_typestr[inset->daxes->yyorient][0];
      voxel_order[2]=ORIENT_typestr[inset->daxes->zzorient][0];

      inpmap = (float ****) calloc( Dim[0], sizeof(float ***) );
      for ( i = 0 ; i < Dim[0] ; i++ ) 
         inpmap[i] = (float ***) calloc( Dim[1], sizeof(float **) );
      for ( i = 0 ; i < Dim[0] ; i++ ) 
         for ( j = 0 ; j < Dim[1] ; j++ ) 
            inpmap[i][j] = (float **) calloc( Dim[2], sizeof(float *) );
      for ( i=0 ; i<Dim[0] ; i++ ) 
         for ( j=0 ; j<Dim[1] ; j++ ) 
            for ( k= 0 ; k<Dim[2] ; k++ ) 
               inpmap[i][j][k] = (float *) calloc( Dim[3], sizeof(float) );

      if( inpmap == NULL ) { 
         fprintf(stderr, "\n MemAlloc failure with inputs.\n");
         exit(18);
      }
      
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]); 
               for( m=0 ; m<Dim[3] ; m++ ) 
                  inpmap[i][j][k][m] = THD_get_voxel(inset,idx,m);
            }
   }
   else
      ERROR_exit("Need either '-inset' or '-preinfl_inset'.");

   if( HAVE_PREINF) {
      if( !HAVESKEL )
         ERROR_exit("When using '-preinfl_inset', need a '-wm_skel'");

      if( !SKEL_STOP ) {
         WARNING_message("When using '-preinfl_inset', you need '-skel_stop'"
                         "also.\n    It will be set for you here.");
         SKEL_STOP = 1;
      }


   }

	INFO_message("Value of threshold is: %f",THR);

	// some checks on sizes of arrays/sets
	if(HAVEREF) {
		if((Dim[0] != DSET_NX(insetREF)) || (Dim[1] != DSET_NY(insetREF)) ||
			(Dim[2] != DSET_NZ(insetREF)) )
			ERROR_exit("The xyz-dimensions of refset and inset don't match");
		
		if( Dim[3] == HAVEREF )
			INFO_message("Each subrik of refset will be applied to corresponding "
                      "inset brik.");
		else
			ERROR_exit("The number of inset and refset briks must match: "
                    " here, ref=%d, inset=%d",
                    HAVEREF,Dim[3]);

		if( voxel_order[0] != ORIENT_typestr[insetREF->daxes->xxorient][0] ||
			 voxel_order[1] != ORIENT_typestr[insetREF->daxes->yyorient][0] ||
			 voxel_order[2] != ORIENT_typestr[insetREF->daxes->zzorient][0] )
			ERROR_exit("Refset orientation is not %s like the inset.",
                    voxel_order);
	}

   if(HAVE_MASK) {
		if((Dim[0] != DSET_NX(MASK)) || (Dim[1] != DSET_NY(MASK)) ||
			(Dim[2] != DSET_NZ(MASK)) )
			ERROR_exit("The xyz-dimensions of mask and inset don't match");
		
		if( Dim[3] == HAVE_MASK )
			INFO_message("Each subrik of mask will be applied to corresponding "
                      "inset brik.");
      else if( HAVE_MASK == 1)
         INFO_message("Will apply single mask to the multiple inset briks.");
		else
			ERROR_exit("The number of mask briks must be either one or match the"
                    " number of inset briks.\n"
                    "\t  Here: mask=%d, inset=%d",HAVE_MASK,Dim[3]);

		if( voxel_order[0] != ORIENT_typestr[MASK->daxes->xxorient][0] ||
			 voxel_order[1] != ORIENT_typestr[MASK->daxes->yyorient][0] ||
			 voxel_order[2] != ORIENT_typestr[MASK->daxes->zzorient][0] )
			ERROR_exit("Mask orientation is not %s like the inset.",voxel_order);
	}


	
	if(HAVESKEL) {
		if((Dim[0] != DSET_NX(insetSKEL)) || (Dim[1] != DSET_NY(insetSKEL)) ||
			(Dim[2] != DSET_NZ(insetSKEL)) )
			ERROR_exit("The xyz-dimensions of WM skeleton and inset don't match");
	
		if( voxel_order[0] != ORIENT_typestr[insetSKEL->daxes->xxorient][0] ||
			 voxel_order[1] != ORIENT_typestr[insetSKEL->daxes->yyorient][0] ||
			 voxel_order[2] != ORIENT_typestr[insetSKEL->daxes->zzorient][0] )
			ERROR_exit("WM skeleton orientation is not %s like the inset.",
						  voxel_order);

	}

	if(HAVE_CSFSKEL) {
		if((Dim[0] != DSET_NX(insetCSF_SKEL)) || 
			(Dim[1] != DSET_NY(insetCSF_SKEL)) ||
			(Dim[2] != DSET_NZ(insetCSF_SKEL)) )
			ERROR_exit("The xyz-dimensions of WM skeleton and inset don't match");
	
		if(voxel_order[0] != ORIENT_typestr[insetCSF_SKEL->daxes->xxorient][0] ||
			voxel_order[1] != ORIENT_typestr[insetCSF_SKEL->daxes->yyorient][0] ||
			voxel_order[2] != ORIENT_typestr[insetCSF_SKEL->daxes->zzorient][0] )
			ERROR_exit("CSF skeleton orientation is not %s like the inset.",
						  voxel_order);
	}

	if( (!HAVESKEL) && SKEL_STOP) {
		INFO_message("*+ You asked to stop inflation at the WM skeleton, but\n"
                   "\t didn't give a skeleton using `-wm_skel'-- will just"
                   " ignore that.");
		SKEL_STOP=0; //reset
	}

	if( (!HAVESKEL) && TRIM_OFF_WM) {
		INFO_message("*+ You asked to trim GM input with WM skeleton, but \n"
                   "\t didn't give a skeleton using `-wm_skel'-- will just"
                   " ignore that.");
		TRIM_OFF_WM = 0; //reset
	}

	// ****************************************************************
	// ****************************************************************
	//                    make inset storage
	// ****************************************************************
	// ****************************************************************
	
	// WM SKELETON MAKING
	SKEL = (short int ***) calloc( Dim[0], sizeof(short int **));
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		SKEL[i] = (short int **) calloc( Dim[1], sizeof(short int *));
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			SKEL[i][j] = (short int *) calloc( Dim[2], sizeof(short int));
	
	CSF_SKEL = (short int ***) calloc( Dim[0], sizeof(short int **));
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		CSF_SKEL[i] = (short int **) calloc( Dim[1], sizeof(short int *));
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			CSF_SKEL[i][j] = (short int *) calloc( Dim[2], sizeof(short int));

	if( (SKEL == NULL) || (CSF_SKEL == NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(16);
	}

	// make skeleton: either with file input, or just put 1s everywhere.
	//idx = 0;
	// should preserve relative ordering of data
   i = MakeSkels( Dim, 
                  HAVE_CSFSKEL,
                  CSF_SKEL, 
                  insetCSF_SKEL,
                  HAVESKEL,
                  SKEL, 
                  insetSKEL,
                  SKEL_THR );

	N_thr = (int *)calloc(Dim[3],sizeof(int)); // num of init vox per brik,const
	relab_vox = (int *)calloc(Dim[3],sizeof(int)); // num of vox >thr per brik
	NROI_IN = (int *)calloc(Dim[3],sizeof(int)); // num of roi per brik
	
	// for use in HAVEREF
	NROI_IN_b = (int *)calloc(Dim[3],sizeof(int)); // num of roi per brik

	DATA = (int ****) calloc( Dim[0], sizeof(int ***) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		DATA[i] = (int ***) calloc( Dim[1], sizeof(int **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			DATA[i][j] = (int **) calloc( Dim[2], sizeof(int *) );
	for ( i=0 ; i<Dim[0] ; i++ ) 
		for ( j=0 ; j<Dim[1] ; j++ ) 
			for ( k= 0 ; k<Dim[2] ; k++ ) 
				DATA[i][j][k] = (int *) calloc( Dim[3], sizeof(int) );

	// will be output
	temp_arr = calloc( Dim[3],sizeof(temp_arr));  // XYZ components
	for(i=0 ; i<Dim[3] ; i++) 
		temp_arr[i] = calloc( Nvox,sizeof(short int) ); 
   // jul,2015
   temp_arr_tmp = calloc( Dim[3],sizeof(temp_arr_tmp));  // XYZ components
	for(i=0 ; i<Dim[3] ; i++) 
		temp_arr_tmp[i] = calloc( Nvox,sizeof(short int) ); 

	if( (DATA == NULL) || (N_thr == NULL) || (NROI_IN == NULL)
		 || (NROI_IN_b == NULL) || (temp_arr == NULL) 
       || (temp_arr_tmp == NULL)) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(14);
	}


   if( HAVE_PREINF ){
      int ***fakeCOUNT_GM=NULL;
      int **fakeINV_LABELS_GM=NULL;

      invSKEL = (short int ***) calloc( Dim[0], sizeof(short int **));
      for ( i = 0 ; i < Dim[0] ; i++ ) 
         invSKEL[i] = (short int **) calloc( Dim[1], sizeof(short int *));
      for ( i = 0 ; i < Dim[0] ; i++ ) 
         for ( j = 0 ; j < Dim[1] ; j++ ) 
            invSKEL[i][j] = (short int *) calloc( Dim[2], sizeof(short int));

      // pretend like it's just one big binary thing expanding here, hence
      // the '1+1'
      fakeCOUNT_GM = ( int ***) calloc( Dim[3], sizeof( int **));
      for ( i = 0 ; i < Dim[3] ; i++ ) 
         fakeCOUNT_GM[i] = ( int **) calloc( (1+1), sizeof( int *));
      for ( i = 0 ; i < Dim[3] ; i++ ) 
         for ( j = 0 ; j < (1+1) ; j++ ) 
            fakeCOUNT_GM[i][j] = ( int *) calloc( 3, sizeof( int));
      fakeINV_LABELS_GM = calloc( Dim[3],sizeof(fakeINV_LABELS_GM));  
      for(i=0 ; i<Dim[3] ; i++) 
         fakeINV_LABELS_GM[i] = calloc(1+1,sizeof(int)); 

      if( (invSKEL == NULL) || (fakeCOUNT_GM == NULL) || 
          (fakeINV_LABELS_GM == NULL) ) { 
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(14);
      }
      
      
      
      
      // !!!!!!! should also include CSF here, probably as well, if a
      // user entered it
      
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
               if( !SKEL[i][j][k] )
                  invSKEL[i][j][k] = 1;
               //if( HAVE_CSFSKEL) // and can turn it off if in CSF
                    // if( CSF_SKEL[i][j][k] )
               //invSKEL[i][j][k] = 0;
            }



      i = Make_BinaryMask( Dim,
                           HAVE_MASK,
                           MASK,
                           inpmap,
                           THR,
                           0,
                           SKEL,
                           CSF_SKEL,
                           HAVE_CSFSKEL,
                           DATA,
                           N_thr );

      // go through and start inflating
      // do 1 layer at a time, in case of squeezed neighborhoods and 
      // book counting of WM intersections, etc.
      i = ROI_make_inflate( Dim, 
                            PREINFL_NUM,
                            1,
                            NEIGHBOR_LIMIT,
                            HAVE_MASK,
                            MASK,
                            DATA,
                            invSKEL,
                            fakeCOUNT_GM,
                            fakeINV_LABELS_GM );


      // final editing before subtractions
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
               if( HAVE_CSFSKEL) // and can turn it off if in CSF
                  if( CSF_SKEL[i][j][k] )
                     invSKEL[i][j][k] = 0;
            }



      i = MoveData_to_InpSet( Dim,
                              inpmap,
                              DATA,
                              invSKEL );


      for( i=0 ; i<Dim[3] ; i++) 
         free(fakeCOUNT_GM[i]);
      free(fakeCOUNT_GM);

      
      for(i=0 ; i<Dim[3] ; i++) 
         free(fakeINV_LABELS_GM[i]);
      free(fakeINV_LABELS_GM);

   }


	// STEP 1: go through brik by brik to apply thresholding -> prod bin mask
   i = Make_BinaryMask( Dim,
                        HAVE_MASK,
                        MASK,
                        inpmap,
                        THR,
                        TRIM_OFF_WM,
                        SKEL,
                        CSF_SKEL,
                        HAVE_CSFSKEL,
                        DATA,
                        N_thr );


	// make a supplement array of large possible number of vox to be
	// found in any brik;  will be reused/cleaned after each use
	for( m=0 ; m<Dim[3] ; m++ )
		if( max_nroi < N_thr[m] )
			max_nroi = N_thr[m];

	if(max_nroi==0)
		ERROR_exit("No keeper voxels in ROIs in any brik!");

	
	// as big as could be needed for labelling...  probably way bigger
	// than necessary (could buffer/realloc, but just start with this, 
	// at least for time being)
	ROI_LABELS_pre = calloc( Dim[3],sizeof(ROI_LABELS_pre));  
	for(i=0 ; i<Dim[3] ; i++) 
		ROI_LABELS_pre[i] = calloc(2*N_thr[i],sizeof(int));
   // always make, for ease of code use later; if no refset given, it
   // just stays as 0s.  By definition, if HAVEREF>0, then HAVEREF==Dim[3],
   // so this definition works
   RESCALES = (int *)calloc(Dim[3], sizeof(int)); 

	if( (ROI_LABELS_pre == NULL) 
		 || (RESCALES == NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(15);
	}		
	
	// ****************************************************************
	// ****************************************************************
	//                    labelling inset
	// ****************************************************************
	// ****************************************************************
	


   if(HOT_POINTS && HOT_CONN)
      ERROR_exit("Can't use both '-only_some_top' and '-only_conn_top'\n"
                    "\t together:  gotta choose just one!");
   else if(HOT_POINTS)
      INFO_message("Will shrink large clusters to keep at most %d voxels"
                   " of high values.", HOT_POINTS);
   else if(HOT_CONN)
      INFO_message("Will shrink large clusters to keep at most %d contiguous"
                   " voxels of neighboring high values.", HOT_CONN);


	// STEP 2: label separate ROIs with ints
	i = Make_SepLabels( Dim,
                       DATA,
                       max_nroi,
                       N_thr,
                       NROI_IN, 
                       ROI_LABELS_pre,
                       VOLTHR,
                       NEIGHBOR_LIMIT,
                       HOT_POINTS,
                       HOT_CONN,
                       inpmap );
	
	// Step 3: do matching, or subtract values back to where they should be
	if( HAVEREF > 0 ) {
				
		// Step 3A-1: SET UP MATRICES FOR THIS STUFF
		NROI_REF = (int *)calloc(HAVEREF, sizeof(int)); 
		NROI_REF_b = (int *)calloc(HAVEREF, sizeof(int)); // for counting
		INVROI_REF = (int *)calloc(HAVEREF, sizeof(int)); 
      temp_ref = calloc( HAVEREF,sizeof(temp_ref));  // XYZ components
      for(i=0 ; i<HAVEREF ; i++) 
         temp_ref[i] = calloc( Nvox,sizeof(short int) ); 

		if( (NROI_REF == NULL) || (INVROI_REF == NULL) 
          || (NROI_REF_b == NULL) || (temp_ref == NULL) ) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(122);
		}
		
      // NEW: allow for having `antimask'/negative/NOT-ROIs by simple
      // rescaling up of REFSET values, if there's a negative present;
      // then do normal labelling, and then rescale down again.
		for( i=0 ; i<HAVEREF ; i++) {
         THD_subbrick_minmax(insetREF, i, 1,&dum1[0], &dum2[0]);
         if( dum1[0]<0 )//??inset-->fixed
            RESCALES[i] = 1 - ((int) dum1[0]);
         //printf("\n%d\t%d --> refscal=%d",(int) dum1[0], 
         //     (int) dum2[0], RESCALES[i]);  
      }
      
      // rescale values as necessary
      for( m=0 ; m<Dim[3] ; m++ ) {
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
                  idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
						if( THD_get_voxel(insetREF,idx,m)>0.5 ||
                      THD_get_voxel(insetREF,idx,m)<-0.5 ) {
                     temp_ref[m][idx] = (short int) 
                        THD_get_voxel(insetREF,idx,m);
                     if( RESCALES[m] )
                        temp_ref[m][idx]+= (short int) RESCALES[m];
                  }
               }
      }
      set_REFSCAL = EDIT_empty_copy( insetREF ) ; 

      EDIT_dset_items(set_REFSCAL,
							 ADN_datum_all , MRI_short , 
                      ADN_brick_fac, NULL,
							 ADN_none ) ;

      for( m=0 ; m<Dim[3] ; m++ ) {
         EDIT_substitute_brick(set_REFSCAL, m, MRI_short, temp_ref[m]);
         temp_ref[m]=NULL; // to not get into trouble...
      }
      free(temp_ref);

      for( i=0 ; i<HAVEREF ; i++)
         THD_subbrick_minmax(set_REFSCAL, i, 1, &dum1[0], &dum2[0]);
      
		for( i=0 ; i<HAVEREF ; i++) {
			INVROI_REF[i] = (int) THD_subbrick_max(set_REFSCAL, i, 1);
         if( INVROI_REF[i] >= MIN_NTHR_MAX_NROILAB ) // Jan 2015: safety catch for small input sets
            ERROR_exit("There is a very large reference value label (%d) and this will break\n"
                       "\t the delicate inner balance of the program. Contact PT for assistance\n",
                       INVROI_REF[i] );
      }

		ROI_LABELS_REF = calloc( HAVEREF,sizeof(ROI_LABELS_REF));  
		for(i=0 ; i<HAVEREF ; i++) 
			ROI_LABELS_REF[i] = calloc(INVROI_REF[i]+1,sizeof(int)); 
		INV_LABELS_REF = calloc( HAVEREF,sizeof(INV_LABELS_REF));  
		for(i=0 ; i<HAVEREF ; i++) 
			INV_LABELS_REF[i] = calloc(INVROI_REF[i]+1,sizeof(int)); 
		if( (ROI_LABELS_REF == NULL) || (ROI_LABELS_REF == NULL) ) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(123);
		}
		
		// Step 3A-2: find out the labels in the ref, organize them
		//            both backwards and forwards.
		bb = ViveLeRoi(set_REFSCAL,
							ROI_LABELS_REF, INV_LABELS_REF, 
							NROI_REF,       INVROI_REF);
		if( bb != 1)
			ERROR_exit("Problem loading/assigning ROI labels");

		N_olap_RI = calloc( HAVEREF,sizeof(N_olap_RI));  
		for(i=0 ; i<HAVEREF ; i++) 
			N_olap_RI[i] = calloc(NROI_REF[i]+1,sizeof(int)); 
		N_olap_IR = calloc( HAVEREF,sizeof(N_olap_IR));  
		for(i=0 ; i<HAVEREF ; i++) 
			N_olap_IR[i] = calloc(NROI_IN[i]+1,sizeof(int)); 
		EXTRA_LAB = (int *)calloc(Dim[3], sizeof(int)); 

		OLAP_RI = (int ****) calloc( Dim[3], sizeof(int ***) );
		for ( i=0 ; i<Dim[3] ; i++ ) 
			OLAP_RI[i] = (int ***) calloc( NROI_REF[i]+1, sizeof(int **) );
		for ( i=0 ; i<Dim[3] ; i++ ) 
			for ( j = 0 ; j<NROI_REF[i]+1  ; j++ ) 
				OLAP_RI[i][j] = (int **) calloc( NROI_IN[i]+1, sizeof(int *) );
		for ( i=0 ; i<Dim[3] ; i++ ) 
			for ( j=0 ; j<NROI_REF[i]+1 ; j++ ) 
				for ( k= 0 ; k<NROI_IN[i]+1 ; k++ ) 
					OLAP_RI[i][j][k] = (int *) calloc( 2,  sizeof(int) );

		N_refvox_R = calloc( HAVEREF,sizeof(N_refvox_R));  
		for(i=0 ; i<HAVEREF ; i++) 
			N_refvox_R[i] = calloc(NROI_REF[i]+1,sizeof(int)); 
		
		if( (OLAP_RI == NULL) || (N_refvox_R == NULL) ||
			 (N_olap_RI == NULL) || (N_olap_IR == NULL) 
			 ) { 
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(17);
		}

		
		// Step 3A-3: go through data set and refset, keep track of
		// any overlaps and number of voxels in each overlapping region.
		// Also, count number of vox per refset ROI
		for( m=0 ; m<Dim[3] ; m++ ) {

			//idx=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
                  idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
						if( THD_get_voxel(set_REFSCAL,idx,m)>0 ) {
							// for ease/shortcut, define X, the compact form of
							// the refROI label index, and Y, the inset ROI smallval
							X = INV_LABELS_REF[m][(int) THD_get_voxel(set_REFSCAL,
                                                               idx,
                                                               m)];
							N_refvox_R[m][X]+=1;
							if( DATA[i][j][k][m]>0 ) {
								Y = DATA[i][j][k][m] - N_thr[m];
								OLAP_RI[m][X][Y][0]+=1;
							}
						}
						//idx++;
					}
		}
		
		// At this point we should know: Nvox/ROI of the inset; Nvox/ROI
		// of the refset, Noverlap voxels for any combination of ref and
		// inset ROIs.  Can calculate percents of olap and dice coeffs
		
		// Step 3A-4: check and see the situation with overlapping of indices
		for( m=0 ; m<Dim[3] ; m++ ) {
			
			for( i=1 ; i<=NROI_REF[m]; i++ ) // ind range due to labels
				for( j=1 ; j<=NROI_IN[m]; j++ ) // ind range due to labels
					if( OLAP_RI[m][i][j][0]>0 ) {
						N_olap_RI[m][i]+=1;
						N_olap_IR[m][j]+=1;
					}
			
			// go through for each ref ROI and see what's up.
			for( i=1 ; i<=NROI_REF[m]; i++ ) {// ind range due to labels
				
				// no matching overlap
				if(N_olap_RI[m][i]==0) {
					for( j=0 ; j<=NROI_IN[m]; j++ ) // range from zero...
						OLAP_RI[m][i][j][1] = -1; // label
					NROI_REF_b[m]++; // keep tally of how many ROIs accounted for
				}

				// single overlap
				else if(N_olap_RI[m][i]==1) {
					for( j=1 ; j<=NROI_IN[m]; j++ ) // to find it
						if( OLAP_RI[m][i][j][0]>0) { //found it
							if(N_olap_IR[m][j]==1) { // chaste partners
								OLAP_RI[m][i][j][1] = 3; // label
								NROI_REF_b[m]++; // accounted.
								NROI_IN_b[m]++; // accounted.
							}
							else { // to be grown on it
								OLAP_RI[m][i][j][1] = 4; // label
								NROI_REF_b[m]++; // accounted.
							}
						}
				}

				// multiple overlap
				else { 
					for( j=1 ; j<=NROI_IN[m]; j++ ) {// to find it
						if( OLAP_RI[m][i][j][0]>0) { //found it
							if(N_olap_IR[m][j]==1) { // chaste partners
								OLAP_RI[m][i][j][1] = 3; // label
								NROI_IN_b[m]++; // accounted.
							}
							else { // grow on it
								OLAP_RI[m][i][j][1] = 4; // label
							}
						}
					}
				}
			}

			for( j=1 ; j<=NROI_IN[m]; j++ )
				if(N_olap_IR[m][j]==0) {
					EXTRA_LAB[m]++; 
					// store this value to use to relabel below; have to make
					// sure later than when this is subtracted down, it is
					// bigger still than largest NROI_REF label, as well.
					// negative to help distinguish it
					OLAP_RI[m][0][j][1] = -(NROI_IN[m] + N_thr[m] + EXTRA_LAB[m]);
					for( i=1 ; i<=NROI_REF[m]; i++ ) // ind range due to labels
						OLAP_RI[m][i][j][1] = -1; // label
					NROI_IN_b[m]++; // keep tally of how many ROIs accounted for
				}
		
		}

		// now go through and start doing replacements, based on what
		// overlaps were found above.
		for( m=0 ; m<Dim[3] ; m++ ) {
			
			relab_vox[m]=0;
			//idx=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
                  idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
						if( DATA[i][j][k][m]>0 ) {
							Y = DATA[i][j][k][m] - N_thr[m];
							if( OLAP_RI[m][0][Y][1]<0 ) {// was neg val when stored
								DATA[i][j][k][m] = -OLAP_RI[m][0][Y][1]; 
								relab_vox[m]++;
							}
							for( ii=1 ; ii<=NROI_REF[m]; ii++ ) {
								if( OLAP_RI[m][ii][Y][1]==3 ) {// relabel
									DATA[i][j][k][m] = ROI_LABELS_REF[m][ii]; // ref #
									relab_vox[m]++;
								}
								else if( (OLAP_RI[m][ii][Y][1]==4) &&
											(THD_get_voxel(set_REFSCAL,idx,m)>0) ) {
                           // relabel
									DATA[i][j][k][m] = THD_get_voxel(set_REFSCAL,idx,m);
									relab_vox[m]++;
									break;
								}
							}
						}
						//idx++;
					}
		}

		// ok, at this point, all *direct* overlaps and non-overlaps should 
		// have been claimed.  Now, we go through and grow each region 
		// (labelled with a 4) into ones with index value larger than N_thr[m].  
		i = Relabel_IfNecessary( Dim,
                               DATA,
                               N_thr,
                               relab_vox,
                               NROI_IN, 
                               NROI_REF,
                               ROI_LABELS_REF,
                               NEIGHBOR_LIMIT);


	}
	else { // simple case, just keep labels in order they were found.
		for( m=0 ; m<Dim[3] ; m++ ) 
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if( DATA[i][j][k][m]>0 ) 
							DATA[i][j][k][m]-= N_thr[m];

      for( m=0 ; m<Dim[3] ; m++ ) 
         NROI_IN_b[m] = NROI_IN[m];
   }


	// **************************************************************
	// **************************************************************
	//                 Store and output GM info
	// **************************************************************
	// **************************************************************
	
	outsetGM_tmp = EDIT_empty_copy( inset ) ; // jul,2015
	outsetGM = EDIT_empty_copy( inset ) ; 
   if( NIFTI_OUT )
      sprintf(prefix_GM,"%s_GM.nii.gz",prefix); // jan,2015
   else
      sprintf(prefix_GM,"%s_GM",prefix);

   
	EDIT_dset_items( outsetGM,
						  ADN_datum_all , MRI_short , 
                    ADN_brick_fac, NULL,
						  ADN_prefix    , prefix_GM ,
						  ADN_none ) ;
   EDIT_dset_items( outsetGM_tmp,
						  ADN_datum_all , MRI_short , 
                    ADN_brick_fac, NULL,
						  ADN_none ) ; // jul,2015

	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetGM)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetGM));
	
   
	for( m=0 ; m<Dim[3] ; m++ ) {
		idx=0;
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
               if( DATA[i][j][k][m] ) {// account for neg values rescaled
                  temp_arr[m][idx] = DATA[i][j][k][m]-RESCALES[m];
                  temp_arr_tmp[m][idx] = DATA[i][j][k][m]; // jul,2015
                  N_nonzer++; // Aug,2015
               }
					idx+=1;
				}
		EDIT_substitute_brick(outsetGM, m, MRI_short, temp_arr[m]); 
		EDIT_substitute_brick(outsetGM_tmp, m, MRI_short, temp_arr_tmp[m]); 
		temp_arr[m]=NULL; // to not get into trouble...
		temp_arr_tmp[m]=NULL; // to not get into trouble...
	}

	
	for( m=0 ; m<Dim[3] ; m++ ) {
		free(temp_arr[m]);
 		free(temp_arr_tmp[m]);
   }
	free(temp_arr);
   free(temp_arr_tmp);

   //   if( NEIGHBOR_LIMIT == 2 )
   //  WARNING_message("NB: as of Nov. 2014, "
   //              "voxel neighborhoods are facewise-only as a default\n"
   //              "\t(it's the general AFNI standard). "
   //              "See the helpfile for switches to change this feature.");

   if (N_nonzer)
      INFO_message("GM map is done.");
   else
      WARNING_message("No ROIs left in GM map: nothing to output.\n"
                      "\t Perhaps a different threshold might be warranted?\n"
                      "\t Or maybe just <<c'est la vie.>>");

	// **************************************************************
	// **************************************************************
	//                 Expand GM maps
	// **************************************************************
	// **************************************************************

   if (N_nonzer) {
	
      // find all index numbers for GM systematically.
      NROI_GM = (int *)calloc(Dim[3], sizeof(int)); 
      INVROI_GM = (int *)calloc(Dim[3], sizeof(int)); 
      for( i=0 ; i<Dim[3] ; i++) 
         INVROI_GM[i] = (int) THD_subbrick_max(outsetGM_tmp, i, 1);//??inset-->fixed. jul,2015: new fix, temp values b/c of rescaling
      if( (INVROI_GM == NULL ) || ( NROI_GM == NULL ) ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(122);
      }

      ROI_LABELS_GM = calloc( Dim[3],sizeof(ROI_LABELS_GM));  
      for(i=0 ; i<Dim[3] ; i++) 
         ROI_LABELS_GM[i] = calloc(INVROI_GM[i]+1,sizeof(int)); 
      INV_LABELS_GM = calloc( Dim[3],sizeof(INV_LABELS_GM));  
      for(i=0 ; i<Dim[3] ; i++) 
         INV_LABELS_GM[i] = calloc(INVROI_GM[i]+1,sizeof(int)); 

      // will hold counts of ROIs (total and on skeleton) and some switches:
      // GROW_ON = 1
      COUNT_GM = ( int ***) calloc( Dim[3], sizeof( int **));
      for ( i = 0 ; i < Dim[3] ; i++ ) 
         COUNT_GM[i] = ( int **) calloc( INVROI_GM[i]+1, sizeof( int *));
      for ( i = 0 ; i < Dim[3] ; i++ ) 
         for ( j = 0 ; j < INVROI_GM[i]+1 ; j++ ) 
            COUNT_GM[i][j] = ( int *) calloc( 3, sizeof( int));

      if( (ROI_LABELS_GM == NULL) || (INV_LABELS_GM == NULL) 
          || (COUNT_GM == NULL)) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(123);
      }

      // jul,2015: use '_tmp' one to match with DATA, which might be rescaled
      bb = ViveLeRoi(outsetGM_tmp, 
                     ROI_LABELS_GM, INV_LABELS_GM, 
                     NROI_GM,       INVROI_GM);
      if( bb != 1)
         ERROR_exit("Problem loading/assigning GM labels");

      DSET_delete(outsetGM_tmp); // jul,2015: unneeded hereafter
      free(outsetGM_tmp);


      // preliminary setting up of COUNT_GM
      for( m=0 ; m<Dim[3] ; m++ ) {
         // ?? Not sure what the next two lines were for...
         //for( i=0 ; i<NROI_GM[m]+1 ; i++ ) 
         //	ROI_LABELS_GM[m][i] = 1; //switch to keep adding to it

         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) 
                  if( DATA[i][j][k][m]>0 ) {
                     COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][1]++;
                     if(SKEL[i][j][k])
                        COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][2]++;
                  }
      }

      // go through and start inflating
      // do 1 layer at a time, in case of squeezed neighborhoods and 
      // book counting of WM intersections, etc.
      i = ROI_make_inflate( Dim, 
                            INFL_NUM,
                            SKEL_STOP,
                            NEIGHBOR_LIMIT,
                            HAVE_MASK,
                            MASK,
                            DATA,
                            SKEL,
                            COUNT_GM,
                            INV_LABELS_GM );


      if(HAVE_PREINF) { 
         // get rid of last vestiges of WM
      
         for( m=0 ; m<Dim[3] ; m++ )
            for( k=0 ; k<Dim[2] ; k++ ) 
               for( j=0 ; j<Dim[1] ; j++ ) 
                  for( i=0 ; i<Dim[0] ; i++ ) 
                     if( SKEL[i][j][k]) 
                        DATA[i][j][k][m] = 0;
      }
   
      // **************************************************************
      // **************************************************************
      //                 LABEL stuff
      // **************************************************************
      // **************************************************************
      // nov, 2014:  labeltable stuff
      ROI_STR_LABELS = (char ***) calloc( Dim[3], sizeof(char **) );
      for ( i=0 ; i<Dim[3] ; i++ ) 
         ROI_STR_LABELS[i] = (char **) calloc( NROI_GM[i]+1, sizeof(char *) );
      for ( i=0 ; i<Dim[3] ; i++ ) 
         for ( j=0 ; j<NROI_GM[i]+1 ; j++ ) 
            ROI_STR_LABELS[i][j] = (char *) calloc( 100 , sizeof(char) );
      if( ROI_STR_LABELS == NULL ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(123);
      }
   
      // Nov 2014:  Labeltable stuff
      // check refset for table
      if(insetREF)
         if (insetREF->Label_Dtable = DSET_Label_Dtable(insetREF) ) {
            if ((LabTabStr = Dtable_to_nimlstring( DSET_Label_Dtable(insetREF),
                                                   "VALUE_LABEL_DTABLE"))) {
               // fprintf(stdout,"%s", LabTabStr);
               if (!(roi_dtable = Dtable_from_nimlstring(LabTabStr))) {
                  ERROR_exit("Could not parse labeltable.");
               }
            } 
            else {
               INFO_message("No label table from '-refset'.");
            }
         }
   
      bb = Make_ROI_Output_Labels( ROI_STR_LABELS,
                                   ROI_LABELS_GM, 
                                   Dim[3],
                                   NROI_GM,
                                   roi_dtable, 
                                   DUMP_with_LABELS);
   
      for( i=0 ; i<Dim[3] ; i++) 
         if( NROI_GM[i]>MAXNROI )
            MAXNROI = NROI_GM[i];
   
      // **************************************************************
      // **************************************************************
      //                 Store and output GMI info
      // **************************************************************
      // **************************************************************

      temp_arr2 = calloc( Dim[3],sizeof(temp_arr2));  // XYZ components
      for(i=0 ; i<Dim[3] ; i++) 
         temp_arr2[i] = calloc( Nvox,sizeof(short int) ); 
	
      if( temp_arr2 == NULL ) { 
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(14);
      }

      outsetGMI = EDIT_empty_copy( inset ) ; 
      if( NIFTI_OUT )
         sprintf(prefix_GMI,"%s_GMI.nii.gz",prefix); // jan,2015
      else
         sprintf(prefix_GMI,"%s_GMI",prefix);

      // start labelly stuff
      // Nov 2014
      if( DUMP_with_LABELS ) {
         sprintf(prefix_dtable,"%s_GMI.niml.lt",prefix); 
         sprintf(prefix_dtableGM,"%s_GM.niml.lt",prefix); 

         if( roi_dtable ) {
            // copy dtable
            Dtable_str = Dtable_to_nimlstring(roi_dtable, "VALUE_LABEL_DTABLE");
            new_dt = Dtable_from_nimlstring(Dtable_str);
            free(Dtable_str); Dtable_str=NULL;
         }
         else {
            new_dt = new_Dtable( MAXNROI );
         }
      
         for( mm=0 ; mm<Dim[3] ; mm++) {
            for( bb=1 ; bb<=NROI_GM[mm] ; bb++) {
               snprintf(mini, 50, "%d", ROI_LABELS_GM[mm][bb]);
               if(!(findin_Dtable_a( mini, new_dt ))) {
                  addto_Dtable(mini, ROI_STR_LABELS[mm][bb], new_dt );
               }
            }
         }

         Dtable_str = Dtable_to_nimlstring(new_dt, "VALUE_LABEL_DTABLE");
         destroy_Dtable(new_dt); new_dt = NULL;

         // copy for GM
         Dtable_strGM = strdup(Dtable_str);


         THD_set_string_atr( outsetGMI->dblk , 
                             "VALUE_LABEL_DTABLE" , Dtable_str);
      
         // output for GMI
         if( (fout1 = fopen(prefix_dtable, "w")) == NULL) {
            fprintf(stderr, "Error opening file %s.",prefix_dtable);
            exit(19);
         }
         fprintf(fout1,"%s",Dtable_str);
         fclose(fout1);
         free(Dtable_str); Dtable_str = NULL;
 
         // copy for GM map
      
         THD_set_string_atr( outsetGM->dblk , 
                             "VALUE_LABEL_DTABLE" , Dtable_strGM);

         if( (fout1 = fopen(prefix_dtableGM, "w")) == NULL) {
            fprintf(stderr, "Error opening file %s.",prefix_dtableGM);
            exit(19);
         }
         fprintf(fout1,"%s",Dtable_strGM);
         fclose(fout1);
         free(Dtable_strGM); Dtable_strGM = NULL;


         // end labelly stuff
      }

      THD_load_statistics(outsetGM);
      tross_Make_History("3dROIMaker", argc, argv, outsetGM);
      THD_write_3dim_dataset(NULL, NULL, outsetGM, True);


      EDIT_dset_items( outsetGMI,
                       ADN_datum_all , MRI_short , 
                       ADN_brick_fac, NULL,
                       ADN_prefix    , prefix_GMI ,
                       ADN_none ) ;
	
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetGMI)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(outsetGMI));
		
      for( m=0 ; m<Dim[3] ; m++ ) {
         idx=0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  if( DATA[i][j][k][m] )
                     temp_arr2[m][idx] = DATA[i][j][k][m]-RESCALES[m];
                  idx+=1;
               }
         EDIT_substitute_brick(outsetGMI, m, MRI_short, temp_arr2[m]); 
         temp_arr2[m]=NULL; // to not get into trouble...
      }

      THD_load_statistics(outsetGMI);
      tross_Make_History("3dROIMaker", argc, argv, outsetGMI);
      THD_write_3dim_dataset(NULL, NULL, outsetGMI, True);

      for( m=0 ; m<Dim[3] ; m++ )
         free(temp_arr2[m]);
      free(temp_arr2);
	
      INFO_message("GMI map is done.");

      if( DUMP_with_LABELS )
         INFO_message("Put labeltables on both the GM and GMI files.");
      else
         INFO_message("Did *not* put labeltables on both the GM and GMI files "
                      "(but could have)");


      // ************************************************************
      // ************************************************************
      //                    Freeing
      // ************************************************************
      // ************************************************************
		
      for ( i=0 ; i<Dim[3] ; i++ ) 
         for ( j=0 ; j<NROI_GM[i]+1 ; j++ ) 
            free(ROI_STR_LABELS[i][j]);
      for ( i=0 ; i<Dim[3] ; i++ ) 
         free(ROI_STR_LABELS[i]);
      free(ROI_STR_LABELS);
      
      if(LabTabStr)
         free(LabTabStr); 
      if(roi_dtable)
         free(roi_dtable);
      
      for( i=0 ; i<Dim[3] ; i++) {
         for ( j = 0 ; j<NROI_GM[i]+1 ; j++ ) 
            free(COUNT_GM[i][j]);
         free(COUNT_GM[i]);
         free(ROI_LABELS_GM[i]);
         free(INV_LABELS_GM[i]);
      }
      free(ROI_LABELS_GM);
      free(INV_LABELS_GM);
      free(COUNT_GM);
      free(NROI_GM);
      free(INVROI_GM);


   }


	DSET_delete(inset);
	free(inset);
	DSET_delete(insetREF);
	DSET_delete(outsetGM);
	DSET_delete(outsetGMI);
	DSET_delete(MASK);
	free(insetREF);
	free(outsetGM);
	free(outsetGMI);
	DSET_delete(insetSKEL);
	free(insetSKEL);
	DSET_delete(insetCSF_SKEL);
	free(insetCSF_SKEL);
	free(MASK);

	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			for( k=0 ; k<Dim[2] ; k++) {
				free(DATA[i][j][k]);
				free(inpmap[i][j][k]);
         }
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) {
			free(DATA[i][j]);
			free(inpmap[i][j]);
			free(SKEL[i][j]);
			free(CSF_SKEL[i][j]);
		}
	for( i=0 ; i<Dim[0] ; i++) {
		free(DATA[i]);	
      free(inpmap[i]);	
		free(SKEL[i]);
		free(CSF_SKEL[i]);
	}
	free(DATA);
	free(inpmap);
	free(SKEL);
	free(CSF_SKEL);

   if( HAVE_PREINF ) {
      for( i=0 ; i<Dim[0] ; i++) 
         for( j=0 ; j<Dim[1] ; j++) 
            free(invSKEL[i][j]);
      for( i=0 ; i<Dim[0] ; i++) 
         free(invSKEL[i]);
      free(invSKEL);   
   }      

	if(HAVEREF>0) {

		for( i=0 ; i<HAVEREF ; i++) {
			free(ROI_LABELS_REF[i]);
			free(INV_LABELS_REF[i]);
			free(N_olap_RI[i]);
			free(N_olap_IR[i]);
			for ( j = 0 ; j<NROI_REF[i]+1 ; j++ ) 
				for ( k = 0 ; k<NROI_IN[i]+1 ; k++ ) 
					free(OLAP_RI[i][j][k]);
			for ( j = 0 ; j<NROI_REF[i]+1 ; j++ ) 
				free(OLAP_RI[i][j]);
			free(OLAP_RI[i]);
		}
		free(ROI_LABELS_REF);
		free(INV_LABELS_REF);
		free(OLAP_RI);
		free(N_olap_RI);
		free(N_olap_IR);
		
		free(NROI_REF);
		free(NROI_REF_b);
		free(INVROI_REF);
		free(EXTRA_LAB);
      
      free(set_REFSCAL);
      DSET_delete(set_REFSCAL);
	}
	free(RESCALES);


	for( i=0 ; i<Dim[3] ; i++) {
		free(ROI_LABELS_pre[i]);
	}
	free(ROI_LABELS_pre);
	free(NROI_IN);
	free(NROI_IN_b);
	free(N_thr);
	free(relab_vox);

	free(Dim); // need to free last because it's used for other arrays...
	free(prefix);

	return 0;
}

