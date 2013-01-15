/* 
   Turn ROIs into prep for tractography, written by PA Taylor
   (Oct/Nov, 2012).

	Main goal: take maps from from ICA, correlation, et al. and
	threshold to make GM-ROIs; then that latter and make into ROIs for
	tractography (most likely prob. tractography to define WM-ROIs)

	Inflation/detection for voxels sharing face/edge, but not only
	vertex.

	Dec. 2012: 
	     fixed bug about thresholds,
        include maskability,
	     rename outputs, `*_WM' -> `*_GMI'.

	Jan. 2013:
	     csf_skel option
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <3ddata.h>    

#define DEP (1)              // search rad of defining ROIs
#define BASE_DVAL (-1)       // intermed values for ROI-finding
#define ALLOWED_NROI (150)   // buffer size for array of refset...

void usage_ROIMaker(int detail) 
{
	printf(
"\n"
"  ROIMaker, written by PA Taylor (Nov, 2012).\n\n"
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
"  for guiding tractography, such as with 3dProbTrackID.\n"
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
"                       ROI in question.\n"
"     -volthr   MINVOL :integer number representing minimum size a cluster of\n"
"                       voxels must have in order to remain a GM ROI after \n"
"                       the values have been thresholded.  Number might be\n"
"                       estimated with 3dAlphaSim, or otherwise, to reduce\n"
"                       number of `noisy' clusters.\n"
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
"\n"
"  + EXAMPLE:\n"
"      3dROIMaker -inset CORR_VALUES+orig. -thresh 0.6 -prefix ROI_MAP \\\n"
"            -volthr 100 -inflate 2 -wm_skel WM_T1+orig. -skel_stop .\n"
"\n"
"\n");
	return;
}


int main(int argc, char *argv[]) {
	int i,j,k,m,ii,jj,kk,mm,bb,n;
	int X,Y,Z;
	int iarg=0;
	THD_3dim_dataset *inset=NULL;
	char in_name[300];
	float THR=0.;
	char *prefix="NAME_ROIMaker";
	THD_3dim_dataset *insetREF=NULL;
	THD_3dim_dataset *insetSKEL=NULL;
	THD_3dim_dataset *insetCSF_SKEL=NULL;
	THD_3dim_dataset *outsetGM=NULL, *outsetGMI=NULL;

	THD_3dim_dataset *MASK=NULL;
	char in_mask[300];
	int HAVE_MASK=0;


	char in_REF[300];
	char prefix_GM[300];
	char prefix_GMI[300];
	char voxel_order[4]="---";

	int HAVEREF = 0; // switch if an external ref file for ROI labels is present
	int HAVESKEL = 0; // switch if an external ref file for ROI labels is present
	int HAVE_CSFSKEL = 0; // similar to above...
	int SKEL_STOP=0; // switch if inflation will be stopped in WM skeleton
	int TRIM_OFF_WM=0; // switch if WM_skel will be used to trim initial input map
	int idx = 0;
	int VOLTHR = 0; // switch about thresholding GM ROI volume size
	int val=0,Nvox=0;
	int max_nroi=0, index1=0;
	int found_this_iter=0, KEEP_GOING=1;

	int ****DATA=NULL;
	short int ***SKEL=NULL;
	short int ***CSF_SKEL=NULL;
	int *N_thr=NULL, *relab_vox=NULL; // num of ROI vox per brik, pre-thr
	int *VOX=NULL;
	short int **temp_arr=NULL,**temp_arr2=NULL;
	int **ROI_LABELS_pre=NULL;
	int **list1=NULL; // growing list of vox while investigating ROI
	int investigated=0, found=0;
	int **N_refvox_R=NULL;
	float SKEL_THR=0.5;// default, such as if input SKEL is a mask

	int *Dim=NULL; 
	int *NROI_IN=NULL,*NROI_IN_b=NULL,*INVROI_IN=NULL; // num of rois per brik, and inv labels
	int **ROI_LABELS_REF=NULL, **INV_LABELS_REF=NULL; // allow labels to be non-consecutive.
	int *NROI_REF=NULL,*NROI_REF_b=NULL,*INVROI_REF=NULL;
	int ****OLAP_RI=NULL; // info of overlap WRT ref and T inset
	int **N_olap_RI=NULL,**N_olap_IR=NULL; // nums of rois per olap
	int *EXTRA_LAB=NULL; // count inROIs unmatched to any refROI
	int INFL_NUM=0; // number of vox to inflate ROIs by

	// stuff for part 2: allow labels to be non-consecutive.
	int **ROI_LABELS_GM=NULL, **INV_LABELS_GM=NULL; 
	int *NROI_GM=NULL,*INVROI_GM=NULL;
	int ***COUNT_GM=NULL;


	mainENTRY("3dROIMaker"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

	INFO_message("version: NU");
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
			inset = THD_open_dataset(in_name) ;
			if( (inset == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_name);
			DSET_load(inset); CHECK_LOAD_ERROR(inset);

			Dim[0] = DSET_NX(inset); Dim[1] = DSET_NY(inset); 
			Dim[2] = DSET_NZ(inset); Dim[3]= DSET_NVALS(inset); 
			Nvox = DSET_NVOX(inset) ;
			voxel_order[0]=ORIENT_typestr[inset->daxes->xxorient][0];
			voxel_order[1]=ORIENT_typestr[inset->daxes->yyorient][0];
			voxel_order[2]=ORIENT_typestr[inset->daxes->zzorient][0];

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

			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-refset") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-refset'");
			
			sprintf(in_REF,"%s", argv[iarg]); 
			insetREF = THD_open_dataset(in_REF) ;
			if( (insetREF == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_REF);
			
			DSET_load(insetREF); CHECK_LOAD_ERROR(insetREF);
			HAVEREF = DSET_NVALS(insetREF);

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
			if( (insetSKEL == NULL ))
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
			if( (insetCSF_SKEL == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",argv[iarg]);
			
			DSET_load(insetCSF_SKEL); CHECK_LOAD_ERROR(insetCSF_SKEL);
			HAVE_CSFSKEL = 1;

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


		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
	INFO_message("Value of threshold is: %f",THR);

	// some checks on sizes of arrays/sets
	if(HAVEREF) {
		if((Dim[0] != DSET_NX(insetREF)) || (Dim[1] != DSET_NY(insetREF)) ||
			(Dim[2] != DSET_NZ(insetREF)) )
			ERROR_exit("The xyz-dimensions of refset and inset don't match");
		
		if( Dim[3] == HAVEREF )
			INFO_message("Each subrik of refset will be applied to corresponding inset brik.");
		else
			ERROR_exit("The number of inset and refset briks must match: here, ref=%d, inset=%d",HAVEREF,Dim[3]);

		if( voxel_order[0] != ORIENT_typestr[insetREF->daxes->xxorient][0] ||
			 voxel_order[1] != ORIENT_typestr[insetREF->daxes->yyorient][0] ||
			 voxel_order[2] != ORIENT_typestr[insetREF->daxes->zzorient][0] )
			ERROR_exit("Refset orientation is not %s like the inset.",voxel_order);
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
		INFO_message("*+ You asked to stop inflation at the WM skeleton, but didn't give a skeleton using `-wm_skel'-- will just ignore that.");
		SKEL_STOP=0; //reset
	}

	if( (!HAVESKEL) && TRIM_OFF_WM) {
		INFO_message("*+ You asked to trim GM input with WM skeleton, but didn't give a skeleton using `-wm_skel'-- will just ignore that.");
		TRIM_OFF_WM = 0; //reset
	}


	if (iarg < 3) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
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

	if( (SKEL == NULL) || (CSF_SKEL == NULL)
		 ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(16);
	}

	// make skeleton: either with file input, or just put 1s everywhere.
	idx = 0;
	// should preserve relative ordering of data
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
				if( ((HAVESKEL==1) && (THD_get_voxel(insetSKEL,idx,0)>SKEL_THR)) 
					 || (HAVESKEL==0) ) 
					SKEL[i][j][k] = 1;
				if( ((HAVE_CSFSKEL==1) && (THD_get_voxel(insetCSF_SKEL,idx,0)>0.5)) 
					 || (HAVE_CSFSKEL==0) ) 
					CSF_SKEL[i][j][k] = 1;
				idx+= 1; 			
			}


	N_thr = (int *)calloc(Dim[3],sizeof(int)); // num of init vox per brik,const
	VOX = (int *)calloc(Dim[3],sizeof(int)); // num of vox >thr per brik,var
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
	
	if( (DATA == NULL) || (N_thr == NULL) || (NROI_IN == NULL)
		 || (temp_arr == NULL) || (VOX == NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(14);
	}

	// STEP 1: go through brik by brik to apply thresholding -> prod bin mask
	for( m=0 ; m< Dim[3] ; m++ ) {
		idx = 0;
		// should preserve relative ordering of data
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
					if( (HAVE_MASK==0) || 
						 (HAVE_MASK && ( THD_get_voxel(MASK,idx,0)>0 ) ) )
						if( THD_get_voxel(inset,idx,m) > THR  ) 
							if( !TRIM_OFF_WM || (TRIM_OFF_WM && !SKEL[i][j][k]) )
								if( !HAVE_CSFSKEL 
									 || (HAVE_CSFSKEL && !CSF_SKEL[i][j][k]) )
									{
										// temporary until ROIs are 1st labelled
										DATA[i][j][k][m] = BASE_DVAL;
										N_thr[m]+= 1;
									}
					idx+= 1; 					
				}
	}
	
	// make a supplement array of large possible number of vox to be
	// found in any brik;  will be reused/cleaned after each use
	for( m=0 ; m< Dim[3] ; m++ )
		if( max_nroi < N_thr[m] )
			max_nroi = N_thr[m];

	if(max_nroi==0)
		ERROR_exit("No keeper voxels in ROIs in any brik!");

	list1 = calloc( max_nroi, sizeof(int *) );
	for ( j = 0 ; j < max_nroi ; j++ ) 
		list1[j] = (int *) calloc( 3, sizeof(int) );
	
	list1 = calloc( max_nroi, sizeof(int *) );
	for ( j = 0 ; j < max_nroi ; j++ ) 
		list1[j] = (int *) calloc( 3, sizeof(int) );
	// as big as could be needed for labelling...  probably way bigger
	// than necessary (could buffer/realloc, but just start with this, 
	// at least for time being)
	ROI_LABELS_pre = calloc( Dim[3],sizeof(ROI_LABELS_pre));  
	for(i=0 ; i<Dim[3] ; i++) 
		ROI_LABELS_pre[i] = calloc(2*N_thr[i],sizeof(int));

	if( (list1 == NULL) || (ROI_LABELS_pre == NULL)
		 ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(15);
	}		
	
	// ****************************************************************
	// ****************************************************************
	//                    labelling inset
	// ****************************************************************
	// ****************************************************************
	
	// STEP 2: label separate ROIs with ints
	for( m=0 ; m<Dim[3] ; m++ ) {
		// make it bigger than any final index possibly could be
		index1 = N_thr[m]; 

		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
					if( DATA[i][j][k][m] == BASE_DVAL ) {
						// now we go into separate action to fill out other ones
						investigated = 0;
						NROI_IN[m]+= 1; // keep track of # of rois per brik
						index1+= 1;
						DATA[i][j][k][m] = index1;
						found = 1;
						list1[0][0]=i; list1[0][1]=j; list1[0][2]=k;
						
						while(investigated<found) {
							// shorter notation for use in loops...
							X=list1[investigated][0]; 
							Y=list1[investigated][1];
							Z=list1[investigated][2];
							// investigate its neighbors
							for( ii=-DEP ; ii<=DEP ; ii++)
								for( jj=-DEP ; jj<=DEP ; jj++)
									for( kk=-DEP ; kk<=DEP ; kk++)
										// need to share face or edge, not only vertex
										if(abs(ii)+abs(jj)+abs(kk)<3)
											// keep in bounds
											if((0 <= X+ii) && (X+ii < Dim[0]) && 
												(0 <= Y+jj) && (Y+jj < Dim[1]) && 
												(0 <= Z+kk) && (Z+kk < Dim[2])) {
												// if a neighbor has value of -one...
												if( DATA[X+ii][Y+jj][Z+kk][m] == BASE_DVAL) {
													DATA[X+ii][Y+jj][Z+kk][m] = index1; // change
													list1[found][0] = X+ii; // keep the coors
													list1[found][1] = Y+jj;
													list1[found][2] = Z+kk;
													found+= 1; // add to list to investigate,
												}
											}
							investigated+=1;
						}
						
						// if one is thresholding based on volume size, 
						// and if this one volume fails:
						if( (VOLTHR>0) && (found<VOLTHR) ) {
							// erase these, reset val to zero; careful of mm and m...
							for( mm=0 ; mm<found ; mm++ )
								DATA[list1[mm][0]][list1[mm][1]][list1[mm][2]][m] = 0;
							index1-=1; // reset index value
							NROI_IN[m]-=1; // reset index value
						}
						else {
							ROI_LABELS_pre[m][NROI_IN[m]] = found; // !! hold value of Nvox in the ROI; ROI_LABEL for the ith NROI is i+N_thr[m].... //index1; // Xth ROI gets value Y
							VOX[m]+=found;
						}

						// so, we found a 1, grew it out and changed all assoc.
						// indices-- keep track of how many voxels there were...
						// numperroi1(index1) = found;
						for( ii=0 ; ii<found ; ii++) // clean up list for use again
							for( j=0 ; jj<3 ; jj++ )
								list1[ii][jj]=0;
					}
				}
	}
	
	// Step 3: do matching, or subtract values back to where they should be
	if( HAVEREF > 0 ) {
				
		// Step 3A-1: SET UP MATRICES FOR THIS STUFF
		NROI_REF = (int *)calloc(HAVEREF, sizeof(int)); 
		NROI_REF_b = (int *)calloc(HAVEREF, sizeof(int)); // for counting
		INVROI_REF = (int *)calloc(HAVEREF, sizeof(int)); 
		if( (NROI_REF == NULL) || (NROI_REF == NULL) || (NROI_REF_b == NULL) 
			 ) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(122);
		}
		
		for( i=0 ; i<HAVEREF ; i++) 
			INVROI_REF[i] = (int) THD_subbrick_max(insetREF, i, 1);//??inset-->fixed
		
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
		bb = ViveLeRoi(insetREF, 
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

			idx=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
						if( THD_get_voxel(insetREF,idx,m)>0 ) {
							// for ease/shortcut, define X, the compact form of
							// the refROI label index, and Y, the inset ROI smallval
							X = INV_LABELS_REF[m][(int) THD_get_voxel(insetREF,idx,m)];
							N_refvox_R[m][X]+=1;
							if( DATA[i][j][k][m]>0 ) {
								Y = DATA[i][j][k][m] - N_thr[m];
								OLAP_RI[m][X][Y][0]+=1;
							}
						}
						idx++;
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
			idx=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
						if( DATA[i][j][k][m]>0 ) {
							Y = DATA[i][j][k][m] - N_thr[m];
							if( OLAP_RI[m][0][Y][1]<0 ) {// was neg val when stored
								DATA[i][j][k][m] = -OLAP_RI[m][0][Y][1]; 
								relab_vox[m]++;
							}
							for( ii=1 ; ii<=NROI_REF[m]; ii++ ) {
								if( (OLAP_RI[m][ii][Y][1]==3) ) {// relabel
									DATA[i][j][k][m] = ROI_LABELS_REF[m][ii]; // ref #
									relab_vox[m]++;
								}
								else if( (OLAP_RI[m][ii][Y][1]==4) &&
											(THD_get_voxel(insetREF,idx,m)>0) ) {// relabel
									DATA[i][j][k][m] = THD_get_voxel(insetREF,idx,m);
									relab_vox[m]++;
									break;
								}
							}
						}
						idx++;
					}
		}

		// ok, at this point, all *direct* overlaps and non-overlaps should 
		// have been claimed.  Now, we go through and grow each region 
		// (labelled with a 4) into ones with index value larger than N_thr[m].  
		
		for( m=0 ; m<Dim[3] ; m++ ) {
			KEEP_GOING = 1;
			while( KEEP_GOING==1 ) {
				
				found_this_iter=0;
				idx=0;
				for( k=0 ; k<Dim[2] ; k++ ) 
					for( j=0 ; j<Dim[1] ; j++ ) 
						for( i=0 ; i<Dim[0] ; i++ ) {
							// find ones whose neighbor's might need to be relabelled
							// take data values which are only between (0, N_thr), 
							// which at this point should just be ref values-- temp 
							// inlabel values are >N_thr.
							if( (DATA[i][j][k][m]>0) &&
								 (DATA[i][j][k][m]<N_thr[m]) ) { 
								// check neighbors
								for( ii=-DEP ; ii<=DEP ; ii++)
									for( jj=-DEP ; jj<=DEP ; jj++)
										for( kk=-DEP ; kk<=DEP ; kk++)
											// need to share face or edge, not only vertex
											if(abs(ii)+abs(jj)+abs(kk)<3)
												
												// keep in bounds
												if((0 <= i+ii) && (i+ii < Dim[0]) && 
													(0 <= j+jj) && (j+jj < Dim[1]) && 
													(0 <= k+kk) && (k+kk < Dim[2])) {
													// grow if ngb>=N_thr; give temp value
													// of -[value it will have]
													if( DATA[i+ii][j+jj][k+kk][m]>=N_thr[m]) {
														DATA[i+ii][j+jj][k+kk][m] = -DATA[i][j][k][m];
														found_this_iter++;
													}
												}
							}
							idx++;
						}
				
				if( found_this_iter==0 )
					KEEP_GOING=0;
				else {
					relab_vox[m]+= found_this_iter;
					
					for( k=0 ; k<Dim[2] ; k++ ) 
						for( j=0 ; j<Dim[1] ; j++ ) 
							for( i=0 ; i<Dim[0] ; i++ ) 
								if( DATA[i][j][k][m]<0 )
									DATA[i][j][k][m]*= -1;
				}
				if( relab_vox[m]==N_thr[m] )
					KEEP_GOING=0;				
			}
			
			// final part, relabel the inROIs which are unmatched with the 
			// ref ones
			found_this_iter=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if( DATA[i][j][k][m]>N_thr[m] ) {
							DATA[i][j][k][m]-= NROI_IN[m] + N_thr[m];
							DATA[i][j][k][m]+= ROI_LABELS_REF[m][NROI_REF[m]];
							found_this_iter+=1 ;
						}
			relab_vox[m]+= found_this_iter;
		}
	}
	else { // simple case, just keep labels in order they were found.
		for( m=0 ; m<Dim[3] ; m++ ) 
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if( DATA[i][j][k][m]>0 ) 
							DATA[i][j][k][m]-= N_thr[m];
	}
	
	// **************************************************************
	// **************************************************************
	//                 Store and output GM info
	// **************************************************************
	// **************************************************************
	
	outsetGM = EDIT_empty_copy( inset ) ; 
	sprintf(prefix_GM,"%s_GM",prefix);
	
	EDIT_dset_items( outsetGM,
						  ADN_datum_all , MRI_short , 
						  ADN_prefix    , prefix_GM ,
						  ADN_none ) ;
	
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetGM)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetGM));
	
	for( m=0 ; m<Dim[3] ; m++ ) {
		idx=0;
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
					temp_arr[m][idx] = DATA[i][j][k][m];
					idx+=1;
				}
		EDIT_substitute_brick(outsetGM, m, MRI_short, temp_arr[m]); 
		temp_arr[m]=NULL; // to not get into trouble...
	}
	
	THD_load_statistics(outsetGM);
	tross_Make_History("3dROIMaker", argc, argv, outsetGM);
	THD_write_3dim_dataset(NULL, NULL, outsetGM, True);
	
	for( m=0 ; m<Dim[3] ; m++ )
		free(temp_arr[m]);
	free(temp_arr);
	
	INFO_message("GM map is done.");

	// **************************************************************
	// **************************************************************
	//                 Expand GM maps
	// **************************************************************
	// **************************************************************

	
	// find all index numbers for GM systematically.
	NROI_GM = (int *)calloc(Dim[3], sizeof(int)); 
	INVROI_GM = (int *)calloc(Dim[3], sizeof(int)); 
	if( (NROI_GM == NULL) || (NROI_GM == NULL) 
		 ) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
	
	for( i=0 ; i<Dim[3] ; i++) 
		INVROI_GM[i] = (int) THD_subbrick_max(outsetGM, i, 1);//??inset-->fixed
	
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

	if( (ROI_LABELS_GM == NULL) || (ROI_LABELS_GM == NULL) 
		 || (COUNT_GM == NULL)) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(123);
	}

	bb = ViveLeRoi(outsetGM, 
						ROI_LABELS_GM, INV_LABELS_GM, 
						NROI_GM,       INVROI_GM);
	if( bb != 1)
		ERROR_exit("Problem loading/assigning GM labels");
	
	// preliminary setting up of COUNT_GM
	for( m=0 ; m<Dim[3] ; m++ ) {
		for( i=0 ; i<NROI_GM[m]+1 ; i++ ) 
			ROI_LABELS_GM[m][i] = 1; //switch to keep adding to it
		
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

	for( n=0 ; n<INFL_NUM ; n++) {
		for( m=0 ; m<Dim[3] ; m++ ) {
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if(DATA[i][j][k][m]>0) {
							// now check surroundings
							if( !(SKEL_STOP && SKEL[i][j][k]))
								for( ii=-DEP ; ii<=DEP ; ii++)
									for( jj=-DEP ; jj<=DEP ; jj++)
										for( kk=-DEP ; kk<=DEP ; kk++)
											// need to share face or edge, not only vertex
											if(abs(ii)+abs(jj)+abs(kk)<3)
												
												// keep in bounds
												if((0 <= i+ii) && (i+ii < Dim[0]) && 
													(0 <= j+jj) && (j+jj < Dim[1]) && 
													(0 <= k+kk) && (k+kk < Dim[2])) {
													idx = THREE_TO_IJK(i+ii,j+jj,k+kk,Dim[0],Dim[0]*Dim[1]);
													if( (HAVE_MASK==0) || 
														 (HAVE_MASK && 
														  ( THD_get_voxel(MASK,idx,0)>0 ) ) ) {
														
														// grow if ngb=0; give temp value
														// of -[value it will have]
														if( DATA[i+ii][j+jj][k+kk][m]==0) {
															DATA[i+ii][j+jj][k+kk][m] = 
																-DATA[i][j][k][m];
															//found_this_iter++;
														}
													}
												}
						}
			
			// and now convert the layer to being part of the ROI
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if(DATA[i][j][k][m]<0) {
							DATA[i][j][k][m] = -DATA[i][j][k][m];
							COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][1]++;
							if(SKEL[i][j][k])
								COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][2]++;
						}
			
		}

	}


	// **************************************************************
	// **************************************************************
	//                 Store and output GMI info
	// **************************************************************
	// **************************************************************

	temp_arr2 = calloc( Dim[3],sizeof(temp_arr2));  // XYZ components
	for(i=0 ; i<Dim[3] ; i++) 
		temp_arr2[i] = calloc( Nvox,sizeof(short int) ); 
	
	if( (temp_arr2 == NULL)  ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(14);
	}

	outsetGMI = EDIT_empty_copy( inset ) ; 
	sprintf(prefix_GMI,"%s_GMI",prefix);
	EDIT_dset_items( outsetGMI,
						  ADN_datum_all , MRI_short , 
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
					temp_arr2[m][idx] = DATA[i][j][k][m];
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
	
	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************
		
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
			for( k=0 ; k<Dim[2] ; k++) 
				free(DATA[i][j][k]);
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) {
			free(DATA[i][j]);
			free(SKEL[i][j]);
			free(CSF_SKEL[i][j]);
		}
	for( i=0 ; i<Dim[0] ; i++) {
		free(DATA[i]);	
		free(SKEL[i]);
		free(CSF_SKEL[i]);
	}
	free(DATA);
	free(SKEL);
	free(CSF_SKEL);

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
	}
	
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

	for ( j = 0 ; j < max_nroi ; j++ ) 
		free(list1[j]);
	free(list1);
	for( i=0 ; i<Dim[3] ; i++) {
		free(ROI_LABELS_pre[i]);
	}
	free(ROI_LABELS_pre);
	free(NROI_IN);
	free(NROI_IN_b);
	free(N_thr);
	free(relab_vox);
	free(VOX);

	free(Dim); // need to free last because it's used for other arrays...
	free(prefix);

	return 0;
}

