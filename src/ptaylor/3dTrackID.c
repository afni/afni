/* 
   FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (2012)
   
   first draft at AFNIfication, March 2012.

	updated version (switch for output *.trk file origin info; 
	                 memory stuff), Sept. 2012

	updated version, Dec. 2012:
	        + allow non-FA map to define `WM' for tracks,

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
#include <TrackIO.h>
#include <DoTrackit.h>


void usage_TrackID(int detail) 
{
	printf(
"\n"
"  FACTID code, from Taylor, Cho, Lin and Biswal (2012)\n"
"  \n"
"  USAGE: read in data from 3dDWItoDT results; use L1 and V1 for actual\n"
"    algorithm, and include MD and FA for stats\n"
"  \n"
"  COMMAND: 3dTrackID -mask1 FILE -mask2 FILE -input [base of FA/MD/etc.] \\\n"
"           -logic AND|OR|ALL  -prefix NAME {-algopt FILE} {\\}\n"
"           {-verb VERB  -tract_out_mode MODE  -rec_orig}\n"
"  \n\n"
"  + OUTPUTS (named using prefix):  \n"
"    1) TRK file, Trackvis-readable file of tracts\n"
"    2) MAP file, AFNI-format file, recording number of tracts through\n"
"       each voxel\n"
"    3) MASK file, AFNI-format file, with 1 where a tract passed, 0 else\n"
"    4) STATS file, simple statistics from tracts produced (2 cols, 6 rows):\n"
"        [Number of tracts]          [Number of voxels with >=1 tract]\n"
"        [ave number of vox/tract]   [ave phys length/tract]\n"
"        [unweighted mean of FA]     [st dev of FA]\n"
"        [unweighted mean of MD]     [st dev of MD]\n"
"        [unweighted mean of RD]     [st dev of RD]\n"
"        [unweighted mean of L1]     [st dev of L1]\n"
"        \n\n"
"  + TRACTOGRAPHY OPTIONS via {-algopt [file of tractog options}: \n"
"        MinFA       :FA cutoff value, lower bound\n"
"        MaxAng      :max angle (in deg) to propagate between vox\n"
"        MinL        :min physical length (in mm) of tracts to keep\n"
"        SeedPerV[0] :Number of seeds per vox in x direc\n"
"        SeedPerV[1] :Number of seeds per vox in y direc\n"
"        SeedPerV[2] :Number of seeds per vox in z direc\n"
"        M           :number of grads used to scan (not used yet)\n"
"        bval        :non-b=0 value (mm^2/s), e.g., 1000 (not used yet)\n"
"  \n\n"
"  + RUNNING, need to provide:\n"
"    -mask1  MASK1   :mask of ROI1\n"
"    -mask2  MASK2   :mask of ROI2\n"
"    -prefix PREFIX  :output file name part\n"
"    -input  INPREF  :Basename of DTI volumes output by, e.g., 3dDWItoDT.\n"
"                     NB- following volumes are currently expected:\n"
"                     INPREF_V1, INPREF_MD, INPREF_L1, and INPREF_FA.\n" 
"    -algopt ALGO    :ASCII file with eight numbers defining parameter \n"
"                     quantities just above. ALGO can also be in NIML format,\n"
"                     which is clearer to read. Use -write_opts to get an\n"
"                     idea of the format. NB- You do not need to use this\n"
"                     option if you want to use the built in defaults. If you\n"
"                     want the values of the defaults, add option -write_opts\n"
"                     to your command.\n"
"    -logic [AND, OR, ALL] :Set the combinatorial logic of two ROIs;\n"
"                     for just one ROI, make both masks the same file and\n"
"                     use, e.g., AND.  Use ALL to keep all tracks meeting\n"
"                     minimum length criterion.\n"
"                     For now, you still have to specify a mask even if you\n"
"                     use ALL (complain to Paul Taylor about this).\n"
"    -rec_orig       :Record dataset origin in the header of the *.trk file.\n"
"                     As of Sept. 2012, TrackVis doesn't use this info so it\n"
"                     wasn't included, but if you might want to map your \n"
"                     *.trk file later, then use the switch as the datasets's\n"
"                     Origin is necessary info for the mapping (the default\n"
"                     image in TrackVis might not pop up in the center of the\n"
"                     viewing window, though, just be aware).  NB: including\n"
"                     the origin might become default at some point in time.\n"
"    -extra_set SET  :if you want to use a non-FA derived definition for the\n"
"                     WM skeleton in which tracts run, you can input one, and\n"
"                     then the threshold in the ALGO file refers to values\n"
"                     in this map.\n"
"    -write_opts     :Write out all the option values into PREFIX.niml.opts.\n" 
"    -verb VERB      :Verbosity level, default is 0.\n"
"    -tract_out_mode MODE :Type of output format. Choose from:\n"
"                     NI_fast_binary, NI_fast_text,\n"
"                     NI_slow_binary, NI_slow_text\n"
"                     At the moment, Trackvis formatted file is also written\n"
"                     out, regardless of what MODE is set to be. Default mode\n"
"                     is: NI_fast_binary.\n"
" \n\n" 
"  + EXAMPLE:\n"
"      Download and install this archive:\n"
"         curl -O http://..../FACTID_draft.tar.gz \n"
"      tar xvzf FACTID_draft.tar.gz\n"
"      3dTrackID \\\n"
"         -mask1 TEST_FILES/DTI/mask_ballF+orig \\\n"
"         -mask2 TEST_FILES/DTI/mask_ballG+orig \\\n"
"         -prefix TEST_FILES/DTI/o.TRACK_ballFG \\\n"
"         -input TEST_FILES/DTI/DT \\\n"
"         -algopt TEST_FILES/ALGOPTS.dat \\\n"
"         -logic AND\n"
"  \n\n"
"  If you use this program, please reference:\n"
"    Taylor PA, Cho K-H, Lin C-P, Biswal BB (2012) Improving DTI\n"
"    Tractography by including Diagonal Tract Propagation. PLoS ONE\n"
"    7(9): e43415. \n"
"   \n" );
	return;
}

int main(int argc, char *argv[]) {
	int i,j,k,m,n,aa,ii,jj,kk,mm,rr;
	int iarg;
	int nmask1=0;
	int nmask2=0;
	THD_3dim_dataset *insetFA = NULL, *insetV1 = NULL, 
		*insetMD = NULL, *insetL1 = NULL;
	THD_3dim_dataset *insetEXTRA=NULL; 
	THD_3dim_dataset *mset2=NULL; 
	THD_3dim_dataset *mset1=NULL; 
	THD_3dim_dataset *outsetMAP=NULL, *outsetMASK=NULL;
	char *prefix="tracky";
	int LOG_TYPE=0;
	char in_FA[300];
	char in_V1[300];
	char in_MD[300];
	char in_L1[300];
	int EXTRAFILE=0; // switch for whether other file is input as WM map

	char OUT_bin[300];
	char OUT_tracstat[300];
	char prefix_mask[300];
	char prefix_map[300];

	// FACT algopts
	FILE *fout0;
	float MinFA=0.2,MaxAngDeg=45,MinL=20.0;
	float MaxAng;
	int SeedPerV[3]={2,2,2};
	int ArrMax=0;
	float tempvmagn;
  
	int Nvox=-1;   // tot number vox
	int Dim[3]={0,0,0}; // dim in each dir
	int Nseed=0,M=30,bval=1000;
	int DimSeed[3]; // number of seeds there will be
	float Ledge[3]; // voxel edge lengths

	int *ROI1, *ROI2;
	short int *temp_arr;
	char *temp_byte; 
	int **Tforw, **Tback;
	int **Ttot;
	float **flTforw, **flTback;
	float ****coorded;
	int ****INDEX;
	int len_forw, len_back; // int count of num of squares through
	float phys_forw[1], phys_back[1];
	int idx;

	float ave_tract_len, ave_tract_len_phys;
	int inroi1, inroi2, KEEPIT; // switches for detecting
	int in[3]; // to pass to trackit
	float physin[3]; // also for trackit, physical loc, 
	int totlen; 
	float totlen_phys;
	int Numtract;

	int READS_in;
	float READS_fl;
	int end[2][3];
	int test_ind[2][3];

	int  roi3_ct=0, id=0;
	float roi3_mu_MD = 0.,roi3_mu_RD = 0.,roi3_mu_L1 = 0.,roi3_mu_FA = 0.;  
	float roi3_sd_MD = 0.,roi3_sd_RD = 0.,roi3_sd_L1 = 0.,roi3_sd_FA = 0.;  
	float tempMD,tempFA,tempRD,tempL1;
	char dset_or[4] = "RAI";
	THD_3dim_dataset *dsetn;
	int TV_switch[3] = {0,0,0};
	TAYLOR_BUNDLE *tb=NULL;
	TAYLOR_TRACT *tt=NULL;
	char *mode = "NI_fast_binary";
	NI_element *nel=NULL;
	int dump_opts=0;

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
	
  	// for testing names...
	char *postfix[4]={"+orig.HEAD\0",".nii.gz\0",".nii\0","+tlrc.HEAD\0"};
  	int FOUND =-1;
	int RECORD_ORIG = 0; 
	float Orig[3] = {0.0,0.0,0.0};

	mainENTRY("3dTrackID"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

	INFO_message("version: MU");

	/** scan args **/
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
			dump_opts=1;
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-rec_orig") == 0) {
			RECORD_ORIG=1;
			iarg++ ; continue ;
		}
    
		if( strcmp(argv[iarg],"-tract_out_mode") == 0) {
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
    
		if( strcmp(argv[iarg],"-mask1") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-mask1'") ;
			mset1 = THD_open_dataset( argv[iarg] ) ;
			if( mset1 == NULL ) 
				ERROR_exit("Can't open mask1 dataset '%s'", argv[iarg]) ;
			DSET_load(mset1) ; CHECK_LOAD_ERROR(mset1) ;
			nmask1 = DSET_NVOX(mset1) ;

			iarg++ ; continue ;
		}
		if( strcmp(argv[iarg],"-mask2") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-mask2'") ;
			mset2 = THD_open_dataset( argv[iarg] ) ;
			if( mset2 == NULL ) 
				ERROR_exit("Can't open mask2 dataset '%s'",
							  argv[iarg]) ;
			DSET_load(mset2) ; CHECK_LOAD_ERROR(mset2) ;
			nmask2 = DSET_NVOX(mset2) ;
		
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

			for( i=0 ; i<4 ; i++) {
				sprintf(in_FA,"%s_FA%s", argv[iarg],postfix[i]); 
				if(THD_is_ondisk(in_FA)) {
					FOUND = i;
					break;
				}
			}
			insetFA = THD_open_dataset(in_FA) ;
			if( (insetFA == NULL ) || (FOUND==-1))
				ERROR_exit("Can't open dataset '%s': for FA.",in_FA);
			
			DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
			Nvox = DSET_NVOX(insetFA) ;
			Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); 
			Dim[2] = DSET_NZ(insetFA); 
			Ledge[0] = fabs(DSET_DX(insetFA)); Ledge[1] = fabs(DSET_DY(insetFA)); 
			Ledge[2] = fabs(DSET_DZ(insetFA)); 
			Orig[0] = DSET_XORG(insetFA); Orig[1] = DSET_YORG(insetFA);
			Orig[2] = DSET_ZORG(insetFA);

			// check tot num vox match (as proxy for dims...)
			if( (Nvox != nmask1) || (Nvox != nmask2) )
				ERROR_exit("Input dataset does not match both mask volumes!");
		
      
			// this stores the original data file orientation for later use,
			// as well since we convert everything to RAI temporarily, as
			// described below
			header1.voxel_order[0]=ORIENT_typestr[insetFA->daxes->xxorient][0];
			header1.voxel_order[1]=ORIENT_typestr[insetFA->daxes->yyorient][0];
			header1.voxel_order[2]=ORIENT_typestr[insetFA->daxes->zzorient][0];
			for( i=0 ; i<3 ; i++) {
				header1.dim[i] = Dim[i];
				header1.voxel_size[i] = Ledge[i];
				// will want this when outputting file later for TrackVis.
				TV_switch[i] = !(dset_or[i]==header1.voxel_order[i]);
			}
			dset_or[3]='\0';
      
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

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-algopt") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-algopt'");
		
			if (!(nel = ReadTractAlgOpts(argv[iarg]))) {
				ERROR_message("Failed to read options in %s\n", argv[iarg]);
				exit(19);
			}
			if (NI_getTractAlgOpts(nel, &MinFA, &MaxAngDeg, &MinL, 
										  SeedPerV, &M, &bval)) {
				ERROR_message("Failed to get options");
				exit(1);
			}
			NI_free_element(nel); nel=NULL;
      
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-logic") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-logic'");

			INFO_message("ROI logic type is: %s",argv[iarg]);
			if( strcmp(argv[iarg],"AND") == 0 ) 
				LOG_TYPE = 1;
			else if( strcmp(argv[iarg],"OR") == 0 ) 
				LOG_TYPE = 0;
			else if( strcmp(argv[iarg],"ALL") == 0 )
				LOG_TYPE = -1;
			else 
				ERROR_exit("Illegal after '-logic': need 'OR' or 'AND'");
			iarg++ ; continue ;
		}
    
		//@@
		if( strcmp(argv[iarg],"-extra_set") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-extra_set'");
			EXTRAFILE = 1; // switch on

			insetEXTRA = THD_open_dataset(argv[iarg]);
			if( (insetEXTRA == NULL ) )
				ERROR_exit("Can't open dataset '%s': for extra set.",argv[iarg]);
			DSET_load(insetEXTRA) ; CHECK_LOAD_ERROR(insetEXTRA) ;

			if( !((Dim[0] == DSET_NX(insetEXTRA)) && (Dim[1] == DSET_NY(insetEXTRA)) && (Dim[2] == DSET_NZ(insetEXTRA))))
				ERROR_exit("Dimensions of extra set '%s' don't match those of the DTI prop ones ('%s', etc.).",argv[iarg], in_FA);
			
			iarg++ ; continue ;
		}


		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
	 
	if (iarg < 4) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}
	 
	if (dump_opts) {
      nel = NI_setTractAlgOpts(NULL, &MinFA, &MaxAngDeg, &MinL, 
										 SeedPerV, &M, &bval);
      WriteTractAlgOpts(prefix, nel);
      NI_free_element(nel); nel=NULL;
	}
	 
        
	// Process the options a little 
	for( i=0 ; i<3 ; i++)
		DimSeed[i] = Dim[i]*SeedPerV[i];
	Nseed = Nvox*SeedPerV[0]*SeedPerV[1]*SeedPerV[2];
	 
	// convert to cos of rad value for comparisons, instead of using acos()
	MaxAng = cos(CONV*MaxAngDeg); 
	 
	// switch to add header-- option for now, added Sept. 2012
	// for use with map_TrackID to map tracks to different space
	if(RECORD_ORIG) {
		for( i=0 ; i<3 ; i++)
			header1.origin[i] = Orig[i];
	}
	 
	// at some point, we will have to convert indices into
	// pseudo-locations; being forced into this choice means that
	// different data set orientations would be represented differently
	// and incorrectly in some instances... so, for now, we'll resample
	// everything to RAI, and then resample back later.  guess this will
	// just slow things down slightly.
	 
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
		
		dsetn = r_new_resam_dset(mset2, NULL, 0.0, 0.0, 0.0,
										 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
		DSET_delete(mset2); 
		mset2=dsetn;
		dsetn=NULL;

		if(EXTRAFILE) {
			dsetn = r_new_resam_dset(insetEXTRA, NULL, 0.0, 0.0, 0.0,
											 dset_or, RESAM_NN_TYPE, NULL, 1, 0);
			DSET_delete(insetEXTRA); 
			insetEXTRA=dsetn;
			dsetn=NULL;
		}


	}
	 
	 

	// ****************************************************************
	// ****************************************************************
	//                    make arrays for tracking
	// ****************************************************************
	// ****************************************************************

	// for temp storage array, just a multiple of longest dimension!
	if(Dim[0] > Dim[1])
		ArrMax = Dim[0] * 4;
	else
		ArrMax = Dim[1] * 4;
	if(4*Dim[2] > ArrMax)
		ArrMax = Dim[2] * 4;

	ROI1 = (int *)calloc(Nvox, sizeof(int)); 
	ROI2 = (int *)calloc(Nvox, sizeof(int)); 
	temp_arr = (short int *)calloc(Nvox, sizeof(short int)); 
	temp_byte = (char *)calloc(Nvox, sizeof(char)); 
	// temp storage whilst tracking
	Tforw = calloc(ArrMax, sizeof(Tforw)); 
	for(i=0 ; i<ArrMax ; i++) 
		Tforw[i] = calloc(3, sizeof(int)); 
	Ttot = calloc(2*ArrMax , sizeof(Ttot)); 
	for(i=0 ; i<2*ArrMax ; i++) 
		Ttot[i] = calloc(3, sizeof(int)); 
	Tback = calloc(ArrMax, sizeof(Tback)); 
	for(i=0 ; i<ArrMax ; i++) 
		Tback[i] = calloc(3, sizeof(int)); 
	// temp storage whilst tracking, physical loc
	flTforw = calloc(ArrMax, sizeof(flTforw)); 
	for(i=0 ; i<ArrMax ; i++) 
		flTforw[i] = calloc(3, sizeof(int)); 
	flTback = calloc(ArrMax,sizeof(flTback)); 
	for(i=0 ; i<ArrMax ; i++) 
		flTback[i] = calloc(3, sizeof(int)); 
	if( (ROI1 == NULL) || (ROI2 == NULL) || (temp_arr == NULL) 
		 || (Tforw == NULL) || (Tback == NULL) || (flTforw == NULL) 
		 || (flTback == NULL) || (Ttot == NULL)) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(12);
	}
  
	coorded = (float ****) calloc( Dim[0], sizeof(float ***) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		coorded[i] = (float ***) calloc( Dim[1], sizeof(float **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			coorded[i][j] = (float **) calloc( Dim[2], sizeof(float *) );
	for ( i=0 ; i<Dim[0] ; i++ ) 
		for ( j=0 ; j<Dim[1] ; j++ ) 
			for ( k= 0 ; k<Dim[2] ; k++ ) //3 comp of V1 and FA
				coorded[i][j][k] = (float *) calloc( 4, sizeof(float) ); 
  
	INDEX = (int ****) calloc( Dim[0], sizeof(int ***) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		INDEX[i] = (int ***) calloc( Dim[1], sizeof(int **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			INDEX[i][j] = (int **) calloc( Dim[2], sizeof(int *) );
	for ( i=0 ; i<Dim[0] ; i++ ) 
		for ( j=0 ; j<Dim[1] ; j++ ) 
			for ( k= 0 ; k<Dim[2] ; k++ ) 
				INDEX[i][j][k] = (int *) calloc( 4,  sizeof(int) );

	// this statement will never be executed if allocation fails above
	if( (INDEX == NULL) || (coorded == NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
  
	for(i=0 ; i<Nvox ; i++) {
		if(THD_get_voxel( mset1, i, 0) >0.5){
			ROI1[i] = 1;
		}
		if(THD_get_voxel( mset2, i, 0) >0.5)
			ROI2[i] = 1;
	}

	// set up eigvecs in 3D coord sys,
	// mark off where ROIs are and keep index handy
	idx=0;
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
				for( m=0 ; m<3 ; m++ ) 
					coorded[i][j][k][m] = THD_get_voxel(insetV1, idx, m);
				if(EXTRAFILE)
					coorded[i][j][k][3] = THD_get_voxel(insetEXTRA, idx, 0); 
				else
					coorded[i][j][k][3] = THD_get_voxel(insetFA, idx, 0); 
   
				// make sure that |V1| == 1 for all eigenvects, otherwise it's
				/// a problem in the tractography; currently, some from
				// 3dDWItoDT do not have this property...
				tempvmagn = sqrt(coorded[i][j][k][0]*coorded[i][j][k][0]+
									  coorded[i][j][k][1]*coorded[i][j][k][1]+
									  coorded[i][j][k][2]*coorded[i][j][k][2]);
				if( tempvmagn<0.99 ) 
					for( m=0 ; m<3 ; m++ ) 
						coorded[i][j][k][m]/= tempvmagn;
   
				INDEX[i][j][k][0] =idx; // first value is the index itself
				if( ROI1[idx]==1 ) 
					INDEX[i][j][k][1]=1; // second value identifies ROI1 mask
				else
					INDEX[i][j][k][1]=0;
				if( ROI2[idx]==1 )
					INDEX[i][j][k][2]=1; // third value identifies ROI2 mask
				else
					INDEX[i][j][k][2]=0;

				// fourth value will be counter for number of kept tracks
				// passing through
				INDEX[i][j][k][3] = 0;  
				idx+= 1;
			}
  
	// *************************************************************
	// *************************************************************
	//                    Beginning of main loop
	// *************************************************************
	// *************************************************************

	Numtract = 0;
	ave_tract_len = 0.;
	ave_tract_len_phys = 0.;
 
	sprintf(OUT_bin,"%s.trk",prefix);
	if( (fout0 = fopen(OUT_bin, "w")) == NULL) {
		fprintf(stderr, "Error opening file %s.",OUT_bin);
		exit(16);
	}
	fwrite(&header1,sizeof(tv_io_header),1,fout0);
  
	if (get_tract_verb()) {
		INFO_message("Begin tracking...");
	}

	tb = AppCreateBundle(NULL, 0, NULL, insetFA); // start bundle
	id = 0;
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) 
				if(coorded[i][j][k][3] >= MinFA) { 
					for( ii=0 ; ii<SeedPerV[0] ; ii++ ) 
						for( jj=0 ; jj<SeedPerV[1] ; jj++ ) 
							for( kk=0 ; kk<SeedPerV[2] ; kk++ ) {

								in[0] = i;
								in[1] = j;
								in[2] = k;
								physin[0] = ((float) in[0] + 
												 (0.5 + (float) ii)/SeedPerV[0])*Ledge[0];
								physin[1] = ((float) in[1] + 
												 (0.5 + (float) jj)/SeedPerV[1])*Ledge[1];
								physin[2] = ((float) in[2] + 
												 (0.5 + (float) kk)/SeedPerV[2])*Ledge[2];
      
								len_forw = TrackIt(coorded, in, physin, Ledge, Dim, 
														 MinFA, MaxAng, ArrMax, Tforw, 
														 flTforw, 1, phys_forw);
      
								// reset, because it's changed in TrackIt func
								in[0] = i; 
								in[1] = j;
								in[2] = k;

								physin[0] = ((float) in[0] + 
												 (0.5 + (float) ii)/SeedPerV[0])*Ledge[0];
								physin[1] = ((float) in[1] + 
												 (0.5 + (float) jj)/SeedPerV[1])*Ledge[1];
								physin[2] = ((float) in[2] + 
												 (0.5 + (float) kk)/SeedPerV[2])*Ledge[2];

								len_back = TrackIt(coorded, in, physin, Ledge, Dim, 
														 MinFA, MaxAng, ArrMax, Tback, 
														 flTback, -1, phys_back);
            
								KEEPIT = 0; // a simple switch

								totlen = len_forw+len_back-1; // NB: overlap of starts
								totlen_phys = phys_forw[0] + phys_back[0];
		
								if( totlen_phys >= MinL ) {
		  
									// glue together for simpler notation later
									for( n=0 ; n<len_back ; n++) { // all of this
										rr = len_back-n-1; // read in backward
										for(m=0;m<3;m++)
											Ttot[rr][m] = Tback[n][m];
									}
									for( n=1 ; n<len_forw ; n++){// skip first->overlap
										rr = n+len_back-1; // put after
										for(m=0;m<3;m++)
											Ttot[rr][m] = Tforw[n][m];
									}
									// <<So close and orthogonal condition>>:
									// test projecting ends, to see if they abut ROI.  
									for(m=0;m<3;m++) { 
										//actual projected ends
										end[1][m] = 2*Ttot[totlen-1][m]-Ttot[totlen-2][m];
										end[0][m] = 2*Ttot[0][m]-Ttot[1][m];
										// default choice, just retest known ends 
										// as default
										test_ind[1][m] = test_ind[0][m] = Ttot[0][m];
									}
		  
									tt = Create_Tract(len_back, flTback, len_forw, 
															flTforw, id, insetFA); ++id; 
        
									if (LOG_TYPE == -1) {
										KEEPIT = 1; 
									} else {
										inroi1 = 0;
										// check forw
										for( n=0 ; n<len_forw ; n++) {
											if(INDEX[Tforw[n][0]][Tforw[n][1]][Tforw[n][2]][1]==1){
												inroi1 = 1;
												break;
											} else
												continue;
										}
										if( inroi1==0 ){// after 1st half, check 2nd half
											for( m=0 ; m<len_back ; m++) {
												if(INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][1]==1){
													inroi1 = 1;
													break;
												} else
													continue;
											}
										}
										// after 1st&2nd halves, check bound/neigh
										if( inroi1==0 ) {
											if(INDEX[test_ind[1][0]][test_ind[1][1]][test_ind[1][2]][1]==1)
												inroi1 = 1;
											if(INDEX[test_ind[0][0]][test_ind[0][1]][test_ind[0][2]][1]==1)
												inroi1 = 1;
										}
			 
										if( ((LOG_TYPE ==0) && (inroi1 ==0)) || 
											 ((LOG_TYPE ==1) && (inroi1 ==1))) {
											// have to check in ROI2
				
											inroi2 = 0;
											// check forw
											for( n=0 ; n<len_forw ; n++) {
												if(INDEX[Tforw[n][0]][Tforw[n][1]][Tforw[n][2]][2]==1){
													inroi2 = 1;
													break;
												} else
													continue;
											}
											//after 1st half, check 2nd half
											if( inroi2==0 ) { 
												for( m=0 ; m<len_back ; m++) {
													if(INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][2]==1){
														inroi2 = 1;
														break;
													} else
														continue;
												}
											}
											// after 1st&2nd halves, check bound/neigh
											if( inroi2==0 ) { 
												if(INDEX[test_ind[1][0]][test_ind[1][1]][test_ind[1][2]][2]==1)
													inroi2 = 1;
												if(INDEX[test_ind[0][0]][test_ind[0][1]][test_ind[0][2]][2]==1)
													inroi2 = 1;
											}
				
											// for both cases, need to see it here to keep
											if( inroi2 ==1 )
												KEEPIT = 1; // otherwise, it's gone
				
										} else if((LOG_TYPE ==0) && (inroi1 ==1))
											KEEPIT = 1;
									}
								}
      
								// by now, we *know* if we're keeping this or not.
								if( KEEPIT == 1 ) {
									tb = AppCreateBundle(tb, 1, tt, NULL); 
									tt = Free_Tracts(tt, 1);
        
									READS_in = totlen;
									fwrite(&READS_in,sizeof(READS_in),1,fout0);
									for( n=0 ; n<len_back ; n++) {
										//put this one in backwords, to make it connect
										m = len_back - 1 - n; 
										for(aa=0 ; aa<3 ; aa++) {
											// recenter phys loc for trackvis, if nec...
											// just works this way (where they define 
											// origin)
											READS_fl = flTback[m][aa];
											if(!TV_switch[aa])
												READS_fl = Ledge[aa]*Dim[aa]-READS_fl;
											fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										}
										mm = INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][0];
										READS_fl =THD_get_voxel(insetFA, mm, 0); // FA
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										READS_fl =THD_get_voxel(insetMD, mm, 0); // MD
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										READS_fl =THD_get_voxel(insetL1, mm, 0); // L1
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										// count this voxel for having a tract
										INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][3]+= 1; 
									}
        
									for( m=1 ; m<len_forw ; m++) {
										for(aa=0 ; aa<3 ; aa++) {
											// recenter phys loc for trackvis, if nec...
											READS_fl = flTforw[m][aa];
											if(!TV_switch[aa])
												READS_fl = Ledge[aa]*Dim[aa]-READS_fl;
											fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										}
										mm = INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][0];
										READS_fl =THD_get_voxel(insetFA, mm, 0); // FA
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										READS_fl =THD_get_voxel(insetMD, mm, 0); // MD
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										READS_fl =THD_get_voxel(insetL1, mm, 0); // L1 
										fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
										// count this voxel for having a tract
										INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][3]+= 1; 
									}
        
									ave_tract_len+= totlen;
									ave_tract_len_phys+= totlen_phys;
									Numtract+=1;
								}   
							}
				}
	fclose(fout0); 
  
	if (get_tract_verb()) {
		INFO_message("Done tracking, have %d tracks.", tb->N_tracts);
		Show_Taylor_Bundle(tb, NULL, 3);
	}

	if (!Write_Bundle(tb,prefix,mode)) {
		ERROR_message("Failed to write the bundle");
	}
   
	// **************************************************************
	// **************************************************************
	//                    Some simple stats on ROIs and outputs
	// **************************************************************
	// **************************************************************

	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
				if( INDEX[i][j][k][3]>=1 ) {
					tempMD = THD_get_voxel(insetMD,INDEX[i][j][k][0],0);
					tempFA = THD_get_voxel(insetFA,INDEX[i][j][k][0],0);
					tempL1 = THD_get_voxel(insetL1,INDEX[i][j][k][0],0);
					tempRD = 0.5*(3*tempMD-tempL1);
					roi3_mu_MD+= tempMD;
					roi3_mu_FA+= tempFA;
					roi3_mu_L1+= tempL1;
					roi3_mu_RD+= tempRD;
					roi3_sd_MD+= tempMD*tempMD;
					roi3_sd_FA+= tempFA*tempFA;
					roi3_sd_L1+= tempL1*tempL1;
					roi3_sd_RD+= tempRD*tempRD;
					roi3_ct+= 1;
				}
			}
  
	if(roi3_ct > 0 ) { // !!!! make into afni file
		roi3_mu_MD/= (float) roi3_ct; 
		roi3_mu_FA/= (float) roi3_ct;
		roi3_mu_L1/= (float) roi3_ct;
		roi3_mu_RD/= (float) roi3_ct;
    
		roi3_sd_MD-= roi3_ct*roi3_mu_MD*roi3_mu_MD;
		roi3_sd_FA-= roi3_ct*roi3_mu_FA*roi3_mu_FA;
		roi3_sd_L1-= roi3_ct*roi3_mu_L1*roi3_mu_L1;
		roi3_sd_RD-= roi3_ct*roi3_mu_RD*roi3_mu_RD;
		roi3_sd_MD/= (float) roi3_ct-1; 
		roi3_sd_FA/= (float) roi3_ct-1;
		roi3_sd_L1/= (float) roi3_ct-1;
		roi3_sd_RD/= (float) roi3_ct-1;
		roi3_sd_MD = sqrt(roi3_sd_MD); 
		roi3_sd_FA = sqrt(roi3_sd_FA);
		roi3_sd_L1 = sqrt(roi3_sd_L1);
		roi3_sd_RD = sqrt(roi3_sd_RD);
  
		sprintf(OUT_tracstat,"%s.stats",prefix);
		if( (fout0 = fopen(OUT_tracstat, "w")) == NULL) {
			fprintf(stderr, "Error opening file %s.",OUT_tracstat);
			exit(19);
		}
		fprintf(fout0,"%d\t%d\n",Numtract,roi3_ct);
		fprintf(fout0,"%.3f\t%.3f\n",ave_tract_len/Numtract,
				  ave_tract_len_phys/Numtract);
		// as usual, these next values would have to be divided by the
		// bval to get their actual value in standard phys units
		fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_FA,roi3_sd_FA);
		fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_MD,roi3_sd_MD);
		fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_RD,roi3_sd_RD);
		fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_L1,roi3_sd_L1);
		fclose(fout0);

		sprintf(prefix_map,"%s_MAP",prefix); 
		sprintf(prefix_mask,"%s_MASK",prefix); 

		outsetMAP = EDIT_empty_copy( mset1 ) ;
		EDIT_dset_items( outsetMAP ,
							  ADN_datum_all , MRI_short , 
							  ADN_prefix    , prefix_map ,
							  ADN_none ) ;
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetMAP)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetMAP));
    
		outsetMASK = EDIT_empty_copy( mset1 ) ;
		EDIT_dset_items( outsetMASK ,
							  ADN_datum_all , MRI_byte , 
							  ADN_prefix    , prefix_mask ,
							  ADN_none ) ;
		if(!THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetMASK)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetMASK));
    
		m=0;
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
					temp_arr[m]=INDEX[i][j][k][3];
					if(temp_arr[m]>0.5)
						temp_byte[m]=1;
					else
						temp_byte[m]=0;
					m++;
				}
    
		// re-orient the data as original inputs 
		// (this function copies the pointer)
		EDIT_substitute_brick(outsetMAP, 0, MRI_short, temp_arr); 
		temp_arr=NULL;
		if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
			dsetn = r_new_resam_dset(outsetMAP, NULL, 0.0, 0.0, 0.0,
											 header1.voxel_order, RESAM_NN_TYPE, 
											 NULL, 1, 0);
			DSET_delete(outsetMAP); 
			outsetMAP=dsetn;
			dsetn=NULL;
		}
		EDIT_dset_items( outsetMAP ,
							  ADN_prefix , prefix_map ,
							  ADN_none ) ;
		THD_load_statistics(outsetMAP );
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetMAP)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetMAP));
		tross_Make_History( "3dTrackID" , argc , argv ,  outsetMAP) ;
		THD_write_3dim_dataset(NULL, NULL, outsetMAP, True);
		// re-orient the data as original inputs
		EDIT_substitute_brick(outsetMASK, 0, MRI_byte, temp_byte);
		temp_byte=NULL;
		if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
			dsetn = r_new_resam_dset(outsetMASK, NULL, 0.0, 0.0, 0.0,
											 header1.voxel_order, RESAM_NN_TYPE, 
											 NULL, 1, 0);
			DSET_delete(outsetMASK); 
			outsetMASK=dsetn;
			dsetn=NULL;
		}
		EDIT_dset_items( outsetMASK ,
							  ADN_prefix , prefix_mask ,
							  ADN_none ) ;
		THD_load_statistics(outsetMASK);
		if(!THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetMASK)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(outsetMASK));
		tross_Make_History( "3dTrackID" , argc , argv ,  outsetMASK) ;
		THD_write_3dim_dataset(NULL, NULL, outsetMASK, True);

		INFO_message("Number of tracts found = %d",Numtract) ;
	}
	else 
		INFO_message("\n No Tracts Found!!!\n");
  

	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************

	// !!! need to free afni-sets?
	DSET_delete(insetFA);
	DSET_delete(insetMD);
	DSET_delete(insetL1);
	DSET_delete(insetV1);
	DSET_delete(insetEXTRA);
	//DSET_delete(outsetMAP);  
	//DSET_delete(outsetMASK);
	DSET_delete(mset2);
	DSET_delete(mset1);

	free(prefix);
	free(insetV1);
	free(insetFA);
	free(mset1);
	free(mset2);
  	free(insetEXTRA);

	free(ROI1);
	free(ROI2);
	free(temp_byte);
  
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
  
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			for( k=0 ; k<Dim[2] ; k++) 
				free(coorded[i][j][k]);
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			free(coorded[i][j]);
	for( i=0 ; i<Dim[0] ; i++) 
		free(coorded[i]);
	free(coorded);

	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			for( k=0 ; k<Dim[2] ; k++) 
				free(INDEX[i][j][k]);
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			free(INDEX[i][j]);
	for( i=0 ; i<Dim[0] ; i++) 
		free(INDEX[i]);
	free(INDEX);

	free(temp_arr); // need to free
	for( i=0 ; i<2*ArrMax ; i++) 
		free(Ttot[i]);
	free(Ttot);

	//free(mode);
	
	return 0;
}

