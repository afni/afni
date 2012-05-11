/* 
   Probabilistic tracking,  first draft at AFNIfication, March 2012.
   using FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (some year!)
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     
#include <3ddata.h>     
#include <DoTrackit.h>
#include <time.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>





// from <editvol.h>
#define EDIT_DSET_ORIENT(ds,ox,oy,oz)                              \
 do{ THD_ivec3 orixyz ;                                            \
     LOAD_IVEC3( orixyz , (ox),(oy),(oz) ) ;                       \
     EDIT_dset_items( (ds) , ADN_xyzorient , orixyz , ADN_none ) ; \
 } while(0)

// from 3dUndump.c
#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )



void usage_ProbTrackID(int detail) 
{
  printf(
"  FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (some year!)\n"
"   \n"
"   first draft at AFNIfication, March 2012.\n"
"\n"
"   + read in data from 3dDWItoDTI results and\n"
"     also results from 3dDWUncert, for uncertainty measures\n"
"\n\n"
"   + current outputs: \n"
"     (and note that this is for each network, with subbricks per ROI) \n"
"      - MAP/MASK file, AFNI format with 0th brick containing the number\n"
"                  of tracts which passed through each voxel, and each i-th\n"
"                  brick containing the WM ROIs between the i-th ROI and\n"
"                  each other one in the network, if they exist.\n"
"      - STATS file, simple statistics in matrix format (N_ROI by N_ROI per \n"
"                  network :\n"
"              o  Numbers of tracts connecting ROIs (and has diag entries)\n"
"              o  Ratio of numbers of tracts connecting ROIS to total number\n"
"	          of tracts found through all iters (and has diag entries)\n"
"	       o  mean of FA value in WM ROIs connecting two GM-ROIs, with \n"
"	          WM ROI thresholded using NmNsFr value, described in input \n"
"	       o  the assoc std of FA value in WM ROIs connecting two GM-ROIs \n"
"	       o  mean of MD value in WM ROIs connecting two GM-ROIs, with \n"
"	          WM ROI thresholded using NmNsFr value, described in input \n"
"	       o  the assoc std of MD value in WM ROIs connecting two GM-ROIs \n"
"	       o  mean of RD value in WM ROIs connecting two GM-ROIs, with \n"
"	          WM ROI thresholded using NmNsFr value, described in input \n"
"	       o  the assoc std of RD value in WM ROIs connecting two GM-ROIs \n"
"	       o  mean of L1 value in WM ROIs connecting two GM-ROIs, with \n"
"	          WM ROI thresholded using NmNsFr value, described in input \n"
"	       o  the assoc std of L1 value in WM ROIs connecting two GM-ROIs \n"
"\n\n"
"   + SPECIAL INPUT needed: \n"
"     -algop ALGO_PROB: Algorithm options file, which is a single col:\n"
"        MinFA,    FA cutoff value, lower bound\n"
"	 MaxAng,   angle parameter between vox, max bound\n"
"	 MinL,     min physical length of tracts to keep\n"
"	 NmNsFr    Frac thresh for Ntracts through vox for stats (e.g., 0.1)\n"
"	 Nseed,    Number of seeds per vox, will be placed randomly\n"
"	 Nmonte,   Number of Monte Carlo iterations\n"
"	 M         number of grads used to scan\n"
"	 bval      non-b=0 value, e.g., 1000 or so\n"
"\n\n"
"   + to run, need to give:\n"
"        -netrois ROIS:  mask(s) of ROIs- can be many subbricks!\n"
"        -uncert  FILE:   uncertainty values [6 subbricks] \n"
"        -prefix  PREFIX: output file name part\n"
"        -input   INPREF: Specify DTI volumes output by 3dDWItoDTI\n"
"                         The program expects to the following volumes:\n"
"                         INPREF_V1, INPREF_MD, INPREF_L1, and INPREF_FA\n" 
"                         INPREF_V2, INPREF_V3, INPREF_DT\n"
"        -algopt  ALGO:   ASCII file with eight numbers defining parameter \n"
"                         quantities just above.  NB: Diff than that for\n"
"                         3dTrackID!!!\n"
"\n"
"   If you use this program, please reference:\n"
"     Taylor, Kuan-Hung, Lin and Biswal 2012\n"
"\n"
"   Example:\n"
"     Download and install this archive:\n"
"     curl -O http://..../FACTID_draft.tar.gz if you like I can put it under\n"
"            some tmp directory under AFNI's website.\n"
"     tar xvzf FACTID_draft.tar.gz\n"
"\n"
"Example run (requires output of 3dDWUncert program's sample command):\n"
"3dProbTrackID \\\n"
"     -netrois TEST_FILES/DTI/masks_together+orig.BRIK \\\n"
"     -uncert TEST_FILES/DTI/o.UNCERT_TESTb_UNC+orig.BRIK \\\n"
"     -prefix TEST_FILES/DTI/o.PROB5 \\\n"
"     -input TEST_FILES/DTI/DT \\\n"
"     -algopt TEST_FILES/ALGOPTS_PROB.dat\n"
"   \n" );
  return;
}



int main(int argc, char *argv[]) {
  int i,j,k,m,n,aa,ii,jj,kk,mm,gg,nn,hh,bb,cc,tt;
  int iarg;
  int nmask1=0;
  int nmask2=0;
  THD_3dim_dataset *insetV1 = NULL,*insetV2 = NULL,*insetV3 = NULL;
  THD_3dim_dataset *insetL1 = NULL;
  THD_3dim_dataset *insetFA = NULL,*insetMD = NULL,*insetUC = NULL;
  THD_3dim_dataset *mset1=NULL; 
  THD_3dim_dataset *networkMAPS=NULL, *outsetMASK=NULL;
  char *prefix="tracky" ;
  char in_FA[300];
  char in_V1[300];
  char in_V2[300];
  char in_V3[300];
  char in_MD[300];
  char in_L1[300];
  char in_L2[300];
  char in_L3[300];

  char OUT_bin[300];
  char OUT_grid[300];
  char OUT_map[300];
  char OUT_mask[300];
  FILE *fin4, *fin1, *fout1;


  // FACT algopts
  float MinFA,MaxAng,MinL,NmNsFr;
  int NmNsThr=0;
  int ArrMax;

  int Nvox=-1;   // tot number vox
  int Ndata=-1;
  int Dim[3]; // dim in each dir
  int Nseed,M,bval;
  float **LocSeed=NULL; // fractional locations within voxel, 
                        // set randomly each iter
  float Ledge[3]; // voxel edge lengths

  int **Tforw, **Tback;
  float **flTforw, **flTback;
  float **coorded;
  float **copy_coorded; // copy will have perturbed info;
  int ***INDEX,***INDEX2;
  int **MAPROI; // store what ROIs are where, per data voxel
  int ****NETROI; // store connection info
  int len_forw, len_back; // int count of num of squares through
  float phys_forw[1], phys_back[1];
  int idx,idx2;

  int in[3]; // to pass to trackit
  float physin[3]; // also for trackit, physical loc, 
  int totlen; 
  float totlen_phys;
  int Numtract; 

  char READS_ch;
  short int READS_sh; 
  int READS_in;
  float READS_fl;

  int  roi3_ct=0;
  float roi3_mu_MD = 0.,roi3_mu_RD = 0.,roi3_mu_L1 = 0.,roi3_mu_FA = 0.;  
  float roi3_sd_MD = 0.,roi3_sd_RD = 0.,roi3_sd_L1 = 0.,roi3_sd_FA = 0.;  
  float tempMD,tempFA,tempRD,tempL1;
  char dset_or[3] = "RAI";
  char voxel_order[3]="---";
  THD_3dim_dataset *dsetn;
  int TV_switch[3] = {0,0,0};

  int ***Prob_grid; // will be a NROIxNROI grid, where NROI is number of ROIs
  float ****Param_grid; // same size as Prob_Grid, with extra dim for ROI stats
  int *list_rois, *temp_list;
  int MAXNROI=0;
  int Nmonte=0;
  int N_nets=0;
  float w1,w2,w3,thetval;
  float tempv[3];
  float tempvmagn;
  float testang;

  char **prefix_netmap=NULL;
  int *NROI=NULL;

  long seed;   
  const gsl_rng_type * T;
  gsl_rng *r;
   
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
  
  INFO_message("version: EPSILON");

  // scan args 
  if (argc == 1) { usage_ProbTrackID(1); exit(0); }
  iarg = 1;
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || 
	strcmp(argv[iarg],"-h") == 0 ) {
      usage_ProbTrackID(strlen(argv[iarg])>3 ? 2:1);
      exit(0);
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
      
      prefix_netmap = malloc( N_nets*sizeof(prefix_netmap));  
      for(i=0 ; i<N_nets ; i++) 
	prefix_netmap[i] = malloc( 300*sizeof(char)); 
      NROI = (int *)malloc(N_nets * sizeof(int)); 
      if( (NROI == NULL) ||  (prefix_netmap == NULL)) {
	fprintf(stderr, "\n\n MemAlloc failure.\n\n");
	exit(122);
      }
      
      for( i=0 ; i<N_nets ; i++) {
	NROI[i] = (int) THD_subbrick_max(mset1, i, 1);
	if( NROI[i]>MAXNROI )
	  MAXNROI = NROI[i];
      }
      
      for( bb=0 ; bb<N_nets ; bb++)
	INFO_message("Number of ROIs in netw=%d; N_netw=%d ",bb,NROI[bb]);
      
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
      sprintf(in_FA,"%s_FA+orig", argv[iarg]); 
      insetFA = THD_open_dataset(in_FA) ;//argv[iarg] ) ;
      if( insetFA == NULL ) 
	ERROR_exit("Can't open dataset '%s':FA",in_FA);
      DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
      Nvox = DSET_NVOX(insetFA) ;
      Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); 
      Dim[2] = DSET_NZ(insetFA); 
      Ledge[0] = fabs(DSET_DX(insetFA)); Ledge[1] = fabs(DSET_DY(insetFA)); 
      Ledge[2] = fabs(DSET_DZ(insetFA)); 
      // check tot num vox match (as proxy for dims...)
      if( (Nvox != nmask1) || (Nvox != nmask2) )
	ERROR_exit("Input dataset does not match both mask volumes!");
	
      
      // this stores the original data file orientation for later use,
      // as well since we convert everything to RAI temporarily, as
      // described below
      voxel_order[0]=ORIENT_typestr[insetFA->daxes->xxorient][0];
      voxel_order[1]=ORIENT_typestr[insetFA->daxes->yyorient][0];
      voxel_order[2]=ORIENT_typestr[insetFA->daxes->zzorient][0];
      
      sprintf(in_V1,"%s_V1+orig", argv[iarg]); 
      insetV1 = THD_open_dataset(in_V1);
      if( insetV1 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V1",in_V1);
      DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;

      sprintf(in_V2,"%s_V2+orig", argv[iarg]); 
      insetV2 = THD_open_dataset(in_V2);
      if( insetV2 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V2",in_V2);
      DSET_load(insetV2) ; CHECK_LOAD_ERROR(insetV2) ;

      sprintf(in_V3,"%s_V3+orig", argv[iarg]); 
      insetV3 = THD_open_dataset(in_V3);
      if( insetV3 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V3",in_V3);
      DSET_load(insetV3) ; CHECK_LOAD_ERROR(insetV3) ;

      sprintf(in_MD,"%s_MD+orig", argv[iarg]); 
      insetMD = THD_open_dataset(in_MD);
      if( insetMD == NULL ) 
	ERROR_exit("Can't open dataset '%s':MD",in_MD);
      DSET_load(insetMD) ; CHECK_LOAD_ERROR(insetMD) ;

      sprintf(in_L1,"%s_L1+orig", argv[iarg]); 
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
      
      // Opening/Reading in FACT params
      if( (fin4 = fopen(argv[iarg], "r")) == NULL) {
	fprintf(stderr, "Error opening file %s.",argv[iarg]);
	exit(19);
      }
      fscanf(fin4, "%f %f %f %f %d %d %d %d",
	     &MinFA,&MaxAng,&MinL,&NmNsFr,&Nseed,&Nmonte,&M,&bval);
      fclose(fin4);
      
      LocSeed = malloc(Nseed*sizeof(LocSeed)); 
      for(i=0 ; i<Nseed ; i++) 
	LocSeed[i] = malloc(3*sizeof(float)); 
      
      // convert to cos of rad value for comparisons, instead of using acos()
      MaxAng = cos(CONV*MaxAng); 
      
      // will take stats on voxels with number of tracts >=NmNsThr
      NmNsThr =  (int) ceil(NmNsFr*Nseed*Nmonte); 
      INFO_message("Effective Monte Iters:%d. Voxel threshold:%d.",Nseed*Nmonte,NmNsThr);

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
  
  dsetn = r_new_resam_dset(insetUC, NULL, 0.0, 0.0, 0.0,
			   dset_or, RESAM_NN_TYPE, NULL, 1, 0);
  DSET_delete(insetUC); 
  insetUC=dsetn;
  dsetn=NULL;
  
  
  
  
  // ****************************************************************
  // ****************************************************************
  //                    make arrays for tracking
  // ****************************************************************
  // ****************************************************************
  
  // for temp storage array, just a multiple of longest dimension!
  // essentially, a buffer size per tract we're making
  if(Dim[0] > Dim[1])
    ArrMax = Dim[0] * 4;
  else
    ArrMax = Dim[1] * 4;
  if(4*Dim[2] > ArrMax)
    ArrMax = Dim[2] * 4;
  
  // will hold indices of all voxels with actual data, i.e., takes in
  // (i,j,k) coor and gives index, for efficiency of storage because
  // lots of zeros in (Dimx, Dimy,Dimz grid).
  // INDEX:  for afni THD_* things
  // INDEX2: for coor*, NETROI
  INDEX = (int ***) malloc( Dim[0] * sizeof(int **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    INDEX[i] = (int **) malloc( Dim[1] * sizeof(int *) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      INDEX[i][j] = (int *) malloc( Dim[2] * sizeof(int) );
  INDEX2 = (int ***) malloc( Dim[0] * sizeof(int **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    INDEX2[i] = (int **) malloc( Dim[1] * sizeof(int *) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      INDEX2[i][j] = (int *) malloc( Dim[2] * sizeof(int) );
  
   if( (INDEX == NULL) || (INDEX2 == NULL) ) {
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
	if( THD_get_voxel(insetL1,idx,0)>EPS_V) { 
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
  // 3 comp of V1 and FA for each data voxel
  coorded = malloc( (Ndata+1)*sizeof(coorded)); // to have all ind be >=1
  for(i=0 ; i<=Ndata ; i++) 
    coorded[i] = malloc(4*sizeof(float)); 
  // copy for perturb
  copy_coorded = malloc( (Ndata+1)*sizeof(copy_coorded)); 
  for(i=0 ; i<=Ndata ; i++) // to have all ind be >=1
    copy_coorded[i] = malloc(4*sizeof(float)); 

  MAPROI = malloc( (Ndata+1)*sizeof(MAPROI)); // to have all ind be >=1
  for(i=0 ; i<=Ndata ; i++) 
    MAPROI[i] = malloc(N_nets*sizeof(int)); 

  NETROI = (int ****) malloc( (Ndata+1)* sizeof(int ***) );
  for ( i = 0 ; i<=Ndata ; i++ ) 
    NETROI[i] = (int ***) malloc( N_nets * sizeof(int **) );
  for ( i=0 ; i<=Ndata ; i++ ) 
    for ( j=0 ; j<N_nets ; j++ ) // jth net has NROI[j] rois
      NETROI[i][j] = (int **) malloc( NROI[j] * sizeof(int *) );
  for ( i=0 ; i<=Ndata ; i++ ) 
    for ( j=0 ; j<N_nets ; j++ ) 
      for ( k=0 ; k<NROI[j] ; k++ ) 
	NETROI[i][j][k] = (int *) malloc( NROI[j] * sizeof(int) );

  if( (coorded == NULL) || (copy_coorded == NULL) 
      || (NETROI == NULL) || (MAPROI == NULL)) {
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(122);
  }
  
  
  Prob_grid = (int ***) malloc( N_nets * sizeof(int **));
  for ( i = 0 ; i < N_nets ; i++ ) 
    Prob_grid[i] = (int **) malloc( (NROI[i]) * sizeof(int *));
  for ( i = 0 ; i < N_nets ; i++ ) 
    for ( j = 0 ; j < (NROI[i]) ; j++ ) 
      Prob_grid[i][j] = (int *) malloc( (NROI[i]) * sizeof(int));

  Param_grid = (float ****) malloc( N_nets * sizeof(float ***));
  for ( i = 0 ; i < N_nets ; i++ ) 
    Param_grid[i] = (float ***) malloc( (NROI[i]) * sizeof(float **));
  for ( i = 0 ; i < N_nets ; i++ ) 
    for ( j = 0 ; j < (NROI[i]) ; j++ ) 
      Param_grid[i][j] = (float **) malloc( (NROI[i]) * sizeof(float *));
  for ( i = 0 ; i < N_nets ; i++ ) 
    for ( j = 0 ; j < (NROI[i]) ; j++ ) 
      for ( k = 0 ; k < (NROI[i]) ; k++ ) // mu and std of FA,MD,RD,L1; count
	Param_grid[i][j][k] = (float *) malloc( 9 * sizeof(float));

  temp_list = ( int *)malloc((MAXNROI+1) * sizeof( int)); 
  list_rois = ( int *)malloc((MAXNROI+1) * sizeof( int)); 

  Tforw = malloc(ArrMax*sizeof(Tforw)); 
  for(i=0 ; i<ArrMax ; i++) 
    Tforw[i] = malloc(3*sizeof(int)); 
  Tback = malloc(ArrMax*sizeof(Tback)); 
  for(i=0 ; i<ArrMax ; i++) 
    Tback[i] = malloc(3*sizeof(int)); 
  // temp storage whilst tracking, physical loc
  flTforw = malloc(ArrMax*sizeof(flTforw)); 
  for(i=0 ; i<ArrMax ; i++) 
    flTforw[i] = malloc(3*sizeof(int)); 
  flTback = malloc(ArrMax*sizeof(flTback)); 
  for(i=0 ; i<ArrMax ; i++) 
    flTback[i] = malloc(3*sizeof(int)); 
  if(  (flTback == NULL) || (Tforw == NULL) || (Tback == NULL) 
       || (flTforw == NULL) || (temp_list == NULL) 
       || (list_rois == NULL) || (Param_grid == NULL)
       || (Prob_grid == NULL) ) {
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(12);
  }
  
  // this will be what we fill in 
  for( k=0 ; k<N_nets ; k++ )
    for( i=0 ; i<(NROI[k]) ; i++ )
      for( j=0 ; j<(NROI[k]) ; j++ ) {
	Prob_grid[k][i][j] = 0;  
	for( ii=0 ; ii<9 ; ii++)
	  Param_grid[k][i][j][ii] = 0.;  
      }
  
  // set up eigvecs in 3D coord sys,
  // mark off where ROIs are and keep index handy
  for( k=0 ; k<Dim[2] ; k++ ) 
    for( j=0 ; j<Dim[1] ; j++ ) 
      for( i=0 ; i<Dim[0] ; i++ ) 
	if( THD_get_voxel(insetL1,INDEX[i][j][k],0)>EPS_V) { 
	  idx = INDEX2[i][j][k];
	  for( m=0 ; m<3 ; m++ ) 
	    coorded[idx][m]=copy_coorded[idx][m]=THD_get_voxel(insetV1,INDEX[i][j][k],m);
	  coorded[idx][3]=copy_coorded[idx][3]=THD_get_voxel(insetFA,INDEX[i][j][k],0); 
	  
	  for( m=0 ; m<N_nets ; m++ ) {
	    // allow indentification by index 
	    if( THD_get_voxel(mset1, INDEX[i][j][k], m)>0.5 )
	      MAPROI[idx][m] = THD_get_voxel(mset1,INDEX[i][j][k],m);
	    else
	      MAPROI[idx][m] = 0;
	    
	    // counter for number of kept tracks passing through
	    for( mm=0 ; mm<NROI[m] ; mm++ )
	      for( tt=0 ; tt<NROI[m] ; tt++ )
		NETROI[idx][m][mm][tt] = 0;
	  }
	}
  
  
  // *************************************************************
  // *************************************************************
  //                    Beginning of main loops
  // *************************************************************
  // *************************************************************

  Numtract = 0;
 
  for (gg=0 ; gg<Nmonte ; gg++) {
    
    // relative location of each seed within voxel for this iter
    for( k=0 ; k<Nseed ; k++ ) 
      for( j=0 ; j<3 ; j++ ) 
	LocSeed[k][j] = rand()*1.0/RAND_MAX;
        
    if( gg>0) // first time through is no change
      for( k=0 ; k<Dim[2] ; k++ ) 
	for( j=0 ; j<Dim[1] ; j++ ) 
	  for( i=0 ; i<Dim[0] ; i++ ) {
	    // only do region in brain mask
	    if( THD_get_voxel(insetL1,INDEX[i][j][k],0)>EPS_V) { 
	      idx = INDEX2[i][j][k];
	      
	      // w1 = 1.0;
	      // these are weights determined by rotation angle,
	      // (prob. determined by jackknifing with 3dDWUncert)
	      // each tips in the +/- direc toward/away from each evec 
	      // by averaging and that's why tan of angle is taken
	      thetval = pow(THD_get_voxel(insetUC,INDEX[i][j][k],0),2) +
		pow(THD_get_voxel(insetUC,INDEX[i][j][k],1),2); // MSE
	      testang = gsl_ran_gaussian_ziggurat(r,1.0)*sqrt(thetval);
	      w2 = tan(testang); 

	      thetval = pow(THD_get_voxel(insetUC,INDEX[i][j][k],2),2) +
		pow(THD_get_voxel(insetUC,INDEX[i][j][k],3),2); // MSE
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
	      
	      copy_coorded[idx][3] = coorded[idx][3] - 
		THD_get_voxel(insetUC,INDEX[i][j][k],4)+
		(THD_get_voxel(insetUC,INDEX[i][j][k],5) * 
		 gsl_ran_gaussian_ziggurat(r,1.0));

	      if(copy_coorded[idx][3] <0)
		copy_coorded[idx][3] =0.;
	      if(copy_coorded[idx][3] >1)
		copy_coorded[idx][3] =1.;
	    }
	    else
	      for( m=0 ; m<4 ; m++)
	      copy_coorded[idx][m] = 0;
	  }

    // this is where we start the tracking for a given data set
    // start of Monte Carlo loop
    for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
	for( i=0 ; i<Dim[0] ; i++ ) 
	  if(copy_coorded[INDEX2[i][j][k]][3] >= MinFA) 
	    for( kk=0 ; kk<Nseed ; kk++ ) {
	      
	      in[0] = i;
	      in[1] = j;
	      in[2] = k;
	      
	      for( jj=0 ; jj<3 ; jj++ ) 
		physin[jj] = ((float) in[jj] + LocSeed[kk][jj])*Ledge[jj];
	      
	      len_forw = TrackItP(copy_coorded, in, physin, Ledge, Dim, 
				  MinFA, MaxAng, ArrMax, Tforw, 
				  flTforw, 1, phys_forw,INDEX2);

	      in[0] = i; // reset, because it's changed in TrackIt func
	      in[1] = j;
	      in[2] = k;
	      
	      for( jj=0 ; jj<3 ; jj++ ) 
		physin[jj] = ((float) in[jj] + LocSeed[kk][jj])*Ledge[jj];
	      
	      len_back = TrackItP(copy_coorded, in, physin, Ledge, Dim, 
				  MinFA, MaxAng, ArrMax, Tback, 
				  flTback, -1, phys_back,INDEX2);
	      
	      totlen = len_forw+len_back-1; // b/c of overlap of starts
	      totlen_phys = phys_forw[0] + phys_back[0];
	      
	      if( totlen_phys >= MinL ) {
		Numtract += 1; //keeping tally of tot num of tracts
		
		for( hh=0 ; hh<N_nets ; hh++) {
		  for( n=0 ; n<=NROI[hh] ; n++)
		    list_rois[n] = temp_list[n] = 0;
		  
		  // have to go all the way through each track 
		  // checking every vox for intersections
		  for( n=0 ; n<len_forw ; n++) {
		    tt = INDEX2[Tforw[n][0]][Tforw[n][1]][Tforw[n][2]];
		    if(MAPROI[tt][hh]>0)
		      list_rois[MAPROI[tt][hh]]=1;
		  }
		  for(m=0;m<len_back;m++){
		    tt = INDEX2[Tback[m][0]][Tback[m][1]][Tback[m][2]];
		    if(MAPROI[tt][hh]>0)
		      list_rois[MAPROI[tt][hh]]=1;
		  }
		  
		  // now, for this track, record 
		  // first, write shorter list of which ones were hit
		  m = 0;
		  for( n=1 ; n<=NROI[hh] ; n++)
		    if(list_rois[n]>0 ) {
		      // values stored are 1...NROI -> 0...NROI-1
		      temp_list[m] = n-1; // keep track of which was hit
		      m = m+1;
		    }
		  
		  // lets keep track of where tracts connecting regions go
		  // we'll keep stats on individ ROI tracks
		  if(m>0) {
		    for( mm=1 ; mm<len_forw ; mm++) {
		      tt = INDEX2[Tforw[mm][0]][Tforw[mm][1]][Tforw[mm][2]];
		      for( bb=0 ; bb<m ; bb++)
			for( cc=0 ; cc<m ; cc++) {
			  NETROI[tt][hh][temp_list[bb]][temp_list[cc]]+=1;
			  //  if( bb != cc) {
			  if(((NETROI[tt][hh][temp_list[bb]][temp_list[cc]]==NmNsThr)
			      && (bb != cc)) || // correct count for bb==cc later
			     ((NETROI[tt][hh][temp_list[bb]][temp_list[cc]]==2*NmNsThr)
			      && (bb == cc)) ){
			    idx2 = INDEX[Tforw[mm][0]][Tforw[mm][1]][Tforw[mm][2]];
			    //mu and std of FA,MD,RD,L1
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][0]+= 
			      THD_get_voxel(insetFA,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][1]+= 
			      (float) pow(THD_get_voxel(insetFA,idx2,0),2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][2]+= 
			      THD_get_voxel(insetMD,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][3]+= 
			      (float) pow(THD_get_voxel(insetMD,idx2,0),2);
			    READS_fl = 0.5*(3.0*THD_get_voxel(insetMD,idx2,0)-
					    THD_get_voxel(insetL1,idx2,0));
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][4]+= 
			      READS_fl;
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][5]+= 
			      (float) pow(READS_fl,2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][6]+= 
			      THD_get_voxel(insetL1,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][7]+= 
			      (float) pow(THD_get_voxel(insetL1,idx2,0),2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][8]+= 1.0;
			  }
			  
			}
		    }
		    for( nn=0 ; nn<len_back ; nn++) {
		      // put this one in backwards, to make it connect
		      mm = len_back - 1 - nn; 
		      tt = INDEX2[Tback[mm][0]][Tback[mm][1]][Tback[mm][2]];
		      //NETROI[tt][hh][0]+= 1; 
		      for( bb=0 ; bb<m ; bb++)
			for( cc=0 ; cc<m ; cc++) {
			  NETROI[tt][hh][temp_list[bb]][temp_list[cc]]+=1;
			  //if( bb != cc) {
			  if(((NETROI[tt][hh][temp_list[bb]][temp_list[cc]]==NmNsThr)
			      && (bb != cc)) || // correct count for bb==cc later
			     ((NETROI[tt][hh][temp_list[bb]][temp_list[cc]]==2*NmNsThr)
			      && (bb == cc)) ){
			    // NETROI[tt][hh][temp_list[cc]] = temp_list[bb]; 
			    idx2 = INDEX[Tback[mm][0]][Tback[mm][1]][Tback[mm][2]];
			    //mu and std of FA,MD,RD,L1
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][0]+= 
			      THD_get_voxel(insetFA,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][1]+= 
			      (float) pow(THD_get_voxel(insetFA,idx2,0),2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][2]+= 
			      THD_get_voxel(insetMD,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][3]+= 
			      (float) pow(THD_get_voxel(insetMD,idx2,0),2);
			    READS_fl = 0.5*(3.0*THD_get_voxel(insetMD,idx2,0)-
					    THD_get_voxel(insetL1,idx2,0));
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][4]+= 
			      READS_fl;
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][5]+= 
			      (float) pow(READS_fl,2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][6]+= 
			      THD_get_voxel(insetL1,idx2,0);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][7]+= 
			      (float) pow(THD_get_voxel(insetL1,idx2,0),2);
			    Param_grid[hh][temp_list[cc]][temp_list[bb]][8]+= 1.0;
			  }
			}
		      
		    }
		  }
		  
		  for( mm=0 ; mm<m ; mm++)
		    for( nn=mm ; nn<m ; nn++) {
		      // this will fill in UHT part of matrix 
		      // store as values in range 1...NROI
		      Prob_grid[hh][ temp_list[mm] ][ temp_list[nn] ]+= 1;
		    }
		}
	      }
	    }
    
  } // end of Monte Carlo loop
  
  
  // **************************************************************
  // **************************************************************
  //                    Some outputs
  // **************************************************************
  // **************************************************************
  
  
  if(Numtract > 0 ) {

    for( k=0 ; k<N_nets ; k++) 
      for( i=0 ; i<NROI[k] ; i++ ) 
	for( j=0 ; j<NROI[k] ; j++ ) 
	  if(Param_grid[k][i][j][8]>0.5) {
	    for( m=0 ; m<4 ; m++) {
	      // means
	      Param_grid[k][i][j][2*m]/= Param_grid[k][i][j][8];
	      // stdevs
	      if(Param_grid[k][i][j][8]>1.5) {
		Param_grid[k][i][j][2*m+1]-= 
		  Param_grid[k][i][j][8]*pow(Param_grid[k][i][j][2*m],2);
		Param_grid[k][i][j][2*m+1]/= Param_grid[k][i][j][8]-1;
		Param_grid[k][i][j][2*m+1] = sqrt(Param_grid[k][i][j][2*m+1]);
	      }
	      else
		Param_grid[k][i][j][2*m+1]=0.0;
	    }
	  }

    
    for( k=0 ; k<N_nets ; k++) { // each netw gets own file

      // print out prob grid
      sprintf(OUT_grid,"%s_%03d.grid",prefix,k+1);
      if( (fout1 = fopen(OUT_grid, "w")) == NULL) {
	fprintf(stderr, "Error opening file %s.",OUT_grid);
	exit(19);
      }
    
      fprintf(fout1,"%d\n\n",NROI[k]);
      for( i=0 ; i<NROI[k] ; i++ ) {
	for( j=0 ; j<NROI[k]-1 ; j++ ) // b/c we put \n specially after last one...
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
    }

    // output AFNI files mapping WM-- NB: right now, these are the 
    // unthresholded ones, i.e., not same as what had statistic above.
    // these are so that one can define one's one ROIs/thresholds later.
    // !!!! may not keep this functionality
    for( hh=0 ; hh<N_nets ; hh++) {
      
      sprintf(prefix_netmap[hh],"%s_%03d_NETMAPS",prefix,hh+1); 
      // just get one of right dimensions!
      networkMAPS = EDIT_empty_copy( insetFA ) ; 
      EDIT_dset_items(networkMAPS,
		      ADN_datum_all , MRI_short , 
		      ADN_brick_label_one , "ALLROI",
		      ADN_prefix    , prefix_netmap[hh] ,
		      ADN_none ) ;
      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(networkMAPS)) )
	ERROR_exit("Can't overwrite existing dataset '%s'",
		   DSET_HEADNAME(networkMAPS));
      EDIT_add_bricklist(networkMAPS ,
			 NROI[hh], NULL , NULL , NULL );
      
      short int **temp_arr;
      temp_arr = malloc( (NROI[hh]+1)*sizeof(temp_arr));  // XYZ components
      for(i=0 ; i<(NROI[hh]+1) ; i++) 
	temp_arr[i] = malloc( Nvox*sizeof(int)); 
    
      // threshold the `overall' (`0th') map; individual ones already have
      // been above, just by how they were created
      for( i=0 ; i<=Ndata ; i++ ) 
	for( j=0 ; j<NROI[hh] ; j++ ) {
	  // first correct double counting from above
	  NETROI[i][hh][j][j]/=2; 
	  // apply thresholding by count
	  for( k=0 ; k<NROI[hh] ; k++ ) 
	    if( NETROI[i][hh][j][k] < NmNsThr)
	      NETROI[i][hh][j][k]=0;
	}

      for( bb=0 ; bb<=NROI[hh] ; bb++) // zero array
	for( k=0 ; k<Nvox ; k++ ) 
	  temp_arr[bb][k]=0;

      for( bb=1 ; bb<=NROI[hh] ; bb++) {
	idx=0;
	for( k=0 ; k<Dim[2] ; k++ ) 
	  for( j=0 ; j<Dim[1] ; j++ ) 
	    for( i=0 ; i<Dim[0] ; i++ ) {
	      temp_arr[bb][idx] = 0;
	      // allow for more than one ROI to be partnered/overlapping tracts
	      if( THD_get_voxel(insetL1,idx,0)>EPS_V) 
		for( tt=0 ; tt<NROI[hh] ; tt++) 
		  if(NETROI[INDEX2[i][j][k]][hh][bb-1][tt]>0) {
		    temp_arr[0][idx] = 1; // zeroth is mask of all through ROI
		    if(bb-1 != tt) // exclude non-paired tracks
		      temp_arr[bb][idx]+=tt+1; // then add value if overlap
		  }
	      idx+=1;
	    }
	
	EDIT_substitute_brick(networkMAPS, bb, MRI_short, temp_arr[bb]);
      } 
      EDIT_substitute_brick(networkMAPS, 0, MRI_short, temp_arr[0]);
      // well, guess it will stay as RAI....

      THD_load_statistics(networkMAPS);
      THD_write_3dim_dataset(NULL, NULL, networkMAPS, True);
      DSET_delete(networkMAPS); 
      free(temp_arr);
      
    }
    INFO_message("Writing output. NB: it will be %s!",dset_or);

    INFO_message("Number of tracts found = %d",Numtract);
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
  
  
  DSET_delete(insetFA);
  DSET_delete(insetMD);
  DSET_delete(insetL1);
  DSET_delete(insetV1);
  DSET_delete(insetV2);
  DSET_delete(insetV3);
  DSET_delete(insetUC);
  DSET_delete(mset1);
  
  free(prefix);
  free(insetV1);
  free(insetV2);
  free(insetV3);
  free(insetL1);
  free(insetFA);
  free(insetUC);

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
  }
  free(coorded);
  free(copy_coorded);
  free(NETROI);
  free(MAPROI);

  for( i=0 ; i<Dim[0] ; i++) 
    for( j=0 ; j<Dim[1] ; j++) 
      free(INDEX[i][j]);
  for( i=0 ; i<Dim[0] ; i++) 
    free(INDEX[i]);
  free(INDEX);
  
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
  
  return 0;
}

