/* 
   ZSS: I used all of this comment in the usage function. 
   
   FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (some year!)
   
   first draft at AFNIfication, March 2012.

   + read in data from 3dDWItoDTI results; use L1 and V1 for actual
   algorithm, and include MD and FA for stats

   + current outputs (named using prefix):  
      - TRK file, Trackvis-readable file of tracts
      - MAP file, AFNI format with number of tracts which passed through each voxel
      - MASK file, AFNI format with 1 where a tract passed, 0 elsewhere
      - STATS file, simple statistics from tracts produced in the run (2 cols, 6 rows):
             [Number of tracts]          [Number of voxels with >=1 tract]
	     [ave number of vox/tract]   [ave phys length/tract]
	     [unweighted mean of FA]     [st dev of FA]
	     [unweighted mean of MD]     [st dev of MD]
	     [unweighted mean of RD]     [st dev of RD]
	     [unweighted mean of L1]     [st dev of L1]

 
   + SPECIAL INPUT needed: Algorithm options file (-algop), which is a single col:
         MinFA,         FA cutoff value, lower bound
	 MaxAng,        angle parameter between vox, max bound
	 MinL,          min physical length of tracts to keep
	 SeedPerV[0],   Number of seeds per vox in x direc
	 SeedPerV[1],   Number of seeds per vox in y direc
	 SeedPerV[2],   Number of seeds per vox in z direc
	 M              number of grads used to scan (for later use with probabilist!)
	 bval           non-b=0 value, e.g., 1000 or so (for later use with probabilist!)
	 
   + to run, need to give:
      -mask1  [mask of ROI1]
      -mask2  [mask of ROI2]
      -prefix [output file name part]
      -input  [prefix of 3dDWItoDTI outputs]
      -algopt [ASCII file with eight numbers defining parameter quantities just above]
      -logic  [AND or OR, determining combinatorial logic of two ROIs;
               for just one ROI, make both `mask's the same file and use, e.g., AND]

 */




// !!!! need to use brick factors??



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     // AFNIadd
#include <3ddata.h>     // AFNIadd
#include <DoTrackit.h>

#define EPS_V (0.0000001) // for eigvec 'vel' to not have badness dividing
#define EPS_MASK (0.001) // theshold for masked data to be ignored
#define CONV (3.141592654/180)

void usage_tract_test(int detail) 
{
   /* ZSS, I did not use variable detail here, but you could.
      you'll get detail=2 if you use -help, 1 if you use -h */
   printf(
"  FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (some year!)\n"
"   \n"
"   first draft at AFNIfication, March 2012.\n"
"\n"
"   + read in data from 3dDWItoDTI results; use L1 and V1 for actual\n"
"   algorithm, and include MD and FA for stats\n"
"\n"
"   + current outputs (named using prefix):  \n"
"      - TRK file, Trackvis-readable file of tracts\n"
"      - MAP file, AFNI format with number of tracts which passed through \n"
"                  each voxel\n"
"      - MASK file, AFNI format with 1 where a tract passed, 0 elsewhere\n"
"      - STATS file, simple statistics from tracts produced in the run \n"
"                   (2 cols, 6 rows) :\n"
"       [Number of tracts]          [Number of voxels with >=1 tract]\n"
"	     [ave number of vox/tract]   [ave phys length/tract]\n"
"	     [unweighted mean of FA]     [st dev of FA]\n"
"	     [unweighted mean of MD]     [st dev of MD]\n"
"	     [unweighted mean of RD]     [st dev of RD]\n"
"	     [unweighted mean of L1]     [st dev of L1]\n"
"\n"
" \n"
"   + SPECIAL INPUT needed: \n"
"     -algop ALGO: Algorithm options file, which is a single col:\n"
"         MinFA,         FA cutoff value, lower bound\n"
"	 MaxAng,        angle parameter between vox, max bound\n"
"	 MinL,          min physical length of tracts to keep\n"
"	 SeedPerV[0],   Number of seeds per vox in x direc\n"
"	 SeedPerV[1],   Number of seeds per vox in y direc\n"
"	 SeedPerV[2],   Number of seeds per vox in z direc\n"
"	 M              number of grads used to scan (for later use with\n"
"                  probabilist!)\n"
"	 bval           non-b=0 value, e.g., 1000 or so (for later use \n"
"                  with probabilist!)\n"
"	 \n"
"   + to run, need to give:\n"
"      -mask1  MASK1: mask of ROI1\n"
"      -mask2  MASK2: mask of ROI2\n"
"      -prefix PREFIX: output file name part\n"
"      -input  INPREF: Specify DTI volumes output by 3dDWItoDTI\n"
"                      The program expects to the following volumes:\n"
"                      INPREF_V1, INPREF_MD, INPREF_L1, and INPREF_FA\n" 
"      -algopt ALGO: ASCII file with eight numbers defining parameter \n"
"                    quantities just above.\n"
"      -logic  [AND or OR]: Set the combinatorial logic of two ROIs;\n"
"               for just one ROI, make both `mask's the same file and use, e.g., AND]\n"
"   If you use this program, please reference:\n"
"     Taylor, Kuan-Hung, Lin and Biswal 2012\n"
"\n"
"   Example:\n"
"     Download and install this archive:\n"
"     curl -O http://..../FACTID_draft.tar.gz if you like I can put it under\n"
"            some tmp directory under AFNI's website.\n"
"     tar xvzf FACTID_draft.tar.gz\n"
"     tract_test \\\n"
"           -mask1 TEST_FILES/DTI/mask_ballF+orig \\\n"
"           -mask2 TEST_FILES/DTI/mask_ballG+orig \\\n"
"           -prefix TEST_FILES/DTI/TRACK_ballFG \\\n"
"           -input TEST_FILES/DTI/DT \\\n"
"           -algopt TEST_FILES/ALGOPTS.dat \\\n"
"           -logic AND\n"
"   \n" );
  return;
}

// following TrackVis requirements for *.trk file, defining the header as a struct
struct io_header
{
  char	        id_string[6];	// 6 	ID string for track file. The first 5 characters must be "TRACK".
  short int	dim[3];	        // 6 	Dimension of the image volume.
  float	        voxel_size[3];	// 12 	Voxel size of the image volume.
  float	        origin[3];	// 12 	Origin of the image volume. This field is not yet being used by TrackVis. That means the origin is always (0, 0, 0).
  short int	n_scalars;	// 2 	Number of scalars saved at each track point (besides x, y and z coordinates).
  char	        scal_n[10][20];	// 200 	Name of each scalar. Cannot be longer than 20 characters each. Can only store up to 10 names.
  short int	n_properties;	// 2 	Number of properties saved at each track.
  char	        prop_n[10][20];	// 200 	Name of each property. Cannot be longer than 20 characters each. Can only store up to 10 names.
  float	        vox_to_ras[4][4];// 64 	4x4 matrix for voxel to RAS (crs to xyz) transformation. If vox_to_ras[3][3] is 0, the matrix is not recorded
  char	        reserved[444];	// 444 	Reserved space for future version.
  char	        voxel_order[4];	// 4 	Storing order of the original image data. Explained here.
  char	        pad2[4];	// 4 	Paddings.
  float	        img_orient_p[6];// 24 	Image orientation of the original image. As defined in the DICOM header.
  char	        pad1[2];	// 2 	Paddings.
  unsigned char	invert_x;	// 1 	Inversion/rotation flags used to generate this track file. For internal use only.
  unsigned char	invert_y;	// 1 	As above.
  unsigned char	invert_z;	// 1 	As above.
  unsigned char	swap_xy;	// 1 	As above.
  unsigned char	swap_yz;	// 1 	As above.
  unsigned char	swap_zx;	// 1 	As above.
  int	        n_count;	// 4 	Number of tracks stored in this track file. 0 means the number was NOT stored.
  int	        version;      // 4 	Version number. Current version is 2.
  int	        hdr_size;	// 4 	Size of the header. Used to determine byte swap. Should be 1000.
} header1 = {.id_string = "TRACK\0", 
	     .origin = {0,0,0},	
	     .n_scalars = 3,
	     .scal_n[0] = "FA",
	     .scal_n[1] = "MD",
	     .scal_n[2] = "L1",
	     .n_properties = 0,
	     .vox_to_ras = { {0.,0.,0.,0.},{0.,0.,0.,0.},{0.,0.,0.,0.},{0.,0.,0.,0.}},
	     .voxel_order = "LPS\0", // !!!!! will reset this later based on actual data set!!1
	     .invert_x = 0,
	     .invert_y = 0,
	     .invert_z = 0,
	     .swap_xy = 0,
	     .swap_yz = 0,
	     .swap_zx = 0,
	     .n_count = 0,
	     .version = 2,
	     .hdr_size = 1000};	


int main(int argc, char *argv[]) {
  

  // AFNIadd
  int i,j,k,m,n,aa,ii,jj,kk;
  int iarg;
  byte *mask1=NULL ; // !!! may or may not use these??
  byte *mask2=NULL ; 
  int nmask1=0;
  int nmask2=0;
  int mmm;
  THD_3dim_dataset *insetFA = NULL, *insetV1 = NULL, 
                   *insetMD = NULL, *insetL1 = NULL;
  THD_3dim_dataset *outsetMAP, *outsetMASK;
  char *prefix="tracky" ;
  //int nvalsFA, nvalsV1, nvoxFA, nvoxV1;
  int LOG_TYPE=0;
  char in_FA[300];
  char in_V1[300];
  char in_MD[300];
  char in_L1[300];

  char OUT_bin[300];
  char OUT_tracstat[300];
  char OUT_map[300];
  char OUT_mask[300];
  char prefix_mask[300];
  char prefix_map[300];

  THD_3dim_dataset *mset2=NULL; 
  THD_3dim_dataset *mset1=NULL; 

  // FACT algopts
  FILE *fin4, *fin1, *fout0;
  float MinFA,MaxAng,MinL;
  float SideAng = 45.0; // opening angle for ID
  int SeedPerV[3];
  int ArrMax;
  float frac; 

  int Nvox=-1;   // tot number vox
  int Dim[3]; // dim in each dir
  int Nseed,M,bval;
  int DimSeed[3]; // number of seeds there will be
  float Ledge[3]; // voxel edge lengths

  int *ROI1, *ROI2;
  float *temp_arr;
  char *temp_byte; 
  int **Tforw, **Tback;
  float **flTforw, **flTback;
  float ****coorded;
  int ****INDEX;
  int len_forw, len_back; // used to be int count of num of squares through
  float phys_forw[1], phys_back[1];
  int idx;

  float ave_tract_len, ave_tract_len_phys;
  int inroi1, inroi2, KEEPIT; // switches for detecting
  int in[3]; // to pass to trackit
  float physin[3]; // also for trackit, physical loc, to have more than one seed per vox
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

  
  mainENTRY("tract_test"); machdep(); /* ZSS: Good for variety of small things*/
   
  // ************************************************************************
  // ************************************************************************
  //                    load AFNI stuff
  // ************************************************************************
  // ************************************************************************

  /** scan args **/
  iarg = 1;
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0 ) {
      usage_tract_test(strlen(argv[iarg])>3 ? 2:1);
      exit(0);
    }
    if( strcmp(argv[iarg],"-mask1") == 0 ){
      //THD_3dim_dataset *mset1 ; //int mmm ;
      if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask1'") ;
      if( mask1 != NULL ) ERROR_exit("Can't have two '-mask1' options") ;
      mset1 = THD_open_dataset( argv[iarg] ) ;
      if( mset1 == NULL ) ERROR_exit("Can't open mask1 dataset '%s'",argv[iarg]) ;
      DSET_load(mset1) ; CHECK_LOAD_ERROR(mset1) ;
      nmask1 = DSET_NVOX(mset1) ;
      mask1  = THD_makemask( mset1 , 0 , 1.0f,-1.0f ) ;// DSET_delete(mset1) ; //!!!?? maybe use?
      if( mask1 == NULL ) ERROR_exit("Can't make mask1 from dataset '%s'",argv[iarg]) ;
      mmm = THD_countmask( nmask1 , mask1 ) ;
      INFO_message("Number of voxels in mask1 = %d",mmm) ;
      if( mmm < 2 ) ERROR_exit("Mask1 is too small to process") ;
      iarg++ ; continue ;
    }
    if( strcmp(argv[iarg],"-mask2") == 0 ){
      //THD_3dim_dataset *mset2 ; //int mmm ;
      if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask2'") ;
      if( mask2 != NULL ) ERROR_exit("Can't have two '-mask2' options") ;
      mset2 = THD_open_dataset( argv[iarg] ) ;
      if( mset2 == NULL ) ERROR_exit("Can't open mask2 dataset '%s'",argv[iarg]) ;
      DSET_load(mset2) ; CHECK_LOAD_ERROR(mset2) ;
      nmask2 = DSET_NVOX(mset2) ;
      mask2  = THD_makemask( mset2 , 0 , 1.0f,-1.0f ) ; //DSET_delete(mset2) ;//!!!?? maybe use?
      if( mask2 == NULL ) ERROR_exit("Can't make mask2 from dataset '%s'",argv[iarg]) ;
      mmm = THD_countmask( nmask2 , mask2 ) ;
      INFO_message("Number of voxels in mask2 = %d",mmm) ;
      if( mmm < 2 ) ERROR_exit("Mask2 is too small to process") ;
      iarg++ ; continue ;
    }
      
    if( strcmp(argv[iarg],"-prefix") == 0 ){
      iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-prefix'");
      prefix = strdup(argv[iarg]) ;
      if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal name after '-prefix'");
      iarg++ ; continue ;
    }
      
    if( strcmp(argv[iarg],"-input") == 0 ){
      //if( inset != NULL ) ERROR_exit("Can't use 2 '-input' options") ;
      iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-input'");
	
      sprintf(in_FA,"%s_FA+orig", argv[iarg]); 
      insetFA = THD_open_dataset(in_FA) ;//argv[iarg] ) ;
      if( insetFA == NULL ) ERROR_exit("Can't open dataset '%s':FA",in_FA);//argv[iarg]) ;
      //INFO_message("Loading FA dataset '%s'",in_FA);//argv[iarg]) ;
      DSET_load(insetFA) ; CHECK_LOAD_ERROR(insetFA) ;
      //nvalsFA = DSET_NVALS(insetFA) ; 
      Nvox = DSET_NVOX(insetFA) ;
      Dim[0] = DSET_NX(insetFA); Dim[1] = DSET_NY(insetFA); Dim[2] = DSET_NZ(insetFA); 
      Ledge[0] = DSET_DX(insetFA); Ledge[1] = DSET_DY(insetFA); Ledge[2] = DSET_DZ(insetFA); 
      INFO_message("Dimensions: %d,%d,%d.  Nvox: %d",Dim[0],Dim[1],Dim[2],Nvox);//argv[iarg]) ;

      // check tot num vox match (as proxy for dims...)
      if( (Nvox != nmask1) || (Nvox != nmask2) )
	ERROR_exit("Input dataset does not match both mask volume!");
	
      sprintf(in_V1,"%s_V1+orig", argv[iarg]); 
      insetV1 = THD_open_dataset(in_V1);//argv[iarg] ) ;
      if( insetV1 == NULL ) ERROR_exit("Can't open dataset '%s':V1",in_V1);//argv[iarg]) ;
      //INFO_message("Loading V1 dataset '%s'",in_V1);//argv[iarg]) ;
      DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;
      //nvalsV1 = DSET_NVALS(insetV1) ;// nvoxV1 = DSET_NVOX(insetV1) ; // !!!just for floatcheck?
      //INFO_message("NVALS in V1 '%d' NVALS in FA '%d'  ",nvalsV1,nvalsFA);//argv[iarg]) ;

      sprintf(in_MD,"%s_MD+orig", argv[iarg]); 
      insetMD = THD_open_dataset(in_MD);//argv[iarg] ) ;
      if( insetMD == NULL ) ERROR_exit("Can't open dataset '%s':MD",in_MD);//argv[iarg]) ;
      //INFO_message("Loading MD dataset '%s'",in_MD);//argv[iarg]) ;
      DSET_load(insetMD) ; CHECK_LOAD_ERROR(insetMD) ;

      sprintf(in_L1,"%s_L1+orig", argv[iarg]); 
      insetL1 = THD_open_dataset(in_L1);//argv[iarg] ) ;
      if( insetL1 == NULL ) ERROR_exit("Can't open dataset '%s':L1",in_L1);//argv[iarg]) ;
      //INFO_message("Loading L1 dataset '%s'",in_L1);//argv[iarg]) ;
      DSET_load(insetL1) ; CHECK_LOAD_ERROR(insetL1) ;

      iarg++ ; continue ;
    }

    if( strcmp(argv[iarg],"-algopt") == 0 ){
      iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-algopt'");
	
      // Opening/Reading in FACT params
      if( (fin4 = fopen(argv[iarg], "r")) == NULL) {
	fprintf(stderr, "Error opening file %s.",argv[iarg]);
	exit(19);
      }
      fscanf(fin4, "%f %f %f %d %d %d %d %d",
	     &MinFA,&MaxAng,&MinL,&SeedPerV[0],&SeedPerV[1],&SeedPerV[2],&M,&bval);
      fclose(fin4);
	
      for( i=0 ; i<3 ; i++)
	DimSeed[i] = Dim[i]*SeedPerV[i];
      Nseed = Nvox*SeedPerV[0]*SeedPerV[1]*SeedPerV[2];
	
      // convert to radians for comparisons b/c using acos!
      MaxAng*= CONV; 

      iarg++ ; continue ;
    }

    if( strcmp(argv[iarg],"-logic") == 0 ){
      iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-logic'");
      //printf("\n\n\tlogic string: '%s'\t true %d\n",argv[iarg], argv[iarg] =="AND");
      INFO_message("ROI logic type is: %s",argv[iarg]);//argv[iarg]) ;
      if( strcmp(argv[iarg],"AND") == 0 ) 
	LOG_TYPE = 1;
      else if( strcmp(argv[iarg],"OR") == 0 ) 
	LOG_TYPE = 0;
      else ERROR_exit("Illegal name after '-logic': need 'OR' or 'AND' ");
      iarg++ ; continue ;
    }
    
    /* ZSS: Help the poor user */
     ERROR_message("Bad option '%s'\n",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);
  }
  
  // ************************************************************************
  // ************************************************************************
  //                    make arrays for tracking
  // ************************************************************************
  // ************************************************************************

  // for temp storage array, just try making a multiple of longest dimension!
  if(Dim[0] > Dim[1])
    ArrMax = Dim[0] * 4;
  else
    ArrMax = Dim[1] * 4;
  if(4*Dim[2] > ArrMax)
    ArrMax = Dim[2] * 4;

  ROI1 = (int *)malloc(Nvox * sizeof(int)); 
  ROI2 = (int *)malloc(Nvox * sizeof(int)); 
  temp_arr = (float *)malloc(Nvox * sizeof(float)); 
  temp_byte = (char *)malloc(Nvox * sizeof(char)); 
  Tforw = malloc(ArrMax*sizeof(Tforw)); // temp storage whilst tracking
  for(i=0 ; i<ArrMax ; i++) 
    Tforw[i] = malloc(3*sizeof(int)); 
  Tback = malloc(ArrMax*sizeof(Tback)); // temp storage whilst tracking
  for(i=0 ; i<ArrMax ; i++) 
    Tback[i] = malloc(3*sizeof(int)); 
  flTforw = malloc(ArrMax*sizeof(flTforw)); // temp storage whilst tracking, *physical* loc
  for(i=0 ; i<ArrMax ; i++) 
    flTforw[i] = malloc(3*sizeof(int)); 
  flTback = malloc(ArrMax*sizeof(flTback)); // temp storage whilst tracking, *physical* loc
  for(i=0 ; i<ArrMax ; i++) 
    flTback[i] = malloc(3*sizeof(int)); 
  if( //(FA == NULL) || (L1 == NULL) || (MD == NULL) || 
      (ROI1 == NULL) || (ROI2 == NULL) || (temp_arr == NULL) 
      || (Tforw == NULL) || (Tback == NULL) || (flTforw == NULL) 
      || (flTback == NULL)) {
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(12);
  }
  
  coorded = (float ****) malloc( Dim[0] * sizeof(float ***) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    coorded[i] = (float ***) malloc( Dim[1] * sizeof(float **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      coorded[i][j] = (float **) malloc( Dim[2] * sizeof(float *) );
  for ( i=0 ; i<Dim[0] ; i++ ) 
    for ( j=0 ; j<Dim[1] ; j++ ) 
      for ( k= 0 ; k<Dim[2] ; k++ ) 
	coorded[i][j][k] = (float *) malloc( 4 * sizeof(float) ); //3 comp and FA

  INDEX = (int ****) malloc( Dim[0] * sizeof(int ***) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    INDEX[i] = (int ***) malloc( Dim[1] * sizeof(int **) );
  for ( i = 0 ; i < Dim[0] ; i++ ) 
    for ( j = 0 ; j < Dim[1] ; j++ ) 
      INDEX[i][j] = (int **) malloc( Dim[2] * sizeof(int *) );
  for ( i=0 ; i<Dim[0] ; i++ ) 
    for ( j=0 ; j<Dim[1] ; j++ ) 
      for ( k= 0 ; k<Dim[2] ; k++ ) 
	INDEX[i][j][k] = (int *) malloc( 4 * sizeof(int) );

  if( (INDEX == NULL) || (coorded == NULL) ) {
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(122);
  }
  
  
  for(i=0 ; i<Nvox ; i++) {
    if(THD_get_voxel( mset1, i, 0) >0.0005){
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
	coorded[i][j][k][3] = THD_get_voxel(insetFA, idx, 0); 

	INDEX[i][j][k][0] =idx;  // first value is the index itself
	if( ROI1[idx]==1 ) 
	  INDEX[i][j][k][1]=1; // second value identifies ROI1 mask
	else
	  INDEX[i][j][k][1]=0;
	if( ROI2[idx]==1 )
	  INDEX[i][j][k][2]=1;   // third value identifies ROI2 mask
	else
	  INDEX[i][j][k][2]=0;
	INDEX[i][j][k][3] = 0;  // fourth value will be counter for number of kept tracks passing through
	idx+= 1;
      }
  
  

  // ************************************************************************
  // ************************************************************************
  //                    Beginning of main loop
  // ************************************************************************
  // ************************************************************************

  SideAng/= 2.0; // just how it's defined;  frac worked out on halfangle...
  frac = (float) sin(SideAng*CONV)/sin((90-SideAng)*CONV);

  Numtract = 0;
  ave_tract_len = 0.;
  ave_tract_len_phys = 0.;
  
  
  for( i=0 ; i<3 ; i++) {
    header1.dim[i] = Dim[i];
    header1.voxel_size[i] = Ledge[i];
  }

  //!!!!!!!! put in about RPI, etc. here!

  //for( i=0 ; i<444 ; i++) 
  // header1.reserved[i] = "0";
  
  sprintf(OUT_bin,"%s.trk",prefix);
  if( (fout0 = fopen(OUT_bin, "w")) == NULL) {
    fprintf(stderr, "Error opening file %s.",OUT_bin);
    exit(16);
  }
  fwrite(&header1,sizeof(struct io_header),1,fout0);

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
		physin[0] = ((float) in[0] + (0.5 + (float) ii)/SeedPerV[0])*Ledge[0];
		physin[1] = ((float) in[1] + (0.5 + (float) jj)/SeedPerV[1])*Ledge[1];
		physin[2] = ((float) in[2] + (0.5 + (float) kk)/SeedPerV[2])*Ledge[2];
		
		
		len_forw = TrackIt( coorded, in, physin, Ledge, Dim, MinFA, 
				    MaxAng, ArrMax, Tforw, flTforw, 1, frac, phys_forw);
		
		in[0] = i; // reset, because it's changed in TrackIt func
		in[1] = j;
		in[2] = k;
		physin[0] = ((float) in[0] + (0.5 + (float) ii)/SeedPerV[0])*Ledge[0];
		physin[1] = ((float) in[1] + (0.5 + (float) jj)/SeedPerV[1])*Ledge[1];
		physin[2] = ((float) in[2] + (0.5 + (float) kk)/SeedPerV[2])*Ledge[2];

		len_back = TrackIt( coorded, in, physin, Ledge, Dim, MinFA, 
				    MaxAng, ArrMax, Tback, flTback, -1,frac, phys_back);
		
		KEEPIT = 0; //switch
		totlen = len_forw+len_back-1; // b/c of overlap of starts
		totlen_phys = phys_forw[0] + phys_back[0];

		if( totlen_phys >= MinL ) {
		  inroi1 = 0;
		  // check forw
		  for( n=0 ; n<len_forw ; n++) {
		    if( INDEX[ Tforw[n][0] ][ Tforw[n][1] ][ Tforw[n][2] ][1]==1) {
		      inroi1 = 1;
		      break;
		    }
		    else
		      continue;
		  }
		  if( inroi1==0 ) { //after first half, check second half
		    for( m=0 ; m<len_back ; m++) {
		      if( INDEX[ Tback[m][0] ][ Tback[m][1] ][ Tback[m][2] ][1]==1) {
			inroi1 = 1;
			break;
		      }
		      else
			continue;
		    }
		  }
		  
		  
		  if(  ((LOG_TYPE ==0) && (inroi1 ==0)) || ((LOG_TYPE ==1) && (inroi1 ==1))) {
		    // have to check in ROI2
		    
		    inroi2 = 0;
		    // check forw
		    for( n=0 ; n<len_forw ; n++) {
		      if( INDEX[ Tforw[n][0] ][ Tforw[n][1] ][ Tforw[n][2] ][2]==1) {
			inroi2 = 1;
		  break;
		      }
		      else
			continue;
		    }
		    if( inroi2==0 ) { //after first half, check second half
		      for( m=0 ; m<len_back ; m++) {
			if( INDEX[ Tback[m][0] ][ Tback[m][1] ][ Tback[m][2] ][2]==1) {
			  inroi2 = 1;
			  break;
			}
			else
			  continue;
		      }
		    }
		    
		    // for both cases, need to see it here to keep
		    if( inroi2 ==1 )
		      KEEPIT = 1; // otherwise, it's gone
		    
		  }
		  else if((LOG_TYPE ==0) && (inroi1 ==1))
		    KEEPIT = 1;
		}
				
		// by now, we *know* if we're keeping this or not.
		if( KEEPIT ==1 ) {
		  
		  READS_in = totlen;
		  fwrite(&READS_in,sizeof(READS_in),1,fout0);
		  for( n=0 ; n<len_back ; n++) {
		    m = len_back - 1 - n; //put this one in backwords, to make it connect
		    for(aa=0 ; aa<3 ; aa++) {
		      READS_fl =  flTback[m][aa];
		      fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    }

		    READS_fl =THD_get_voxel(insetFA, INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][0], 0);// MD
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    READS_fl =THD_get_voxel(insetMD, INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][0], 0);// MD
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    READS_fl =THD_get_voxel(insetL1, INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][0], 0);// L1 
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    
		  }
		  
		  for( m=1 ; m<len_forw ; m++) {
		    for(aa=0 ; aa<3 ; aa++) {
		      READS_fl = flTforw[m][aa];
		      fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    }

		    READS_fl =THD_get_voxel(insetFA, INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][0], 0);// FA
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    READS_fl =THD_get_voxel(insetMD, INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][0], 0);// MD
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    READS_fl =THD_get_voxel(insetL1, INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][0], 0);// L1 
		    fwrite(&READS_fl,sizeof(READS_fl),1,fout0);
		    
		  }
		  
		  for( m=1 ; m<len_forw ; m++) 
		    INDEX[Tforw[m][0]][Tforw[m][1]][Tforw[m][2]][3]+= 1; 
		  for( n=0 ; n<len_back ; n++) {
		    m = len_back - 1 - n; //put this one in backwords, to make it connect
		    INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][3]+= 1; 
		  }
		  
		  ave_tract_len+= totlen;
		  ave_tract_len_phys+= totlen_phys;
		  Numtract+=1;
		}	
	      }
	}
  fclose(fout0); 


  // ************************************************************************
  // ************************************************************************
  //                    Some simple stats on ROIs and outputs
  // ************************************************************************
  // ************************************************************************

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
    fprintf(fout0,"%.3f\t%.3f\n",ave_tract_len/Numtract,ave_tract_len_phys/Numtract);
    fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_FA,roi3_sd_FA); // as usual, these values would 
    fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_MD,roi3_sd_MD); // have to be divided by the bval
    fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_RD,roi3_sd_RD); // to get their actual value in 
    fprintf(fout0,"%.4f\t%.4f\n",roi3_mu_L1,roi3_sd_L1); // standard phys units
    fclose(fout0);

    
    sprintf(prefix_map,"%s_MAP",prefix); 
    sprintf(prefix_mask,"%s_MASK",prefix); 


    // !!! have to check on the particulars of this!!!
    outsetMAP = EDIT_empty_copy( mset1 ) ;
    EDIT_dset_items( outsetMAP ,
    		     ADN_datum_all , MRI_float , // !!!! why can't I make this an MRI_int??
    		     ADN_prefix    , prefix_map ,
		     //		     ADN_nvals     , nrow ,
		     //		     ADN_ntt       , HAS_TIMEAXIS(inset) ? nrow : 0 ,
    		     ADN_none ) ;
    if( THD_is_ondisk(DSET_HEADNAME(outsetMAP)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",DSET_HEADNAME(outsetMAP));
    
    outsetMASK = EDIT_empty_copy( mset1 ) ;
    EDIT_dset_items( outsetMASK ,
    		     ADN_datum_all , MRI_byte , // !!!! why can't I make this an MRI_int??
    		     ADN_prefix    , prefix_mask ,
		     //		     ADN_nvals     , nrow ,
		     //		     ADN_ntt       , HAS_TIMEAXIS(inset) ? nrow : 0 ,
    		     ADN_none ) ;
    if( THD_is_ondisk(DSET_HEADNAME(outsetMASK)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",DSET_HEADNAME(outsetMASK));
    
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
    
    EDIT_substitute_brick( outsetMAP , 0 , MRI_float , temp_arr ) ;
    THD_load_statistics( outsetMAP ) ;
    THD_write_3dim_dataset( NULL,NULL , outsetMAP , True ) ;
    WROTE_DSET(outsetMAP) ;
    EDIT_substitute_brick( outsetMASK , 0 , MRI_byte , temp_byte ) ;
    THD_load_statistics( outsetMASK ) ;
    THD_write_3dim_dataset( NULL,NULL , outsetMASK , True ) ;
    WROTE_DSET(outsetMASK) ;
    
  }
  else 
    INFO_message("\n No Tracts Found!!!\n");



  

  // ************************************************************************
  // ************************************************************************
  //                    Freeing
  // ************************************************************************
  // ************************************************************************

  // !!! need to free afni-sets?
  free(mask1);
  free(mask2);
  free(prefix);
  free(insetV1);
  free(insetFA);
  free(mset1);
  free(mset2);
  
  free(ROI1);
  free(ROI2);
  free(temp_arr);
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


  INFO_message("Done done.");

  return 0;
}

