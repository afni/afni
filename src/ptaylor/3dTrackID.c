/* 
   FACTID code, from Taylor, Kuan-Hung, Lin and Biswal (some year!)
   
   first draft at AFNIfication, March 2012.
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

//#define EPS_V (0.0000001) // for eigvec 'vel' to not have badness dividing
//#define CONV (3.141592654/180)

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



void usage_TrackID(int detail) 
{
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
"\n"
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
"               for just one ROI, make both `mask's the same file and use," 
"               e.g., AND]\n"
"\n"
"   If you use this program, please reference:\n"
"     Taylor, Kuan-Hung, Lin and Biswal 2012\n"
"\n"
"   Example:\n"
"     Download and install this archive:\n"
"     curl -O http://..../FACTID_draft.tar.gz if you like I can put it under\n"
"            some tmp directory under AFNI's website.\n"
"     tar xvzf FACTID_draft.tar.gz\n"
"     3dTrackID \\\n"
"           -mask1 TEST_FILES/DTI/mask_ballF+orig \\\n"
"           -mask2 TEST_FILES/DTI/mask_ballG+orig \\\n"
"           -prefix TEST_FILES/DTI/o.TRACK_ballFG \\\n"
"           -input TEST_FILES/DTI/DT \\\n"
"           -algopt TEST_FILES/ALGOPTS.dat \\\n"
"           -logic AND\n"
"   \n" );
  return;
}

// TrackVis requirements for *.trk file, defining the header as a struct
struct io_header
{
  // 6 	 ID string for track file. The first 5 characters must be "TRACK".
  char	        id_string[6];	
  // 6   Dimension of the image volume.
  short int	dim[3];	        
  // 12  Voxel size of the image volume.
  float	        voxel_size[3];	
  // 12  Origin of the image volume (Field is not yet being used by
  //     TrackVis). Origin is always (0, 0, 0).
  float	        origin[3];	
  // 2   Number of scalars saved at each track point (besides x, y and z 
  //     coordinates).
  short int	n_scalars;	
  // 200 Name of each scalar. Cannot be longer than 20 characters
  //     each. Can only store up to 10 names.
  char	        scal_n[10][20];	
  // 2   Number of properties saved at each track.
  short int	n_properties;	
  // 200 Name of each property. Cannot be longer than 20 characters
  //     each. Can only store up to 10 names.
  char	        prop_n[10][20];	
  // 64  4x4 matrix for voxel to RAS (crs to xyz) transf. If
  //     vox_to_ras[3][3] is 0, matrix not recorded.
  float	        vox_to_ras[4][4];
  char	        reserved[444];  // 444 Reserved space for future version.
  // 4 	 Storing order of the original image data. Explained here.
  char	        voxel_order[4];	
  char	        pad2[4];  // 4 	 Paddings.	
  // 24  Image orientation of the original image. As defined in the
  //     DICOM header.
  float	        img_orient_p[6];
  char	        pad1[2];  // 2 	 Paddings.	
  // 1 	 Inversion/rotation flags used to generate this track file. For 
  // internal use only.	
  unsigned char	invert_x;  
  unsigned char	invert_y;  // 1 As above.	
  unsigned char	invert_z;  // 1 As above.	
  unsigned char	swap_xy;   // 1 As above.
  unsigned char	swap_yz;   // 1 As above.
  unsigned char	swap_zx;   // 1 As above.
  // 4 	 Number of tracks stored in this track file. 0 means the number 
  //     was NOT stored.
  int	        n_count;  // 4 	 Version number. Current version is 2.
  int	        version;      
  // 4 	 Size of the header. Used to determine byte swap. Should be 1000.
  int	        hdr_size;	
} header1 = {.id_string = "TRACK\0", 
	     .origin = {0,0,0},	
	     .n_scalars = 3,
	     .scal_n[0] = "FA",
	     .scal_n[1] = "MD",
	     .scal_n[2] = "L1",
	     .n_properties = 0,
	     .vox_to_ras = { {0.,0.,0.,0.},{0.,0.,0.,0.},
			     {0.,0.,0.,0.},{0.,0.,0.,0.}},
	     // !!!!! will reset this later based on actual data set!!1
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

int main(int argc, char *argv[]) {
  int i,j,k,m,n,aa,ii,jj,kk,mm;
  int iarg;
  //  byte *mask1=NULL ; // !!! may or may not use these??
  // byte *mask2=NULL ; 
  int nmask1=0;
  int nmask2=0;
  int mmm;
  THD_3dim_dataset *insetFA = NULL, *insetV1 = NULL, 
                   *insetMD = NULL, *insetL1 = NULL;
  THD_3dim_dataset *mset2=NULL; 
  THD_3dim_dataset *mset1=NULL; 
  THD_3dim_dataset *outsetMAP=NULL, *outsetMASK=NULL;
  char *prefix="tracky" ;
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

  // FACT algopts
  FILE *fin4, *fin1, *fout0;
  float MinFA,MaxAng,MinL;
  //  float SideAng = 45.0; // opening angle for ID
  int SeedPerV[3];
  int ArrMax;
  //  float frac; 

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

  char READS_ch;
  short int READS_sh; 
  int READS_in;
  float READS_fl;

  int  roi3_ct=0;
  float roi3_mu_MD = 0.,roi3_mu_RD = 0.,roi3_mu_L1 = 0.,roi3_mu_FA = 0.;  
  float roi3_sd_MD = 0.,roi3_sd_RD = 0.,roi3_sd_L1 = 0.,roi3_sd_FA = 0.;  
  float tempMD,tempFA,tempRD,tempL1;
  char dset_or[3] = "RAI";
  THD_3dim_dataset *dsetn;
  int TV_switch[3] = {0,0,0};

  mainENTRY("3dTrackID"); machdep(); 
   
  // ****************************************************************
  // ****************************************************************
  //                    load AFNI stuff
  // ****************************************************************
  // ****************************************************************

  /** scan args **/
  iarg = 1;
  while( iarg < argc && argv[iarg][0] == '-' ){
    if( strcmp(argv[iarg],"-help") == 0 || 
	strcmp(argv[iarg],"-h") == 0 ) {
      usage_TrackID(strlen(argv[iarg])>3 ? 2:1);
      exit(0);
    }
    if( strcmp(argv[iarg],"-mask1") == 0 ){
      if( ++iarg >= argc ) 
	ERROR_exit("Need argument after '-mask1'") ;
      //if( mask1 != NULL ) 
      //	ERROR_exit("Can't have two '-mask1' options") ;
      mset1 = THD_open_dataset( argv[iarg] ) ;
      if( mset1 == NULL ) 
	ERROR_exit("Can't open mask1 dataset '%s'", argv[iarg]) ;
      DSET_load(mset1) ; CHECK_LOAD_ERROR(mset1) ;
      nmask1 = DSET_NVOX(mset1) ;
      //mask1  = THD_makemask( mset1 , 0 , 1.0f,-1.0f ) ;// DSET_delete(mset1) ; //!!!?? maybe use?
      //if( mask1 == NULL ) 
      //ERROR_exit("Can't make mask1 from dataset '%s'",argv[iarg]) ;
      //mmm = THD_countmask( nmask1 , mask1 ) ;
      //INFO_message("Number of voxels in mask1 = %d",mmm) ;
      //if( mmm < 2 ) ERROR_exit("Mask1 is too small to process") ;
      iarg++ ; continue ;
    }
    if( strcmp(argv[iarg],"-mask2") == 0 ){
      //THD_3dim_dataset *mset2 ; //int mmm ;
      if( ++iarg >= argc ) 
	ERROR_exit("Need argument after '-mask2'") ;
      //if( mask2 != NULL ) 
      //	ERROR_exit("Can't have two '-mask2' options") ;
      mset2 = THD_open_dataset( argv[iarg] ) ;
      if( mset2 == NULL ) 
	ERROR_exit("Can't open mask2 dataset '%s'",
				     argv[iarg]) ;
      DSET_load(mset2) ; CHECK_LOAD_ERROR(mset2) ;
      nmask2 = DSET_NVOX(mset2) ;
      //mask2  = THD_makemask( mset2 , 0 , 1.0f,-1.0f ) ; //DSET_delete(mset2) ;//!!!?? maybe use?
      // if( mask2 == NULL ) 
      //	ERROR_exit("Can't make mask2 from dataset '%s'",argv[iarg]) ;
      //mmm = THD_countmask( nmask2 , mask2 ) ;
      //INFO_message("Number of voxels in mask2 = %d",mmm) ;
      //if( mmm < 2 ) ERROR_exit("Mask2 is too small to process") ;
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
      //INFO_message("Dimensions: %d,%d,%d.  Nvox: %d",
      //	      Dim[0],Dim[1],Dim[2],Nvox);

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
	TV_switch[i] = (dset_or[i]==header1.voxel_order[i]);
      }

      sprintf(in_V1,"%s_V1+orig", argv[iarg]); 
      insetV1 = THD_open_dataset(in_V1);
      if( insetV1 == NULL ) 
	ERROR_exit("Can't open dataset '%s':V1",in_V1);
      DSET_load(insetV1) ; CHECK_LOAD_ERROR(insetV1) ;

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
      fscanf(fin4, "%f %f %f %d %d %d %d %d",
	     &MinFA,&MaxAng,&MinL,&SeedPerV[0],&SeedPerV[1],
	     &SeedPerV[2],&M,&bval);
      fclose(fin4);
	
      for( i=0 ; i<3 ; i++)
	DimSeed[i] = Dim[i]*SeedPerV[i];
      Nseed = Nvox*SeedPerV[0]*SeedPerV[1]*SeedPerV[2];
	
      // convert to cos of rad value for comparisons, instead of using acos()
      MaxAng = cos(CONV*MaxAng); 

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
      else 
	ERROR_exit("Illegal after '-logic': need 'OR' or 'AND'");
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
  
  // at some point, we will have to convert indices into
  // pseudo-locations; being forced into this choice means that
  // different data set orientations would be represented differently
  // and incorrectly in some instances... so, for now, we'll resample
  // everything to RAI, and then resample back later.  guess this will
  // just slow things down slightly.
   
  /* !!!! why didn't this work to actually change things?
    printf("\n before %c%c%c\n" ,ORIENT_typestr[insetFA->daxes->xxorient][0],ORIENT_typestr[insetFA->daxes->yyorient][0],ORIENT_typestr[insetFA->daxes->zzorient][0]);

    EDIT_DSET_ORIENT(insetFA,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );
    
    EDIT_DSET_ORIENT(insetMD,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );

    EDIT_DSET_ORIENT(insetL1,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );

    EDIT_DSET_ORIENT(insetV1,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );

    EDIT_DSET_ORIENT(mset1,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );

    EDIT_DSET_ORIENT(mset2,
		     ORCODE(dset_or[0]),
		     ORCODE(dset_or[1]),
		     ORCODE(dset_or[2]) );
    printf("\n after %c%c%c\n" ,ORIENT_typestr[insetFA->daxes->xxorient][0],ORIENT_typestr[insetFA->daxes->yyorient][0],ORIENT_typestr[insetFA->daxes->zzorient][0]);
    printf("\n after %c%c%c\n" ,ORIENT_typestr[insetMD->daxes->xxorient][0],ORIENT_typestr[insetMD->daxes->yyorient][0],ORIENT_typestr[insetMD->daxes->zzorient][0]);
    printf("\n after %c%c%c\n" ,ORIENT_typestr[insetL1->daxes->xxorient][0],ORIENT_typestr[insetL1->daxes->yyorient][0],ORIENT_typestr[insetL1->daxes->zzorient][0]);
    printf("\n after %c%c%c\n" ,ORIENT_typestr[insetV1->daxes->xxorient][0],ORIENT_typestr[insetV1->daxes->yyorient][0],ORIENT_typestr[insetV1->daxes->zzorient][0]);
    printf("\n after %c%c%c\n" ,ORIENT_typestr[mset1->daxes->xxorient][0],ORIENT_typestr[mset1->daxes->yyorient][0],ORIENT_typestr[mset1->daxes->zzorient][0]);
    printf("\n after %c%c%c\n" ,ORIENT_typestr[mset2->daxes->xxorient][0],ORIENT_typestr[mset2->daxes->yyorient][0],ORIENT_typestr[mset2->daxes->zzorient][0]);
  */

    

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

  ROI1 = (int *)malloc(Nvox * sizeof(int)); 
  ROI2 = (int *)malloc(Nvox * sizeof(int)); 
  temp_arr = (float *)malloc(Nvox * sizeof(float)); 
  temp_byte = (char *)malloc(Nvox * sizeof(char)); 
  // temp storage whilst tracking
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
  if( (ROI1 == NULL) || (ROI2 == NULL) || (temp_arr == NULL) 
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
      for ( k= 0 ; k<Dim[2] ; k++ ) //3 comp of V1 and FA
	coorded[i][j][k] = (float *) malloc( 4 * sizeof(float) ); 
  
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
    if(THD_get_voxel( mset1, i, 0) >0.5){
      ROI1[i] = 1;
    }
    if(THD_get_voxel( mset2, i, 0) >0.5)
      ROI2[i] = 1;
    //printf("\n%d\t%d",i,ROI1[i]);
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

  //  SideAng/= 2.0; // just how it's defined; frac of halfangle...
  //frac = (float) sin(SideAng*CONV)/sin((90-SideAng)*CONV);

  Numtract = 0;
  ave_tract_len = 0.;
  ave_tract_len_phys = 0.;
 

 
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
		physin[0] = ((float) in[0] + 
			     (0.5 + (float) ii)/SeedPerV[0])*Ledge[0];
		physin[1] = ((float) in[1] + 
			     (0.5 + (float) jj)/SeedPerV[1])*Ledge[1];
		physin[2] = ((float) in[2] + 
			     (0.5 + (float) kk)/SeedPerV[2])*Ledge[2];
		
		len_forw = TrackIt(coorded, in, physin, Ledge, Dim, 
				   MinFA, MaxAng, ArrMax, Tforw, 
				   flTforw, 1, phys_forw);

		in[0] = i; // reset, because it's changed in TrackIt func
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
		totlen = len_forw+len_back-1; // b/c of overlap of starts
		totlen_phys = phys_forw[0] + phys_back[0];

		if( totlen_phys >= MinL ) {
		  inroi1 = 0;
		  // check forw
		  for( n=0 ; n<len_forw ; n++) {
		    if(INDEX[Tforw[n][0]][Tforw[n][1]][Tforw[n][2]][1]==1){
		      inroi1 = 1;
		      break;
		    }
		    else
		      continue;
		  }
		  if( inroi1==0 ) { // after 1st half, check 2nd half
		    for( m=0 ; m<len_back ; m++) {
		      if(INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][1]==1){
			inroi1 = 1;
			break;
		      }
		      else
			continue;
		    }
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
		      }
		      else
			continue;
		    }
		    if( inroi2==0 ) { //after 1st half, check 2nd half
		      for( m=0 ; m<len_back ; m++) {
			if(INDEX[Tback[m][0]][Tback[m][1]][Tback[m][2]][2]==1){
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
		    //put this one in backwords, to make it connect
		    m = len_back - 1 - n; 
		    for(aa=0 ; aa<3 ; aa++) {
		      // recenter phys loc for trackvis, if nec...
		      // just works this way (where they define origin)
		      READS_fl = flTback[m][aa];
		      if(TV_switch[aa])
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
		      if(TV_switch[aa])
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

    // !!! have to check on the particulars of this!!!
    outsetMAP = EDIT_empty_copy( mset1 ) ;
    EDIT_dset_items( outsetMAP ,
    		     ADN_datum_all , MRI_float , // !!!! why can't I make this an MRI_int??
    		     ADN_prefix    , prefix_map ,
    		     ADN_none ) ;
    if( THD_is_ondisk(DSET_HEADNAME(outsetMAP)) )
      ERROR_exit("Can't overwrite existing dataset '%s'",
		 DSET_HEADNAME(outsetMAP));
    
    outsetMASK = EDIT_empty_copy( mset1 ) ;
    EDIT_dset_items( outsetMASK ,
    		     ADN_datum_all , MRI_byte , 
    		     ADN_prefix    , prefix_mask ,
    		     ADN_none ) ;
    if( THD_is_ondisk(DSET_HEADNAME(outsetMASK)) )
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
    EDIT_substitute_brick(outsetMAP, 0, MRI_float, temp_arr);
    EDIT_DSET_ORIENT(outsetMAP, // have to make sure this way is fine enough!!!
		     ORCODE(header1.voxel_order[0]),
		     ORCODE(header1.voxel_order[1]),
		     ORCODE(header1.voxel_order[2]) );
    THD_load_statistics(outsetMAP );
    THD_write_3dim_dataset(NULL, NULL, outsetMAP, True);
    // re-orient the data as original inputs
    EDIT_substitute_brick(outsetMASK, 0, MRI_byte, temp_byte);
    EDIT_DSET_ORIENT(outsetMASK,
		     ORCODE(header1.voxel_order[0]),
		     ORCODE(header1.voxel_order[1]),
		     ORCODE(header1.voxel_order[2]) );
    THD_load_statistics(outsetMASK);
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
  //DSET_delete(outsetMAP);  // !!! why not need to free these??
  //DSET_delete(outsetMASK);
  DSET_delete(mset2);
  DSET_delete(mset1);

  
  
  //  free(mask1);
  //free(mask2);
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

  //  INFO_message("Done done.");

  return 0;
}

