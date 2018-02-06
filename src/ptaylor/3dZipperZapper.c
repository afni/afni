/* 
   Description
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
//#include <rsfc.h>    
//#include <gsl/gsl_rng.h>
#include "DoTrackit.h"

 
int check_orient_match( THD_3dim_dataset *A, 
                        char *dset_or );

int check_orient_xyz( THD_3dim_dataset *A);

int find_bad_slices_streak( float **slipar,
                            int *Nmskd,
                            int **slibad,
                            int *Dim,
                            int   MIN_STREAK_LEN,
                            float MIN_STREAK_WARN );

int find_bad_slices_drop( float **slipar,
                          int *Nmskd,
                          int **slibad,
                          int *Dim,
                          float MIN_DROP_DIFF,
                          float MIN_DROP_FRAC );

int find_bad_slices_corr( float **slicorr,
                          int *Nmskd,
                          int **slibad,
                          int *Dim,
                          int MIN_CORR_LEN,
                          float MIN_CORR_CORR );

int make_goodstring_from_badlist( char *goodstring,
                                  int *volbad,
                                  int Nvolgood,
                                  int *Dim
                                  );

/* maybe come back to later...
int do_calc_entrop( float **diffarr,
                    int *Nmskd2,
                    int *mskd2,
                    float **slient,
                    int *Dim);
*/


void usage_ZipperZapper(int detail) 
{
   printf(
" # ------------------------------------------------------------------------\n"
" \n"
" This is a basic program to help highlight problematic volumes in data\n"
" sets, specifically in EPI/DWI data sets with interleaved acquisition.\n"
" \n"
" Intra-volume subject motion can be quite problematic, potentially\n"
" bad-ifying the data values in the volume so much that it is basically\n"
" useless for analysis.  In FMRI analysis, outlier counts might be\n"
" useful to find ensuing badness (e.g., via 3dToutcount). However, with\n"
" DWI data, we might want to find it without aligning the volumes\n"
" (esp. due to the necessarily differing contrasts) and without tensor\n"
" fitting.\n"
" \n"
" *Therefore*, this program will look through axial slices of a data set\n"
" for brightness fluctuations and/or dropout slices.  It will build a\n"
" list of volumes indices that it identifies as bad, and the user can\n"
" then use something like the 'fat_proc_filter_dwis' program after to\n"
" apply the filtration to the volumetric dset *as well as* to any\n"
" accompanying b-value, gradient vector, b-matrix, etc., text files.\n"
" \n"
" The program works by looking for alternating brightness patterns in\n"
" the data (again, specifically in axial slices, so if your data was\n"
" acquired differently, this program ain't for you! (weeellll, some\n"
" tricks with changing header info miiiight be able to work then)).  It\n"
" should be run *before* any processing, particularly alignments or\n"
" unwarping things, because those could change the slice locations.\n"
" Additionally, it has mainly been tested on 3T data of humans; it is\n"
" possible that it will work equally well on 7T or non-humans, but be\n"
" sure to check results carefully in the latter cases (well, *always*\n"
" check your data carefully!).\n"
" \n"
" Note that there is also 'fat_proc_select_vols' program for\n"
" interactively selecting out bad volumes, by looking at a sheet of\n"
" sagittal images from the DWI set.  That might be useful for amending\n"
" or altering the output from this program, if necessary.\n"
" \n"
" written by PA Taylor (started Jan, 2018)\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" USAGE:\n"
" \n"
"     Input: + a 3D+time data set of DWI or EPI volumes,\n"
"            + a mask of the brain-ish region.\n"
"    \n"
"    Output: + a mask of potentially bad slices across the input dset,\n"
"            + a 1D (text) file containing a list of the bad volumes,\n"
"            + a 1D file of the per-volume parameters used to detect\n"
"              badness,\n"
"            + a 1D file of the slices within which calculations were made,\n"
"            + a text file with the selector string of *good* volumes\n"
"              in the dset (for easy use with fat_proc_filter_dwis, \n"
"              for example).\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" COMMAND: \n"
"  \n"
"  3dZipperZapper                                            \\\n"
"      -input FFF  {-mask MMM}                               \\\n"
"      -prefix PPP                                           \\\n"
"      {-min_slice_nvox N}                                   \\\n"
"      {-min_streak_len L}                                   \\\n"
"      {-do_out_slice_param}                                 \\\n"
"      {-no_out_bad_mask}                                    \\\n"
"      {-no_out_text_vals}                                   \\\n"
" \n"
"    where:\n"
" \n"
"    -input FFF   :input the 3D+time file of DWIs or EPIs.\n"
"    -mask MMM    :optional input of a single volume mask file, which \n"
"                  gets applied to the each volume in FFF.  Otherwise,\n"
"                  the dataset is assumed to be masked already.\n"
" \n"
"    -prefix PPP  :prefix for output file name.  Any volumetric file\n"
"                  extension included here (e.g., '.nii.gz') is\n"
"                  propagated to any output volumetric dsets.\n"
" \n"
"    -min_slice_nvox N\n"
"                 :set the minimum number of voxels to be in the mask\n"
"                  for a given slice to be included in the calcs. \n"
"                  N must be >0 (and likely much more so, to be useful).\n"
"                  Default: use 10 percent of the axial slice's size.\n"
"    -min_streak_len L\n"
"                 :set the minimum number of slices in a row to look for\n"
"                  fluctuations within (def: L=4).  That is, if 'large\n"
"                  enough' fluctuations are found in L consecutive slices,\n"
"                  then the volume is flagged for motion.  A larger L means\n"
"                  that more slices need to vary for a volume to be flagged\n"
"                  for 'brightness fluctuations'.  NB: this does parameter\n"
"                  setting does not affect the search for dropout slices.\n"
" \n"
"    -do_out_slice_param\n"
"                 :output the map of slice parameters (not done by\n"
"                  default).  Might be of interest for investigating\n"
"                  data.  Output file name base will be: PPP_param.\n"
"    -no_out_bad_mask\n"
"                 :do *not* output the mask of 'bad' slices that shows\n"
"                  which volumes are considered bad (is output by\n"
"                  default). Output file name base will be: PPP_badmask.\n"
"    -no_out_text_vals\n"
"                 :do *not* output the 1D files of the slice parameter\n"
"                  values (are output by default). The list of slices\n"
"                  in the mask (file name: PPP_sli.1D) and the list of\n"
"                  values per slice per volume (file name: PPP_param.1D)\n"
"                  are output.\n"
" \n"
" * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" EXAMPLE:\n"
" \n"
"     1) All types of outputs:\n"
"     3dZipperZapper                                    \\\n"
"         -input AP.nii.gz                              \\\n"
"         -mask  AP_mask.nii.gz                         \\\n"
"         -prefix ZZZ.nii.gz                            \\\n"
"         -do_out_slice_param\n"
" \n"
"     2) No volumetric outputs (only if speed/write time is super\n"
"        important?):\n"
"     3dZipperZapper                                    \\\n"
"         -input AP.nii.gz                              \\\n"
"         -mask  AP_mask.nii.gz                         \\\n"
"         -prefix ZZZ.nii.gz                            \\\n"
"         -no_out_bad_mask\n"
" \n"
" \n"
" \n"
" # ------------------------------------------------------------------\n"
" \n"
" # ------------------------------------------------------------------------\n"
);
	return;
}

int main(int argc, char *argv[]) {
   int i,j,k,m,n,mm;
   int idx=0, imsk=0;
   int iarg;

   THD_3dim_dataset *insetA = NULL;
   THD_3dim_dataset *MASK=NULL;
   char *iprefix="PREFIX" ;
   char prefix[THD_MAX_PREFIX];
   char tprefixx[THD_MAX_PREFIX];
   char tprefixy[THD_MAX_PREFIX];
   char tprefixc[THD_MAX_PREFIX];
   char bprefix[THD_MAX_PREFIX];
   char lprefix[THD_MAX_PREFIX];
   char sprefix[THD_MAX_PREFIX];
   char gprefix[THD_MAX_PREFIX];
   char *ext=NULL, nullch; 
   char goodstring[3000];

   // char in_name[300];

   FILE *fout0, *fout1, *fout2;

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;
   byte ***mskd=NULL; // define mask of where time series are nonzero
   int *mskd2=NULL;   // another format of mask: hold sli num, and dilate

   int TEST_OK = 0;
   double checksum = 0.;

   int *Nmskd=NULL;                // num of vox in slice inp mask
   int *Nmskd2=NULL;               // num of vox in slice dil mask
   int MIN_NMSKD = -1;             // calc how many vox/sli
                                   // are needed for calc
   int mink = -1, maxk = -1;
   int upk = -1, delsli = -1;      // some loop pars
   
   // for vox counting and fracs
   float **slipar=NULL;            // per sli, per vol par
   float **slicorr=NULL;           // per sli, per vol par
   int **slinvox=NULL;             // per sli, per vol count
   // for entropy/zipping
   //float **slient=NULL;          // per sli, per vol par

   int **slibad=NULL;              // per sli, badness marker
   int *volbad=NULL;               // per vol, badness marker
   int Nvolbad=0, Nvolgood=-1;

   int   MIN_STREAK_LEN  = 4;      // alternating streak
   float MIN_STREAK_WARN = 0.3;    // seq of diffs of this mag -> BAD
   float MIN_DROP_DIFF   = 0.7;    // any diff of this mag -> BAD 
   float MIN_DROP_FRAC   = 0.05;   // any frac outside this edge -> BAD
   int   MIN_CORR_LEN    = 4;      //
   float MIN_CORR_CORR   = 0.3;

   double *xx=NULL, *yy=NULL;      // tmp arrs for corr calc

   THD_3dim_dataset *baddset=NULL;          // output dset of diffs
   float **diffarr=NULL;
   THD_3dim_dataset *diffdset=NULL;         // output dset of diffs

	char dset_or[4] = "RAI";
	THD_3dim_dataset *dsetn=NULL;

   int DO_OUT_SLIPAR=0;                   // output slice param map
   int DO_OUT_BADMASK=1;                  // output slice mask 
   int DO_OUT_TEXTVALS=1;                 // output text files 

   char A_ori[4], A_ori2[4];

   mainENTRY("3dZipperZapper"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   INFO_message("version: 2018_02_06");
	
   /** scan args **/
   if (argc == 1) { usage_ZipperZapper(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_ZipperZapper(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      /*
      // NO ARG:
      if( strcmp(argv[iarg],"-TESTING") == 0) {
         TEST_OK=1;
         iarg++ ; continue ;
      }
      */

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-prefix'");
         // use 'iprefix' for output file, in case user specified
         // NIFTI, 3D, Niml, Analyze, ...
         iprefix = strdup(argv[iarg]) ;
         sprintf(prefix,"%s",iprefix);
         if(has_known_non_afni_extension(iprefix)){   
            ext = find_filename_extension(iprefix);
            // use 'prefix' as basic text file prefix, by removing
            // ext
            prefix[strlen(iprefix) - strlen(ext)] = '\0';  
         }
         else {
            nullch = '\0';
            ext = &nullch;
         }

         //INFO_message("%s %s", prefix, iprefix);

         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-prefix'");
         iarg++ ; continue ;
      }
	 
      if( strcmp(argv[iarg],"-input") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-input'");

         insetA = THD_open_dataset(argv[iarg]);
         if( (insetA == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

         DSET_load(insetA); CHECK_LOAD_ERROR(insetA);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");

         MASK = THD_open_dataset(argv[iarg]);
         if( MASK == NULL )
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

         DSET_load(MASK); CHECK_LOAD_ERROR(MASK);
			
         iarg++ ; continue ;
      }

      // Nvox per slice to include in mask; default: 10 of axi sli
      if( strcmp(argv[iarg],"-min_slice_nvox") == 0 ){
        iarg++ ; if( iarg >= argc ) 
        ERROR_exit("Need argument after '-min_slice_nvox'");
      
        MIN_NMSKD = atoi(argv[iarg]);

        if( MIN_NMSKD < 1 )
           ERROR_exit("Need a positive integer after '-min_slice_nvox'");

        iarg++ ; continue ;
        }

      // Nvox per slice to include in mask; default: 10 of axi sli
      if( strcmp(argv[iarg],"-min_streak_len") == 0 ){
        iarg++ ; if( iarg >= argc ) 
        ERROR_exit("Need argument after '-min_streak_len'");
      
        MIN_STREAK_LEN = atoi(argv[iarg]);
        if( MIN_STREAK_LEN < 1)
           ERROR_exit("Need a positive integer after '-min_streak_len'");

        iarg++ ; continue ;
        }

      if( strcmp(argv[iarg],"-min_streak_val") == 0 ){
        iarg++ ; if( iarg >= argc ) 
        ERROR_exit("Need argument after '-min_streak_val'");
      
        MIN_STREAK_WARN = atof(argv[iarg]);
        if( MIN_STREAK_WARN < 0)
           ERROR_exit("Need a positive float after '-min_streak_val'");

        iarg++ ; continue ;
        }


      // ---------------- control output ---------------------

      if( strcmp(argv[iarg],"-do_out_slice_param") == 0) {
         DO_OUT_SLIPAR=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-no_out_bad_mask") == 0) {
         DO_OUT_BADMASK=0;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-no_out_text_vals") == 0) {
         DO_OUT_TEXTVALS=0;
         iarg++ ; continue ;
      }

      // ----------------- finish up -----------------

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }
	
   // ===============================================================

   // TEST BASIC INPUT PROPERTIES
   if (iarg < 3) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }
	
   // ===================== resamp, if nec =======================

   // What is initial orientation of dset?  save for outputs, too.
   THD_fill_orient_str_3(insetA->daxes, A_ori);
   //INFO_message("%s", A_ori );

   // Make sure insetA is xyz-like; resample if not.
   if (check_orient_xyz( insetA )) {
      dsetn = r_new_resam_dset( insetA, NULL, 0.0, 0.0, 0.0,
                                dset_or, RESAM_NN_TYPE, NULL, 1, 0);
      DSET_delete(insetA); 
      insetA=dsetn;
      dsetn=NULL;
   }

   // In any case, make sure mask orient *matches* insetA; insetA's
   // orientation might have changed.
   THD_fill_orient_str_3(insetA->daxes, A_ori2);
   if( MASK )
      if (check_orient_match( MASK, A_ori2 )) {
         dsetn = r_new_resam_dset( MASK, NULL, 0.0, 0.0, 0.0,
                                   A_ori2, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(MASK); 
         MASK=dsetn;
         dsetn=NULL;
      }

   // Now, collect this info!
   Dim = (int *)calloc(4, sizeof(int));
   if( (Dim == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (small array!).\n\n");
      exit(12);
   }

   Nvox = DSET_NVOX(insetA) ;
   Dim[0] = DSET_NX(insetA); Dim[1] = DSET_NY(insetA); 
   Dim[2] = DSET_NZ(insetA); Dim[3] = DSET_NVALS(insetA); 
   
   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   mskd = (byte ***) calloc( Dim[0], sizeof(byte **) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      mskd[i] = (byte **) calloc( Dim[1], sizeof(byte *) );
   for ( i = 0 ; i < Dim[0] ; i++ ) 
      for ( j = 0 ; j < Dim[1] ; j++ ) 
         mskd[i][j] = (byte *) calloc( Dim[2], sizeof(byte) );

   // Store number of voxels in ipnut axial slice mask: 
   Nmskd = (int *)calloc(Dim[2], sizeof(int)); 
   
   mskd2  = (int *)calloc(Nvox, sizeof(int)); // dilated form
   Nmskd2 = (int *)calloc(Dim[2], sizeof(int)); 

   if( (mskd == NULL) || (Nmskd == NULL) || 
       (mskd2 == NULL) || (Nmskd2 == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (masks).\n\n");
      exit(12);
   }

   // if user doesn't enter one, use default 10%
   if ( MIN_NMSKD < 0 ){ // min num of vox in mask per sli
      MIN_NMSKD = (int) (0.1 * Dim[0]*Dim[1]); 
      if( MIN_NMSKD <= 0 )
         ERROR_exit("Min num of vox per slice was somehow %d?? No thanks!", 
                    MIN_NMSKD);
   }

   // array for diffs; later, maybe just do one by one!
   diffarr = calloc( Dim[3], sizeof(diffarr) );
   for(i=0 ; i<Dim[3] ; i++) 
      diffarr[i] = calloc( Nvox, sizeof(float) ); 

   // summary pars and count for slices
   slipar = calloc( Dim[3], sizeof(slipar) );
   for(i=0 ; i<Dim[3] ; i++) 
      slipar[i] = calloc( Dim[2], sizeof(float) );
   slinvox = calloc( Dim[3], sizeof(slinvox) );
   for(i=0 ; i<Dim[3] ; i++) 
      slinvox[i] = calloc( Dim[2], sizeof(int) ); 

   // keep track of bad slices
   slibad = calloc( Dim[3], sizeof(slibad) );
   for(i=0 ; i<Dim[3] ; i++) 
      slibad[i] = calloc( Dim[2], sizeof(int) );
   // list of bad vols
   volbad = (int *)calloc( Dim[3], sizeof(int) ); 

   if( (diffarr == NULL) || (slipar == NULL) || (slinvox == NULL) ||
       (slibad == NULL) || (volbad == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (arrs).\n\n");
      exit(13);
   }
   
   // *************************************************************
   // *************************************************************
   //                    Beginning of main loops
   // *************************************************************
   // *************************************************************
	
   INFO_message("Masking and counting.");

   // go through once: define data vox
   idx = 0;
   for( k=0 ; k<Dim[2] ; k++ )  
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( MASK ) {
               if( THD_get_voxel(MASK,idx,0)>0 ) {
                  mskd[i][j][k] = 1;
                  //mskd2[idx] = k+1;  // !! just do later now in values used!
                  Nmskd[k]++;
               }
            }
            else{
               checksum = 0.;
               for( m=0 ; m<Dim[3] ; m++ ) 
                  checksum+= fabs(THD_get_voxel(insetA,idx,m)); 
               if( checksum > EPS_V ) {
                  mskd[i][j][k] = 1;
                  //mskd2[idx] = k+1;  // !! just do later now in values used!
                  Nmskd[k]++;
               }
            }
            idx++; 
         }
   
   // will only look at slices with "enough" vox.  Also, ignore top
   // level, because we can't take diff above it.
   for( k=0 ; k<Dim[2] ; k++ ) 
      if( Nmskd[k] < MIN_NMSKD )
         Nmskd[k] = 0;
   Nmskd[Dim[2]-1]=0; 

   // **************************************************************
   // **************************************************************
   //                 Calculate stuff
   // **************************************************************
   // **************************************************************

   INFO_message("Start processing.");
   
   // ---------------- Calc (scaled) diff values ------------------

   // Single pass through.  
   idx = 0;
   delsli = Dim[0]*Dim[1];
   maxk = Dim[2] - 1;
   for( k=0 ; k<maxk ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            if( mskd[i][j][k] && Nmskd[k] ) {
               if( mskd[i][j][k+1] ) {
                  upk = idx + delsli; // index one slice up
                  mskd2[idx] = k+1;   // record sli as k+1 bc of zeros.
                  Nmskd2[k]+= 1;        // count nvox per sli in dil mask
                  for( m=0 ; m<Dim[3] ; m++ ) {
                     diffarr[m][idx] = 0.5*(THD_get_voxel(insetA, idx, m) -
                                            THD_get_voxel(insetA, upk, m) );
                     diffarr[m][idx]/= fabs(THD_get_voxel(insetA, idx, m)) +
                        fabs(THD_get_voxel(insetA, upk, m)) +
                        0.0000001; // guard against double-zero val badness
                  }
               }
            }
            idx++;
         }
   
   // ---------------- Calc corr of diff values ------------------

   xx = (double *)calloc( delsli, sizeof(double) ); 
   yy = (double *)calloc( delsli, sizeof(double) ); 
   slicorr = calloc( Dim[3], sizeof(slicorr) );
   for(i=0 ; i<Dim[3] ; i++) 
      slicorr[i] = calloc( Dim[2], sizeof(float) );

   if( (xx == NULL) || (yy == NULL) || (slicorr == NULL) ) { 
      fprintf(stderr, "\n\n MemAlloc failure (arrs).\n\n");
      exit(13);
   }

   // Single pass through.  
   for( m=0 ; m<Dim[3] ; m++ ) {
      idx = 0;
      for( k=0 ; k<maxk ; k++ ) {
         imsk = 0; // count voxels *in* mask in this slice
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               if( mskd2[idx] ) {
                  upk = idx + delsli; // index one slice up                  
                  xx[imsk] = diffarr[m][idx];
                  yy[imsk] = diffarr[m][upk];
                  imsk++;
               }
               idx++;
            }
         slicorr[m][k] = CORR_FUN(xx,yy,imsk);
      }
   }

   // ---------------- counts of slice vals ------------------

   // Count 'em
   for( idx=0 ; idx<Nvox ; idx++)
      if( mskd2[idx] ) {
         for( m=0 ; m<Dim[3] ; m++ ) {
            slinvox[m][ mskd2[idx]-1 ] +=1;  // count this
            if( diffarr[m][idx] > 0 )
               slipar[m][ mskd2[idx]-1 ] +=1;
         }
      }

   // Calc fraction per slice of uppers
   for( k=0 ; k<maxk ; k++)
      for( m=0 ; m<Dim[3] ; m++ ) 
         if( slinvox[m][k] ) {
            slipar[m][k]/= slinvox[m][k];
            slipar[m][k]-= 0.5; // center around 0
         }

   // ---------------- linear entropy of slice vals -----------------

   /*
   slient = calloc( Dim[3], sizeof(slient) );
   for(i=0 ; i<Dim[3] ; i++) 
      slient[i] = calloc( Dim[2], sizeof(float) );
   if( (slient == NULL)  ) { 
      fprintf(stderr, "\n\n MemAlloc failure (slient).\n\n");
      exit(13);
   }
   
   i = do_calc_entrop( diffarr,
                       Nmskd2,
                       mskd2,
                       slient,
                       Dim);
   */

   // --------------- identify bad slices from counts ---------------


   i = find_bad_slices_streak( slipar,
                               Nmskd,
                               slibad,
                               Dim,
                               MIN_STREAK_LEN,
                               MIN_STREAK_WARN );
   
   i = find_bad_slices_drop( slipar,
                             Nmskd,
                             slibad,
                             Dim,
                             MIN_DROP_DIFF,
                             MIN_DROP_FRAC );

   i = find_bad_slices_corr( slicorr,
                             Nmskd,
                             slibad,
                             Dim,
                             MIN_CORR_LEN,
                             MIN_CORR_CORR );

   // keep track of badness
   for( m=0 ; m<Dim[3] ; m++ ) {
      for( k=0 ; k<maxk ; k++) 
         if( slibad[m][k] ) 
            volbad[m]+= slibad[m][k];
      if( volbad[m] )
         Nvolbad++;
   }
   Nvolgood = Dim[3]-Nvolbad;
   INFO_message("Nvolgood:   %5d", Nvolgood);
   INFO_message("Nvolbad :   %5d", Nvolbad);

   // **************************************************************
   // **************************************************************
   //                 Write stuff
   // **************************************************************
   // **************************************************************

   INFO_message("Calcs done: to the writing cave!");


   if ( DO_OUT_TEXTVALS ) {

      sprintf(tprefixx,"%s_sli.1D", prefix);
      sprintf(tprefixy,"%s_param.1D", prefix);
      sprintf(tprefixc,"%s_parcorr.1D", prefix);

      if( (fout0 = fopen(tprefixx, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.", tprefixx);
         exit(19);
      }
      if( (fout1 = fopen(tprefixy, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.", tprefixy);
         exit(19);
      }
      if( (fout2 = fopen(tprefixc, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.", tprefixc);
         exit(19);
      }

      for( k=0 ; k<maxk ; k++) {
         if (Nmskd[k]) {
            fprintf(fout0, " %5d\n", k);
            for( m=0 ; m<Dim[3] ; m++) {
               fprintf(fout1, " %8.5f ", fabs(slipar[m][k]-slipar[m][k+1]));
               fprintf(fout2, " %8.5f ",  -slicorr[m][k]); // !!!!!!!!!!!!!!!!
               //fprintf(fout1, " %8.5f ", slipar[m][k]); // ORIG!
            }
            fprintf(fout1, "\n");
            fprintf(fout2, "\n");
         }
      }
      fclose(fout0);
      fclose(fout1);
      fclose(fout2);
      INFO_message("--> Wrote text file:    %s", tprefixx);
      INFO_message("--> Wrote text file:    %s", tprefixy);
      INFO_message("--> Wrote text file:    %s", tprefixc);
   }

   if ( DO_OUT_SLIPAR ) {
      
      diffdset = EDIT_empty_copy( insetA );
      sprintf(sprefix,"%s_param%s", prefix,ext);

      EDIT_dset_items( diffdset,
                       ADN_prefix    , sprefix,
                       ADN_datum_all , MRI_float,
                       ADN_brick_fac , NULL,
                       ADN_nvals     , Dim[3],
                       ADN_none );

      for( m=0 ; m<Dim[3] ; m++) {
         EDIT_substitute_brick(diffdset, m, MRI_float, diffarr[m]);
         diffarr[m]=NULL; // to not get into trouble...
         free(diffarr[m]);
      }

      THD_load_statistics( diffdset );
      tross_Copy_History( insetA, diffdset );
      tross_Make_History( "3dZipperZapper", argc, argv, diffdset );

      // make sure the output has same orientation as input
      if (check_orient_match( diffdset, A_ori )) {
         dsetn = r_new_resam_dset( diffdset, NULL, 0.0, 0.0, 0.0,
                                   A_ori, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(diffdset); 
         diffdset=dsetn;
         dsetn=NULL;
      }
      EDIT_dset_items( diffdset ,
                       ADN_prefix      , sprefix,
                       ADN_none ) ;

      if( !THD_ok_overwrite() && 
          THD_is_ondisk(DSET_HEADNAME(diffdset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(diffdset));
      THD_write_3dim_dataset(NULL, NULL, diffdset, True);
      INFO_message("--> Wrote dataset:    %s\n", DSET_BRIKNAME(diffdset));
   }


   // !!! maybe just temp: output map of badness, *if* there are bad
   // !!! vols
   if ( DO_OUT_BADMASK && Nvolbad ) {
      
      byte **badarr=NULL;

      // array for diffs; later, maybe just do one by one!
      badarr = calloc( Dim[3], sizeof(badarr) );
      for(i=0 ; i<Dim[3] ; i++) 
         badarr[i] = calloc( Nvox, sizeof(byte) ); 
      
      if( (badarr == NULL) ) { 
         fprintf(stderr, "\n\n MemAlloc failure (bad-list array).\n\n");
         exit(13);
      }

      // write out array of "bad sli" map
      idx = 0;
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               if( mskd[i][j][k] && Nmskd[k] ) 
                  for( m=0 ; m<Dim[3] ; m++ ) 
                     if( slibad[m][k] ) 
                        badarr[m][idx] = 1; //slibad[m][k];
               idx++;
            }
            
      sprintf(bprefix,"%s_badmask%s", prefix, ext);
      baddset = EDIT_empty_copy( insetA );

      EDIT_dset_items( baddset,
                       ADN_prefix    , bprefix,
                       ADN_datum_all , MRI_byte,
                       ADN_brick_fac , NULL,
                       ADN_nvals     , Dim[3],
                       ADN_none );

      for( m=0 ; m<Dim[3] ; m++) {
         EDIT_substitute_brick(baddset, m, MRI_byte, badarr[m]);
         badarr[m]=NULL; // to not get into trouble...
         free(badarr[m]);
      }

      THD_load_statistics( baddset );
      tross_Copy_History( insetA, baddset );
      tross_Make_History( "3dZipperZapper", argc, argv, baddset );

      // make sure the output has same orientation as input
      if (check_orient_match( baddset, A_ori )) {
         INFO_message("Resample baddset to %s", A_ori);
         dsetn = r_new_resam_dset( baddset, NULL, 0.0, 0.0, 0.0,
                                   A_ori, RESAM_NN_TYPE, NULL, 1, 0);
         DSET_delete(baddset); 
         baddset=dsetn;
         dsetn=NULL;
      }
      EDIT_dset_items( baddset ,
                       ADN_prefix      , bprefix,
                       ADN_none ) ;

      if( !THD_ok_overwrite() && 
          THD_is_ondisk(DSET_HEADNAME(baddset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(baddset));
      THD_write_3dim_dataset(NULL, NULL, baddset, True);
      INFO_message("--> Wrote dataset:    %s\n", DSET_BRIKNAME(baddset));
      
      if(badarr) {
         for( i=0 ; i<Dim[3] ; i++) 
            free(badarr[i]);
         free(badarr);
      }
   }      


   if( Nvolgood > 0 ) {
      i = make_goodstring_from_badlist( goodstring,
                                        volbad,
                                        Nvolgood,
                                        Dim
                                        );
      
      // ------- write out good string, single row file --------------
      
      sprintf(gprefix,"%s_goodstri.txt", prefix);
      if( (fout0 = fopen(gprefix, "w")) == NULL) {
         fprintf(stderr, "Error opening file %s.", gprefix);
         exit(19);
      }

      fprintf(fout0, "%s", goodstring);
      fclose(fout0);

      fprintf(stderr, "++ String selector for %d good vols:\n",
              Nvolgood);
      fprintf(stderr, "   %s\n", goodstring);
      INFO_message("Wrote text file of good vol string selector:    %s", 
                   gprefix);
   }
   else if( !Nvolgood )
      WARNING_message("Hey, *no* UNcorrupted vols were found??");
   else
      ERROR_message("Hey, a *negative* number of good vols were found???");

   // ------- write out list of bads, single col file --------------

   sprintf(lprefix,"%s_badlist.txt", prefix);
   if( (fout0 = fopen(lprefix, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s.", lprefix);
      exit(19);
   }


   fprintf(stderr, "++ The following %d vols were identified as bad:\n",
           Nvolbad);
   fprintf(stderr, "%15s %15s\n", "Bad vols", "N bad slices");

   for( m=0 ; m<Dim[3] ; m++) {
      if( volbad[m] ) {
         fprintf(fout0, "%8d\n", m);
         fprintf(stderr, "%15d %15d\n", m, volbad[m]);
      }
   }
   fclose(fout0);

   if( !Nvolbad )
      fprintf(stderr, "%15s %15s\n", "(None!)", "(zilch!)");

   INFO_message("Wrote text file listing bads:    %s", lprefix);


   // ************************************************************
   // ************************************************************
   //                    Free dsets, arrays, etc.
   // ************************************************************
   // ************************************************************
	
   INFO_message("Done with calcs, just cleaning up.");

   if(insetA){
      DSET_delete(insetA);
      free(insetA);
   }

   if( MASK ) {
      DSET_delete(MASK);
      free(MASK);
   }
      
   if(mskd) {
      for( i=0 ; i<Dim[0] ; i++) 
         for( j=0 ; j<Dim[1] ; j++) {
            free(mskd[i][j]);
         }
      for( i=0 ; i<Dim[0] ; i++) {
         free(mskd[i]);
      }
      free(mskd);
   }
   if(mskd2)
      free(mskd2);

   if(diffarr) {
      for( i=0 ; i<Dim[3] ; i++) 
         free(diffarr[i]);
      free(diffarr);
   }

   free(Nmskd);
   free(Nmskd2);

   for( i=0 ; i<Dim[3] ; i++) {
      free(slipar[i]);
      free(slinvox[i]);
      free(slibad[i]);
      free(slicorr[i]);
      // free(slient[i]);
   }
   free(slipar);
   free(slinvox);
   free(slibad);
   free(slicorr);
   // free(slient);

   free(xx);
   free(yy);

   free(volbad);
      
   if(iprefix)
      free(iprefix);

   if(Dim)
      free(Dim);
	
   return 0;
}

// ---------------------------------------------------------

int check_orient_match( THD_3dim_dataset *A, 
                        char *dset_or )
{
   int i;
   char ostr[4];
   THD_fill_orient_str_3(A->daxes, ostr);
   //INFO_message("strcmp: %s %s --> %d", 
   //             dset_or, ostr, strcmp(dset_or, ostr));
   return strcmp(dset_or, ostr);
}

// ---------------------------------------------------------

int check_orient_xyz( THD_3dim_dataset *A )
{
   int i;
   char ro[2][4] = { "RAI", "LPS" }; // just need to 'xyz'-like coor
                                     // order
   char ostr[4];

   THD_fill_orient_str_3(A->daxes, ostr);

   for( i=0 ; i<3 ; i++ ) {
      if( ostr[i] != ro[0][i] && ostr[i] != ro[1][i] ){
         INFO_message("[%d] Do internal of resample %s.", i, ostr);
         return 1;
      }
   }
         
   INFO_message("No need to resample %s.", ostr);

   return 0;
}









// ---------------------------------------------------------
int find_bad_slices_corr( float **slicorr,
                          int   *Nmskd,
                          int   **slibad,
                          int   *Dim,
                          int   MIN_CORR_LEN,
                          float MIN_CORR_CORR )
{
   int k,m,i;
   int topk = Dim[2] - MIN_CORR_LEN; // max sli to check iteratively
   int THIS_SLI = 1;                 // switch to check or not
   int IS_BAD = 0;

   float xx[Dim[0]*Dim[1]];  // arrs to populate for corr
   float yy[Dim[0]*Dim[1]];


   for( m=0 ; m<Dim[3] ; m++ ) 
      for( k=0 ; k<topk ; k++ ) {

         // long enough streak to check?
         THIS_SLI = 1;
         for( i=0 ; i<MIN_CORR_LEN ; i++ )
            if ( Nmskd[k+i] == 0 ) {
               THIS_SLI = 0;
               break;
            }
         if( THIS_SLI ) {
            IS_BAD = 1;
            for( i=0 ; i<MIN_CORR_LEN ; i++ )
               // look for signed correlation stuff
               if( -slicorr[m][k+i] < MIN_CORR_CORR )
                  IS_BAD = 0; // -> the streak is broken
            if( IS_BAD ) 
               for( i=0 ; i<MIN_CORR_LEN ; i++ ) {
                  slibad[m][k+i]+=1001;
               }
         }
      }
   
   return 0;
}














// ---------------------------------------------------------
int find_bad_slices_streak( float **slipar,
                            int *Nmskd,
                            int **slibad,
                            int *Dim,
                            int   MIN_STREAK_LEN,
                            float MIN_STREAK_WARN )
{
   int k,m,i;
   int topk = Dim[2] - MIN_STREAK_LEN; // max sli to check iteratively
   int THIS_SLI = 1;                   // switch to check or not
   int IS_BAD = 0;

   for( m=0 ; m<Dim[3] ; m++ ) 
      for( k=0 ; k<topk ; k++ ) {

         // long enough streak to check?
         THIS_SLI = 1;
         for( i=0 ; i<MIN_STREAK_LEN ; i++ )
            if ( Nmskd[k+i] == 0 ) {
               THIS_SLI = 0;
               break;
            }
         if( THIS_SLI ) {
            IS_BAD = 1;
            for( i=0 ; i<MIN_STREAK_LEN ; i++ )
               if( fabs(slipar[m][k+i]-slipar[m][k+i+1]) < MIN_STREAK_WARN )
                  IS_BAD = 0; // -> the streak is broken
            if( IS_BAD ) 
               for( i=0 ; i<MIN_STREAK_LEN ; i++ ) {
                  slibad[m][k+i]+=1;
               }
         }
      }
   
   return 0;
}

// -------------------------------------------------------

int find_bad_slices_drop( float **slipar, // shape: Dim[3] x Dim[2]
                          int *Nmskd,
                          int **slibad,
                          int *Dim,
                          float MIN_DROP_DIFF,
                          float MIN_DROP_FRAC)
{
   int k,m,i;
   float BOUND = 0.5 - MIN_DROP_FRAC;
   int topk = Dim[2] - 1; // max sli to check iteratively

   for( m=0 ; m<Dim[3] ; m++ ) 
      for( k=0 ; k<Dim[2] ; k++ ) 
         if( Nmskd[k] ) {
            if( fabs(slipar[m][k]) >= BOUND ) {
               slibad[m][k]+=1;
            }
            else if( k < topk ) {
               if( fabs(slipar[m][k]-slipar[m][k+1]) >= MIN_DROP_DIFF )
                  slibad[m][k]+=1;
            }
         }
  
   return 0;
}

// ------------------------------------------------------------------

int make_goodstring_from_badlist( char *goodstring,
                                  int *volbad,
                                  int Nvolgood,
                                  int *Dim
                                  )
{
   int i,jj;
   NI_int_array iar;
   char *code=NULL;

   iar.num = Nvolgood; 
   iar.ar = (int *)calloc(Nvolgood, sizeof(int));
   i = 0;
   for( jj=0 ; jj<Dim[3] ; jj++ ) 
      if( !volbad[jj] ) {
         iar.ar[i] = jj;
         i++;
      }
   code = NI_encode_int_list( &iar , NULL ) ;
   strcpy(goodstring, code);

   //NI_delete_int_array(iar);
   return 0;
}


// ---------------------------------------------------------
/*
int do_calc_entrop( float **diffarr,
                    int *Nmskd2,
                    int *mskd2,
                    float **slient,
                    int *Dim)
{
   int i,j,k,m,n;
   int idx;
   char *slisrc=NULL;
   float scaler = 0.;

   // Single pass through.  
   for( m=0 ; m<Dim[3] ; m++ ) {
      idx = 0;
      for( k=0 ; k<Dim[2] ; k++ ) {
         if( Nmskd2[k] ) {
            if( slisrc )
               realloc(slisrc, Nmskd2[k] * sizeof(byte) ); 
            else
               slisrc = (byte *)calloc( Nmskd2[k], sizeof(byte) ); 
            if( (slisrc == NULL)  ) { 
               fprintf(stderr, "\n\n MemAlloc failure (slisrc).\n\n");
               exit(13);
            }
            
            // set scale
            for( n=0 ; n<Nmskd2[k] ; n++ )
               if( n % 2 ) 
                  slisrc[n] = 1;
               else
                  slisrc[n] = 0;
            scaler = zz_compress_all( Nmskd2[k], slisrc, NULL );
            n = 0;
         }

         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               if( Nmskd2[k] && mskd2[idx] ) {
                  if( diffarr[m][idx] > 0 )
                     slisrc[n] = 1;
                  else
                     slisrc[n] = 0;
                  n++;
               }
               idx++;
            }
         
         if( slisrc ) {
            slient[m][k] = zz_compress_all( Nmskd2[k], slisrc, NULL );
            // HOW TO SCALE????????????????
            slient[m][k]/= scaler*scaler; //log10(Nmskd2[k]) * log10(Nmskd2[k]);
            INFO_message("%5d    %f", Nmskd2[k], scaler);
            slisrc=NULL; //free(slisrc);
         }
      }
   }
   free(slisrc);

   return 0;
}
*/
