/* 
   Description

   2018-05-08
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
#include "basic_boring.h"

#define BIIIIG_NUM 1.e9

void MCW_fc7( float qval , char *buf );

void usage_Clusterize(int detail) 
{
   printf(
" # ------------------------------------------------------------------------\n"
" \n"
" \n"
" This program is for performing clusterizing: one can perform voxelwise\n"
" thresholding on a dataset (such as a statistic), and then make a map\n"
" of remaining clusters of voxels larger than a certain volume.\n"
" \n"
" This program is specifically meant to reproduce behavior of the muuuch\n"
" older 3dclust, as well as to include additional clustering behavior\n"
" such as the '-bisided ...' variety (essentially, two-sided testing\n"
" where a cluster cannot be comprised of voxels passing both the left-\n"
" and right-sided parts of the distribution).\n"
" \n"
" cobbled together by PA Taylor (NIMH, NIH), but importantly from code\n"
" written by many legends: RW Cox, BD Ward, MS Beauchamp, ZS Saad, and\n"
" more.\n"
" \n"
"  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" USAGE ~1~\n"
"  \n"
"      Input: \n"
"        + A dataset of one or more bricks\n"
"        + Specify an index of the volume to threshold\n"
"        + Declare a voxelwise threshold, and optionally a cluster-volume\n"
"          threshold\n"
"        + Optionally specify the index an additional 'data' brick\n"
"        + Optionally specify a mask\n"
" \n"
"      Output:\n"
" \n"
"        + A dataset volume containing a map of cluster ROIs (sorted by\n"
"          size) after thresholding (and clusterizing, if specified)\n"
"        + A report about the clusters (center of mass, extent, volume, etc.)\n"
"          that can be dumped into a text file\n"
"        + Optional: a mask\n"
" \n"
" Explanation of 3dclust text report output\n"
" -----------------------------------------     \n"
" \n"
" Nvoxel       : Number of voxels in the cluster       \n"
"                                                                      \n"
" CM RL        : Center of mass (CM) for the cluster in the Right-Left \n"
"                direction (i.e., the coordinates for the CM)          \n"
"                                                                      \n"
" CM AP        : Center of mass for the cluster in the                 \n"
"                Anterior-Posterior direction                          \n"
"                                                                      \n"
" CM IS        : Center of mass for the cluster in the                 \n"
"                Inferior-Superior direction                           \n"
"                                                                      \n"
" minRL, maxRL : Bounding box for the cluster, min and max             \n"
"                coordinates in the Right-Left direction               \n"
"                                                                      \n"
" minAP, maxAP : Min and max coordinates in the Anterior-Posterior     \n"
"                direction of the volume cluster                       \n"
"                                                                      \n"
" minIS, maxIS : Min and max coordinates in the Inferior-Superior      \n"
"                direction of the volume cluster                       \n"
"                                                                      \n"
" Mean         : Mean value for the volume cluster                     \n"
"                                                                      \n"
" SEM          : Standard Error of the Mean for the volume cluster     \n"
"                                                                      \n"
" Max Int      : Maximum Intensity value for the volume cluster        \n"
"                                                                      \n"
" MI RL        : Coordinate of the Maximum Intensity value in the      \n"
"                Right-Left direction of the volume cluster            \n"
"                                                                      \n"
" MI AP        : Coordinate of the Maximum Intensity value in the      \n"
"                Anterior-Posterior direction of the volume cluster    \n"
"                                                                      \n"
" MI IS        : Coordinate of the Maximum Intensity value in the      \n"
"                Inferior-Superior direction of the volume cluster     \n"
" \n"
"   * CM values use the absolute value of the voxel values as weights.\n"
" \n"
"   * The program does not work on complex- or rgb-valued datasets!      \n"
" \n"
"   * SEM values are not realistic for interpolated data sets!  A ROUGH\n"
"     correction is to multiply the SEM of the interpolated data set by\n"
"     the square root of the number of interpolated voxels per original\n"
"     voxel.\n"
" \n"
" \n"
"  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" COMMAND ~1~\n"
" \n"
" \n"
" -inset  III    :Load in a dataset III of one or more bricks for thresholding\n"
"                 and clusterizing; one can choose to use either just a \n"
"                 single sub-brick within it for all operations (e.g., a\n"
"                 'statistics' brick), or to specify an additional sub-brick\n"
"                 within it for the actual clusterizing+reporting (after the\n"
"                 mask from the thresholding dataset has been applied to it).\n"
" \n"
" -prefix  PPP   :The prefix/filename of the output map of cluster ROIs.\n"
"                 (To save the text file report, use the redirect '>' after\n"
"                 the 3dClusterize command and dump the text into a   \n"
"                 separate file of your own naming.\n"
" \n"
" -mask MMM      :Load in a dataset MMM to use as a mask, within which\n"
"                 to look for clusters.\n"
" -mask_from_hdr :If 3dClustSim put an internal attribute into the       \n"
"                 input dataset that describes a mask, 3dClusterize will      \n"
"                 use this mask to eliminate voxels before clustering,   \n"
"                 if you give this option (this is how the AFNI \n"
"                 Clusterize GUI works by default).\n"
"                 If there is no internal mask in the dataset header,\n"
"                 then '-inmask' doesn't do anything. \n"
" \n"
" -ithr   j      :Uses sub-brick [j] as the threshold source (required).\n"
" -idat   k      :Uses sub-brick [k] as the data source (optional);\n"
"                 if this option is used, thresholding is still done by the\n"
"                 'threshold' dataset, but that threshold map is applied\n"
"                 to this 'data' set, which is in turn used for clusterizing \n"
"                 and the 'data' set values are used to make the report.\n"
"                 If a 'data' dataset is NOT input with '-idat ..', then\n"
"                 thresholding, clustering and reporting are all done using\n"
"                 the 'threshold' dataset.\n"
" \n"
"    One of the following methods of clustering MUST be chosen:\n"
" -1sided SSS TT :Perform one-sided testing. Two arguments are required:\n"
"                   SSS -> either 'RIGHT_TAIL' or 'LEFT_TAIL', to specify\n"
"                          which side of the distribution to test.\n"
"                   TT  -> the threshold value itself.\n"
" -2sided  LL RR :Perform two-sided testing. Two arguments are required:\n"
"                   LL  -> the bound of the left-side tail (i.e., upper\n"
"                          bound of the left tail).\n"
"                   RR  -> the bound of the right-side tail (i.e., lower\n"
"                          bound of the right tail).\n"
"                 *NOTE* that in this case, potentially a cluster could\n"
"                 be made of both left- and right-tail survivors (e.g., \n"
"                 both positive and negative values). For this reason, \n"
"                 probably '-bisided ...' is a preferable choice.\n"
" -bisided LL RR :Same as '-2sided ...', except that the tails are tested\n"
"                 independently, so a cluster cannot be made of both.\n"
" -within_range AA BB\n"
"                :Perform a kind of clustering where a different kind of \n"
"                 thresholding is first performed, compared to the above\n"
"                 cases;  here, one keeps values within the range [AA, BB],\n"
"                 INSTEAD of keeping values on the tails. Is this useful?\n"
"                 Who knows, but it exists.\n"
" \n"
" -NN {1|2|3}    :Necessary option to specify how many neighbors\n"
"                 a voxel has; one MUST put one of 1, 2 or 3 after it:\n"
"                   1 -> 6  facewise neighbors\n"
"                   2 -> 18 face+edgewise neighbors\n"
"                   3 -> 26 face+edge+nodewise neighbors\n"
"                 For example, this should match what was chosen \n"
"                 in 3dClustSim, if that program were run on the data\n"
"                 prior to this one. (In many AFNI programs, NN=1 is\n"
"                 a default choice, but BE SURE YOURSELF!)\n"
" \n"
" -clust_nvox M  :specify the minimum cluster size in terms of number\n"
"                 of voxels M (such as output by 3dClustSim, for example).\n"
" \n"
" -clust_volml V :specify the minimum cluster size in terms of volume V,\n"
"                 in milliliters (requires knowing the voxel size). Probably\n"
"                 '-clust_nvox ...' is more useful.\n"
" \n"
" -1Dformat      :Write output in 1D format (now default). You can       \n"
"                 redirect the output to a .1D file and use the file     \n"
"                 as input to whereami for obtaining Atlas-based         \n"
"                 information on cluster locations.                      \n"
"                 See whereami -help for more info.                      \n"
" -no_1Dformat   :Do not write output in 1D format.                      \n"
" \n"
" -summarize     :Write out only the total nonzero voxel                 \n"
"                 count and volume for each dataset         \n"
" -nosum         :Suppress printout of the totals \n"
" \n"
" -quiet         :Suppress all non-essential output\n"
" \n"
" -orient OOO    :change the coordinate output order to 'OOO' (def:\n"
"                 is DICOM, RAI); alternatively, one could set the\n"
"                 environment variable AFNI_ORIENT (see the file\n"
"                 README.environment).\n"
" \n"
" -noabs         :Use the signed voxel intensities (not the absolute     \n"
"                 value) for calculation of the mean and Standard        \n"
"                 Error of the Mean (SEM)                \n"
" \n"
" -binary        :This turns the output map of cluster ROIs into a binary     \n"
"                 (0 or 1) mask, rather than a cluster-index mask.       \n"
"                 If no clusters are found, the mask is not written!\n"
"                 (def: each cluster has separate values)\n"
" \n"
"  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
" \n"
" EXAMPLES ~1~\n"
" \n"
"   1. Take an output of FMRI testing (e.g., from afni_proc.py), whose\n"
"   [1] brick contains the effect estimate from a statistical model and\n"
"   whose [2] brick contains the associated statistic; use the results\n"
"   of 3dClustSim run with NN=1 (here, a cluster threshold volume of 157\n"
"   voxels) and perform one-sided testing with a threshold at an\n"
"   appropriate value (here, 3.313).\n"
"      \n"
"     3dClusterize                  \\\n"
"        -inset stats.FT+tlrc.      \\\n"
"        -ithr 2                    \\\n"
"        -idat 1                    \\\n"
"        -mask mask_group+tlrc.     \\\n"
"        -NN 1                      \\\n"
"        -1sided RIGHT_TAIL 3.313   \\\n"
"        -clust_nvox 157            \\\n"
"        -prefix ClusterMap\n"
" \n"
"   2. The same as Ex. 1, but using bisided testing (two sided testing\n"
"   where the results of each tail can't be joined into the same\n"
"   cluster). Note, the tail thresholds do NOT have to be symmetric (but\n"
"   often they are).\n"
"      \n"
"     3dClusterize                  \\\n"
"        -inset stats.FT+tlrc.      \\\n"
"        -ithr 2                    \\\n"
"        -idat 1                    \\\n"
"        -mask mask_group+tlrc.     \\\n"
"        -NN 1                      \\\n"
"        -bisided -3.313 3.313      \\\n"
"        -clust_nvox 157            \\\n"
"        -prefix ClusterMa\n"
" \n"
" # ------------------------------------------------------------------------\n"
);
	return;
}

int main(int argc, char *argv[]) {
   int i,j,k,m,n;
   int idx=0;
   int iarg;

   THD_3dim_dataset *insetA = NULL;
   THD_3dim_dataset *INMASK=NULL;
   char *prefix="PREFIX" ;
   // char in_name[300];

   // FILE *fout0, *fout1;

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;

   int NNTYPE = 0;   // user *needs* to choose? or just make 1 by
   // default?
   int thr_type = 0;  // user needs to choose with -1sided, -2sided,
                      // etc.; 1 -> 1sided,right; -1 -> 1sided,left;
                      // 2 -> 2sided; -2 -> bisided; 3 -> within 
                      // interval
   float thr_1sid = -1.1e10; 
   
   int TEST_OK = 0;

   // names modeled on existing progs, like afni_vedit.c
   float vmul;        // cluster vol thr: can be neg, pos, zero
   float rmm=0;       // for now, leave as zero to flag using NN neigh val
   float thb, tht;    // bot thr val for Rtail; top thr val for Ltail
   int   ival=-1, ithr=-1; // idxs for data and thr vols
   int   posfunc = 0;      // use for "pos only" behavior-- not needed here
   int   no_inmask = 1 ;   // about reading mask from header
   byte *mask=NULL;        // can be input or read from header
   int nmask=0; 

   int nx, ny, nz, nxy, nxyz; 
   float dxf, dyf, dzf;
   MCW_cluster_array *clar=NULL, *clar2=NULL, *clbig=NULL;
   char c1d[2] = {""}, c1dn[2] = {""};

   MRI_IMAGE *tim=NULL, *dim=NULL;        // NB: 'dim' is other ex. 'bim'
   THD_datablock *dblk=NULL;
   MRI_IMAGE *cim=NULL; void *car=NULL;   // copies of thr vol to apply thr
   MRI_IMAGE *cim2=NULL; void *car2=NULL; // same as cim, but for bisided

   int qv; 
   short *mmm=NULL;
   int iclu, ipt, ii, jj, kk, ptmin;
   MCW_cluster *cl=NULL;

   // mainly report-related
   int CL_quiet     = 0;
   int CL_summarize = 0;
   int CL_do_mni    = 0; 
   int CL_1Dform    = 1;
   int CL_noabs     = 0;  
   int do_binary    = 0;
   THD_coorder CL_cord;
   float dx, dy, dz, xx, yy, zz, mm, ms, fimfac,
      xxmax, yymax, zzmax, mmmax, msmax,
      RLmax, RLmin, APmax, APmin, ISmax, ISmin;
   double xxsum, yysum, zzsum, mmsum, volsum, mssum;
   double mean, sem, sqsum, glmmsum, glsqsum, glmssum,
      glmean, glxxsum, glyysum, glzzsum;
   char buf1[16], buf2[16], buf3[16];
   THD_fvec3 fv;
   int nvox_total;
   float vol_total;
   int ndet, nopt, do_mni;

   mainENTRY("3dClusterize"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************

   // INFO_message("version: NU");
	
   /** scan args **/
   if (argc == 1) { usage_Clusterize(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_Clusterize(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
      }
		
      // NO ARG:
      if( strcmp(argv[iarg],"-TESTING") == 0) {
         TEST_OK=1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-inset") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-inset'");

         insetA = THD_open_dataset(argv[iarg]);
         if( (insetA == NULL ))
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);
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

      if( strcmp(argv[iarg],"-mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-mask'");

         INMASK = THD_open_dataset(argv[iarg]);
         if( INMASK == NULL )
            ERROR_exit("Can't open time series dataset '%s'.",
                       argv[iarg]);

         DSET_load(INMASK); CHECK_LOAD_ERROR(INMASK);
         iarg++ ; continue ;
      }

      // the same as "-inmask" in 3dclust
      if( strcmp(argv[iarg],"-mask_from_hdr") == 0) {
         no_inmask = 0;
         iarg++ ; continue ;
      }

      // ---------- which vols are data and thr vols

      // !!!!!!! also want to select by bricklabel!!!

      // index for vol to be thresholded
      if( strcmp(argv[iarg],"-ithr") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-ithr'");         
         ithr = atoi(argv[iarg]);
         iarg++ ; continue ;
      }

      // index for vol to be clusterized
      if( strcmp(argv[iarg],"-idat") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-idat'");         
         ival = atoi(argv[iarg]);
         iarg++ ; continue ;
      }

      // ---------- sidedness ----------------------

      // 2 necessary args needed after this opt
      if( strcmp(argv[iarg],"-1sided") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-1sided'");
         // get first arg
         if( strcmp(argv[iarg],"RIGHT_TAIL") == 0 ) 
            thr_type = 1; // def: full FOV
         else if( strcmp(argv[iarg],"LEFT_TAIL") == 0 )
            thr_type = -1; // only where A or B is nonzero
         else 
            // out of tails
            ERROR_exit("Illegal immediately following '-1sided'"
                       ": need 'RIGHT_TAIL' or 'LEFT_TAIL'");
         // get second arg
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-1sided'");
         thr_1sid = atof(argv[iarg]);

         // assign one thr to user's value, and other effectively to
         // -/+ infty
         if( thr_type == 1 ) {
            thb = thr_1sid;
            tht = - BIIIIG_NUM;
         }
         else {
            tht = thr_1sid;
            thb = BIIIIG_NUM;
         }
         iarg++ ; continue ;
      }

      // 2 necessary args needed after this opt
      if( strcmp(argv[iarg],"-2sided") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-2sided'");
         tht = atof(argv[iarg]);
         // get second arg
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-2sided'");
         thb = atof(argv[iarg]);
         thr_type = 2;
         iarg++ ; continue ;
      }
      
      // 2 necessary args needed after this opt
      if( strcmp(argv[iarg],"-bisided") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-bisided'");
         tht = atof(argv[iarg]);
         // get second arg
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-bisided'");
         thb = atof(argv[iarg]);
         thr_type = -2;
         iarg++ ; continue ;
      }

      // 2 necessary args needed after this opt
      if( strcmp(argv[iarg],"-within_range") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-within_range'");
         tht = atof(argv[iarg]);
         // get second arg
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-within_range'");
         thb = atof(argv[iarg]);
         thr_type = 3;
         iarg++ ; continue ;
      }
      
      // ---------- control pars

      if( strcmp(argv[iarg],"-NN") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-NN'");
         NNTYPE = atoi(argv[iarg]);
         switch( NNTYPE ) {
         case 1: rmm = 1.11f; break;
         case 2: rmm = 1.44f; break;
         case 3: rmm = 1.77f; break;
         default:
            ERROR_exit("Need an integer (1, 2 or 3) after '-NN'.");
         }
         INFO_message("Neighborhood definition (NN=%d) accepted",
                      NNTYPE);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-clust_nvox") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-clust_nvox'");
         vmul = atof(argv[iarg]);
         if ( vmul <= 0 )
            ERROR_exit( "Need the volume threshold to be >=0 (not %f)",
                        vmul);
         else if( vmul )
            vmul = -vmul; // we do this because of syntax of older progs!
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-clust_volml") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-clust_volml'");
         vmul = atof(argv[iarg]);
         if ( vmul <= 0 )
            ERROR_exit( "Need the volume threshold to be >=0 (not %f)",
                        vmul);
         else if( vmul )
            vmul = -vmul; // ALSO do this because of syntax of older progs!
         iarg++ ; continue ;
      }

      // ------------- boring opts from original 3dclust

      if( strcmp(argv[iarg],"-1Dformat") == 0 ){
         CL_1Dform = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-no_1Dformat") == 0 ){
         CL_1Dform = 0 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-summarize") == 0 ){
         CL_summarize = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nosum") == 0 ){
         CL_summarize = -1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-quiet") == 0 ){
         CL_quiet = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-orient") == 0 ){
         THD_coorder_fill( argv[++iarg] , &CL_cord );
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-noabs") == 0 ){
         CL_noabs = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-binary") == 0 ){
         do_binary = 1 ;
         iarg++ ; continue ;
      }
      // ------------- fin

      ERROR_message("Bad option '%s'\n",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   // ****************************************************************
   // ****************************************************************
   //                    check inputs, set basic vals
   // ****************************************************************
   // ****************************************************************

   if (iarg < 3)
      ERROR_exit("Too few options. Try -help for details.\n");
	
   if( !TEST_OK )
      ERROR_exit("HEY! Just testing/building mode right now!\n");

   if( !thr_type )
      ERROR_exit("Hey, you need to put in threshold type/value!"
                 "For example, '-2sided ...', '-1sided ...', etc.\n");

   // check input dsets and matching, if necessary
   DSET_load(insetA); CHECK_LOAD_ERROR(insetA);
   Dim = (int *)calloc(4, sizeof(int));
   if( (Dim == NULL) ) 
      ERROR_exit("MemAlloc failure (arrs).\n\n");
   Nvox = Basic_Info_Dim_and_Nvox( insetA, Dim, 4);

   // Compare dsets: if no match, this will exit with error
   if ( INMASK ) 
      i = Basic_Compare_DSET_dims(insetA, INMASK, 3);

   if ( !no_inmask && INMASK )
      ERROR_exit("Can't input a mask AND ask to use any mask from header!");

   if( ival > DSET_NVALS(insetA) )
      ERROR_exit("Bad index (too large) for data volume: %d", ival);
   else if( ival < 0 )
      INFO_message("No extra data block input: "
                   "using threshold dset for all computations and info.");

   if( ithr < 0 || ithr > DSET_NVALS(insetA) )
      ERROR_exit("Bad index for threshold volume: %d", ithr);

   if ( !NNTYPE ) 
      ERROR_exit("Need to choose a neighborhood type: '-NN ?'");

   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // ----------- mask ------------

   if ( INMASK ) { // explicit from user
      INFO_message("Reading in user's mask");
      mask = (byte *)calloc(Nvox, sizeof(byte));
      
      for( i=0 ; i<Nvox ; i++ ) 
         if( THD_get_voxel(INMASK, i, 0) )
            mask[i] = 1;
   }
   else if( !no_inmask ) { // just from 3dclust! the old '-inmask' there
      INFO_message("Getting mask from header");
      ATR_string *atr = 
         THD_find_string_atr(insetA->dblk,"AFNI_CLUSTSIM_MASK");
      if( atr != NULL ){                       // mask stored as B64 string
         nmask = mask_b64string_nvox(atr->ch);            // length of mask
         if( nmask != DSET_NVOX(insetA) )                // must match dataset
            nmask = -1;
         else if( !no_inmask )
            mask = mask_from_b64string(atr->ch,&nmask);
      } else {                       // mask name stored in NN1 NIML header
         atr = THD_find_string_atr(insetA->dblk,"AFNI_CLUSTSIM_NN1");
         if( atr != NULL ){         // mask stored as reference to a dataset
            NI_element *nel = NI_read_element_fromstring(atr->ch);
            char *nnn;
            nnn = NI_get_attribute(nel,"mask_dset_name");   // dataset name?
            if( nnn == NULL ){
               nnn = NI_get_attribute(nel,"mask_dset_idcode");  // try idcode
               if( nnn != NULL ) nmask = -1;              // can't use idcode
            } else {
               THD_3dim_dataset *mset = THD_open_dataset(nnn); // try to read
               if( mset != NULL ){                              // the dataset
                  nmask = DSET_NVOX(mset);
                  if( nmask != DSET_NVOX(insetA) )       // must match dataset
                     nmask = -1;
                  else if( !no_inmask )
                     mask = THD_makemask(mset,0,1.0f,0.0f);
                  DSET_delete(mset);
               } else {
                  nmask = -1;                            // can't read dataset
               }
            }
            NI_free_element(nel);
         }
      }
   }
   else {
      WARNING_message("No mask being used? That *could* be OK, "
                      "but thought I'd let you know...");
	}

   // apply masks, *IF* one was input somehow. Does nada if mask==NULL
   if( mask ) {
      INFO_message("Applying mask.");
      mri_maskify( DSET_BRICK(insetA, ival), mask );
      mri_maskify( DSET_BRICK(insetA, ithr), mask );
   }

   // ---------- make intermed dsets and pointers -------------

   // basically copying afni_vedit.c's version of clusterizing
   // stuff with data volume
   dblk = insetA->dblk;

   dim = DBLK_BRICK(dblk,ival);
   if( dim == NULL || mri_data_pointer(dim) == NULL ) 
      ERROR_exit("Bad properties of dset to be clusterized");
   
   // stuff with threshold volume
   tim = DBLK_BRICK(dblk,ithr) ;
   // !!!! doublecheck what next part does!!! ** don't think it's nec
   // if( bisid && 
   //    THD_stat_is_2sided(DSET_BRICK_STATCODE(insetA, ithr),0)==0 ) {
   //  INFO_message("UNsetting bisidedness, because stat dset "
   //              "doesn't have sidedness to use (I think).");
   // bisid = 0;
   // }
   // !!!! doublecheck on necessity of next step! probably do need bc
   // !!!! using dblk
   if( DSET_BRICK_FACTOR(insetA, ithr) > 0.0f ) {
      thb /= DSET_BRICK_FACTOR(insetA, ithr);
      tht /= DSET_BRICK_FACTOR(insetA, ithr);
   }

   // ------- what dsets get clusterized and reportized?
   // ------- IF: the user input an additional dataset, then that;
   // ------- ELSE: all depends on the statistics


   if( ival < 0 ) // i.e., no data set input: use only thr dset
      cim = mri_copy(tim); 
   else // use extra dset
      cim = mri_copy(dim); 

   car = mri_data_pointer(cim);
   if( car == NULL ){ 
      mri_free(cim) ;
      ERROR_exit("Badness in internal copy");
   }

   if( (thr_type==-2 ) ) { // in case of bisidedness
      if( ival < 0 ) // i.e., no data set input: use only thr dset
         cim2 = mri_copy(tim); 
      else // use extra dset
         cim2 = mri_copy(dim); 

      car2 = mri_data_pointer(cim2);
      if( car2 == NULL ){ 
         mri_free(cim2) ;
         ERROR_exit("Badness in internal copy");
      }
   }

   // ---------- somewhat odd but nec quants ----------------

   dim->dx = fabs(DSET_DX(insetA));
   dim->dy = fabs(DSET_DY(insetA));
   dim->dz = fabs(DSET_DZ(insetA));

   nx    = insetA->daxes->nxx;
   ny    = insetA->daxes->nyy;
   nz    = insetA->daxes->nzz;
   nxy   = nx * ny; 
   nxyz = nxy * nz;
   dxf = dyf = dzf = 1.0;

   // translate cluster threshold into relevant units
   if( vmul >= 0.0 )
      ptmin = (int) (vmul / (dxf*dyf*dzf) + 0.99);
   else
      ptmin = (int) fabs(vmul);

   // ------------- reporting-type params
   if( CL_1Dform ) {
      sprintf(c1d, "#");
      sprintf(c1dn, " ");
   } 
   else {
      c1d[0] = '\0';
      c1dn[0] = '\0';
   }

   // **************************************************************
   // **************************************************************
   //                 main calcs: threshold+cluster
   // **************************************************************
   // **************************************************************
   
   INFO_message("Thresholding...");

   // for 1sided or 2sided, do this
   if( (thr_type == 1) || (thr_type == -1) || (thr_type == 2) ) {

      // threshold data CIM:
      mri_threshold( tht, thb, tim, cim );

      // find clusters of ALL size
      clar = NIH_find_clusters( nx, ny, nz, dxf, dyf, dzf,
                                cim->kind, car, rmm,
                                0 );
   } 
   else if( thr_type == -2 ) { // bisided


      // threshold data CIM:
      mri_threshold( -BIIIIG_NUM, thb, tim, cim );
      
      // find clusters of ALL size
      clar = NIH_find_clusters( nx, ny, nz, dxf, dyf, dzf,
                                cim->kind, car, rmm,
                                0 );

      // ... and same for the other side
      mri_threshold( tht, BIIIIG_NUM, tim, cim2 );
      
      // find clusters of ALL size
      clar2 = NIH_find_clusters( nx, ny, nz, dxf, dyf, dzf,
                                 cim2->kind, car2, rmm,
                                 0 );
   } else if( thr_type == 3 ) { // within-range

      // threshold data CIM:
      mri_threshold( -BIIIIG_NUM, tht, tim, cim );
      mri_threshold( thb, BIIIIG_NUM, tim, cim );

      // find clusters of ALL size
      clar = NIH_find_clusters( nx, ny, nz, dxf, dyf, dzf,
                                cim->kind, car, rmm,
                                0 );
   }

   INFO_message("Obtaining suprathreshold clusters...");

   // Now apply cluster size threshold
   INIT_CLARR(clbig);
   for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
      cl = clar->clar[iclu];
      if( cl != NULL && cl->num_pt >= ptmin ){ // big enough 
         ADDTO_CLARR(clbig,cl);                // copy pointer 
         clar->clar[iclu] = NULL;              // null out original 
      }
   }
   DESTROY_CLARR(clar);

   if ( thr_type == -2 ) { // also add in other side, if bisided ON
      for( iclu=0 ; iclu < clar2->num_clu ; iclu++ ){
         cl = clar2->clar[iclu];
         if( cl != NULL && cl->num_pt >= ptmin ){ // big enough 
            ADDTO_CLARR(clbig,cl);                // copy pointer 
            clar2->clar[iclu] = NULL;             // null out original 
         }
      }
      DESTROY_CLARR(clar2);
   }

   // final summary/check for no clusters
   clar = clbig ;
   clbig = NULL;
   if( clar == NULL || clar->num_clu == 0 ) {
      printf("%s** NO CLUSTERS FOUND ***\n", c1d);
      if( AFNI_yesenv("AFNI_3dclust_report_zero") ) printf(" 0\n");
      if( clar != NULL ) DESTROY_CLARR(clar);
   }

   // Sort clusters by size
   if( clar->num_clu < 3333 ) {
      INFO_message("Sorting clusters by size.");
      SORT_CLARR(clar);
   } 
   else if( CL_summarize != 1 ){
      printf("%s** TOO MANY CLUSTERS TO SORT BY VOLUME ***\n", c1d) ;
   }

   // --------------- write out map of cluster ROIs

   INFO_message("Writing out map of cluster ROIs.");

   mmm = (short *) calloc(sizeof(short), nxyz);
   for( iclu=0 ; iclu < clar->num_clu ; iclu++ ) {
      cl = clar->clar[iclu] ; 
      if( cl == NULL ) continue ;
      for( ipt=0 ; ipt < cl->num_pt ; ipt++ ) {
         ii = cl->i[ipt]; 
         jj = cl->j[ipt];
         kk = cl->k[ipt];
         mmm[ii+jj*nx+kk*nxy] = (do_binary) ? 1 : (iclu+1);
      }
   }

   THD_3dim_dataset *qset=NULL;
   qset = EDIT_empty_copy(insetA);
   EDIT_dset_items( qset ,
                    ADN_prefix , prefix ,
                    ADN_nvals  , 1 ,
                    ADN_none );
   EDIT_substitute_brick(qset, 0, MRI_short, mmm); 
   mmm = NULL;

   tross_Copy_History( insetA , qset ) ;
   tross_Make_History( "3dClusterize", argc , argv , qset );
   DSET_write(qset); WROTE_DSET(qset); DSET_delete(qset);
   
   // ---------- write out report --------------------

   do_mni = (CL_do_mni && insetA->view_type == VIEW_TALAIRACH_TYPE);
   THD_coorder_fill( my_getenv("AFNI_ORIENT") , &CL_cord);
   if( CL_do_mni )
     THD_coorder_fill( "LPI", &CL_cord );

   if( !CL_quiet ) {
      
      if( CL_summarize != 1 ) {
         printf( 
                "%s\n"
                "%sCluster report for file %s %s\n"
#if 0
                "%s[3D Dataset Name: %s ]\n"
                "%s[    Short Label: %s ]\n"
#endif
                "%s[Connectivity radius = %.2f mm"
                "  Volume threshold = %.2f ]\n"
                "%s[Single voxel volume = %.1f (microliters) ]\n"
                "%s[Voxel datum type    = %s ]\n"
                "%s[Voxel dimensions    = %.3f mm X %.3f mm X %.3f mm ]\n"
                "%s[Coordinates Order   = %s ]\n",
                c1d,
                c1d, argv[iarg], do_mni ? "[MNI coords]" : "",
#if 0
                c1d, insetA->self_name ,
                c1d, insetA->label1 ,
#endif
                c1d, rmm, ptmin*dx*dy*dz ,
                c1d, dx*dy*dz,
                c1d, MRI_TYPE_name[ DSET_BRICK_TYPE(insetA, ival) ],
                c1d, dx,dy,dz,
                c1d, CL_cord.orcode );

         // always fake voxel dims
         printf("%s[Fake voxel dimen    = %.3f mm X %.3f mm X %.3f mm ]\n",
                c1d, dxf,dyf,dzf);
         
         if( nmask > 0 && mask != NULL )           
            printf("%s[Using internal mask]\n", c1d);
         else if( nmask > 0 )
            printf("%s[Skipping internal mask]\n", c1d);
         else if( nmask < 0 )
            printf("%s[Un-usable internal mask]\n", c1d); // shd not happen
         
         if (CL_noabs)
            printf ("%sMean and SEM based on "
                    "Signed voxel intensities: \n%s\n", c1d, c1d);
         else
            printf ("%sMean and SEM based on Absolute Value "
                    "of voxel intensities: \n%s\n", c1d, c1d);

         printf (
                 "%sVolume  CM %s  CM %s  CM %s  min%s  max%s  min%s  max%s  min%s  max%s    Mean     SEM    Max Int  MI %s  MI %s  MI %s\n"
                 "%s------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n",
              c1d,
              ORIENT_tinystr[ CL_cord.xxor ] ,
              ORIENT_tinystr[ CL_cord.yyor ] ,
              ORIENT_tinystr[ CL_cord.zzor ] ,
              ORIENT_tinystr[ CL_cord.xxor ] , ORIENT_tinystr[ CL_cord.xxor ] ,
              ORIENT_tinystr[ CL_cord.yyor ] , ORIENT_tinystr[ CL_cord.yyor ] ,
              ORIENT_tinystr[ CL_cord.zzor ] , ORIENT_tinystr[ CL_cord.zzor ] ,
              ORIENT_tinystr[ CL_cord.xxor ] ,
              ORIENT_tinystr[ CL_cord.yyor ] ,
              ORIENT_tinystr[ CL_cord.zzor ] ,
              c1d
             );

      } else {
         if (CL_noabs) 
            printf ("%sMean and SEM based on Signed voxel intensities: \n", 
                    c1d);
         else
            printf ("%sMean and SEM based on Absolute Value "
                    "of voxel intensities: \n", c1d);
         printf("%sCluster summary for file %s %s\n" ,
                c1d, argv[iarg] , do_mni ? "[MNI coords]" : "");
         printf("%sVolume  CM %s  CM %s  CM %s  Mean    SEM    \n", 
                c1d, 
                ORIENT_tinystr[ CL_cord.xxor ],
                ORIENT_tinystr[ CL_cord.yyor ],
                ORIENT_tinystr[ CL_cord.zzor ]);
      }
   } // end of report header

   ndet = 0;

   vol_total = nvox_total = 0;
   glmmsum = glmssum = glsqsum = glxxsum = glyysum = glzzsum = 0;
   
   for( iclu=0; iclu < clar->num_clu; iclu++ ){
      cl = clar->clar[iclu];
      if( cl == NULL || cl->num_pt < ptmin ) continue;  // no good
      
      volsum = cl->num_pt * dxf*dyf*dzf;
      xxsum = yysum = zzsum = mmsum = mssum = 0.0;
      xxmax = yymax = zzmax = mmmax = msmax = 0.0;
      sqsum = sem = 0;
      
      // These should be pegged at whatever actual max/min values are 
      RLmax = APmax = ISmax = -1000;
      RLmin = APmin = ISmin = 1000;
      
      for( ipt=0; ipt < cl->num_pt; ipt++ ){

#if 0
         // this is obsolete and nonfunctional code 
         IJK_TO_THREE( cl->ijk[ipt] , ii,jj,kk , nx,nxy );
#endif
         ii = cl->i[ipt]; jj = cl->j[ipt]; kk = cl->k[ipt];
         
         fv = THD_3dind_to_3dmm( insetA, TEMP_IVEC3(ii,jj,kk) );
         fv = THD_3dmm_to_dicomm( insetA, fv );
         xx = fv.xyz[0]; yy = fv.xyz[1]; zz = fv.xyz[2];
         if( !do_mni )
            THD_dicom_to_coorder( &CL_cord , &xx,&yy,&zz );
         else
            THD_3tta_to_3mni( &xx , &yy , &zz );
         
         ms = cl->mag[ipt];
         mm = fabs(ms);
         
         mssum += ms;
         mmsum += mm; 
         
         sqsum += mm * mm;

         // PT: forward looking note-- to calculate center of mass of
         // clusters *without* using the value of the voxel, could
         // probably edit here (or the 'mm' values above).  Note that
         // one would have to check about the global ones, to see how
         // those are affected, as well as mssum.
         xxsum += mm * xx ; yysum += mm * yy ; zzsum += mm * zz ;
         if( mm > mmmax ){
            xxmax = xx ; yymax = yy ; zzmax = zz ;
            mmmax = mm ; msmax = ms ;
         }
         
         /* Dimensions: */
         if ( xx > RLmax )
            RLmax = xx;
         if ( xx < RLmin )
            RLmin = xx;	
         if ( yy > APmax )
            APmax = yy;
         if ( yy < APmin )
            APmin = yy;		
         if ( zz > ISmax )
            ISmax = zz;
         if ( zz < ISmin )
            ISmin = zz;
      }
      if( mmsum == 0.0 ) continue ;
      
      // PT: I think that the "gl*" parameters are "global" ones.
      glmssum += mssum;
      glmmsum += mmsum;
      glsqsum += sqsum;
      glxxsum += xxsum;
      glyysum += yysum;
      glzzsum += zzsum;
      
      ndet++ ;
      xxsum /= mmsum; 
      yysum /= mmsum; 
      zzsum /= mmsum;
    
      if (CL_noabs)   
         mean = mssum / cl->num_pt;
      else            
         mean = mmsum / cl->num_pt;
    
      if( fimfac != 0.0 ) {
         mean  *= fimfac;  
         msmax *= fimfac;
         sqsum *= fimfac*fimfac; 
      }  

      /* MSB 11/1/96  Calculate SEM using SEM^2=s^2/N,
         where s^2 = (SUM Y^2)/N - (Ymean)^2
         where sqsum = (SUM Y^2 ) 
      */
    
      if (cl->num_pt > 1) {
         sem = (sqsum - (cl->num_pt * mean * mean)) / (cl->num_pt - 1);
         if (sem > 0.0) 
            sem = sqrt( sem / cl->num_pt );
         else 
            sem = 0.0;
      }
      else
         sem = 0.0;
    
      if( CL_summarize != 1 ) {
         MCW_fc7(mean,  buf1);
         MCW_fc7(msmax, buf2);
         MCW_fc7(sem,   buf3);
       
         printf("%s%6.0f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %7s  %7s  %7s  %5.1f  %5.1f  %5.1f \n",
                c1dn, volsum, xxsum, yysum, zzsum, 
                RLmin, RLmax, APmin, APmax, ISmin, ISmax, 
                buf1, buf3, buf2, xxmax, yymax, zzmax );
      }
    
      nvox_total += cl->num_pt;
      vol_total  += volsum;
   }

   DESTROY_CLARR(clar);
   if( ndet == 0 ) {
      printf("%s** NO CLUSTERS FOUND ABOVE THRESHOLD VOLUME ***\n", c1d);
      if( AFNI_yesenv("AFNI_3dclust_report_zero") ) 
         printf(" 0\n");
   }
   
   // Calculate global SEM 
   if (CL_noabs)   glmean = glmssum / nvox_total; 
   else            glmean = glmmsum / nvox_total;
   
   if( fimfac != 0.0 ) { 
      glsqsum *= fimfac*fimfac; 
      glmean *= fimfac; 
   }
   if (nvox_total > 1) {
      sem = (glsqsum - (nvox_total*glmean*glmean)) / (nvox_total - 1);
      if (sem > 0.0) sem = sqrt( sem / nvox_total );  
      else sem = 0.0;
	}
   else
      sem = 0.0;

   glxxsum /= glmmsum ; glyysum /= glmmsum ; glzzsum /= glmmsum ;
   
   // Modified so that mean and SEM would print in correct column
   if( CL_summarize == 1 ) {
      if( !CL_quiet )
         printf( "%s------  -----  -----  ----- -------- -------- \n", c1d);
      printf("%s%6.0f  %5.1f  %5.1f  %5.1f %8.1f %6.3f\n", 
             c1d, vol_total, glxxsum, glyysum, glzzsum, glmean, sem);
   }
   else if( ndet > 1 && CL_summarize != -1 ) {
      MCW_fc7(glmean ,buf1) ;
      MCW_fc7(sem    ,buf3) ;
      if( !CL_quiet )
         printf ("%s------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n", c1d);
      printf ("%s%6.0f  %5.1f  %5.1f  %5.1f                                            %7s  %7s                             \n",
              c1d, vol_total, glxxsum, glyysum, glzzsum, buf1, buf3 ) ;
   }
   
   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   INFO_message("Cleaning");

   if( mmm )
      free(mmm);

   if(insetA) {
      DSET_delete(insetA);
      free(insetA);
   }

   if( INMASK ) {
      DSET_delete(INMASK);
      free(INMASK);
   }
      
   if(mask)
      free(mask);

   if(prefix)
      free(prefix);

   if(Dim)
      free(Dim);

   // if( cl )
   // KILL_CLUSTER(cl);

   if( cim ) 
      free(cim);

   if( cim2 ) 
      free(cim2);

   INFO_message("Done!");

   return 0;
}

// -----------------------------------------------------------

void MCW_fc7( float qval , char * buf )
{
   float aval = fabs(qval) ;
   int lv , il ;
   char lbuf[16] ;

   /* special case if the value is an integer */

   lv = (int) qval ;

   if( qval == lv && abs(lv) < 100000 ){
      if( lv >= 0 ) sprintf( buf , " %d" , lv ) ;
      else          sprintf( buf , "%d"  , lv ) ;
      return ;
   }

/* macro to strip trailing zeros from output */

#define BSTRIP \
   for( il=6 ; il>1 && lbuf[il]=='0' ; il-- ) lbuf[il] = '\0'

   /* noninteger: choose floating format based on magnitude */

   lv = (int) (10.0001 + log10(aval)) ;

   switch( lv ){

      default:
         if( qval > 0.0 ) sprintf( lbuf , "%7.1e" , qval ) ;
         else             sprintf( lbuf , "%7.0e" , qval ) ;
      break ;

      case  7:  /* 0.001 -0.01  */
      case  8:  /* 0.01  -0.1   */
      case  9:  /* 0.1   -1     */
      case 10:  /* 1     -9.99  */
         sprintf( lbuf , "%7.4f" , qval ) ; BSTRIP ; break ;

      case 11:  /* 10-99.9 */
         sprintf( lbuf , "%7.3f" , qval ) ; BSTRIP ; break ;

      case 12:  /* 100-999.9 */
         sprintf( lbuf , "%7.2f" , qval ) ; BSTRIP ; break ;

      case 13:  /* 1000-9999.9 */
         sprintf( lbuf , "%7.1f" , qval ) ; BSTRIP ; break ;

      case 14:  /* 10000-99999.9 */
         sprintf( lbuf , "%7.0f" , qval ) ; break ;

   }
   strcpy(buf,lbuf) ;
   return ;
}
// -----------------------------------------------------------
