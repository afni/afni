/* 
   Description

   2018 05 08: + inception for this version

   2018 05 12: + working version, new options for p-to-stat, new
                 report labels

   2018 05 13: + [PT] bug fix: wouldn't work with extra data set
                 entered

   2018 05 17: + [PT] new report fields, more labels and stat_aux info
               + [PT] can use brick_labels to specify vols, too

   2018 05 23: + [PT] Adding in D. Glen suggestions/fixes (thanks!):
                 - cleaning up some stuff, pointer errors if no clusters
                 - default prefixes for output
                 - case of Fstat -> no bisided/twosided allowed! More
                   generally, there is a check to make sure that when
                   the user asks for multi-sided clusterizing that the
                   stat itself is multisided;  else, fail.
                 - allow RIGHT and LEFT as -1sided flags
                 - no datasets output unless asked for

   2018 05 23: + [PT] Adding in more D. Glen suggestions/fixes:
                 - but who runs a 1sided-LEFT test on an Fstat???

   2018 05 27: + [PT] Adding in more D. Glen suggestions/fixes:
                 - INT_CMAP attribute set for pref_map output (cluster map)
                 - fix report strings, all hashed

   2018 06 01: + [PT] null map output in case of no clusts, if user desires:
                 - for troublemaking users, only!

   2018 07 23: + [PT] put in overwrite checks
                 - should fail with nonzero exit if can't write output

   2018 08 10: + [PT] allow non-stat brick to be thresholdable
                 - check istatfunc differently now

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
#include "DoTrackit.h"
#include "basic_boring.h"

#define BIIIIG_NUM 1.e9

// allow 'negative p-value', for syntax reasons
#define IsNotValidP(p, nsides) ( ( (p<0) || (p>(0.5*nsides)) ) ? 1 : 0 )

void MCW_fc7( float qval , char *buf );
int CheckStringStart( char *a, char *b, char *c);
int ssgn(float x);

int MakeBrickLab1(char *c0, char *c1, char *c2, char *c3);
int MakeBrickLab2(char *c0, char *c1, char *c2);

int mycheck_is_int(char *x);

void usage_Clusterize(int detail) 
{
   printf(
" PURPOSE ~1~\n"
" \n"
" This program is for performing clusterizing: one can perform voxelwise\n"
" thresholding on a dataset (such as a statistic), and then make a map\n"
" of remaining clusters of voxels larger than a certain volume.  The\n"
" main output of this program is a single volume dataset showing a map\n"
" of the cluster ROIs.\n"
" \n"
" This program is specifically meant to reproduce behavior of the muuuch\n"
" older 3dclust, but this new program:\n"
"   + uses simpler syntax (hopefully);\n"
"   + includes additional clustering behavior such as the '-bisided ...'\n"
"     variety (essentially, two-sided testing where all voxels in a\n"
"     given cluster come from either the left- or right- tail, but not\n"
"     mixed);\n"
"   + a mask (such as the whole brain) can be entered in;\n"
"   + voxelwise thresholds can be input as statistic values or p-values.\n"
" \n"
" This program was also written to have simpler/more direct syntax of\n"
" usage than 3dclust.  Some minor options have been carried over for\n"
" similar behavior, but many of the major option names have been\n"
" altered.  Please read the helps for those below carefully.\n"
" \n"
" This program was cobbled together by PA Taylor (NIMH, NIH), but it\n"
" predominantly uses code written by many legends: RW Cox, BD Ward, MS\n"
" Beauchamp, ZS Saad, and more.\n"
" \n"
" \n"
" USAGE ~1~\n"
" \n"
"   Input: ~2~\n"
" \n"
"     + A dataset of one or more bricks\n"
"     + Specify an index of the volume to threshold\n"
"     + Declare a voxelwise threshold, and optionally a cluster-volume\n"
"       threshold\n"
"     + Optionally specify the index an additional 'data' brick\n"
"     + Optionally specify a mask\n"
" \n"
"   Output: ~2~\n"
" \n"
"     + A report about the clusters (center of mass, extent, volume,\n"
"       etc.) that can be dumped into a text file.\n"
"\n"
"     + Optional: A dataset volume containing a map of cluster ROIs \n"
"       (sorted by size) after thresholding (and clusterizing, if\n"
"       specified).\n"
"       That is, a data set where the voxels in the largest cluster all\n"
"       have a value 1, those in the next largest are all 2, etc.\n"
"     + Optional: a cluster-masked version of an input data set. That is,\n"
"       the values of a selected data set (e.g., effect estimate) that fall\n"
"       within a cluster are output unchanged, and those outside a cluster\n"
"       are zeroed.\n"
"     + Optional: a mask.\n"
" \n"
"   Explanation of 3dClusterize text report: ~2~\n"
" \n"
"     Nvoxel       : Number of voxels in the cluster\n"
" \n"
"     CM RL        : Center of mass (CM) for the cluster in the Right-Left\n"
"                    direction (i.e., the coordinates for the CM)\n"
" \n"
"     CM AP        : Center of mass for the cluster in the\n"
"                    Anterior-Posterior direction\n"
" \n"
"     CM IS        : Center of mass for the cluster in the\n"
"                    Inferior-Superior direction\n"
" \n"
"     minRL, maxRL : Bounding box for the cluster, min and max\n"
"                    coordinates in the Right-Left direction\n"
" \n"
"     minAP, maxAP : Min and max coordinates in the Anterior-Posterior\n"
"                    direction of the volume cluster\n"
" \n"
"     minIS, maxIS : Min and max coordinates in the Inferior-Superior\n"
"                    direction of the volume cluster\n"
" \n"
"     Mean         : Mean value for the volume cluster\n"
" \n"
"     SEM          : Standard Error of the Mean for the volume cluster\n"
" \n"
"     Max Int      : Maximum Intensity value for the volume cluster\n"
" \n"
"     MI RL        : Coordinate of the Maximum Intensity value in the\n"
"                    Right-Left direction of the volume cluster\n"
" \n"
"     MI AP        : Coordinate of the Maximum Intensity value in the\n"
"                    Anterior-Posterior direction of the volume cluster\n"
" \n"
"     MI IS        : Coordinate of the Maximum Intensity value in the\n"
"                    Inferior-Superior direction of the volume cluster\n"
" \n"
"     * CM values use the absolute value of the voxel values as weights.\n"
" \n"
"     * The program does not work on complex- or rgb-valued datasets!\n"
" \n"
"     * SEM values are not realistic for interpolated data sets!  A\n"
"       ROUGH correction is to multiply the SEM of the interpolated data\n"
"       set by the square root of the number of interpolated voxels per\n"
"       original voxel.\n"
" \n"
"     * Some summary or 'global' values are placed at the bottoms of\n"
"       report columns, by default.  These include the 'global' volume,\n"
"       CM of the combined cluster ROIs, and the mean+SEM of that\n"
"       Pangaea.\n"
" \n"
" \n"
" COMMAND OPTIONS ~1~\n"
" \n"
" -inset  III    :Load in a dataset III of one or more bricks for\n"
"                 thresholding and clusterizing; one can choose to use\n"
"                 either just a single sub-brick within it for all\n"
"                 operations (e.g., a 'statistics' brick), or to specify\n"
"                 an additional sub-brick within it for the actual\n"
"                 clusterizing+reporting (after the mask from the\n"
"                 thresholding dataset has been applied to it).\n"
" \n"
" -mask MMM      :Load in a dataset MMM to use as a mask, within which\n"
"                 to look for clusters.\n"
" \n"
" -mask_from_hdr :If 3dClustSim put an internal attribute into the\n"
"                 input dataset that describes a mask, 3dClusterize will\n"
"                 use this mask to eliminate voxels before clustering,\n"
"                 if you give this option (this is how the AFNI\n"
"                 Clusterize GUI works by default).  If there is no\n"
"                 internal mask in the dataset header, then this\n"
"                 doesn't do anything.\n"
" \n"
" -out_mask OM   :specify that you wanted the utilized mask dumped out\n"
"                 as a single volume dataset OM.  This is probably only\n"
"                 really useful if you are using '-mask_from_hdr'.  If\n"
"                 not mask option is specified, there will be no output.\n"
" \n"
" -ithr   j      :(required) Uses sub-brick [j] as the threshold source;\n"
"                 'j' can be either an integer *or* a brick_label string.\n"
" \n"
" -idat   k      :Uses sub-brick [k] as the data source (optional);\n"
"                 'k' can be either an integer *or* a brick_label string.\n"
"                 If this option is used, thresholding is still done by\n"
"                 the 'threshold' dataset, but that threshold map is\n"
"                 applied to this 'data' set, which is in turn used for\n"
"                 clusterizing and the 'data' set values are used to\n"
"                 make the report.  If a 'data' dataset is NOT input\n"
"                 with '-idat ..', then thresholding, clustering and\n"
"                 reporting are all done using the 'threshold' dataset.\n"
" \n"
" -1sided SSS TT :Perform one-sided testing. Two arguments are required:\n"
"                   SSS -> either 'RIGHT_TAIL' (or 'RIGHT') or 'LEFT_TAIL'\n"
"                          (or 'LEFT') to specify which side of the \n"
"                          distribution to test.\n"
"                   TT  -> the threshold value itself.\n"
"                 See 'NOTES' below to use a p-value as threshold.\n"
" \n"
" -2sided  LL RR :Perform two-sided testing. Two arguments are required:\n"
"                   LL  -> the upper bound of the left tail.\n"
"                   RR  -> lower bound of the right tail.\n"
"                 *NOTE* that in this case, potentially a cluster could\n"
"                 be made of both left- and right-tail survivors (e.g.,\n"
"                 both positive and negative values). For this reason,\n"
"                 probably '-bisided ...' is a preferable choice.\n"
"                 See 'NOTES' below to use a p-value as threshold.\n"
" \n"
" -bisided LL RR :Same as '-2sided ...', except that the tails are tested\n"
"                 independently, so a cluster cannot be made of both.\n"
"                 See 'NOTES' below to use a p-value as threshold.\n"
" \n"
" -within_range AA BB\n"
"                :Perform a kind of clustering where a different kind of\n"
"                 thresholding is first performed, compared to the above\n"
"                 cases;  here, one keeps values within the range [AA, BB],\n"
"                 INSTEAD of keeping values on the tails. Is this useful?\n"
"                 Who knows, but it exists.\n"
"                 See 'NOTES' below to use a p-value as threshold.\n"
" \n"
" -NN {1|2|3}    :Necessary option to specify how many neighbors a voxel\n"
"                 has; one MUST put one of 1, 2 or 3 after it:\n"
"                   1 -> 6 facewise neighbors\n"
"                   2 -> 18 face+edgewise neighbors\n"
"                   3 -> 26 face+edge+cornerwise neighbors\n"
"                 If using 3dClustSim (or any other method), make sure\n"
"                 that this NN value matches what was used there. (In\n"
"                 many AFNI programs, NN=1 is a default choice, but BE\n"
"                 SURE YOURSELF!)\n"
" \n"
" -clust_nvox M  :specify the minimum cluster size in terms of number\n"
"                 of voxels M (such as output by 3dClustSim).\n"
" \n"
" -clust_vol   V :specify the minimum cluster size in terms of volume V,\n"
"                 in microliters (requires knowing the voxel\n"
"                 size). Probably '-clust_nvox ...' is more useful.\n"
" \n"
" -pref_map PPP  :The prefix/filename of the output map of cluster ROIs.\n"
"                 The 'map' shows each cluster as a set of voxels with the\n"
"                 same integer.  The clusters are ordered by size, so the\n"
"                 largest cluster is made up of 1s, the next largest of 2s,\n"
"                 etc.\n"
"                 (def:  no map of clusters output).\n"
" \n"
" -pref_dat DDD  :Including this option instructs the program to output\n"
"                 a cluster-masked version of the 'data' volume\n"
"                 specified by the '-idat ..' index.  That is, only data\n"
"                 values within the cluster ROIs are included in the\n"
"                 output volume.  Requires specifying '-idat ..'.\n"
"                 (def:  no cluster-masked dataset output).\n"
" \n"
" -1Dformat      :Write output in 1D format (now default). You can\n"
"                 redirect the output to a .1D file and use the file\n"
"                 as input to whereami for obtaining Atlas-based\n"
"                 information on cluster locations.\n"
"                 See whereami -help for more info.\n"
" \n"
" -no_1Dformat   :Do not write output in 1D format.\n"
" \n"
" -summarize     :Write out only the total nonzero voxel count and\n"
"                 volume for each dataset\n"
" \n"
" -nosum         :Suppress printout of the totals\n"
" \n"
" -quiet         :Suppress all non-essential output\n"
" \n"
" -outvol_if_no_clust: flag to still output an (empty) vol if no \n"
"                 clusters are found.  Even in this case, no report is\n"
"                 is produced if no clusters are found.  This option is\n"
"                 likely used for some scripting scenarios; also, the\n"
"                 user would still need to specify '-pref_* ...' options\n"
"                 as above in order to output any volumes with this opt.\n"
"                 (def: no volumes output if no clusters found).\n"
"\n"
" -orient OOO    :change the coordinate output order to 'OOO' (def:\n"
"                 is DICOM, RAI); alternatively, one could set the\n"
"                 environment variable AFNI_ORIENT (see the file\n"
"                 README.environment).\n"
" \n"
" -noabs         :Use the signed voxel intensities (not the absolute\n"
"                 value) for calculation of the mean and Standard\n"
"                 Error of the Mean (SEM)\n"
" \n"
" -binary        :This turns the output map of cluster ROIs into a binary\n"
"                 (0 or 1) mask, rather than a cluster-index mask.\n"
"                 If no clusters are found, the mask is not written!\n"
"                 (def: each cluster has separate values)\n"
" \n"
" \n"
" NOTES ~1~\n"
" \n"
"   Saving the text report ~2~\n"
" \n"
"     To save the text file report, use the redirect '>' after the\n"
"     3dClusterize command and dump the text into a separate file of\n"
"     your own naming.\n"
" \n"
"   Using p-values as thresholds ~2~\n"
" \n"
"     By default, numbers entered as voxelwise thresholds are assumed to\n"
"     be appropriate statistic values that you have calculated for your\n"
"     desired significance (e.g., using p2dsetstat).  HOWEVER, if you\n"
"     just want to enter p-values and have the program do the conversion\n"
"     work for you, then do as follows: prepend 'p=' to your threshold\n"
"     number.\n"
" \n"
"     - For one-sided tests, the *_TAIL specification is still used, so\n"
"       in either case the p-value just represents the area in the\n"
"       statistical distribution's tail (i.e., you don't have to worry\n"
"       about doing '1-p').  Examples:\n"
"         -1sided RIGHT_TAIL p=0.005\n"
"         -1sided LEFT_TAIL  p=0.001\n"
" \n"
"     - For the two-sided/bi-sided tests, the a single p-value is\n"
"       entered to represent the total area under both tails in the\n"
"       statistical distribution, which are assumed to be symmetric.\n"
"       Examples:\n"
"         -bisided p=0.001\n"
"         -2sided  p=0.005\n"
" \n"
"       If you want asymmetric tails, you will have to enter both\n"
"       threshold values as statistic values (NB: you could use\n"
"       p2dsetstat to convert each desired p-value to a statistic, and\n"
"       then put in those stat values to this program).\n"
" \n"
"     You will probably NEED to have negative signs for the cases of\n"
"     '-1sided LEFT_TAIL ..', and for the first entries of '-bisided ..'\n"
"     or '-2sided ..'.\n"
" \n"
"     You cannot mix p-values and statistic values (for two-sided\n"
"     things, enter either the single p-value or both stats).\n"
" \n"
"   Performing appropriate testing ~2~\n"
" \n"
"     Don't use a pair of one-sided tests when you *should* be using a\n"
"     two-sided test!\n"
" \n"
" \n"
" EXAMPLES ~1~\n"
" \n"
"   1. Take an output of FMRI testing (e.g., from afni_proc.py), whose\n"
"   [1] brick contains the effect estimate from a statistical model and\n"
"   whose [2] brick contains the associated statistic; use the results\n"
"   of 3dClustSim run with NN=1 (here, a cluster threshold volume of 157\n"
"   voxels) and perform one-sided testing with a threshold at an\n"
"   appropriate value (here, 3.313).\n"
" \n"
"     3dClusterize                  \\\n"
"        -inset stats.FT+tlrc.      \\\n"
"        -ithr 2                    \\\n"
"        -idat 1                    \\\n"
"        -mask mask_group+tlrc.     \\\n"
"        -NN 1                      \\\n"
"        -1sided RIGHT_TAIL 3.313   \\\n"
"        -clust_nvox 157            \\\n"
"        -pref_map ClusterMap\n"
" \n"
"   2. The same as Ex. 1, but using bisided testing (two sided testing\n"
"   where the results of each tail can't be joined into the same\n"
"   cluster). Note, the tail thresholds do NOT have to be symmetric (but\n"
"   often they are).  Also, here we output the cluster-masked 'data'\n"
"   volume.\n"
" \n"
"     3dClusterize                  \\\n"
"        -inset stats.FT+tlrc.      \\\n"
"        -ithr 2                    \\\n"
"        -idat 1                    \\\n"
"        -mask mask_group+tlrc.     \\\n"
"        -NN 1                      \\\n"
"        -bisided -3.313 3.313      \\\n"
"        -clust_nvox 157            \\\n"
"        -pref_map ClusterMap       \\\n"
"        -pref_dat ClusterEffEst\n"
" \n"
"   3. The same as Ex. 2, but specifying a p-value to set the voxelwise\n"
"   thresholds (in this case, tails DO have to be symmetric).\n"
" \n"
"     3dClusterize                  \\\n"
"        -inset stats.FT+tlrc.      \\\n"
"        -ithr 2                    \\\n"
"        -idat 1                    \\\n"
"        -mask mask_group+tlrc.     \\\n"
"        -NN 1                      \\\n"
"        -bisided p=0.001           \\\n"
"        -clust_nvox 157            \\\n"
"        -pref_map ClusterMap       \\\n"
"        -pref_dat ClusterEffEst\n"
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
   char *prefix=NULL; 

   int STATINPUT  = 1; // flag for whether user input stat (1) or pval (0)
   int STATINPUT2 = 1;
   char chtmp[200]; //=NULL;
   float tmpb, tmpt;

   char blab1[100]="", blab2[100]="", blab3[100]="NN";

   int Nvox=-1;   // tot number vox
   int *Dim=NULL;

   int NNTYPE = 0;   // user *needs* to choose? or just make 1 by
   // default?
   int thr_type = 0;  // user needs to choose with -1sided, -2sided,
                      // etc.; 
                      //  1 -> 1sided, right; 
                      // -1 -> 1sided, left;
                      //  2 -> 2sided; 
                      // -2 -> bisided; 
                      //  3 -> within_range
   float thr_1sid = -1.1e10; 
   
   // names modeled on existing progs, like afni_vedit.c
   float vmul;        // cluster vol thr: can be neg, pos, zero
   float rmm=0;       // for now, leave as zero to flag using NN neigh val
   float thb, tht;    // bot thr val for Rtail; top thr val for Ltail
   int   ival=-1, ithr=-1;    // idxs for dat and thr vols
   char cval[THD_MAX_SBLABEL]=""; // for opt brick_label input...
   char cthr[THD_MAX_SBLABEL]=""; // ... of dat and thr vols
   int   posfunc = 0;      // use for "pos only" behavior-- not needed here
   int   no_inmask = 1 ;   // about reading mask from header
   byte *mask=NULL;        // can be input or read from header
   int nmask=0; 

   int nx, ny, nz, nxy, nxyz; 
   float dxf, dyf, dzf;
   MCW_cluster_array *clar=NULL, *clar2=NULL, *clbig=NULL;
   char c1d[2] = {""}, c1dn[2] = {""};

   THD_datablock *dblk=NULL;
   MRI_IMAGE *tim=NULL, *dim=NULL;        // NB: 'dim' is other ex. 'bim'
   MRI_IMAGE *cim=NULL;  void *car=NULL;  // copies of thr vol to apply thr
   MRI_IMAGE *cim2=NULL; void *car2=NULL; // same as cim, but for bisided

   int qv; 
   short *mmm=NULL;
   int iclu, ipt, ii, jj, kk, ptmin;
   MCW_cluster *cl=NULL;

   char repstr[200]  = "";
   char repstat[200] = "";
   int  istatfunc    = -1;
   char rrstr[50]    = "";

   char repthr[200]  = "";
   char repval[200] = "";
   char repmask[200] = "";

   // mainly report-related
   char *CL_prefix  = NULL;  // output data volume, if asked for
   char *CL_maskout = NULL;  // output WB mask volume, if asked for
   char *CL_mritype = NULL;
   int CL_quiet     = 0;
   int CL_summarize = 0;
   int CL_do_mni    = 0; 
   int CL_1Dform    = 1;
   int CL_noabs     = 0;  
   int do_binary    = 0;
   THD_coorder CL_cord;
   float dx, dy, dz, xx, yy, zz, mm, ms, fimfac=0.0,
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

   int FOUND_CLUSTERS = 0; 
   int OUTVOL_IF_NO_CLUST = 0; 
   int STAT_nsides = -1;

   mainENTRY("3dClusterize"); machdep(); 
  
   // ****************************************************************
   // ****************************************************************
   //                    load AFNI stuff
   // ****************************************************************
   // ****************************************************************
	
   /** scan args **/
   if (argc == 1) { usage_Clusterize(1); exit(0); }
   iarg = 1; 
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strcmp(argv[iarg],"-help") == 0 || 
          strcmp(argv[iarg],"-h") == 0 ) {
         usage_Clusterize(strlen(argv[iarg])>3 ? 2:1);
         exit(0);
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

      if( strcmp(argv[iarg],"-pref_map") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-pref_map'");
         prefix = strdup(argv[iarg]);
         if( !THD_filename_ok(prefix) ) 
            ERROR_exit("Illegal name after '-pref_map'");
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-pref_dat") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-pref_dat'");
         CL_prefix = strdup(argv[iarg]);
         if( !THD_filename_ok(CL_prefix) ) 
            ERROR_exit("Illegal name after '-pref_dat'");
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-out_mask") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '%s'",
                                argv[iarg-1]);
         CL_maskout = strdup(argv[iarg]);
         if( !THD_filename_ok(CL_maskout) ) 
            ERROR_exit("Illegal name after '%s'", argv[iarg-1]);
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
         sprintf(repmask,"%s",argv[iarg]);
         iarg++ ; continue ;
      }

      // the same as "-inmask" in 3dclust
      if( strcmp(argv[iarg],"-mask_from_hdr") == 0) {
         no_inmask = 0;
         sprintf(repmask,"(mask from header)");
         iarg++ ; continue ;
      }

      // ---------- which vols are data and thr vols

      // index for vol to be thresholded; can also be brick_label str
      if( strcmp(argv[iarg],"-ithr") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-ithr'");         
         if( mycheck_is_int(argv[iarg]) )
            ithr = atoi(argv[iarg]);
         else
            sprintf(cthr,"%s",argv[iarg]);
         iarg++ ; continue ;
      }

      // index for vol to be clusterized; can also be brick_label str
      if( strcmp(argv[iarg],"-idat") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-idat'");         
         if( mycheck_is_int(argv[iarg]) )
            ival = atoi(argv[iarg]);
         else
            sprintf(cval,"%s",argv[iarg]);
         iarg++ ; continue ;
      }

      // ---------- sidedness ----------------------

      // 2 necessary args needed after this opt
      if( strcmp(argv[iarg],"-1sided") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-1sided'");
         // get first arg
         if( ( strcmp(argv[iarg],"RIGHT_TAIL") == 0 ) ||
             ( strcmp(argv[iarg],"RIGHT") == 0 ) )
            thr_type = 1; // def: full FOV
         else if( ( strcmp(argv[iarg],"LEFT_TAIL") == 0 ) ||
                  ( strcmp(argv[iarg],"LEFT") == 0 ) )
            thr_type = -1; // only where A or B is nonzero
         else 
            // out of tails
            ERROR_exit("Illegal immediately following '-1sided'"
                       ": need 'RIGHT_TAIL' or 'LEFT_TAIL'");
         // get second arg
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need 2 arguments after '-1sided'");

         // STATINPUT gets acted on later, when we know thr vol for
         // possible p-value conversion

         STATINPUT = CheckStringStart(argv[iarg],"p=", chtmp);
         thr_1sid = atof(chtmp); // atof(argv[iarg]);
         if( !STATINPUT )
            if( IsNotValidP(thr_1sid, 1) )
               ERROR_exit("Non-valid p-value (%f) entered!", thr_1sid);

         // assign one thr to user's value, and other effectively to
         // -/+ infty
         if( thr_type == 1 ) { // R
            thb = thr_1sid;
            tht = - BIIIIG_NUM;
            sprintf(repstr, "right-tail stat=%f", thb);
         }
         else { // L
            tht = thr_1sid;
            thb = BIIIIG_NUM;
            sprintf(repstr, "left-tail stat=%f", tht);
         }         

         MakeBrickLab1(blab1,argv[iarg-2]+1,argv[iarg-1],argv[iarg]);

         iarg++ ; continue ;
      }

      // if 'p=..' is entered, just one argument; else, 2 args
      // necessary; 2sided, bisided and within_range all have same
      // input syntax!
      if( ( strcmp(argv[iarg],"-2sided") == 0 ) ||
          ( strcmp(argv[iarg],"-bisided") == 0 ) ||
          ( strcmp(argv[iarg],"-within_range") == 0 ) ) {
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need at least one argument after '%s'",
                                argv[iarg-1]);

         if( strcmp(argv[iarg-1],"-2sided") == 0 )
            thr_type = 2;
         else if( strcmp(argv[iarg-1],"-bisided") == 0 )
            thr_type = -2;
         else if( strcmp(argv[iarg-1],"-within_range") == 0 )
            thr_type = 3;
         else
            ERROR_exit("Problem keeping track of input arguments for test");

         STATINPUT = CheckStringStart(argv[iarg],"p=", chtmp);
         tht = atof(chtmp); // for top of L tail

         if( !STATINPUT ) {
            if( IsNotValidP(tht, 2) )
               ERROR_exit("Non-valid p-value (%f) entered!", tht);
            thb = tht;
            // ... and we're done here with 1 arg
         }
         else { // get second arg: must not be 'p=...'
            iarg++ ; if( iarg >= argc ) 
                        ERROR_exit("Need 2 arguments after '%s' here",
                                   argv[iarg-2]);
            STATINPUT2 = CheckStringStart(argv[iarg],"p=", chtmp);
            if( !STATINPUT ) 
               ERROR_exit("If using 'p=...', only use one argument!");
            
            thb = atof(chtmp); // for bot of R tail
            sprintf(repstr, "left-tail stat=%f;  "
                    "right-tail stat=%f", tht, thb);
         }

         if( STATINPUT )
            MakeBrickLab1(blab1,argv[iarg-2]+1,argv[iarg-1],argv[iarg]);
         else
            MakeBrickLab2(blab1,argv[iarg-1],argv[iarg]);

         iarg++ ; continue ;
      }
            
      // ---------- control pars

      if( strcmp(argv[iarg],"-NN") == 0 ) {
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
         strcat(blab3, argv[iarg]);
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

         MakeBrickLab2(blab2,argv[iarg-1]+1,argv[iarg]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-clust_vol") == 0 ){
         iarg++ ; if( iarg >= argc ) 
                     ERROR_exit("Need argument after '-clust_vol'");
         vmul = atof(argv[iarg]);
         if ( vmul <= 0 )
            ERROR_exit( "Need the volume threshold to be >=0 (not %f)",
                        vmul);
         else if( vmul )
            vmul = -vmul; // ALSO do this because of syntax of older progs!

         MakeBrickLab2(blab2,argv[iarg-1]+1,argv[iarg]);

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-outvol_if_no_clust") == 0 ) {
         OUTVOL_IF_NO_CLUST = 1 ;
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
	
   if( !thr_type )
      ERROR_exit("Hey, you need to put in threshold type/value!"
                 "For example, '-2sided ...', '-1sided ...', etc.\n");

   // check input dsets and matching, if necessary
   // (insetA is possibly modified my masking operation)
   THD_load_no_mmap();
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

   // check if str labels were input
   if( strlen(cthr) ) {
      for( i=0 ; i<Dim[3] ; i++)
         if( !strcmp (DSET_BRICK_LABEL(insetA, i), cthr) )
            ithr = i;
      if( ithr < 0 )
         ERROR_exit("Could not find string label: %s", cthr);
      INFO_message("Found string label '%s' as volume [%d]", cthr, ithr);
   }

   if( strlen(cval) ) {
      for( i=0 ; i<Dim[3] ; i++)
         if( !strcmp (DSET_BRICK_LABEL(insetA, i), cval) )
            ival = i;
      if( ival < 0 )
         ERROR_exit("Could not find string label: %s", cval);
      INFO_message("Found string label '%s' as volume [%d]", cval, ival);
   }

   if( ival >= DSET_NVALS(insetA) )
      ERROR_exit("Bad index (too large) for data volume: %d", ival);
   else if( ival < 0 ) {
      INFO_message("No extra data block input (via '-idat ..'): "
                   "using threshold dset for all computations and info.");
      if(CL_prefix)
       WARNING_message("... even though you specified an '-pref_dat ..', "
                         "no cluster-masked output can be made.");
      sprintf(repval, "(none)");
   }
   else {
      if ( DSET_HAS_LABEL(insetA, ival) )
         sprintf(repval, "[%d] '%s'", ival, DSET_BRICK_LAB(insetA, ival));
      else
         sprintf(repval, "[%d]", ival);
      INFO_message("Data volume:      %s", repval);
   }

   if( ithr < 0 || ithr >= DSET_NVALS(insetA) )
      ERROR_exit("Bad index for threshold volume: %d", ithr);
   else {
      if ( DSET_HAS_LABEL(insetA, ithr) )
         sprintf(repthr, "[%d] '%s'", ithr, DSET_BRICK_LAB(insetA, ithr));
      else
         sprintf(repthr, "[%d]", ithr);
      INFO_message("Threshold volume: %s", repthr);
   }

   // how many sides to this stat?  Check that user has appropriate
   // number of tails tested for it
   STAT_nsides = STAT_SIDES(DSET_BRICK_STATCODE(insetA, ithr));
   //INFO_message("How many sides to this stat? %d",
   //             STAT_nsides);
   if ( (STAT_nsides < 2) && ( abs(thr_type) >=2 ) )
      ERROR_exit("You are asking for multisided clustering on a "
                 "single-sided stat!");
   else if( (STAT_nsides < 2) && (thr_type == -1) )
      ERROR_exit("The Glen Rule applies here: \n\t"
                 "Can't run a left-sided test on this stat, as "
                 "it only has one (positive) side.");

   if ( !NNTYPE ) 
      ERROR_exit("Need to choose a neighborhood type: '-NN ?'");

   // ****************************************************************
   // ****************************************************************
   //                    pre-stuff, make storage
   // ****************************************************************
   // ****************************************************************

   // ----------- convert p to dset stat, if necessary ------------

   // ALL THIS JUST TO MAKE J. RAJENDRA HAPPY!!!
   if( !STATINPUT ) {
      tmpb = thb;
      tmpt = tht;

      if( thr_type == -1 ) {// 1sided, left; factor of 2 for 1sided conv
         tht = - THD_pval_to_stat( 2*tht,
                                   DSET_BRICK_STATCODE(insetA, ithr),
                                   DSET_BRICK_STATAUX(insetA, ithr));
         INFO_message("Converted left-tail p=%f -> stat=%f", tmpt, tht);
         sprintf(repstr, "left-tail p=%f -> stat=%f", tmpt, tht);
         
      }
      else if( thr_type == 1 ) {// 1sided, right; factor of 2 for 1sided conv
         thb = THD_pval_to_stat( 2*thb,
                                 DSET_BRICK_STATCODE(insetA, ithr),
                                 DSET_BRICK_STATAUX(insetA, ithr));
         INFO_message("Converted right-tail p=%f -> stat=%f", tmpb, thb);
         sprintf(repstr, "right-tail p=%f -> stat=%f", tmpb, thb);
      }
      else {// adjust all of bisided, 2sided, and within range
         thb = THD_pval_to_stat( thb,
                                 DSET_BRICK_STATCODE(insetA, ithr),
                                 DSET_BRICK_STATAUX(insetA, ithr));
         tht = - THD_pval_to_stat( tht,
                                   DSET_BRICK_STATCODE(insetA, ithr),
                                   DSET_BRICK_STATAUX(insetA, ithr));
         INFO_message("Converted left-tail p=%f -> stat=%f", tmpt, tht);
         INFO_message("Converted right-tail p=%f -> stat=%f", tmpb, thb);
         sprintf(repstr, "left-tail p=%f -> stat=%f;  "
                 "right-tail p=%f -> stat=%f", tmpt, tht, tmpb, thb);
      }
   }

   // final label, combining opts; for cluster map output
   strcat(blab1, blab2);
   strcat(blab1, blab3);
   INFO_message("Opt code: %s", blab1);

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
      if( CL_maskout ) {
         WARNING_message("And ANOTHER warning, since you also apparently\n"
                         "\tasked to output a mask volume, without providing\n"
                         "\tany means to procure mask info.  See help file.");
         // ... and unset the masking output
         free(CL_maskout);
         CL_maskout = NULL;
      }
	}

   if( !no_inmask && !mask )
      ERROR_exit("User asked to use a mask from a header, but \n"
                 "\tthere doesn't appear to be one stored.");
 
   // apply masks, *IF* one was input somehow. Does nada if mask==NULL
   if( mask ) {
      //INFO_message("Applying mask.");
      mri_maskify( DSET_BRICK(insetA, ithr), mask );
      if( ival >= 0 )
         mri_maskify( DSET_BRICK(insetA, ival), mask );
   }

   // ---------- make intermed dsets and pointers -------------

   // basically copying afni_vedit.c's version of clusterizing
   // stuff with data volume
   dblk = insetA->dblk;

   if( ival >= 0 ) {
      dim = DBLK_BRICK(dblk,ival);
      if( dim == NULL || mri_data_pointer(dim) == NULL ) 
         ERROR_exit("Bad properties of dset to be clusterized");
   }

   // stuff with threshold volume
   tim = DBLK_BRICK(dblk,ithr) ;

   if( DSET_BRICK_FACTOR(insetA, ithr) > 0.0f ) {
      thb /= DSET_BRICK_FACTOR(insetA, ithr);
      tht /= DSET_BRICK_FACTOR(insetA, ithr);
   }

   // [PT: Aug 10, 2018] Check 'istatfunc' in a new way, which
   // protects against having an error if the ithr volume is *not* a
   // stat.
   // OLD: istatfunc = dblk->brick_statcode[ithr];
   istatfunc = DBLK_BRICK_STATCODE(dblk, ithr);

   // build string of supplementary pars for report; check whether the
   // ithr volume is, indeed, a stat.
   if ( istatfunc != ILLEGAL_TYPE ) {
      if( FUNC_need_stat_aux[istatfunc] > 0 ) {
         sprintf(repstat, "%s :", FUNC_label_stat_aux[istatfunc]); 
         for( i=0 ; i < FUNC_need_stat_aux[istatfunc] ; i++ ) {
            sprintf(rrstr," %g ", DBLK_BRICK_STATPAR(dblk, ithr, i));
            strcat(repstat, rrstr);
         }
      }
   }
   else {
      INFO_message( "Threshold volume [%d] does *not* appear to be a stat!",
                    ithr);
      strcat(repstat, "not a stat!");
   }
         
   // ------- what dsets get clusterized and reportized?
   // ------- IF: the user input an additional dataset, then that;
   // ------- ELSE: all depends on the statistics

   if( ival < 0 ) { // i.e., no data set input: use only thr dset
      cim = mri_copy(tim); 
      CL_mritype = strdup(MRI_TYPE_name[ DSET_BRICK_TYPE(insetA, ithr) ]);
      }
   else { // use extra dset
      cim = mri_copy(dim); 
      CL_mritype = strdup(MRI_TYPE_name[ DSET_BRICK_TYPE(insetA, ival) ]);
      }

   car = mri_data_pointer(cim);
   if( car == NULL ){ 
      mri_free(cim) ;
      ERROR_exit("Badness in first internal copy");
   }

   if( (thr_type==-2 ) ) { // in case of bisidedness
      if( ival < 0 ) // i.e., no data set input: use only thr dset
         cim2 = mri_copy(tim); 
      else // use extra dset
         cim2 = mri_copy(dim); 

      car2 = mri_data_pointer(cim2);
      if( car2 == NULL ){ 
         mri_free(cim2) ;
         ERROR_exit("Badness in second internal copy");
      }
   }

   // ---------- somewhat odd but nec quants ----------------

   cim->dx = fabs(DSET_DX(insetA));
   cim->dy = fabs(DSET_DY(insetA));
   cim->dz = fabs(DSET_DZ(insetA));

   nx    = insetA->daxes->nxx;
   ny    = insetA->daxes->nyy;
   nz    = insetA->daxes->nzz;
   nxy   = nx * ny; 
   nxyz = nxy * nz;
   dx = fabs(insetA->daxes->xxdel);
   dy = fabs(insetA->daxes->yydel);
   dz = fabs(insetA->daxes->zzdel);
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
   
   //INFO_message("Thresholding...");

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

   //INFO_message("Obtaining suprathreshold clusters...");

   // Now apply cluster size threshold
   INIT_CLARR(clbig);

   if( clar ) {
      for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
         cl = clar->clar[iclu];
         if( cl != NULL && cl->num_pt >= ptmin ){ // big enough 
            ADDTO_CLARR(clbig,cl);                // copy pointer 
            clar->clar[iclu] = NULL;              // null out original 
         }
      }
      DESTROY_CLARR(clar);
   }

   // also add in other side, if bisided ON; and if any clusters were
   // found in other side
   if ( (thr_type == -2) && clar2 ) { 
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
      WARNING_message("No clusters found!");
      printf("%s** NO CLUSTERS FOUND ***\n", c1d);
      if( AFNI_yesenv("AFNI_3dclust_report_zero") ) printf(" 0\n");
      if( clar != NULL ) DESTROY_CLARR(clar);

      // [PT: June 1, 2018] will still output a volume if outvol if no
      // clust were found, IF the user ask for it.
      // Here, make array of cluster ROIs
      if ( OUTVOL_IF_NO_CLUST )
          mmm = (short *) calloc(sizeof(short), nxyz);
   }
   else {
      FOUND_CLUSTERS = 1;

      // Sort clusters by size
      if( clar->num_clu < 3333 ) {
         INFO_message("Sorting clusters by size.");
         SORT_CLARR(clar);
      } 
      else if( CL_summarize != 1 ){
         printf("%s** TOO MANY CLUSTERS TO SORT BY VOLUME ***\n", c1d) ;
      }
      
      // --------------- make array of cluster ROIs
      
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
   }

   // --------------- write out copy of the data volume, if asked

   if( CL_prefix && (ival >= 0) && 
       ( FOUND_CLUSTERS || OUTVOL_IF_NO_CLUST) ) {

      INFO_message("Writing out dataset masked by clusters.");
      
      THD_3dim_dataset *dset=NULL;
      
      // copy the single brick we will output
      dset = THD_copy_one_sub( insetA, ival );
           
      switch( DSET_BRICK_TYPE(dset, 0) ) {
         
      case MRI_byte: {
         byte *bar = (byte *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii] = 0;
      }
         break;

      case MRI_short: {
         short *bar = (short *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii] = 0;
      }
         break;

      case MRI_int: {
         int *bar = (int *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii] = 0;
      }
         break;

      case MRI_float: {
         float *bar = (float *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii] = 0.;
      }
         break;

      case MRI_double: {
         double *bar = (double *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii] = 0.;
      }
         break;

      case MRI_complex: {
         complex *bar = (complex *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[ii].r = bar[ii].i = 0.0;
      }
         break;

      case MRI_rgb: {
         byte *bar = (byte *) DSET_ARRAY(dset, 0);
         for( ii=0 ; ii < nxyz ; ii++ )
            if( mmm[ii] == 0 ) bar[3*ii] = bar[3*ii+1] = bar[3*ii+2] = 0;
      }
         break;

      default:
         ERROR_exit("Unrecognized data file type to write out??");

      }

      EDIT_dset_items( dset ,
                       ADN_prefix, CL_prefix,
                       ADN_none );

      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(dset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(dset));

      tross_Copy_History( insetA , dset );
      tross_Make_History( "3dClusterize", argc, argv, dset );
      DSET_write(dset); 
      WROTE_DSET(dset); 
      DSET_delete(dset);

   }

   // --------------- write out map of cluster ROIs

   if ( FOUND_CLUSTERS || OUTVOL_IF_NO_CLUST ) {
      
      if ( prefix ) {
         INFO_message("Writing out map of cluster ROIs.");
         
         THD_3dim_dataset *qset=NULL;
         qset = EDIT_empty_copy(insetA);
         
         EDIT_dset_items( qset ,
                          ADN_prefix , prefix ,
                          ADN_nvals  , 1 ,
                          ADN_none );

         if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(qset)) )
            ERROR_exit("Can't overwrite existing dataset '%s'",
                       DSET_HEADNAME(qset));

         EDIT_substitute_brick(qset, 0, MRI_short, mmm); 
         mmm = NULL;
         
         // useful props: integer colormap and brick labelling
         qset->int_cmap = INT_CMAP;
         EDIT_BRICK_LABEL(qset, 0, blab1);
         
         tross_Copy_History( insetA , qset ) ;
         tross_Make_History( "3dClusterize", argc , argv , qset );
         DSET_write(qset); WROTE_DSET(qset); DSET_delete(qset);
      }

   }

   // ---------- write out report --------------------
   if ( FOUND_CLUSTERS ) { // still only if clusters found

      do_mni = (CL_do_mni && insetA->view_type == VIEW_TALAIRACH_TYPE);
      THD_coorder_fill( my_getenv("AFNI_ORIENT") , &CL_cord);
      if( CL_do_mni )
         THD_coorder_fill( "LPI", &CL_cord );

      if( !CL_quiet ) {
      
         if( CL_summarize != 1 ) {
            printf( 
                   "%s\n"
                   //"%sCluster report for file %s %s\n"
                   "%s  Cluster report \n"
                   "%s[ Dataset prefix      = %s ]\n"
                   "%s[ Threshold vol       = %s ]\n"
                   "%s[ Supplement dat vol  = %s ]\n"
                   "%s[ Option summary      = %s ]\n"
                   "%s[ Threshold value(s)  = %s ]\n"
                   "%s[ Aux. stat. info.    = %s ]\n"
                   "%s[ Nvoxel threshold    = %d;"
                   //"  Connectivity radius = %.2f mm;"
                   "  Volume threshold = %.3f ]\n"
                   "%s[ Single voxel volume = %.3f (microliters) ]\n"
                   "%s[ Neighbor type, NN   = %d ]\n"
                   "%s[ Voxel datum type    = %s ]\n"
                   "%s[ Voxel dimensions    = %.3f mm X %.3f mm X %.3f mm ]\n"
                   "%s[ Coordinates Order   = %s ]\n",
                   c1d,
                   c1d, //argv[iarg], do_mni ? "[MNI coords]" : "",
                   c1d, DSET_PREFIX(insetA),
                   c1d, repthr,
                   c1d, repval,
                   c1d, blab1,
                   c1d, repstr,
                   c1d, repstat,
                   c1d, ptmin, ptmin*dx*dy*dz , // !!!
                   c1d, dx*dy*dz,
                   c1d, NNTYPE,
                   c1d, CL_mritype,
                   c1d, dx,dy,dz,
                   c1d, CL_cord.orcode );

            // always fake voxel dims-- just confusing to put; only refers
            // to internal/calculation considerations.
            //printf("%s[Fake voxel dimen    = %.3f mm X %.3f mm X %.3f mm ]\n",
            //       c1d, dxf,dyf,dzf);
         
            if( mask && nmask ==0 ) 
               printf("%s[ Mask                = %s ]\n", c1d, repmask);
            else if( !no_inmask && mask != NULL )           
               printf("%s[ Mask                = %s ]\n", c1d, repmask);
            else if( nmask > 0 )
               printf("%s[ Mask                = (skipping internal) ]\n", c1d);
            else if( nmask < 0 ) // shd not happen
               printf("%s[ Mask                = (un-usable internal) ]\n", c1d);
         
            if (CL_noabs)
               printf ("%s[ Mean and SEM based on "
                       "signed voxel intensities ]\n%s\n", c1d, c1d);
            else
               printf ("%s Mean and SEM based on absolute value "
                       "of voxel intensities ]\n%s\n", c1d, c1d);

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
      
         for( ipt=0; ipt < cl->num_pt; ipt++ ) {

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
   
   }

   // --------------- write out utilized mask

   if( CL_maskout && mask ) {

      INFO_message("Writing out whole brain mask.");
      
      THD_3dim_dataset *mset=NULL;
      mset = EDIT_empty_copy(insetA);
      EDIT_dset_items( mset ,
                       ADN_prefix , CL_maskout ,
                       ADN_nvals  , 1 ,
                       ADN_none );

      if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(mset)) )
         ERROR_exit("Can't overwrite existing dataset '%s'",
                    DSET_HEADNAME(mset));

      EDIT_substitute_brick(mset, 0, MRI_byte, mask); 
      mask = NULL;
      
      tross_Copy_History( insetA , mset ) ;
      tross_Make_History( "3dClusterize", argc , argv , mset );
      DSET_write(mset); 
      WROTE_DSET(mset); 
      DSET_delete(mset);
   }

   // ************************************************************
   // ************************************************************
   //                    Freeing
   // ************************************************************
   // ************************************************************
	
   //INFO_message("Cleaning");

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

   //INFO_message("Done!");

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

int CheckStringStart( char *a, char *b, char *c)
{ // look for string 'b' in 'a'; if it is at the start of it, then
  // chop it off and make 'c' be the remainder; if 'b' is not in 'a',
  // then just have 'c' be a copy of 'a'; else, whine.
   int i;
   int la, lb;
   char *d="tmp";

   la = strlen(a);
   lb = strlen(b);

   d = strstr(a,b);

   // if b is NOT in a, c is null
   if( !d ) {
      strcpy(c, a);
      INFO_message("User input stat threshold: %s", c);

      return 1;
   }
   else if( d != a ) {
      WARNING_message("Wrong threshold input format!");
      strcpy(c, a);
      return -1;
   }

   // else, push c to the end of the searched for string to get
   // remainder
   strcpy(c, a+lb);

   INFO_message("User input p-value threshold: %s", c);

   return 0;
}

// -----------------------------------------------------------

int ssgn(float x)
{
   if(x>0)
      return 1;
   if(x<0)
      return -1;
   return 0;
}

// -----------------------------------------------------------

int MakeBrickLab1(char *c0, char *c1, char *c2, char *c3)
{ // string together strings to make a label; sep each with a comma

   strcat(c0, c1); 
   strcat(c0, ",");
   strcat(c0, c2); 
   strcat(c0, ",");
   strcat(c0, c3); 
   strcat(c0, ",");

   return 0;
}

int MakeBrickLab2(char *c0, char *c1, char *c2)
{ // string together strings to make a label; sep each with a comma

   strcat(c0, c1); 
   strcat(c0, ",");
   strcat(c0, c2); 
   strcat(c0, ",");

   return 0;
}

// -----------------------------------------------------------

int mycheck_is_int(char *x)
{
   int n, i=0;
   
   n = strlen(x);
   while( i<n && isdigit(x[i]) ) 
      i++;

   if( n-i ) 
      return 0;
   else 
      return 1;
}
