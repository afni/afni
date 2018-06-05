/* A ripped off version of 3dGrpInCorr, to see if the idea goes anywhere */


#include "mrilib.h"
#include "SUMA_suma.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*--- prototypes ---*/

double      GIC_student_t2z( double tt, double dof ) ;
float_pair  ttest_toz( int numx, float *xar, int numy, float *yar, int opcode );
void        GRINCOR_many_ttest( int nvec , int numx , float **xxar ,
                                int numy , float **yyar ,
                                float *dar , float *zar  ) ;

static int verb  = 1 ;  /* default verbosity level */
static int debug = 0 ;  /* default non-debug mode */

#undef  UINT32
#define UINT32 unsigned int  /* 20 May 2010 */
#undef  MAXCOV
#define MAXCOV 31

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             ) ;

void GRINCOR_many_regress( int nvec , int numx , float **xxar ,
                                      int numy , float **yyar ,
                                      int nout , float **dtar  ) ;

/*----------------------------------------------------------------------------*/

static int vstep_n = 0 ;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vstep_n%10] ) ;
   if( vstep_n%10 == 9) fprintf(stderr,".") ;
   vstep_n++ ;
}

/*----------------------------------------------------------------------------*/
void RestSymHelp(void) 
{
     printf(
      "Usage: RestSym [options]\n"
      "\n"
      "* Also see\n"
      "  https://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/afni20_instastuff.pdf\n"
      "\n"
      "* This program operates as a server for AFNI or SUMA.  It reads in dataset\n"
      "  collections that have been prepared by 3dSetupGroupInCorr, and then\n"
      "  connects to the AFNI or SUMA GUI program (via TCP/IP).  Then it waits\n"
      "  for a command to be sent from AFNI/SUMA before it actually does anything.\n"
      "\n"
      "* At the same time as you run RestSym, you also have to run the\n"
      "  AFNI GUI program, with a command like 'afni -niml'.  RestSym\n"
      "  by itself will only do something when AFNI sends it a command, which\n"
      "  you do by using the 'InstaCorr Set' button on the [A] image viewer\n"
      "  right-click popup menu, after RestSym has connected to AFNI.\n"
      "\n"
      "* When AFNI sends a seed voxel command, RestSym will extract\n"
      "  that voxel times series from each input dataset, will compute the\n"
      "  correlation map of each dataset with the corresponding seed time\n"
      "  series, then will compute the voxel-wise collection of t-tests of\n"
      "  that bunch of correlation maps, and return the resulting 3D volumes\n"
      "  to AFNI for display.\n"
      "\n"
      "* You must start AFNI with the '-niml' option to allow it to accept\n"
      "  incoming TCP/IP socket connections.\n"
      " ++ Or you can press the 'NIML+PO' button in the GUI, if you forgot\n"
      "    to type the AFNI command line correctly.\n"
      " ++ If you are running RestSym and AFNI on separate computers,\n"
      "    you also have to setup 'host trusting' correctly -- for details,\n"
      "    see the description of the '-ah' option, far below.\n"
      "\n"
      "* More detailed outline of processing in RestSym:\n"
      " ++ For each 3D+time dataset in the input dataset collections:\n"
      "   -- Extract the seed voxel time series (averaging locally per 'seedrad')\n"
      "        [you could do this manually with 3dmaskave]\n"
      "   -- Correlate it with all other voxel time series in the same dataset\n"
      "       [you could do this manually with 3dDeconvolve or 3dfim]\n"
      "   -- Result is one 3D correlation map per input dataset\n"
      " ++ Then carry out the t-test between/among these 3D correlation maps.\n"
      "   -- Actually, between the arctanh() of these maps;\n"
      "       cf. RA Fisher: http://en.wikipedia.org/Fisher_transformation\n"
      "       [you could do the arctanh() conversion manually with 3dcalc]\n"
      "       [and then do the t-test manually with 3dttest; then convert]\n"
      "       [the t-statistics to Z-scores with another run of 3dcalc   ]\n"
      " ++ The dataset returned to AFNI converts the t-statistic maps\n"
      "    to Z-scores, for various reasons of convenience.\n"
      "   -- The individual correlation maps that were t-test-ed are discarded.\n"
      "\n"
      "* When RestSym starts up, it has to 'page fault' all the data\n"
      "  into memory.  This can take several minutes, if it is reading (say)\n"
      "  10 Gbytes of data from a slow disk.  After that, if your computer\n"
      "  has enough RAM, then the program should run pretty quickly.\n"
#ifdef USE_OMP
      "\n"
      "* One reason this program is a server (rather than being built in\n"
      "  to AFNI) is that it is compiled to use OpenMP, which will let\n"
      "  it make use of multiple CPU cores on the computer system :-)\n"
      " ++ For more information, see the end of this '-help' output.\n"
#else
      "\n"
      "* One reason this program is a server (rather than being built in\n"
      "  to AFNI) is that it can be compiled to use OpenMP, which will let\n"
      "  it make use of multiple CPU cores on the computer system.\n"
      " ++ However, this binary version is not compiled with OpenMP :-(\n"
      " ++ OpenMP is supported in gcc 4.2 and above (included on Mac OS X),\n"
      "    and in some commercial C compilers (e.g., Sun's, Intel's).\n"
#endif
      "\n"
      "====================\n"
      "COMMAND LINE OPTIONS\n"
      "====================\n"
      "\n"
      "*** Input Files ***\n"
      "\n"
      " -setA AAA.grpincorr.niml\n"
      "   = Give the setup file (from 3dSetupGroupInCorr) that describes\n"
      "     the first dataset collection:\n"
      "  ++ This 'option' is MANDATORY (you have to input SOMETHING).\n"
      "  ++ Of course, 'AAA' should be replaced with the correct name of\n"
      "     your input dataset collection file!\n"
      "  ++ RestSym can use byte-valued or short-valued data as\n"
      "     produced by the '-byte' or '-short' options to 3dSetupGroupInCorr.\n"
      "  ++ You can also put the '.data' filename here, or leave off the '.niml';\n"
      "     the program will look for these cases and patch the filename as needed.\n"
      "\n"
      " -setB BBB.grpincorr.niml\n"
      "   = Give the setup file that describes the second dataset collection:\n"
      "  ++ This option IS optional.\n"
      "  ++ If you use only -setA, then the program computes a one-sample t-test.\n"
      "  ++ If you use also -setB, then the program computes a two-sample t-test.\n"
      "    -- The exact form of the 2-sample t-test used is controlled by one of the\n"
      "       three options described below (which are mutually exclusive).\n"
      "  ++ The sign of a two sample t-test is 'A-B'; that is, a positive result\n"
      "     means that the A set of correlations average larger than the B set.\n"
      "  ++ The output t-statistics are converted to Z-scores for transmission to AFNI,\n"
      "     using the same code as the 'fitt_t2z(t,d)' function in 3dcalc:\n"
      "    -- e.g, the output of the command\n"
      "          ccalc 'fitt_t2z(4,15)'\n"
      "       is 3.248705, showing that a t-statistic of 4 with 15 degrees-of-freedom\n"
      "       (DOF) has the same p-value as a Z-score [N(0,1) deviate] of 3.248705.\n"
      "    -- One reason for using Z-scores is that the DOF parameter varies between\n"
      "       voxels when you choose the -unpooled option for a 2-sample t-test.\n"
      "\n"
      " -labelA aaa = Label to attach (in AFNI) to sub-bricks corresponding to setA.\n"
      "               If you don't give this option, the label used will be the prefix\n"
      "               from the -setA filename.\n"
      " -labelB bbb = Label to attach (in AFNI) to sub-bricks corresponding to setB.\n"
      "              ++ At most the first 11 characters of each label will be used!\n"
      "\n"
      "*** Two-Sample Options ***\n"
      "\n"
      " -pooled   = For a two-sample un-paired t-test, use a pooled variance estimator\n"
      "            ++ This is the default, but it can be changed from the AFNI GUI.\n"
      " -unpooled = For a two-sample un-paired t-test, use an unpooled variance estimator\n"
      "            ++ Statistical power declines a little, and in return,\n"
      "               the test becomes a little more robust.\n"
      " -paired   = Use a two-sample paired t-test\n"
      "            ++ Which is the same as subtracting the two sets of 3D correlation\n"
      "               maps, then doing a one-sample t-test.\n"
      "            ++ To use '-paired', the number of datasets in each collection\n"
      "               must be the same, and the datasets must have been input to\n"
      "               3dSetupGroupInCorr in the same relative order when each\n"
      "               collection was created. (Duh.)\n"
#if 0
      " -nosix    = For a 2-sample situation, the program by default computes\n"
      "             not only the t-test for the difference between the samples,\n"
      "             but also the individual (setA and setB) 1-sample t-tests, giving\n"
      "             6 sub-bricks that are sent to AFNI.  If you don't want\n"
      "             these 4 extra 1-sample sub-bricks, use the '-nosix' option.\n"
#endif
      "   ++ None of these options means anything for a 1-sample t-test\n"
      "      (i.e., where you don't use -setB).\n"
      "\n"
      "*** Dataset-Level Covariates [26 May 2010] ***\n"
      "\n"
      " -covariates cf = Read file 'cf' that contains covariates values for each dataset\n"
      "                  input (in both -setA and -setB; there can only at most one\n"
      "                  -covariates option).  Format of the file\n"
      "     FIRST LINE -->   subject IQ   age\n"
      "     LATER LINES -->  Elvis   143   42\n"
      "                      Fred     85   59\n"
      "                      Ethel   109   49\n"
      "                      Lucy    133   32\n"
      "        This file format should be compatible with 3dMEMA.\n"
      "        ++ The first column contains the labels that must match the dataset\n"
      "            labels stored in the input *.grpincorr.niml files, which are\n"
      "            either the dataset prefixes or whatever you supplied in the\n"
      "            3dSetupGroupInCorr program via '-labels'.\n"
      "            -- If you ran 3dSetupGroupInCorr before this update, its output\n"
      "               .grpincorr.niml file will NOT have dataset labels included.\n"
      "               Such a file cannot be used with -covariates -- Sorry.\n"
      "        ++ The later columns contain numbers.\n"
      "        ++ The first line contains column headers.  The header label for the\n"
      "            first column isn't used for anything.  The later header labels are\n"
      "            used in the sub-brick labels sent to AFNI.\n"
      "        ++ At this time, only the -paired and -pooled options can be used with\n"
      "            covariates.  If you use -unpooled, it will be changed to -pooled.\n"
      "            -unpooled still works with a pure t-test (no -covariates option).\n"
      "            -- This restriction may be lifted in the future.\n"
      "        ++ If you use -paired, then the covariates for -setB will be the same\n"
      "            as those for -setA, even if the dataset labels are different!\n"
      "            -- This restriction may be lifted in the future.\n"
      "        ++ Each covariate column in the regression matrix will have its mean\n"
      "            removed (centered). If there are 2 sets of subjects, each set's\n"
      "            matrix will be centered separately.\n"
      "        ++ For each covariate, 2 sub-bricks are produced:\n"
      "            -- The estimated slope of arctanh(correlation) vs covariate\n"
      "            -- The Z-score of the t-statistic of this slope\n"
      "        ++ If there are 2 sets of subjects, then each pair of sub-bricks is\n"
      "            produced for the setA-setB, setA, and setB cases, so that you'll\n"
      "            get 6 sub-bricks per covariate (plus 6 more for the mean, which\n"
      "            is treated as a special covariate whose values are all 1).\n"
      "            -- At present, there is no way to tell RestSym not to send\n"
      "               all this information back to AFNI/SUMA.\n"
      "        ++ A maximum of 31 covariates are allowed.  If you have more, then\n"
      "            seriously consider the possibility that you are completely deranged.\n"
      "\n"
      "*** Other Options ***\n"
      "\n"
      " -seedrad r = Before performing the correlations, average the seed voxel time\n"
      "              series for a radius of 'r' millimeters.  This is in addition\n"
      "              to any blurring done prior to 3dSetupGroupInCorr.  The default\n"
      "              radius is 0, but the AFNI user can change this interactively.\n"
      "\n"
      " -ah host = Connect to AFNI/SUMA on the computer named 'host', rather than\n"
      "            on the current computer system 'localhost'.\n"
      "     ++ This allows RestSym to run on a separate system than\n"
      "        the AFNI GUI.\n"
      "       -- e.g., If your desktop is weak and pitiful, but you have access\n"
      "          to a strong and muscular multi-CPU server (and the network\n"
      "          connection is fast).\n"
      "     ++ Note that AFNI must be setup with the appropriate\n"
      "        'AFNI_TRUSTHOST_xx' environment variable, so that it will\n"
      "        allow the external socket connection (for the sake of security):\n"
      "      -- Example: AFNI running on computer 137.168.0.3 and RestSym\n"
      "         running on computer 137.168.0.7\n"
      "      -- Start AFNI with a command like\n"
      "           afni -DAFNI_TRUSTHOST_01=137.168.0.7 -niml ...\n"
      "      -- Start RestSym with a command like\n"
      "           RestSym -ah 137.168.0.3 ...\n"
      "      -- You may use hostnames in place of IP addresses, but numerical\n"
      "         IP addresses may work more reliably.\n"
      "      -- If you are very trusting, you can set NIML_COMPLETE_TRUST to YES\n"
      "         to allow NIML socket connections from anybody. (This only affects\n"
      "         AFNI programs, not any other software on your computer.)\n"
      "      -- You might also need to adjust your firewall settings to allow\n"
      "         the reception of TCP/IP socket connections from outside computers.\n"
      "         Firewalls are a separate issue from setting up AFNI host 'trusting',\n"
      "         and the mechanics of how you can setup your firewall permissions is\n"
      "         not something about which we can give you advice.\n"
      "\n"
      " -np port = Connect to AFNI/SUMA using the TCP/IP port number given here,\n"
      "            rather than the default port number [53212 for AFNI, 53224 for\n"
      "            SUMA].  You must give the corresponding option to AFNI to\n"
      "            get proper communication going.  Using '-np' properly is the\n"
      "            only way to have multiple copies of RestSym and AFNI\n"
      "            talking to each other!\n"
#ifndef DONT_USE_SHM
      "\n"
      " -NOshm = Do NOT reconnect to AFNI using shared memory, rather than TCP/IP,\n"
      "          when using 'localhost' (i.e., AFNI and RestSym are running\n"
      "          on the same system).\n"
      "       ++ The default is to use shared memory for communication when\n"
      "          possible, since this method of transferring large amounts of\n"
      "          data between programs on the same computer is much faster.\n"
      "       ++ If you have a problem with the shared memory communication,\n"
      "          use '-NOshm' to use TCP/IP for all communications.\n"
      "       ++ If you use '-VERB', you will get a very detailed progress report\n"
      "          from RestSym as it computes, including elapsed times for\n"
      "          each stage of the process.\n"
#endif
      "\n"
      " -quiet = Turn off the 'fun fun fun in the sun sun sun' informational messages\n"
      " -verb  = Print out extra informational messages for more fun\n"
      " -VERB  = Print out even more informational messages for even more fun!\n"
#ifdef isfinite
      " -debug = Do some internal testing (slows things down a little)\n"
#endif
      "\n"
      "-------============= Talairach (+trlc) vs. Original (+orig) =============-------\n"
      "\n"
      "Normally, AFNI assigns the dataset sent by RestSym to the +tlrc view.\n"
      "However, you can tell AFNI to assign it to the +orig view instead.\n"
      "To do this, set environment variable AFNI_GROUPINCORR_ORIG to YES when\n"
      "starting AFNI; for example:\n"
      "\n"
      "  afni -DAFNI_GROUPINCORR_ORIG=YES -niml\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("RestSym",NULL) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }
/* Binary search for tt in a sorted integer array. */

static int mybsearch_int( int tt , int nar , int *ar )
{
   int targ , ii , jj , kk , nn ;

   if( nar <= 0 || ar == NULL ) return -1 ; /* bad inputs */

   targ = tt ; ii = 0 ; jj = nar-1 ;      /* setup */

        if( targ <  ar[0]  ) return -1 ;  /* not found */
   else if( targ == ar[0]  ) return  0 ;  /* at start! */

        if( targ >  ar[jj] ) return -1 ;  /* not found */
   else if( targ == ar[jj] ) return jj ;  /* at end!   */

   /* at the start of this loop, we've already checked
      indexes ii and jj, so check the middle of them (kk),
      and if that doesn't work, make the middle the
      new ii or the new jj -- so again we will have
      checked both ii and jj when the loop iterates back. */

   while( jj-ii > 1 ){
     kk = (ii+jj) / 2 ;         /* midpoint */
     nn = ar[kk] - targ ;       /* sign of difference */
     if( nn == 0 ) return kk ;  /* found it! */
     if( nn <  0 ) ii = kk ;    /* must be above kk */
     else          jj = kk ;    /* must be below kk */
   }

   return -1 ;
}

/*--------------------------------------------------------------------------*/

static int string_search( char *targ , int nstr , char **str )
{
   int ii ;

   if( targ == NULL || *targ == '\0' || str == NULL || nstr < 1 ) return -1 ;

   for( ii=0 ; ii < nstr ; ii++ )
     if( str[ii] != NULL && strcmp(targ,str[ii]) == 0 ) return ii ;

   return -1 ;
}

/*--------------------------------------------------------------------------*/

typedef struct {

  int nvec  ;  /* number of vectors in a dataset */
  int ndset ;  /* number of datasets */
  int *nvals ; /* nvals[i] = number of values in a vector in i-th dataset */
  int datum ;  /* 1 for sbyte, 2 for short */

  int nuse , *use ;  /* 07 Apr 2010: subset of datasets to use */

  char *geometry_string ;
  THD_3dim_dataset *tdset ; /* template dataset */
  int nx,ny,nz,nvox ;       /* number of nodes in template */

  char *dfname ;  /* data file name */
  int  *ivec   ;  /* ivec[i] = spatial index of i-th vector, i=0..nvec-1 */
  float *fac   ;  /* fac[i] = scale factor for i-th dataset, i=0..ndset-1 */
  short **sv   ;  /* sv[i] = short array [nvals[i]*nvec] for i-th dataset */
  sbyte **bv   ;  /* bv[i] = sbyte array [nvals[i]*nvec] for i-th dataset */

  char **dslab ;  /* dslab[i] = label string for i-th dataset [23 May 2010] */

  long long nbytes ;  /* number of bytes in the data array */

  /* Surface stuff  ZSS Jan 09 2010 */

  int nnode[2]   ;
  int ninmask[2] ;

} MRI_shindss ;  /* short/sbyte indexed datasets */

/*----- get index in array, given voxel ijk value -----*/

#undef  IJK_TO_INDEX
#define IJK_TO_INDEX(shd,ijk)      \
  ( ((shd)->ivec == NULL) ? (ijk)  \
                          : mybsearch_int((ijk),(shd)->nvec,(shd)->ivec) )

/*--------------------------------------------------------------------------*/

#undef  GQUIT
#define GQUIT(sss)                                                \
 do{ if( tdset != NULL ) DSET_delete(tdset) ;                     \
     if( dfname != NULL ) free(dfname) ;                          \
     if( geometry_string != NULL ) free(geometry_string) ;        \
     NI_free_element(nel) ;                                       \
     if( sss != NULL ) ERROR_message("file %s: %s",fname,(sss)) ; \
     return(NULL) ;                                               \
 } while(0)

/*--------------------------------------------------------------------------*/

static const long long twogig = 2ll * 1024ll * 1024ll * 1024ll ;  /* 2 GB */

/*----- read a PREFIX.grpincorr.niml file into a struct -----*/

MRI_shindss * GRINCOR_read_input( char *fname )
{
   NI_element *nel=NULL ;
   char *dfname=NULL , *atr ;
   NI_float_array *facar ; NI_int_array *nvar, *nnode=NULL, *ninmask=NULL;
   MRI_shindss *shd ;
   long long nbytes_needed , nbytes_dfname ; int fdes ;
   void *var ; int ids ;
   int datum , datum_size ;

   char *geometry_string=NULL ;
   THD_3dim_dataset *tdset=NULL; int nvox;
   int no_ivec=0 , *ivec=NULL , *nvals=NULL , nvec,ndset ; float *fac=NULL ;
   NI_str_array *slabar=NULL ;

   if( fname == NULL || *fname == '\0' ) GQUIT(NULL) ;

   /* get data element */

   nel = NI_read_element_fromfile(fname) ;
   if( nel == NULL || nel->type != NI_ELEMENT_TYPE )
     GQUIT("not properly formatted") ;
   if( strcmp(nel->name,"3dGroupInCorr") != 0 )
     GQUIT("data element name is not '3dGroupInCorr'") ;

   /* no data vector ==> using all voxels */

   no_ivec = ( nel->vec_num < 1 ||
               nel->vec_len < 1 || nel->vec_typ[0] != NI_INT ) ;

   /* number of vectors in each dataset */

   atr = NI_get_attribute(nel,"nvec");
   if( atr == NULL ) GQUIT("nvec attribute missing?") ;
   nvec = (int)strtod(atr,NULL) ;
   if( nvec < 2 || (!no_ivec && nel->vec_len != nvec) )
     GQUIT("nvec attribute has illegal value") ;

   /* number of datasets */

   atr = NI_get_attribute(nel,"ndset");
   if( atr == NULL ) GQUIT("ndset attribute missing") ;
   ndset = (int)strtod(atr,NULL) ;
   if( ndset < 1 ) GQUIT("ndset attribute has illegal value") ;

   /* number of time points in each dataset (varies with dataset) */

   atr = NI_get_attribute(nel,"nvals");
   if( atr == NULL ) GQUIT("nvals attribute missing") ;
   nvar = NI_decode_int_list(atr,",") ;
   if( nvar == NULL || nvar->num < ndset )
     GQUIT("nvals attribute doesn't match ndset") ;
   nvals = nvar->ar ; nvar->ar = NULL ; NI_delete_int_array(nvar) ;

   /* dataset labels [23 May 2010] */

   atr = NI_get_attribute(nel,"dset_labels") ;
   if( atr != NULL ){
     slabar = NI_decode_string_list(atr,";,") ;
     if( slabar == NULL || slabar->num < ndset )
       GQUIT("dset_labels attribute invalid") ;
   }

   /* datum of datasets */

   atr = NI_get_attribute(nel,"datum") ;
   if( atr != NULL && strcasecmp(atr,"byte") == 0 ){
     datum = 1 ; datum_size = sizeof(sbyte) ;
   } else {
     datum = 2 ; datum_size = sizeof(short) ;
   }

   /* number of bytes needed:
        sizeof(datum) * number of vectors per dataset
                      * number of datasets
                      * sum of per dataset vector lengths */

   nbytes_needed = 0 ;
   for( ids=0 ; ids < ndset ; ids++ ) nbytes_needed += nvals[ids] ;
   nbytes_needed *= ((long long)nvec) * datum_size ;

   if( nbytes_needed >= twogig &&
       ( sizeof(void *) < 8 || sizeof(size_t) < 8 ) ) /* too much for 32-bit */
     GQUIT("datafile size exceeds 2 GB -- you need a 64-bit computer!") ;

   /* scale factor for each dataset */

   atr = NI_get_attribute(nel,"fac") ;
   if( atr == NULL ) GQUIT("fac attribute missing") ;
   facar = NI_decode_float_list(atr,",") ;
   if( facar == NULL || facar->num < ndset )
     GQUIT("can't decode fac attribute") ;
   fac = facar->ar ; facar->ar = NULL ; NI_delete_float_array(facar) ;

   for( ids=0 ; ids < ndset ; ids++ ) if( fac[ids] <= 0.0f ) fac[ids] = 1.0f ;

   /* grid definition */

   atr = NI_get_attribute(nel,"geometry") ;
   if( atr == NULL ) GQUIT("geometry attribute missing") ;
   geometry_string = strdup(atr) ;
   tdset = EDIT_geometry_constructor( geometry_string , "GrpInCorr" ) ;
   if( tdset == NULL ) GQUIT("can't decode geometry attribute") ;
   nvox = DSET_NVOX(tdset) ;
   if(  no_ivec && nvox != nvec )
     GQUIT("geometry attribute doesn't match nvec attribute") ;
   if( !no_ivec && nvox <  nvec )
     GQUIT("geometry attribute specifies too few voxels") ;

   /* name of data file: check its size against what's needed */

   atr = NI_get_attribute(nel,"datafile") ;
   if( atr == NULL ) GQUIT("datafile attribute missing") ;
   dfname = strdup(atr) ; nbytes_dfname = THD_filesize(dfname) ;
   if( nbytes_dfname <= 0 )
     GQUIT("datafile is missing") ;
   else if( nbytes_dfname < nbytes_needed ){
     char str[2048] ;
     sprintf(str,"datafile has %s bytes but needs at least %s",
              commaized_integer_string(nbytes_dfname) ,
              commaized_integer_string(nbytes_needed) ) ;
     GQUIT(str) ;
   }
   fdes = open( dfname , O_RDONLY ) ;
   if( fdes < 0 ) GQUIT("can't open datafile") ;

   /* ivec[i] is the voxel spatial index of the i-th vector */

   if( no_ivec ){
     ivec = NULL ;  /* means all voxels: ivec[i] == i */
   } else {
     ivec = (int *)nel->vec[0] ; /* copy pointer */
     nel->vec[0] = NULL ;        /* NULL out in element so won't be free-ed */
   }

   /* And stuff for LR surface pairs      ZSS Jan 09*/
   if ((atr=NI_get_attribute(nel,"LRpair_nnode"))) {
      nnode = NI_decode_int_list(atr,",") ;
   }
   if ((atr=NI_get_attribute(nel,"LRpair_ninmask"))) {
      ninmask = NI_decode_int_list(atr,",") ;
   }

   NI_free_element(nel) ;  /* don't need this anymore */

   /* create output struct */

   shd = (MRI_shindss *)malloc(sizeof(MRI_shindss)) ;

   shd->nvals = nvals ;
   shd->nvec  = nvec  ;
   shd->ndset = ndset ;

   shd->geometry_string = geometry_string ;
   shd->tdset           = tdset ;
   shd->dfname          = dfname ;
   shd->nvox            = nvox ;
   shd->nx = DSET_NX(tdset); shd->ny = DSET_NY(tdset); shd->nz = DSET_NZ(tdset);

   shd->ivec = ivec ;
   shd->fac  = fac  ;

   /* and surface fields...      ZSS      Jan 09 */
   if (nnode) {
      if (nnode->num != 2) GQUIT("LRpair_nnode must have 2 values");
      shd->nnode[0] = nnode->ar[0];
      shd->nnode[1] = nnode->ar[1];
      NI_delete_int_array(nnode); nnode=NULL;
   } else {
      shd->nnode[0] = shd->nnode[1] = -1 ;
   }
   if (ninmask) {
      if (ninmask->num != 2) GQUIT("LRpair_ninmask must have 2 values");
      shd->ninmask[0] = ninmask->ar[0];
      shd->ninmask[1] = ninmask->ar[1];
      NI_delete_int_array(ninmask); ninmask=NULL;
   } else {
      shd->ninmask[0] = shd->ninmask[1] = -1 ;
   }

   /*--- 07 Apr 2010: setup default use list (all of them) ---*/

   shd->nuse = ndset ;
   shd->use  = (int *)malloc(sizeof(int)*ndset) ;
   for( ids=0 ; ids < ndset ; ids++ ) shd->use[ids] = ids ;

   shd->dslab = (slabar != NULL) ? slabar->str : NULL ;  /* 23 May 2010 */

   /*--- now have to map data from disk ---*/

   var = mmap( 0 , (size_t)nbytes_needed ,
                   PROT_READ , THD_MMAP_FLAG , fdes , 0 ) ;
   close(fdes) ;  /* close file descriptor does not unmap data */

   if( var == (void *)(-1) ){ /* this is bad */
     ERROR_message(
       "file %s: can't mmap() datafile -- memory space exhausted?" , dfname ) ;
     free(shd) ; return NULL ;
   }

   /*-- create array of pointers to each dataset's data array --*/

   shd->datum = datum ;

   if( datum == 2 ){  /* shorts */
     shd->sv    = (short **)malloc(sizeof(short *)*ndset) ;
     shd->bv    = NULL ;
     shd->sv[0] = (short *)var ;
     for( ids=1 ; ids < ndset ; ids++ )
       shd->sv[ids] = shd->sv[ids-1] + nvals[ids-1]*nvec ;
   } else {           /* sbytes */
     shd->sv    = NULL ;
     shd->bv    = (sbyte **)malloc(sizeof(sbyte *)*ndset) ;
     shd->bv[0] = (sbyte *)var ;
     for( ids=1 ; ids < ndset ; ids++ )
       shd->bv[ids] = shd->bv[ids-1] + nvals[ids-1]*nvec ;
   }

   shd->nbytes = nbytes_needed ;
   return shd ;
}

#undef GQUIT

/*--------------------------------------------------------------------------*/

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

#define UNROLL  /* to speed things up in the correlation inner loop */

/*--------------------------------------------------------------------------*/
/* This cute little function consumes a lot of CPU time. */

void GRINCOR_dotprod_short( MRI_shindss *shd, int ids, float *vv, float *dp )
{
   int nvec = shd->nvec , nvals = shd->nvals[ids] , iv,ii ;
   float sum , fac = shd->fac[ids]*0.9999f ;
   short *sv = shd->sv[ids] , *svv ;

#ifndef UNROLL
   for( iv=0 ; iv < nvec ; iv++ ){
     svv = sv + iv*nvals ;
     for( sum=0.0f,ii=0 ; ii < nvals ; ii++ ) sum += vv[ii]*svv[ii] ;
     sum *= fac ; dp[iv] = MYatanh(sum) ;
   }
#else
   if( nvals%2 == 0 ){  /* even number of samples */
     for( iv=0 ; iv < nvec ; iv++ ){
       svv = sv + iv*nvals ; sum = 0.0f ;
       for( ii=0 ; ii < nvals ; ii+=2 ) sum += vv[ii]*svv[ii] + vv[ii+1]*svv[ii+1] ;
       sum *= fac ; dp[iv] = MYatanh(sum) ;
     }
   } else {             /* odd number of samples */
     for( iv=0 ; iv < nvec ; iv++ ){
       svv = sv + iv*nvals ; sum = vv[0]*svv[0] ;
       for( ii=1 ; ii < nvals ; ii+=2 ) sum += vv[ii]*svv[ii] + vv[ii+1]*svv[ii+1] ;
       sum *= fac ; dp[iv] = MYatanh(sum) ;
     }
   }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/
/* This cute little function consumes a lot of CPU time. */

void GRINCOR_dotprod_sbyte( MRI_shindss *shd, int ids, float *vv, float *dp )
{
   int nvec = shd->nvec , nvals = shd->nvals[ids] , iv,ii ;
   float sum , fac = shd->fac[ids]*0.9999f ;
   sbyte *bv = shd->bv[ids] , *bvv ;

#ifndef UNROLL
   for( iv=0 ; iv < nvec ; iv++ ){
     bvv = bv + iv*nvals ;
     for( sum=0.0f,ii=0 ; ii < nvals ; ii++ ) sum += vv[ii]*bvv[ii] ;
     sum *= fac ; dp[iv] = MYatanh(sum) ;
   }
#else
   if( nvals%2 == 0 ){  /* even number of samples */
     for( iv=0 ; iv < nvec ; iv++ ){
       bvv = bv + iv*nvals ; sum = 0.0f ;
       for( ii=0 ; ii < nvals ; ii+=2 ) sum += vv[ii]*bvv[ii] + vv[ii+1]*bvv[ii+1] ;
       sum *= fac ; dp[iv] = MYatanh(sum) ;
     }
   } else {             /* odd number of samples */
     for( iv=0 ; iv < nvec ; iv++ ){
       bvv = bv + iv*nvals ; sum = vv[0]*bvv[0] ;
       for( ii=1 ; ii < nvals ; ii+=2 ) sum += vv[ii]*bvv[ii] + vv[ii+1]*bvv[ii+1] ;
       sum *= fac ; dp[iv] = MYatanh(sum) ;
     }
   }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/

void GRINCOR_dotprod( MRI_shindss *shd, int ids, float *vv, float *dp )
{
  if( shd->datum == 1 ) GRINCOR_dotprod_sbyte( shd, ids, vv, dp ) ;
  else                  GRINCOR_dotprod_short( shd, ids, vv, dp ) ;
  return ;
}

/*--------------------------------------------------------------------------*/

void GRINCOR_many_dotprod( MRI_shindss *shd , float **vv , float **ddp )
{

#pragma omp parallel
 { int ids , ndset=shd->ndset ;
   AFNI_OMP_START ;
#pragma omp for
   for( ids=0 ; ids < ndset ; ids++ ){
     if( verb > 3 ) fprintf(stderr," +   start correlation on dataset #%d\n",ids) ;
     GRINCOR_dotprod( shd , ids , vv[ids] , ddp[ids] ) ;
   }
   AFNI_OMP_END ;
 }

#ifdef isfinite
   if( debug ){
     int nvec=shd->nvec , nbad , iv , ids ;
     for( ids=0 ; ids < shd->ndset ; ids++ ){
       for( nbad=iv=0 ; iv < nvec ; iv++ ){
         if( !isfinite(ddp[ids][iv]) ){ ddp[ids][iv] = 0.0f; nbad++; }
       }
       if( nbad > 0 ) WARNING_message("%d bad correlations in dataset #%d",nbad,ids) ;
     }
   }
#endif

   return ;
}

/*----------------------------------------------------------------------------*/
/* Load the seed vectors from each dataset */

void GRINCOR_load_seedvec( MRI_shindss *shd , MCW_cluster *nbhd ,
                           int voxijk       , float **seedvec    )
{
   int nx,ny,nz,nxy, ndset,nvals, voxind,ii,jj,kk, aa,bb,cc,xx,yy,zz, qijk,qind ;
   short *sv=NULL , *svv=NULL ; float *vv ; sbyte *bv=NULL , *bvv=NULL ;

   nx = shd->nx; ny = shd->ny; nz = shd->nz; nxy = nx*ny; ndset = shd->ndset;

   IJK_TO_THREE(voxijk,aa,bb,cc,nx,nxy) ;
   voxind = IJK_TO_INDEX(shd,voxijk) ;
   for( kk=0 ; kk < ndset ; kk++ ){
     nvals = shd->nvals[kk] ;
     if( shd->datum == 1 ){ bv = shd->bv[kk] ; bvv = bv + voxind*nvals ; }
     else                 { sv = shd->sv[kk] ; svv = sv + voxind*nvals ; }
     vv = seedvec[kk] ;
     if( shd->datum == 1 )
       for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)bvv[ii] ;
     else
       for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)svv[ii] ;
     if( nbhd != NULL ){  /* average in with nbhd */
       for( jj=1 ; jj < nbhd->num_pt ; jj++ ){
         xx = aa + nbhd->i[jj] ; if( xx < 0 || xx >= nx ) continue ;
         yy = bb + nbhd->j[jj] ; if( yy < 0 || yy >= ny ) continue ;
         zz = cc + nbhd->k[jj] ; if( zz < 0 || zz >= nz ) continue ;
         qijk = THREE_TO_IJK(xx,yy,zz,nx,nxy) ;
         qind = IJK_TO_INDEX(shd,qijk) ;
         if( qind >= 0 ){
           if( shd->datum == 1 ){
             bvv = bv + qind*nvals ;
             for( ii=0 ; ii < nvals ; ii++ ) vv[ii] += (float)bvv[ii] ;
           } else {
             svv = sv + qind*nvals ;
             for( ii=0 ; ii < nvals ; ii++ ) vv[ii] += (float)svv[ii] ;
           }
         }
       }
     }
     (void)THD_normalize( nvals , vv ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------------*/

static int nport = -1 ;                 /* 02 Aug 2010 */

NI_stream GI_stream = (NI_stream)NULL ;

/*=============================================================================*/

static char *pname = "AFNI" ;  /* name of partner program */

void GI_exit(void)                   /* Function to be called to make sure */
{                                    /* the AFNI data channel gets closed. */
   if( GI_stream != (NI_stream)NULL ){
     fprintf(stderr,"** RestSym exits: closing connection to %s\n",pname) ;
     NI_stream_close(GI_stream) ;
   } else if( verb > 2 ){
     fprintf(stderr,"** RestSym atexit() function invoked\n") ;
   }
   return ;
}

/*-----------------------------------------------------------------------------*/

#include <signal.h>

void GI_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char *sname ;
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\n** RestSym: Fatal Signal %d (%s) received\n",sig,sname) ;
   exit(1) ;
}

/*--------------------------------------------------------------------------*/
SUMA_Boolean SUMA_RestSym_Dsets(SUMA_SurfaceObject *SOv[],
                              GICOR_setup *giset,
                              char *target_name,
                              DList *DsetList,
                              SUMA_DSET *sdsetv[],
                              SUMA_DSET *mxsets[],
                              NI_element *nel) 
{
   static char FuncName[]={"SUMA_RestSym_Dsets"};
   static char mxset_ID[2][50]={"\0", "\0"};
   char *targetv[2]={NULL, NULL}, *targetmv[2]={NULL, NULL},
        *dset_namev[2]={NULL, NULL}, *atr=NULL;
   int i, ii, *Ti=NULL, ovind = 0, nvals=0, vv=0;
   static char *blab[6] = { "GIC_Delta" , "GIC_Zscore" ,
                            "AAA_Delta" , "AAA_Zscore" ,
                            "BBB_Delta" , "BBB_Zscore"  } ;
   NI_str_array *labar=NULL ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (target_name && 
       giset->sdset_ID[0][0] != '\0' && giset->sdset_ID[1][0] != '\0') {
       SUMA_S_Warn("Hello anew from 3dGroupInCorr, \n"
                   "Attempting to reuse previous setup...");
       goto CHECK_DSET_AND_OVERLAYS;
   }
   
   if (target_name) { /* Brand new init, search/create by name */
      SUMA_S_Notev("Brand new init, target_name=%s\n"
                   "SOv = [%p %p]\n"
                   , target_name, SOv[0], SOv[1]);
      if (!nel) {
         SUMA_S_Err("Need GICOR setup nel for creating new dsets");
         SUMA_RETURN(NOPE);
      }  
      /* Form the names of the dsets to be created */
      if (SOv[1]) { /* have two surfaces */
         targetv[0] = SUMA_append_string(target_name,".Left");
         targetv[1] = SUMA_append_string(target_name,".Right");
         targetmv[0] = SUMA_append_string(target_name,".Max.Left");
         targetmv[1] = SUMA_append_string(target_name,".Max.Right");
      } else {
         targetv[0] = SUMA_copy_string(target_name);
         targetmv[0] = SUMA_append_string(target_name, ".Max");
      }

      /* Now create a dataset for each case */
      for (i=0; i<2; ++i) {
         if (targetv[i]) {
            SUMA_LHv("Working %s\n", targetv[i]);
            /* dset names */
            dset_namev[i] = SUMA_append_string(targetv[i], SOv[i]->idcode_str);
            sdsetv[i] = SUMA_CreateDsetPointer (dset_namev[i], 
                                        SUMA_NODE_BUCKET,
                                        NULL,
                                        SOv[i]->idcode_str,
                                        SOv[i]->N_Node);
            sprintf(giset->sdset_ID[i],"%s", SDSET_ID(sdsetv[i]));
            
            /* insert that element into DaList */
            if (!SUMA_InsertDsetPointer(&sdsetv[i], DsetList, 0)) {
               SUMA_SL_Err("Failed to insert dset into list");
               SUMA_free(dset_namev[i]); SUMA_free(targetv[i]);
               SUMA_RETURN(NOPE);
            }
            /* add the columns */
            Ti = (int *) SUMA_calloc(SDSET_VECLEN(sdsetv[i]), sizeof(int));
            for (ii=0; ii <SDSET_VECLEN(sdsetv[i]); ++ii) Ti[ii]=ii;
            SUMA_AddDsetNelCol (sdsetv[i], "node index", 
                                SUMA_NODE_INDEX, Ti, NULL, 1);
            SUMA_free(Ti); Ti=NULL;
            
            atr = NI_get_attribute( nel , "target_nvals" ) ;  
               if( atr == NULL )        SUMA_GIQUIT;
            nvals = (int)strtod(atr,NULL) ;  nvals = MAX(1,nvals);     
         
            atr = NI_get_attribute( nel , "target_labels" ) ;
            if( atr != NULL )
               labar = NI_decode_string_list( atr , ";" ) ;
            
            for( vv=0 ; vv < nvals ; vv++ ){
               if (labar != NULL && vv < labar->num) atr = labar->str[vv];
               else if (vv < 6) {
                  atr = blab[vv];
               } else {
                  atr = "What the hell is this?";
               }
               if (vv%2 == 0) { /* beta */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_FLOAT, NULL, NULL, 1);
               } else { /* zscore */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_ZSCORE, NULL, NULL, 1);  
               }
            }
            if (labar) SUMA_free_NI_str_array(labar); labar=NULL;
            SUMA_free(dset_namev[i]); dset_namev[i]=NULL;
            SUMA_free(targetv[i]); targetv[i]=NULL;
         
            /* now create the maxsym output */
            SUMA_LH("Output time");
            dset_namev[i] = SUMA_append_string(targetmv[i], SOv[i]->idcode_str);
            mxsets[i] = SUMA_CreateDsetPointer (dset_namev[i], 
                                        SUMA_NODE_BUCKET,
                                        NULL,
                                        SOv[i]->idcode_str,
                                        SOv[i]->N_Node);
            sprintf(mxset_ID[i],"%s", SDSET_ID(mxsets[i]));
            
            /* insert that element into DaList */
            if (!SUMA_InsertDsetPointer(&mxsets[i], DsetList, 0)) {
               SUMA_SL_Err("Failed to insert dset into list");
               SUMA_free(dset_namev[i]); SUMA_free(targetv[i]);
               SUMA_RETURN(NOPE);
            }
            /* add the columns */
            Ti = (int *) SUMA_calloc(SDSET_VECLEN(mxsets[i]), sizeof(int));
            for (ii=0; ii <SDSET_VECLEN(mxsets[i]); ++ii) Ti[ii]=ii;
            SUMA_AddDsetNelCol (mxsets[i], "node index", 
                                SUMA_NODE_INDEX, Ti, NULL, 1);
            SUMA_free(Ti); Ti=NULL;
            SUMA_AddDsetNelCol (mxsets[i], "LabelOfMax",
                                SUMA_NODE_FLOAT, NULL, NULL, 1);
            SUMA_AddDsetNelCol (mxsets[i], "MaxZ",
                                SUMA_NODE_FLOAT, NULL, NULL, 1);
            SUMA_free(dset_namev[i]); dset_namev[i]=NULL;
            SUMA_free(targetmv[i]); targetmv[i]=NULL;
         }
      }
      
      /* Done with brand new init */
      SUMA_RETURN(YUP);
   } 
   
   CHECK_DSET_AND_OVERLAYS:
   SUMA_LH("Checking Dset and Overlays.\n");

   { /* just use what is in giset */
      if (giset->sdset_ID[0][0] == '\0') {
         SUMA_S_Err("No ID in sdset_ID. Unexpected happenstance");
         SUMA_RETURN(NOPE);
      }
      if (!(sdsetv[0] = SUMA_FindDset_s(giset->sdset_ID[0], DsetList))) {
         SUMA_S_Err("SDSET for 0 not found");
         SUMA_RETURN(NOPE);
      }
      if (giset->sdset_ID[1][0] != '\0') {
         if (!(sdsetv[1] = SUMA_FindDset_s(giset->sdset_ID[1], DsetList))) {
            SUMA_S_Err("SDSET for 1 not found");
            SUMA_RETURN(NOPE);
         }
      }
      if (!(mxsets[0] = SUMA_FindDset_s(mxset_ID[0], DsetList))) {
         SUMA_S_Err("MXSET for 0 not found");
         SUMA_RETURN(NOPE);
      }
      if (giset->sdset_ID[1][0] != '\0') {
         if (!(mxsets[1] = SUMA_FindDset_s(mxset_ID[1], DsetList))) {
            SUMA_S_Err("MXSET for 1 not found");
            SUMA_RETURN(NOPE);
         }
      }
   }
   
   /* at this point, we have the relevant dsets in sdsetv, 
      and mxsets */
   SUMA_RETURN(YUP);
}
/*! find surfaces appropriate for giset */
SUMA_Boolean SUMA_RestSym_Surfaces(GICOR_setup *giset, SUMA_SurfaceObject *SOv[]) 
{
   static char FuncName[]={"SUMA_RestSym_Surfaces"};  
   
   SUMA_ENTRY;
   
   if (!(SOv[0] = SUMA_FindSOp_inDOv_from_N_Node(
                        giset->nnode_domain[0], 
                        giset->nnode_domain[1] ? SUMA_LEFT:SUMA_NO_SIDE, 
                        1, 1, 
                        SUMAg_DOv, SUMAg_N_DOv))) {
      SUMA_S_Errv("Could not find domain parent for a domain of %d nodes\n",
               giset->nnode_domain[0]);
      SUMA_RETURN(NOPE);
   }
   
   if (giset->nnode_domain[1]) {
      if (!(SOv[1]=SUMA_FindSOp_inDOv_from_N_Node(
                           giset->nnode_domain[1], SUMA_RIGHT, 
                           1, 1, 
                           SUMAg_DOv, SUMAg_N_DOv))) {
         SUMA_S_Errv("Could not find domain parent for a "
                     "RH domain of %d nodes\n",
                     giset->nnode_domain[1]);
         SUMA_RETURN(NOPE);
      }
   }
   
   SUMA_RETURN(YUP); 
}
SUMA_Boolean SUMA_RestSym_setup_func( NI_stream nsg , NI_element *nel )
{
   static char FuncName[]={"SUMA_RestSym_setup_func"};
   GICOR_setup *giset = NULL;
   char *atr=NULL , *pre=NULL, *s=NULL;
   SUMA_DSET *sdsetv[2]={NULL, NULL};
   SUMA_DSET *mxsets[2]={NULL, NULL};
   int nnode_dom[2]={0,0};
   int nnode_mask[2]={0,0};
   SUMA_SurfaceObject *SOv[2]={NULL, NULL};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* fetch the giset struct */
   giset = SUMAg_CF->giset; 
   if( giset != NULL && giset->ready ) SUMA_RETURN(YUP) ;

   if( giset == NULL ){
     giset = (GICOR_setup *)SUMA_calloc(1,sizeof(GICOR_setup)) ;
     SUMAg_CF->giset = giset;
   } else {
     memset(giset, 0, sizeof(GICOR_setup)) ;
   }
   
   giset->ns    = nsg ;  /* save socket for I/O back to 3dGroupInCorr */
   giset->ready = 0 ;    /* not ready yet */

   /* set various parameters from the NIML header */

   atr = NI_get_attribute( nel , "ndset_A" ) ; 
      if( atr == NULL )        SUMA_GIQUIT;
   giset->ndset_A = (int)strtod(atr,NULL) ;    
      if( giset->ndset_A < 2 ) SUMA_GIQUIT;

   atr = NI_get_attribute( nel , "ndset_B" ) ; 
      if( atr == NULL )        SUMA_GIQUIT;
   giset->ndset_B = (int)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "nvec" ) ;  
      if( atr == NULL )        SUMA_GIQUIT;
   giset->nvec = (int)strtod(atr,NULL) ;       
      if( giset->nvec < 2 )    SUMA_GIQUIT;

   atr = NI_get_attribute( nel , "seedrad" ) ;
   if( atr != NULL ) giset->seedrad = (float)strtod(atr,NULL) ;

   atr = NI_get_attribute( nel , "ttest_opcode" ) ;
   if( atr != NULL ) giset->ttest_opcode = (int)strtod(atr,NULL) ;

   /* create output dataset(s), to be filled in from 3dGroupInCorr data later */
               
   atr = NI_get_attribute( nel , "geometry_string" ); 
      if( atr == NULL ) {
         SUMA_S_Err("No geometry string");
         SUMA_RETURN(NOPE);
      }
   pre = NI_get_attribute( nel , "target_name" ) ;
   if( pre == NULL || *pre == '\0' ) pre = "GICorrelletto" ;
   
   /* How many dsets? */
   SUMA_LHv("attr=%s\nval0=%s,val1=%s\n", 
            NI_get_attribute(nel,"LRpair_nnode"),
            SUMA_NI_get_ith_string(NI_get_attribute(nel,"LRpair_nnode"),",",0),
            SUMA_NI_get_ith_string(NI_get_attribute(nel,"LRpair_nnode"),",",1));
   if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_nnode"),",",0))) {
      giset->nnode_domain[0] = (int)strtol(s, NULL, 10);
      SUMA_free(s); s = NULL;
      if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_nnode"),",",1))) {
         giset->nnode_domain[1] = (int)strtol(s, NULL, 10);
         SUMA_free(s); s = NULL;
      }
   } else {
      giset->nnode_domain[0] = giset->nvec; 
      giset->nnode_domain[1] = 0; 
   }
               
   if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_ninmask"),",",0))) {
      giset->nnode_mask[0] = (int)strtol(s, NULL, 10);
      SUMA_free(s); s = NULL;
      if ((s=SUMA_NI_get_ith_string(
               NI_get_attribute(nel,"LRpair_ninmask"),",",1))) {
         giset->nnode_mask[1] = (int)strtol(s, NULL, 10);
         SUMA_free(s); s = NULL;
      }
   } else {
      giset->nnode_mask[0] = giset->nnode_domain[0]; 
      giset->nnode_mask[1] = giset->nnode_domain[1];
   }
   
   SUMA_LH("Look for surfaces");
   /* Now find surfaces that can be the domain */
   if (!SUMA_RestSym_Surfaces(giset, SOv)) {
      SUMA_S_Err("Failed to find surfaces for giset");
      SUMA_RETURN(NOPE);
   }
   
   /* Now create appropriate dsets */
   SUMA_LH("Look for dsets");
   if (!SUMA_RestSym_Dsets(SOv, giset, pre, SUMAg_CF->DsetList, 
                         sdsetv, mxsets, nel)) {
      SUMA_S_Err("Failed to find/create dsets for giset");
      SUMA_RETURN(NOPE);
   }

   giset->nvox = giset->nvec ;

   SUMA_LH("Setup node list");
   /* list of voxels to expect from each 3dGroupInCorr data */
   if( nel->vec_len == 0 || nel->vec_num == 0 || nel->vec == NULL ){  /* all */
     giset->ivec = NULL ; giset->nivec = 0 ;
      INFO_message("DEBUG: GICOR_setup_func has ivec=NULL") ; 
   } else {                                     /* make index list of voxels */
     int ii , nn , *iv=(int *)nel->vec[0] ;
     giset->ivec = (int *)calloc(sizeof(int),giset->nvec) ;
     nn = MIN(giset->nvec,nel->vec_len) ; giset->nivec = nn ;
     for( ii=0 ; ii < nn ; ii++ ) giset->ivec[ii] = iv[ii] ;
     INFO_message("DEBUG: GICOR_setup_func has ivec=int[%d]",nn) ; 
   }

   giset->ready = 1 ;
   
   if (LocalHead) {
      SUMA_Show_GISET(giset, NULL, 0);
   }
     
   SUMA_RETURN(YUP) ;
}

typedef struct {
   float delta;
   float zscore;
   int   idelta;
   int   izscore;
   int   inode;
} MAXINFO;

void SowMxInf(MAXINFO mx) {
   fprintf(SUMA_STDOUT, "delta %f\t%d\n"
                        "zscor %f\t%d\n"
                        "inode   %d\n",
                        mx.delta, mx.idelta,
                        mx.zscore, mx.izscore,
                        mx.inode);
   return;
}
SUMA_Boolean RestSym_Proc (NI_element *nel, int inode, int flg, int inode_dbg)
{
   static char FuncName[]={"SUMA_RestSym_Proc"};
   GICOR_setup *giset = SUMAg_CF->giset ;
   char *sbuf=NULL;
   float *neldar , *nelzar , *dsdar , *dszar, *farlb , *farmx;
   int nvec,nn , vmul ; float thr ;
   int id=0, ic=0, ipair=0, kk, kko;
   SUMA_SurfaceObject *SOv[2]={NULL,NULL};
   SUMA_DSET *sdsetv[2]={NULL, NULL};
   SUMA_DSET *mxsets[2]={NULL, NULL};
   MAXINFO mxinf[2];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LHv("In at node %d\n", inode);

   if( nel == NULL || nel->vec_num < 2 ){  /* should never happen */
     ERROR_message("badly formatted dataset from 3dGroupInCorr!") ;
     SUMA_RETURN(NOPE) ;
   }

   if (nel->vec_num % 2) {
      SUMA_SLP_Err("Number of sub-bricks not multiple of two!");
      SUMA_RETURN(NOPE) ;
   }
   
   if( giset == NULL ||
       !giset->ready   ){   /* should not happen */

     if( giset != NULL ) giset->ready = 0 ;
     /* AFNI_misc_CB(im3d->vwid->func->gicor_pb,(XtPointer)im3d,NULL) ; */
     SUMA_SLP_Err(" ******* SUMA: *********\n"
                  "  3dGrpInCorr sent data \n"
                  "  but setup isn't ready!\n" ) ;
     SUMA_RETURN(NOPE) ;
   }

   /* get the surfaces, the dsets, and their overlays */
   if (!SUMA_RestSym_Surfaces(giset, SOv)) {
      SUMA_S_Err("Failed to find surfaces for giset");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_RestSym_Dsets(SOv, giset, NULL, SUMAg_CF->DsetList, 
                         sdsetv, mxsets, NULL)) {
      SUMA_S_Errv("Failed to find/create dsets for giset, inode = %d\n", inode);
      SUMA_RETURN(NOPE);
   }
   
   /* copy NIML data into dataset */

   SUMA_LH("Populating the dset in question");
   for (id=0; id < 2; ++id) {
      memset((void*)(mxinf+id),0, sizeof(MAXINFO));
      mxinf[id].inode = inode;
      farlb = (float *)(SDSET_VEC(mxsets[id], 0));
      farmx = (float *)(SDSET_VEC(mxsets[id], 1));
      for (ipair=0; ipair < nel->vec_num/2; ++ipair) {
         neldar = (float *)nel->vec[2*ipair+0] ;  /* delta array */
         nelzar = (float *)nel->vec[2*ipair+1] ;  /* zscore array */
         nvec   = nel->vec_len ;

         if (giset->nnode_domain[id]) {
            dsdar = (float *)SDSET_VEC(sdsetv[id],(2*ipair+0)) ;
            dszar = (float *)SDSET_VEC(sdsetv[id],(2*ipair+1)) ;
            if (LocalHead) {
               sbuf=SUMA_ShowMeSome(dsdar,
                           SUMA_float, SDSET_VECLEN(sdsetv[id]),10,"dsdar:\n");
               SUMA_LHv("pre copy surf%d %s\n",id, sbuf); 
               SUMA_free(sbuf); sbuf=NULL;
            }

            if( giset->ivec == NULL ){  /* all nodes */
               if (giset->nvox != nvec) {
                  SUMA_S_Errv( "nvox=%d, nvec=%d, ivec=NULL\n"
                              "Did not expect that.\n",
                              giset->nvox, nvec);
                  SUMA_RETURN(NOPE) ;
               }
               if (id == 0) {
                  nn = MAX(0, nvec-giset->nnode_domain[1]);
                  SUMA_LHv("Copying %d values from neldar, surf%d\n", 
                           nn, id);
                  if (LocalHead) {   
                     sbuf=SUMA_ShowMeSome(neldar,SUMA_float, nn,10,"neldar:\n");
                     SUMA_LHv("from the tube surf%d: %s\n", id, sbuf); 
                     SUMA_free(sbuf); sbuf=NULL;
                  }
                  memcpy(dsdar,neldar,sizeof(float)*nn) ;
                  memcpy(dszar,nelzar,sizeof(float)*nn) ;
               } else {
                  nn = MAX(0, nvec-giset->nnode_domain[0]);
                  SUMA_LHv("Copying %d values from neldar+%d, surf%d\n", 
                           nn, giset->nnode_domain[0], id);
                  if (LocalHead) {
                     sbuf=SUMA_ShowMeSome((neldar+giset->nnode_domain[0]),
                                          SUMA_float, nn, 10,"neldar:\n");
                     SUMA_LHv("from the tube surf%d: %s\n", id, sbuf); 
                     SUMA_free(sbuf); sbuf=NULL;
                  }
                  memcpy(dsdar,(neldar+giset->nnode_domain[0]),
                           sizeof(float)*nn) ;
                  memcpy(dszar,(nelzar+giset->nnode_domain[0]),
                           sizeof(float)*nn) ;
               }
               /* Now search for the max */
               for (kk=0; kk<nn; ++kk) {
                  if (dsdar[kk] > farmx[kk]) {
                     farmx[kk] = dsdar[kk];
                     farlb[kk] = flg;
                  }
               }
               if (LocalHead) {
                  sbuf=SUMA_ShowMeSome(dsdar,SUMA_float, nn, 10,"dsdar:\n");
                  SUMA_LHv("post copy surf%d %s\n", id, sbuf); 
                  SUMA_free(sbuf); sbuf=NULL;
               }
            } else { /* Have index vector */
               int *ivec=giset->ivec ;
               nn = MIN( giset->nnode_mask[id] , nvec ) ;
               if (id == 0) {
                  for( kk=0 ; kk < nn ; kk++ ){
                     dsdar[ivec[kk]] = neldar[kk] ; 
                     dszar[ivec[kk]] = nelzar[kk] ;
                     if (dsdar[ivec[kk]] > farmx[ivec[kk]]) {
                        farmx[ivec[kk]] = dsdar[ivec[kk]];
                        farlb[ivec[kk]] = flg;
                     }
                  }
               } else {
                  for( kk=0 ; kk < nn ; kk++ ){
                     kko = ivec[kk]-giset->nnode_domain[0];
                     dsdar[kko] = neldar[kk] ; 
                     dszar[kko] = nelzar[kk] ;
                     if (dsdar[kko] > 
                                   farmx[kko]) {
                        farmx[kko] = dsdar[kko];
                        farlb[kko] = flg;
                     }
                  }
               }
            }
         } /* if (giset->nnode_domain[id]) */
      } /* for (ipair ...) */
   }
   if (LocalHead) {
      SUMA_ShowDset(sdsetv[0],0,NULL);
      SUMA_ShowDset(sdsetv[1],0,NULL);
   }
   if (inode == inode_dbg) {
      char sout[256], sid[50];
      sprintf(sout,"CorrFrom.leftseed%d.lh", inode_dbg);
      strcpy(sid, SDSET_ID(sdsetv[0])); /* preserve id
                                           write function might change it */
      SUMA_WriteDset_s(sout,   sdsetv[0], SUMA_ASCII_NIML, 1, 1);
      NI_set_attribute (sdsetv[0]->ngr, "self_idcode", sid);
      
      sprintf(sout,"CorrFrom.leftseed%d.rh", inode_dbg);
      strcpy(sid, SDSET_ID(sdsetv[1]));
      SUMA_WriteDset_s(sout, sdsetv[1], SUMA_ASCII_NIML, 1, 1);
      NI_set_attribute (sdsetv[1]->ngr, "self_idcode", sid);
   }
   
   
   SUMA_RETURN(YUP);
}
/*--------------------------------------------------------------------------*/

static char *afnihost       = "localhost" ;
static MRI_shindss *shd_AAA = NULL ;
static MRI_shindss *shd_BBB = NULL ;
static int ttest_opcode     = -1   ;  /* 0=pooled, 1=unpooled, 2=paired */
static int ttest_opcode_max =  2   ;

static UINT32 testA, testB, testAB ;  /* 23 May 2010 */
static int mcov = 0 ;
static int nout = 0 ;
static float *axx , *axx_psinv , *axx_xtxinv ;
static float *bxx , *bxx_psinv , *bxx_xtxinv ;

#define MAX_LABEL_SIZE 12
#define NSEND_LIMIT     9

#undef COVTEST  /* this is for Cox ONLY */

int main( int argc , char *argv[] )
{
   static char FuncName[]={"RestSym"};
   int nopt , kk , nn , ii,jj, TalkToAfni=1, inode = 0;
   char nsname[2048]  ; /* NIML socket name */
   NI_element *nelset ; /* NIML element with dataset to send to AFNI */
   NI_element *nelcmd ; /* NIML element with command from AFNI */
   float *neldar=NULL     , *nelzar=NULL          ;
   float *neldar_AAA=NULL , *nelzar_AAA=NULL ;
   float *neldar_BBB=NULL , *nelzar_BBB=NULL ; int dosix=0 , nosix=0 ;
   char buf[1024] ;
   float seedrad=0.0f , dx,dy,dz , dmin ; int nx,ny,nz,nxy ;
   MCW_cluster *nbhd=NULL ;
   int voxijk , voxind , aa,bb,cc , xx,yy,zz , qijk,qind ; char *atr ;
   int nvec , ndset_AAA=0,ndset_BBB=0 , *nvals_AAA=NULL,*nvals_BBB=NULL ;
   float **seedvec_AAA=NULL , **dotprod_AAA=NULL ;
   float **seedvec_BBB=NULL , **dotprod_BBB=NULL ;
   int ctim,btim,atim, btim_all, do_shm=2 , nsend=0 , shm_active=0 ;
   char label_AAA[MAX_LABEL_SIZE]="AAA" , label_BBB[MAX_LABEL_SIZE]="BBB" ;
   char *qlab_AAA=NULL , *qlab_BBB=NULL ;
   int   lset_AAA=0    ,  lset_BBB=0 ;
   int   *use_AAA=NULL ,  *use_BBB=NULL ;  /* lists of subjects to use */

   NI_element   *covnel=NULL ;       /* covariates */
   NI_str_array *covlab=NULL ;
   MRI_IMAGE *axxim , *axxim_psinv , *axxim_xtxinv ;
   MRI_IMAGE *bxxim , *bxxim_psinv , *bxxim_xtxinv ;
   float **dtar=NULL ;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0, i=0;
   SUMA_SurfaceObject *SOv[2];
   SUMA_DSET *sdsetv[2]={NULL, NULL};
   SUMA_DSET *mxsets[2]={NULL, NULL};
   byte *nmask=NULL;
   int N_nmask=0;
   char *nmaskname = NULL; 
   char *prefix=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
#ifdef COVTEST
   float *ctarA=NULL , *ctarB=NULL ; char *ctnam ;
#endif

   /*-- enlighten the ignorant and brutish sauvages? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) {
   RestSymHelp();
   }
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   
   
   /*-- process command line options --*/
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-s;-o;-talk;-mask;-dset;");
      
   
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }

   SUMA_LHv("Loading surface %d specs...", N_Spec);
   if (!SUMA_LoadSpec_eng (&Spec[0], SUMAg_DOv, &SUMAg_N_DOv, 
                                          NULL, 0, SUMAg_CF->DsetList)) {
			            fprintf( SUMA_STDERR,
                              "Error %s: Failed in SUMA_LoadSpec.\n", FuncName);
			            exit(1);
		            }
   if (SUMAg_N_DOv<2) {
      ERROR_exit("DOH!");
   }  
   if (LocalHead) SUMA_Show_DOv (SUMAg_DOv, SUMAg_N_DOv, NULL);
   
   SUMA_LH("Parsing other opts...");
   nopt = 1 ;
   while( nopt < argc ){

#ifndef DONT_USE_SHM
     if( strcasecmp(argv[nopt],"-NOshm") == 0 ){
       do_shm = 0 ; nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-quiet") == 0 ){
       verb = 0 ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-verb") == 0 ){
       verb++ ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-VERB") == 0 ){
       verb += 2 ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-debug") == 0 ){
       debug++ ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-suma") == 0 ){
       TalkToAfni = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) 
            ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       prefix =  argv[nopt] ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-nmask") == 0 ){
         if( ++nopt >= argc ) 
            ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
         nmaskname = argv[nopt] ;
       nopt++ ; continue ;       
     }

#if 0
     if( strcasecmp(argv[nopt],"-nosix") == 0 ){
       nosix = 1 ; nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-seedrad") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       seedrad = (float)strtod(argv[nopt],NULL) ;
       if( seedrad < 0.0f ){
         WARNING_message("Negative -seedrad being set back to zero!?") ;
         seedrad = 0.0f ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-np") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       nport = (int)strtod(argv[nopt],NULL) ;
       if( nport < 1024 || nport > 65535 ){
         WARNING_message("Illegal port after '-np': should be in range 1024..65535") ;
         nport = -1 ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-ah") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       afnihost = strdup(argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-pooled") == 0 ){
       ttest_opcode = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-unpooled") == 0 ){
       ttest_opcode = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-paired") == 0 ){
       ttest_opcode = 2 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-covariates") == 0 ){  /* 20 May 2010 */
       char *lab ; float sig ; int nbad ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( covnel != NULL ) ERROR_exit("can't use -covariates twice!") ;
       covnel = THD_simple_table_read( argv[nopt] ) ;
       if( covnel == NULL )
         ERROR_exit("Can't read table from -covariates file '%s'",argv[nopt]) ;
       INFO_message("Covariates file: %d columns, each with %d rows",
                    covnel->vec_num , covnel->vec_len ) ;
       mcov = covnel->vec_num - 1 ;
       if( mcov < 1 )
         ERROR_exit("Need at least 2 columns in -covariates file!") ;
       else if( mcov > MAXCOV )
         ERROR_exit("%d covariates in file, more than max allowed (%d)",mcov,MAXCOV) ;
       lab = NI_get_attribute( covnel , "Labels" ) ;
       if( lab != NULL ){
         ININFO_message("Covariate column labels: %s",lab) ;
         covlab = NI_decode_string_list( lab , ";," ) ;
         if( covlab == NULL || covlab->num < mcov+1 )
           ERROR_exit("can't decode labels properly?!") ;
       } else {
         ERROR_exit("Can't get labels from -covariates file '%s'",argv[nopt]) ;
       }
       for( nbad=0,kk=1 ; kk <= mcov ; kk++ ){
         meansigma_float(covnel->vec_len,(float *)covnel->vec[kk],NULL,&sig) ;
         if( sig <= 0.0f ){
           ERROR_message("Covariate '%s' is constant; how can this be used?!" ,
                         covlab->str[kk] ) ;
           nbad++ ;
         }
         if( strlen(covlab->str[kk]) > MAX_LABEL_SIZE )  /* truncate labels to fit */
           covlab->str[kk][MAX_LABEL_SIZE] = '\0' ;
       }
       if( nbad > 0 ) ERROR_exit("Cannot continue :-(") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelA") == 0 || strcasecmp(argv[nopt],"-labA") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( argv[nopt][0] != '\0' ){
         NI_strncpy(label_AAA,argv[nopt],MAX_LABEL_SIZE) ;
         THD_filename_fix(label_AAA) ; lset_AAA = 1 ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelB") == 0 || strcasecmp(argv[nopt],"-labB") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       if( argv[nopt][0] != '\0' ){
         NI_strncpy(label_BBB,argv[nopt],MAX_LABEL_SIZE) ;
         THD_filename_fix(label_BBB) ; lset_BBB = 1 ;
       }
       nopt++ ; continue ;
     }


     if( strcasecmp(argv[nopt],"-setA") == 0 ){
       char *fname , *cpt ;
       if( shd_AAA != NULL ) ERROR_exit("can only use '-setA' once!") ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]);
       fname = strdup(argv[nopt]) ;
       if( STRING_HAS_SUFFIX(fname,".data") ){
         strcpy(fname+strlen(fname)-5,".niml") ;
         WARNING_message("Replaced '.data' with '.niml' in -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,".niml") ;
         INFO_message("Added '.niml' to end of -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr.") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,"niml") ;
         INFO_message("Added 'niml' to end of -setA filename") ;
       }
       shd_AAA = GRINCOR_read_input( fname ) ;
       if( shd_AAA == NULL ) ERROR_exit("Cannot continue after -setA input error") ;
       if( verb ) INFO_message("-setA opened, contains %d datasets, %d time series, %s bytes",
                                shd_AAA->ndset , shd_AAA->nvec , commaized_integer_string(shd_AAA->nbytes));
       qlab_AAA = fname ;
       cpt = strchr(qlab_AAA,'.') ; if( cpt != NULL && cpt != qlab_AAA ) *cpt = '\0' ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-setB") == 0 ){
       char *fname , *cpt ;
       if( shd_BBB != NULL ) ERROR_exit("can only use '-setB' once!") ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       fname = strdup(argv[nopt]) ;
       if( STRING_HAS_SUFFIX(fname,".data") ){
         strcpy(fname+strlen(fname)-5,".niml") ;
         WARNING_message("Replaced '.data' with '.niml' in -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,".niml") ;
         INFO_message("Added '.niml' to end of -setB filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr.") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,"niml") ;
         INFO_message("Added 'niml' to end of -setB filename") ;
       }
       shd_BBB = GRINCOR_read_input( fname ) ;
       if( shd_BBB == NULL ) ERROR_exit("Cannot continue after -setB input error") ;
       if( verb ) INFO_message("-setB opened, contains %d datasets, %d time series, %s bytes",
                                shd_BBB->ndset , shd_BBB->nvec , commaized_integer_string(shd_BBB->nbytes));
       qlab_BBB = fname ;
       cpt = strchr(qlab_BBB,'.') ; if( cpt != NULL && cpt != qlab_BBB ) *cpt = '\0' ;
       nopt++ ; continue ;
     }

     if (!ps->arg_checked[nopt]) {
      ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
     } else {
      nopt++;
     }
   }

   /*-- check inputs for OK-ness --*/

   if( shd_AAA == NULL ) ERROR_exit(" !! You must use the '-setA' option !!") ;

   /* 18 Mar 2010: if -labelA not used, get it from input filename (mm for B) */

   if( !lset_AAA && qlab_AAA != NULL )
     NI_strncpy(label_AAA,qlab_AAA,MAX_LABEL_SIZE) ;

   if( !lset_BBB && qlab_BBB != NULL )
     NI_strncpy(label_BBB,qlab_BBB,MAX_LABEL_SIZE) ;

   nvec      = shd_AAA->nvec  ;
   ndset_AAA = shd_AAA->ndset ;
   nvals_AAA = shd_AAA->nvals ;
   if( shd_BBB != NULL ){
     ndset_BBB = shd_BBB->ndset ; nvals_BBB = shd_BBB->nvals ; dosix = !nosix ;
   } else {
     ndset_BBB = 0 ; nvals_BBB = NULL ; dosix = 0 ;
   }

   if( shd_BBB != NULL && shd_AAA->nvec != shd_BBB->nvec )
     ERROR_exit("-setA and -setB don't have same number of voxels") ;

   if( shd_BBB != NULL && strcmp(shd_AAA->dfname,shd_BBB->dfname) == 0 )
     ERROR_exit("-setA and -setB can't use the same datafile!") ;

        if( shd_BBB        == NULL           ) ttest_opcode_max = 0 ;
   else if( shd_BBB->ndset != shd_AAA->ndset ) ttest_opcode_max = 1 ;

   if( ttest_opcode < 0 || ttest_opcode > ttest_opcode_max ){
     if( shd_BBB != NULL && verb > 2 )
       INFO_message("Setting t-test option to default value of 'pooled'") ;
     ttest_opcode = 0 ;
   }

   if( ttest_opcode == 1 && mcov > 0 ){
     INFO_message("-covariates does not support unpooled variance (yet)") ;
     ttest_opcode = 0 ;
   }

   /*---------- Process covariates into matrices [23 May 2010] ----------*/

#undef  AXX
#define AXX(i,j) axx[(i)+(j)*(nA)]    /* i=0..nA-1 , j=0..mcov */
#undef  BXX
#define BXX(i,j) bxx[(i)+(j)*(nB)]    /* i=0..nB-1 , j=0..mcov */

   if( mcov > 0 ){
     int nbad=0 , nA , nB ;

     /* simple tests for stoopid users [is there any other kind?] */

     if( shd_AAA->dslab == NULL ){
       ERROR_message("Can't use covariates, since setA doesn't have dataset labels!") ;
       nbad++ ;
     if( shd_BBB != NULL && shd_BBB->dslab == NULL )
       ERROR_message("Can't use covariates, since setB doesn't have dataset labels!") ;
       nbad++ ;
     }

     if( ndset_AAA < mcov+3 ){
       ERROR_message("-setA has %d datasets, but you have %d covariates!") ; nbad++ ;
     }
     if( ndset_BBB > 0 && ndset_BBB < mcov+3 ){
       ERROR_message("-setB has %d datasets, but you have %d covariates!") ; nbad++ ;
     }

     if( nbad ) ERROR_exit("Can't continue :-(") ;

     if( verb ) INFO_message("Setting up regression matrices for covariates") ;

     /*--- setup the setA regression matrix ---*/

     nA    = shd_AAA->ndset ;
     axxim = mri_new( nA , mcov+1 , MRI_float ) ;
     axx   = MRI_FLOAT_PTR(axxim) ;
     for( kk=0 ; kk < shd_AAA->ndset ; kk++ ){  /* loop over datasets */
       ii = string_search( shd_AAA->dslab[kk] , /* find which covariate */
                           covnel->vec_len , (char **)covnel->vec[0] ) ;
       if( ii < 0 ){
         ERROR_message("Can't find dataset label '%s' in covariates file" ,
                       shd_AAA->dslab[kk] ) ;
         nbad++ ;
       } else {             /* ii-th row of covariates == kk-th dataset */
         AXX(kk,0) = 1.0f ;
         for( jj=1 ; jj <= mcov ; jj++ )
           AXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
       }
     }
     if( nbad == 0 ){  /* process the matrix */
       MRI_IMARR *impr ; float sum ;
       for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
         for( sum=0.0f,kk=0 ; kk < shd_AAA->ndset ; kk++ ) sum += AXX(kk,jj) ;
         sum /= shd_AAA->ndset ;
         for( kk=0 ; kk < shd_AAA->ndset ; kk++ ) AXX(kk,jj) -= sum ;
       }
       /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
       impr = mri_matrix_psinv_pair( axxim , 0.0f ) ;
       if( impr == NULL ) ERROR_exit("Can't process setA covariate matrix?! :-(") ;
       axxim_psinv  = IMARR_SUBIM(impr,0) ; axx_psinv  = MRI_FLOAT_PTR(axxim_psinv ) ;
       axxim_xtxinv = IMARR_SUBIM(impr,1) ; axx_xtxinv = MRI_FLOAT_PTR(axxim_xtxinv) ;

#if defined(COVTEST) && 0
       ININFO_message("axx matrix: %d X %d",axxim->nx,axxim->ny) ;
        mri_write_1D("stderr:",axxim) ;
       ININFO_message("axxim_psinv matrix: %d X %d",axxim_psinv->nx,axxim_psinv->ny) ;
        mri_write_1D("stderr:",axxim_psinv) ;
       ININFO_message("axxim_xtxinv matrix: %d X %d",axxim_xtxinv->nx,axxim_xtxinv->ny) ;
        mri_write_1D("stderr:",axxim_xtxinv) ;
#endif
     }

     /*--- setup the setB regression matrix ---*/

     if( shd_BBB != NULL && ttest_opcode != 2 ){  /* un-paired case */
       nB    = shd_BBB->ndset ;
       bxxim = mri_new( nB , mcov+1 , MRI_float ) ;
       bxx   = MRI_FLOAT_PTR(bxxim) ;
       for( kk=0 ; kk < shd_BBB->ndset ; kk++ ){  /* loop over datasets */
         ii = string_search( shd_BBB->dslab[kk] , /* find which covariate */
                             covnel->vec_len , (char **)covnel->vec[0] ) ;
         if( ii < 0 ){
           ERROR_message("Can't find dataset label '%s' in covariates file" ,
                         shd_BBB->dslab[kk] ) ;
           nbad++ ;
         } else {             /* ii-th row of covariates == kk-th dataset */
           BXX(kk,0) = 1.0f ;
           for( jj=1 ; jj <= mcov ; jj++ )
             BXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
         }
       }
       if( nbad == 0 ){  /* process the matrix */
         MRI_IMARR *impr ; float sum ;
         for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
           for( sum=0.0f,kk=0 ; kk < shd_BBB->ndset ; kk++ ) sum += BXX(kk,jj) ;
           sum /= shd_BBB->ndset ;
           for( kk=0 ; kk < shd_BBB->ndset ; kk++ ) BXX(kk,jj) -= sum ;
         }
         /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
         impr = mri_matrix_psinv_pair( bxxim , 0.0f ) ;
         if( impr == NULL ) ERROR_exit("Can't process setB covariate matrix?! :-(") ;
         bxxim_psinv  = IMARR_SUBIM(impr,0) ; bxx_psinv  = MRI_FLOAT_PTR(bxxim_psinv ) ;
         bxxim_xtxinv = IMARR_SUBIM(impr,1) ; bxx_xtxinv = MRI_FLOAT_PTR(bxxim_xtxinv) ;
       }

     } else if( shd_BBB != NULL && ttest_opcode == 2 ){  /* paired case */

       bxx = axx ; bxx_psinv = axx_psinv ; bxx_xtxinv = axx_xtxinv ;

     }

     if( nbad )
       ERROR_exit("Can't continue past the above covariates errors :-((") ;

   } /* covariates regression matrices now setup */

   /* scan through all the data, which will make it be page faulted
      into RAM, which will make the correlation-izing process faster;
      the downside is that this may take quite a while, which is boring */

#undef  BSTEP
#define BSTEP 256
   { long long pp , vstep=9 ; char *qv ; float sum=0.0f ;
     if( verb ) INFO_message("page faulting (reading) data into memory") ;
     if( shd_AAA->datum == 1 ) qv = (char *)shd_AAA->bv[0] ;
     else                      qv = (char *)shd_AAA->sv[0] ;
     if( verb ){
       vstep = (shd_AAA->nbytes / BSTEP) / 50 ; fprintf(stderr," + setA:") ;
     }
     for( pp=0 ; pp < shd_AAA->nbytes ; pp+=BSTEP,qv+=BSTEP ){
       sum += *qv ;
       if( verb && (pp/BSTEP)%vstep == vstep-1 ) vstep_print() ;
     }
     if( verb ){ fprintf(stderr,"!\n") ; vstep_n = 0 ; }
     if( shd_BBB != NULL ){
       if( shd_BBB->datum == 1 ) qv = (char *)shd_BBB->bv[0] ;
       else                      qv = (char *)shd_BBB->sv[0] ;
       if( verb ){
         vstep = (shd_BBB->nbytes / BSTEP) / 50 ; fprintf(stderr," + setB:") ;
       }
       for( pp=0 ; pp < shd_BBB->nbytes ; pp+=BSTEP,qv+=BSTEP ){
         sum += *qv ;
         if( verb && (pp/BSTEP)%vstep == vstep-1 ) vstep_print() ;
       }
       if( verb ){ fprintf(stderr,"!\n") ; vstep_n = 0 ; }
     }
     if( verb == 666 ) INFO_message(" data sum = %g",sum) ; /* e.g., never */
   }
#undef BSTEP

   if( verb ){
     long long nbtot = shd_AAA->nbytes ;
     if( shd_BBB != NULL ) nbtot += shd_BBB->nbytes ;
     INFO_message("total bytes input = %s (about %s)" ,
                   commaized_integer_string(nbtot) ,
                   approximate_number_string((double)nbtot) ) ;
   }

   /*-- Create VOLUME_DATA NIML element to hold the brick data --*/

   nelset = NI_new_data_element( "RestSym_dataset" , shd_AAA->nvec ) ;

   /*----- if no covariates, do it the olden style way -----*/

   if( mcov == 0 ){
     NI_add_column( nelset, NI_FLOAT, NULL );
     NI_add_column( nelset, NI_FLOAT, NULL );
     neldar = (float *)nelset->vec[0];  /* neldar = delta  sub-brick */
     nelzar = (float *)nelset->vec[1];  /* nelzar = Zscore sub-brick */
     if( neldar == NULL || nelzar == NULL )
       ERROR_exit("Can't setup output dataset?") ; /* should never happen */

     /* for a 2-sample test, create arrays for the 1-sample results as well */

     if( dosix ){
       NI_add_column( nelset, NI_FLOAT, NULL ); neldar_AAA = (float *)nelset->vec[2];
       NI_add_column( nelset, NI_FLOAT, NULL ); nelzar_AAA = (float *)nelset->vec[3];
       NI_add_column( nelset, NI_FLOAT, NULL ); neldar_BBB = (float *)nelset->vec[4];
       NI_add_column( nelset, NI_FLOAT, NULL ); nelzar_BBB = (float *)nelset->vec[5];
       if( neldar_AAA == NULL || nelzar_AAA == NULL ||
           neldar_BBB == NULL || nelzar_BBB == NULL   )
        ERROR_exit("Can't setup output dataset?") ; /* should never transpire */
     }

     nout = (dosix) ? 6 : 2 ;

   } else {  /* alternative setup for covariates results */

     nout = (dosix) ? 6*(mcov+1) : 2*(mcov+1) ;
     dtar = (float **)malloc(sizeof(float *)*nout) ;
     for( kk=0 ; kk < nout ; kk++ ){
       NI_add_column( nelset , NI_FLOAT , NULL ) ;
       dtar[kk] = (float *)nelset->vec[kk] ;
       if( dtar[kk] == NULL ) ERROR_exit("Can't setup output dataset?!") ;
     }
     if( shd_BBB != NULL ){
       testAB = (UINT32)(-1) ; testA  = testB = (dosix) ? testAB : 0 ;
     } else {
       testAB = testB = 0 ; testA = (UINT32)(-1) ;
     }

   }

   /*========= message for the user =========*/

   if( verb ){
     INFO_message    ("--- Be sure to start %s with the '-niml' command line option",pname) ;
     if( TalkToAfni ){
       ININFO_message("---  [or press the NIML+PO button if you forgot '-niml']") ;
       ININFO_message("--- Then open Define Overlay and pick GrpInCorr from the Clusters menu") ;
     }
   }

#ifdef NOZ
   /*========= this stuff is one-time-only setup of the I/O to AFNI =========*/
   /* DELETED */
#else
   fprintf(stderr,"++ Opening NIML socket file") ;
   GI_stream = NI_stream_open( "file:setup_element.niml" , "w" ) ;
#endif

   /** store some info about the dataset we are constructing **/

   nx = DSET_NX(shd_AAA->tdset) ; dx = fabsf(DSET_DX(shd_AAA->tdset)) ;
   ny = DSET_NY(shd_AAA->tdset) ; dy = fabsf(DSET_DY(shd_AAA->tdset)) ;
   nz = DSET_NZ(shd_AAA->tdset) ; dz = fabsf(DSET_DZ(shd_AAA->tdset)) ;
   dmin = MIN(dx,dy) ; dmin = MIN(dmin,dz) ; nxy = nx*ny ;

   /** now send our setup info to AFNI **/

   if( shd_AAA->nvec == shd_AAA->nvox ){
     nelcmd = NI_new_data_element( "RestSym_setup" , 0 ) ;  /* no data */
   } else {
     nelcmd = NI_new_data_element( "RestSym_setup" , shd_AAA->nvec ) ;
     NI_add_column( nelcmd , NI_INT , shd_AAA->ivec ) ;    /* data = indexes */
   }

   /* set various attributes to let AFNI know what's up, doc */

   sprintf(buf,"%d",shd_AAA->ndset) ;
   NI_set_attribute( nelcmd , "ndset_A" , buf ) ;

   sprintf(buf,"%d",(shd_BBB != NULL) ? shd_BBB->ndset : 0 ) ;
   NI_set_attribute( nelcmd , "ndset_B" , buf ) ;

   sprintf(buf,"%d",shd_AAA->nvec) ;
   NI_set_attribute( nelcmd , "nvec" , buf ) ;

   sprintf(buf,"%.2f",seedrad) ;
   NI_set_attribute( nelcmd , "seedrad" , buf ) ;

   sprintf(buf,"%d",ttest_opcode) ;
   NI_set_attribute( nelcmd , "ttest_opcode" , buf ) ;

   NI_set_attribute( nelcmd , "geometry_string", shd_AAA->geometry_string  ) ;
   NI_set_attribute( nelcmd , "target_name"    , "A_GRP_ICORR"             ) ;

   /* 04 Feb 2010: create sub-brick labels here, based on what we compute */

   NI_set_attribute( nelcmd , "label_AAA" , label_AAA ) ;
   if( shd_BBB != NULL )
     NI_set_attribute( nelcmd , "label_BBB" , label_BBB ) ;

   NI_set_attribute_int( nelcmd , "target_nvals" , nout ) ;

   if( mcov == 0 ){
     char bricklabels[32*MAX_LABEL_SIZE+64] ;

     if( shd_BBB == NULL ){  /* 1 sample */
       sprintf( bricklabels , "%s_mean ; %s_Zscr" , label_AAA , label_AAA ) ;
     } else {                /* 2 samples */
       sprintf( bricklabels , "%s-%s_mean ; %s-%s_Zscr" ,
                label_AAA , label_BBB , label_AAA , label_BBB ) ;
       if( dosix )           /* plus the extras */
         sprintf( bricklabels+strlen(bricklabels) ,
                  " ; %s_mean ; %s_Zscr ; %s_mean ; %s_Zscr" ,
                  label_AAA , label_AAA , label_BBB , label_BBB ) ;
     }
     NI_set_attribute( nelcmd , "target_labels" , bricklabels ) ;

   } else {  /* labels for the myriad of covariates results [23 May 2010] */
     char *bricklabels = (char *)calloc(sizeof(char),(3*MAX_LABEL_SIZE+16)*(nout+1)) ;

     if( testAB ){
       sprintf( bricklabels , "%s-%s_mean ; %s-%s_Zscr ;" ,
                label_AAA , label_BBB , label_AAA , label_BBB ) ;
       for( kk=1 ; kk <= mcov ; kk++ )
         sprintf( bricklabels+strlen(bricklabels) ,
                  "%s-%s_%s ; %s-%s_%s_Zscr ;" ,
                label_AAA , label_BBB , covlab->str[kk] ,
                label_AAA , label_BBB , covlab->str[kk]  ) ;
     }
     if( testA ){
       sprintf( bricklabels+strlen(bricklabels) ,
                "%s_mean ; %s_mean_Zscr ;" , label_AAA , label_AAA ) ;
       for( kk=1 ; kk <= mcov ; kk++ )
         sprintf( bricklabels+strlen(bricklabels) ,
                  "%s_%s ; %s_%s_Zscr ;" ,
                  label_AAA , covlab->str[kk] ,
                  label_AAA , covlab->str[kk]  ) ;
     }
     if( testB ){
       sprintf( bricklabels+strlen(bricklabels) ,
                "%s_mean ; %s_Zscr ;" , label_BBB , label_BBB ) ;
       for( kk=1 ; kk <= mcov ; kk++ )
         sprintf( bricklabels+strlen(bricklabels) ,
                  "%s_%s ; %s_%s_Zscr ;" ,
                  label_BBB , covlab->str[kk] ,
                  label_BBB , covlab->str[kk]  ) ;
     }

     kk = strlen(bricklabels) ; bricklabels[kk-1] = '\0' ;
     NI_set_attribute( nelcmd , "target_labels" , bricklabels ) ;
     free(bricklabels) ;
   }

   /* ZSS: set surface attributes */

   if (shd_AAA->nnode[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->nnode[0], shd_AAA->nnode[1]);
      NI_set_attribute( nelcmd , "LRpair_nnode", buf);
   }
   if (shd_AAA->ninmask[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->ninmask[0], shd_AAA->ninmask[1]);
      NI_set_attribute( nelcmd , "LRpair_ninmask", buf);
   }

#ifdef NOZ
   /* actually send the setup NIML element now */
   /* DELETED */
#else
   SUMA_LH("Running setup");
   nn = NI_write_element(GI_stream , nelcmd , NI_TEXT_MODE);
   if (!SUMA_RestSym_setup_func( NULL , nelcmd )) {
      ERROR_message("bonkers");
      exit(1);
   }
   if (nmaskname) {
      if (!(nmask = SUMA_load_1D_n_mask(nmaskname, shd_AAA->nnode[0], NULL, 
                                 "", &N_nmask))) {
         SUMA_S_Errv("Bad node mask %s\n", nmaskname);                          
         exit(1);
      }
      SUMA_S_Notev("Have %d nodes in mask file %s\n", N_nmask, nmaskname);
   }else {
      N_nmask  = shd_AAA->nnode[0];
      SUMA_S_Notev("Processing all %d nodes of left hemisphere\n",
                  N_nmask);
   }
#endif   
   if( nn < 0 ){
     ERROR_exit("Can't send setup data to %s!?",pname) ;
   }
   NI_free_element(nelcmd) ;
   
   /** make neighborhood struct for seedrad usage **/

   if( seedrad >= dmin ){
     nbhd = MCW_spheremask( dx,dy,dz , seedrad ) ;
     if( nbhd != NULL && nbhd->num_pt < 2 ) KILL_CLUSTER(nbhd) ;
   }

   /** make space for seed vectors and correlations **/

   seedvec_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   dotprod_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   for( kk=0 ; kk < ndset_AAA ; kk++ ){
     seedvec_AAA[kk] = (float *)malloc(sizeof(float)*nvals_AAA[kk]) ;
     dotprod_AAA[kk] = (float *)malloc(sizeof(float)*nvec) ;
   }

   if( shd_BBB != NULL ){
     seedvec_BBB = (float **)malloc(sizeof(float *)*ndset_BBB) ;
     dotprod_BBB = (float **)malloc(sizeof(float *)*ndset_BBB) ;
     for( kk=0 ; kk < ndset_BBB ; kk++ ){
       seedvec_BBB[kk] = (float *)malloc(sizeof(float)*nvals_BBB[kk]) ;
       dotprod_BBB[kk] = (float *)malloc(sizeof(float)*nvec) ;
     }
   }

   if( verb ){
     INFO_message("RestSym stands ready to do thy bidding :-) !!") ;
#ifdef USE_OMP
#pragma omp parallel
 {
  if( omp_get_thread_num() == 0 )
    ININFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#endif
   }

#ifdef COVTEST
   ctnam = getenv("GI_AAA_add") ;
   if( ctnam != NULL && strncmp(ctnam,"1D:",3) == 0 ){
     MRI_IMAGE *ctimm = mri_1D_fromstring(ctnam) ;
     if( ctimm != NULL && ctimm->nx >= shd_AAA->ndset ) ctarA = MRI_FLOAT_PTR(ctimm) ;
   }
   ctnam = getenv("GI_BBB_add") ;
   if( dotprod_BBB != NULL && ctnam != NULL && strncmp(ctnam,"1D:",3) == 0 ){
     MRI_IMAGE *ctimm = mri_1D_fromstring(ctnam) ;
     if( ctimm != NULL && ctimm->nx >= shd_BBB->ndset ) ctarB = MRI_FLOAT_PTR(ctimm) ;
   }
#endif

   /** now wait for commands from AFNI */
   
   inode = 0;
   btim_all = NI_clock_time() ;
   while(inode < shd_AAA->nnode[0]){  /* loop forever? */
      if (nmask && !nmask[inode]) {
         ++inode; /* skip */
         continue;
      }
#ifdef NOZ     
     /*  nelcmd = NI_read_element( GI_stream , 333 ) ; */ /* get command? */
     /* DELETED */
#endif
     /** start timer **/

     if( verb > 1 || (verb==1 && nsend < NSEND_LIMIT) )
       INFO_message("Request to compute for inode %d", inode) ;

     atim = btim = NI_clock_time() ;

     /** Command = set seed voxel index **/

#ifdef NOZ
     /* if( strcmp(nelcmd->name,"SETREF_ijk") == 0 ){ */
     /* DELETED */
#else
   voxijk = inode;
   voxind = IJK_TO_INDEX(shd_AAA,inode) ;
   
#endif
     /***** compute the result *****/

     /* step 1: for each dataset, get the seed voxel time series from voxind */

     GRINCOR_load_seedvec( shd_AAA , nbhd , voxijk , seedvec_AAA ) ;
     if( shd_BBB != NULL )
       GRINCOR_load_seedvec( shd_BBB , nbhd , voxijk , seedvec_BBB ) ;

     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
       ctim = NI_clock_time() ;
       ININFO_message(" loaded seed vectors: elapsed=%d ms",ctim-btim) ;
       btim = ctim ;
     }

     /* step 2: lots and lots of correlation-ization */

     if( verb > 3 ) ININFO_message(" start correlation-izing for %s",label_AAA) ;
     GRINCOR_many_dotprod( shd_AAA , seedvec_AAA , dotprod_AAA ) ;
     if( shd_BBB != NULL ){
       if( verb > 3 ) ININFO_message(" start correlation-izing for %s",label_BBB) ;
       GRINCOR_many_dotprod( shd_BBB , seedvec_BBB , dotprod_BBB ) ;
     }

#ifdef COVTEST
     if( ctarA != NULL ){
       for( kk=0 ; kk < ndset_AAA ; kk++ )
         for( jj=nvec/2 ; jj < nvec ; jj++ ) dotprod_AAA[kk][jj] += ctarA[kk] ;
     }
     if( ctarB != NULL ){
       for( kk=0 ; kk < ndset_BBB ; kk++ )
         for( jj=nvec/2 ; jj < nvec ; jj++ ) dotprod_BBB[kk][jj] += ctarB[kk] ;
     }
#endif

     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
       ctim = NI_clock_time() ;
       ININFO_message(" finished correlation-izing: elapsed=%d ms",ctim-btim) ;
       btim = ctim ;
     }

     /* step 3: lots of t-test-ification */

     if( mcov == 0 ){   /*-- no covariates ==> pure t-tests --*/

       if( verb > 2)
         ININFO_message(" start %d-sample t-test-izing" , (ndset_BBB > 0) ? 2 : 1 ) ;
       GRINCOR_many_ttest( nvec , ndset_AAA , dotprod_AAA ,
                                  ndset_BBB , dotprod_BBB , neldar,nelzar ) ;

       
       /* 1-sample results for the 2-sample case? */       
       if( dosix ){
         if( verb > 3 ) ININFO_message(" start 1-sample t-test-izing for %s",label_AAA) ;
         GRINCOR_many_ttest( nvec , ndset_AAA , dotprod_AAA ,
                                    0         , NULL        , neldar_AAA,nelzar_AAA ) ;
         if( verb > 3 ) ININFO_message(" start 1-sample t-test-izing for %s",label_BBB) ;
         GRINCOR_many_ttest( nvec , ndset_BBB , dotprod_BBB ,
                                    0         , NULL        , neldar_BBB,nelzar_BBB ) ;
       }

       if( verb > 3 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message(" finished t-test-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     } else {  /*-- covariates ==> regression analyses --*/

       if( verb > 2 )
         ININFO_message(" start %d-sample regression-izing" , (ndset_BBB > 0) ? 2 : 1 ) ;

       GRINCOR_many_regress( nvec , ndset_AAA , dotprod_AAA ,
                                    ndset_BBB , dotprod_BBB , nout , dtar ) ;

       if( verb > 3 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message(" finished regression-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     }

#ifdef NOZ
     /** re-attach to AFNI using shared memory? **/
     /* DELETED */
#endif
     ctim = NI_clock_time() ;

     if( verb > 3 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message(" Total elapsed time = %d msec",ctim-atim) ;

      if (!RestSym_Proc (nelset, inode, nsend+1, 14286000)) {
         SUMA_S_Errv("Failed to process dset, inode %d, nsend %d\n"
                     , inode, nsend);
         exit(1);
      }
     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message(" Done with RestSym_Proc ") ;

     nsend++ ;  /* number of results sent back so far */
   
   if (! (nsend % 100) ) {
      float ffr;
      ctim = NI_clock_time() ;
      ffr = (ctim-btim_all)/3600000.0/(float)nsend;
      ININFO_message(" %d/%d (%f%%) in %f minutes, this inode = %d\n"
                     "Total expected completion time %f hours.\n"
                     "Remaining completion time: %f hours (%f minutes)\n", 
            nsend, N_nmask, (float)nsend/N_nmask*100.0,
                     (ctim-btim_all)/60000.0, inode,
                     ffr*(float)N_nmask,
                  ffr*(float)(N_nmask-nsend), ffr*(float)(N_nmask-nsend)*60.0);
   }
  LoopBack: ; /* loop back for another command from AFNI */
   inode = inode + 1;
   }

   /*-- bow out gracefully --*/

GetOutOfDodge :
   SUMA_S_Note("All done, will write results");
   if (!SUMA_RestSym_Dsets(SOv, SUMAg_CF->giset, NULL, SUMAg_CF->DsetList, 
                         sdsetv, mxsets, NULL)) {
      SUMA_S_Err("Failed to find/create dsets for giset");
      SUMA_RETURN(NOPE);
   }
   /* update header attributes */
   SUMA_UpdateDsetColRange(mxsets[0],-1);
   SUMA_UpdateDsetColRange(mxsets[1],-1);
   
   /* write out results */
   if (!prefix) prefix = "Max";
   sprintf(buf,"%s.lh", prefix);
   SUMA_WriteDset_s(buf,   mxsets[0], SUMA_ASCII_NIML, 1, 1);
   sprintf(buf,"%s.rh", prefix);
   SUMA_WriteDset_s(buf, mxsets[1], SUMA_ASCII_NIML, 1, 1);
   /* NI_free_element(nelset) ; */

   INFO_message("Exeunt RestSym and its %s of data" ,
                (nsend%2 == 0) ? "trove" : "hoard") ;
   exit(0) ;
}

/*=======================================================================*/
/** The following routines are for the t-to-z conversion, and are
    adapted from mri_stats.c to be parallelizable (no static data).
=========================================================================*/

static double GIC_qginv( double p )
{
   double dp , dx , dt , ddq , dq ;
   int    newt ;                       /* not Gingrich, but Isaac */

   dp = (p <= 0.5) ? (p) : (1.0-p) ;   /* make between 0 and 0.5 */

   if( dp <= 1.e-37 ){
      dx = 13.0 ;                      /* 13 sigma has p < 10**(-38) */
      return ( (p <= 0.5) ? (dx) : (-dx) ) ;
   }

/**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/

   dt = sqrt( -2.0 * log(dp) ) ;
   dx = dt
        - ((.010328*dt + .802853)*dt + 2.515517)
        /(((.001308*dt + .189269)*dt + 1.432788)*dt + 1.) ;

/**  Step 2:  do 3 Newton steps to improve this
              (uses the math library erfc function) **/

   for( newt=0 ; newt < 3 ; newt++ ){
     dq  = 0.5 * erfc( dx / 1.414213562373095 ) - dp ;
     ddq = exp( -0.5 * dx * dx ) / 2.506628274631000 ;
     dx  = dx + dq / ddq ;
   }

   if( dx > 13.0 ) dx = 13.0 ;
   return ( (p <= 0.5) ? (dx) : (-dx) ) ;  /* return with correct sign */
}

#ifdef NO_GAMMA
/*-----------------------------------------------------------------------*/
/* If the system doesn't provide lgamma() for some primitive reason.
-------------------------------------------------------------------------*/

/**----- log of gamma, for argument between 1 and 2 -----**/

static double gamma_12( double y )
{
   double x , g ;
   x = y - 1.0 ;
   g = ((((((( 0.035868343 * x - 0.193527818 ) * x
                               + 0.482199394 ) * x
                               - 0.756704078 ) * x
                               + 0.918206857 ) * x
                               - 0.897056937 ) * x
                               + 0.988205891 ) * x
                               - 0.577191652 ) * x + 1.0 ;
   return log(g) ;
}

/**----- asymptotic expansion of ln(gamma(x)) for large positive x -----**/

#define LNSQRT2PI 0.918938533204672  /* ln(sqrt(2*PI)) */

static double gamma_asympt(double x)
{
   double sum ;

   sum = (x-0.5)*log(x) - x + LNSQRT2PI + 1.0/(12.0*x) - 1./(360.0*x*x*x) ;
   return sum ;
}

/**----- log of gamma, argument positive (not very efficient!) -----**/

static double GIC_lgamma( double x )
{
   double w , g ;

   if( x <= 0.0 ) return 0.0 ;  /* should not happen */

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;
   if( x >= 6.0 ) return gamma_asympt(x) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){ w -= 1.0 ; g += log(w) ; }
   return ( gamma_12(w) + g ) ;
}

#define lgamma GIC_lgamma

#endif  /*----- NO_GAMMA ------------------------------------------------*/

/*----------------------------------------------------------------------*/

static double GIC_lnbeta( double p , double q )
{
   return (lgamma(p) + lgamma(q) - lgamma(p+q)) ;
}

/*----------------------------------------------------------------------*/

#define ZERO 0.0
#define ONE  1.0
#define ACU  1.0e-15

static double GIC_incbeta( double x , double p , double q , double beta )
{
   double betain , psq , cx , xx,pp,qq , term,ai , temp , rx ;
   int indx , ns ;

   if( p <= ZERO || q <= ZERO ) return -1.0 ;  /* error! */

   if( x <= ZERO ) return ZERO ;
   if( x >= ONE  ) return ONE ;

   /**  change tail if necessary and determine s **/

   psq = p+q ;
   cx  = ONE-x ;
   if(  p < psq*x ){
      xx   = cx ; cx   = x ; pp   = q ; qq   = p ; indx = 1 ;
   } else {
      xx   = x ; pp   = p ; qq   = q ; indx = 0 ;
   }

   term   = ONE ;
   ai     = ONE ;
   betain = ONE ;
   ns     = qq + cx*psq ;

   /** use soper's reduction formulae **/

      rx = xx/cx ;

lab3:
      temp = qq-ai ;
      if(ns == 0) rx = xx ;

lab4:
      term   = term*temp*rx/(pp+ai) ;
      betain = betain+term ;
      temp   = fabs(term) ;
      if(temp <= ACU && temp <= ACU*betain) goto lab5 ;

      ai = ai+ONE ;
      ns = ns-1 ;
      if(ns >= 0) goto lab3 ;
      temp = psq ;
      psq  = psq+ONE ;
      goto lab4 ;

lab5:
      betain = betain*exp(pp*log(xx)+(qq-ONE)*log(cx)-beta)/pp ;
      if(indx) betain=ONE-betain ;

   return betain ;
}

/*----------------------------------------------------------------------*/

#undef  ZMAX
#define ZMAX 13.0

double GIC_student_t2z( double tt , double dof )
{
   double xx , pp , bb ;

   bb = GIC_lnbeta( 0.5*dof , 0.5 ) ;

   xx = dof/(dof + tt*tt) ;
   pp = GIC_incbeta( xx , 0.5*dof , 0.5 , bb ) ;

   if( tt > 0.0 ) pp = 1.0 - 0.5 * pp ;
   else           pp = 0.5 * pp ;

   xx = - GIC_qginv(pp) ;
   if( xx > ZMAX ) xx = ZMAX ; else if( xx < -ZMAX ) xx = -ZMAX ;
   return xx ;
}

/*=============================================================================*/

void GRINCOR_many_ttest( int nvec , int numx , float **xxar ,
                                    int numy , float **yyar ,
                                    float *dar , float *zar  )
{
   if( numy > 0 && yyar != NULL ){  /*--- 2 sample t-test ---*/

#pragma omp parallel
 { int ii,kk ; float *xar,*yar ; float_pair delzsc ;
   AFNI_OMP_START ;
   xar = (float *)malloc(sizeof(float)*numx) ;
   yar = (float *)malloc(sizeof(float)*numy) ;
#pragma omp for
     for( kk=0 ; kk < nvec ; kk++ ){
       for( ii=0 ; ii < numx ; ii++ ) xar[ii] = xxar[ii][kk] ;
       for( ii=0 ; ii < numy ; ii++ ) yar[ii] = yyar[ii][kk] ;
       delzsc  = ttest_toz( numx , xar , numy , yar , ttest_opcode ) ;
       dar[kk] = delzsc.a ; zar[kk] = delzsc.b ;
     }
   free(yar) ; free(xar) ;
   AFNI_OMP_END ;
 }

   } else {  /*--- 1 sample t-test ---*/

#pragma omp parallel
 { int kk,ii ; float *xar ; float_pair delzsc ;
   AFNI_OMP_START ;
   xar = (float *)malloc(sizeof(float)*numx) ;
#pragma omp for
     for( kk=0 ; kk < nvec ; kk++ ){
       for( ii=0 ; ii < numx ; ii++ ) xar[ii] = xxar[ii][kk] ;
       delzsc  = ttest_toz( numx , xar , 0 , NULL , ttest_opcode ) ;
       dar[kk] = delzsc.a ; zar[kk] = delzsc.b ;
     }
   free(xar) ; /* ZSS This should be freed I believe ...*/
   AFNI_OMP_END ;
 }

   }

   return ;
}

/*----------------------------------------------------------------------------*/
/*! Various sorts of t-tests; output = Z-score.
   - numx = number of points in the first sample (must be > 1)
   - xar  = array with first sample
   - numy = number of points in the second sample
             - numy = 0 ==> a 1 sample test of first sample against mean=0
  DISABLED   - numy = 1 ==> a 1 sample test of first sample against mean=yar[0]
             - numy > 1 ==> a 2 sample test; opcode determines what kind
   - opcode = 0 for unpaired test with pooled variance
   - opcode = 1 for unpaired test with unpooled variance
   - opcode = 2 for paired test (numx == numy is required)
   - The return value is the Z-score of the t-statistic.
*//*--------------------------------------------------------------------------*/

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode )
{
   float_pair result = {0.0f,0.0f} ;
   register int ii ; register float val ;
   float avx,sdx , avy,sdy , dof , tstat=0.0f,delta=0.0f ;
   int paired=(opcode==2) , pooled=(opcode==0) ;

#if 0
   /* check inputs for stoopidities or other things that need to be changed */

   if( numx < 2 || xar == NULL                 ) return result ; /* bad */
   if( paired && (numy != numx || yar == NULL) ) return result ; /* bad */
#endif

   if( numy < 2 || yar == NULL ){ numy = paired = pooled = 0 ; yar = NULL ; }

   if( paired ){   /* Case 1: paired t test */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii]-yar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-yar[ii]-avx; sdx += val*val; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ;  /* delta = diff in means */

   } else if( numy == 0 ){  /* Case 2: 1 sample test against mean==0 */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-avx ; sdx += val*val ; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ; /* delta = mean */

   } else {  /* Case 3: 2 sample test (pooled or unpooled) */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii] - avx ; sdx += val*val ; }

     avy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
     avy /= numy ; sdy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ){ val = yar[ii] - avy ; sdy += val*val ; }

     delta = avx - avy ; /* difference in means */

     if( sdx+sdy == 0.0f ){

            if( delta > 0.0f ) tstat =  19.0f ;
       else if( delta < 0.0f ) tstat = -19.0f ;
       else                    tstat =   0.0f ;
       dof = numx+numy-2.0f ;

     } else if( pooled ){  /* Case 3a: pooled variance estimate */

       sdx   = (sdx+sdy) / (numx+numy-2.0f) ;
       tstat = delta / sqrtf( sdx*(1.0f/numx+1.0f/numy) ) ;
       dof   = numx+numy-2.0f ;

     } else {       /* Case 3b: unpooled variance estimate */

       sdx  /= (numx-1.0f)*numx ; sdy /= (numy-1.0f)*numy ; val = sdx+sdy ;
       tstat = delta / sqrtf(val) ;
       dof   = (val*val) / (sdx*sdx/(numx-1.0f) + sdy*sdy/(numy-1.0f) ) ;

     }

   } /* end of all possible cases */

   result.a = delta ;
   result.b = (float)GIC_student_t2z( (double)tstat , (double)dof ) ;
   return result ;
}

/*---------------------------------------------------------------------------*/

#undef  PA
#undef  PB
#undef  XA
#undef  XB
#define PA(i,j) psinvA[(i)+(j)*mm]  /* i=0..mm-1 , j=0..numA-1 */
#define PB(i,j) psinvB[(i)+(j)*mm]
#define XA(i,j) xA[(i)+(j)*(nA)]    /* i=0..nA-1 , j=0..mm-1 */
#define XB(i,j) xB[(i)+(j)*(nB)]

#undef  xtxA
#undef  xtxB
#define xtxA(i) xtxinvA[(i)+(i)*mm] /* diagonal elements */
#define xtxB(i) xtxinvB[(i)+(i)*mm]

#undef  VBIG
#define VBIG 1.0e+24f

/*---------------------------------------------------------------------------*/
/*  opcode defines what to do for 2-sample tests:
      0 ==> unpaired, pooled variance
      1 ==> unpaired, unpooled variance (not yet implemented)
      2 ==> paired (numA==numB required)

    xA      = numA X (mcov+1) matrix -- in column-major order
    psinvA  = (mcov+1) X numA matrix -- in column-major order
    xtxinvA = (mcov+1) X (mcov+1) matrix = inv[xA'xA]
*//*-------------------------------------------------------------------------*/

#if defined(COVTEST) && 0
static int first=1 ;
#endif

#undef UNROLL

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             )
{
   int kt=0,nws , mm=mcov+1 , nA=numA , nB=numB ;
   float *betA=NULL , *betB=NULL , *zdifA=NULL , *zdifB=NULL ;
   float ssqA=0.0f , ssqB=0.0f , varA=0.0f , varB=0.0f ; double dof=0.0 ;
   register float val ; register int ii,jj,tt ;

   nws = 0 ;
   if( testA || testAB ){
     betA  = workspace + nws ; nws += mm ;
     zdifA = workspace + nws ; nws += nA ;
   }
   if( testB || testAB ){
     betB  = workspace + nws ; nws += mm ;
     zdifB = workspace + nws ; nws += nB ;
   }

   /*-- compute estimates for A parameters --*/

   if( testA || testAB ){
#ifndef UNROLL
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nA ; jj++ ) val += PA(ii,jj)*zA[jj] ;
       betA[ii] = val ;
     }
#else
     if( nA%2 == 0 ){
       for( ii=0 ; ii < mm ; ii++ ){
         val = 0.0f ;
         for( jj=0 ; jj < nA ; jj+=2 )
           val += PA(ii,jj)*zA[jj] + PA(ii,jj+1)*zA[jj+1] ;
         betA[ii] = val ;
       }
     } else {
       for( ii=0 ; ii < mm ; ii++ ){
         val = PA(ii,0)*zA[0] ;
         for( jj=1 ; jj < nA ; jj+=2 )
           val += PA(ii,jj)*zA[jj] + PA(ii,jj+1)*zA[jj+1] ;
         betA[ii] = val ;
       }
     }
#endif
     for( jj=0 ; jj < nA ; jj++ ){
       val = -zA[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XA(jj,ii)*betA[ii] ;
       zdifA[ii] = val ; ssqA += val*val ;
     }
     if( testA ){ varA = ssqA / (nA-mm) ; if( varA <= 0.0f ) varA = VBIG ; }
   }

   /*-- compute estimates for B parameters --*/

   if( testB || testAB ){
#ifndef UNROLL
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nB ; jj++ ) val += PB(ii,jj)*zB[jj] ;
       betB[ii] = val ;
     }
#else
     if( nB%2 == 0 ){
       for( ii=0 ; ii < mm ; ii++ ){
         val = 0.0f ;
         for( jj=0 ; jj < nB ; jj+=2 )
           val += PB(ii,jj)*zB[jj] + PB(ii,jj+1)*zB[jj+1] ;
         betB[ii] = val ;
       }
     } else {
       for( ii=0 ; ii < mm ; ii++ ){
         val = PB(ii,0)*zB[0] ;
         for( jj=1 ; jj < nB ; jj+=2 )
           val += PB(ii,jj)*zB[jj] + PB(ii,jj+1)*zB[jj+1] ;
         betB[ii] = val ;
       }
     }
#endif
     for( jj=0 ; jj < nB ; jj++ ){
       val = -zB[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XB(jj,ii)*betB[ii] ;
       zdifB[ii] = val ; ssqB += val*val ;
     }
     if( testB ){ varB = ssqB / (nB-mm) ; if( varB <= 0.0f ) varB = VBIG ; }
   }

   /*-- carry out 2-sample (A-B) tests, if any --*/

   if( testAB ){
     float varAB ;

     if( opcode == 2 ){  /* paired (nA==nB, xA==xB, etc.) */

       for( varAB=0.0f,ii=0 ; ii < nA ; ii++ ){
         val = zdifA[ii] - zdifB[ii] ; varAB += val*val ;
       }
       varAB /= (nA-mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA - mm ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         val          = outvec[kt-1] / sqrtf( varAB*xtxA(tt) ) ;
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }

     } else {            /* unpaired, pooled variance */

       varAB = (ssqA+ssqB)/(nA+nB-2*mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA + nB - 2*mm ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         val          = outvec[kt-1] / sqrtf( varAB*(xtxA(tt)+xtxB(tt)) );
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }
     } /* end of unpaired pooled variance */
   }

   /*-- carry out 1-sample A tests, if any --*/

   if( testA ){
#if defined(COVTEST) && 0
#pragma omp critical
     { if( first ){
         first = 0 ;
         fprintf(stderr,"testA varA=%g xtxA=",varA) ;
         for( tt=0 ; tt < mm ; tt++ ) fprintf(stderr," %g",xtxA(tt)) ;
         fprintf(stderr,"\n") ;
       }
     }
#endif
     dof = nA - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testA & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betA[tt] ;
       val          = betA[tt] / sqrtf( varA * xtxA(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }

   /*-- carry out 1-sample B tests, if any --*/

   if( testB ){
     dof = nB - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testB & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betB[tt] ;
       val          = betB[tt] / sqrtf( varB * xtxB(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }

   return ;
}

/*---------------------------------------------------------------------------*/

void GRINCOR_many_regress( int nvec , int numx , float **xxar ,
                                      int numy , float **yyar ,
                                      int nout , float **dtar  )
{
   if( numy > 0 && yyar != NULL ){  /*--- 2 sample ---*/
#pragma omp parallel
   { int ii,kk ; float *xar,*yar,*var,*wss ;
     AFNI_OMP_START ;
     xar = (float *)malloc(sizeof(float)*numx) ;
     yar = (float *)malloc(sizeof(float)*numy) ;
     var = (float *)malloc(sizeof(float)*nout) ;
     wss = (float *)malloc(sizeof(float)*(2*mcov+numx+numy+9)) ;
#pragma omp for
     for( kk=0 ; kk < nvec ; kk++ ){
       for( ii=0 ; ii < numx ; ii++ ) xar[ii] = xxar[ii][kk] ;
       for( ii=0 ; ii < numy ; ii++ ) yar[ii] = yyar[ii][kk] ;
       regress_toz( numx , xar , numy , yar , ttest_opcode ,
                    mcov ,
                    axx , axx_psinv , axx_xtxinv ,
                    bxx , bxx_psinv , bxx_xtxinv , var , wss ) ;
       for( ii=0 ; ii < nout ; ii++ ) dtar[ii][kk] = var[ii] ;
     }
     free(wss) ; free(var) ; free(yar) ; free(xar) ;
     AFNI_OMP_END ;

   }} else {  /*--- 1 sample ---*/

#pragma omp parallel
   { int ii,kk ; float *xar,*var,*wss ;
     AFNI_OMP_START ;
     xar = (float *)malloc(sizeof(float)*numx) ;
     var = (float *)malloc(sizeof(float)*nout) ;
     wss = (float *)malloc(sizeof(float)*(2*mcov+numx+9)) ;
#pragma omp for
     for( kk=0 ; kk < nvec ; kk++ ){
       for( ii=0 ; ii < numx ; ii++ ) xar[ii] = xxar[ii][kk] ;
       regress_toz( numx , xar , 0 , NULL , ttest_opcode ,
                    mcov ,
                    axx , axx_psinv , axx_xtxinv ,
                    NULL, NULL      , NULL       , var , wss ) ;
       for( ii=0 ; ii < nout ; ii++ ) dtar[ii][kk] = var[ii] ;
     }
     free(wss) ; free(var) ; free(xar) ;
     AFNI_OMP_END ;
   }}

   return ;
}
