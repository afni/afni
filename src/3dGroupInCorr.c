/* Group InstaCorr == GrpInCorr */

#include "mrilib.h"

#ifdef USE_OMP
#include "omp.h"
#include "thd_ttest.c"  /* to make sure it's compiled in with OpenMP */
#endif

/*--------------------------------------------------------------------------*/

typedef struct {

  int nvec  ;  /* number of vectors in a dataset */
  int ndset ;  /* number of datasets */
  int *nvals ; /* nvals[i] = number of values in a vector in i-th dataset */

  char *geometry_string ;
  THD_3dim_dataset *tdset ; /* template dataset */

  char *dfname ; /* data file name */
  int  *ivec   ;  /* ivec[i] = spatial index of i-th vector, i=0..nvec-1 */
  float *fac   ;  /* fac[i] = scale factor for i-th dataset, i=0..ndset-1 */
  short **sv   ;  /* sv[i] = array [nvals[i]*nvec] for i-th dataset */

  long long nbytes ;

} MRI_shindss ;  /* short indexed datasets */

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

static long long twogig = 2ll * 1024ll * 1024ll * 1024ll ;  /* 2 GB */

MRI_shindss * GRINCOR_read_input( char *fname )
{
   NI_element *nel=NULL ;
   char *dfname=NULL , *atr ;
   NI_float_array *facar ; NI_int_array *nvar ;
   MRI_shindss *shd ;
   long long nbytes_needed , nbytes_dfname ; int fdes ;
   void *var ; int ids ;

   char *geometry_string=NULL ;
   THD_3dim_dataset *tdset=NULL;
   int need_ivec=0 , *ivec=NULL , *nvals=NULL , nvec,ndset ; float *fac=NULL ;

   if( fname == NULL || *fname == '\0' ) GQUIT(NULL) ;

   /* get data element */

   nel = NI_read_element_fromfile(fname) ;
   if( nel == NULL || nel->type != NI_ELEMENT_TYPE )
     GQUIT("not properly formatted") ;
   if( strcmp(nel->name,"3dGroupInCorr") != 0 )
     GQUIT("data element name is not '3dGroupInCorr'") ;

   /* headerless ==> using all voxels */

   need_ivec = ( nel->vec_num < 1 ||
                 nel->vec_len < 1 || nel->vec_typ[0] != NI_INT ) ;

   /* number of vectors in each dataset */

   atr = NI_get_attribute(nel,"nvec");
   if( atr == NULL ) GQUIT("nvec attribute missing?") ;
   nvec = (int)strtod(atr,NULL) ;
   if( nvec < 2 || (!need_ivec && nel->vec_len != nvec) )
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

   /* number of bytes needed:
        sizeof(short) * number of vectors per dataset
                      * number of datasets
                      * sum of per dataset vector lengths */

   nbytes_needed = 0 ;
   for( ids=0 ; ids < ndset ; ids++ ) nbytes_needed += nvals[ids] ;
   nbytes_needed *= ((long long)nvec) * sizeof(short) ;

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
   if(  need_ivec && DSET_NVOX(tdset) != nvec )
     GQUIT("geometry attribute doesn't match nvec attribute") ;
   if( !need_ivec && DSET_NVOX(tdset) <  nvec )
     GQUIT("geometry attribute specifies too few voxels") ;

   /* name of data file: check its size against what's needed */

   atr = NI_get_attribute(nel,"datafile") ;
   if( atr == NULL ) GQUIT("datafile attribute missing") ;
   dfname = strdup(atr) ; nbytes_dfname = THD_filesize(dfname) ;
   if( nbytes_dfname <= 0 )
     GQUIT("datafile is missing") ;
   else if( nbytes_dfname < nbytes_needed ){
     char str[2048] ;
     sprintf(str,"datafile has %lld bytes but needs at least %lld",
             nbytes_dfname , nbytes_needed ) ;
     GQUIT(str) ;
   }
   fdes = open( dfname , O_RDONLY ) ;
   if( fdes < 0 ) GQUIT("can't open datafile") ;

   /* ivec[i] is the voxel spatial index of the i-th vector */

   if( need_ivec ){
     ivec = (int *)malloc(sizeof(int)*nvec) ;
     for( ids=0 ; ids < nvec ; ids++ ) ivec[ids] = ids ;
   } else {
     ivec = (int *)nel->vec[0] ; /* copy pointer */
     nel->vec[0] = NULL ;        /* NULL out in element so won't be free-ed */
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

   shd->ivec = ivec ;
   shd->fac  = fac  ;

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

   shd->sv    = (short **)malloc(sizeof(short *)*ndset) ;
   shd->sv[0] = (short *)var ;
   for( ids=1 ; ids < ndset ; ids++ )
     shd->sv[ids] = shd->sv[ids-1] + nvals[ids-1]*nvec ;

   shd->nbytes = nbytes_needed ;
   return shd ;
}

#undef GQUIT

/*--------------------------------------------------------------------------*/
/* This cute little function consumes a lot of CPU time. */

void GRINCOR_dotprod( MRI_shindss *shd, int ids, float *vv, float *dp )
{
   int nvec = shd->nvec , nvals = shd->nvals[ids] , iv,ii ;
   float sum , fac = shd->fac[ids] ;
   short *sv = shd->sv[ids] , *ssv ;

   for( iv=0 ; iv < nvec ; iv++ ){
     ssv = sv + iv*nvals ;
     for( sum=0.0f,ii=0 ; ii < nvals ; ii++ ) sum += vv[ii]*ssv[ii] ;
     dp[iv] = atanhf(sum*fac) ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/

void GRINCOR_many_dotprod( MRI_shindss *shd , float **vv , float **dp )
{
   int ndset=shd->ndset ;

#pragma omp parallel
 { int ids ;
   AFNI_OMP_START ;
#pragma omp for
   for( ids=0 ; ids < ndset ; ids++ ){
     GRINCOR_dotprod( shd , ids , vv[ids] , dp[ids] ) ;
   }
   AFNI_OMP_END ;
 }

   return ;
}

/*--------------------------------------------------------------------------*/

static char *afnihost       = "localhost" ;
static MRI_shindss *shd_AAA = NULL ;
static MRI_shindss *shd_BBB = NULL ;
static int ttest_opcode     = -1   ;  /* 0=pooled, 1=pooled, 2=paired */

int main( int argc , char *argv[] )
{
   int nopt ;
   long long nbtot ;

   /*-- enlighten the ignorant and brutish sauvages? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dGroupInCorr [options]\n"
      "\n"
      "* This program operates as a server for AFNI.  It reads in dataset\n"
      "  collections that have been prepared by 3dSetupGroupInCorr, and\n"
      "  it connects to the AFNI GUI program (via TCP/IP).  Then it waits\n"
      "  for a command to be sent from AFNI before it actually does anything.\n"
      "\n"
      "* When AFNI sends a seed voxel command, 3dGroupInCorr will extract\n"
      "  that voxel times series from each input dataset, will compute the\n"
      "  correlation map of each dataset with the appropriate seed time\n"
      "  series, then will compute the t-test of that collection of\n"
      "  correlation maps, and return the resulting 3D volumes to AFNI\n"
      "  for display.\n"
      "\n"
      "* You must start AFNI with the '-niml' option to allow it to accept\n"
      "  incoming TCP/IP socket connections.\n"
      " ++ Or you can press the 'NIML+PO' button in the GUI, if you forgot\n"
      "    to type the command line correctly.\n"
      " ++ If you are running 3dGroupInCorr and AFNI on separate computers,\n"
      "    you also have to setup 'host trusting' correctly -- for details,\n"
      "    see the description of the '-ah' option, far below.\n"
      "\n"
      "* More detailed outline of processing:\n"
      " ++ For each 3D+time dataset in the input dataset collections:\n"
      "   -- Extract the seed voxel time series\n"
      "   -- Correlate it with all other voxels in the same dataset\n"
      "   -- Result is one 3D correlation map per input dataset\n"
      " ++ Then carry out the t-test between these 3D correlation maps.\n"
      "   -- Actually, between the arctanh() of these maps (cf. RA Fisher).\n"
      " ++ The dataset returned to AFNI converts the t-statistic maps\n"
      "    to z-scores, for various reasons of convenience.\n"
#ifdef USE_OMP
      "\n"
      "* One reason this program is a server (rather than being built in\n"
      "  to AFNI) is that it is compiled to use OpenMP, which will let\n"
      "  it make use of multiple CPU cores on the computer system.\n"
      " ++ For more information, see the end of this '-help' output.\n"
#endif
      "\n"
      "=======\n"
      "OPTIONS\n"
      "=======\n"
      "\n"
      " -setA AAA.grpincorr.niml\n"
      "   = Give the setup file (from 3dSetupGroupInCorr) that describes\n"
      "     the first dataset collection:\n"
      "  ++ This 'option' is mandatory (you have to input SOMETHING).\n"
      "  ++ Of course, 'AAA' should be replaced with the correct name of\n"
      "     your input dataset collection file!\n"
      "\n"
      " -setB BBB.grpincorr.niml\n"
      "   = Give the setup file that describes the second dataset collection:\n"
      "  ++ This option IS optional.\n"
      "  ++ If you use only -setA, then the program computes a one-sample t-test.\n"
      "  ++ If you use also -setB, then the program computes a two-sample t-test.\n"
      "    -- The exact form of the t-test used is controlled by one of the next\n"
      "       three options (which are mutually exclusive):\n"
      "  ++ The sign of a two sample t-test is 'A-B'; that is, a positive result\n"
      "     means that the A set of correlations average larger than the B set.\n"
      "\n"
      " -pooled   = For a two-sample un-paired t-test, use a pooled variance estimator\n"
      "             [this is the default]\n"
      " -unpooled = For a two-sample un-paired t-test, use an unpooled variance estimator\n"
      " -paired   = Use a two-sample paired t-test\n"
      "            ++ Which is the same as subtracting the two sets of 3D correlation\n"
      "               maps, then doing a one-sample t-test.\n"
      "            ++ To use '-paired', the number of datasets in each collection\n"
      "               must be the same, and the datasets must have been input to\n"
      "               3dSetupGroupInCorr in the same relative order when each\n"
      "               collection was created. (Duh.)\n"
      "\n"
      " -ah host  = Connect to AFNI on the computer named 'host', rather than on\n"
      "             the current computer system 'localhost'.\n"
      "            ++ This allows 3dGroupInCorr to run on a separate system than\n"
      "               the AFNI GUI.\n"
      "              -- e.g., If your desktop is wimpy, but you have access to\n"
      "                 a strong and muscular multi-CPU server (and the network\n"
      "                 connection is fast).\n"
      "            ++ Note that AFNI must be setup with the appropriate\n"
      "               'AFNI_TRUSTHOST_xx' environment variable, so that it will\n"
      "               allow the external socket connection (for the sake of security):\n"
      "             -- Example: AFNI running on computer 137.168.0.3 and 3dGroupInCorr\n"
      "                running on computer 137.168.0.7\n"
      "             -- Start AFNI with a command like\n"
      "                  afni -DAFNI_TRUSTHOST_01=137.168.0.7 -niml ...\n"
      "             -- Start 3dGroupInCorr with a command like\n"
      "                  3dGroupInCorr -ah 137.168.0.3 ...\n"
      "             -- You may use hostnames in place of IP addresses, but numerical\n"
      "                IP addresses will probably work more reliably.\n"
      "             -- If you are very trusting, you can set NIML_COMPLETE_TRUST to YES\n"
      "                to allow socket connections from anybody.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dGroupInCorr",NULL) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- process command line options --*/

   nopt = 1 ;
   while( nopt < argc ){

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

     if( strcasecmp(argv[nopt],"-setA") == 0 ){
       if( shd_AAA != NULL ) ERROR_exit("can only use '-setA' once!") ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       shd_AAA = GRINCOR_read_input( argv[nopt] ) ;
       if( shd_AAA == NULL ) ERROR_exit("Cannot continue after -setA input error") ;
       INFO_message("-setA loaded, using %lld bytes",shd_AAA->nbytes) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-setB") == 0 ){
       if( shd_BBB != NULL ) ERROR_exit("can only use '-setB' once!") ;
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       shd_BBB = GRINCOR_read_input( argv[nopt] ) ;
       if( shd_BBB == NULL ) ERROR_exit("Cannot continue after -setB input error") ;
       INFO_message("-setB loaded, using %lld bytes",shd_BBB->nbytes) ;
       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
   }

   /*-- check inputs for OK-ness --*/

   if( shd_AAA == NULL ) ERROR_exit("You must use the '-setA' option!") ;

   if( shd_BBB != NULL && shd_AAA->nvec != shd_BBB->nvec )
     ERROR_exit("-setA and -setB don't have same number of voxels") ;

   if( shd_BBB != NULL && strcmp(shd_AAA->dfname,shd_BBB->dfname) == 0 )
     ERROR_exit("-setA and -setB can't use the same datafile!") ;

   if( shd_BBB == NULL && ttest_opcode >= 0 )
     WARNING_message("Specifying form of 2-sample t-test means nothing without '-setB'");

   nbtot = shd_AAA->nbytes ;
   if( shd_BBB != NULL ) nbtot += shd_BBB->nbytes ;
   INFO_message("total bytes read  = %lld (about %s)" ,
                nbtot , approximate_number_string((double)nbtot) ) ;

   INFO_message("Be sure to start afni with the '-niml' option") ;

   /*-- at this point, should actually do some work --*/

   ERROR_exit("This program doesn't do anything (yet) **") ;

   exit(0) ;
}
