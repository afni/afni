/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <string.h>
#include "mrilib.h"
#include <stdlib.h>
#include <ctype.h>

/*-------------------------- global data --------------------------*/

/** inputs **/

static int PC_dmean      = 0 ; /* default is not to remove means */
static int PC_vmean      = 0 ;
static int PC_vnorm      = 0 ; /* 07 July 1999 */
static int PC_normalize  = 0 ; /* and not to normalize */
static int PC_lprin_save = 0 ; /* # of principal components to save */
static int PC_be_quiet   = 1 ; /* quiet is the default */
static int PC_do_float   = 0 ; /* shorts are the default */
static int PC_samp_scale = 0; /* Scale covariance matrix 
                                 (and consequently the eigen values) by 
                                 the number of samples */
static char **PC_dsname = NULL ; /* dataset names */
static int    PC_dsnum  = 0    ; /* number of them */
static int    PC_brnum  = 0    ; /* number of bricks */
static int    PC_1ddum  = 0    ; /* number of dummies for 1D files */

#define PC_lprin_calc PC_brnum

static THD_3dim_dataset **PC_dset = NULL ; /* pointers to datasets */

static char PC_prefix[THD_MAX_PREFIX] = "pc" ;

static float **PC_brickdata ;   /* pointer to data bricks */

static byte *PC_mask      = NULL ;   /* 15 Sep 1999 */
static int   PC_mask_nvox = 0 ;
static int   PC_mask_hits = 0 ;

static int   PC_eigonly   = 0 ;      /* 21 Feb 2008 */
static int   PC_reduce    = 0 ;      /* 22 Feb 2008 */
static char *PC_reprefix  = NULL ;

/*--------------------------- useful macros ------------------------*/

   /** i'th element of j'th input brick (i=voxel dim, j=time dimen) **/

#define XX(i,j) PC_brickdata[(j)][(i)]

   /** i,j element of covariance matrix **/

#define AA(i,j) (aa[(i)+(j)*adim])

   /** i'th element of j'th eigenvector **/

#define VV(i,j) (vout[(i)+(j)*adim])

   /** truncated projection matrix **/

#define PP(i,j) (pp[(i)+(j)*adim])   /* 22 Feb 2008 */

/*--------------------------- prototypes ---------------------------*/

void PC_read_opts( int , char ** ) ;
void PC_syntax(void) ;

#include "cs.h"  /** Use EISPACK **/

/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

void PC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  kk, nx, ny, nz, nxyz, mm,nn ;
   THD_3dim_dataset * dset ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -float ****/

      if( strncmp(argv[nopt],"-float",6) == 0 ){
         PC_do_float = 1 ;
         nopt++ ; continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc )
           ERROR_exit("need argument after -prefix!") ;
         MCW_strncpy( PC_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         if( !THD_filename_ok(PC_prefix) )
           ERROR_exit("Illegal argument after -prefix!") ;
         continue ;
      }

      if( strcmp(argv[nopt],"-eigonly") == 0 ){  /* 21 Feb 2008 */
        PC_eigonly = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-reduce") == 0 ){  /* 22 Feb 2008 */
        nopt++ ;
        if( nopt+1 >= argc )
          ERROR_exit("need 2 arguments after -reduce!") ;
        PC_reduce = (int)strtod(argv[nopt++],NULL) ;
        if( PC_reduce <= 0 )
          ERROR_exit("need positive value after -reduce!") ;
        PC_reprefix = strdup(argv[nopt]) ;
        if( !THD_filename_ok(PC_reprefix) )
          ERROR_exit("Illegal prefix after -reduce!") ;
        nopt++ ; continue ;
      }

      /**** -nscale ****/

      if( strncmp(argv[nopt],"-nscale",6) == 0 ){
         PC_samp_scale = 1 ;
         nopt++ ; continue ;
      }

      /**** -dmean ****/

      if( strncmp(argv[nopt],"-dmean",6) == 0 ){
         PC_dmean = 1 ;
         nopt++ ; continue ;
      }

      /**** -vmean ****/

      if( strncmp(argv[nopt],"-vmean",6) == 0 ){
         PC_vmean = 1 ;
         nopt++ ; continue ;
      }

      /**** -vnorm ****/

      if( strncmp(argv[nopt],"-vnorm",6) == 0 ){
         PC_vnorm = 1 ;
         nopt++ ; continue ;
      }

      /**** -normalize ****/

      if( strncmp(argv[nopt],"-normalize",6) == 0 ){
         PC_normalize = 1 ;
         nopt++ ; continue ;
      }

      /**** -pcsave # ****/

      if( strncmp(argv[nopt],"-pcsave",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) ERROR_exit("need argument after -pcsave!") ;
         if (!strcmp(argv[nopt],"ALL")) {
            PC_lprin_save = 987654321;
         } else {
            PC_lprin_save = strtol( argv[nopt] , NULL , 10 ) ;
         }
         if( PC_lprin_save < 0 ) ERROR_exit("value after -pcsave is illegal!") ;
         nopt++ ; continue ;
      }

      /**** -1ddum # ****/

      if( strncmp(argv[nopt],"-1ddum",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) ERROR_exit("need argument after -1ddum!") ;
         PC_1ddum = strtol( argv[nopt] , NULL , 10 ) ;
         if( PC_1ddum < 0 ) ERROR_exit("value after -1ddum is illegal!") ;
         nopt++ ; continue ;
      }

      /**** -mask mset [15 Sep 1999] ****/

      if( strncmp(argv[nopt],"-mask",5) == 0 ){
         THD_3dim_dataset * mset ; int ii,mc ;
         nopt++ ;
         if( nopt >= argc ) ERROR_exit("need argument after -mask!") ;
         mset = THD_open_dataset( argv[nopt] ) ;
         if( mset == NULL ) ERROR_exit("can't open -mask dataset!") ;
         PC_mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
         PC_mask_nvox = DSET_NVOX(mset) ;
         DSET_delete(mset) ;
         if( PC_mask == NULL ) ERROR_exit("can't use -mask dataset!") ;
         for( ii=mc=0 ; ii < PC_mask_nvox ; ii++ ) if( PC_mask[ii] ) mc++ ;
         if( mc == 0 ) ERROR_exit("mask is all zeros!") ;
         if( !PC_be_quiet ) INFO_message("%d voxels in mask",mc) ;
         PC_mask_hits = mc ;
         nopt++ ; continue ;
      }

      /**** -verbose ****/

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         PC_be_quiet = 0 ;
         nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-quiet",2) == 0 ){  /* 21 Feb 2008 */
         PC_be_quiet = 2 ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      ERROR_exit("unrecognized option '%s'",argv[nopt]) ;

   }  /* end of loop over options */

   /*--- a simple consistency check ---*/

   if( PC_vmean && PC_dmean )
     ERROR_exit("can't have both -dmean and -vmean!") ;

   /*--- rest of inputs are dataset names ---*/

   PC_dsnum  = argc - nopt ;
   if( PC_dsnum < 1 ) ERROR_exit("no input dataset names?") ;

   PC_dsname = (char **) malloc( sizeof(char *) * PC_dsnum ) ;
   for( kk=0 ; kk < PC_dsnum ; kk++ ) PC_dsname[kk] = argv[kk+nopt] ;

   PC_dset = (THD_3dim_dataset **) malloc( sizeof(THD_3dim_dataset *) * PC_dsnum ) ;
   for( kk=0 ; kk < PC_dsnum ; kk++ ){
     PC_dset[kk] = dset = THD_open_dataset( PC_dsname[kk] ) ;  /* allow for selector */
     if( !ISVALID_3DIM_DATASET(dset) )
       ERROR_exit("can't open dataset file %s",PC_dsname[kk]) ;
     PC_brnum += DSET_NVALS(dset) ;
   }
   if( PC_brnum < 2 ) ERROR_exit("need at least 2 input bricks!") ;

   /*--- another consistency check ---*/

   if( PC_be_quiet < 2 ){
     if( PC_lprin_save <= 0 ){
       WARNING_message("Not saving any eigen-timeseries or eigen-bricks") ;
     } else if( PC_lprin_save > PC_brnum ){
       PC_lprin_save = PC_brnum ;
       WARNING_message("saving all %d components",PC_brnum) ;
     }
   }

   /*--- load bricks for all input datasets ---*/

   nx = DSET_NX(PC_dset[0]) ;
   ny = DSET_NY(PC_dset[0]) ;
   nz = DSET_NZ(PC_dset[0]) ;
   nxyz = nx * ny * nz ;      /* Total number of voxels per brick */

   /* 15 Sep 1999: check if mask is right size */

   if( PC_mask_nvox > 0 && PC_mask_nvox != nxyz )
      ERROR_exit("mask and input dataset bricks don't match in size!") ;

   PC_brickdata = (float **) malloc( sizeof(float *) * PC_brnum ) ;

   nn = 0 ; /* current brick index: will be incremented below */

   if( !PC_be_quiet ) INFO_message("read dataset %d bricks", PC_brnum) ;

   for( kk=0 ; kk < PC_dsnum ; kk++ ){

      if( DSET_NVOX(PC_dset[kk]) != nxyz ){
         ERROR_exit(
                 "dataset in file %s nonconformant with first dataset!\n"
                 "**   nx1=%d ny1=%d nz1=%d nx=%d ny=%d nz=%d",
                 PC_dsname[kk], nx, ny, nz ,
                 DSET_NX(PC_dset[kk]), DSET_NY(PC_dset[kk]), DSET_NZ(PC_dset[kk]) ) ;
      }

      DSET_load( PC_dset[kk] ) ;  CHECK_LOAD_ERROR(PC_dset[kk]) ;

      /* copy brick data into float storage */

      for( mm=0 ; mm < DSET_NVALS(PC_dset[kk]) ; mm++,nn++ ){

         PC_brickdata[nn] = (float *) malloc( sizeof(float) * nxyz ) ;

         if( PC_brickdata[nn] == NULL )
           ERROR_exit("*** can't malloc intermediate storage") ;

         EDIT_coerce_type( nxyz , DSET_BRICK_TYPE(PC_dset[kk],mm) ,
                                  DSET_ARRAY(PC_dset[kk],mm) ,
                           MRI_float , PC_brickdata[nn] ) ;

         DSET_unload_one( PC_dset[kk] , mm ) ;

         if( PC_mask != NULL ){             /* 15 Sep 1999 */
           int kk ;
           for( kk=0 ; kk < nxyz ; kk++ )
             if( !PC_mask[kk] ) PC_brickdata[nn][kk] = 0.0 ;
         }

      } /* end of loop over sub-bricks of this dataset */

      if( kk == 0 ){
        DSET_unload( PC_dset[0] ) ;  /* don't need dataset's data anymore */
      } else {
        DSET_delete( PC_dset[kk] ) ;  /* don't need this at all anymore */
        PC_dset[kk] = NULL ;
      }

   } /* end of loop over datasets */

   free(PC_dsname) ; PC_dsname = NULL ;
   return ;
}

/*---------------------------------------------------------------------------*/

void PC_syntax(void)
{
   printf(
    "Principal Component Analysis of 3D Datasets\n"
    "Usage: 3dpc [options] dataset dataset ...\n"
    "\n"
    "Each input dataset may have a sub-brick selector list.\n"
    "Otherwise, all sub-bricks from a dataset will be used.\n"
    "\n"
    "OPTIONS:\n"
    "  -dmean        = remove the mean from each input brick (across space)\n"
    "  -vmean        = remove the mean from each input voxel (across bricks)\n"
    "                    [N.B.: -dmean and -vmean are mutually exclusive]\n"
    "                    [default: don't remove either mean]\n"
    "  -vnorm        = L2 normalize each input voxel time series\n"
    "                    [occurs after the de-mean operations above,]\n"
    "                    [and before the brick normalization below. ]\n"
    "  -normalize    = L2 normalize each input brick (after mean subtraction)\n"
    "                    [default: don't normalize]\n"
    "  -nscale       = Scale the covariance matrix by the number of samples\n"
    "                  This is not done by default for backward compatibility.\n"
    "                  You probably want this option on. \n"
    "  -pcsave sss   = 'sss' is the number of components to save in the output;\n"
    "                    it can't be more than the number of input bricks\n"
    "                    [default = none of them]\n"
    "                  * To get all components, set 'sss' to a very large\n"
    "                    number (more than the time series length), like 99999\n"
    "                    You can also use the key word ALL, as in -pcsave ALL\n"
    "                    to save all the components.\n"
    "  -reduce r pp  = Compute a 'dimensionally reduced' dataset with the top\n"
    "                    'r' eigenvalues and write to disk in dataset 'pp'\n"
    "                    [default = don't compute this at all]\n"
    "                  * If '-vmean' is given, then each voxel's mean will\n"
    "                    be added back into the reduced time series.  If you\n"
    "                    don't want this behaviour, you could remove the mean\n"
    "                    with 3dDetrend before running 3dpc.\n"
    "                  * On the other hand, the effects of '-vnorm' and '-dmean'\n"
    "                    and '-normalize' are not reversed in this output\n"
    "                    (at least at present -- send some cookies and we'll talk).\n"
    "  -prefix pname = Name for output dataset (will be a bucket type);\n"
    "                  * Also, the eigen-timeseries will be in 'pname'_vec.1D\n"
    "                    (all of them) and in 'pnameNN.1D' for eigenvalue\n"
    "                    #NN individually (NN=00 .. 'sss'-1, corresponding\n"
    "                    to the brick index in the output dataset)\n"
    "                  * The eigenvalues will be printed to file 'pname'_eig.1D\n"
    "                    All eigenvalues are printed, regardless of '-pcsave'.\n"
    "                    [default value of pname = 'pc']\n"
    "  -1ddum ddd    = Add 'ddd' dummy lines to the top of each *.1D file.\n"
    "                    These lines will have the value 999999, and can\n"
    "                    be used to align the files appropriately.\n"
    "                    [default value of ddd = 0]\n"
    "  -verbose      = Print progress reports during the computations\n"
    "  -quiet        = Don't print progress reports [the default]\n"
    "  -eigonly      = Only compute eigenvalues, then\n"
    "                    write them to 'pname'_eig.1D, and stop.\n"
    "  -float        = Save eigen-bricks as floats\n"
    "                    [default = shorts, scaled so that |max|=10000]\n"
    "  -mask mset    = Use the 0 sub-brick of dataset 'mset' as a mask\n"
    "                    to indicate which voxels to analyze (a sub-brick\n"
    "                    selector is allowed) [default = use all voxels]\n"
    "\n"
    "Example using 1D data a input, with each column being the equivalent\n"
    "of a sub-brick:\n"
    " 3dpc -prefix mmm -dmean -nscale -pcsave ALL datafile.1D\n"
    "\n"
   ) ;

#if 0
   printf("\n" MASTER_SHORTHELP_STRING ) ;
#endif

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nx,ny,nz , nxyz , ii,jj,ll , nn,mm,mmmm , npos,nneg ;
   float fmax , ftem ;
   THD_3dim_dataset *dset , *new_dset ;
   double *dsdev = NULL, div = 1.0 ;
   float  *fout , *perc ;
   short  *bout  ;
   register float  sum ;
   register double dsum ;
   register int    kk ;
   int ifirst,ilast,idel ;
   int ierror;          /* number of errors in editing data */
   MRI_IMAGE *vecim ;
   char vname[THD_MAX_NAME] ;
   FILE *fpp ;

   double *aa , *eval , *vout=NULL ;
   double atrace ;
   int adim ;
   float *vxmean=NULL ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) PC_syntax() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dpc main"); machdep(); PRINT_VERSION("3dpc") ;

   { int new_argc ; char **new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dpc",argc,argv) ;

   PC_read_opts( argc , argv ) ;

   /*-- get dimensions --*/

   dset = PC_dset[0]    ;
   nx   = DSET_NX(dset) ;
   ny   = DSET_NY(dset) ;
   nz   = DSET_NZ(dset) ;
   nxyz = nx * ny * nz  ;

   nn = nxyz ;           /* vector length = number of voxels */
   mm = PC_brnum ;       /* number of vectors = number of time points */

   if( PC_reduce >= mm ){  /* 22 Feb 2008 */
     WARNING_message("-reduce '%s' value is too large: ignoring!",PC_reduce) ;
     PC_reduce = 0 ;
   }

   /*-- space for eigenvalue computations --*/

   adim   = mm ;
   aa     = (double *) malloc( sizeof(double) * adim * adim ) ; /* matrix */
   eval   = (double *) malloc( sizeof(double) * adim ) ;        /* evals  */

   if( aa == NULL || eval == NULL )
     ERROR_exit("can't malloc space for covariance matrix") ;

   /*-- remove means, if ordered --*/

   if( PC_vmean ){
     vxmean = (float *)calloc( sizeof(float) , nn ) ;  /* voxel means */

     if( !PC_be_quiet ) INFO_message("remove timeseries means") ;

     for( jj=0 ; jj < mm ; jj++ )
       for( kk=0 ; kk < nn ; kk++ ) vxmean[kk] += XX(kk,jj) ;

     sum = 1.0f / mm ;
     for( kk=0 ; kk < nn ; kk++ ) vxmean[kk] *= sum ;

     for( jj=0 ; jj < mm ; jj++ )
       for( kk=0 ; kk < nn ; kk++ ) XX(kk,jj) -= vxmean[kk] ;

   } else if( PC_dmean ){
     if( !PC_be_quiet ) INFO_message("remove brick means") ;

     if( PC_mask == NULL ){
       for( jj=0 ; jj < mm ; jj++ ){
         sum = 0.0f ;
         for( kk=0 ; kk < nn ; kk++ ) sum += XX(kk,jj) ;
         sum /= nn ;
         for( kk=0 ; kk < nn ; kk++ ) XX(kk,jj) -= sum ;
       }
     } else {                           /* 15 Sep 1999 */
       for( jj=0 ; jj < mm ; jj++ ){
         sum = 0.0f ;
         for( kk=0 ; kk < nn ; kk++ ) if( PC_mask[kk] ) sum += XX(kk,jj) ;
         sum /= PC_mask_hits ;
         for( kk=0 ; kk < nn ; kk++ ) if( PC_mask[kk] ) XX(kk,jj) -= sum ;
       }
     }
   }

   /*-- 07 July 1999: vnorm --*/

   if( PC_vnorm ){
     float *vxnorm = (float *) malloc( sizeof(float) * nn ) ;  /* voxel norms */

     if( !PC_be_quiet ) INFO_message("normalize timeseries") ;

     for( kk=0 ; kk < nn ; kk++ ) vxnorm[kk] = 0.0 ;

     for( jj=0 ; jj < mm ; jj++ )
       for( kk=0 ; kk < nn ; kk++ ) vxnorm[kk] += XX(kk,jj) * XX(kk,jj) ;

     for( kk=0 ; kk < nn ; kk++ )
       if( vxnorm[kk] > 0.0 ) vxnorm[kk] = 1.0 / sqrt(vxnorm[kk]) ;

     for( jj=0 ; jj < mm ; jj++ )
       for( kk=0 ; kk < nn ; kk++ ) XX(kk,jj) *= vxnorm[kk] ;

     free(vxnorm) ;
   }

   /*-- load covariance matrix
        (very short code that takes a long time to run!) --*/

   if( !PC_be_quiet ) 
      INFO_message("compute covariance matrix, %d vals", 
                   PC_mask ? PC_mask_hits:nn) ;
   
   div = 1.0;
   if (PC_samp_scale) {
      if (PC_mask) div = (double)PC_mask_hits;
      else div = (double)nn;
   }
   
   idel = 1 ;                           /* ii goes forward */
   for( jj=0 ; jj < mm ; jj++ ){

     ifirst = (idel==1) ?    0 : jj ;  /* back and forth in ii to   */
     ilast  = (idel==1) ? jj+1 : -1 ;  /* maximize use of cache/RAM */

     for( ii=ifirst ; ii != ilast ; ii += idel ){
       dsum = 0.0 ;
       if( PC_mask == NULL ){
          for( kk=0 ; kk < nn ; kk++ ) dsum += XX(kk,ii) * XX(kk,jj) ;
       } else {
          for( kk=0 ; kk < nn ; kk++ )
                     if( PC_mask[kk] ) dsum += XX(kk,ii) * XX(kk,jj) ;
       }
       AA(ii,jj) = AA(jj,ii) = dsum/div ;
     }

      idel = -idel ;                    /* reverse direction of ii */
   }

   /*-- check diagonal for OK-ness --**/

   atrace = 0.0 ;
   for( ii=jj=0 ; jj < mm ; jj++ ){
     if( AA(jj,jj) <= 0.0 ){  /* should never happen */
       ERROR_message("covariance diagonal (%d,%d) = %g",jj+1,jj+1,AA(jj,jj));
       ii++ ;
     }
     atrace += AA(jj,jj) ;
   }
   if( ii > 0 ) ERROR_exit("*** program exiting right here and now!\n") ;

   if( !PC_be_quiet ) {
      INFO_message("covariance trace = %g\n",atrace) ;
      if (mm < 5) {
         for (ii=0; ii<mm; ++ii) {
            for (jj=0; jj<mm; ++jj) {
               fprintf(stderr,"%f\t", AA(ii,jj));
            }
            fprintf(stderr,"\n");
         }
      }
   }
   
   /*-- normalize, if desired --*/

   if( PC_normalize ){
     if( !PC_be_quiet ) INFO_message("normalizing covariance") ;

     dsdev = (double *) malloc( sizeof(double) * mm ) ; /* brick stdev */

     for( jj=0 ; jj < mm ; jj++ ) dsdev[jj] = sqrt( AA(jj,jj) ) ;

     for( jj=0 ; jj < mm ; jj++ )
       for( ii=0 ; ii < mm ; ii++ ) AA(ii,jj) /= (dsdev[ii]*dsdev[jj]) ;

     atrace = mm ;
   }

   /*---------*/

   if( !PC_be_quiet ) INFO_message("compute eigensolution") ;

   if( PC_eigonly ){
     symeigval_double( mm , aa , eval ) ; /* 22 Feb 2008 */
   } else {
     symeig_double( mm , aa , eval ) ;  /* eigenvectors go over aa; */
     vout = aa ;                        /* eigenvalues go into eval */
   }

   /*---------*/

   sum = 0.0 ;

   perc = (float *) malloc( sizeof(float) * PC_lprin_calc ) ;

   sprintf(vname,"%s_eig.1D",PC_prefix) ; fpp = fopen(vname,"w") ;
   if( !PC_be_quiet )
     printf("#Num.  --Eigenvalue--  -Var.Fraction-  -Cumul.Fract.-\n") ;
   if( fpp != NULL )
     fprintf(fpp,"#Num.  --Eigenvalue--  -Var.Fraction-  -Cumul.Fract.-\n") ;
   for( jj=0 ; jj < PC_lprin_calc ; jj++ ){
      ll = PC_lprin_calc - 1-jj ;      /* reversed order of eigensolution! */
      perc[jj] = eval[ll]/atrace ;
      sum     += perc[jj] ;
      if( !PC_be_quiet )
        printf("%4d  %14.7g  %14.7g  %14.7g\n",
               jj+1 , eval[ll] , perc[jj] , sum ) ;
      if( fpp != NULL )
        fprintf(fpp,"%4d  %14.7g  %14.7g  %14.7g\n",
                jj+1 , eval[ll] , perc[jj] , sum ) ;
   }
   if( fpp != NULL ) fclose(fpp) ;
   if( PC_eigonly ) exit(0) ;

   /*--- form and save eigen-brick dataset ---*/

   if( PC_lprin_save > 0 ){
     dset = PC_dset[0] ;
     new_dset = EDIT_empty_copy( dset ) ;

     tross_Copy_History( dset , new_dset ) ;
     tross_Make_History( "3dpc" , argc,argv , new_dset ) ;

     EDIT_dset_items( new_dset,
                         ADN_prefix    , PC_prefix ,
                         ADN_nvals     , PC_lprin_save ,
                         ADN_ntt       , 0 ,    /* no time axis */
                         ADN_brick_fac , NULL , /* no scale factors */
                         ADN_func_type , ISANAT(dset) ? ANAT_BUCK_TYPE
                                                      : FUNC_BUCK_TYPE ,
                      ADN_none ) ;

     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) )
       WARNING_message(
               "Eigen-brick dataset %s already exists--will be destroyed!",
               DSET_HEADNAME(new_dset) ) ;
     else if( !PC_be_quiet )
       INFO_message("Eigen-brick dataset %s" , DSET_BRIKNAME(new_dset) ) ;

     fout = (float *) malloc( sizeof(float) * nn ) ; /* output buffer */

     for( jj=0 ; jj < PC_lprin_save ; jj++ ){
       ll = PC_lprin_calc - 1-jj ;

       /** output = weighted sum of input datasets,
                    with weights from the ll'th eigenvector **/

       for( kk=0 ; kk < nn ; kk++ ) fout[kk] = 0.0 ;

       for( ii=0 ; ii < mm ; ii++ ){
         sum = VV(ii,ll) ; if( PC_normalize ) sum /= dsdev[ii] ;

         for( kk=0 ; kk < nn ; kk++ ) fout[kk] += XX(kk,ii) * sum ;
       }

       fmax = 0.0 ; npos = nneg = 0 ;
       for( kk=0 ; kk < nn ; kk++ ){
         ftem = fabs(fout[kk]) ; if( fmax < ftem ) fmax = ftem ;
              if( fout[kk] > 0 ) npos++ ;
         else if( fout[kk] < 0 ) nneg++ ;
       }

       if( PC_do_float ){
         if( nneg > npos )
           for( kk=0 ; kk < nn ; kk++ ) fout[kk] = -fout[kk] ;

         EDIT_substitute_brick( new_dset , jj , MRI_float , fout ) ;

         fout = (float *) malloc( sizeof(float) * nn ) ;  /* new buffer */
       } else {

         bout = (short *) malloc( sizeof(short) * nn ) ; /* output buffer */
         if( fmax != 0.0 ){
           fmax = 10000.49/fmax ; if( nneg > npos ) fmax = -fmax ;
           for( kk=0 ; kk < nn ; kk++ ) bout[kk] = fmax * fout[kk] ;
         } else {
           for( kk=0 ; kk < nn ; kk++ ) bout[kk] = 0.0 ;
         }
         EDIT_substitute_brick( new_dset , jj , MRI_short , bout ) ;
       }

       sprintf(vname,"var=%6.3f%%" , 100.0*perc[jj]+0.499 ) ;
       EDIT_BRICK_LABEL( new_dset , jj , vname ) ;
     }
     free(fout) ;

     DSET_write(new_dset) ;
     if( !PC_be_quiet ) WROTE_DSET(new_dset) ;
     DSET_delete(new_dset) ;
   }

   /*-- write eigenvectors also --*/

   if( PC_lprin_save > 0 ){
     mmmm  = mm + PC_1ddum ;

     /* all eigenvectors to 1 file */

     vecim = mri_new( PC_lprin_save , mmmm , MRI_float ) ;
     fout  = MRI_FLOAT_PTR(vecim) ;
     for( jj=0 ; jj < PC_lprin_save ; jj++ ){
       ll = PC_lprin_calc - 1-jj ;
       for( ii=0 ; ii < PC_1ddum ; ii++ )
         fout[jj + ii*PC_lprin_save] = 999999.0 ;
       for( ii=0 ; ii < mm ; ii++ )
         fout[jj + (ii+PC_1ddum)*PC_lprin_save] = VV(ii,ll) ;
     }
     /* Modified name to include _vec, else it conflicts
     with the eigenbricks above with .1D files as input */
     sprintf(vname,"%s_vec.1D",PC_prefix) ;
     mri_write_ascii( vname, vecim ) ;
     mri_free(vecim) ;

     /* individual eigenvectors to individual files */

     for( jj=0 ; jj < PC_lprin_save ; jj++ ){
       ll = PC_lprin_calc - 1-jj ;
       vecim = mri_new( 1 , mmmm , MRI_float ) ;
       fout  = MRI_FLOAT_PTR(vecim) ;
       for( ii=0 ; ii < PC_1ddum ; ii++ ) fout[ii]          = 999999.0 ;
       for( ii=0 ; ii < mm       ; ii++ ) fout[ii+PC_1ddum] = VV(ii,ll) ;
       sprintf(vname,"%s%02d.1D",PC_prefix,jj) ;
       mri_write_ascii( vname, vecim ) ;
       mri_free(vecim) ;
     }
   }

   /* 22 Feb 2008: reconstruct truncated expansion of data */

   if( PC_reduce > 0 ){
     float *pp = (float *)calloc( sizeof(float) , adim*adim ) ;
     for( kk=0 ; kk < PC_reduce ; kk++ ){
       ll = (adim-1)-kk ;
       for( jj=0 ; jj < adim ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ) PP(ii,jj) += VV(ii,ll)*VV(jj,ll) ;
       }
     }
     for( jj=0 ; jj < adim ; jj++ ){
       for( ii=jj+1 ; ii < adim ; ii++ ) PP(ii,jj) = PP(jj,ii) ;
     }

     dset = PC_dset[0] ;
     new_dset = EDIT_empty_copy( dset ) ;

     tross_Copy_History( dset , new_dset ) ;
     tross_Make_History( "3dpc" , argc,argv , new_dset ) ;
     EDIT_dset_items( new_dset,
                         ADN_prefix    , PC_reprefix ,
                         ADN_brick_fac , NULL        ,
                      ADN_none ) ;

     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) )
       WARNING_message(
               "Reduced dataset %s already exists--will be destroyed!",
               DSET_HEADNAME(new_dset) ) ;
     else if( !PC_be_quiet )
       INFO_message("Reduced dataset %s" , DSET_BRIKNAME(new_dset) ) ;

     for( jj=0 ; jj < adim ; jj++ ){
       fout = (float *)calloc( sizeof(float) , nn ) ; /* output brick */
       for( ii=0 ; ii < nn ; ii++ ){
         if( PC_mask != NULL && PC_mask[ii] == 0 ) continue ;
         sum = (vxmean == NULL) ? 0.0f : vxmean[ii] ;
         for( kk=0 ; kk < adim ; kk++ ) sum += XX(ii,kk) * PP(kk,jj) ;
         fout[ii] = sum ;
       }
       EDIT_substitute_brick( new_dset , jj , MRI_float , fout ) ;
     }
     free(pp) ;
     DSET_write(new_dset) ;
     if( !PC_be_quiet ) WROTE_DSET(new_dset) ;
     DSET_delete(new_dset) ;
   }

#if 0
   free(PC_dset) ;
   for( ii=0 ; ii < mm ; ii++ ) free(PC_brickdata[ii]) ;
   free(PC_brickdata) ;
   free(aa) ; free(eval) ; free(perc) ; if( dsdev!=NULL ) free(dsdev) ;
#endif

   exit(0) ;
}
