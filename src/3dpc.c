#include "mrilib.h"
#include <string.h>

/******
        This program has not been modified to work with
        the AFNI-96 new system of dataset storage!
        Do not try to compile or run it!
*******/

#undef PCDEBUG

/*-------------------------- global data --------------------------*/

/** inputs **/

static int   PC_dmean      = 0 ;
static int   PC_vmean      = 0 ;
static int   PC_normalize  = 0 ;
static int   PC_lprin_calc = 1 ;  /* # of principal components to compute */
static int   PC_lprin_save = 1 ;  /* # of principal components to save    */
static int   PC_use_mask   = 0 ;
static int   PC_be_quiet   = 0 ;

static THD_string_array       * PC_dsname = NULL ;  /* set of dataset names */
static THD_3dim_dataset_array * PC_dset   = NULL ;  /* set of datasets */
static int                      PC_dsnum  = 0   ;   /* number of them */
static short                 ** PC_dsfim  = NULL ;  /* pointers to bricks */

static char PC_session[THD_MAX_NAME]  = "./" ;
static char PC_prefix[THD_MAX_PREFIX] = "pc" ;
static char PC_label[THD_MAX_LABEL]   = "\0" ;

#define DTYPE double

/*--------------------------- useful macros ------------------------*/

/** i'th element of j'th input dataset **/

#define XX(i,j) ((DTYPE)PC_dsfim[(j)][(i)])

/** i'th element of j'th input dataset, minus mean of j'th dataset **/

#define XXDM(i,j) (XX(i,j)-dsmean[j])

/** i'th element of j'th input dataset, minus mean of i'th element **/

#define XXVM(i,j) (XX(i,j)-vxmean[i])

/** i,j element of covariance matrix **/

#define AA(i,j) (aa[(i)+(j)*adim])

/** i'th element of j'th eigenvector **/

#define VV(i,j) (zout[(i)+(j)*adim])

/*--------------------------- prototypes ---------------------------*/

void PC_read_opts( int , char ** ) ;
void PC_syntax(char *) ;

/** Eigenvalue routine from LAPACK **/

extern int dsyevx_( char * , char * , char * , int * ,
                    double * , int * , double * , double * , int * ,
                    int * , double * , int * , double * ,
                    double * , int * , double * , int * ,
                    int * , int * , int * ) ;

/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

#ifdef PCDEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

void PC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival , kk , nx,ny,nz , mm , got_prefix = 0 ;
   THD_3dim_dataset * dset ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",6) == 0 ){
         PC_be_quiet = 1 ;
         nopt++ ; continue ;
      }

      /**** -session dirname ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) PC_syntax("need argument after -session!") ;
         MCW_strncpy( PC_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) PC_syntax("need argument after -prefix!") ;
         MCW_strncpy( PC_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         got_prefix = 1 ;
         continue ;
      }

      /**** -label string ****/

      if( strncmp(argv[nopt],"-label",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) PC_syntax("need argument after -label!") ;
         MCW_strncpy( PC_label , argv[nopt++] , THD_MAX_LABEL ) ;
         continue ;
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

      /**** -normalize ****/

      if( strncmp(argv[nopt],"-normalize",6) == 0 ){
         PC_normalize = 1 ;
         nopt++ ; continue ;
      }

      /**** -pccalc # ****/

      if( strncmp(argv[nopt],"-pccalc",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) PC_syntax("need argument after -pccalc!") ;
         PC_lprin_calc = strtol( argv[nopt] , NULL , 10 ) ;
         if( PC_lprin_calc <= 0 ) PC_syntax("value after -pccalc is illegal!") ;
         nopt++ ; continue ;
      }

      /**** -pcsave # ****/

      if( strncmp(argv[nopt],"-pcsave",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) PC_syntax("need argument after -pcsave!") ;
         PC_lprin_save = strtol( argv[nopt] , NULL , 10 ) ;
         if( PC_lprin_save <  0 ) PC_syntax("value after -pcsave is illegal!") ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\n",argv[nopt]) ;
      exit(-1) ;

   }  /* end of loop over options */

   if( got_prefix && strlen(PC_label) == 0 ){
      MCW_strncpy(PC_label,PC_prefix,THD_MAX_LABEL) ;
   }

   /*--- rest of inputs are dataset names ---*/

   INIT_SARR( PC_dsname ) ;
   INIT_3DARR( PC_dset ) ;
   for( kk=nopt ; kk < argc ; kk++ ){
      ADDTO_SARR( PC_dsname , argv[kk] ) ;
      dset = THD_open_one_dataset( argv[kk] ) ;
      if( ! ISVALID_3DIM_DATASET(dset) ){
         fprintf(stderr,"\n*** can't open dataset file %s\n",argv[kk]) ;
         exit(-1) ;
      }
      ADDTO_3DARR( PC_dset , dset ) ;
   }
   PC_dsnum = PC_dsname->num ;
   if( PC_dsnum < 2 ) PC_syntax("need at least 2 input datasets!") ;

   /*--- check arguments for consistency ---*/

   if( PC_lprin_calc > PC_dsname->num )
      PC_syntax("can't calculate more components than input datasets!") ;

   if( PC_lprin_save > PC_lprin_calc )
      PC_syntax("can't save more components than calculated!") ;

   if( PC_vmean && PC_dmean )
      PC_syntax("can't have both -dmean and -vmean!") ;

   /*--- load bricks for all input datasets ---*/

   nx = PC_dset->ar[0]->daxes->nxx ;
   ny = PC_dset->ar[0]->daxes->nyy ;
   nz = PC_dset->ar[0]->daxes->nzz ;

   PC_dsfim = (short **) malloc( sizeof(short *) * PC_dsnum ) ;
   if( PC_dsfim == NULL ){
      fprintf(stderr, "\n*** cannot allocate storage for pointers to bricks!\n") ;
      exit(-1) ;
   }

   for( kk=0 ; kk < PC_dsnum ; kk++ ){

      if( PC_dset->ar[kk]->daxes->nxx != nx ||
          PC_dset->ar[kk]->daxes->nyy != ny ||
          PC_dset->ar[kk]->daxes->nzz != nz   ){

         fprintf(stderr,
                 "*** dataset in file %s non-conformant with first dataset!\n"
                 "    nx1=%d ny1=%d nz1=%d nx=%d ny=%d nz=%d\n",
                 PC_dsname->ar[kk] , nx,ny,nz ,
                 PC_dset->ar[kk]->daxes->nxx ,
                 PC_dset->ar[kk]->daxes->nyy , PC_dset->ar[kk]->daxes->nzz ) ;
         exit(-1) ;
      }

      /*** check for duplicate brick file names (for bootstrap applications) ***/

      for( mm=0 ; mm < kk ; mm++ ){
         if( strcmp( PC_dset->ar[mm]->dblk->diskptr->brick_name ,
                     PC_dset->ar[kk]->dblk->diskptr->brick_name  ) == 0 ) break ;
      }
      if( mm < kk ){
         PC_dsfim[kk] = PC_dsfim[mm] ;
      } else {
         THD_load_datablock( PC_dset->ar[kk]->dblk , NULL ) ;

         if( PC_dset->ar[kk]->dblk->brick == NULL ){
            fprintf(stderr,
                    "\n*** can't load datablock from file %s\n",
                    PC_dsname->ar[kk] ) ;
            exit(-1) ;
         }
         PC_dsfim[kk] = PC_dset->ar[kk]->dblk->brick ;
      }
   }

   return ;
}

/*------------------------------------------------------------------*/

void PC_syntax(char * msg)
{
   if( msg != NULL ){
      fprintf(stderr,"*** %s\n",msg) ;
      exit(-1) ;
   }

   printf(
    "Copyright 1994,1995 Medical College of Wisconsin\n\n"
    "Principal Component Analysis of 3D Datasets\n"
    "Usage: 3dpc [options] datasets ...\n"
    "\n"

    "PRINCIPAL COMPONENT OPTIONS\n"
    "  -dmean       = remove the mean from each input dataset\n"
    "  -vmean       = remove the mean from each input voxel\n"
    "                 [-dmean and -vmean are mutually exclusive]\n"
    "  -normalize   = L2 normalize each input dataset\n"
    "  -pccalc ccc  = 'ccc' is the number of components to compute;\n"
    "                 it can't be more than the number of input datasets\n"
    "                 (default=1)\n"
    "  -pcsave sss  = 'sss' is the number of components to save;\n"
    "                 it can't be more than 'ccc' (default=1)\n"
    "\n"

    "OUTPUT DATASET NAMING OPTIONS\n"
    "  -session  dirname  = write output into given directory (default=./)\n"
    "  -prefix   pname    = use 'pname'jj for the output directory prefix\n"
    "                       for component # jj, jj=01..sss (default=pc)\n"
    "  -label    string   = use 'string'jj for the label in the output\n"
    "                       dataset (the label is used for switching\n"
    "                       between datasets in AFNI)\n"
    "\n"
    "The above options control the output file names.\n"
    "Note that the input datasets are specified by their .HEAD files,\n"
    "but that their .BRIK files must exist also!\n"
   ) ;

   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int nx,ny,nz , nxyz , ii,jj,ll , nn,mm , output_type,output_nvals ;
   int   npos , nneg ;
   DTYPE fmax , ftem ;
   THD_3dim_dataset * dset , * new_dset ;
   char prefix[THD_MAX_PREFIX] ;
   DTYPE  * dsmean , * dsdev , * vxmean ;
   short  * bout   ;
   DTYPE  * fout   ;
   register DTYPE sum ;
   register int   kk ;
   int ifirst , ilast , idel ;

   /** data for eigenvalue routine **/

   double * aa , * wout , * zout , * work ;
   double abstol , atrace ;
   int adim , il , iu , mout , lwork , info ;
   int * iwork , * ifail ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ) PC_syntax(NULL) ;
   PC_read_opts( argc , argv ) ;

   /*-- get dimensions --*/

   dset = PC_dset->ar[0] ;
   nx   = dset->daxes->nxx ;
   ny   = dset->daxes->nyy ;
   nz   = dset->daxes->nzz ; nxyz = nx * ny * nz ;

   nn = nxyz ;      /* vector length */
   mm = PC_dsnum ;  /* number of vectors */

   /*-- space for eigenvalue computations --*/

   adim   = mm ;
   aa     = (double *) malloc( sizeof(double) * adim * adim ) ;
   il     = adim + 1 - PC_lprin_calc ;                          /* lowest index */
   iu     = adim ;                                              /* upper index */
   abstol = 0.0 ;
   wout   = (double *) malloc( sizeof(double) * adim ) ;
   zout   = (double *) malloc( sizeof(double) * adim * PC_lprin_calc ) ;
   lwork  = 32 * adim ;
   work   = (double *) malloc( sizeof(double) * lwork ) ;
   iwork  = (int *)    malloc( sizeof(int) * 6 * adim ) ;
   ifail  = (int *)    malloc( sizeof(int) * adim ) ;

   dsmean = (DTYPE *) malloc( sizeof(DTYPE) * mm ) ;   /* dataset means */
   dsdev  = (DTYPE *) malloc( sizeof(DTYPE) * mm ) ;   /* dataset stdev */
   bout   = (short *) malloc( sizeof(short) * nxyz ) ; /* output buffer */
   fout   = (DTYPE *) malloc( sizeof(DTYPE) * nxyz ) ; /* output buffer */

   if( PC_vmean ){
      vxmean = (DTYPE *) malloc( sizeof(DTYPE) * nxyz ) ;  /* voxel means */
   } else {
      vxmean = NULL ;
   }

   if( wout   == NULL || zout  == NULL || aa    == NULL ||
       work   == NULL || iwork == NULL || ifail == NULL ||
       dsmean == NULL || bout  == NULL || fout  == NULL ||
       dsdev  == NULL || (PC_vmean && vxmean==NULL)       ){

      fprintf(stderr,"\n*** cannot malloc workspace!\n") ;
      exit(-1) ;
   }

   /*-- load means, if needed --*/

   if( PC_vmean ){
      printf("--- computing mean dataset\n") ; fflush(stdout) ;

      for( kk=0 ; kk < nn ; kk++ ) vxmean[kk] = 0.0 ;

      for( jj=0 ; jj < mm ; jj++ )
         for( kk=0 ; kk < nn ; kk++ ) vxmean[kk] += XX(kk,jj) ;

      sum = 1.0 / mm ;
      for( kk=0 ; kk < nn ; kk++ ) vxmean[kk] *= sum ;

   } else if( PC_dmean ){
      printf("--- computing mean of each dataset\n") ; fflush(stdout) ;

      for( jj=0 ; jj < mm ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < nn ; kk++ ) sum += XX(kk,jj) ;
         dsmean[jj] = sum / nn ;
      }
   }

   /*-- load covariance matrix
        (very short code that takes a long time to run!) --*/

   printf("--- computing covariance matrix\n") ; fflush(stdout) ;


   idel = 1 ;
   for( jj=0 ; jj < mm ; jj++ ){
      printf(" -- row%3d",jj+1) ; fflush(stdout) ;

      ifirst = (idel==1) ?    0 : jj ;
      ilast  = (idel==1) ? jj+1 : -1 ;

      for( ii=ifirst ; ii != ilast ; ii += idel ){
         printf(".") ; fflush(stdout) ;
         sum = 0.0 ;
         if( PC_vmean ){
            for( kk=0 ; kk < nn ; kk++ ) sum += XXVM(kk,ii) * XXVM(kk,jj) ;
         } else if( PC_dmean ){
            for( kk=0 ; kk < nn ; kk++ ) sum += XXDM(kk,ii) * XXDM(kk,jj) ;
         } else {
            for( kk=0 ; kk < nn ; kk++ ) sum += XX(kk,ii) * XX(kk,jj) ;
         }
         AA(ii,jj) = AA(jj,ii) = sum ;
      }
      printf("\n") ; fflush(stdout) ;

      idel = -idel ;
   }

   /*-- check diagonal for OK-ness --**/

   atrace = 0.0 ;
   ii     = 0 ;
   for( jj=0 ; jj < mm ; jj++ ){
      if( AA(jj,jj) <= 0.0 ){
         fprintf(stderr,"*** covariance diagonal %d = %g\n",jj+1,AA(jj,jj)) ;
         ii++ ;
      }
      atrace += AA(jj,jj) ;
   }
   if( ii > 0 ) exit(-1) ;
   printf("--- covariance trace = %g\n",atrace) ; fflush(stdout) ;

   /*-- normalize, if desired --*/

   if( PC_normalize ){
      for( jj=0 ; jj < mm ; jj++ ) dsdev[jj] = sqrt( AA(jj,jj) ) ;

      for( jj=0 ; jj < mm ; jj++ )
         for( ii=0 ; ii < mm ; ii++ ) AA(ii,jj) /= (dsdev[ii]*dsdev[jj]) ;

      atrace = mm ;
   }

   printf("--- computing eigensolution of covariance matrix\n") ; fflush(stdout) ;

   (void) dsyevx_( "V"     , /* eigenvalues and vectors */
                   "I"     , /* a subrange of eigenvalues */
                   "U"     , /* use upper triangle of A */
                   &adim   , /* dimension of A */
                   aa      , /* the matrix A */
                   &adim   , /* leading dimension of A */
                   NULL    , /* not used */
                   NULL    , /* not used */
                   &il     , /* lowest eigen-index */
                   &iu     , /* highest eigen-index */
                   &abstol , /* tolerance */
                   &mout   , /* output # of eigenvalues */
                   wout    , /* output eigenvalues */
                   zout    , /* output eigenvectors */
                   &adim   , /* leading dimension of zout */
                   work    , /* double work array */
                   &lwork  , /* size of work array */
                   iwork   , /* another work array */
                   ifail   , /* output failure list */
                   &info     /* results code */
                 ) ;

   free(aa) ; free(work) ; free(iwork) ; free(ifail) ;

   if( info != 0 ){
      fprintf(stderr,"** DSYEVX returns error code info=%d\n",info);
      if( info < 0 ) exit(-1) ;
   }

   printf("Num.  --Eigenvalue--  -Var.Fraction-\n") ;
   for( jj=0 ; jj < PC_lprin_calc ; jj++ ){
      ll = PC_lprin_calc - 1-jj ;             /* reversed order of eigensolution! */
      printf("%4d  %14.7g  %14.7g\n",
             jj+1 , wout[ll] , wout[ll]/atrace ) ;
   }
   fflush(stdout) ;

   /*--- form and save output datasets ---*/

   dset         = PC_dset->ar[0] ;
   output_type  = (ISFUNC(dset)) ? EDIT_DSET_TO_FIM : EDIT_DSET_NONE ;
   output_nvals = 1 ;

   for( jj=0 ; jj < PC_lprin_save ; jj++ ){
      ll = PC_lprin_calc - 1-jj ;

      printf("--- output%3d\n",jj+1) ; fflush(stdout) ;

      /** output = weighted sum of input datasets,
                   with weights from the ll'th eigenvector **/

      for( kk=0 ; kk < nn ; kk++ ) fout[kk] = 0.0 ;

      for( ii=0 ; ii < mm ; ii++ ){
         sum = VV(ii,ll) ; if( PC_normalize ) sum /= dsdev[ii] ;
         printf(" -- input dataset %3d weight = %g\n",ii+1,sum) ; fflush(stdout) ;

         if( PC_vmean ){
            for( kk=0 ; kk < nn ; kk++ ) fout[kk] += XXVM(kk,ii) * sum ;
         } else if( PC_dmean ){
            for( kk=0 ; kk < nn ; kk++ ) fout[kk] += XXDM(kk,ii) * sum ;
         } else {
            for( kk=0 ; kk < nn ; kk++ ) fout[kk] += XX(kk,ii) * sum ;
         }
      }
      fmax = 0.0 ; npos = nneg = 0 ;
      for( kk=0 ; kk < nn ; kk++ ){
         ftem = fabs(fout[kk]) ; if( fmax < ftem ) fmax = ftem ;
         if( fout[kk] > 0 ) npos++ ;
         if( fout[kk] < 0 ) nneg++ ;
      }
      if( fmax != 0.0 ){
         fmax = 10000.49/fmax ; if( nneg > npos ) fmax = -fmax ;
         for( kk=0 ; kk < nn ; kk++ ) bout[kk] = fmax * fout[kk] ;
      } else {
         fprintf(stderr,
                 "\n*** Output component %d is all zeros--skipping!\n",jj+1) ;
         continue ;
      }

      /*-- make an empty copy of first dataset --*/

      sprintf( prefix , "%s%02d" , PC_prefix , jj+1 ) ;
      new_dset = EDIT_make_empty( dset , PC_session , prefix , output_type ) ;

      if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
         fprintf(stderr,
                 "\n*** Output dataset file %s already exists--skipping!\n",
                 new_dset->dblk->diskptr->header_name ) ;
         continue ;
      }

      /*-- change the output dataset's names --*/

      sprintf( new_dset->self_name , "%s/(PC%02d)" , dset->self_name , jj+1 ) ;

      if( strlen(PC_label) > 0 ){
         MCW_strncpy( prefix , PC_label , THD_MAX_LABEL-3 ) ;
      } else {
         MCW_strncpy( prefix , dset->label1 , THD_MAX_LABEL-3 ) ;
      }
      sprintf( prefix + strlen(prefix) , "%02d" , jj+1 ) ;
      MCW_strncpy( new_dset->label1 , prefix , THD_MAX_LABEL ) ;

      printf(" -- Writing dataset: label=%s\n",new_dset->label1) ; fflush(stdout) ;
      new_dset->dblk->brick = bout ;
      THD_load_statistics( new_dset ) ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
      new_dset->dblk->brick = NULL ;
      THD_delete_3dim_dataset( new_dset , False ) ;
   }

   exit(0) ;
}
