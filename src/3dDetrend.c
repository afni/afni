#include "mrilib.h"
#include "parser.h"

/*---------------------------------------------------------------------------
  This program detrends voxel time series.
  It will also cure bad breath, fix parking tickets, and
  get Green Bay to the Super Bowl.
----------------------------------------------------------------------------*/

static THD_3dim_dataset * DT_dset    = NULL ;
static MRI_IMARR *        DT_imar    = NULL ;
static char **            DT_expr    = NULL ;
static PARSER_code **     DT_excode  = NULL ;
static float *            DT_exdel   = NULL ;
static int *              DT_exvar   = NULL ;
static int                DT_exnum   = 0    ;
static int                DT_verb    = 0    ;
static int                DT_replace = 0    ;

static float              DT_current_del = -1.0 ;

static char DT_output_prefix[THD_MAX_PREFIX] = "detrend" ;
static char DT_session[THD_MAX_NAME]         = "./"   ;

/*--------------------------- prototypes ---------------------------*/

void DT_read_opts( int , char ** ) ;
void DT_Syntax(void) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void DT_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , nvals , ii ;

   INIT_IMARR(DT_imar) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( DT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -session!\n") ; exit(1) ;
         }
         MCW_strncpy( DT_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         DT_verb++ ;
         nopt++ ; continue ;
      }

      /**** -replace ****/

      if( strncmp(argv[nopt],"-replace",5) == 0 ){
         DT_replace++ ;
         nopt++ ; continue ;
      }

      /**** -vector ****/

      if( strncmp(argv[nopt],"-vector",4) == 0 ){
         MRI_IMAGE * flim ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -vector!\n"); exit(1);
         }
         flim = mri_read_1D( argv[nopt++] ) ;
         if( flim == NULL ){
            fprintf(stderr,"*** can't read -vector %s\n",argv[nopt-1]); exit(1);
         }
         ADDTO_IMARR(DT_imar,flim) ;
         if( DT_verb )
            fprintf(stderr,"+++ Read in %d: rows=%d cols=%d\n",
                           argv[nopt-1],flim->ny,flim->nx ) ;
         continue ;
      }

      /**** -del ****/

      if( strncmp(argv[nopt],"-del",4) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -del!\n"); exit(1);
         }
         DT_current_del = strtod( argv[nopt++] , NULL ) ;
         if( DT_current_del <= 0.0 ){
            fprintf(stderr,"*** can't use -del %s\n",argv[nopt-1]); exit(1);
         }
         if( DT_verb )
            fprintf(stderr,"+++ Set expression stepsize = %g\n",DT_current_del) ;
         continue ;
      }

      /**** -expr ****/

      if( strncmp(argv[nopt],"-expr",4) == 0 ){
         int nexp , qvar , kvar ;
         char sym[4] ;

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -expr!\n"); exit(1);
         }

         nexp = DT_exnum + 1 ;
         if( DT_exnum == 0 ){   /* initialize storage */
            DT_expr   = (char **)        malloc( sizeof(char *) ) ;
            DT_excode = (PARSER_code **) malloc( sizeof(PARSER_code *) ) ;
            DT_exdel  = (float *)        malloc( sizeof(float) ) ;
            DT_exvar  = (int *)          malloc( sizeof(int) ) ;
         } else {
            DT_expr   = (char **)        realloc( DT_expr ,
                                                  sizeof(char *)*nexp ) ;
            DT_excode = (PARSER_code **) realloc( DT_excode ,
                                                  sizeof(PARSER_code *)*nexp ) ;
            DT_exdel  = (float *)        realloc( DT_exdel ,
                                                  sizeof(float)*nexp) ;
            DT_exvar  = (int *)          realloc( DT_exvar ,
                                                  sizeof(int)*nexp) ;
         }
         DT_expr[DT_exnum]   = argv[nopt] ;                         /* string */
         DT_exdel[DT_exnum]  = DT_current_del ;                     /* delta */
         DT_excode[DT_exnum] = PARSER_generate_code( argv[nopt] ) ; /* compile */
         if( DT_excode[DT_exnum] == NULL ){
            fprintf(stderr,"*** Illegal expression: %s\n",argv[nopt]); exit(1);
         }

         qvar = 0 ; kvar = -1 ;                       /* find symbol */
         for( ii=0 ; ii < 26 ; ii++ ){
            sym[0] = 'A' + ii ; sym[1] = '\0' ;
            if( PARSER_has_symbol(sym,DT_excode[DT_exnum]) ){
               qvar++ ; if( kvar < 0 ) kvar = ii ;
               if( DT_verb )
                  fprintf(stderr,"+++ Found expression symbol %s\n",sym) ;
            }
         }
         if( qvar > 1 ){
            fprintf(stderr,"*** Expression %s should have just one symbol!\n",
                           DT_expr[DT_exnum] ) ;
            exit(1) ;
         }
         DT_exvar[DT_exnum] = kvar ;

         DT_exnum = nexp ; nopt++ ; continue ;
      }

      /**** ERROR ****/

      fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]) ; exit(1) ;

   }  /* end of scan over options */

   /*-- check for errors --*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

   if( IMARR_COUNT(DT_imar) + DT_exnum == 0 ){
      fprintf(stderr,"*** No -vector or -expr options!?\n") ; exit(1) ;
   }

   /*--- read input dataset ---*/

   DT_dset = THD_open_dataset( argv[nopt] ) ;
   if( DT_dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]) ; exit(1) ;
   }

   DT_current_del = DSET_TR(DT_dset) ;
   if( DT_current_del <= 0.0 ){
      DT_current_del = 1.0 ;
      if( DT_verb )
         fprintf(stderr,"+++ Input has no TR value; setting TR=1.0\n") ;
   } else if( DT_verb ){
         fprintf(stderr,"+++ Input has TR=%g\n",DT_current_del) ;
   }

   /*-- check vectors for good size --*/

   nvals = DSET_NVALS(DT_dset) ;
   for( ii=0 ; ii < IMARR_COUNT(DT_imar) ; ii++ ){
      if( IMARR_SUBIMAGE(DT_imar,ii)->nx < nvals ){
         fprintf(stderr,"*** %d-th -vector is shorter than dataset!\n",ii+1) ;
         exit(1) ;
      }
   }

   /*--- create time series from expressions */

   if( DT_exnum > 0 ){
      double atoz[26] , del ;
      int kvar , jj ;
      MRI_IMAGE * flim ;
      float * flar ;

      for( jj=0 ; jj < DT_exnum ; jj++ ){
         if( DT_verb ) fprintf(stderr,"+++ Evaluating %d-th -expr\n",jj+1) ;
         kvar = DT_exvar[jj] ;
         del  = DT_exdel[jj] ;
         if( del <= 0.0 ) del = DT_current_del ;
         flim = mri_new( nvals , 1 , MRI_float ) ;
         flar = MRI_FLOAT_PTR(flim) ;
         for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = 0.0 ;
         for( ii=0 ; ii < nvals ; ii++ ){
            if( kvar >= 0 ) atoz[kvar] = ii * del ;
            flar[ii]   = PARSER_evaluate_one( DT_excode[jj] , atoz ) ;
         }
         ADDTO_IMARR( DT_imar , flim ) ;
      }
   }

   return ;
}

/*-------------------------------------------------------------------------*/

void DT_Syntax(void)
{
   printf(
    "Usage: 3dDetrend [options] dataset\n"
    "This program removes components from voxel time series using\n"
    "linear least squares.  Each voxel is treated independently.\n"
    "The input dataset may have a sub-brick selector string; otherwise,\n"
    "all sub-bricks will be used.\n\n"
   ) ;

   printf(
    "General Options:\n"
    " -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    "                   [default='detrend']\n"
    " -session dir  = Use 'dir' for the output dataset session directory.\n"
    "                   [default='./'=current working directory]\n"
    " -verb         = Print out some verbose output as the program runs.\n"
    " -replace      = Instead of subtracting the fit from each voxel,\n"
    "                   replace the voxel data with the time series fit.\n"
    "\n"
    "Component Options:\n"
    "These options determine the components that will be removed from\n"
    "each dataset voxel time series.  They may be repeated to specify\n"
    "multiple regression.  At least one component must be specified.\n"
    "\n"
    " -vector vvv   = Remove components proportional to the columns vectors\n"
    "                   of the ASCII *.1D file 'vvv'.  You may use a\n"
    "                   sub-vector selector string to specify which columns\n"
    "                   to use; otherwise, all columns will be used.\n"
    "                   For example:\n"
    "                    -vector 'xyzzy.1D[3,5]'\n"
    "                   will remove the 4th and 6th columns of file xyzzy.1D\n"
    "                   from the dataset (sub-vector indexes start at 0).\n"
    "\n"
    " -expr eee     = Remove components proportional to the function\n"
    "                   specified in the expression string 'eee'.\n"
    "                   Any single letter from a-z may be used as the\n"
    "                   independent variable in 'eee'.  For example:\n"
    "                    -expr 'cos(2*PI*t/40)' -expr 'sin(2*PI*t/40)'\n"
    "                   will remove sine and cosine waves of period 40\n"
    "                   from the dataset.  Another example:\n"
    "                    -expr '1' -expr 't' -expr 't*t'\n"
    "                   will remove a quadratic trend from the data.\n"
    "\n"
    " -del ddd      = Use the numerical value 'ddd' for the stepsize\n"
    "                   in subsequent -expr options.  If no -del option\n"
    "                   is ever given, then the TR given in the dataset\n"
    "                   header is used for 'ddd'; if that isn't available,\n"
    "                   then 'ddd'=1.0 is assumed.  The j-th time point\n"
    "                   will have independent variable = j * ddd, starting\n"
    "                   at j=0.  For example:\n"
    "                     -expr 'sin(x)' -del 2.0 -expr 'z**3'\n"
    "                   means that the stepsize in 'sin(x)' is delta-x=TR,\n"
    "                   but the stepsize in 'z**3' is delta-z = 2.\n"
   ) ;

   exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int iv,nvals , nvec , ii,jj,kk , nvox ;
   THD_3dim_dataset * new_dset=NULL ;
   double * choleski ;
   float ** refvec , * fv , * fc , * fit ;
   MRI_IMAGE * flim ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) DT_Syntax() ;

   DT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   new_dset = EDIT_empty_copy( DT_dset ) ; /* make a copy of its header */

   /* modify its header */

   tross_Copy_History( DT_dset , new_dset ) ;
   tross_Make_History( "3dDetrend" , argc,argv , new_dset ) ;

   EDIT_dset_items( new_dset ,
                      ADN_prefix        , DT_output_prefix ,
                      ADN_directory_name, DT_session ,
                    ADN_none ) ;

   /* can't re-write existing dataset */

   if( THD_is_file(DSET_HEADNAME(new_dset)) ){
      fprintf(stderr,"*** Fatal error: file %s already exists!\n",
              DSET_HEADNAME(new_dset) ) ;
      exit(1) ;
   }

   /* read input in, and attach its bricks to the output dataset */
   /* (not good in a plugin, but OK in a standalone program!)    */

   if( DT_verb ) fprintf(stderr,"+++ Loading input dataset .BRIK\n") ;

   DSET_mallocize( new_dset ) ;
   DSET_mallocize( DT_dset ) ;
   DSET_load( DT_dset ) ;
   if( !DSET_LOADED(DT_dset) ){
      fprintf(stderr,"*** Can't read input dataset .BRIK!\n") ;
      exit(1) ;
   }

   nvals = DSET_NVALS(new_dset) ;
   for( iv=0 ; iv < nvals ; iv++ )
      EDIT_substitute_brick( new_dset , iv ,
                             DSET_BRICK_TYPE(DT_dset,iv) ,
                             DSET_ARRAY(DT_dset,iv)       ) ;

   /* load reference (detrending) vectors;
      setup to do least squares fitting of each voxel */

   nvec = 0 ;
   for( ii=0 ; ii < IMARR_COUNT(DT_imar) ; ii++ )
      nvec += IMARR_SUBIMAGE(DT_imar,ii)->ny ;

   refvec = (float **) malloc( sizeof(float *)*nvec ) ;
   for( kk=ii=0 ; ii < IMARR_COUNT(DT_imar) ; ii++ ){
      fv = MRI_FLOAT_PTR( IMARR_SUBIMAGE(DT_imar,ii) ) ;
      for( jj=0 ; jj < IMARR_SUBIMAGE(DT_imar,ii)->ny ; jj++ )
         refvec[kk++] = fv + ( jj * IMARR_SUBIMAGE(DT_imar,ii)->nx ) ;
   }

   choleski = startup_lsqfit( nvals , NULL , nvec , refvec ) ;
   if( choleski == NULL ){
      fprintf(stderr,"*** Linearly dependent vectors can't be used!\n") ;
      exit(1) ;
   }

   /* loop over voxels, fitting and detrending (or replacing) */

   nvox = DSET_NVOX(new_dset) ;
   fit  = (float *) malloc( sizeof(float) * nvals ) ;

   if( DT_verb ) fprintf(stderr,"+++ Computing voxel fits\n") ;

   for( kk=0 ; kk < nvox ; kk++ ){

      flim = THD_extract_series( kk , new_dset , 0 ) ;              /* data */
      fv   = MRI_FLOAT_PTR(flim) ;
      fc   = delayed_lsqfit( nvals, fv, nvec, refvec, choleski ) ;  /* coef */

      for( ii=0 ; ii < nvals ; ii++ ) fit[ii] = 0.0 ;

      for( jj=0 ; jj < nvec ; jj++ )
         for( ii=0 ; ii < nvals ; ii++ )
            fit[ii] += fc[jj] * refvec[jj][ii] ;                    /* fit */

      if( !DT_replace )                                             /* remove */
         for( ii=0 ; ii < nvals ; ii++ ) fit[ii] = fv[ii] - fit[ii] ;

      THD_insert_series( kk, new_dset, nvals, MRI_float, fit, 0 ) ;

      free(fc) ; mri_free(flim) ;
   }

   if( DT_verb ) fprintf(stderr,"+++ Writing output dataset\n") ;
   DSET_write(new_dset) ;
   exit(0) ;
}
