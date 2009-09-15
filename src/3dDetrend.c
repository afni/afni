/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "parser.h"

/*---------------------------------------------------------------------------
  This program detrends voxel time series.
  It will also cure bad breath, fix parking tickets, and
  get Green Bay to the Super Bowl.
----------------------------------------------------------------------------*/

#define ALLOW_BYSLICE

static THD_3dim_dataset * DT_dset    = NULL ;
static MRI_IMARR *        DT_imar    = NULL ;
static char **            DT_expr    = NULL ;
static PARSER_code **     DT_excode  = NULL ;
static float *            DT_exdel   = NULL ;
static int *              DT_exvar   = NULL ;
static int                DT_exnum   = 0    ;
static int                DT_verb    = 0    ;
static int                DT_replace = 0    ;
static int                DT_norm    = 0    ;  /* 23 Nov 1999 */
static int                DT_nvector = 0    ;  /* 08 Dec 1999 */

#ifdef ALLOW_BYSLICE
static int                DT_byslice = 0    ;  /* 08 Dec 1999 */
#endif

static float              DT_current_del = -1.0 ;

static int                DT_polort  = -1   ;  /* 10 Apr 2006 */

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
   int nopt = 1 , nvals , ii , nvcheck , nerr=0 ;
   MRI_IMARR *slice_imar ;

   INIT_IMARR(DT_imar) ;
   INIT_IMARR(slice_imar) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** -polort p ****/

      if( strncmp(argv[nopt],"-polort",6) == 0 ){
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("Need argument after -polort") ;
        DT_polort = (int)strtod(argv[nopt],NULL) ;
        if( DT_polort < 0 )
          WARNING_message("Ignoring negative value after -polort") ;
        nopt++ ; continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("Need argument after -prefix") ;
        MCW_strncpy( DT_output_prefix , argv[nopt] , THD_MAX_PREFIX ) ;
        if( !THD_filename_ok(DT_output_prefix) )
          ERROR_exit("bad name '%s' after -prefix",argv[nopt]) ;
        nopt++ ; continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("Need argument after -session") ;
        MCW_strncpy( DT_session , argv[nopt] , THD_MAX_NAME ) ;
        if( !THD_filename_ok(DT_session) )
          ERROR_exit("bad name '%s' after -session",argv[nopt]) ;
        nopt++ ; continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
        DT_verb++ ; nopt++ ; continue ;
      }

      /**** -replace ****/

      if( strncmp(argv[nopt],"-replace",5) == 0 ){
        DT_replace++ ; nopt++ ; continue ;
      }

      /**** -byslice [08 Dec 1999] ****/

      if( strncmp(argv[nopt],"-byslice",5) == 0 ){
#ifdef ALLOW_BYSLICE
        if( IMARR_COUNT(slice_imar) > 0 )
          ERROR_exit("can't mix -byslice and -slicevector") ;
        DT_byslice++ ; nopt++ ; continue ;
#else
        ERROR_exit("-byslice is no longer suppported") ;
#endif
      }

      /**** -normalize [23 Nov 1999] ****/

      if( strncmp(argv[nopt],"-normalize",5) == 0 ){
        DT_norm++ ; nopt++ ; continue ;
      }

      /**** -vector ****/

      if( strncmp(argv[nopt],"-vector",4) == 0 ){
        MRI_IMAGE * flim ;
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("need argument after -vector") ;
        flim = mri_read_1D( argv[nopt++] ) ;
        if( flim == NULL ) ERROR_exit("can't read -vector '%s'",argv[nopt-1]) ;
        ADDTO_IMARR(DT_imar,flim) ;
        if( DT_verb ) INFO_message("Read file %s: rows=%d cols=%d",
                                   argv[nopt-1],flim->ny,flim->nx ) ;
        continue ;
      }

      /**** -slicevector ****/

      if( strncmp(argv[nopt],"-slicevector",6) == 0 ){
        MRI_IMAGE *flim ;
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("need argument after -slicevector") ;
#ifdef ALLOW_BYSLICE
        if( DT_byslice )   ERROR_exit("can't mix -slicevector and -byslice") ;
#endif
        flim = mri_read_1D( argv[nopt++] ) ;
        if( flim == NULL ) ERROR_exit("can't read -slicevector '%s'",argv[nopt-1]) ;
        ADDTO_IMARR(slice_imar,flim) ;
        if( DT_verb ) INFO_message("Read file %s: rows=%d cols=%d",
                                   argv[nopt-1],flim->ny,flim->nx ) ;
        continue ;
      }

      /**** -del ****/

      if( strncmp(argv[nopt],"-del",4) == 0 ){
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("need argument after -del") ;
        DT_current_del = strtod( argv[nopt++] , NULL ) ;
        if( DT_verb )
          INFO_message("Set expression stepsize = %g\n",DT_current_del) ;
        continue ;
      }

      /**** -expr ****/

      if( strncmp(argv[nopt],"-expr",4) == 0 ){
        int nexp , qvar , kvar ;
        char sym[4] ;

        nopt++ ;
        if( nopt >= argc ) ERROR_exit("need argument after -expr") ;

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
        if( DT_excode[DT_exnum] == NULL )
          ERROR_exit("Illegal expression: '%s'",argv[nopt]) ;

        qvar = 0 ; kvar = -1 ;                       /* find symbol */
        for( ii=0 ; ii < 26 ; ii++ ){
          sym[0] = 'A' + ii ; sym[1] = '\0' ;
          if( PARSER_has_symbol(sym,DT_excode[DT_exnum]) ){
            qvar++ ; if( kvar < 0 ) kvar = ii ;
            if( DT_verb )
              INFO_message("Found expression symbol %s\n",sym) ;
          }
        }
        if( qvar > 1 )
          ERROR_exit("-expr '%s' has too many symbols",DT_expr[DT_exnum]) ;
        else if( qvar == 0 )
          WARNING_message("-expr '%s' is constant",DT_expr[DT_exnum]) ;
        DT_exvar[DT_exnum] = kvar ;
        DT_exnum = nexp ; nopt++ ; continue ;
      }

      /**** ERROR ****/

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;

   }  /* end of scan over options */

   /*-- check for errors --*/

   if( nopt >= argc ) ERROR_exit("No input dataset?!") ;

#ifdef ALLOW_BYSLICE
   if( IMARR_COUNT(slice_imar) > 0 && DT_byslice )
     ERROR_exit("Illegal mixing of -slicevector and -byslice") ;
#endif

   DT_nvector = IMARR_COUNT(DT_imar) ;
   if( DT_nvector + DT_exnum == 0 && DT_polort < 0 )
     ERROR_exit("No detrending options ordered!") ;

#ifdef ALLOW_BYSLICE
   if( DT_nvector == 0 && DT_byslice )
     ERROR_exit("No -vector option supplied with -byslice!") ;
#endif

   /*--- read input dataset ---*/

   DT_dset = THD_open_dataset( argv[nopt] ) ;
   CHECK_OPEN_ERROR(DT_dset,argv[nopt]) ;
   if( DT_dset == NULL )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]) ;

   DT_current_del = DSET_TR(DT_dset) ;
   if( DT_current_del <= 0.0 ){
     DT_current_del = 1.0 ;
     if( DT_verb )
       WARNING_message("Input has no TR value; setting TR=1.0\n") ;
   } else if( DT_verb ){
     INFO_message("Input has TR=%g\n",DT_current_del) ;
   }

   /*-- check vectors for good size --*/

   nvcheck = nvals = DSET_NVALS(DT_dset) ;
#ifdef ALLOW_BYSLICE
   if( DT_byslice ) nvcheck *= DSET_NZ(DT_dset) ;
#endif
   for( ii=0 ; ii < DT_nvector ; ii++ ){
     if( IMARR_SUBIMAGE(DT_imar,ii)->nx < nvcheck ){
       ERROR_message("%d-th -vector is shorter (%d) than dataset (%d)",
                     ii+1,IMARR_SUBIMAGE(DT_imar,ii)->nx,nvcheck) ;
       nerr++ ;
     }
   }
   if( nerr > 0 ) ERROR_exit("Cannot continue") ;

   /*--- create time series from expressions */

   if( DT_exnum > 0 ){
     double atoz[26] , del ;
     int kvar , jj ;
     MRI_IMAGE *flim ;
     float *flar ;

     for( jj=0 ; jj < DT_exnum ; jj++ ){
       if( DT_verb ) INFO_message("Evaluating %d-th -expr\n",jj+1) ;
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

   /*--- from polort [10 Apr 2006] ---*/

   if( DT_polort >= 0 ){
     int kk ;
     MRI_IMAGE *flim ;
     float *flar ; double fac=2.0/(nvals-1.0) ;

     for( kk=0 ; kk <= DT_polort ; kk++ ){
       flim = mri_new( nvals , 1 , MRI_float ) ;
       flar = MRI_FLOAT_PTR(flim) ;
       for( ii=0 ; ii < nvals ; ii++ ) flar[ii] = Plegendre(fac*ii-1.0,kk) ;
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
    "* This program removes components from voxel time series using\n"
    "  linear least squares.  Each voxel is treated independently.\n"
    "* Note that least squares detrending is equivalent to orthogonalizing\n"
    "  the input dataset time series with respect to the basis time series\n"
    "  provided by the '-vector', '-polort', et cetera options.\n"
    "* The input dataset may have a sub-brick selector string; otherwise,\n"
    "  all sub-bricks will be used.\n\n"
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
    " -normalize    = Normalize each output voxel time series; that is,\n"
    "                   make the sum-of-squares equal to 1.\n"
    "           N.B.: This option is only valid if the input dataset is\n"
    "                   stored as floats! (1D files are always floats.)\n"
#ifdef ALLOW_BYSLICE
    " -byslice      = Treat each input vector (infra) as describing a set of\n"
    "                   time series interlaced across slices.  If NZ is the\n"
    "                   number of slices and NT is the number of time points,\n"
    "                   then each input vector should have NZ*NT values when\n"
    "                   this option is used (usually, they only need NT values).\n"
    "                   The values must be arranged in slice order, then time\n"
    "                   order, in each vector column, as shown here:\n"
    "                       f(z=0,t=0)       // first slice, first time\n"
    "                       f(z=1,t=0)       // second slice, first time\n"
    "                       ...\n"
    "                       f(z=NZ-1,t=0)    // last slice, first time\n"
    "                       f(z=0,t=1)       // first slice, second time\n"
    "                       f(z=1,t=1)       // second slice, second time\n"
    "                       ...\n"
    "                       f(z=NZ-1,t=NT-1) // last slice, last time\n"
#endif
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
    "                   from the dataset.\n"
    "\n"
    " -polort ppp   = Add Legendre polynomials of order up to and\n"
    "                   including 'ppp' in the list of vectors to remove.\n"
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
#ifdef ALLOW_BYSLICE
    "\n"
    " N.B.: expressions are NOT calculated on a per-slice basis when the\n"
    "        -byslice option is used.  If you have to do this, you could\n"
    "        compute vectors with the required time series using 1deval.\n"
#endif
   ) ;

   printf("\n"
    "Detrending 1D files\n"
    "-------------------\n"
    "As far as '3d' programs are concerned, you can input a 1D file as\n"
    "a 'dataset'.  Each row is a separate voxel, and each column is a\n"
    "separate time point.  If you want to detrend a single column, then\n"
    "you need to transpose it on input.  For example:\n"
    "\n"
    "  3dDetrend -prefix - -vector G1.1D -polort 3 G5.1D\\' | 1dplot -stdin\n"
    "\n"
    "Note that the '-vector' file is NOT transposed with \\', but that\n"
    "the input dataset file IS transposed.  This is because in the first\n"
    "case the program expects a 1D file, and so knows that the column\n"
    "direction is time.  In the second case, the program expects a 3D\n"
    "dataset, and when given a 1D file, knows that the row direction is\n"
    "time -- so it must be transposed.  I'm sorry if this is confusing,\n"
    "but that's the way it is.\n"
    "\n"
    "NOTE: to have the output file appear so that time is in the column\n"
    "      direction, you'll have to add the option '-DAFNI_1D_TRANOUT=YES'\n"
    "      to the command line, as in\n"
    "\n"
    "  3dDetrend -DAFNI_1D_TRANOUT=YES -prefix - -vector G1.1D -polort 3 G5.1D\\' > Q.1D\n"
    "\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
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

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dDetrend main"); machdep() ; PRINT_VERSION("3dDetrend");
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

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

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) )
     ERROR_exit("File %s already exists!\n",DSET_HEADNAME(new_dset) ) ;

   /* read input in, and attach its bricks to the output dataset */
   /* (not good in a plugin, but OK in a standalone program!)    */

   if( DT_verb ) INFO_message("Loading input dataset bricks\n") ;

   DSET_mallocize( new_dset ) ;
   DSET_mallocize( DT_dset ) ;
   DSET_load( DT_dset ) ; CHECK_LOAD_ERROR(DT_dset) ;

   nvals = DSET_NVALS(new_dset) ;
   for( iv=0 ; iv < nvals ; iv++ )
     EDIT_substitute_brick( new_dset , iv ,
                            DSET_BRICK_TYPE(DT_dset,iv) ,
                            DSET_ARRAY(DT_dset,iv)       ) ;

   if( DT_norm && DSET_BRICK_TYPE(new_dset,0) != MRI_float ){
     INFO_message("Turning -normalize option off (input not in float format)");
     DT_norm = 0 ;
   }

   /* load reference (detrending) vectors;
      setup to do least squares fitting of each voxel */

   nvec = 0 ;
   for( ii=0 ; ii < IMARR_COUNT(DT_imar) ; ii++ )  /* number of detrending vectors */
      nvec += IMARR_SUBIMAGE(DT_imar,ii)->ny ;

   refvec = (float **) malloc( sizeof(float *)*nvec ) ;
   for( kk=ii=0 ; ii < IMARR_COUNT(DT_imar) ; ii++ ){
     fv = MRI_FLOAT_PTR( IMARR_SUBIMAGE(DT_imar,ii) ) ;
     for( jj=0 ; jj < IMARR_SUBIMAGE(DT_imar,ii)->ny ; jj++ )         /* compute ptr */
       refvec[kk++] = fv + ( jj * IMARR_SUBIMAGE(DT_imar,ii)->nx ) ;  /* to vectors  */
   }

   fit = (float *) malloc( sizeof(float) * nvals ) ;  /* will get fit to voxel data */

   /*--- do the all-voxels-together case ---*/

   if( !DT_byslice ){
      choleski = startup_lsqfit( nvals , NULL , nvec , refvec ) ;
      if( choleski == NULL )
        ERROR_exit("Choleski factorization fails: linearly dependent vectors!\n") ;

      /* loop over voxels, fitting and detrending (or replacing) */

      nvox = DSET_NVOX(new_dset) ;

      if( DT_verb ) INFO_message("Computing voxel fits\n") ;

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

         if( DT_norm ) THD_normalize( nvals , fit ) ;  /* 23 Nov 1999 */

         THD_insert_series( kk, new_dset, nvals, MRI_float, fit, 0 ) ;

         free(fc) ; mri_free(flim) ;
      }

      free(choleski) ;

      /*- end of all-voxels-together case -*/

   }
#ifdef ALLOW_BYSLICE
     else {                                 /*- start of slice case [08 Dec 1999] -*/
      int ksl , nslice , tt , nx,ny , nxy , kxy ;
      MRI_IMAGE * vim ;

      /* make separate space for the slice-wise detrending vectors */

      for( kk=ii=0 ; ii < DT_nvector ; ii++ ){
         for( jj=0 ; jj < IMARR_SUBIMAGE(DT_imar,ii)->ny ; jj++ )       /* replace ptrs */
            refvec[kk++] = (float *) malloc( sizeof(float) * nvals ) ;  /* to vectors   */
      }

      nslice = DSET_NZ(new_dset) ;
      nxy    = DSET_NX(new_dset) * DSET_NY(new_dset) ;

      /* loop over slices */

      for( ksl=0 ; ksl < nslice ; ksl++ ){

         if( DT_verb ) INFO_message("Computing voxel fits for slice %d\n",ksl) ;

         /* extract slice vectors from input interlaced vectors */

         for( kk=ii=0 ; ii < DT_nvector ; ii++ ){        /* loop over vectors */
            vim = IMARR_SUBIMAGE(DT_imar,ii) ;           /* ii-th vector image */
            nx = vim->nx ; ny = vim->ny ;                /* dimensions */
            for( jj=0 ; jj < ny ; jj++ ){                /* loop over columns */
               fv = MRI_FLOAT_PTR(vim) + (jj*nx) ;       /* ptr to column */
               for( tt=0 ; tt < nvals ; tt++ )           /* loop over time */
                  refvec[kk][tt] = fv[ksl+tt*nslice] ;   /* data point */
            }
         }

         /* initialize fitting for this slice */

         choleski = startup_lsqfit( nvals , NULL , nvec , refvec ) ;
         if( choleski == NULL )
           ERROR_exit("Choleski fails: linearly dependent vectors at slice %d\n",ksl) ;

         /* loop over voxels in this slice */

         for( kxy=0 ; kxy < nxy ; kxy++ ){

            kk   = kxy + ksl*nxy ;                                        /* 3D index */
            flim = THD_extract_series( kk , new_dset , 0 ) ;              /* data */
            fv   = MRI_FLOAT_PTR(flim) ;
            fc   = delayed_lsqfit( nvals, fv, nvec, refvec, choleski ) ;  /* coef */

            for( ii=0 ; ii < nvals ; ii++ ) fit[ii] = 0.0 ;

            for( jj=0 ; jj < nvec ; jj++ )
               for( ii=0 ; ii < nvals ; ii++ )
                  fit[ii] += fc[jj] * refvec[jj][ii] ;                    /* fit */

            if( !DT_replace )                                             /* remove */
               for( ii=0 ; ii < nvals ; ii++ ) fit[ii] = fv[ii] - fit[ii] ;

            if( DT_norm ) THD_normalize( nvals , fit ) ;  /* 23 Nov 1999 */

            THD_insert_series( kk, new_dset, nvals, MRI_float, fit, 0 ) ;

            free(fc) ; mri_free(flim) ;
         }

         free(choleski) ;

      } /* end of loop over slices */

   } /*- end of -byslice case -*/
#endif

   /*-- done done done done --*/

   DSET_write(new_dset) ;
   if( DT_verb ) WROTE_DSET(new_dset) ;
   exit(0) ;
}
