#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*----------------------------------------------------------------------------*/

typedef struct {
   THD_3dim_dataset         *inset ;
   THD_3dim_dataset        *outset ;
   THD_3dim_dataset       *maskset ;
   int                      polort ;
   int                  do_despike ;
   int                     do_norm ;
   MRI_IMARR                *ortar ;
   THD_3dim_dataset_array *dsortar ;
   float_pair            *stopband ;
   int                   nstopband ;
   float                        dt ;
   char                    *prefix ;
   float                      blur ;
   float                   *censar ;
   int_triple          *abc_CENSOR ;
   int                  num_CENSOR ;
   int                     cenmode ;
   int                        verb ;
} TPR_input ;

#define CEN_ZERO 1
#define CEN_KILL 2

static TPR_input tin = { NULL,NULL,NULL , 2 , 0,0 ,
                         NULL,NULL,       NULL,0 ,
                         0.0f , "Tproject" , 0.0f , NULL,NULL,0,CEN_KILL , 1 } ;

static TPR_input *tinp = &tin ;

#undef  ADD_STOPBAND
#define ADD_STOPBAND(bb,tt)                                                       \
 do{ tinp->stopband = (float_pair *)realloc(                                      \
                        tinp->stopband, sizeof(float_pair)*(tinp->nstopband+1)) ; \
     tinp->stopband[tinp->nstopband].a = (bb) ;                                   \
     tinp->stopband[tinp->nstopband].b = (tt) ; tinp->nstopband++ ;               \
 } while(0)

/*----------------------------------------------------------------------------*/

void TPR_help(void)
{
  printf(
   "\n"
   "Usage:  3dTproject [options]\n"
   "\n"
   "This program projects (detrends) out various 'nuisance' time series from each\n"
   "voxel in the input dataset.  Note that all the projections are done via linear\n"
   "regression, including the frequency-based options such as '-passband'.  In this\n"
   "way, you can bandpass time-censored data, and at the same time, remove other\n"
   "time series of no interest (e.g., physiological estimates, motion parameters).\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -input dataset      = Specifies the input dataset.\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
   "\n"
   " -despike            = Despike each input dataset time series before\n"
   "                       other processing.\n"
   "\n"
   " -censor cname       = As in 3dDeconvolve.\n"
   " -CENSORTR clist     = As in 3dDeconvolve.\n"
   " -cenmode mode       = 'mode' specifies how censored time points are treated in\n"
   "                       the output dataset:\n"
   "                       ++ mode = ZERO ==> put zero values in their place\n"
   "                                      ==> output datset is same length as input\n"
   "                       ++ mode = KILL ==> remove those time points\n"
   "                                      ==> output dataset is shorter than input\n"
   "                       ** The default mode is KILL !!!\n"
   "\n"
   " -ort f.1D           = Remove each column in f.1D\n"
   "                       ++ Multiple -ort options are allowed.\n"
   " -polort pp          = Remove polynomials up to and including degree pp.\n"
   "                       ++ Default value is 2.\n"
   "                       ++ It makes no sense to use a value of pp greater than\n"
   "                          2, if you are bandpassing out the lower frequences!\n"
   " -dsort fset         = Remove the 3D+time time series in dataset fset.\n"
   "                       ++ That is, 'fset' contains a different nuisance time\n"
   "                          series for each voxel (e.g., from AnatICOR).\n"
   "                       ++ Multiple -dsort options are allowed.\n"
   "                       ++ These datasets are NOT despiked or blurred!\n"
   "\n"
   " -passband fbot ftop = Remove all frequences EXCEPT those in the range\n"
   "                       fbot..ftop.\n"
   "                       ++ Only one -passband option is allowed.\n"
   " -stopband sbot stop = Remove all frequencies in the range sbot..stop.\n"
   "                       ++ More than one -stopband option is allowed.\n"
   "                       ++ For example, '-passband 0.01 0.10' is equivalent to\n"
   "                          '-stopband 0 0.0099 -stopband 0.1001 9999'\n"
   " -dt dd              = Use time step dd for the frequency calculations,\n"
   " *OR* -TR dd           rather than the value stored in the dataset header.\n"
   "\n"
   " -mask mset          = Only operate on voxels nonzero in the mset dataset.\n"
   "                       ++ Use '-mask AUTO' to have the program generate the\n"
   "                          mask automatically.\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
   "\n"
   " -blur fff           = Blur (inside the mask only) with a filter that has\n"
   "                       width (FWHM) of fff millimeters.\n"
   "                       ++ Spatial blurring (if done) is the LAST operation,\n"
   "                          after the time series filtering.\n"
   "\n"
   " -norm               = Normalize each output time series to have sum of\n"
   "                       squares = 1.\n"
   "\n"
   " -quiet              = Hide the super-fun and thrilling progress messages.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* The input file is treated as one continuous imaging 'run'; no time\n"
   "   discontinuities (breaks) are allowed -- that is, you can't use a\n"
   "   '-concat' option.\n"
   "* All projections are done in one operation.  If you like technical\n"
   "   math jargon (and who doesn't?), this program performs orthogonal\n"
   "   projection onto the null space of the set of input vectors\n"
   "   assembled from the various options.\n"
   "* If option '-dsort' is used, each voxel has a different matrix of\n"
   "   regressors.  For efficiency, the voxel-independent and voxel-\n"
   "   -dependent parts of the projection are calculated separately\n"
   "   and merged using a bordering algorithm.\n"
#ifdef USE_OMP
   "* This version of the program is compiled using OpenMP for speed.\n"
#else
   "* This version of the program is not compiled with OpenMP, but OpenMP\n"
   "   binaries DO exist, and using OpenMP will speed the program up.\n\n"
#endif
   "* Authored by RWCox in a fit of excessive linear algebra [summer 2013].\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

static char * get_psinv_wsp( int m , int n )
{
   char *wsp ;

#if 0
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */
   vmat = (double *)calloc( sizeof(double),n*n );
   umat = (double *)calloc( sizeof(double),m*n );   /* left singular vectors */
   sval = (double *)calloc( sizeof(double),n   );   /* singular values */
#endif

   wsp = (char *)malloc( sizeof(double) * (2*m*n + 2*n + n*n) ) ;
   return wsp ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float image.
    If the input (rmat) is m X n, the output (pmat) is n X m.
*//*--------------------------------------------------------------------------*/

static void compute_psinv( int m, int n, float *rmat, float *pmat, double *wsp )
{
   int ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del ;
   register double sum ;

   /* deal with a single vector (of length m) */

   if( n == 1 ){
     for( sum=0.0,ii=0 ; ii < m ; ii++ ) sum += rmat[ii]*rmat[ii] ;
     if( sum > 0.0 ){
       sum = 1.0 / sum ;
       for( ii=0 ; ii < m ; ii++ ) pmat[ii] = sum * rmat[ii] ;
     } else {
       for( ii=0 ; ii < m ; ii++ ) pmat[ii] = 0.0f ;
     }
     return ;
   }

   /* OK, have a real matrix to handle here */

#if 0
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */
   vmat = (double *)calloc( sizeof(double),n*n );
   umat = (double *)calloc( sizeof(double),m*n );   /* left singular vectors */
   sval = (double *)calloc( sizeof(double),n   );   /* singular values */
#else
   amat = wsp ;
   xfac = amat + m*n ;
   vmat = xfac + n ;
   umat = vmat + n*n ;
   sval = umat + m*n ;
#endif

#undef  PSINV_EPS
#define PSINV_EPS 1.e-12

#undef  R
#undef  A
#undef  P
#undef  U
#undef  V
#define R(i,j) rmat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define A(i,j) amat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define P(i,j) pmat[(i)+(j)*n]   /* i=0..n-1 , j=0..m-1 */
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     for( sum=0.0,ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ;
     xfac[jj] = sum ;
     if( sum > 0.0 ){
       for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
     }
   }

   /* compute SVD of scaled matrix */

   svd_double( m , n , amat , sval , umat , vmat ) ;

   /* find largest singular value */

   smax = sval[0] ; if( smax < 0.0 ) smax = 0.0 ;
   for( ii=1 ; ii < n ; ii++ ){
          if( sval[ii] > smax ) smax = sval[ii] ;
     else if( sval[ii] < 0.0  ) sval[ii] = 0.0 ;   /* should not happen */
   }

   if( smax <= 0.0 ){                        /* this is bad */
     static int first = 1 ;
#pragma omp critical (STDERR)
     { if( first ) ERROR_message("SVD fails in compute_psinv()!\n"); }
     AAmemset( pmat , 0 , sizeof(double)*m*n ) ; first = 0 ; return ;
   }

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

   del = PSINV_EPS * smax*smax ;
   for( ii=0 ; ii < n ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
       P(ii,jj) = (float)sum ;
     }
   }

   /** must now rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

static void project_out_once( int nr, int nc,
                              float *aar, float *par,
                              float *var, float *wsp )
{
   float *ap,*pp,vv ; int ii,jj,kk ;

   for( ii=0 ; ii < nc ; ii++ ) wsp[ii] = 0.0f ;
   for( kk=0 ; kk < nr ; kk++ ){
     pp = par + kk*nc ; vv = var[kk] ;
     for( ii=0 ; ii < nc ; ii++ ) wsp[ii] += pp[ii]*vv ;
   }
   for( kk=0 ; kk < nc ; kk++ ){
     ap = aar + kk*nr ; vv = wsp[kk] ;
     for( ii=0 ; ii < nr ; ii++ ) var[ii] -= ap[ii]*vv ;
   }
   return ;
}

/*----------------------------------------------------------------------------*/

static void project_out_twice( int nr , int nc , float *aar , float *par  ,
                                        int nb , float *bar , float *pbar ,
                                                 float *var , char *wsp    )
{
   project_out_once( nr,nc , aar,par  , var , (float *)wsp ) ;

   if( nb > 0 ){
     int kk ;
     for( kk=0 ; kk < nb ; kk++ )
       project_out_once( nr,nc,aar,par , bar+kk*nr , (float *)wsp ) ;
     compute_psinv( nr,nb , bar , pbar , (double *)wsp ) ;
     project_out_once( nr,nb , bar,pbar , var , (float *)wsp ) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

void TPR_process_data( TPR_input *tp )
{
   /* check for errors:
        ortar vectors too short
        dsortar time series too short
        just too many orts            */

   /* make censor array */

   /* make mask, if present */

   /* make stopband frequency mask */

   /* create array of all fixed orts */

   /* make vector images from all input datasets */

   /* despike input dataset */

   /* censor fixed orts and all input datasets */

   /* filter time series */

   /* blurring */

   /* convert output time series into dataset */

   /* clean up whatever trash is still left */
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ct , nact=0 ;

   /*----------*/

   AFNI_SETUP_OMP(0) ;
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) TPR_help() ;

   /*----- scan options -----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       tinp->verb = 0 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( tinp->inset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg      >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->inset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(tinp->inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       if( tinp->maskset != NULL ) ERROR_exit("Can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg        >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->maskset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(tinp->maskset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->polort = (int)strtod(argv[iarg],NULL) ;
       if( tinp->polort > 20 ) ERROR_exit("-polort value can't be over 20 :-(") ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-despike") == 0 ){
       tinp->do_despike = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-norm") == 0 ){
       tinp->do_norm = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-ort") == 0 ){
       MRI_IMAGE *oim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->ortar == NULL ) INIT_IMARR(tinp->ortar) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         oim = mri_read_1D( argv[iarg] ) ;
         if( oim == NULL ) ERROR_exit("-ort: can't read file '%s'",argv[iarg]) ;
         ADDTO_IMARR(tinp->ortar,oim) ;
       }
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-dsort") == 0 ){
       THD_3dim_dataset *dsim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->dsortar == NULL ) INIT_3DARR(tinp->dsortar) ;
       dsim = THD_open_dataset(argv[iarg]) ; CHECK_OPEN_ERROR(dsim,argv[iarg]) ;
       ADDTO_3DARR(tinp->dsortar,dsim) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-stopband") == 0 ){
       float sbot,stop ;
       if( ++iarg >= argc-1 ) ERROR_exit("Need 2 values after option '%s'",argv[iarg-1]) ;
       sbot = (float)strtod(argv[iarg++],NULL) ;
       stop = (float)strtod(argv[iarg++],NULL) ;
       if( sbot <  0.0f ) sbot = 0.0f ;
       if( stop <= sbot ) ERROR_exit("-stopband: range %.5f %.5f is illegal :-(",sbot,stop) ;
       ADD_STOPBAND(sbot,stop) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-passband") == 0 ){
       float fbot,ftop ;
       if( ++iarg >= argc-1 ) ERROR_exit("Need 2 values after option '%s'",argv[iarg-1]) ;
       fbot = (float)strtod(argv[iarg++],NULL) ;
       ftop = (float)strtod(argv[iarg++],NULL) ;
       if( fbot <  0.0f ) fbot = 0.0f ;
       if( ftop <= fbot ) ERROR_exit("-stopband: range %.5f %.5f is illegal :-(",fbot,ftop) ;
       ADD_STOPBAND(0.0f,fbot) ;
       ADD_STOPBAND(ftop,99999.9f) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-dt") == 0 || strcasecmp(argv[iarg],"-TR") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->dt = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-blur") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->blur = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       tinp->prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(tinp->prefix) )
         ERROR_exit("-prefix '%s' is not acceptable :-(",tinp->prefix) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-cenmode") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
            if( strcasecmp(argv[iarg],"KILL") == 0 ) tinp->cenmode = CEN_KILL ;
       else if( strcasecmp(argv[iarg],"ZERO") == 0 ) tinp->cenmode = CEN_ZERO ;
       else ERROR_message("unknown value '%s' after -cenmode",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-censor") == 0 ){
       MRI_IMAGE *cenim ;
       if( ++iarg >= argc ) ERROR_exit("Need value after option '%s'",argv[iarg-1]) ;
       if( tinp->censar != NULL ) ERROR_exit("Can't use -censor more than once!") ;
       cenim = mri_read_1D(argv[iarg]) ;
       if( cenim == NULL ) ERROR_exit("-censor can't read file '%s'",argv[iarg]) ;
       tinp->censar = MRI_FLOAT_PTR(cenim) ;
       mri_clear_and_free(cenim) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-censorTR") == 0 ){
        NI_str_array *nsar ;
        char *src=malloc(1), *cpt, *dpt ;
        int ns, r,a,b, nerr=0 ; int_triple rab ;

        *src = '\0' ;   /* cat all following options until starts with '-' */
        for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
          ns = strlen(argv[iarg]) ; if( ns == 0 ) continue ;
          src = realloc(src,strlen(src)+ns+2) ;
          strcat(src," ") ; strcat(src,argv[iarg]) ;
        }
        if( *src == '\0' ) ERROR_exit("Bad (or no) argument after -CENSORTR") ;
        nsar = NI_decode_string_list( src , "," ) ; /* break into substrings */
        for( ns=0 ; ns < nsar->num ; ns++ ){ /* loop over substrings */
          cpt = nsar->str[ns] ; dpt = strchr(cpt,':') ; r = 0 ;
          if( *cpt == '\0' ) continue ;   /* skip an empty string */
          if( dpt != NULL ){              /* found 'run:' */
            if( *cpt == '*' ){ /* wildcard = all runs */
              r = -666 ;
            } else {
              r = (int)strtol(cpt,NULL,10) ;
              if( r <= 0 ){  /* skip out */
                ERROR_message("-CENSORTR %s -- run index '%d' is bad! [iarg=%d]",nsar->str[ns],r,iarg);
                nerr++ ; continue ;
              }
            }
            cpt = dpt+1 ;  /* skip to character after ':' */
            if( *cpt == '\0' ){  /* skip out */
              ERROR_message("-CENSORTR %s -- no data after run index! [iarg=%d]",nsar->str[ns],iarg);
              nerr++ ; continue ;
            }
          }
          a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
          if( a < 0 ){  /* skip out */
            ERROR_message("-CENSORTR %s -- time index '%d' is bad! [iarg=%d]",nsar->str[ns],a,iarg);
            nerr++ ; continue ;
          }
          if( *dpt == '\0' ){  /* no second number */
            b = a ;
          } else {             /* get second number */
            for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
            b = (int)strtol(dpt,NULL,10) ;
            if( b < a || b < 0 ){  /* skip out */
              ERROR_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad! [iarg=%d]",
                            nsar->str[ns],a,b,iarg);
              nerr++ ; continue ;
            }
          }
          tinp->abc_CENSOR = (int_triple *)realloc( tinp->abc_CENSOR ,
                                              sizeof(int_triple)*(tinp->num_CENSOR+1) );
          rab.i = r; rab.j = a; rab.k = b; tinp->abc_CENSOR[tinp->num_CENSOR++] = rab ;
        } /* end of loop over -CENSORTR strings */
        if( nerr > 0 ) ERROR_exit("Can't proceed after -CENSORTR errors! [iarg=%d]",iarg) ;
        NI_delete_str_array(nsar) ; free(src) ;
        continue ;  /* next option */
     }

     /*--- error! ---*/

     ERROR_exit("Don't know what to do with option '%s' :-(",argv[iarg]) ;

   } /* end of option scanning loop */

   /*----- error checking -----*/

   if( tinp->inset == NULL )
     ERROR_exit("no input dataset?") ;

   if( DSET_NVALS(tinp->inset) < 9 )
     ERROR_exit("input dataset has fewer than 9 time points?") ;

   if( tinp->maskset != NULL && !EQUIV_GRIDXYZ(tinp->inset,tinp->maskset) )
     ERROR_exit("mask and input datasets are NOT on the same 3D grid?") ;

   DSET_LOAD(tinp->inset) ; CHECK_LOAD_ERROR(tinp->inset) ;
   if( tinp->maskset != NULL ){
     DSET_LOAD(tinp->maskset) ; CHECK_LOAD_ERROR(tinp->maskset) ;
   }

   nact = 0 ;
   if( tinp->do_despike       ) nact++ ;
   if( tinp->polort >= 0      ) nact++ ;
   if( tinp->do_norm          ) nact++ ;
   if( tinp->ortar != NULL    ) nact++ ;
   if( tinp->dsortar != NULL  ) nact++ ;
   if( tinp->nstopband > 0    ) nact++ ;
   if( tinp->blur      > 0.0f ) nact++ ;
   if( tinp->censar != NULL   ) nact++ ;
   if( tinp->num_CENSOR > 0   ) nact++ ;
   if( nact == 0 )
     ERROR_exit("Don't you want to DO something?") ;

   /*----- bureaucracy -----*/

   mainENTRY("3dTproject"); machdep();
   AFNI_logger("3dTproject",argc,argv);
   PRINT_VERSION("3dTproject"); AUTHOR("Cox the Algebraic (Linear)") ;
   ct = NI_clock_time() ;

   /*----- process the data -----*/

   TPR_process_data( tinp ) ;

   if( tinp->outset != NULL ){
     tross_Copy_History( tinp->inset , tinp->outset ) ;
     tross_Make_History( "3dTproject" , argc,argv , tinp->outset ) ;
     DSET_write(tinp->outset) ;
     if( tinp->verb ) WROTE_DSET(tinp->outset) ;
   }

   exit(0) ;
}
