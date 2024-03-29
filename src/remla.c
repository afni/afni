/******* This file is meant to be #include-d into another application! *******/

#undef  MTYPE
#undef  MPAIR
#include "matrix.h"
#define MTYPE double
#define MPAIR double_pair

#include "mrilib.h"  /* Keep after decision about matrix.h inclusion
                                                      ZSS  Nov. 21 2014*/
#undef  BIGVAL
#define BIGVAL 1.e+38

/*---------------------------------------------------------------------------*/
/*** Some prototypes [May 2020] ***/

doublevec * REML_compute_arma31_correlations( double aa, double rr1, double th1,
                                              int kmax , double sfac ) ;

doublevec * REML_compute_arma51_correlations( double aa,
                                              double rr1, double th1,
                                              double rr2, double th2,
                                              int kmax  , double sfac ) ;

rcmat * rcmat_arma31( int nt, int *tau,
                      double aa, double rr, double th , double sfac ) ;

/*===========================================================================*/
#undef ENABLE_REMLA_MALLOC
#ifdef ENABLE_REMLA_MALLOC   /* only for tracking memory usage [20 May 2020] */
                             /* will make it run much more slowly with OMP! */

static int64_t remla_memtot = 0 ;      /* total memory used herein */
static Htable *remla_htable = NULL ;   /* table to keep track of it */

#undef malloc  /* make sure library functions are used below */
#undef calloc
#undef free

/*........................................*/

void * remla_malloc( size_t nb )
{
  void *mmm ;
  if( nb == 0 ) return NULL ;
#pragma omp critical (remla_malloc)
  { mmm = malloc(nb) ;
    if( mmm == NULL ){
      ERROR_message("remla_malloc(%ull) fails :(",nb) ;
    } else {
      remla_memtot += nb ;
      if( remla_htable != NULL ){
        char key[32] , *len ;
        sprintf(key,"%p",mmm) ;
        len = malloc(32) ; sprintf(len,"%lld",nb) ;
        addto_Htable( key , len , remla_htable ) ;
      }
    }
  }
  return mmm ;
}

/*........................................*/

void * remla_calloc( size_t na , size_t nb )
{
  void *mmm ; size_t nab=na*nb ;
  mmm = remla_malloc( nab ) ;
#pragma omp critical (remla_malloc)
  { if( mmm != NULL ) memset(mmm,0,nab) ; }
  return mmm ;
}

/*........................................*/

void remla_free( void *mmm )
{
   if( mmm == NULL ) return ;
#pragma omp critical (remla_malloc)
   { if( remla_htable != NULL ){
       char key[32] , *len ;
       sprintf(key,"%p",mmm) ;
       len = findin_Htable( key , remla_htable ) ;
       if( len != NULL ){
         size_t nb=0 ; sscanf(len,"%lld",&nb) ;
         remla_memtot -= nb ;
         removefrom_Htable( key , remla_htable ) ;
       }
     }
     free(mmm) ;
   }
   return ;
}

/*........................................*/

#define REMLA_memprint                                            \
  INFO_message("REML setup memory = %s (%s)",                     \
               approximate_number_string((double)remla_memtot) ,  \
               commaized_integer_string(remla_memtot)            )

#define REMLA_memreset  \
  do{ remla_memtot=0; destroy_Htable(remla_htable); remla_htable=NULL; } while(0)

#define REMLA_memsetup  \
  do{ remla_memtot=0; destroy_Htable(remla_htable); remla_htable=new_Htable(999); Htable_set_vtkill(1); } while(0)

#else  /*----------------- ENABLE_REMLA_MALLOC not defined -----------------*/

#define REMLA_memreset /*nada*/
#define REMLA_memsetup /*nada*/
#define REMLA_memprint /*nada*/

#define remla_malloc malloc
#define remla_calloc calloc
#define remla_free   free

#endif /*--------------- ENABLE_REMLA_MALLOC ---------------*/

/*===========================================================================*/

#ifdef USE_OMP     /* disable tracking macros from debugtrace.h */

# undef  RETURN
# undef  EXRETURN
# undef  ENTRY
# define ENTRY(x)  /*nada*/
# define RETURN(x) return(x)
# define EXRETURN  return

#endif /* USE_OMP */

#ifdef ENABLE_REMLA_MALLOC     /* cf. supra */
# define malloc remla_malloc   /* these are just for matrix.c */
# define calloc remla_calloc   /* and rcmat.c, included below */
# define free   remla_free
#endif

#include "matrix.c"    /* included here for optimization */
#include "rcmat.c"     /* and for memory tracking, maybe */

#ifdef ENABLE_REMLA_MALLOC
# undef malloc
# undef calloc
# undef free
#endif

/*------------------------------------------------------------------------*/

/*****
 Struct to hold a random sparse rectangular matrix.
*****/

typedef struct {
   int rows , cols ;
   int *cnum ;        /* cnum[j] is number of elements in col #j */
   int **cii ;        /* cii[j][k] is the i-index of cee[j][k]   */
   double **cee ;     /* for j=0..cols-1 , k=0..cnum[j]-1        */
} sparmat ;

/*****
  Struct to hold the info needed for a partial-F statistic
*****/

typedef struct {
   int mpar , rglt ; /* [beta_R] = [Jleft] [Jright] [beta_Full] */
   matrix *Jright ;  /* r X m matrix */
   matrix *Jleft ;   /* m x r matrix */
   vector *sig ;     /* rglt elements = normalized stdev */
} gltfactors ;

/*****
  Struct to hold the information needed to compute
  the REML log-likelihood for a given ARMA(1,1) case.
*****/

#undef  LAMBDA
#define LAMBDA(a,b) ((b+a)*(1.0+a*b)/(1.0+2.0*a*b+b*b))

#undef  ARMA11_MODEL
#define ARMA11_MODEL 1

#undef  ARMA31_MODEL
#define ARMA31_MODEL 3

#undef  ARMA51_MODEL
#define ARMA51_MODEL 5

typedef struct {
  int neq , mreg ;
  int noise_model ;  /* one of the MODEL constants above */

  /* params for ARMA11 */
  double rho , lam , barm ;

  /* params for ARMA31 or ARMA51 */
  double aa, rr1,th1 , rr2,th2 , sfac ;

  rcmat  *cc ;        /* banded matrix:    neq  X neq  */
  matrix *dd ;        /* upper triangular: mreg X mreg */
  double cc_logdet , dd_logdet ;

  int         nglt ;
  gltfactors **glt ;
} reml_setup ;

typedef struct {             /* for voxel-wise regression */
  reml_setup *rset ;
  matrix *X ; sparmat *Xs ;
} reml_setup_plus ;          /* 22 Jul 2015 */

/*****
  Struct to hold the information needed for all
  ARMA(1,1) cases, so that the best one can be found.
*****/

typedef struct {
  int na,nb , nab , pna,pnb , nset , izero , istwo ;
  double abot,da , bbot,db ;
  matrix *X ; sparmat *Xs ;
  reml_setup **rs ;
  char *savfil ;
  float avglen ;
} reml_collection ;

#undef  RC_SAVED
#define RC_SAVED(rc) \
  ( (rc)->savfil != NULL && (rc)->rs[(rc)->izero]->cc == NULL )

#undef  IAB
#define IAB(rc,a,b) ( (int)rint( ((a)-(rc)->abot) / (rc)->da )  \
     + (1+(rc)->na) * (int)rint( ((b)-(rc)->bbot) / (rc)->db ) )


/** correlation cutoff to control bandwidth of matrix **/
/** the value of corcut might be changed in 3dREMLfit **/

#define CORCUT_default 0.0001          /* reduced to 0.0001 [22 Aug 2019] */
static double corcut = CORCUT_default ;

#undef  TAU
#define TAU(i) ((tau==NULL) ? (i) : tau[i])

/****************************************************************************/
/******* Generic functions to process a sparse matrix in sparmat form. ******/
/****************************************************************************/

/*--------------------------------------------------------------------------*/

void vector_spc_multiply( sparmat *a , double *b , double *c )
{
   int rows=a->rows , cols=a->cols ;
   int k , j , cn , *ci ;
   double *ce , bj ;

   for( k=0 ; k < rows ; k++ ) c[k] = 0.0 ;

   for( j=0 ; j < cols ; j++ ){
     cn = a->cnum[j] ;
     ci = a->cii[j] ;
     ce = a->cee[j] ; bj = b[j] ;
     if( cn == rows )   /* all entries present in this column */
       for( k=0 ; k < cn ; k++ ) c[k] += ce[k] * bj ;
     else
       for( k=0 ; k < cn ; k++ ) c[ci[k]] += ce[k] * bj ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

void vector_spc_multiply_transpose( sparmat *a , double *b , double *c )
{
   int rows=a->rows , cols=a->cols ;
   int i , k , cn , *ci ;
   double *ce , bj , sum ;

   for( i=0 ; i < cols ; i++ ){
     cn = a->cnum[i] ;
     ci = a->cii[i] ;
     ce = a->cee[i] ;
     sum = 0.0 ;
     if( cn == rows )   /* all entries present in this column */
       for( k=0 ; k < cn ; k++ ) sum += ce[k] * b[k] ;
     else
       for( k=0 ; k < cn ; k++ ) sum += ce[k] * b[ci[k]] ;
     c[i] = sum ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

void vector_full_multiply( matrix *a , double *b , double *c )
{
   int rows=a->rows , cols=a->cols , i ;
   int j ; double sum , *aa ;

   switch( cols%4 ){
     case 0:
     for( i=0 ; i < rows ; i++ ){
       aa = a->elts[i] ; sum = 0.0 ;
       for( j=0;  j < cols;  j+=4 )
          sum += aa[j]*b[j]+aa[j+1]*b[j+1]+aa[j+2]*b[j+2]+aa[j+3]*b[j+3];
       c[i] = sum ;
     }
     break ;

     case 1:
     for( i=0 ; i < rows ; i++ ){
       aa = a->elts[i] ; sum = aa[0]*b[0] ;
       for( j=1;  j < cols;  j+=4 )
          sum += aa[j]*b[j]+aa[j+1]*b[j+1]+aa[j+2]*b[j+2]+aa[j+3]*b[j+3];
       c[i] = sum ;
     }
     break ;

     case 2:
     for( i=0 ; i < rows ; i++ ){
       aa = a->elts[i] ; sum = aa[0]*b[0]+aa[1]*b[1] ;
       for( j=2;  j < cols;  j+=4 )
          sum += aa[j]*b[j]+aa[j+1]*b[j+1]+aa[j+2]*b[j+2]+aa[j+3]*b[j+3];
       c[i] = sum ;
     }
     break ;

     case 3:
     for( i=0 ; i < rows ; i++ ){
       aa = a->elts[i] ; sum = aa[0]*b[0]+aa[1]*b[1]+aa[2]*b[2] ;
       for( j=3;  j < cols;  j+=4 )
          sum += aa[j]*b[j]+aa[j+1]*b[j+1]+aa[j+2]*b[j+2]+aa[j+3]*b[j+3];
       c[i] = sum ;
     }
     break ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/

void vector_full_multiply_transpose( matrix *a , double *b , double *c )
{
   int rows=a->rows , cols=a->cols , j ;
   int i ; double bj , *aa ;

   for( i=0 ; i < cols ; i++ ) c[i] = 0.0 ;

   switch( cols%4 ){
     case 0:
     for( j=0 ; j < rows ; j++ ){
       aa = a->elts[j] ; bj = b[j] ;
       for( i=0 ; i < cols ; i+=4 ){
         c[i]   += aa[i  ]*bj ; c[i+1] += aa[i+1]*bj ;
         c[i+2] += aa[i+2]*bj ; c[i+3] += aa[i+3]*bj ;
       }
     }
     break ;

     case 1:
     for( j=0 ; j < rows ; j++ ){
       aa = a->elts[j] ; bj = b[j] ;
       c[0] += aa[0]*bj ;
       for( i=1 ; i < cols ; i+=4 ){
         c[i]   += aa[i  ]*bj ; c[i+1] += aa[i+1]*bj ;
         c[i+2] += aa[i+2]*bj ; c[i+3] += aa[i+3]*bj ;
       }
     }
     break ;

     case 2:
     for( j=0 ; j < rows ; j++ ){
       aa = a->elts[j] ; bj = b[j] ;
       c[0] += aa[0]*bj ; c[1] += aa[1]*bj ;
       for( i=2 ; i < cols ; i+=4 ){
         c[i]   += aa[i  ]*bj ; c[i+1] += aa[i+1]*bj ;
         c[i+2] += aa[i+2]*bj ; c[i+3] += aa[i+3]*bj ;
       }
     }
     break ;

     case 3:
     for( j=0 ; j < rows ; j++ ){
       aa = a->elts[j] ; bj = b[j] ;
       c[0] += aa[0]*bj ; c[1] += aa[1]*bj ; c[2] += aa[2]*bj ;
       for( i=3 ; i < cols ; i+=4 ){
         c[i]   += aa[i  ]*bj ; c[i+1] += aa[i+1]*bj ;
         c[i+2] += aa[i+2]*bj ; c[i+3] += aa[i+3]*bj ;
       }
     }
     break ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/
/*! Solve [R] [x] = [b] for [x] where R is upper triangular. */

void vector_full_rr_solve( matrix *R , double *b , double *x )
{
   int n , ii,jj , n1 ;
   double sum , *rr ;

   n = R->rows ; n1 = n-1 ;

   /* backwards loop, from last element to first */

   for( ii=n1 ; ii >= 0 ; ii-- ){
     rr = R->elts[ii] ; sum = b[ii] ;
     for( jj=ii+1 ; jj < n1 ; jj+=2 )         /* unrolled by 2 */
       sum -= rr[jj] * x[jj] + rr[jj+1] * x[jj+1] ;
     if( jj == n1 ) sum -= rr[jj] * x[jj] ;  /* fix unroll if odd length */
     x[ii] = sum / rr[ii] ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/
/*! Solve [R]' [x] = [b] for [x] where R is upper triangular. */

void vector_full_rrtran_solve( matrix *R , double *b , double *x )
{
   int n , ii,jj , n1 ;
   double sum , *rr ;

   n = R->rows ; n1 = n-1 ;

#if 0             /* the obvious way, but is slower */
   for( ii=0 ; ii < n ; ii++ ){
     for( sum=b[ii],jj=0 ; jj < ii ; jj++ )
       sum -= R->elts[jj][ii] * x[jj] ;
     x[ii] = sum / R->elts[ii][ii] ;
   }
#else             /* the row ordered way, which is faster in cache */
   for( ii=0 ; ii < n ; ii++ ) x[ii] = b[ii] ;
   for( ii=0 ; ii < n ; ii++ ){
     rr = R->elts[ii] ; sum = x[ii] = x[ii] / rr[ii] ;
     for( jj=ii+1 ; jj < n1 ; jj+=2 ){     /* unrolled by 2 */
       x[jj] -= rr[jj]*sum ; x[jj+1] -= rr[jj+1]*sum ;
     }
     if( jj == n1 ) x[jj] -= rr[jj]*sum ; /* fix unroll if odd length */
   }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/

double sparsity_fraction( matrix a )
{
   int rows=a.rows , cols=a.cols , i,j,k=0 ; double val ;
   for( j=0 ; j < cols ; j++ )
     for( i=0 ; i < rows ; i++ ) if( a.elts[i][j] != 0.0 ) k++ ;
   val = k / (double)(rows*cols) ; return val ;
}

/*--------------------------------------------------------------------------*/

sparmat * matrix_to_sparmat( matrix a )
{
   int rows=a.rows , cols=a.cols ;
   int i , j , k ;
   sparmat *sa ;

   sa = (sparmat *)remla_malloc(sizeof(sparmat)) ;

   sa->rows = rows ; sa->cols = cols ;
   sa->cnum = (int *)   remla_calloc(sizeof(int)    ,cols) ;
   sa->cii  = (int **)  remla_calloc(sizeof(int *)  ,cols) ;
   sa->cee  = (double **)remla_calloc(sizeof(double *),cols) ;

   for( j=0 ; j < cols ; j++ ){
     sa->cii[j] = (int *)  remla_malloc(sizeof(int)  *rows) ;
     sa->cee[j] = (double *)remla_malloc(sizeof(double)*rows) ;
     for( k=i=0 ; i < rows ; i++ ){
       if( a.elts[i][j] != 0.0 ){
         sa->cii[j][k] = i; sa->cee[j][k] = a.elts[i][j]; k++;
       }
     }
     sa->cnum[j] = k ;

     if( k < (int)(0.7*rows+1) ){  /* column is sparse: truncate arrays */
       sa->cii[j] = (int *)  realloc( (void *)sa->cii[j] , sizeof(int)  *k ) ;
       sa->cee[j] = (double *)realloc( (void *)sa->cee[j] , sizeof(double)*k ) ;

     } else if( k < rows ){        /* column is nearly full: re-create as full */
       remla_free(sa->cii[j]); sa->cii[j] = NULL; sa->cnum[j] = rows;
       for( i=0 ; i < rows ; i++ ) sa->cee[j][i] = a.elts[i][j] ;

     } else {                      /* column is completely full */
       remla_free(sa->cii[j]); sa->cii[j] = NULL;
     }
   }
   return sa ;
}

/*--------------------------------------------------------------------------*/

void sparmat_destroy( sparmat *sa )
{
   int i , cols ;
   if( sa == NULL ) return ;
   cols = sa->cols ;
   for( i=0 ; i < cols ; i++ ){
     if( sa->cii[i] != NULL ) remla_free(sa->cii[i]) ;
     if( sa->cee[i] != NULL ) remla_free(sa->cee[i]) ;
   }
   remla_free(sa->cee) ; remla_free(sa->cii) ; remla_free(sa->cnum) ; remla_free(sa) ; return ;
}

/*--------------------------------------------------------------------------*/
/*============ Functions to save and restore REML stuff to disk ============*/
/*--------------------------------------------------------------------------*/

static void my_fwrite( const void *ptr, size_t size, size_t nitems, FILE *fp )
{
   size_t jj ;

   jj = fwrite( ptr , size , nitems , fp ) ;
   if( jj < nitems )
     ERROR_exit("Failure to write -usetemp data! Is disk full? Check files\n"
                "    %s/REML_%s*" ,
                mri_purge_get_tmpdir() , mri_purge_get_tsuf() ) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Write an rcmat struct to a file. */

void rcmat_writebin( FILE *fp , rcmat *rcm )
{
   int ii , jj ;
ENTRY("rcmat_writebin") ;

   if( fp == NULL || rcm == NULL || rcm->nrc < 1 ) EXRETURN ;

   my_fwrite( &(rcm->nrc) , sizeof(int) , 1 , fp ) ;
   my_fwrite( rcm->len    , sizeof(LENTYP) , rcm->nrc , fp ) ;
   for( ii=0 ; ii < rcm->nrc ; ii++ )
     my_fwrite( rcm->rc[ii] , sizeof(double) , rcm->len[ii] , fp ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Read an rcmat struct from a file. */

rcmat * rcmat_readbin( FILE *fp )
{
   rcmat *qcm ; int ii,nn=-666 ;
ENTRY("rcmat_readbin") ;

   if( fp == NULL ) RETURN(NULL) ;

   fread( &nn , sizeof(int) , 1 , fp ) ;
   if( nn < 1 || nn > 999999 ) RETURN(NULL) ;
   qcm = rcmat_init(nn) ;
   fread( qcm->len , sizeof(LENTYP) , nn , fp ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = remla_malloc( sizeof(double)*qcm->len[ii] ) ;
     fread( qcm->rc[ii] , sizeof(double) , qcm->len[ii] , fp ) ;
   }

   RETURN(qcm) ;
}

/*--------------------------------------------------------------------------*/
/*! Write a matrix to a file. */

void matrix_writebin( FILE *fp , matrix *a )
{
   int ii ;
ENTRY("matrix_writebin") ;

   if( fp == NULL || a == NULL || a->rows < 1 || a->cols < 1 ) EXRETURN ;

   my_fwrite( &(a->rows) , sizeof(int) , 1 , fp ) ;
   my_fwrite( &(a->cols) , sizeof(int) , 1 , fp ) ;

   for( ii=0 ; ii < a->rows ; ii++ )
     my_fwrite( a->elts[ii] , sizeof(double) , a->cols , fp ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Read a matrix from a file. */

matrix * matrix_readbin( FILE *fp )
{
   int rows=-666,cols=-666,ii ; matrix *a ;

ENTRY("matrix_readbin") ;

   if( fp == NULL ) RETURN(NULL) ;

   fread( &rows , sizeof(int) , 1 , fp ) ; if( rows < 1 || rows > 999999 ) RETURN(NULL);
   fread( &cols , sizeof(int) , 1 , fp ) ; if( cols < 1 || cols > 999999 ) RETURN(NULL);

   a = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(a) ;
   matrix_create( rows , cols , a ) ;
   for( ii=0 ; ii < rows ; ii++ )
     fread( a->elts[ii] , sizeof(double) , cols , fp ) ;

   RETURN(a) ;
}

/*--------------------------------------------------------------------------*/
/*! Save the matrices in a reml_setup struct to a file (and erase them). */

void reml_setup_savemat( FILE *fp , reml_setup *rs )
{
ENTRY("reml_setup_savemat") ;
   if( fp     == NULL || rs     == NULL ) EXRETURN ;
   if( rs->cc == NULL || rs->dd == NULL ) EXRETURN ;

   rcmat_writebin (fp,rs->cc); rcmat_destroy (rs->cc);                     rs->cc = NULL;
   matrix_writebin(fp,rs->dd); matrix_destroy(rs->dd); remla_free(rs->dd); rs->dd = NULL;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Restore the matrices in a reml_setup struct from a file. */

void reml_setup_restoremat( FILE *fp , reml_setup *rs )
{
ENTRY("reml_setup_restoremat") ;
   if( fp     == NULL || rs     == NULL ) EXRETURN ;
   if( rs->cc != NULL || rs->dd != NULL ) EXRETURN ;

   rs->cc = rcmat_readbin(fp) ; rs->dd = matrix_readbin(fp) ;
   EXRETURN ;
}

/****************************************************************************/

/*--------------------------------------------------------------------------*/
/* Funcs for user to set allow ARMA(1,1) parameter ranges */

static int allow_negative_cor         = 0 ;
static int allow_only_pos_white_noise = 0 ;

void REML_allow_negative_correlations( int i ){
  allow_negative_cor = i ;
  if( i ) allow_only_pos_white_noise = 0 ;
}

void REML_allow_only_pos_white_noise( int i ){
   allow_only_pos_white_noise = i ;
   if( i ) allow_negative_cor = 0 ;
}

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct):
      [ 1 lam lam*rho lam*rho^2 lam*rho^3 ... ]
    which is the ARMA(1,1) model with the AR parameter a = rho,
    and the MA parameter b such that (b+a)*(1+a*b)/(1+2*a*b+b*b) = lam.
    * For reasonable models of FMRI noise, 0 < lam < rho < 0.9.
    * The maximum bandwidth of the matrix is chosen so that the last
      correlation element is about 0.01.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma11( int nt, int *tau, double rho, double lam )
{
   rcmat  *rcm ;
   LENTYP *len ;
   double **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 ){
     if( verb ) ERROR_message("rcmat_arma11: nt=%d < 2",nt) ;
     return NULL ;
   }

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.9 ) rho =  0.9 ;  /* max allowed NN correlation */
   else if( rho < -0.9 ) rho = -0.9 ;

   /* set maximum bandwidth */

   alam = fabs(lam) ;
   if( alam >= corcut ){
     if( rho != 0.0 ) /* bmax is such that last element is about 'corcut' */
       bmax = 1 + (int)ceil( log(corcut/alam) / log(fabs(rho)) ) ;
     else
       bmax = 1 ;     /* pure MA(1) case */
   } else {
     bmax = 0 ;       /* identity matrix case */
   }

   /* special and trivial case: identity matrix */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(double)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = remla_malloc(sizeof(double)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwidth */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){       /* a purely diagonal row/colum (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(double)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = remla_calloc(sizeof(double),len[ii]); /* space for the entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
            if( jtt == 1 ) rii[jj] = lam ;               /* lag==1 means lam */
       else if( jtt >  1 ) rii[jj] = lam * pow( rho , jtt-1.0 ) ;
     }
   }

#if 0
{ if( rho > 0.6 && lam/rho > 0.8 ){
    INFO_message("profile for rho=%g lam=%g",rho,lam) ;
    for( ii=0 ; ii < nt ; ii++ ) fprintf(stderr," %d",len[ii]) ;
    fprintf(stderr,"\n") ;
  }
}
#endif

   return rcm ;
}

/*--------------------------------------------------------------------------*/
/*! Create the struct for REML calculations for a particular ARMA(1,1)
    set of parameters, and for a particular regression matrix X.       */

reml_setup * setup_arma11_reml( int nt, int *tau,
                                double rho, double lam , matrix *X )
{
   int ii , jj , mm ;
   reml_setup *rset ;
   rcmat *rcm ;
   matrix *W , *D ;
   double *vec , csum,dsum,val ;

   if( nt < 2 || X == NULL || X->rows != nt ){
     if( verb ) ERROR_message("setup_arma11_reml: bad inputs?!") ;
     return NULL ;
   }

   /* check if correlation matrix parameters are allowable [02 Jan 2020] */

   if( ! allow_negative_cor && ( rho < 0.0 || lam < 0.0 ) ) return NULL ;

   if( allow_only_pos_white_noise &&
       ( rho < 0.0 || lam < 0.0 || lam > rho ) ) return NULL ;

   mm = X->cols ;  /* number of regression parameters */
   if( mm >= nt || mm <= 0 ){
     if( verb ) ERROR_message("setup_arma11_reml: bad inputs?!") ;
     return NULL ;
   }

   /* Form R = correlation matrix for ARMA(1,1) model */

   rcm = rcmat_arma11( nt , tau , rho , lam ) ;
   if( rcm == NULL ){
     if( verb ) ERROR_message("rcmat_arma11 fails!?") ;
     return NULL ;
   }

   /* form C = Choleski factor of R (in place) */

   ii = rcmat_choleski( rcm ) ;
   if( ii != 0 ){
     if( verb )
       ERROR_message("rcmat_choleski fails with code=%d: rho=%f lam=%f",ii,rho,lam) ;
     rcmat_destroy(rcm); return NULL;
   }

   /* prewhiten each column of X into W matrix */

   W = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(W) ;
   matrix_create(nt,mm,W) ;
   vec = (double *)remla_malloc(sizeof(double)*nt) ;
   for( jj=0 ; jj < X->cols ; jj++ ){
     for( ii=0 ; ii < nt ; ii++ ) vec[ii] = X->elts[ii][jj] ; /* extract col */
     rcmat_lowert_solve( rcm , vec ) ;                          /* prewhiten */
     for( ii=0 ; ii < nt ; ii++ ) W->elts[ii][jj] = vec[ii] ;  /* put into W */
   }
   remla_free((void *)vec) ;

   /* compute QR decomposition of W, save R factor into D, toss W */

   D = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(D) ;
   ii = matrix_qrr( *W , D ) ;
   matrix_destroy(W) ; remla_free((void *)W) ;
   if( D->rows <= 0 ){
     if( verb )
       ERROR_message("matrix_qrr fails?! a=%.3f lam=%.3f",rho,lam) ;
     matrix_destroy(D) ; remla_free((void *)D) ; rcmat_destroy(rcm) ; return NULL ;
   } else if( ii > 0 ){
#pragma omp critical (QRERR)
 {  static int iold=0 ;
     if( ii > iold ){
       WARNING_message("-----") ;
       WARNING_message(
         "QR decomposition of [R]^(-1/2) [X] had %d collinearity problem%s" ,
         ii , (ii==1) ? "\0" : "s"                       ) ;
       WARNING_message("-----") ;
       iold = ii ;
     }
 } /* end of OpenMP critical section */
   }

   /* create the setup struct, save stuff into it */

   rset = (reml_setup *)remla_malloc(sizeof(reml_setup)) ;
   rset->neq  = nt ;
   rset->mreg = mm ;
   rset->rho  = rho ;
   rset->lam  = lam ;
   rset->cc   = rcm ;
   rset->dd   = D ;

   rset->nglt = 0 ;
   rset->glt  = NULL ;

   /* compute 2 * log det[D] */

   for( dsum=0.0,ii=0 ; ii < mm ; ii++ ){
     val = D->elts[ii][ii] ;
     if( val > 0.0 ) dsum += log(val) ;
   }
   rset->dd_logdet = 2.0 * dsum ;

   /* and 2 * log det[C] */

   for( csum=0.0,ii=0 ; ii < nt ; ii++ ){
     jj  = rcm->len[ii] ;
     val = rcm->rc[ii][jj-1] ;
     if( val > 0.0 ) csum += log(val) ;
   }
   rset->cc_logdet = 2.0 * csum ;

   return rset ;
}

/*--------------------------------------------------------------------------*/

reml_setup * REML_setup_one( matrix *X , int *tau , double rho , double bb )
{
   reml_setup *rset ;
   double lam = LAMBDA(rho,bb) ;

   rset = setup_arma11_reml( X->rows , tau , rho , lam , X ) ;
   if( rset != NULL ) rset->barm = bb ;
   return rset ;
}

/*--------------------------------------------------------------------------*/
/* Similar to above, BUT
     * first add columns in Z to matrix
     * then carry out the REML setup for this new matrix
   This is to allow for voxel-wise regressors in 3dREMLfit.    [22 Jul 2015]
*//*------------------------------------------------------------------------*/

reml_setup_plus * REML_setup_plus( matrix *X, matrix *Z, int *tau, double rho, double bb )
{
   reml_setup_plus *rsetplus=NULL ;
   matrix *XZ=NULL ;
   double spcut ;

ENTRY("REML_setup_plus") ;

   if( X == NULL ) RETURN(rsetplus) ; /* bad */

   /* create the catenated matrix XZ */

   XZ = (matrix *)remla_malloc(sizeof(matrix)) ;
   if( Z == NULL || Z->cols == 0 ){
     matrix_initialize(XZ) ;
     matrix_equate(*X,XZ) ;
   } else {
     int ii,jj , nr,ncX,ncZ ; double *Xar, *Zar, *XZar ;
     nr = X->rows ; ncX = X->cols ; ncZ = Z->cols ;
     if( nr != Z->rows ) RETURN(rsetplus) ;    /* matrices don't conform?! */
     matrix_initialize(XZ) ;
     matrix_create( nr, ncX+ncZ , XZ ) ;
     for( ii=0 ; ii < nr ; ii++ ){                       /* loop over rows */
       Xar = X->elts[ii] ; Zar = Z->elts[ii] ; XZar = XZ->elts[ii] ;
       for( jj=0 ; jj < ncX ; jj++ ) XZar[jj]     = Xar[jj] ; /* over cols */
       for( jj=0 ; jj < ncZ ; jj++ ) XZar[jj+ncX] = Zar[jj] ;
     }
   }

   rsetplus = (reml_setup_plus *)remla_calloc(1,sizeof(reml_setup_plus)) ;

   /* setup the REML calculations for this fun fun fun matrix */

   rsetplus->rset = REML_setup_one( XZ , tau , rho , bb ) ;
   if( rsetplus->rset == NULL ){        /* bad bad bad */
     remla_free(rsetplus) ; matrix_destroy(XZ) ; remla_free(XZ) ; RETURN(NULL) ;
   }

   rsetplus->X = XZ ;  /* store matrix, then maybe its sparsification */

   spcut = AFNI_numenv("AFNI_REML_SPARSITY_THRESHOLD") ;
   if( spcut <= 0.0 ) spcut = 1.01 ;  /* always use sparmat */
   if( sparsity_fraction(*XZ) <= spcut ) rsetplus->Xs = matrix_to_sparmat(*XZ) ;
   else                                  rsetplus->Xs = NULL ;

   RETURN(rsetplus) ;
}

/*==========================================================================*/

/*--------------------------------------------------------------------------*/
/* Variable do_logqsumq controls how the REML function uses the residuals.
   * For the case where the matrices are CORRELATION matrices with
      unit diagonals, and the variance is estimated from the residuals,
      this variable should be 1. This case is for the ARMA estimation of
      (residual) time series correlation structure.
   * For the case (not yet implemented) where the matrices are COVARIANCE
      matrices, so that the variances are embedded in the matrices, then
      this variable should be 0. This case is for the linear mixed model
      stuff (to come), where there are different variance components added
      together in the covariance matrix.
*//*------------------------------------------------------------------------*/

static int do_logqsumq = 1 ; /* 28 Apr 2020 */

/*--------------------------------------------------------------------------*/
/*! Compute the REML -log(likelihood) function for a particular case,
    given the case's setup and the data and the regression matrix X.

    [24 Jun 2009] Modified to use workspace vectors passed in via bbar[],
                  rather than static or malloc-ed vectors, for OpenMP's sake.
*//*------------------------------------------------------------------------*/

double REML_func( vector *y , reml_setup *rset , matrix *X , sparmat *Xs ,
                 double *bbar[7] , double *bbsumq )
{
   int n , ii ;
   double val ;
   double *bb1 , *bb2 , *bb3 , *bb4 , *bb5 , *bb6 , *bb7 ;
   double qsumq ;

ENTRY("REML_func") ;

   if( y == NULL || rset == NULL || X == NULL || bbar[0] == NULL ) RETURN(BIGVAL) ;

   /* assign pointers to workspace vectors */

   bb1 = bbar[0] ; bb2 = bbar[1] ; bb3 = bbar[2] ;
   bb4 = bbar[3] ; bb5 = bbar[4] ; bb6 = bbar[5] ; bb7 = bbar[6] ;

   n = rset->neq ;

   /** Seven matrix-vector steps to compute the prewhitened residuals **/

   for( ii=0 ; ii < n ; ii++ ) bb1[ii] = y->elts[ii] ;
   rcmat_lowert_solve( rset->cc , bb1 ) ;            /* bb1 = C^(-T) y */
                                                     /* prewhitened data */
   for( ii=0 ; ii < n ; ii++ ) bb2[ii] = bb1[ii] ;
   rcmat_uppert_solve( rset->cc , bb2 ) ;            /* bb2 = C^(-1) bb1 */

   if( Xs != NULL ){
     vector_spc_multiply_transpose( Xs , bb2, bb3 );
   } else {
     vector_full_multiply_transpose( X , bb2, bb3 ); /* bb3 = X^T bb2 */
   }

   vector_full_rrtran_solve( rset->dd , bb3 , bb4 ); /* bb4 = D^(-T) bb3 */

   vector_full_rr_solve(     rset->dd , bb4 , bb5 ); /* bb5 = D^(-1) bb4 */
                                                     /*     = beta_hat  */
   if( Xs != NULL ){
     vector_spc_multiply( Xs , bb5 , bb6 ) ;
   } else {
     vector_full_multiply( X , bb5 , bb6 ) ;         /* bb6 = X bb5 */
   }                                                 /* fitted model */

   for( ii=0 ; ii < n ; ii++ ) bb7[ii] = bb6[ii] ;
   rcmat_lowert_solve( rset->cc , bb7 ) ;            /* bb7 = C^(-T) bb6 */
                                                     /* prewhitened fit */
   qsumq = 0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
     bb2[ii] = bb1[ii] - bb7[ii] ;                   /* result = bb1 - bb7 */
     qsumq += bb2[ii]*bb2[ii] ;                      /* =prewhitened residual */
   }                                                 /* qsumq = sum of sqrs */

   if( bbsumq != NULL ) *bbsumq = qsumq ;   /* output sum of residual squares */

   if( do_logqsumq ){
     if( qsumq > 0.0 )
       val = (n - rset->mreg) * log(qsumq)             /* the REML function! */
            + rset->dd_logdet + rset->cc_logdet ;      /* -log(likelihood) */
     else
       val = 0.0 ;                                     /* should not happen */
   } else {
     val = qsumq + rset->dd_logdet + rset->cc_logdet ; /* 28 Apr 2020 */
   }

   RETURN(val) ;
}

/*==========================================================================*/
/*--------------------------------------------------------------------------*/

void gltfactors_destroy( gltfactors *gf )
{
   if( gf == NULL ) return ;
   if( gf->Jright != NULL ){ matrix_destroy(gf->Jright); remla_free(gf->Jright); }
   if( gf->Jleft  != NULL ){ matrix_destroy(gf->Jleft ); remla_free(gf->Jleft ); }
   if( gf->sig    != NULL ){ vector_destroy(gf->sig   ); remla_free(gf->sig   ); }
   remla_free(gf) ;
}

/*--------------------------------------------------------------------------*/

void reml_setup_destroy( reml_setup *rset )
{
   int ii ;

   if( rset == NULL ) return ;
   if( rset->cc != NULL ) rcmat_destroy( rset->cc ) ;
   if( rset->dd != NULL ){ matrix_destroy( rset->dd ); remla_free(rset->dd); }
   for( ii=0 ; ii < rset->nglt ; ii++ ) gltfactors_destroy( rset->glt[ii] ) ;
   if( rset->glt != NULL ) remla_free(rset->glt) ;
   remla_free((void *)rset) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void reml_setup_plus_destroy( reml_setup_plus *rsp )  /* 22 Jul 2015 */
{
   if( rsp == NULL ) return ;
   reml_setup_destroy( rsp->rset ) ;
   if( rsp->X  != NULL ){ matrix_destroy(rsp->X) ; remla_free(rsp->X) ; }
   if( rsp->Xs != NULL ) sparmat_destroy(rsp->Xs) ;
   remla_free(rsp) ; return ;
}

/*----------------------------------------------------------------------------*/
/* Functions to save a list of REML_* files, so that they can
   be deleted at program rundown, if they weren't deleted already.
   If mainENTRY() was used, then even a program crash might do cleanup, since
   the most common fatal signals are caught and will end up invoking exit(),
   which will in turn invoke remla_atexit().
------------------------------------------------------------------------------*/

static int atexit_called = 0 ;   /* was atexit() already called for this? */

static int    npurge = 0 ;       /* number of filenames in qpurge array */
static char **qpurge = NULL ;    /* filenames of REML_* files still alive */

static void remla_atexit(void) /*--- called by exit(): delete REML_* files ---*/
{
   int ii , nn ;
   char *tmpdir=mri_purge_get_tmpdir() , *tsuf=mri_purge_get_tsuf() ;

   for( nn=ii=0 ; ii < npurge ; ii++ ){
     if( qpurge[ii] != NULL ){
       ININFO_message("removing temporary REML file %s",qpurge[ii]) ;
       remove(qpurge[ii]) ; nn++ ;
     }
   }
   if( tmpdir != NULL && nn > 0 && tsuf[0] != '\0' )
     ININFO_message("-usetemp: Check for other %s/REML_%s* files",tmpdir,tsuf);
   return ;
}

static void add_purge( char *fn ) /*-------- add fn to the qpurge list ----*/
{
   int ii ;
   if( fn == NULL || *fn == '\0' ) return ;
   for( ii=0 ; ii < npurge ; ii++ )  /* see if already in list */
     if( qpurge[ii] != NULL && strcmp(qpurge[ii],fn) == 0 ) break ;
   if( ii < npurge ) return ;        /* already in list! */
   for( ii=0 ; ii < npurge ; ii++ )  /* find an empty slot */
     if( qpurge[ii] == NULL ) break ;
   if( ii == npurge )                /* make new empty slot */
     qpurge = (char **)realloc(qpurge,sizeof(char *)*(++npurge)) ;
   qpurge[ii] = strdup(fn) ;         /* fill empty slot */

   if( !atexit_called ){ atexit(remla_atexit); atexit_called = 1; }
   return ;
}

static void kill_purge( char *fn ) /*---- remove fn from the qpurge list ----*/
{
   int ii ;
   if( fn == NULL || *fn == '\0' || qpurge == NULL ) return ;
   for( ii=0 ; ii < npurge ; ii++ )  /* find in list */
     if( qpurge[ii] != NULL && strcmp(qpurge[ii],fn) == 0 ) break ;
   if( ii < npurge ){ remla_free(qpurge[ii]) ; qpurge[ii] = NULL ; }
   return ;
}

/*--------------------------------------------------------------------------*/

void reml_setup_savfilnam( char **fnam )
{
   char *pg , *un , *ts ;

   pg = mri_purge_get_tmpdir() ; ts = mri_purge_get_tsuf() ;
   un = UNIQ_idcode() ;
   un[0] = 'R' ; un[1] = 'E'   ; un[2] = 'M'   ; un[3] = 'L' ;
   un[4] = '_' ; un[5] = ts[0] ; un[6] = ts[1] ; un[7] = ts[2] ;
   *fnam = remla_malloc(strlen(pg)+64) ;
   strcpy(*fnam,pg); strcat(*fnam,"/"); strcat(*fnam,un);
   remla_free(un) ; return ;
}

/*--------------------------------------------------------------------------*/
/*! Save a reml_collection struct to disk. */

void reml_collection_save( reml_collection *rcol )
{
   int ii ;
   char *pg , *un , *ts ;
   FILE *fp ;

ENTRY("reml_collection_save") ;

   if( rcol == NULL || rcol->rs[rcol->izero] == NULL ) EXRETURN ;
   if( rcol->savfil != NULL ){
     kill_purge(rcol->savfil) ;
     remove(rcol->savfil) ; remla_free(rcol->savfil) ; rcol->savfil = NULL ;
   }

   reml_setup_savfilnam( &(rcol->savfil) ) ;

   fp = fopen( rcol->savfil , "wb" ) ;
   if( fp == NULL ){
     ERROR_message("3dREMLfit can't write to -usetemp file %s",rcol->savfil) ;
     remla_free(rcol->savfil) ; rcol->savfil = NULL ;
     EXRETURN ;
   }
   add_purge( rcol->savfil ) ;  /* add to list of files to be purged at exit */

   for( ii=0 ; ii < rcol->nab ; ii++ )
     if( rcol->rs[ii] != NULL ) reml_setup_savemat( fp , rcol->rs[ii] ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void reml_collection_restore( reml_collection *rcol )
{
   int ii ; FILE *fp ;

ENTRY("reml_collection_restore") ;

   if( rcol == NULL || rcol->savfil == NULL ) EXRETURN ;

   fp = fopen( rcol->savfil , "rb" ) ;
   if( fp == NULL ){
     ERROR_message("3dREMLfit can't read from -usetemp file %s",rcol->savfil) ;
     remla_free(rcol->savfil) ; rcol->savfil = NULL ;
     EXRETURN ;
   }

   for( ii=0 ; ii < rcol->nab ; ii++ )
     if( rcol->rs[ii] != NULL ) reml_setup_restoremat( fp , rcol->rs[ii] ) ;

   fclose(fp) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Destroy a reml_collection struct:
     - if zsave!=0, then it just
       destroys all but the izero component and leaves that behind;
     - if zsave==0, then it destroys and frees everything
*//*------------------------------------------------------------------------*/

void reml_collection_destroy( reml_collection *rcol , int zsave )
{
   int ii ;

   if( rcol == NULL ) return ;
   if( !zsave && rcol->X  != NULL ) matrix_destroy ( rcol->X  ) ;
   if( !zsave && rcol->Xs != NULL ) sparmat_destroy( rcol->Xs ) ;
   if( rcol->rs != NULL ){
     for( ii=0 ; ii < rcol->nab ; ii++ ){
       if( !(ii == rcol->izero && zsave) ){
         reml_setup_destroy(rcol->rs[ii]) ; rcol->rs[ii] = NULL ;
       }
     }
     if( !zsave ) remla_free((void *)rcol->rs) ;
   }
   if( rcol->savfil != NULL ){
     kill_purge(rcol->savfil) ;
     remove(rcol->savfil) ; remla_free(rcol->savfil) ; rcol->savfil = NULL ;
   }
   if( !zsave ) remla_free((void *)rcol) ;
   return ;
}

/*--------------------------------------------------------------------------*/

reml_collection * REML_setup_all( matrix *X , int *tau ,
                                  int nlev , double atop , double btop )
{
   int ii,jj,kk , nt , nset , pna,pnb , na,nb ;
   double da,db, bb,aa, lam , bbot,abot ;
   reml_collection *rrcol=NULL ;
   float avglen=0.0f ;
   double spcut ;

   if( X == NULL ){  /* super stoopid */
     ERROR_message("REML_setup_all: input matrix is NULL") ;
     return rrcol ;
   }

   nt = X->rows ;
   if( nt < 9 ){     /* medium stoopid */
     ERROR_message(
       "REML_setup_all: number of time points %d is less than 9 -- not allowed!",nt) ;
     return rrcol ;
   }

   /* set grid in a and b parameters */

   if( nlev > 0 ){
     if( nlev < 3 ) nlev = 3 ; else if( nlev > 7 ) nlev = 7 ;
     if( btop <= 0.0 ) btop = 0.08 ; else if( btop > 0.9 ) btop = 0.9 ;
     nb = 1 << (nlev+1) ; pnb = nlev+1 ; bbot = -btop ; db = 2.0*btop / nb ;

     if( atop <= 0.0 ) atop = 0.08 ; else if( atop > 0.9 ) atop = 0.9 ;
     if( allow_negative_cor ){
       na = 1 << (nlev+1) ; pna = nlev+1 ; abot = -atop ;
     } else {
       na = 1 << (nlev)   ; pna = nlev   ; abot = 0.0   ;
     }
     da = (atop-abot) / na ;
   } else {                        /* special case of just 2 (a,b) pairs */
     da = db = 0.0 ; na = nb = pna = pnb = 0 ;
     abot = atop ; bbot = btop ;
   }

   rrcol = (reml_collection *)remla_malloc(sizeof(reml_collection)) ;

   rrcol->X = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(rrcol->X) ;
   matrix_equate( *X , rrcol->X ) ;

   spcut = AFNI_numenv("AFNI_REML_SPARSITY_THRESHOLD") ;
   if( spcut <= 0.0 ) spcut = 1.01 ;  /* always use sparmat [29 Jun 2009] */
   lam = sparsity_fraction( *X ) ;
   if( lam <= spcut ) rrcol->Xs = matrix_to_sparmat( *X ) ;
   else               rrcol->Xs = NULL ;
#if 1
   if( verb > 1 ){
     static int first=1 ;
     if( first ) ININFO_message("X matrix: %.3f%% of elements are nonzero" ,
                                100.0*lam ) ;
     first = 0 ;
   }
#endif

   if( nlev > 0 ){
     rrcol->nab   = (nb+1)*(na+1) ;  /* number of cases to create */
     rrcol->istwo = 0 ;
   } else {
     rrcol->nab   = 2 ;
     rrcol->istwo = 1 ;
   }

   /* allocate REML matrix setups:
       all are NULL to start, and if a matrix setup (rs) element
       is never computed, then it will be skipped later in the
       optimizing function REML_find_best_case */

   rrcol->rs = (reml_setup **)remla_calloc(sizeof(reml_setup *),rrcol->nab) ;

   rrcol->abot = abot ; rrcol->da = da ; rrcol->na = na ; rrcol->pna = pna ;
   rrcol->bbot = bbot ; rrcol->db = db ; rrcol->nb = nb ; rrcol->pnb = pnb ;

   /** set up the (a,b)=(0,0) case **/

   if( nlev > 0 ){
     rrcol->izero = kk = IAB(rrcol,0.0,0.0) ;  /* index for a=b=0 case */
   } else {
     rrcol->izero = kk = 0 ;
   }
   rrcol->rs[kk] = setup_arma11_reml( nt , tau , 0.0 , 0.0 , X ) ;
   rrcol->rs[kk]->barm = 0.0 ; nset = 1 ;
   avglen = rcmat_avglen( rrcol->rs[kk]->cc ) ;

   if( nlev == 0 ){  /* special setup with just 2 (a,b) cases */

     bb = bbot ; aa = abot ; lam = LAMBDA(aa,bb) ; kk = 1 ;
     rrcol->rs[kk] = setup_arma11_reml( nt,tau , aa,lam , X ) ;
     if( rrcol->rs[kk] != NULL ){
       rrcol->rs[kk]->barm = bb ; nset++ ;
       avglen += rcmat_avglen( rrcol->rs[kk]->cc ) ;
     }

   } else {          /* general setup case with an (a,b) grid of cases */

#ifdef USE_OMP
    int nthr , *nset_th ; float *avg_th ; static int first=1 ;
    nthr    = omp_get_max_threads() ;
    nset_th = (int   *)remla_calloc(sizeof(int)  ,nthr) ;
    avg_th  = (float *)remla_calloc(sizeof(float),nthr) ;
    if( first && verb && nthr > 1 ){
      ININFO_message("starting %d OpenMP threads for REML setup",nthr) ;
      first = 0 ;
    }
#endif

 AFNI_OMP_START ;
#pragma omp parallel if( maxthr > 1 && rrcol->nab > 2 )
 { int iab , nab , ii,jj,kk , ithr=0 ;
   double bb,aa, lam ; float avg ;
   nab = rrcol->nab ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
#ifdef ENABLE_REMLA_MALLOC
   if( ithr == 0 && verb > 1 ) fprintf(stderr,"nab=%d ",nab) ;
#endif
#endif
#pragma omp for
     for( iab=0 ; iab < nab ; iab++ ){ /* loop over (aa,bb) pairs */
       ii  = iab % (na+1) ;
       jj  = iab / (na+1) ;  /* the (na+1) is correct here! */
       aa  = ii*da + abot ;                 /* AR parameter */
       bb  = jj*db + bbot ;                 /* MA parameter */
       lam = LAMBDA(aa,bb) ;                /* +1 super-diagonal element */
       kk  = ii + (1+na)*jj ;
       if( kk != rrcol->izero &&
           ( lam >= corcut || (lam <= -corcut && allow_negative_cor) ) ){
         rrcol->rs[kk] = setup_arma11_reml( nt,tau, aa,lam , X ) ;
         if( rrcol->rs[kk] != NULL ){
           rrcol->rs[kk]->barm = bb ;
           avg = rcmat_avglen( rrcol->rs[kk]->cc ) ;
#ifdef USE_OMP
           nset_th[ithr]++ ; avg_th[ithr] += avg ;
#ifdef ENABLE_REMLA_MALLOC
           if( ithr == 0 && verb > 1 ){
             fprintf(stderr," [%d]",kk) ;
             if( kk%20 == 0 ) REMLA_memprint ;
           }
#endif
#else
           nset++ ; avglen += avg ;
#endif
         }
       }
     } /* end loop over (aa,bb) pairs */

 } /* end OpenMP */
 AFNI_OMP_END ;

#ifdef USE_OMP
   for( ii=0 ; ii < nthr ; ii++ ){
     nset += nset_th[ii] ; avglen += avg_th[ii] ;
   }
   remla_free(avg_th) ; remla_free(nset_th) ;
#endif

   } /* end general setup */

   /* fix a couple of details and vamoose */

   rrcol->nset = nset ; rrcol->savfil = NULL ; rrcol->avglen = avglen/nset ;
   return rrcol ;
}

/*--------------------------------------------------------------------------*/
/* Inputs: y=data vector, rrcol=collection of REML setup stuff.
   Output: index of best case in the REML seteup stuff.
*//*------------------------------------------------------------------------*/

int REML_find_best_case( vector *y , reml_collection *rrcol ,
                         int nws , double *ws )
{
   double rbest , rval ;
   int   na,nb , pna,pnb , ltop , mm ;
   int   nab, lev, dab, ia,jb,kk, ibot,itop, jbot,jtop, ibest,jbest,kbest ;
   int   klist[128] , nkl , needed_ws,bb_ws=0,rv_ws=0 ;
   double *bbar[7] , *rvab ;

ENTRY("REML_find_best_case") ;

   if( y == NULL ) RETURN(-666) ;

   /* copy (a,b) grid parameters to local variables */

   na = rrcol->na ; pna = rrcol->pna ;
   nb = rrcol->nb ; pnb = rrcol->pnb ;

   /* make workspace arrays for REML_func() [24 Jun 2009] */

   bb_ws     = 2*y->dim      + 16 ;
   rv_ws     = (na+1)*(nb+1) + 16 ;
   needed_ws = 7*bb_ws + rv_ws ;
   if( nws >= needed_ws && ws != NULL ){
     rvab    = ws+1 ;
     bbar[0] = rvab + rv_ws ;
     for( ia=1 ; ia < 7 ; ia++ ) bbar[ia] = bbar[ia-1] + bb_ws ;
     bb_ws = rv_ws = 0 ;
   } else {
     rvab = (double *)remla_malloc(sizeof(double)*rv_ws) ;
     for( ia=0 ; ia < 7 ; ia++ )
       bbar[ia] = (double *)remla_malloc(sizeof(double)*bb_ws) ;
   }

   /** do the Ordinary Least Squares (olsq) case, mark it as best so far **/

   kbest = rrcol->izero ;
   jbest = kbest / (1+na) ;
   ibest = kbest % (1+na) ;
   rbest = REML_func( y, rrcol->rs[kbest], rrcol->X,rrcol->Xs, bbar,NULL ) ;

   /** do power-of-2 descent through the (a,b) grid to find the best pair **/

   for( kk=0 ; kk < (na+1)*(nb+1) ; kk++ ) rvab[kk] = BIGVAL ;
   rvab[kbest] = rbest ;

   nab  = MIN(pna,pnb)  ; ltop = nab-2 ;
   ibot = 0 ; itop = na ;                 /* scan from ibot..itop, jbot..jtop */
   jbot = 0 ; jtop = nb ;
   for( lev=ltop ; lev >= 0 ; lev-- ){     /* lev = power-of-2 grid scan size */
     dab = 1 << lev ;                      /* dab = 2^lev = step size in scan */

     /* make list of grid points to visit in the REML_func loop below */

     for( nkl=0,jb=jbot ; jb <= jtop ; jb+=dab ){
       for( ia=ibot ; ia <= itop ; ia+=dab ){
         kk = ia + jb*(na+1) ; if( rvab[kk] < BIGVAL ) continue;    /* did it */
         if( rrcol->rs[kk] == NULL ) continue ;         /* invalid grid point */
         klist[nkl++] = kk ;
     }}
     if( nkl == 0 ) continue ; /* should never happen */

     /* The reason the loop above is separate is to make the loop below
        be 1D rather than 2D, hoping that OpenMP would parallelize it better.
        However, this didn't work out, but the code is left the way it is */

     for( mm=0 ; mm < nkl ; mm++ ){  /* this usually takes a lot of CPU time */
       kk = klist[mm] ;
       rvab[kk] = REML_func( y, rrcol->rs[kk], rrcol->X,rrcol->Xs , bbar,NULL ) ;
     }

     /* find the best one so far seen */

     for( jb=jbot ; jb <= jtop ; jb+=dab ){
       for( ia=ibot ; ia <= itop ; ia+=dab ){
         kk = ia + jb*(na+1) ; rval = rvab[kk] ;
         if( rval < rbest ){                          /* the best so far seen */
           rbest = rval ; kbest = kk ; ibest = ia ; jbest = jb ;
         }
     }}

     /* at the next level down, scan near the best so far seen */

     if( lev > 0 ){
       ibot = ibest-dab ; if( ibot < 0  ) ibot = 0  ;
       itop = ibest+dab ; if( itop > na ) itop = na ;
       jbot = jbest-dab ; if( jbot < 0  ) jbot = 0  ;
       jtop = jbest+dab ; if( jtop > nb ) jtop = nb ;
     }

   } /* end of scan descent through different levels */

   if( bb_ws ){
     remla_free(rvab) ;
     for( ia=0 ; ia < 7 ; ia++ ) remla_free(bbar[ia]) ;
   }

   if( ws != NULL ) ws[0] = rbest ;

   /*** deal with the winner ***/

   RETURN(kbest) ;
}

/*--------------------------------------------------------------------------*/

gltfactors * REML_get_gltfactors( matrix *D , matrix *G )
{
   int nn , rr , i,j ; double ete ;
   gltfactors *gf ;
   matrix *JL , *JR , *GT , *E,*F,*Z ; vector *S ;

ENTRY("REML_get_gltfactors") ;

   if( D       == NULL    || G       == NULL    ) RETURN( NULL );
   if( D->rows != D->cols || D->rows != G->cols ) RETURN( NULL );

   nn = D->rows ; rr = G->rows ; if( nn < 1 || rr < 1 ) RETURN( NULL );

   /* [D] is nn X nn (upper triangular); [G] is rr X nn, with rr < nn */

   GT = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(GT) ;
   matrix_transpose( *G , GT ) ;         /* GT = [G'] = nn X rr matrix */

/* INFO_message("GLT GT matrix") ; matrix_print( *GT ) ; */

   F = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(F) ;
   matrix_rrtran_solve( *D , *GT , F ) ; /* F = inv[D'] [G'] = nn X rr matrix */
   matrix_destroy(GT); remla_free(GT);

   S = (vector *)remla_malloc(sizeof(vector)) ; vector_initialize(S) ;

/* INFO_message("GLT F matrix") ; matrix_print( *F ) ; */

   if( rr == 1 ){               /* F is really an nn-vector */
     ete = matrix_frobenius( *F ) ;        /* sum of squares */
     vector_create(rr,S) ; S->elts[0] = sqrt(ete) ; /* sigma */
     if( ete == 0.0f ){
#pragma omp critical (GLTZZZ)
 {     static int first = 1 ;
       if( first ){
         WARNING_message( "GLT setup %dx1 matrix is all zero?" , nn ); first=0;
       }
       ete = 1.0 ;
 } /* end of OpenMP critical section */
     }
     ete = 1.0 / ete ;                       /* scale factor */
     JR = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(JR) ;
     matrix_create(rr,nn,JR) ;
     for( i=0 ; i < nn ; i++ )
       JR->elts[0][i] = G->elts[0][i] * ete ;  /* scale G to get JR */

   } else {                     /* QR factor F to get E, then solve for JR */
     E  = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(E) ;
     Z  = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(Z) ;
     JR = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(JR) ;
     i = matrix_qrr( *F , E ) ;
     if( i > 0 ){
#pragma omp critical (QRERR)
 {     static int iold = 0 ;
       if( i > iold ){
         fprintf(stderr,"\n") ;
         WARNING_message(
           "QR decomposition of GLT setup %dx%d matrix had %d collinearity problem%s" ,
           nn,rr, i , (i==1) ? "\0" : "s"                             ) ;
         iold = i ;
       }
 } /* end of OpenMP critical section */
     }
     matrix_rrtran_solve( *E , *G , Z ) ;
     matrix_rr_solve( *E , *Z , JR ) ;  /* JR = inv[E] inv[E'] G = rr X nn */
     matrix_colsqsums( *E , S ) ;       /* S = [sig] = rr vector */
     matrix_destroy(Z); remla_free(Z);
     matrix_destroy(E); remla_free(E);
   }

   /* compute JL */

   JL = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(JL) ;
   matrix_rr_solve( *D , *F , JL ) ;  /* JL = inv[D] [F] = nn X rr matrix */
   matrix_destroy(F); remla_free(F);

   /* save results into struct */

   gf = (gltfactors *)remla_malloc(sizeof(gltfactors)) ;
   gf->mpar   = nn ;  /* number of parameters */
   gf->rglt   = rr ;  /* number of rows in matrix */
   gf->Jright = JR ;
   gf->Jleft  = JL ;
   gf->sig    = S  ;
   RETURN( gf );
}

/*--------------------------------------------------------------------------*/

void REML_add_glt_to_one( reml_setup *rs , matrix *G )
{
   gltfactors *gf=NULL ; int ng ;

ENTRY("REML_add_glt_to_one") ;

   if( rs == NULL || G == NULL ) EXRETURN ;

   if( G->cols == rs->mreg ){       /* case where G is right size */
     gf = REML_get_gltfactors( rs->dd , G ) ;
   } else if( G->cols < rs->mreg ){ /* G needs extra columns (all zero) */
     matrix Gx ;                    /* [22 Jul 2015] */
     matrix_initialize(&Gx) ; matrix_equate(*G,&Gx) ;
     matrix_enlarge( 0 , rs->mreg - G->cols , &Gx ) ;
     gf = REML_get_gltfactors( rs->dd , &Gx ) ;
     matrix_destroy(&Gx) ;
   } else {                         /* this should never happen! */
     ERROR_message("REML_add_glt_to_one: G matrix has too many columns?!") ;
   }
   if( gf == NULL ) EXRETURN ;

   ng = rs->nglt ;
   rs->glt = (gltfactors **)realloc( (void *)rs->glt ,
                                     sizeof(gltfactors *)*(ng+1) ) ;

   rs->glt[ng] = gf ; rs->nglt = ng+1 ; EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void REML_add_glt_to_all( reml_collection *rc , matrix *G )
{
   int ia,jb , na,nb , kk ;

ENTRY("REML_add_glt_to_all") ;

   if( rc == NULL || G == NULL ) EXRETURN ;

   if( rc->istwo ){
     REML_add_glt_to_one( rc->rs[0] , G ) ;
     REML_add_glt_to_one( rc->rs[1] , G ) ;
   } else {
     for( jb=0 ; jb <= rc->nb ; jb++ ){
       for( ia=0 ; ia <= rc->na ; ia++ ){
         kk = ia + (1+rc->na)*jb ;
         REML_add_glt_to_one( rc->rs[kk] , G ) ;
     }}
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* Return value is F statistic. */

static vector *betaG = NULL ;  /* GLT combinations  */
static vector *betaT = NULL ;  /* GLT t-statistics  */
static double   betaR = 0.0  ;  /* GLT R^2 statistic */
static double   betaF = 0.0  ;  /* GLT F statistic   */

double REML_compute_gltstat( int ddof ,
                            vector *y, vector *bfull, double fsumq ,
                            reml_setup *rset, gltfactors *gf,
                            matrix *G, sparmat *Gs, matrix *X, sparmat *Xs )
{
   double fstat , rsumq ;
   vector ba , bb , br ;

ENTRY("REML_compute_gltstat") ;

   if( betaG == NULL ){
     betaG = (vector *)remla_malloc(sizeof(vector)) ; vector_initialize(betaG) ;
     betaT = (vector *)remla_malloc(sizeof(vector)) ; vector_initialize(betaT) ;
   }

   if( y     == NULL || bfull == NULL ||
       rset  == NULL || gf    == NULL || fsumq <= 0.0 ){
     vector_destroy(betaG) ; vector_destroy(betaT) ;
     remla_free(betaG) ; remla_free(betaT) ; betaG = betaT = NULL ; betaR = 0.0 ;
     RETURN( 0.0 );
   }

   if( ddof <= 0 ) ddof = X->rows - X->cols ;  /* 16 Mar 2010 */

   vector_initialize(&ba); vector_initialize(&bb); vector_initialize(&br);

   /* compute br = beta coefficients in subspace with [G][br] = 0
                 = [bfull] - [Jleft] [Jright] [bfull]
                 = projection of beta coefficient in full model */

   vector_multiply( *(gf->Jright) , *bfull , &ba ) ;
   vector_multiply( *(gf->Jleft)  , ba     , &bb ) ;
   vector_subtract( *bfull , bb , &br ) ;

   /* create fitted model [X][br] */

   if( Xs != NULL ){
     vector_create_noinit( Xs->rows , &ba ) ;
     vector_spc_multiply( Xs , br.elts , ba.elts ) ;
   } else {
     vector_multiply( *X , br , &ba ) ;
   }

   /* subtract from data, compute sum of squares of these residuals */

   vector_subtract( *y , ba , &bb ) ;
   rcmat_lowert_solve( rset->cc , bb.elts ) ;
   rsumq = vector_dotself(bb) ;

   vector_destroy(&br); vector_destroy(&bb); vector_destroy(&ba);

   /* F stat measures the improvement in sum of squares of
      residuals between this restricted model and the full model */

   fstat = ( (rsumq-fsumq) / gf->rglt ) / ( fsumq / ddof ) ;
   if( fstat < 0.0 ) fstat = 0.0 ;  /* should not happen */
   betaF = fstat ;

   /* 23 Oct 2008: generalized correlation coefficient squared */

   if( rsumq > fsumq ) betaR = 1.0 - fsumq / rsumq ;
   else                betaR = 0.0 ;

   /* compute GLT combinations of beta coefficients, and t-statistics */

   if( G != NULL ){
     double fsig ; int ii ;
     fsig = sqrt( fsumq / ddof ) ;  /* noise estimate */

     if( G->cols < bfull->dim ){  /* 22 Jul 2015 */
       matrix Gx ;
       matrix_initialize(&Gx) ; matrix_equate(*G,&Gx) ;
       matrix_enlarge( 0 , bfull->dim - G->cols , &Gx ) ;
       vector_multiply( Gx , *bfull , betaG ) ;
       matrix_destroy(&Gx) ;
     } else if( Gs != NULL ){
       vector_create_noinit( Gs->rows , betaG ) ;
       vector_spc_multiply( Gs , bfull->elts , betaG->elts ) ;
     } else {
       vector_multiply( *G , *bfull , betaG ) ;
     }
     vector_create_noinit( betaG->dim , betaT ) ;
     for( ii=0 ; ii < betaG->dim ; ii++ )
       betaT->elts[ii] = betaG->elts[ii] / (gf->sig->elts[ii]*fsig) ;
   }

   RETURN( fstat );
}

/*---------------------------------------------------------------------------*/

#define NSCUT 5  /* finish when get this many 'small' correlations in a row */

/*---------------------------------------------------------------------------*/
/*  Compute correlations for AR(3) model with one real root [aa] and two
    complex roots [rr1 exp(+-I*th1)], with 0 < aa,rr1 < 1 and 0 < th1 < PI/2

    AR(3) model
      Noise process is
        x[n]   = w[n] + p1 * x[n-1] + p2 * x[n-2] + p3 * x[n-3]
                 where w[n] is white noise iid
      Polynomial of noise process in terms of roots is
        phi(z) = (z-aa)*(z-rr1*exp(I*th1))*(z-rr1*exp(-I*th1))
      In terms of coefficients above is
        phi(z) = z^3 - p1 * z^2 - p2 * z - p3
      Auto-correlations g[k] are given by g[0] = 1, linear equations
      for the next 2
        [  (1-p2)  -p3 ] [ g[1] ]   [ p1 ]
        [ -(p1+p2)  1  ] [ g[2] ] = [ p2 ]
      and then recurrence
        g[k] = p1*g[k-1] + p2*g[k-2] + p3*g[k-3] for k > 2

      Finally, sfac is the ratio sig^2/(sig^2+kap^2) where
        sig^2 = variance of AR(3) process
        kap^2 = variance of WN process added into AR(3) process
      The final off-diagonal (k > 0) correlations are adjusted to be
      sfac*g[k] to allow for this. Note that sfac == 0 is just white noise,
      and sfac == 1 is pure AR(3).

      I call this "ARMA(3,1)" but it is a restricted subset of a full
      ARMA(3,1) model, as phi(z) in the ARMA portion is required to have
      1 real and 2 complex (conjugate) roots, and the MA portion is
      restricted to allow only models that correspond to AR(3) plus some
      extra white noise - unlike the ARMA(1,1) models allowed in other
      parts of this hideous hackwork.
*//*-------------------------------------------------------------------------*/

doublevec * REML_compute_arma31_correlations( double aa,
                                              double rr1, double th1,
                                              int kmax  , double sfac )
{
   doublevec *cvec=NULL ;
   double p1 , p2 , p3 , rc1 ;
   double m11,m12,m21,m22 , mdd , gg1,gg2,ggk , ccut ;
   int kk , nsmall ;

ENTRY("REML_compute_arma31_correlations") ;

   if( aa <= 0.0 || rr1 <= 0.0 || rr1 >= 1.0 || th1 <= 0.0 || th1 >= HPI ){
     ERROR_message("REML_compute_arma31_correlations(%g,%g,%g) is bad",aa,rr1,th1);
     RETURN(NULL) ;
   }

   if( sfac <= 0.0 ) kmax = 1 ;    /* adjustments */
   if( sfac >  1.0 ) sfac = 1.0 ;

   if( kmax == 1 ){             /* very trivial case */
     MAKE_doublevec(cvec,1) ;
     cvec->ar[0] = 1.0 ;
     RETURN(cvec) ;
   }

   rc1 = rr1 * cos(th1) ;      /* compute polynomial coefficients */
   p1  = aa + 2.0*rc1 ;        /* from the input polynomial roots */
   p2 = -2.0 * aa * rc1 - rr1*rr1 ;
   p3 = rr1*rr1 + aa ;

   m11 = 1.0 - p2 ;           /* setup 2x2 matrix for first two gammas */
   m12 = -p3 ;
   m21 = -( p3 + p1 ) ;
   m22 = 1.0 ;

   mdd = m11*m22 - m12*m21 ;  /* determinant of matrix */
   if( mdd == 0.0 ){          /* should never happen */
     ERROR_message("REML_compute_arma31_correlations(%g,%g,%g) bad det",aa,rr1,th1);
     RETURN(NULL) ;
   }

   /* first two correlations (gammas) by solving above 2x2 system */

   gg1 = (  m22 * p1 - m12 * p2 ) / mdd ;
   gg2 = ( -m21 * p1 + m11 * p2 ) / mdd ;

   /* create and initialize output vector of correlations */

   if( kmax <= 0 ) kmax = 9999 ;  /* I hope we never need more than this */
   MAKE_doublevec(cvec,kmax+1) ;

   cvec->ar[0] = 1.0 ;
   cvec->ar[1] = gg1 ;
   cvec->ar[2] = gg2 ;

   if( kmax == 2 ){  /* unusually short output! */
     cvec->ar[1] *= sfac ;
     cvec->ar[2] *= sfac ;
     RETURN(cvec) ;
   }

   ccut = MAX(10.0,1.0/sfac) * corcut ;

   /* load further correlations recursively */

   for( nsmall=0,kk=3 ; kk <= kmax ; kk++ ){
     ggk = p1*cvec->ar[kk-1] + p2*cvec->ar[kk-2] + p3*cvec->ar[kk-3] ;
     cvec->ar[kk] = ggk ;

     if( fabs(ggk) >= ccut ){        /* still big enough */
       nsmall = 0 ;
     } else {                        /* too small */
       nsmall++ ;
       if( nsmall >= NSCUT ) break ; /* too many small in a row */
     }
   }
   if( kk == kmax ) kk-- ; /* so kk = last value actually assigned */

   kmax = kk+1-nsmall ;    /* 1 past last 'big' value assigned */

   /* truncate, if we didn't get all the way to the end */

   RESIZE_doublevec(cvec,kmax) ;

   if( sfac < 1.0 ){
     for( kk=1 ; kk < kmax ; kk++ ) cvec->ar[kk] *= sfac ;
   }

   RETURN(cvec) ;
}

/*---------------------------------------------------------------------------*/
/*  Similar to above but for AR(5) model model with one real root [aa] and
    four complex roots [rr1 exp(+-I*th1) , rr2 exp(+-I*th2)],
    with 0 < aa,rr1 < 1 and 0 < th1,th2 < PI/2
*//*-------------------------------------------------------------------------*/

doublevec * REML_compute_arma51_correlations( double aa,
                                              double rr1, double th1,
                                              double rr2, double th2,
                                              int kmax  , double sfac )
{
   doublevec *cvec=NULL ;
   double p1,p2,p3,p4,p5 , rc1,rc2 , ct1,ct2 , rcp , r1q,r2q ;
   dmat44 M44 , M44inv ;
   double gg1,gg2,gg3,gg4,ggk , ccut ;
   int kk , nsmall ;

ENTRY("REML_compute_arma51_correlations") ;

   if( aa <= 0.0 || rr1 <= 0.0 || rr1 >= 1.0 || th1 <= 0.0 || th1 >= HPI ||
                    rr2 <= 0.0 || rr2 >= 1.0 || th2 <= 0.0 || th2 >= HPI   ){
     ERROR_message("REML_compute_arma51_correlations(%g,%g,%g,%g,%g) is bad",aa,rr1,th1,rr2,th2);
     RETURN(NULL) ;
   }

   if( sfac <= 0.0 ) kmax = 1 ;    /* adjustments */
   if( sfac >  1.0 ) sfac = 1.0 ;

   if( kmax == 1 ){             /* very trivial case */
     MAKE_doublevec(cvec,1) ;
     cvec->ar[0] = 1.0 ;
     RETURN(cvec) ;
   }

   ct1 = cos(th1) ;      /* compute polynomial coefficients */
   ct2 = cos(th2) ;      /* formulas from yacas, massaged */
   rc1 = rr1 * ct1 ;
   rc2 = rr2 * ct2 ;
   rcp = rr1 * ct2 + rr2 * ct1 ;
   r1q = rr1 * rr1 ;
   r2q = rr2 * rr2 ;

   p1  = 2.0*rc1 + 2.0*rc2 + aa ;
   p2  = -( 4.0*rc1*rc2 + 2.0*(rc1+rc2)*aa + (r1q+r2q) ) ;
   p3  = aa*( (r1q+r2q) + 4.0*rc1*rc2 ) + 2.0*rr1*rr2*rcp ;
   p4  = -( 2.0*rr1*rr2*aa*rcp + r1q*r2q ) ;
   p5  = aa + r1q + r2q ;

   /* Fill 4x4 matrix (from yacas) */

   LOAD_DMAT44( M44 ,
                  1.0-p2 ,   -p3    , -p4 , -p5  ,
                -(p3+p1) ,  1.0-p4  , -p5 ,  0.0 ,
                -(p4+p2) , -(p5+p1) , 1.0 ,  0.0 ,
                -(p5+p3) ,   -p2    , -p1 ,  1.0  ) ;

   M44inv = generic_dmat44_inverse( M44 ) ;  /* invert matrix */

   /* get first 4 gammas (correlations) by solving [M44] [gg(1-4)] = [p(1-4)] */

   DMAT44_VEC( M44inv , p1,p2,p3,p4 , gg1,gg2,gg3,gg4 ) ;

   /* create and initialize output vector of correlations */

        if( kmax <= 0 ) kmax = 9999 ;  /* I hope we never need more than this */
   else if( kmax <  4 ) kmax = 4 ;
   MAKE_doublevec(cvec,kmax+1) ;

   cvec->ar[0] = 1.0 ;
   cvec->ar[1] = gg1 ;
   cvec->ar[2] = gg2 ;
   cvec->ar[3] = gg3 ;
   cvec->ar[4] = gg4 ;

   if( kmax == 4  ){  /* unusually short output! */
     cvec->ar[1] *= sfac ;
     cvec->ar[2] *= sfac ;
     cvec->ar[3] *= sfac ;
     cvec->ar[4] *= sfac ;
     RETURN(cvec) ;
   }

   ccut = MAX(10.0,1.0/sfac) * corcut ;

   /* load further correlations recursively */

   for( nsmall=0,kk=5 ; kk <= kmax ; kk++ ){
     ggk =   p1*cvec->ar[kk-1] + p2*cvec->ar[kk-2]
           + p3*cvec->ar[kk-3] + p4*cvec->ar[kk-4] + p5*cvec->ar[kk-5] ;
     cvec->ar[kk] = ggk ;

     if( fabs(ggk) >= ccut ){        /* still big enough */
       nsmall = 0 ;
     } else {                        /* too small */
       nsmall++ ;
       if( nsmall >= NSCUT ) break ; /* too many small in a row */
     }
   }
   if( kk == kmax ) kk-- ; /* so kk = last value actually assigned */

   kmax = kk+1-nsmall ;    /* 1 past last 'big' value assigned */

   /* truncate, if we didn't get all the way to the end */

   RESIZE_doublevec(cvec,kmax) ;

   if( sfac < 1.0 ){
     for( kk=1 ; kk < kmax ; kk++ ) cvec->ar[kk] *= sfac ;
   }

   RETURN(cvec) ;
}

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct)
    for AR(3)+WN noise model with 4 parameters (aa,rr,th,sfac).
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma31( int nt, int *tau,
                      double aa, double rr, double th , double sfac )
{
   rcmat  *rcm ;
   LENTYP *len ;
   double **rc , *rii ;
   int ii , jj , bmax , jbot , itt,jtt ;
   doublevec *corvec ;

   if( nt < 2 ){
     if( verb ) ERROR_message("rcmat_arma31: nt=%d < 2",nt) ;
     return NULL ;
   }

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

   /* edit input params for reasonability */

        if( aa >  0.9 ) aa =  0.9 ;
   else if( aa < -0.9 ) aa = -0.9 ;

        if( rr >  0.9  ) rr =  0.9 ;
   else if( rr <  0.05 ) rr = 0.05 ;

        if( th >= HPI ) th = 0.95 * HPI ;
   else if( th <= 0.0 ) th = 0.05 * HPI ;

        if( sfac < 0.0 ) sfac = 0.0 ;
   else if( sfac > 1.0 ) sfac = 1.0 ;

   corvec = REML_compute_arma31_correlations( aa, rr, th, nt, sfac ) ;

   if( corvec == NULL ){
     if( verb ) ERROR_message("rcmat_arma31 - fails to compute correlations :(") ;
     rcmat_destroy( rcm ) ;
     return NULL ;
   }

   /* set maximum bandwidth */

   bmax = corvec->nar - 1 ;

   /* special and trivial case: identity matrix */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(double)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = remla_malloc(sizeof(double)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwidth */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){       /* a purely diagonal row/colum (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(double)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = remla_calloc(sizeof(double),len[ii]); /* space for the entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
       rii[jj] = corvec->ar[jtt] ;            /* from the correlation vector */
     }
   }

   return rcm ;
}
