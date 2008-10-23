/******* This file is meant to be #include-d into another application! *******/

#include "mrilib.h"

#undef  MTYPE
#undef  MPAIR
#ifndef FLOATIZE
# include "matrix.h"
# define MTYPE double
# define MPAIR double_pair
#else
# include "matrix_f.h"
# define MTYPE float
# define MPAIR float_pair
#endif

/*****
  Struct to hold a banded square matrix that is either symmetric,
  or upper or lower triangular (depending on the needs of the moment).
*****/

typedef struct {
  int nrc ;        /* # of rows and columns */
  short *len ;     /* in row/column #i, there are len[i] elements */
  MTYPE **rc ;     /* so the first column/row index is i+1-len[i] */
                   /* diagonal element #i is in rc[i][len[i]-1]   */
} rcmat ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0]  != NULL )

/*****
 Struct to hold a random sparse rectangular matrix.
*****/

typedef struct {
   int rows , cols ;
   int *cnum ;        /* cnum[j] is number of elements in col #j */
   int **cii ;        /* cii[j][k] is the i-index of cee[j][k]   */
   MTYPE **cee ;      /* for j=0..cols-1 , k=0..cnum[j]-1        */
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

typedef struct {
  int neq , mreg ;
  MTYPE rho , lam , barm ;
  rcmat  *cc ;        /* banded matrix:    neq  X neq  */
  matrix *dd ;        /* upper triangular: mreg X mreg */
  MTYPE cc_logdet , dd_logdet ;

  int         nglt ;
  gltfactors **glt ;
} reml_setup ;

/*****
  Struct to hold the information needed for all
  ARMA(1,1) cases, so that the best one can be found.
*****/

typedef struct {
  int na,nb , nab , pna,pnb , nset , izero , istwo ;
  MTYPE abot,da , bbot,db ;
  matrix *X ; sparmat *Xs ;
  reml_setup **rs ;
  char *savfil ;
} reml_collection ;

#undef  RC_SAVED
#define RC_SAVED(rc) \
  ( (rc)->savfil != NULL && (rc)->rs[(rc)->izero]->cc == NULL )

#undef  IAB
#define IAB(rc,a,b) ( (int)rint( ((a)-(rc)->abot) / (rc)->da )  \
     + (1+(rc)->na) * (int)rint( ((b)-(rc)->bbot) / (rc)->db ) )

static MTYPE corcut = 0.0025 ;

#undef  TAU
#define TAU(i) ((tau==NULL) ? (i) : tau[i])

/****************************************************************************/
/******* Generic functions to process a sparse matrix in sparmat form. ******/
/****************************************************************************/

/*--------------------------------------------------------------------------*/

void vector_spc_multiply( sparmat *a , MTYPE *b , MTYPE *c )
{
   int rows=a->rows , cols=a->cols ;
   int k , j , cn , *ci ;
   MTYPE *ce , bj ;

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

void vector_spc_multiply_transpose( sparmat *a , MTYPE *b , MTYPE *c )
{
   int rows=a->rows , cols=a->cols ;
   int i , k , cn , *ci ;
   MTYPE *ce , bj , sum ;

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

MTYPE sparsity_fraction( matrix a )
{
   int rows=a.rows , cols=a.cols , i,j,k=0 ; MTYPE val ;
   for( j=0 ; j < cols ; j++ )
     for( i=0 ; i < rows ; i++ ) if( a.elts[i][j] != 0.0 ) k++ ;
   val = k / (MTYPE)(rows*cols) ; return val ;
}

/*--------------------------------------------------------------------------*/

sparmat * matrix_to_sparmat( matrix a )
{
   int rows=a.rows , cols=a.cols ;
   int i , j , k ;
   sparmat *sa ;

   sa = (sparmat *)malloc(sizeof(sparmat)) ;

   sa->rows = rows ; sa->cols = cols ;
   sa->cnum = (int *)   calloc(sizeof(int)    ,cols) ;
   sa->cii  = (int **)  calloc(sizeof(int *)  ,cols) ;
   sa->cee  = (MTYPE **)calloc(sizeof(MTYPE *),cols) ;

   for( j=0 ; j < cols ; j++ ){
     sa->cii[j] = (int *)  malloc(sizeof(int)  *rows) ;
     sa->cee[j] = (MTYPE *)malloc(sizeof(MTYPE)*rows) ;
     for( k=i=0 ; i < rows ; i++ ){
       if( a.elts[i][j] != 0.0 ){
         sa->cii[j][k] = i; sa->cee[j][k] = a.elts[i][j]; k++;
       }
     }
     sa->cnum[j] = k ;
     if( k < rows ){
       sa->cii[j] = (int *)  realloc( (void *)sa->cii[j] , sizeof(int)  *k ) ;
       sa->cee[j] = (MTYPE *)realloc( (void *)sa->cee[j] , sizeof(MTYPE)*k ) ;
     } else {
       free(sa->cii[j]); sa->cii[j] = NULL;  /* not needed if column is full */
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
     if( sa->cii[i] != NULL ) free(sa->cii[i]) ;
     if( sa->cee[i] != NULL ) free(sa->cee[i]) ;
   }
   free(sa->cee) ; free(sa->cii) ; free(sa->cnum) ; free(sa) ; return ;
}

/****************************************************************************/
/****** Generic functions to process a sparse matrix in rcmat format. *******/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Create a rcmat struct, ready to be filled up. */

rcmat * rcmat_init( int n )
{
   rcmat *rcm ;

ENTRY("rcmat_init") ;

   if( n <= 1 ) RETURN(NULL) ;

   rcm      = (rcmat *)malloc( sizeof(rcmat) ) ;
   rcm->nrc = n ;
   rcm->len = (short  *)calloc( n , sizeof(short  ) ) ;
   rcm->rc  = (MTYPE **)calloc( n , sizeof(MTYPE *) ) ;
   RETURN(rcm) ;
}

/*--------------------------------------------------------------------------*/
/*! Delete a rcmat structure. */

void rcmat_destroy( rcmat *rcm )
{
   int      nn = rcm->nrc , ii ;
   MTYPE ** rc = rcm->rc ;
   short  *len = rcm->len ;

   if( rc != NULL ){
     for( ii=0 ; ii < nn ; ii++ ) if( rc[ii] != NULL ) free((void *)rc[ii]) ;
     free((void *)rc) ;
   }
   if( len != NULL ) free((void *)len) ;
   free((void *)rcm) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Duplicate a rcmat struct. */
#if 0
rcmat * rcmat_copy( rcmat *rcm )
{
   rcmat *qcm ;
   int ii,nn ;

   if( !ISVALID_RCMAT(rcm) ) return NULL ;

   nn  = rcm->nrc ;
   qcm = rcmat_init(nn) ;
   memcpy( qcm->len , rcm->len , sizeof(short)*nn ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = malloc( sizeof(MTYPE)*qcm->len[ii] ) ;
     memcpy( qcm->rc[ii] , rcm->rc[ii] , sizeof(MTYPE)*qcm->len[ii] ) ;
   }
   return qcm ;
}
#endif

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
   my_fwrite( rcm->len    , sizeof(short) , rcm->nrc , fp ) ;
   for( ii=0 ; ii < rcm->nrc ; ii++ )
     my_fwrite( rcm->rc[ii] , sizeof(MTYPE) , rcm->len[ii] , fp ) ;

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
   fread( qcm->len , sizeof(short) , nn , fp ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = malloc( sizeof(MTYPE)*qcm->len[ii] ) ;
     fread( qcm->rc[ii] , sizeof(MTYPE) , qcm->len[ii] , fp ) ;
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
     my_fwrite( a->elts[ii] , sizeof(MTYPE) , a->cols , fp ) ;

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

   a = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(a) ;
   matrix_create( rows , cols , a ) ;
   for( ii=0 ; ii < rows ; ii++ )
     fread( a->elts[ii] , sizeof(MTYPE) , cols , fp ) ;

   RETURN(a) ;
}

/*--------------------------------------------------------------------------*/
/*! Save the matrices in a reml_setup struct to a file (and erase them). */

void reml_setup_savemat( FILE *fp , reml_setup *rs )
{
ENTRY("reml_setup_savemat") ;
   if( fp     == NULL || rs     == NULL ) EXRETURN ;
   if( rs->cc == NULL || rs->dd == NULL ) EXRETURN ;

   rcmat_writebin (fp,rs->cc); rcmat_destroy (rs->cc);               rs->cc = NULL;
   matrix_writebin(fp,rs->dd); matrix_destroy(rs->dd); free(rs->dd); rs->dd = NULL;

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

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as a symmetric matrix, and
    Choleski factor it in place.  Return value is 0 if all is OK.
    A positive return indicates the row/column that had trouble. */

int rcmat_choleski( rcmat *rcm )
{
   int ii,jj,kk , nn , jbot,kbot ; short *len ;
   MTYPE sum , **rc , *rii , *rjj ;

   if( !ISVALID_RCMAT(rcm) ) return 999999999 ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       if( rc[ii][0] <= 0.0 ) return (ii+1) ;
       rc[ii][0] = sqrt(rc[ii][0]) ; continue ;
     }
     jbot = ii - len[ii] + 1 ;
     rii  = rc[ii] - jbot ;
     for( jj=jbot ; jj <= ii ; jj++ ){
       if( len[jj] == 1 ){
         rii[jj] = rii[jj] / rc[jj][0] ;
         continue ;
       }
       kbot = jj - len[jj] + 1 ;
       rjj  = rc[jj] - kbot ;
       sum  = rii[jj] ;
       if( kbot < jbot ) kbot = jbot ;
       for( kk=kbot ; kk < jj ; kk++ ) sum -= rii[kk]*rjj[kk] ;
       if( jj < ii ){
         rii[jj] = sum / rjj[jj] ;
       } else {
         if( sum <= 0.0 ) return (ii+1) ;
         rii[ii] = sqrt(sum) ;
       }
     }
   }
   return 0 ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as a lower triangular matrix,
    and solve the matrix-vector equation [rcm][x] = [vec], in place. */

void rcmat_lowert_solve( rcmat *rcm , MTYPE *vec )
{
   int nn , jbot ; short *len ; register int ii,jj ;
   MTYPE **rc ; register MTYPE *rii , sum , *vv ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;
   vv  = vec ;

#undef  RV
#define RV(k) rii[k]*vv[k]

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       vv[ii] = vv[ii] / rc[ii][0] ; continue ;
     }
     jbot = ii - len[ii] + 1 ; rii = rc[ii] - jbot ;
#undef UNROLL
#ifdef UNROLL
     switch( len[ii] ){
       default:
#endif
         sum = vv[ii] ;
         for( jj=jbot ; jj < ii ; jj++ ) sum -= RV(jj) ;
#ifdef UNROLL
       break ;
       case 2:
         sum = vv[ii]-RV(ii-1) ; break ;
       case 3:
         sum = vv[ii]-RV(ii-2)-RV(ii-1) ; break ;
       case 4:
         sum = vv[ii]-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 5:
         sum = vv[ii]-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 6:
         sum = vv[ii]-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 7:
         sum = vv[ii]-RV(ii-6)-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 8:
         sum = vv[ii]-RV(ii-7)-RV(ii-6)-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 9:
         sum = vv[ii]-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 10:
         sum = vv[ii]-RV(ii-9)-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 11:
         sum = vv[ii]-RV(ii-10)-RV(ii-9)-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 12:
         sum = vv[ii]-RV(ii-11)-RV(ii-10)-RV(ii-9)
                     -RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
     }
#endif
     vv[ii] = sum / rii[ii] ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as an upper triangular matrix,
    and solve the matrix-vector equation [rcm][x] = [vec], in place. */

void rcmat_uppert_solve( rcmat *rcm , MTYPE *vec )
{
   int nn , ibot ; short *len ; register int ii,jj ;
   MTYPE **rc ; register MTYPE *rjj , *vv , xj ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;
   vv  = vec ;

   for( jj=nn-1 ; jj >= 0 ; jj-- ){
     ibot = jj - len[jj] + 1 ;
     rjj  = rc[jj] - ibot ;
     xj = vv[jj] = vv[jj] / rjj[jj] ;
     for( ii=ibot ; ii < jj ; ii++ ) vv[ii] -= rjj[ii] * xj ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Setup sparse correlation matrix
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

rcmat * rcmat_arma11( int nt, int *tau, MTYPE rho, MTYPE lam )
{
   rcmat *rcm ;
   short *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 ) return NULL ;

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
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwith */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){       /* a purely diagonal row/colum (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = calloc(sizeof(MTYPE),len[ii]) ;      /* space for the entries */
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
                                MTYPE rho, MTYPE lam , matrix *X )
{
   int ii , jj , mm ;
   reml_setup *rset ;
   rcmat *rcm ;
   matrix *W , *D ;
   MTYPE *vec , csum,dsum,val ;

   if( nt < 2 || X == NULL || X->rows != nt ){
     if( verb ) ERROR_message("setup_arma11_reml: bad inputs?!") ;
     return NULL ;
   }

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
     if( verb > 1 )
       ERROR_message("rcmat_choleski fails with code=%d: rho=%f lam=%f",ii,rho,lam) ;
     rcmat_destroy(rcm); return NULL;
   }

   /* prewhiten each column of X into W matrix */

   W = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(W) ;
   matrix_create(nt,mm,W) ;
   vec = (MTYPE *)malloc(sizeof(MTYPE)*nt) ;
   for( jj=0 ; jj < X->cols ; jj++ ){
     for( ii=0 ; ii < nt ; ii++ ) vec[ii] = X->elts[ii][jj] ; /* extract col */
     rcmat_lowert_solve( rcm , vec ) ;                          /* prewhiten */
     for( ii=0 ; ii < nt ; ii++ ) W->elts[ii][jj] = vec[ii] ;  /* put into W */
   }
   free((void *)vec) ;

   /* compute QR decomposition of W, save R factor into D, toss W */

   D = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(D) ;
   matrix_qrr( *W , D ) ;
   matrix_destroy(W) ; free((void *)W) ;
   if( D->rows <= 0 ){
     if( verb > 1 )
       ERROR_message("matrix_qrr fails?! a=%.3f lam=%.3f",rho,lam) ;
     matrix_destroy(D) ; free((void *)D) ; rcmat_destroy(rcm) ; return NULL ;
   }

   /* create the setup struct, save stuff into it */

   rset = (reml_setup *)malloc(sizeof(reml_setup)) ;
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

reml_setup * REML_setup_one( matrix *X , int *tau , MTYPE rho , MTYPE bb )
{
   reml_setup *rset ;
   MTYPE lam = LAMBDA(rho,bb) ;

   rset = setup_arma11_reml( X->rows , tau , rho , lam , X ) ;
   if( rset != NULL ) rset->barm = bb ;
   return rset ;
}

/*--------------------------------------------------------------------------*/

/** intermediate vectors to be saved in case of later need **/

static vector *bb1=NULL,*bb2,*bb3,*bb4,*bb5,*bb6,*bb7 ;
static MTYPE rsumq=0.0 ;  /* sum of squares of residual */

/*--------------------------------------------------------------------------*/
/*! Compute the REML -log(likelihood) function for a particular case,
    given the case's setup and the data and the regression matrix X. */

MTYPE REML_func( vector *y , reml_setup *rset , matrix *X , sparmat *Xs )
{
   int n , ii ;
   MTYPE val ;

ENTRY("REML_func") ;

   if( y == NULL || rset == NULL || X == NULL ){
     if( bb1 != NULL ){
       vector_destroy(bb1) ; bb1 = NULL ;
       vector_destroy(bb2) ; vector_destroy(bb3) ;
       vector_destroy(bb4) ; vector_destroy(bb5) ;
       vector_destroy(bb6) ; vector_destroy(bb7) ;
     }
     RETURN(-666.0) ;
   }

   if( bb1 == NULL ){
     bb1 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb1) ;
     bb2 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb2) ;
     bb3 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb3) ;
     bb4 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb4) ;
     bb5 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb5) ;
     bb6 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb6) ;
     bb7 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb7) ;
   }

   n = rset->neq ;

   /** Seven steps to compute the prewhitened residuals */

   vector_equate( *y , bb1 ) ;
   rcmat_lowert_solve( rset->cc , bb1->elts ) ;      /* bb1 = C^(-T) y */
                                                     /* prewhitened data */
   vector_equate( *bb1 , bb2 ) ;
   rcmat_uppert_solve( rset->cc , bb2->elts ) ;      /* bb2 = C^(-1) bb1 */

   if( Xs != NULL ){
     vector_create_noinit( Xs->cols , bb3 ) ;
     vector_spc_multiply_transpose( Xs , bb2->elts , bb3->elts ) ;
   } else {
     vector_multiply_transpose( *X , *bb2 , bb3 ) ;  /* bb3 = X^T bb2 */
   }

   vector_rrtran_solve( *(rset->dd) , *bb3 , bb4 ) ; /* bb4 = D^(-T) bb3 */

   vector_rr_solve( *(rset->dd) , *bb4 , bb5 ) ;     /* bb5 = D^(-1) bb4 */
                                                     /*    = beta_hat */

   if( Xs != NULL ){
     vector_create_noinit( Xs->rows , bb6 ) ;
     vector_spc_multiply( Xs , bb5->elts , bb6->elts ) ;
   } else {
     vector_multiply( *X , *bb5 , bb6 ) ;            /* bb6 = X bb5 */
   }                                                 /* fitted model */

   vector_equate( *bb6 , bb7 ) ;
   rcmat_lowert_solve( rset->cc , bb7->elts ) ;      /* bb7 = C^(-T) bb6 */
                                                     /* prewhitened fit */
   vector_subtract( *bb1 , *bb7 , bb2 ) ;            /* result = bb1 - bb7 */
                                                     /* prewhitened residual */
   rsumq = vector_dotself(*bb2) ;                    /* sum of squares */

   if( rsumq > 0.0 )
     val = (n - rset->mreg) * log(rsumq)             /* the REML function! */
          + rset->dd_logdet + rset->cc_logdet ;      /* -log(likelihood) */
   else
     val = 0.0 ;

   RETURN(val) ;
}

/*--------------------------------------------------------------------------*/

void gltfactors_destroy( gltfactors *gf )
{
   if( gf == NULL ) return ;
   if( gf->Jright != NULL ){ matrix_destroy(gf->Jright); free(gf->Jright); }
   if( gf->Jleft  != NULL ){ matrix_destroy(gf->Jleft ); free(gf->Jleft ); }
   if( gf->sig    != NULL ){ vector_destroy(gf->sig   ); free(gf->sig   ); }
   free(gf) ;
}

/*--------------------------------------------------------------------------*/

void reml_setup_destroy( reml_setup *rset )
{
   int ii ;

   if( rset == NULL ) return ;
   if( rset->cc != NULL ) rcmat_destroy( rset->cc ) ;
   if( rset->dd != NULL ){ matrix_destroy( rset->dd ); free(rset->dd); }
   for( ii=0 ; ii < rset->nglt ; ii++ ) gltfactors_destroy( rset->glt[ii] ) ;
   if( rset->glt != NULL ) free(rset->glt) ;
   free((void *)rset) ;
   return ;
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
   return ;
}

static void kill_purge( char *fn ) /*---- remove fn from the qpurge list ----*/
{
   int ii ;
   if( fn == NULL || *fn == '\0' || qpurge == NULL ) return ;
   for( ii=0 ; ii < npurge ; ii++ )  /* find in list */
     if( qpurge[ii] != NULL && strcmp(qpurge[ii],fn) == 0 ) break ;
   if( ii < npurge ){ free(qpurge[ii]) ; qpurge[ii] = NULL ; }
   return ;
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
     remove(rcol->savfil) ; free(rcol->savfil) ; rcol->savfil = NULL ;
   }

   if( !atexit_called ){ atexit(remla_atexit); atexit_called = 1; }

   pg = mri_purge_get_tmpdir() ; ts = mri_purge_get_tsuf() ;
   un = UNIQ_idcode() ;
   un[0] = 'R' ; un[1] = 'E'   ; un[2] = 'M'   ; un[3] = 'L' ;
   un[4] = '_' ; un[5] = ts[0] ; un[6] = ts[1] ; un[7] = ts[2] ;
   rcol->savfil = malloc(strlen(pg)+64) ;
   strcpy(rcol->savfil,pg); strcat(rcol->savfil,"/"); strcat(rcol->savfil,un);
   free(un) ;

   fp = fopen( rcol->savfil , "wb" ) ;
   if( fp == NULL ){
     ERROR_message("3dREMLfit can't write to -usetemp file %s",rcol->savfil) ;
     free(rcol->savfil) ; rcol->savfil = NULL ;
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
     free(rcol->savfil) ; rcol->savfil = NULL ;
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
     if( !zsave ) free((void *)rcol->rs) ;
   }
   if( rcol->savfil != NULL ){
     kill_purge(rcol->savfil) ;
     remove(rcol->savfil) ; free(rcol->savfil) ; rcol->savfil = NULL ;
   }
   if( !zsave ) free((void *)rcol) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int allow_negative_cor = 0 ;
void REML_allow_negative_correlations( int i ){ allow_negative_cor = i ; }

/*--------------------------------------------------------------------------*/

reml_collection * REML_setup_all( matrix *X , int *tau ,
                                  int nlev , MTYPE atop , MTYPE btop )
{
   int ii,jj,kk , nt , nset , pna,pnb , na,nb ;
   MTYPE da,db, bb,aa, lam , bbot,abot ;
   reml_collection *rrcol=NULL ;

   if( X == NULL ) return rrcol ;              /* bad */

   nt = X->rows ; if( nt < 9 ) return rrcol ;  /* bad */

   /* set grid in a and b parameters */

   if( nlev > 0 ){
     if( nlev < 3 ) nlev = 3 ;
     if( btop < 0.0 || btop > 0.9 ) btop = 0.9 ;
     nb = 1 << (nlev+1) ; pnb = nlev+1 ; bbot = -btop ; db = 2.0*btop / nb ;

     if( atop < 0.0 || atop > 0.9 ) atop = 0.9 ;
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

   rrcol = (reml_collection *)malloc(sizeof(reml_collection)) ;

   rrcol->X = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(rrcol->X) ;
   matrix_equate( *X , rrcol->X ) ;

   lam = sparsity_fraction( *X ) ;
   if( lam <= 0.30 ) rrcol->Xs = matrix_to_sparmat( *X ) ;
   else              rrcol->Xs = NULL ;

#if 0
   INFO_message("X matrix sparsity = %.1f%% ==> %s",
                100.0*lam ,
                (rrcol->Xs==NULL) ? "NOT USED for speedup" : "USED for speedup" ) ;
#endif

   if( nlev > 0 ){
     rrcol->nab   = (nb+1)*(na+1) ;  /* number of cases to create */
     rrcol->istwo = 0 ;
   } else {
     rrcol->nab   = 2 ;
     rrcol->istwo = 1 ;
   }
   rrcol->rs = (reml_setup **)calloc(sizeof(reml_setup *),rrcol->nab) ;

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

   if( nlev == 0 ){  /* special setup with just 2 (a,b) cases */

     bb = bbot ; aa = abot ; lam = LAMBDA(aa,bb) ; kk = 1 ;
     rrcol->rs[kk] = setup_arma11_reml( nt,tau , aa,lam , X ) ;
     if( rrcol->rs[kk] != NULL ){ rrcol->rs[kk]->barm = bb ; nset++ ; }

   } else {          /* general setup case with an (a,b) grid of cases */

     for( ii=0 ; ii <= na ; ii++ ){
       aa = ii*da + abot ;                    /* AR parameter */
       for( jj=0 ; jj <= nb ; jj++ ){
         bb  = jj*db + bbot ;                 /* MA parameter */
         lam = LAMBDA(aa,bb) ;                /* +1 super-diagonal element */
         kk  = ii + (1+na)*jj ;
         if( kk != rrcol->izero &&
             ( lam >= corcut || (lam <= -corcut && allow_negative_cor) ) ){
           rrcol->rs[kk] = setup_arma11_reml( nt,tau, aa,lam , X ) ;
           if( rrcol->rs[kk] != NULL ){ rrcol->rs[kk]->barm = bb ; nset++ ; }
         }
       }
     }

   }

   rrcol->nset = nset ; rrcol->savfil = NULL ;
   return rrcol ;
}

/*--------------------------------------------------------------------------*/

static int    REML_status   = -1 ;

static MTYPE  REML_best_rho = 0.0 ;
static MTYPE  REML_best_lam = 0.0 ;
static MTYPE  REML_best_bb  = 0.0 ;
static MTYPE  REML_best_ssq = 0.0 ;
static int    REML_best_ind = 0   ;

static vector REML_best_prewhitened_data_vector ;     /* in bb1 */
static vector REML_best_beta_vector ;                 /* in bb5 */
static vector REML_best_fitts_vector ;                /* in bb6 */
static vector REML_best_prewhitened_fitts_vector ;    /* in bb7 */
static vector REML_best_prewhitened_errts_vector ;    /* in bb2 */

static MTYPE  REML_olsq_rho = 0.0 ;
static MTYPE  REML_olsq_lam = 0.0 ;
static MTYPE  REML_olsq_bb  = 0.0 ;
static MTYPE  REML_olsq_ssq = 0.0 ;

static vector REML_olsq_prewhitened_data_vector ;
static vector REML_olsq_beta_vector ;
static vector REML_olsq_fitts_vector ;
static vector REML_olsq_prewhitened_fitts_vector ;
static vector REML_olsq_prewhitened_errts_vector ;

/*--------------------------------------------------------------------------*/
/* Inputs: y=data vector, rrcol=collection of REML setup stuff.
   Output: stored in static data defined just above.
*//*------------------------------------------------------------------------*/

MTYPE REML_find_best_case( vector *y , reml_collection *rrcol )
{
   MTYPE rbest , rval ;
   int   na,nb , pna,pnb , ltop ;
   int   nab, lev, dab,dab2, ia,jb,kk, ibot,itop, jbot,jtop, ibest,jbest,kbest ;

ENTRY("REML_find_best_case") ;

   if( y == NULL ){  /* memory cleanup */
     vector_destroy( &REML_best_prewhitened_data_vector ) ;
     vector_destroy( &REML_best_beta_vector ) ;
     vector_destroy( &REML_best_fitts_vector ) ;
     vector_destroy( &REML_best_prewhitened_fitts_vector ) ;
     vector_destroy( &REML_best_prewhitened_errts_vector ) ;
     REML_func( NULL,NULL,NULL,NULL ) ;
     REML_status = -1 ; RETURN(-666.0) ;
   }

#if 0
   if( REML_status == -1 ){ /* memory setup */
     vector_initialize( &REML_best_prewhitened_data_vector ) ;
     vector_initialize( &REML_best_beta_vector ) ;
     vector_initialize( &REML_best_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_errts_vector ) ;
   }
#endif
   REML_status = 0 ;

   /* copy (a,b) grid parameters to local variables */

   na = rrcol->na ; pna = rrcol->pna ;
   nb = rrcol->nb ; pnb = rrcol->pnb ;

   /** do the Ordinary Least Squares (olsq) case **/

   kbest = rrcol->izero ;
   jbest = kbest / (1+na) ;
   ibest = kbest % (1+na) ;
   rbest = REML_func( y , rrcol->rs[kbest] , rrcol->X,rrcol->Xs ) ;
#if 0
   vector_equate( *bb1 , &REML_olsq_prewhitened_data_vector ) ;
   vector_equate( *bb2 , &REML_olsq_prewhitened_errts_vector ) ;
   vector_equate( *bb5 , &REML_olsq_beta_vector ) ;
   vector_equate( *bb6 , &REML_olsq_fitts_vector ) ;
   vector_equate( *bb7 , &REML_olsq_prewhitened_fitts_vector ) ;
#endif
   REML_olsq_ssq = rsumq ;

   /** do power-of-2 descent through the (a,b) grid to find the best pair **/

   nab  = MIN(pna,pnb)  ; ltop = nab-2 ;
   ibot = 0 ; itop = na ;                 /* scan from ibot..itop, jbot..jtop */
   jbot = 0 ; jtop = nb ;
   for( lev=ltop ; lev >= 0 ; lev-- ){     /* lev = power-of-2 grid scan size */
     dab = 1 << lev ; dab2 = 2*dab ;       /* dab = 2^lev = step size in scan */
     for( jb=jbot ; jb <= jtop ; jb+=dab ){
       for( ia=ibot ; ia <= itop ; ia+=dab ){
         if( lev < ltop && jb%dab2 == 0 && ia%dab2 == 0 ) continue; /* did it */
         kk = ia + jb*(na+1) ;
         if( kk == rrcol->izero ) continue ;      /* OLSQ => did this already */
         if( rrcol->rs[kk] == NULL ) continue ;         /* invalid grid point */
         rval = REML_func( y , rrcol->rs[kk] , rrcol->X,rrcol->Xs ) ;
         if( rval < rbest ){                          /* the best so far seen */
           rbest = rval ; kbest = kk ; ibest = ia ; jbest = jb ;
#if 0
           vector_equate( *bb1 , &REML_best_prewhitened_data_vector ) ;
           vector_equate( *bb2 , &REML_best_prewhitened_errts_vector ) ;
           vector_equate( *bb5 , &REML_best_beta_vector ) ;
           vector_equate( *bb6 , &REML_best_fitts_vector ) ;
           vector_equate( *bb7 , &REML_best_prewhitened_fitts_vector ) ;
#endif
           REML_best_ssq = rsumq ;
         }
       }
     } /* end of scan with step size dab */
     if( lev > 0 ){                    /* set up to scan near the best so far */
       ibot = ibest-dab ; if( ibot < 0  ) ibot = 0  ;
       itop = ibest+dab ; if( itop > na ) itop = na ;
       jbot = jbest-dab ; if( jbot < 0  ) jbot = 0  ;
       jtop = jbest+dab ; if( jtop > nb ) jtop = nb ;
     }
   } /* end of scan descent through different levels */

   /*** deal with the winner ***/

   if( kbest == rrcol->izero ){                 /** OLSQ won! **/
     REML_best_ssq = REML_olsq_ssq ;
#if 0
     vector_equate( REML_olsq_prewhitened_data_vector,
                   &REML_best_prewhitened_data_vector ) ;
     vector_equate( REML_olsq_prewhitened_errts_vector,
                   &REML_best_prewhitened_errts_vector ) ;
     vector_equate( REML_olsq_beta_vector , &REML_best_beta_vector ) ;
     vector_equate( REML_olsq_fitts_vector, &REML_best_fitts_vector ) ;
     vector_equate( REML_olsq_prewhitened_fitts_vector,
                   &REML_best_prewhitened_fitts_vector ) ;
#endif
   }

   REML_best_rho = rrcol->rs[kbest]->rho ;
   REML_best_lam = rrcol->rs[kbest]->lam ;
   REML_best_bb  = rrcol->rs[kbest]->barm;
   REML_best_ind = kbest ;

   REML_status = 1 ; RETURN(rbest) ;
}

/*--------------------------------------------------------------------------*/

gltfactors * REML_get_gltfactors( matrix *D , matrix *G )
{
   int nn , rr , i,j ; MTYPE ete ;
   gltfactors *gf ;
   matrix *JL , *JR , *GT , *E,*F,*Z ; vector *S ;

ENTRY("REML_get_gltfactors") ;

   if( D       == NULL    || G       == NULL    ) RETURN( NULL );
   if( D->rows != D->cols || D->rows != G->cols ) RETURN( NULL );

   nn = D->rows ; rr = G->rows ; if( nn < 1 || rr < 1 ) RETURN( NULL );

   /* [D] is nn X nn (upper triangular); [G] is rr X nn, with rr < nn */

   GT = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(GT) ;
   matrix_transpose( *G , GT ) ;         /* GT = [G'] = nn X rr matrix */

   F = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(F) ;
   matrix_rrtran_solve( *D , *GT , F ) ; /* F = inv[D'] [G'] = nn X rr matrix */
   matrix_destroy(GT); free(GT);

   S = (vector *)malloc(sizeof(vector)) ; vector_initialize(S) ;

   if( rr == 1 ){               /* F is really an nn-vector */
     ete = matrix_frobenius( *F ) ;        /* sum of squares */
     vector_create(rr,S) ; S->elts[0] = sqrt(ete) ; /* sigma */
     ete = 1.0 / ete ;                       /* scale factor */
     JR = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(JR) ;
     matrix_create(rr,nn,JR) ;
     for( i=0 ; i < nn ; i++ )
       JR->elts[0][i] = G->elts[0][i] * ete ;  /* scale G to get JR */

   } else {                     /* QR factor F to get E, then solve for JR */
     E  = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(E) ;
     Z  = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(Z) ;
     JR = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(JR) ;
     matrix_qrr( *F , E ) ;
     matrix_rrtran_solve( *E , *G , Z ) ;
     matrix_rr_solve( *E , *Z , JR ) ;  /* JR = inv[E] inv[E'] G = rr X nn */
     matrix_colsqsums( *E , S ) ;       /* S = [sig] = rr vector */
     matrix_destroy(Z); free(Z);
     matrix_destroy(E); free(E);
   }

   /* compute JL */

   JL = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(JL) ;
   matrix_rr_solve( *D , *F , JL ) ;  /* JL = inv[D] [F] = nn X rr matrix */
   matrix_destroy(F); free(F);

   /* save results into struct */

   gf = (gltfactors *)malloc(sizeof(gltfactors)) ;
   gf->mpar   = nn ;
   gf->rglt   = rr ;
   gf->Jright = JR ;
   gf->Jleft  = JL ;
   gf->sig    = S  ;
   RETURN( gf );
}

/*--------------------------------------------------------------------------*/

void REML_add_glt_to_one( reml_setup *rs , matrix *G )
{
   gltfactors *gf ; int ng ;

ENTRY("REML_add_glt_to_one") ;

   if( rs == NULL || G == NULL ) EXRETURN ;

   gf = REML_get_gltfactors( rs->dd , G ) ;
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
static MTYPE   betaR = 0.0  ;  /* GLT R^2 statistic */
static MTYPE   betaF = 0.0  ;  /* GLT F statistic   */

MTYPE REML_compute_gltstat( vector *y, vector *bfull, MTYPE fsumq ,
                            reml_setup *rset, gltfactors *gf,
                            matrix *G, sparmat *Gs, matrix *X, sparmat *Xs )
{
   MTYPE fstat , rsumq ;
   vector ba , bb , br ;

ENTRY("REML_compute_gltstat") ;

   if( betaG == NULL ){
     betaG = (vector *)malloc(sizeof(vector)) ; vector_initialize(betaG) ;
     betaT = (vector *)malloc(sizeof(vector)) ; vector_initialize(betaT) ;
   }

   if( y     == NULL || bfull == NULL ||
       rset  == NULL || gf    == NULL || fsumq <= 0.0 ){
     vector_destroy(betaG) ; vector_destroy(betaT) ;
     free(betaG) ; free(betaT) ; betaG = betaT = NULL ; betaR = 0.0 ;
     RETURN( 0.0 );
   }

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

   fstat = ( (rsumq-fsumq) / gf->rglt ) / ( fsumq / (X->rows - X->cols) ) ;
   if( fstat < 0.0 ) fstat = 0.0 ;  /* should not happen */
   betaF = fstat ;

   /* 23 Oct 2008: generalized correlation coefficient squared */

   if( rsumq > 0.0 ) betaR = 1.0 - fsumq / rsumq ;
   else              betaR = 0.0 ;

   /* compute GLT combinations of beta coefficients, and t-statistics */

   if( G != NULL ){
     MTYPE fsig ; int ii ;
     fsig = sqrt( fsumq / (X->rows - X->cols) ) ;
     if( Gs != NULL ){
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
