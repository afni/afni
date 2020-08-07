/******* This file is meant to be #include-d into another application! *******/
/*------ [which is to say, 3dREMLfit.c] ------*/

/******* Extensive changes made July 2020 to generalize
         from only ARMA(1,1) noise model to ARMA(3,1) and beyond *******/

#undef  MTYPE
#undef  MPAIR
#include "matrix.h"
#define MTYPE double        /* do NOT change this, or you will be unhappy */
#define MPAIR double_pair

/* other important type in rcmat.h is LENTYP: storing non-negative integers */

#include "mrilib.h"  /* Keep after decision about matrix.h inclusion
                                                      ZSS  Nov. 21 2014*/
#undef  BIGVAL
#define BIGVAL 1.e+38  /* a reasonably big number */

/*===========================================================================*/
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

void * remla_realloc( void *ptr , size_t nb )
{
   void *mmm=NULL ;

   if( ptr == NULL ) return remla_malloc(nb) ;

#pragma omp critical (remla_malloc)
   { if( remla_htable != NULL ){
       char key[32] , *len ; void *mmm ;
       sprintf(key,"%p",mmm) ;
       len = findin_Htable( key , remla_htable ) ;
       if( len != NULL ){
         size_t na=0 ; sscanf(len,"%lld",&na) ;
         if( nb > na ){
           remla_memtot -= na ;
           removefrom_Htable( key , remla_htable ) ;
           mmm = remla_malloc( nb ) ;
           memcpy( mmm , ptr , na ) ; free(ptr) ;
         } else {
           mmm = ptr ;  /* don't bother to truncate */
         }
       }
     }
   }

   if( mmm == NULL ) mmm = realloc( ptr , nb ) ;
   return mmm ;
}

/*........................................*/

#define REMLA_memprint                                            \
  INFO_message("REML setup memory = %s (%s)",                     \
               approximate_number_string((double)remla_memtot) ,  \
               commaized_integer_string(remla_memtot)            )

#define REMLA_memreset                              \
  do{ remla_memtot=0; destroy_Htable(remla_htable); \
      remla_htable=NULL;                                  } while(0)

#define REMLA_memsetup                              \
  do{ remla_memtot=0; destroy_Htable(remla_htable); \
      remla_htable=new_Htable(999); Htable_set_vtkill(1); } while(0)

#else  /*----------------- ENABLE_REMLA_MALLOC not defined -----------------*/

#define REMLA_memreset /*nada*/
#define REMLA_memsetup /*nada*/
#define REMLA_memprint /*nada*/

#define remla_malloc  malloc
#define remla_calloc  calloc
#define remla_free    free
#define remla_realloc realloc

#endif /*--------------- ENABLE_REMLA_MALLOC ---------------*/

/*===========================================================================*/
/*===========================================================================*/

#ifdef USE_OMP     /* disable tracking macros from debugtrace.h */

# undef  RETURN
# undef  EXRETURN
# undef  ENTRY
# define ENTRY(x)  /*nada*/
# define RETURN(x) return(x)
# define EXRETURN  return

#endif /* USE_OMP */

/*------------------------------------------------------------------------*/
#ifdef ENABLE_REMLA_MALLOC      /* cf. supra */
# define malloc  remla_malloc   /* these are just for matrix.c */
# define calloc  remla_calloc   /* and rcmat.c, included below */
# define free    remla_free
# define realloc remla_realloc
#endif

#include "matrix.c"    /* included here for optimization */
#include "rcmat.c"     /* and for memory tracking, maybe */

#undef ALLOW_ARMA51   /* not ready for prime time */
#include "armacor.c"  /* contains code for autocorrelation vectors */

#ifdef ENABLE_REMLA_MALLOC      /* now turn these macros off */
# undef malloc
# undef calloc
# undef free
# undef remla_realloc
#endif
/*------------------------------------------------------------------------*/

/*****
 Struct to hold a random sparse rectangular matrix.
 There are a bunch of functions below to use these beasts.
*****/

typedef struct {
   int rows , cols ;
   int *cnum ;        /* cnum[j] is number of elements in col #j */
                      /*  if(== rows) then cii[j] is NULL and    */
                      /*  full column is stored in cee[j] vector */
   int **cii ;        /* cii[j][k] is the i-index of cee[j][k]   */
                      /*   cii[j] == NULL if cnum[j]==rows       */
   double **cee ;     /* for j=0..cols-1 , k=0..cnum[j]-1        */
} sparmat ;

/*****
  Struct to hold the info needed for a partial-F statistic
  applied to the linear regression results [GLT]
*****/

typedef struct {
   int mpar , rglt ; /* [beta_R] = [Jleft] [Jright] [beta_Full] */
   matrix *Jright ;  /* r X m matrix */
   matrix *Jleft ;   /* m x r matrix */
   vector *sig ;     /* rglt elements = normalized stdev */
} gltfactors ;

/* codes for different temporal correlation models */

#define ARMA11_MODEL  1
#define ARMA11_NPARAM 2

#define ARMA31_MODEL  3  /* July 2020 */
#define ARMA31_NPARAM 4

#define ARMA51_MODEL  5  /* not implemented yet */
#define ARMA51_NPARAM 6

/* macros to access parameters by names used in different models */

/* for ARMA(1,1) */
#define RS_arma11_aa(rs)  (rs)->pvec[0]  /* this is a */
#define RS_arma11_bb(rs)  (rs)->pvec[1]  /* this is b */
#define RS_arma11_lam(rs) (rs)->pvec[2]  /* this is lambda - calculated from (a,b) */

/* compute lambda from ARMA(1,1) (a,b) parameters */
#undef  LAMBDA
#define LAMBDA(a,b) ((b+a)*(1.0+a*b)/(1.0+2.0*a*b+b*b))

/* for ARMA(3,1) */
#define RS_arma31_aa(rs)   (rs)->pvec[0]  /* this is a */
#define RS_arma31_rr(rs)   (rs)->pvec[1]  /* this is r */
#define RS_arma31_th(rs)   (rs)->pvec[2]  /* this is theta */
#define RS_arma31_sfac(rs) (rs)->pvec[3]  /* this is vrt (variance ratio) */

/* macros to load a local array named 'pvec' */

#define LOAD_pvec2(a,b)     ( pvec[0]=(a), pvec[1]=(b) )
#define LOAD_pvec4(a,b,c,d) ( pvec[0]=(a), pvec[1]=(b), pvec[2]=(c), pvec[3]=(d) )

/*****
  Struct to hold the information needed to compute
  the REML log-likelihood for a single case.
*****/

typedef struct {
  int neq , mreg ;
  int noise_model ;  /* one of the _MODEL constants above */
  int junk ;         /* 4 byte spacer */

  /* set of model parameters [see RS_arma... macros above] */

  double pvec[6] ;

  rcmat  *cc ;         /* banded D matrix:           neq  X neq  */
  matrix *dd ;         /* upper triangular D matrix: mreg X mreg */
  double cc_logdet , dd_logdet ; /* 2 * log(determinant(matrix)) */

  int         nglt ; /* number of GLTS added to this sucker */
  gltfactors **glt ; /* the parameters for each GLT */
} reml_setup ;

/*****
  A plus-size version of the above,
  to allow for voxel-wise regression in 3dREMLfit
*****/

typedef struct {
  reml_setup *rset ;
  matrix *X ; sparmat *Xs ;
} reml_setup_plus ;          /* 22 Jul 2015 */

/*****
  Struct to hold a collection of reml setups,
  - with potential save file name,
  - with up to 6 dimensions of parameters (a,b,c,d,e,f)
  - at present, ARMA(1,1) uses 2 parameters, and ARMA(3,1) uses 4
  - ARMA(5,1) if ever implemented will have 5 or 6 parameters
*****/

typedef struct {
  int noise_model ;             /* one of the _MODEL codes above */
  int pnum ;                    /* number of parameters in this model (2..6) */
  int nset ;                    /* total number of setups allocated in rs */
  int ndone ;                   /* total number of setups filled in rs */
  int izero ;                   /* index of the 0 (nominal) model in rs */
  int istwo ;                   /* flag for special case of only 2 models */

                                /* dimensions of each parameter (powers of 2) */
  int  na, nb, nc, nd, ne, nf ; /* grid for 'a' runs 0..na inclusive, etc */
  int pna,pnb,pnc,pnd,pne,pnf ; /* na = 2^pna if pna > 0, otherwise na = 0 */

  int  na1, nb1 , nc1  , nd1   , ne1 , nf1 ;         /* na1=na+1, nb1=nb+1 */
  int       nab1, nabc1, nabcd1, nabcde1, nabcdef1 ; /* nab1=na1*nb1 ... */

  double abot, bbot, cbot, dbot, ebot, fbot ; /* min value of each param */
  double atop, btop, ctop, dtop, etop, ftop ; /* max value of each param */
  double azer, bzer, czer, dzer, ezer, fzer ; /* param values at 0 (nominal) */
  double   da,   db,   dc,   dd,   de,   df ; /* grid spacing of each param */

  float avglen ;                /* average row length of the rcmat's in rs */
  matrix *X ; sparmat *Xs ;     /* regression model matrix */
  reml_setup **rs ;             /* nset reml_setup's - what this is all about */
  char *savfil ;                /* backup filename (might be NULL) */
} reml_collection_generic ;

/* is this collection saved to disk? */

#undef  RCG_SAVED
#define RCG_SAVED(rcg) \
  ( (rcg)->savfil != NULL && (rcg)->rs[(rcg)->izero]->cc == NULL )

/* setup the grid sizes from the powers of 2 */

#undef  POW2     /* 2^pp if pp > 0, otherwise 0 */
#define POW2(pp) ( ((pp) > 0) ? (1 << (pp)) : 0 )

#undef  RCG_setup
#define RCG_setup(rcg,pa,pb,pc,pd,pe,pf)          \
  do{ (rcg)->pna = (pa) ; (rcg)->na = POW2(pa) ;  \
      (rcg)->pnb = (pb) ; (rcg)->nb = POW2(pb) ;  \
      (rcg)->pnc = (pc) ; (rcg)->nc = POW2(pc) ;  \
      (rcg)->pnd = (pd) ; (rcg)->nd = POW2(pd) ;  \
      (rcg)->pne = (pe) ; (rcg)->ne = POW2(pe) ;  \
      (rcg)->pnf = (pf) ; (rcg)->nf = POW2(pf) ;  \
      (rcg)->na1     = (rcg)->na + 1 ;            \
      (rcg)->nb1     = (rcg)->nb + 1 ;            \
      (rcg)->nc1     = (rcg)->nc + 1 ;            \
      (rcg)->nd1     = (rcg)->nd + 1 ;            \
      (rcg)->ne1     = (rcg)->ne + 1 ;            \
      (rcg)->nf1     = (rcg)->nf + 1 ;            \
      (rcg)->nab1    = (rcg)->na1 * rcg->nb1 ;    \
      (rcg)->nabc1   = (rcg)->nab1 * rcg->nc1 ;   \
      (rcg)->nabcd1  = (rcg)->nabc1 * rcg->nd1 ;  \
      (rcg)->nabcde1 = (rcg)->nabcd1 * rcg->ne1 ; \
      (rcg)->nabcdef1= (rcg)->nabcde1 * rcg->nf1; \
  } while(0)

/* map 2D parameters to an index in a collection */

#undef  IAB
#define IAB(rcg,a,b)                                            \
     (              (int)rint( ((a)-(rcg)->abot) / (rcg)->da )  \
      +(rcg)->na1 * (int)rint( ((b)-(rcg)->bbot) / (rcg)->db ) )

/* map 4D parameters to an index in a collection */

#undef  IABCD
#define IABCD(rcg,a,b,c,d)                                        \
     (                (int)rint( ((a)-(rcg)->abot) / (rcg)->da )  \
      +(rcg)->na1   * (int)rint( ((b)-(rcg)->bbot) / (rcg)->db )  \
      +(rcg)->nab1  * (int)rint( ((c)-(rcg)->cbot) / (rcg)->dc )  \
      +(rcg)->nabc1 * (int)rint( ((d)-(rcg)->dbot) / (rcg)->dd ) )

/** correlation cutoff to control bandwidth of matrix **/
/** the value of corcut might be changed in 3dREMLfit **/

#define CORCUT_default 0.00002
static double corcut = CORCUT_default ;

/* pseudo-time of each index;
   correlation of data at index i with data  at index j
   is determined by TAU(i)-TAU(j);
   this allows for gaps in the data (e.g., censoring) */

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

   for( k=0 ; k < rows ; k++ ) c[k] = 0.0 ;  /* output vector */

   for( j=0 ; j < cols ; j++ ){
     cn = a->cnum[j] ;
     ci = a->cii[j] ; /* == NULL if(cn==rows) */
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
     ci = a->cii[i] ; /* == NULL if(cn==rows) */
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

   switch( cols%4 ){  /* unrolled for speed */
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

   switch( cols%4 ){  /* unrolled for speed */
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
/*! Solve [R] [x] = [b] for [x] where R is upper triangular full matrix */

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
/*! Solve [R]' [x] = [b] for [x] where R is upper triangular full matrix */

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
/* compute fraction of elements in a full matrix that are actually nonzero;
   useful to determine if it's worth converting it to sparse storage
*//*------------------------------------------------------------------------*/

double sparsity_fraction( matrix a )
{
   int rows=a.rows , cols=a.cols , i,j,k=0 ; double val ;
   for( j=0 ; j < cols ; j++ )
     for( i=0 ; i < rows ; i++ ) if( a.elts[i][j] != 0.0 ) k++ ;
   val = k / (double)(rows*cols) ; return val ;
}

/*--------------------------------------------------------------------------*/
/* Convert a full matrix to sparse format, for fun and PROFIT! */

sparmat * matrix_to_sparmat( matrix a )
{
   int rows=a.rows , cols=a.cols ;
   int i , j , k ;
   sparmat *sa ;

   sa = (sparmat *)remla_malloc(sizeof(sparmat)) ;

   sa->rows = rows ; sa->cols = cols ;
   sa->cnum = (int *)    remla_calloc(sizeof(int)     ,cols) ; /* count */
   sa->cii  = (int **)   remla_calloc(sizeof(int *)   ,cols) ; /* row indexes */
   sa->cee  = (double **)remla_calloc(sizeof(double *),cols) ; /* row data */

   for( j=0 ; j < cols ; j++ ){
     sa->cii[j] = (int *)   remla_malloc(sizeof(int)   *rows) ;
     sa->cee[j] = (double *)remla_malloc(sizeof(double)*rows) ;
     for( k=i=0 ; i < rows ; i++ ){        /* copy nonzeros into */
       if( a.elts[i][j] != 0.0 ){          /* sparse column vector */
         sa->cii[j][k] = i; sa->cee[j][k] = a.elts[i][j]; k++;
       }
     }
     sa->cnum[j] = k ;  /* number of nonzero elements in this column */

     if( k < (int)(0.7*rows+1) ){  /* column is sparse: truncate array sizes */
       sa->cii[j] = (int *)   remla_realloc( (void *)sa->cii[j] , sizeof(int)   *k ) ;
       sa->cee[j] = (double *)remla_realloc( (void *)sa->cee[j] , sizeof(double)*k ) ;

     } else if( k < rows ){        /* column is nearly full: re-create as full */
       remla_free(sa->cii[j]); sa->cii[j] = NULL; sa->cnum[j] = rows;
       for( i=0 ; i < rows ; i++ ) sa->cee[j][i] = a.elts[i][j] ;

     } else {                      /* column is completely full already */
       remla_free(sa->cii[j]); sa->cii[j] = NULL;
     }
   }
   return sa ;
}

/*--------------------------------------------------------------------------*/
/* Looks like meat's back on the menu, boys! */

void sparmat_destroy( sparmat *sa )
{
   int i , cols ;
   if( sa == NULL ) return ;
   cols = sa->cols ;
   for( i=0 ; i < cols ; i++ ){
     if( sa->cii[i] != NULL ) remla_free(sa->cii[i]) ;
     if( sa->cee[i] != NULL ) remla_free(sa->cee[i]) ;
   }
   remla_free(sa->cee);  remla_free(sa->cii);
   remla_free(sa->cnum); remla_free(sa);
   return ;
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

  /* matrix dimension */
   my_fwrite( &(rcm->nrc) , sizeof(int) , 1 , fp ) ;
  /* number of elements in each row */
   my_fwrite( rcm->len    , sizeof(LENTYP) , rcm->nrc , fp ) ;
  /* data from each row */
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

   fread( &nn , sizeof(int) , 1 , fp ) ;          /* number of rows */
   if( nn < 1 || nn > 999999 ) RETURN(NULL) ;     /* bad */
   qcm = rcmat_init(nn) ;                         /* make matrix */
   fread( qcm->len , sizeof(LENTYP) , nn , fp ) ; /* row lengths */
   for( ii=0 ; ii < nn ; ii++ ){                  /* data from each row */
     qcm->rc[ii] = remla_malloc( sizeof(double)*qcm->len[ii] ) ;
     fread( qcm->rc[ii] , sizeof(double) , qcm->len[ii] , fp ) ;
   }

   RETURN(qcm) ;
}

/*--------------------------------------------------------------------------*/
/*! Write a full matrix to a file. */

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
/*! Read a full matrix from a file. */

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
/*! Save the matrices in a reml_setup struct to a file (and erase them).
    The rest of the reml_setup struct is untouched.
*//*------------------------------------------------------------------------*/

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
/* Variable 'do_logqsumq' controls how the REML function uses the residuals.
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
    Note that this function is not dependent on the correlation model,
    since that was already used to setup the matrices in rset.

    If some input is invalid, the return value is really really big.

    [24 Jun 2009] Modified to use workspace vectors passed in via bbar[],
                  rather than static or malloc-ed vectors, for OpenMP's sake.
*//*------------------------------------------------------------------------*/

#undef  RFDEBUG   /* only enable this if USE_OMP is NOT defined */
#ifdef  RFDEBUG
static double raa,rbb,rvv ;
#endif

double REML_func( vector *y , reml_setup *rset , matrix *X , sparmat *Xs ,
                  double *bbar[7] , double *bbsumq )
{
   int n , ii ;
   double val=-666.0 ;
   double *bb1 , *bb2 , *bb3 , *bb4 , *bb5 , *bb6 , *bb7 ;
   double qsumq ;

ENTRY("REML_func") ;

   if( y == NULL || rset == NULL || X == NULL || bbar[0] == NULL ) RETURN(BIGVAL) ;

   /* assign pointers to workspace vectors */

   bb1 = bbar[0] ; bb2 = bbar[1] ; bb3 = bbar[2] ;
   bb4 = bbar[3] ; bb5 = bbar[4] ; bb6 = bbar[5] ; bb7 = bbar[6] ;

/* ININFO_message("   REML_func 1: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */

   n = rset->neq ;

   /** Seven matrix-vector steps to compute the prewhitened residuals **/

   for( ii=0 ; ii < n ; ii++ ) bb1[ii] = y->elts[ii] ;
   rcmat_lowert_solve( rset->cc , bb1 ) ;            /* bb1 = C^(-T) y */
                                                     /* prewhitened data */
/* ININFO_message("   REML_func 2: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   for( ii=0 ; ii < n ; ii++ ) bb2[ii] = bb1[ii] ;
   rcmat_uppert_solve( rset->cc , bb2 ) ;            /* bb2 = C^(-1) bb1 */

/* ININFO_message("   REML_func 3: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   if( Xs != NULL ){
     vector_spc_multiply_transpose( Xs , bb2, bb3 );
   } else {
     vector_full_multiply_transpose( X , bb2, bb3 ); /* bb3 = X^T bb2 */
   }

/* ININFO_message("   REML_func 4: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   vector_full_rrtran_solve( rset->dd , bb3 , bb4 ); /* bb4 = D^(-T) bb3 */

/* ININFO_message("   REML_func 5: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   vector_full_rr_solve(     rset->dd , bb4 , bb5 ); /* bb5 = D^(-1) bb4 */
                                                     /*     = beta_hat  */
/* ININFO_message("   REML_func 6: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   if( Xs != NULL ){
     vector_spc_multiply( Xs , bb5 , bb6 ) ;
   } else {
     vector_full_multiply( X , bb5 , bb6 ) ;         /* bb6 = X bb5 */
   }                                                 /* fitted model */

/* ININFO_message("   REML_func 7: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
   for( ii=0 ; ii < n ; ii++ ) bb7[ii] = bb6[ii] ;
   rcmat_lowert_solve( rset->cc , bb7 ) ;            /* bb7 = C^(-T) bb6 */
                                                     /* prewhitened fit */
/* ININFO_message("   REML_func 8: a = %g b = %g",rset->pvec[0],rset->pvec[1]) ; */
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

#ifdef  RFDEBUG
   raa = rset->pvec[0] ;
   rbb = rset->pvec[1] ;
   rvv = val ;
#endif

/* ININFO_message("   REML_func 9: a = %g b = %g val = %g",rset->pvec[0],rset->pvec[1],val) ; */
   RETURN(val) ;
}

/****************************************************************************/

/*==========================================================================*/
/* functions below used to destroy used-up data structures */
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

/*==========================================================================*/
/* Functions below to save/restore data structures to temp files,
   when the user needs to save RAM because they have a wimpy computer.
*//*========================================================================*/

/*----------------------------------------------------------------------------*/
/* Functions to save a list of the temporary REML_* files, so that they
   can be deleted at program rundown, if they weren't deleted already.
   If mainENTRY() was used in the calling program, then even a program crash
   might do cleanup, since the most common fatal signals are caught and will
   end up invoking exit(), which will in turn invoke remla_atexit().
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
     qpurge = (char **)remla_realloc(qpurge,sizeof(char *)*(++npurge)) ;
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
/* Assign a quasi-random filename into the user-supplied string space. */

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
/*! Save a reml_collection_generic struct to disk;
    actually, only the matrices in the reml_setup's are saved.
*//*------------------------------------------------------------------------*/

void reml_collection_generic_save( reml_collection_generic *rcg )
{
   int ii ;
   FILE *fp ;

ENTRY("reml_collection_generic_save") ;

   if( rcg == NULL || rcg->rs[rcg->izero] == NULL ) EXRETURN ;

   if( rcg->savfil != NULL ){
     kill_purge(rcg->savfil) ;
     remove(rcg->savfil) ; remla_free(rcg->savfil) ; rcg->savfil = NULL ;
   }

   reml_setup_savfilnam( &(rcg->savfil) ) ;

   fp = fopen( rcg->savfil , "wb" ) ;
   if( fp == NULL ){
     ERROR_message("3dREMLfit can't write to -usetemp file %s",rcg->savfil) ;
     remla_free(rcg->savfil) ; rcg->savfil = NULL ;
     EXRETURN ;
   }
   add_purge( rcg->savfil ) ;  /* add to list of files to be purged at exit */

   for( ii=0 ; ii < rcg->nset ; ii++ )
     if( rcg->rs[ii] != NULL ) reml_setup_savemat( fp , rcg->rs[ii] ) ;

   fclose(fp) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void reml_collection_generic_restore( reml_collection_generic *rcg )
{
   int ii ; FILE *fp ;

ENTRY("reml_collection_generic_restore") ;

   if( rcg == NULL || rcg->savfil == NULL ) EXRETURN ;

   fp = fopen( rcg->savfil , "rb" ) ;
   if( fp == NULL ){
     ERROR_message("3dREMLfit can't read from -usetemp file %s",rcg->savfil) ;
     remla_free(rcg->savfil) ; rcg->savfil = NULL ;
     EXRETURN ;
   }

   for( ii=0 ; ii < rcg->nset ; ii++ )
     if( rcg->rs[ii] != NULL ) reml_setup_restoremat( fp , rcg->rs[ii] ) ;

   fclose(fp) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Destroy a reml_collection_generic struct:
     - if zsave!=0, then it just
       destroys all but the izero component and leaves that behind;
     - if zsave==0, then it destroys and frees everything
*//*------------------------------------------------------------------------*/

void reml_collection_generic_destroy( reml_collection_generic *rcg , int zsave )
{
   int ii ;

   if( rcg == NULL ) return ;
   if( !zsave && rcg->X  != NULL ) matrix_destroy ( rcg->X  ) ;
   if( !zsave && rcg->Xs != NULL ) sparmat_destroy( rcg->Xs ) ;
   if( rcg->rs != NULL ){
     for( ii=0 ; ii < rcg->nset ; ii++ ){
       if( !(ii == rcg->izero && zsave) ){
         reml_setup_destroy(rcg->rs[ii]) ; rcg->rs[ii] = NULL ;
       }
     }
     if( !zsave ) remla_free((void *)rcg->rs) ;
   }
   if( rcg->savfil != NULL ){
     kill_purge(rcg->savfil) ;
     remove(rcg->savfil) ; remla_free(rcg->savfil) ; rcg->savfil = NULL ;
   }
   if( !zsave ) remla_free((void *)rcg) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/* Compute the matrix factors needed for REML-ized GLTs;
   see 3dREMLfit_mathnotes.pdf for details
*//*------------------------------------------------------------------------*/

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
/* Add a GLT from matrix G to one REML setup */
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
   rs->glt = (gltfactors **)remla_realloc( (void *)rs->glt ,
                                           sizeof(gltfactors *)*(ng+1) ) ;

   rs->glt[ng] = gf ; rs->nglt = ng+1 ; EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* Add a GLT from matrix G to all REML setups in a collection */
/*--------------------------------------------------------------------------*/

void REML_add_glt_to_all( reml_collection_generic *rcg , matrix *G )
{
   int kk ;

ENTRY("REML_add_glt_to_all") ;

   if( rcg == NULL || G == NULL ) EXRETURN ;

   for( kk=0 ; kk < rcg->nset ; kk++ ){
     REML_add_glt_to_one( rcg->rs[kk] , G ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* Compute a GLT F statistic from a REML setup,
   the data, and the complete set of betas
*//*------------------------------------------------------------------------*/

static vector *betaG = NULL ;  /* GLT combinations  */
static vector *betaT = NULL ;  /* GLT t-statistics  */
static double  betaR = 0.0  ;  /* GLT R^2 statistic */
static double  betaF = 0.0  ;  /* GLT F statistic   */

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
/*---------------------------------------------------------------------------*/
/**** Stuff for setting up more complicated 'generic' correlation models. ****/
/**** This code replaces the ARMA(1,1) or _arma11 specific code of old.   ****/

/*--------------------------------------------------------------------------*/
/* number of parameters from the noise model code
*//*------------------------------------------------------------------------*/

static int number_of_parameters( int cod )
{
  switch( cod ){
    case ARMA11_MODEL: return ARMA11_NPARAM ;
    case ARMA31_MODEL: return ARMA31_NPARAM ;
    case ARMA51_MODEL: return ARMA51_NPARAM ;
  }
  return 0 ; /* bad bad Leroy Brown, baddest code in the whole damn town  */
}

/*--------------------------------------------------------------------------*/
/* Copy parameter vector qvec into pvec, for a particular model.
*//*------------------------------------------------------------------------*/

void copy_generic_parameter_vector( int code, double *pvec, double *qvec )
{
   int np ;
   if( pvec == NULL || qvec == NULL ) return ;
   np = number_of_parameters( code ) ; if( np <= 0 ) return ;
   if( np > 6 ) np = 6 ;
   AA_memcpy( pvec , qvec , sizeof(double)*np ) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the temporal autocorrelations vector for some model given by
   'code', with model parameters in 'pvec'. Just calls some external
   functions (e.g., armacor.c) to get the vector of values.
*//*------------------------------------------------------------------------*/

doublevec * rcmat_generic_correlations( int code , double *pvec ,
                                        double ccut , int ncmax , void *ws )
{
   doublevec *corvec = NULL ;

   if( pvec == NULL ) return corvec ;

   switch( code ){

     case ARMA11_MODEL:
       if( ws == NULL ){
         corvec = arma11_correlations( pvec[0] , pvec[1] , ccut , ncmax ) ;
       } else {
         corvec = (doublevec *)ws ;
         arma11_correlations_inplace( pvec[0] , pvec[1] , ccut , corvec ) ;
       }
     break ;

     case ARMA31_MODEL:
       if( ws == NULL ){
         corvec = arma31_correlations( pvec[0] , pvec[1] , pvec[2] , pvec[3] ,
                                       ccut , ncmax ) ;
       } else {
         corvec = (doublevec *)ws ;
         arma31_correlations_inplace( pvec[0] , pvec[1] , pvec[2] , pvec[3] ,
                                      ccut , corvec ) ;
       }
     break ;

   }

   return corvec ;
}

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct) for a
    general shift-invariate structure, whose lagged correlations are given
    in corvec.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps. It is
      mandatory that tau[i] > tau[j] for i > j.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
    * The rcmat struct is lower triangular (or upper triangular), as it
      represents a symmetric matrix.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_generic( int nt, int *tau, int code, double *pvec, void *ws )
{
   rcmat  *rcm ;
   LENTYP *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;
   doublevec *corvec ;

ENTRY("rcmat_generic") ;

   if( nt < 2 || pvec == NULL ) RETURN( NULL ) ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

   /* get correlations (at most nt of them) */

   corvec = rcmat_generic_correlations( code , pvec , corcut , nt , ws ) ;

   if( corvec == NULL || corvec->nar <= 0 ){
     WARNING_message("rcmat_generic: pvec = %g %g ==> NULL",pvec[0],pvec[1]) ;
     RETURN( NULL ) ;
   }

   /* set maximum bandwidth from actual number of correlations we got */

   bmax = corvec->kk - 1 ;  /* we have lags 0 .. bmax */

   if( bmax < 0 ){          /* should be impossible -- SHOULD be */
     WARNING_message("rcmat_generic: pvec = %g %g ==> NULLity",pvec[0],pvec[1]) ;
     RETURN( NULL ) ;
   }

/* INFO_message("rcmat_generic: bmax=%d pvec = %g %g",bmax,pvec[0],pvec[1]) ; */

   /* special and trivial case: identity matrix (only got correlation [0]) */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     if( ws == NULL ) KILL_doublevec( corvec ) ;
     RETURN( rcm ) ;
   }

   /* First row/column always has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = remla_malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;    /* earliest allowed index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwith */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){       /* a purely diagonal row/colum (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = remla_malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = remla_calloc(sizeof(MTYPE),len[ii]) ;    /* space for entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
       rii[jj] = corvec->ar[jtt] ;        /* extract correlation from corvec */
     }
   }

   /* check for errors that should never happen */
   ii = rcmat_floatscan(rcm) ;
   if( ii > 0 ) ERROR_message("%d errors found in rcmat_generic matrix",ii);

   if( ws == NULL ) KILL_doublevec( corvec ) ;
   RETURN( rcm ) ;
}

/*--------------------------------------------------------------------------*/
/*! Create the struct for REML calculations for a generic model,
    and for a particular regression matrix X.
*//*------------------------------------------------------------------------*/

reml_setup * setup_generic_reml( int nt, int *tau,
                                 int code, double *pvec, matrix *X, void *ws )
{
   int ii , jj , mm ;
   reml_setup *rset ;
   rcmat *rcm ;
   matrix *W , *D ;
   double *vec , csum,dsum,val ;

   if( nt < 2 || X == NULL || X->rows != nt || pvec == NULL ){
     ERROR_message("setup_generic_reml: bad inputs?!") ;
     return NULL ;
   }

   mm = X->cols ;  /* number of regression parameters */
   if( mm >= nt || mm <= 0 ){
     ERROR_message("setup_generic_reml: bad inputs?!!") ;
     return NULL ;
   }

   /* Form R = sparse correlation matrix */

   rcm = rcmat_generic( nt , tau , code , pvec , ws ) ;
   if( rcm == NULL ){
     ERROR_message("rcmat_generic fails!?") ;
     return NULL ;
   }

   /* form C = Choleski factor of R (in place) */

   ii = rcmat_choleski( rcm ) ;
   if( ii != 0 ){
     ERROR_message("rcmat_choleski fails with code=%d for model %d :(",ii,code) ;
     rcmat_destroy(rcm);
#if 0
     rcm = rcmat_generic( nt , tau , code , pvec , ws ) ;
     rcmat_to_stderr( rcm , "before Choleski") ;
     rcmat_destroy(rcm);
#endif
     return NULL;
   }

   /* check for errors that should never happen */
   ii = rcmat_floatscan(rcm) ;
   if( ii > 0 ) ERROR_message("%d errors found in setup_generic_reml C matrix",ii);

   /* prewhiten each column of X into W matrix */
   /* that is, W = inv(C) X */

   W = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(W) ;
   matrix_create(nt,mm,W) ;             /* this is a full matrix, not sparse */
   vec = (double *)remla_malloc(sizeof(double)*nt) ;  /* temp vector = 1 col */

   for( jj=0 ; jj < X->cols ; jj++ ){
     for( ii=0 ; ii < nt ; ii++ ) vec[ii] = X->elts[ii][jj] ; /* extract col */
     rcmat_lowert_solve( rcm , vec ) ;                          /* prewhiten */
     for( ii=0 ; ii < nt ; ii++ ) W->elts[ii][jj] = vec[ii] ;  /* put into W */
   }

   ii = matrix_floatscan( W ) ;
   if( ii > 0 ) ERROR_message("%d errors found in setup_generic_reml W matrix",ii) ;

   remla_free((void *)vec) ;

   /* compute QR decomposition of W, save R factor into D, toss W */

   D = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(D) ;
   ii = matrix_qrr( *W , D ) ;
   matrix_destroy(W) ; remla_free((void *)W) ;
   if( D->rows <= 0 ){
     ERROR_message("matrix_qrr fails?!") ;
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

   ii = matrix_floatscan( D ) ;
   if( ii > 0 ) ERROR_message("%d errors found in setup_generic_reml D matrix",ii) ;

   /* create the setup struct, save stuff into it */

   rset = (reml_setup *)remla_malloc(sizeof(reml_setup)) ;
   rset->neq         = nt ;     /* number of time points */
   rset->mreg        = mm ;     /* number of regressors */
   rset->cc          = rcm ;    /* Choleski factor of R matrix */
   rset->dd          = D ;      /* R factor of W matrix */
   rset->noise_model = code ;
   copy_generic_parameter_vector( code , rset->pvec , pvec ) ;

/* ININFO_message("  save pvec = %g %g",rset->pvec[0],rset->pvec[1]) ; */

   /* save the lambda parameter - just for fun */

   if( code == ARMA11_MODEL ) pvec[2] = LAMBDA( pvec[0] , pvec[1] ) ;

   rset->nglt = 0 ;             /* no GLTS attached yet */
   rset->glt  = NULL ;

   /* compute 2 * log det[D] = part of REML cost function */

   for( dsum=0.0,ii=0 ; ii < mm ; ii++ ){
     val = D->elts[ii][ii] ;
     if( val > 0.0 ) dsum += log(val) ;
   }
   rset->dd_logdet = 2.0 * dsum ;

   /* and 2 * log det[C] = part of REML cost function */

   for( csum=0.0,ii=0 ; ii < nt ; ii++ ){
     jj  = rcm->len[ii] ;
     val = rcm->rc[ii][jj-1] ;
     if( val > 0.0 ) csum += log(val) ;
   }
   rset->cc_logdet = 2.0 * csum ;

   return rset ;
}

/*--------------------------------------------------------------------------*/
/* A convenience function, that's all */
/*--------------------------------------------------------------------------*/

reml_setup * REML_setup_one_generic( matrix *X, int *tau, int code, double *pvec, void *ws )
{
   reml_setup *rset ;
   rset = setup_generic_reml( X->rows , tau , code , pvec , X , ws ) ;
   return rset ;
}

/*--------------------------------------------------------------------------*/
/* convenience function for ARMA(1,1) and 3dREMLfit */
/*--------------------------------------------------------------------------*/

reml_setup * REML_setup_one_arma11( matrix *X, int *tau, double aa, double bb )
{
   double pvec[6] ;
   pvec[0] = aa ; pvec[1] = bb ;
   return REML_setup_one_generic( X , tau , ARMA11_MODEL , pvec, NULL ) ;
}

/*--------------------------------------------------------------------------*/
/* Similar to above, BUT
     * first add columns in Z to matrix
     * then carry out the REML setup for this new matrix
   This is to allow for voxel-wise regressors in 3dREMLfit,
   once the 'optimal' pvec parameters are known.
*//*------------------------------------------------------------------------*/

reml_setup_plus * REML_setup_plus_generic( matrix *X, matrix *Z, int *tau,
                                           int code , double *pvec        )
{
   reml_setup_plus *rsetplus=NULL ;
   matrix *XZ=NULL ;
   double spcut ;

ENTRY("REML_setup_plus_generic") ;

   if( X == NULL || pvec == NULL ) RETURN(rsetplus) ; /* bad */

   /* create the catenated matrix [ X Z ] */

   XZ = (matrix *)remla_malloc(sizeof(matrix)) ;

   if( Z == NULL || Z->cols == 0 ){   /* trivial case of no Z at all */

     matrix_initialize(XZ) ;
     matrix_equate(*X,XZ) ;

   } else {                           /* Oh god, have to do some work */
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

   rsetplus->rset = REML_setup_one_generic( XZ , tau , code , pvec , NULL ) ;

   if( rsetplus->rset == NULL ){        /* bad bad bad */
     remla_free(rsetplus) ; matrix_destroy(XZ) ; remla_free(XZ) ; RETURN(NULL) ;
   }

   rsetplus->X = XZ ;  /* store matrix, then maybe its sparsification */

   spcut = AFNI_numenv("AFNI_REML_SPARSITY_THRESHOLD") ;
   if( spcut <= 0.0 ) spcut = 1.01 ;  /* always use sparmat if not forbidden */

   if( sparsity_fraction(*XZ) <= spcut ) rsetplus->Xs = matrix_to_sparmat(*XZ) ;
   else                                  rsetplus->Xs = NULL ;

   RETURN(rsetplus) ;
}

/*--------------------------------------------------------------------------*/
/* convenience function for ARMA(1,1) */
/*--------------------------------------------------------------------------*/

reml_setup_plus * REML_setup_plus_arma11( matrix *X, matrix *Z, int *tau,
                                          double aa , double bb          )
{
   double pvec[6] ;
   pvec[0] = aa ; pvec[1] = bb ;
   return REML_setup_plus_generic( X, Z, tau, ARMA11_MODEL , pvec ) ;
}

/*--------------------------------------------------------------------------*/
/* Create a blank (protean) collection of REML setups,
   with regression matrix pointed to by X. Must be filled in later.
*//*------------------------------------------------------------------------*/

reml_collection_generic * REML_initialize_collection( matrix *X )
{
   reml_collection_generic *rcg = NULL ;
   double lam , spcut ;

   if( X == NULL ){    /* who the hell called me like this? */
     ERROR_message("REML_initialize_colleciton: input matrix is NULL :(") ;
     return rcg ;
   }

   /* create collection struct */

   rcg = (reml_collection_generic *)remla_calloc(sizeof(reml_collection_generic),1) ;

   /* copy matrix into struct */

   rcg->X = (matrix *)remla_malloc(sizeof(matrix)) ; matrix_initialize(rcg->X) ;
   matrix_equate( *X , rcg->X ) ;

   /* check for sparsity threshold,
      perhaps to store matrix in sparse form as well as full form (above) */

   spcut = AFNI_numenv("AFNI_REML_SPARSITY_THRESHOLD") ;

   if( spcut <= 0.0 ) spcut = 1.01 ;  /* default is always use sparmat */

   lam = sparsity_fraction( *X ) ;
   if( lam <= spcut ) rcg->Xs = matrix_to_sparmat( *X ) ;
   else               rcg->Xs = NULL ;

   /* initialize various constants to null values */

   rcg->na  = rcg->nb  = rcg->nc  = rcg->nd  = rcg->ne  = rcg->nf  = 0 ;
   rcg->pna = rcg->pnb = rcg->pnc = rcg->pnd = rcg->pne = rcg->pnf = 0 ;
   rcg->abot= rcg->bbot= rcg->cbot= rcg->dbot= rcg->ebot= rcg->fbot= 0.0 ;
   rcg->atop= rcg->btop= rcg->ctop= rcg->dtop= rcg->etop= rcg->ftop= 0.0 ;
   rcg->azer= rcg->bzer= rcg->czer= rcg->dzer= rcg->ezer= rcg->fzer= 0.0 ;
   rcg->da  = rcg->db  = rcg->dc  = rcg->dd  = rcg->de  = rcg->df  = 0.0 ;

   return rcg ;
}

/*--------------------------------------------------------------------------*/
/* Fill in a 2D grid of REML setups, over a range of parameters.
   Parameter (a,b) grid dimensions are 0..2^pna X 0..2^pnb.
   Special case: pna==pnb==0 => only setup two cases (istwo):
     [0] = (azer,bzer)   [1] = (amin,bmin)
*//*------------------------------------------------------------------------*/

reml_collection_generic * REML_setup_all_generic_2D(
                             matrix *X , int *tau , int model_code ,
                             int pna , int pnb ,
                             double amin , double amax ,
                             double bmin , double bmax ,
                             double azer , double bzer  )
{
   int ii,jj,kk , nt , ndone=0 , na,nb , istwo ;
   double aa,bb ;
   reml_collection_generic *rcg=NULL ;
   float avglen=0.0f ;
   double pvec[6] ;   /* for loading parameters (a,b) */

   /* check for stoopiditeeze */

#ifdef RFDEBUG
INFO_message("REML_setup_all_generic_2D:\n"
             "  model_code = %d\n"
             "  pna = %d pnb = %d\n"
             "  amin = %g amax = %g azer = %g\n"
             "  bmin = %g bmax = %g bzer = %g\n" ,
             model_code , pna,pnb , amin,amax,azer , bmin,bmax,bzer ) ;
#endif

   if( X == NULL ){
     ERROR_message("REML_setup_all_generic_2D: input matrix is NULL") ;
     return rcg ;
   }

   if( number_of_parameters(model_code) != 2 ){
     ERROR_message("REML_setup_all_generic_2D: input model_code=%d is illegal here",model_code) ;
     return rcg ;
   }

   /* no grid to use? */

   istwo = (pna == 0 && pnb == 0 ) ;  /* will set up just 2 cases */

   /* check if grid layout makes sense */

   if( !istwo &&
       ( (amin >= amax) || (azer < amin) || (azer > amax) ||
         (bmin >= bmax) || (bzer < bmin) || (bzer > bmax)   ) ){
     ERROR_message("REML_setup_all_generic_2D: (a,b) parameter ranges disordered\n"
                   "     amin = %g  azer = %g  amax = %g\n"
                   "     bmin = %g  bzer = %g  bmax = %g"  ,
                   amin,azer,amax , bmin,bzer,bmax ) ;
     return rcg ;
   }

   /* check number of time points */

   nt = X->rows ;
   if( nt < 9 ){     /* just medium stoopid */
     ERROR_message(
       "REML_setup_all_generic_2D: number of time points %d is less than 9 -- not allowed!",nt) ;
     return rcg ;
   }

   /* set grid size in a and b parameters (powers of two) */

   if( !istwo ){
     if( pna < 4 ) pna = 4 ; else if( pna > 8 ) pna = 8 ;
     if( pnb < 4 ) pnb = 4 ; else if( pnb > 8 ) pnb = 8 ;
   }

   /* create nearly empty collection */

   rcg = REML_initialize_collection( X ) ;

   rcg->noise_model = model_code ;
   rcg->pnum        = 2 ;

   RCG_setup(rcg,pna,pnb,0,0,0,0) ; /* setup dimensional factors for grid */

   na = rcg->na ; nb = rcg->nb ;       /* local copies of grid dimensions */

   /* get ready for array of REML setups */

   if( !istwo ){
     rcg->nset  = rcg->nab1 ;   /* number of setups */
     rcg->istwo = 0 ;
     rcg->abot  = amin; rcg->atop = amax; rcg->da = (amax-amin)/na;   /* grid */
     rcg->bbot  = bmin; rcg->btop = bmax; rcg->db = (bmax-bmin)/nb; /* coords */
     /* might have to adjust (azer,bzer) to fall on a grid point */
     kk = IAB(rcg,azer,bzer) ;
     ii = kk % rcg->na1 ; rcg->azer = azer = rcg->abot + ii*rcg->da ;
     jj = kk / rcg->na1 ; rcg->bzer = bzer = rcg->bbot + jj*rcg->db ;
   } else {
     rcg->nset  = 2 ;
     rcg->istwo = 2 ;
     rcg->abot  = amin; rcg->atop = amin; rcg->azer = azer; rcg->da = 0.0 ;
     rcg->bbot  = bmin; rcg->btop = amin; rcg->bzer = bzer; rcg->db = 0.0 ;
   }

#ifdef RFDEBUG
ININFO_message("  nset = %d  da=%g db=%g  na=%d nb=%d",rcg->nset,rcg->da,rcg->db,na,nb) ;
#endif

   /* calloc-ate REML matrix setups:
       all are NULL to start, and if a matrix setup (rs) element
       is never computed, then it will be skipped later
       in the optimizing function REML_find_best_case_2D() */

   rcg->rs = (reml_setup **)remla_calloc(sizeof(reml_setup *),rcg->nset) ;

   /** set up the (a,b)='zero' case **/

   if( !istwo ){
     rcg->izero = kk = IAB(rcg,azer,bzer) ;  /* set index for 'zero' case */
   } else {
     rcg->izero = kk = 0 ;      /* 'zero' case is first, then second case */
   }

#ifdef RFDEBUG
INFO_message("setup izero=%d  a=%g b=%g",kk,azer,bzer) ;
#endif
   LOAD_pvec2(azer,bzer) ;
   rcg->rs[kk] = REML_setup_one_generic( X, tau, model_code, pvec , NULL ) ;

   if( rcg->rs[kk] != NULL ){
     ndone++ ;
     avglen += rcmat_avglen( rcg->rs[kk]->cc ) ; /* keep track of average row length */
   } else {
     ERROR_message("REML_setup_all_generic_2D: failure to setup model a=%g b=%g",azer,bzer) ;
   }

   if( istwo ){  /* special setup with just 2 (a,b) cases */

     kk = ndone ;
     LOAD_pvec2(amin,bmin) ;
     rcg->rs[kk] = REML_setup_one_generic( X, tau, model_code, pvec , NULL ) ;
     if( rcg->rs[kk] != NULL ){
       ndone++ ;
       avglen += rcmat_avglen( rcg->rs[kk]->cc ) ;
     } else {
       ERROR_message("REML_setup_all_generic_2D: failure to setup model a=%g b=%g",amin,bmin) ;
     }

   } else {      /* general setup case with an (a,b) grid of cases */

   /** variables for use in multi-threaded code below **/

#ifdef USE_OMP
    int nthr,qt , *ndon_th ; float *avg_th ;
    void **ws ; doublevec *dv ;
    nthr    = omp_get_max_threads() ;                     /* number of threads */
    ndon_th = (int   *)remla_calloc(sizeof(int)  ,nthr) ; /* how many per thread */
    avg_th  = (float *)remla_calloc(sizeof(float),nthr) ; /* row lengths per thread */
    ws      = (void **)remla_calloc(sizeof(void*),nthr) ;
    for( qt=0 ; qt < nthr ; qt++ ){
      MAKE_doublevec(dv,nt) ; ws[qt] = (void *)dv ; /* make per-thread workspaces */
    }
    if( verb && nthr > 1 ){
      ININFO_message("starting %d OpenMP threads for REML 2D setup",nthr) ;
    }
# define WS(q) ws[q]   /* pre-allocated workspace for thread #q */
#else
# define WS(q) NULL    /* no pre-allocated workspace when no threads */
#endif

    /** start of potentially multi-threaded code **/

 AFNI_OMP_START ;
#pragma omp parallel if( maxthr > 1 && rcg->nset > 7 )
 { int iab , ii,jj,kk , ithr=0 ;
   int na = rcg->na, na1 = rcg->na1, nb = rcg->nb, nab = rcg->nset ;
   double abot = rcg->abot, da = rcg->da ;
   double bbot = rcg->bbot, db = rcg->db ;
   double bb,aa, lam , pvec[6] ; float avg ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ; /* for saving things on a per-thread basis */
#ifdef ENABLE_REMLA_MALLOC
   if( ithr == 0 && verb > 1 ) fprintf(stderr,"nset=%d ",rcg->nset) ;
#endif
#endif
#pragma omp for
     for( iab=0 ; iab < nab ; iab++ ){ /* loop over (aa,bb) pairs */
       ii = iab % na1 ;     /* index in a-direction */
       jj = iab / na1 ;     /* index in b-direction */
       aa = ii*da + abot ;  /* actual value of a parameter */
       bb = jj*db + bbot ;  /* actual value of b parameter */
       kk = ii + na1*jj ;   /* should == iab */
       if( rcg->rs[kk] == NULL ){
/* ININFO_message("setup kk=%d  a=%g b=%g",kk,aa,bb) ; */
         LOAD_pvec2(aa,bb) ;
         rcg->rs[kk] = REML_setup_one_generic( X, tau, model_code, pvec, WS(ithr) ) ;
         if( rcg->rs[kk] != NULL ){
           avg = rcmat_avglen( rcg->rs[kk]->cc ) ;
#ifdef USE_OMP
           ndon_th[ithr]++ ; avg_th[ithr] += avg ;  /* per thread counting */
#ifdef ENABLE_REMLA_MALLOC
           if( ithr == 0 && verb > 1 ){
             fprintf(stderr," [%d]",kk) ;
             if( kk%20 == 0 ) REMLA_memprint ;
           }
#endif
#else
           ndone++ ; avglen += avg ; /* overall counting - not multi-threaded */
#endif
         }
#if 1
       else { ININFO_message("setup kk=%d  a=%g b=%g (lam=%g) FAILS",kk,aa,bb,LAMBDA(aa,bb)) ; }
#endif
       }
     } /* end loop over (aa,bb) pairs */

 } /* end OpenMP section */
 AFNI_OMP_END ;

#ifdef USE_OMP
   for( ii=0 ; ii < nthr ; ii++ ){
     ndone += ndon_th[ii] ; avglen += avg_th[ii] ; /* combine counts */
     dv = (doublevec *)ws[ii] ; KILL_doublevec(dv) ;
   }
   remla_free(avg_th) ; remla_free(ndon_th) ; remla_free(ws) ;
#endif

   } /* end general 2D setup */

   /* fix a couple of details and vamoose */

   rcg->ndone  = ndone ;        /* how many actually stored in grid */
   rcg->avglen = avglen/ndone ; /* average of average row lengths */
   rcg->savfil = NULL ;
   return rcg ;
}

/*--------------------------------------------------------------------------*/
/* For ARMA(1,1) -- full grid */
/*--------------------------------------------------------------------------*/

reml_collection_generic * REML_setup_all_arma11(
                             matrix *X , int *tau , int nlev ,
                             double amax , double bmax        )
{
   return
     REML_setup_all_generic_2D( X , tau , ARMA11_MODEL ,
                                nlev , nlev , /* positive == 2D grid in a,b */
                                -amax,amax , -bmax,bmax , 0.0,0.0 ) ;
}

/*--------------------------------------------------------------------------*/
/* For ARMA(1,1) -- just do the fixed (a,b) pair and (0,0) */
/*--------------------------------------------------------------------------*/

reml_collection_generic * REML_setup_fixed_arma11(
                             matrix *X, int *tau, double afix, double bfix )
{
   return
     REML_setup_all_generic_2D( X , tau , ARMA11_MODEL ,
                                0 , 0 ,  /* 0,0 == flag for 'istwo' */
                                afix,afix , bfix,bfix , 0.0,0.0 ) ;
}

/*--------------------------------------------------------------------------*/
/* Inputs: y=data vector, rcg=collection of REML setup stuff.
           nws = size of workspace array ws (0 => use malloc)
   Output: index of best case in the REML setup stuff.
           if ws!=NULL, then ws[0] = the best (smallest) REML score
*//*------------------------------------------------------------------------*/

int REML_find_best_case_generic_2D(
                vector *y, reml_collection_generic *rcg, int nws, double *ws )
{
   double rbest , rval ;
   int   na,nb , pna,pnb , ltop , mm , na1,nb1 ;
   int   nab, lev, dab, ia,jb,kk, ibot,itop, jbot,jtop, ibest,jbest,kbest ;
   int   klist[1024] , nkl , needed_ws,bb_ws=0,rv_ws=0 ;
   double *bbar[7] , *rvab ;

ENTRY("REML_find_best_case_generic_2D") ;

   if( y == NULL ) RETURN(-666) ; /* negative return = error message */

   mm = vector_floatscan( y ) ;
   if( mm > 0 ) ERROR_message("%d float errors in REML_find_best_case_generic_2D y input",mm) ;

   /* copy (a,b) grid parameters to local variables */

   na = rcg->na ; pna = rcg->pna ; na1 = rcg->na1 ;
   nb = rcg->nb ; pnb = rcg->pnb ; nb1 = rcg->nb1 ;

   /* make workspace arrays for REML_func() */

   bb_ws     = 2*y->dim + 16 ;
   rv_ws     = na1*nb1  + 16 ;
   needed_ws = 7*bb_ws  + rv_ws ;  /* 7 vectors plus 1 number per grid pt */

   if( nws >= needed_ws && ws != NULL ){  /* user provided workspace */
     rvab    = ws+1 ;
     bbar[0] = rvab + rv_ws ;
     for( ia=1 ; ia < 7 ; ia++ ) bbar[ia] = bbar[ia-1] + bb_ws ;
     bb_ws = rv_ws = 0 ;
   } else {                               /* make up our own workspace */
     rvab = (double *)remla_malloc(sizeof(double)*rv_ws) ;
     for( ia=0 ; ia < 7 ; ia++ )
       bbar[ia] = (double *)remla_malloc(sizeof(double)*bb_ws) ;
   }

   /** do the 'zero' case, mark it as best so far **/

   kbest = rcg->izero ;
   jbest = kbest / na1 ;
   ibest = kbest % na1 ;

#ifdef  RFDEBUG
INFO_message("init at izero=%d  ia=%d  jb=%d  a=%g b=%g",
              kbest , ibest , jbest ,
              rcg->rs[kbest]->pvec[0],rcg->rs[kbest]->pvec[1] ) ;
#endif

   /* first evaluation of REML cost function for this data (y) */

   rbest = REML_func( y, rcg->rs[kbest], rcg->X,rcg->Xs, bbar,NULL ) ;

#ifdef  RFDEBUG
ININFO_message("   rbest=%g at izero=%d a=%g b=%g",rbest,kbest,
               rcg->rs[kbest]->pvec[0],rcg->rs[kbest]->pvec[1] ) ;
ININFO_message("   vs raa=%g rbb=%g rvv=%g",raa,rbb,rvv) ;
#endif

   /* initialize all REML output values to be HUGE, except for the first */

   for( kk=0 ; kk < na1*nb1 ; kk++ ) rvab[kk] = BIGVAL ;
   rvab[kbest] = rbest ;

   /** do power-of-2 descent through the (a,b) grid to find the bestest pair **/

   nab  = MIN(pna,pnb)  ; ltop = nab-2 ;
   ibot = 0 ; itop = na ;                 /* scan from ibot..itop, jbot..jtop */
   jbot = 0 ; jtop = nb ;                          /* in steps of dab (below) */

   for( lev=ltop ; lev >= 0 ; lev-- ){     /* lev = power-of-2 grid scan size */
     dab = 1 << lev ;                      /* dab = 2^lev = step size in scan */

     /* make list of not-yet visited (a,b) grid
        points to visit in the REML_func loop below */

     for( nkl=0,jb=jbot ; jb <= jtop ; jb+=dab ){
       for( ia=ibot ; ia <= itop ; ia+=dab ){
         kk = ia + jb*na1 ; if( rvab[kk] < BIGVAL ) continue;  /* done before */
         if( rcg->rs[kk] == NULL ) continue ;           /* invalid grid point */
         klist[nkl++] = kk ;
     }}
     if( nkl == 0 ) continue ; /* should never happen */

     /* The reason the loop above is separate is to make the loop below
        be 1D rather than 2D, hoping that it would optimize better.
        However, this didn't work out, but the code is left the way it is.
        The goal of this loop is to fill in any gaps in the rvab[] array,
        the list of REML cost function values at the (a,b) parameter pairs. */

     for( mm=0 ; mm < nkl ; mm++ ){  /* this usually takes a lot of CPU time */
       kk = klist[mm] ;
       rvab[kk] = REML_func( y, rcg->rs[kk], rcg->X,rcg->Xs , bbar,NULL ) ;
     }

     /* find the best one so far seen, in the patch we are looking at */

     for( jb=jbot ; jb <= jtop ; jb+=dab ){
       for( ia=ibot ; ia <= itop ; ia+=dab ){
         kk = ia + jb*na1 ; rval = rvab[kk] ;
         if( rval < rbest ){                          /* the best so far seen */
           rbest = rval ; kbest = kk ; ibest = ia ; jbest = jb ;
#ifdef  RFDEBUG
ININFO_message("new rbest=%g at kk=%d a=%g b=%g",rbest,kbest ,
               rcg->rs[kbest]->pvec[0],rcg->rs[kbest]->pvec[1] ) ;
#endif
         }
     }}

     /* at the next level down, scan near the best so far seen */

     if( lev > 0 ){           /* if lev == 0, there is no next level */

       kk   = dab + dab/2 ;   /* The '+dab/2' expands the search at */
                              /* the next level, costing CPU time, */
                              /* but perhaps improving accuracy.  */
                              /* Original code didn't add dab/2. */
                              /* Note that at the next level,   */
                              /* dab will be halved => kk must */
                              /* a multiple of dab/2 here!    */
       ibot = ibest-kk ; if( ibot < 0  ) ibot = 0  ;
       itop = ibest+kk ; if( itop > na ) itop = na ;
       jbot = jbest-kk ; if( jbot < 0  ) jbot = 0  ;
       jtop = jbest+kk ; if( jtop > nb ) jtop = nb ;
     }

   } /* end of scan descent through different levels */

   /* delete workspace if it was allocated inside here */

   if( bb_ws ){
     remla_free(rvab) ;
     for( ia=0 ; ia < 7 ; ia++ ) remla_free(bbar[ia]) ;
   }

   /* save the smallest REML value found */

   if( ws != NULL ) ws[0] = rbest ;

#ifdef  RFDEBUG
ININFO_message("final rbest=%g at kk=%d a=%g b=%g",rbest,kbest ,
               rcg->rs[kbest]->pvec[0],rcg->rs[kbest]->pvec[1] ) ;
#endif

   /*** deal with the winner ***/

   RETURN(kbest) ;
}

/*--------------------------------------------------------------------------*/
/* Fill in a 4D grid of REML setups, over a range of parameters.
   Parameter (a,b,c,d) grid dimensions are
     0..2^pna X 0..2^pnb X 0..2^pnc X 0..2^pnd
*//*------------------------------------------------------------------------*/

reml_collection_generic * REML_setup_all_generic_4D(
                             matrix *X , int *tau , int model_code ,
                             int pna , int pnb , int pnc , int pnd ,
                             double amin , double amax ,
                             double bmin , double bmax ,
                             double cmin , double cmax ,
                             double dmin , double dmax  )
{
   int ii,jj,kk , nt , ndone=0 , na,nb,nc,nd ;
   double aa,bb,cc,dd ;
   reml_collection_generic *rcg=NULL ;
   float avglen=0.0f ;
   double pvec[6] ;   /* for loading parameters (a,b,c,d) */

   /* check for stoopiditeeze */

#ifdef RFDEBUG
INFO_message("REML_setup_all_generic_4D:\n"
             "  model_code = %d\n"
             "  pna = %d pnb = %d pnc = %d pnd = %d\n"
             "  amin = %g amax = %g   bmin = %g bmax = %g\n" 
             "  cmin = %g cmax = %g   dmin = %g dmax = %g"   ,
             model_code , pna,pnb,pnc,pnd ,
                          amin,amax, bmin,bmax, cmin,cmax, dmin,dmax  ) ;
#endif

   if( X == NULL ){
     ERROR_message("REML_setup_all_generic_4D: input matrix is NULL") ;
     return rcg ;
   }

   if( number_of_parameters(model_code) != 4 ){
     ERROR_message("REML_setup_all_generic_4D: input model_code=%d is illegal here",model_code) ;
     return rcg ;
   }

   /* check if grid layout makes sense */

   if( (amin >= amax) || (bmin >= bmax) ||
       (cmin >= cmax) || (dmin >= dmax)   ){
     ERROR_message("REML_setup_all_generic_4D: (a,b,c,d) parameter ranges disordered\n"
                   "     amin = %g amax = %g\n"
                   "     bmin = %g bmax = %g\n"
                   "     cmin = %g cmax = %g\n"
                   "     dmin = %g dmax = %g"  ,
                   amin,amax , bmin,bmax , cmin,cmax , dmin,dmax ) ;
     return rcg ;
   }

   /* check number of time points */

   nt = X->rows ;
   if( nt < 9 ){     /* just medium stoopid */
     ERROR_message(
       "REML_setup_all_generic_4D: number of time points %d is less than 9 -- not allowed!",nt) ;
     return rcg ;
   }

   /* set grid size in a and b parameters (powers of two) */

   if( pna < 3 ) pna = 3 ; else if( pna > 5 ) pna = 5 ;  /* limitations */
   if( pnb < 3 ) pnb = 3 ; else if( pnb > 5 ) pnb = 5 ;
   if( pnc < 3 ) pnc = 3 ; else if( pnc > 5 ) pnc = 5 ;
   if( pnd < 3 ) pnd = 3 ; else if( pnd > 5 ) pnd = 5 ;

   /* create nearly empty collection */

   rcg = REML_initialize_collection( X ) ;

   rcg->noise_model = model_code ;
   rcg->pnum        = 4 ;

   RCG_setup(rcg,pna,pnb,pnc,pnd,0,0) ; /* setup dimensional factors for grid */

   na = rcg->na ; nb = rcg->nb ;       /* local copies of grid dimensions */
   nc = rcg->nc ; nd = rcg->nd ;

   /* get ready for array of REML setups */

   rcg->nset  = rcg->nabcd1 ;   /* number of setups */
   rcg->istwo = 0 ;
   rcg->abot  = amin; rcg->atop = amax; rcg->da = (amax-amin)/na;   /* grid */
   rcg->bbot  = bmin; rcg->btop = bmax; rcg->db = (bmax-bmin)/nb; /* coords */
   rcg->cbot  = cmin; rcg->ctop = cmax; rcg->dc = (cmax-cmin)/nc;
   rcg->dbot  = dmin; rcg->dtop = dmax; rcg->dd = (dmax-dmin)/nd;

   /* calloc-ate REML matrix setups:
       all are NULL to start, and if a matrix setup (rs) element
       is never computed, then it will be skipped later
       in the optimizing function REML_find_best_case_2D() */

   rcg->rs = (reml_setup **)remla_calloc(sizeof(reml_setup *),rcg->nset) ;

   { /* general setup case with an (a,b,c,d) grid of cases */

   /** variables for use in multi-threaded code below **/

#ifdef USE_OMP
    int nthr,qt , *ndon_th ; float *avg_th ;
    void **ws ; doublevec *dv ;
    nthr    = omp_get_max_threads() ;                     /* number of threads */
    ndon_th = (int   *)remla_calloc(sizeof(int)  ,nthr) ; /* how many per thread */
    avg_th  = (float *)remla_calloc(sizeof(float),nthr) ; /* row lengths per thread */
    ws      = (void **)remla_calloc(sizeof(void*),nthr) ;
    for( qt=0 ; qt < nthr ; qt++ ){
      MAKE_doublevec(dv,nt) ; ws[qt] = (void *)dv ; /* make per-thread workspaces */
    }
    if( verb && nthr > 1 ){
      ININFO_message("starting %d OpenMP threads for REML 4D setup",nthr) ;
    }
# define WS(q) ws[q]   /* pre-allocated workspace for thread #q */
#else
# define WS(q) NULL    /* no pre-allocated workspace when no threads */
#endif

    /** start of potentially multi-threaded code **/

 AFNI_OMP_START ;
#pragma omp parallel if( maxthr > 1 && rcg->nset > 7 )
 { int iabcd , ii,jj,kk,ll,qq , ithr=0 ;
   int na = rcg->na, na1 = rcg->na1, nb = rcg->nb, nb1 = rcg->nb1,
       nc = rcg->nc, nc1 = rcg->nc1, nd = rcg->nd, nd1 = rcg->nd1,
       nabcd = rcg->nset ;
   double abot = rcg->abot, dda = rcg->da ;
   double bbot = rcg->bbot, ddb = rcg->db ;
   double cbot = rcg->cbot, ddc = rcg->dc ;
   double dbot = rcg->dbot, ddd = rcg->dd ;
   double dd,cc,bb,aa, pvec[6] ; float avg ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ; /* for saving things on a per-thread basis */
#endif
#pragma omp for
     for( iabcd=0 ; iabcd < nabcd ; iabcd++ ){ /* loop over (aa,bb,cc,dd) */
       qq = iabcd  ; ii = qq % na1 ; /* index in a-direction */
       qq = qq/na1 ; jj = qq % nb1 ; /* index in b-direction */
       qq = qq/nb1 ; kk = qq % nc1 ; /* index in c-direction */
       qq = qq/nc1 ; ll = qq ;       /* index in d-direction */
       qq = iabcd+1 ;
       aa = ii*dda + abot ;   /* actual value of a parameter */
       bb = jj*ddb + bbot ;   /* actual value of b parameter */
       cc = kk*ddc + cbot ;   /* actual value of c parameter */
       dd = kk*ddd + dbot ;   /* actual value of d parameter */
       if( rcg->rs[iabcd] == NULL ){
         LOAD_pvec4(aa,bb,cc,dd) ;
         rcg->rs[iabcd] = REML_setup_one_generic( X, tau, model_code, pvec, WS(ithr) ) ;
         if( rcg->rs[iabcd] != NULL ){
           avg = rcmat_avglen( rcg->rs[iabcd]->cc ) ;
#ifdef USE_OMP
           ndon_th[ithr]++ ; avg_th[ithr] += avg ;  /* per thread counting */
#else
           ndone++ ; avglen += avg ; /* overall counting - not multi-threaded */
#endif
         }
#if 1
       else { ININFO_message("4D setup [%d]  a=%g b=%g c=%g d=%g FAILS",
                             iabcd,aa,bb,cc,dd ) ; }
#endif
       }
     } /* end loop over (aa,bb,cc,dd) */

 } /* end OpenMP section */
 AFNI_OMP_END ;

#ifdef USE_OMP
   for( ii=0 ; ii < nthr ; ii++ ){
     ndone += ndon_th[ii] ; avglen += avg_th[ii] ; /* combine counts */
     dv = (doublevec *)ws[ii] ; KILL_doublevec(dv) ;
   }
   remla_free(avg_th) ; remla_free(ndon_th) ; remla_free(ws) ;
#endif

   } /* end general 4D setup */

   /* fix a couple of details and vamoose */

   rcg->ndone  = ndone ;        /* how many actually stored in grid */
   rcg->avglen = avglen/ndone ; /* average of average row lengths */
   rcg->savfil = NULL ;
   return rcg ;
}

/*--------------------------------------------------------------------------*/
/* For ARMA(3,1) -- full grid */
/*--------------------------------------------------------------------------*/

#define THETA_MIN (PI/32.0)
#define THETA_MAX (31.0*THETA_MIN)

reml_collection_generic * REML_setup_all_arma31(
                             matrix *X , int *tau , int nlev ,
                             double amax , double rmax ,
                             double tbot , double ttop , double vmax )
{
   if( tbot < THETA_MIN ) tbot = THETA_MIN ;
   if( ttop < THETA_MAX ) ttop = THETA_MAX ;
   return
     REML_setup_all_generic_4D( X , tau , ARMA31_MODEL ,
                                nlev , nlev , nlev , nlev ,
                                amax*0.03 , amax ,
                                rmax*0.03 , rmax ,
                                tbot      , ttop ,
                                vmax*0.03 , vmax   ) ;
}

/*--------------------------------------------------------------------------*/
/* Inputs: y=data vector, rcg=collection of REML setup stuff.
           nws = size of workspace array ws (0 => use malloc)
   Output: index of best case in the REML setup stuff.
           if ws!=NULL, then ws[0] = the best (smallest) REML score
*//*------------------------------------------------------------------------*/

int REML_find_best_case_generic_4D(
                vector *y, reml_collection_generic *rcg, int nws, double *ws )
{
   double rbest , rval ;
   int   na,nb , pna,pnb , mm , na1,nb1 ;
   int   nc,nd , pnc,pnd ,      nc1,nd1 ;
   int   nab1 , nabc1 , nabcd1 ;
   int   ibest,jbest,kbest,lbest , qbest ;
   int   ibot,itop , jbot,jtop , kbot,ktop , lbot,ltop ;
   int   lev,levtop , dab, ia,jb,kc,ld , qq ;
   int   qlist[2048] , nql , needed_ws,bb_ws=0,rv_ws=0 ;
   double *bbar[7] , *rvab ;

ENTRY("REML_find_best_case_generic_4D") ;

   if( y == NULL ) RETURN(-666) ; /* negative return = error message */

   mm = vector_floatscan( y ) ;
   if( mm > 0 ) ERROR_message("%d float errors in REML_find_best_case_generic_2D y input",mm) ;

   /* copy (a,b) grid parameters to local variables */

   na = rcg->na ; pna = rcg->pna ; na1 = rcg->na1 ;
   nb = rcg->nb ; pnb = rcg->pnb ; nb1 = rcg->nb1 ;
   nc = rcg->nc ; pnc = rcg->pnc ; nc1 = rcg->nc1 ;
   nd = rcg->nd ; pnd = rcg->pnd ; nd1 = rcg->nd1 ;
   nab1 = na1 * nb1 ; nabc1 = nab1 * nc1 ; nabcd1 = nabc1 * nd1 ;

   /* make workspace arrays for REML_func() */

   bb_ws     = 2*y->dim + 16 ;
   rv_ws     = nabcd1   + 16 ;
   needed_ws = 7*bb_ws  + rv_ws ;  /* 7 vectors plus 1 number per grid pt */

   if( nws >= needed_ws && ws != NULL ){  /* user provided workspace */
     rvab    = ws+1 ;
     bbar[0] = rvab + rv_ws ;
     for( ia=1 ; ia < 7 ; ia++ ) bbar[ia] = bbar[ia-1] + bb_ws ;
     bb_ws = rv_ws = 0 ;
   } else {                               /* make up our own workspace */
     rvab = (double *)remla_malloc(sizeof(double)*rv_ws) ;
     for( ia=0 ; ia < 7 ; ia++ )
       bbar[ia] = (double *)remla_malloc(sizeof(double)*bb_ws) ;
   }

   qbest = -1 ;      /* will be index of best case */
   rbest = BIGVAL ;  /* will be value of best case */
   ibest = na/2 ; jbest = nb/2 ; kbest = nc/2 ; lbest = nd/2 ;
   
   for( qq=0 ; qq < nabcd1 ; qq++ ) rvab[qq] = BIGVAL ; /* mark all as bad */

   /** do power-of-2 descent through the (a,b,c,d) grid to find the bestest pair **/

   levtop = MIN(pna,pnb) ; lev = MIN(pnc,pnd) ; if( lev < levtop ) levtop = lev ;
   levtop = levtop - 2 ;
   ibot = 0 ; itop = na ;                 /* scan from ibot..itop, jbot..jtop */
   jbot = 0 ; jtop = nb ;                          /* in steps of dab (below) */
   kbot = 0 ; ktop = nc ;
   lbot = 0 ; ltop = nd ;

   for( lev=ltop ; lev >= 0 ; lev-- ){     /* lev = power-of-2 grid scan size */
     dab = 1 << lev ;                      /* dab = 2^lev = step size in scan */

     /* make list of not-yet visited (a,b) grid
        points to visit in the REML_func loop below */

     nql = 0 ;
     for( ld=lbot ; ld <= ltop ; ld+=dab ){
      for( kc=kbot ; kc <= ktop ; kc+=dab ){
       for( jb=jbot ; jb <= jtop ; jb+=dab ){
        for( ia=ibot ; ia <= itop ; ia+=dab ){
          qq = ia + jb*na1 + kc*nab1 + ld*nabc1 ;
          if( rvab[qq] < BIGVAL ) continue ;   /* did this before */
          if( rcg->rs[qq] == NULL ) continue ; /* invalid grid point */
          qlist[nql++] = qq ;
     }}}}
     if( nql == 0 ) continue ; /* should never happen */

     for( mm=0 ; mm < nql ; mm++ ){  /* this usually takes a lot of CPU time */
       qq = qlist[mm] ;
       rvab[qq] = REML_func( y, rcg->rs[qq], rcg->X,rcg->Xs , bbar,NULL ) ;
     }

     /* find the best one so far seen, in the patch we are looking at */

     for( ld=lbot ; ld <= ltop ; ld+=dab ){
      for( kc=kbot ; kc <= ktop ; kc+=dab ){
       for( jb=jbot ; jb <= jtop ; jb+=dab ){
        for( ia=ibot ; ia <= itop ; ia+=dab ){
          qq = ia + jb*na1 + kc*nab1 + ld*nabc1 ; rval = rvab[qq] ;
          if( rval < rbest ){                          /* the best so far seen */
            rbest = rval; qbest = qq; ibest = ia; jbest = jb; kbest = kc; lbest = ld;
         }
     }}}}

     /* at the next level down, scan near the best so far seen */

     if( lev > 0 ){           /* if lev == 0, there is no next level */
       qq   = dab + dab/2 ;
       ibot = ibest-qq ; if( ibot < 0  ) ibot = 0  ;
       itop = ibest+qq ; if( itop > na ) itop = na ;
       jbot = jbest-qq ; if( jbot < 0  ) jbot = 0  ;
       jtop = jbest+qq ; if( jtop > nb ) jtop = nb ;
       kbot = kbest-qq ; if( kbot < 0  ) kbot = 0  ;
       ktop = kbest+qq ; if( ktop > nb ) ktop = nb ;
       lbot = lbest-qq ; if( lbot < 0  ) lbot = 0  ;
       ltop = lbest+qq ; if( ltop > nb ) ltop = nb ;
     }

   } /* end of scan descent through different levels */

   /* delete workspace if it was allocated inside here */

   if( bb_ws ){
     remla_free(rvab) ;
     for( ia=0 ; ia < 7 ; ia++ ) remla_free(bbar[ia]) ;
   }

   /* save the smallest REML value found */

   if( ws != NULL ) ws[0] = rbest ;

   /*** deal with the winner ***/

   RETURN(qbest) ;
}
