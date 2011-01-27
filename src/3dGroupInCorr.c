/* Group InstaCorr == GrpInCorr */

/***
  Ideas for making this program more cromulently embiggened:
   ++ 2-way case: produce 1-way result sub-bricks as well -- DONE!
   ++ Rank or other robust analog to t-test
   ++ Send sub-brick data as scaled shorts to reduce transmit time
   ++ Fix shm: bug in AFNI libray
   ++ Have non-server modes:
    -- To output dataset to disk
    -- 3dTcorrMap-like scan through whole brain as seed
   ++ Per-subject covariates -- DONE!
   ++ Send per-subject correlations to AFNI (as an option)
***/

#include "mrilib.h"

#ifdef USE_OMP     /* this is important! */
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

 AFNI_OMP_START ;
#pragma omp parallel
 { int ids , ndset=shd->ndset ;
#pragma omp for
   for( ids=0 ; ids < ndset ; ids++ ){
     if( verb > 3 ) fprintf(stderr," +   start correlation on dataset #%d\n",ids) ;
     GRINCOR_dotprod( shd , ids , vv[ids] , ddp[ids] ) ;
   }
 }
 AFNI_OMP_END ;

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

#define AFNI_NIML_PORT   53212          /* TCP/IP port that AFNI uses */
#define SUMA_GICORR_PORT 53224          /* TCP/IP port that SUMA uses */

static int nport = -1 ;                 /* 02 Aug 2010 */

NI_stream GI_stream = (NI_stream)NULL ;

/*=============================================================================*/

static char *pname = "AFNI" ;  /* name of partner program */

void GI_exit(void)                   /* Function to be called to make sure */
{                                    /* the AFNI data channel gets closed. */
   if( GI_stream != (NI_stream)NULL ){
     fprintf(stderr,"** 3dGroupInCorr exits: closing connection to %s\n",pname) ;
     NI_stream_close(GI_stream) ;
   } else if( verb > 2 ){
     fprintf(stderr,"** 3dGroupInCorr atexit() function invoked\n") ;
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
   fprintf(stderr,"\n** 3dGroupInCorr: Fatal Signal %d (%s) received\n",sig,sname) ;
   exit(1) ;
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
   int nopt , kk , nn , ii,jj, TalkToAfni=1;
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
   int ctim,btim,atim , do_shm=2 , nsend=0 , shm_active=0 ;
   char label_AAA[MAX_LABEL_SIZE]="AAA" , label_BBB[MAX_LABEL_SIZE]="BBB" ;
   char *qlab_AAA=NULL , *qlab_BBB=NULL ;
   int   lset_AAA=0    ,  lset_BBB=0 ;
   int   *use_AAA=NULL ,  *use_BBB=NULL ;  /* lists of subjects to use */

   NI_element   *covnel=NULL ;       /* covariates */
   NI_str_array *covlab=NULL ;
   MRI_IMAGE *axxim , *axxim_psinv , *axxim_xtxinv ;
   MRI_IMAGE *bxxim , *bxxim_psinv , *bxxim_xtxinv ;
   float **dtar=NULL ;
   int no_ttest = 0 ;  /* 02 Nov 2010 */

   int do_sendall=0 , nsaar=0 ; /* 22 Jan 2011 */
   char *bricklabels=NULL ;
   float **saar=NULL ;

#ifdef COVTEST
   float *ctarA=NULL , *ctarB=NULL ; char *ctnam ;
#endif

   /*-- enlighten the ignorant and brutish sauvages? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dGroupInCorr [options]\n"
      "\n"
      "* Also see\n"
      "  http://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/instastuff.pdf\n"
      "\n"
      "* This program operates as a server for AFNI or SUMA.  It reads in dataset\n"
      "  collections that have been prepared by 3dSetupGroupInCorr, and then\n"
      "  connects to the AFNI or SUMA GUI program (via TCP/IP).  Then it waits\n"
      "  for a command to be sent from AFNI/SUMA before it actually does anything.\n"
      "\n"
      "* At the same time as you run 3dGroupInCorr, you also have to run the\n"
      "  AFNI GUI program, with a command like 'afni -niml'.  3dGroupInCorr\n"
      "  by itself will only do something when AFNI sends it a command, which\n"
      "  you do by using the 'InstaCorr Set' button on the [A] image viewer\n"
      "  right-click popup menu, after 3dGroupInCorr has connected to AFNI.\n"
      "\n"
      "* When AFNI sends a seed voxel command, 3dGroupInCorr will extract\n"
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
      " ++ If you are running 3dGroupInCorr and AFNI on separate computers,\n"
      "    you also have to setup 'host trusting' correctly -- for details,\n"
      "    see the description of the '-ah' option, far below.\n"
      "\n"
      "* More detailed outline of processing in 3dGroupInCorr:\n"
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
      "* When 3dGroupInCorr starts up, it has to 'page fault' all the data\n"
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
      "  ++ 3dGroupInCorr can use byte-valued or short-valued data as\n"
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
      "\n"
      " -labelB bbb = Label to attach (in AFNI) to sub-bricks corresponding to setB.\n"
      "              ++ At most the first 11 characters of each label will be used!\n"
      "\n"
      " -sendall    = Send all individual subject results to AFNI, as well as the\n"
      "               various statistics.\n"
      "              ++ These extra sub-bricks will be labeled like 'xxx_zcorr', where\n"
      "                 'xxx' indicates which dataset the results came from; 'zcorr'\n"
      "                 denotes that the values are the arctanh of the correlations.\n"
      "              ++ If there are a lot of datasets, then the results will be VERY\n"
      "                 large and take up a lot of memory in AFNI.\n"
      "              ++ Use this option with somejudgment and wisdom, or bad things\n"
      "                 might happen! (e.g., your computer runs out of memory)\n"
      "              ++ This option is known as the 'Tim Ellmore special'.\n"
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
#if 0
      "\n"
      "*** Special Option for Ziad Saad (and His Ilk) ***\n"
      "\n"
      " -no_ttest = Don't do any t-tests at all.  Just compute the correlations\n"
      "             at each voxel for each dataset and transmit those to the\n"
      "             master program (AFNI or SUMA).\n"
      "            ++ This really is a special case, and not for the normal user.\n"
#endif
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
      "            -- At present, there is no way to tell 3dGroupInCorr not to send\n"
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
      "     ++ This allows 3dGroupInCorr to run on a separate system than\n"
      "        the AFNI GUI.\n"
      "       -- e.g., If your desktop is weak and pitiful, but you have access\n"
      "          to a strong and muscular multi-CPU server (and the network\n"
      "          connection is fast).\n"
      "     ++ Note that AFNI must be setup with the appropriate\n"
      "        'AFNI_TRUSTHOST_xx' environment variable, so that it will\n"
      "        allow the external socket connection (for the sake of security):\n"
      "      -- Example: AFNI running on computer 137.168.0.3 and 3dGroupInCorr\n"
      "         running on computer 137.168.0.7\n"
      "      -- Start AFNI with a command like\n"
      "           afni -DAFNI_TRUSTHOST_01=137.168.0.7 -niml ...\n"
      "      -- Start 3dGroupInCorr with a command like\n"
      "           3dGroupInCorr -ah 137.168.0.3 ...\n"
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
      "            only way to have multiple copies of 3dGroupInCorr and AFNI\n"
      "            talking to each other!\n"
#ifndef DONT_USE_SHM
      "\n"
      " -NOshm = Do NOT reconnect to AFNI using shared memory, rather than TCP/IP,\n"
      "          when using 'localhost' (i.e., AFNI and 3dGroupInCorr are running\n"
      "          on the same system).\n"
      "       ++ The default is to use shared memory for communication when\n"
      "          possible, since this method of transferring large amounts of\n"
      "          data between programs on the same computer is much faster.\n"
      "       ++ If you have a problem with the shared memory communication,\n"
      "          use '-NOshm' to use TCP/IP for all communications.\n"
      "       ++ If you use '-VERB', you will get a very detailed progress report\n"
      "          from 3dGroupInCorr as it computes, including elapsed times for\n"
      "          each stage of the process.\n"
#endif
      "\n"
      " -quiet = Turn off the 'fun fun fun in the sun sun sun' informational messages.\n"
      " -verb  = Print out extra informational messages for more fun!\n"
      " -VERB  = Print out even more informational messages for even more fun!!\n"
#ifdef isfinite
      " -debug = Do some internal testing (slows things down a little)\n"
#endif
      "\n"
      "-------============= Talairach (+trlc) vs. Original (+orig) =============-------\n"
      "\n"
      "Normally, AFNI assigns the dataset sent by 3dGroupInCorr to the +tlrc view.\n"
      "However, you can tell AFNI to assign it to the +orig view instead.\n"
      "To do this, set environment variable AFNI_GROUPINCORR_ORIG to YES when\n"
      "starting AFNI; for example:\n"
      "\n"
      "  afni -DAFNI_GROUPINCORR_ORIG=YES -niml\n"
      "\n"
      "This feature might be useful to you if you are doing a longitudinal study on\n"
      "some subject, comparing resting state maps before and after some treatment.\n"
      "\n"
      "-------========= Group InstaCorr and AFNI's Clusterize function =========-------\n"
      "\n"
      "At this moment in history, you can't use Clusterize in the AFNI A controller at\n"
      "the same time that 3dGroupInCorr is actively connected.  If you also want to\n"
      "Clusterize the maps from this program, there are 2 slightly clumsy methods\n"
      "that work reasonably well:\n"
      "\n"
      "(1) In the A controller, you can switch between 'Clusters' and 'GrpInCorr':\n"
      "   -- When 'Clusters' is active, you can Clusterize, but you can't do a\n"
      "      new 'InstaCorr Set' operation\n"
      "   -- When 'GrpInCorr' is active, then you can do 'InstaCorr Set', but\n"
      "      the new result won't be Clusterize-d, until you switch back to\n"
      "      'Clusters' and then press 'Clusterize' again.\n"
      "   -- This is clumsy because you have to keep switching between GrpInCorr\n"
      "      and Clusterize, which quickly becomes annoying.\n"
      "\n"
      "(2) Alternatively, you can open up the B controller (with the 'New' button),\n"
      "    and then view the A_GRP_ICORR dataset as the Overlay in a separate\n"
      "    set of image viewers, which you can Clusterize.\n"
      "   -- However, since you also have to view the un-Clusterize-d A_GRP_ICORR\n"
      "      dataset in the AFNI A controller, it is necessary to un-Lock the\n"
      "      viewing controls of the two controllers.  Otherwise, the 2 controllers\n"
      "     'fight' for who controls the way the dataset is edited for presentation,\n"
      "      and the Clusterize-ation in controller B can appear and disappear\n"
      "      as you scroll around.\n"
      "   -- To turn off the Lock between the A and B controllers, use the\n"
      "      Datamode->Lock menu and select 'Clear All'.  Or start AFNI with\n"
      "      the command line option '-DAFNI_ALWAYS_LOCK=NO' (to override the\n"
      "      default where all controllers are locked together at startup).\n"
      "   -- This is clumsy because you have to use two controllers, and set\n"
      "      your GrpInCorr seed in the A image viewers but view the results\n"
      "      you want to see in the B image viewers.  And scrolling around in\n"
      "      the unlocked image viewers can also be annoying.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dGroupInCorr",NULL) ;
     printf("++ Authors: Bob Cox and Ziad Saad\n") ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dGroupInCorr"); machdep();
   AFNI_logger("3dGroupInCorr",argc,argv);
   PRINT_VERSION("3dGroupInCorr"); AUTHOR("The Mad Correlator");

   /*-- process command line options --*/

   nopt = 1 ;
   while( nopt < argc ){

#ifndef DONT_USE_SHM
     if( strcasecmp(argv[nopt],"-NOshm") == 0 ){
       do_shm = 0 ; nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-sendall") == 0 ){  /* 22 Jan 2011 */
       do_sendall++ ; nopt++ ; continue ;
     }

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

#if 0
     if( strcasecmp(argv[nopt],"-useA") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       if( use_AAA != NULL ) ERROR_exit("you can't use -useA twice!") ;
       use_AAA = MCW_get_intlist( 999999 , argv[nopt] ) ;
       if( use_AAA == NULL || use_AAA[0] <= 0 )
         ERROR_exit("can't decode argument after -useA") ;
     }

     if( strcasecmp(argv[nopt],"-useB") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       if( use_BBB != NULL ) ERROR_exit("you can't use -useB twice!") ;
       use_BBB = MCW_get_intlist( 999999 , argv[nopt] ) ;
       if( use_BBB == NULL || use_BBB[0] <= 0 )
         ERROR_exit("can't decode argument after -useB") ;
     }
#endif

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

     ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
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

#if 0
   /*-- attach use list to dataset collections [07 Apr 2010] --*/

   if( use_AAA != NULL ){
     int nuse = use_AAA[0] ;
     for( kk=1 ; kk <= nuse ; kk++ ){
       if( use_AAA[kk] < 0 || use_AAA[kk] >= shd_AAA->ndset )
         ERROR_exit("Index in -useAAA outside of range 0..%d",shd_AAA->ndset-1) ;
     }
     shd_AAA->nuse = nuse ;
     shd_AAA->use  = use_AAA + 1 ;
   }

   if( use_BBB != NULL && shd_BBB != NULL ){
     int nuse = use_BBB[0] ;
     for( kk=1 ; kk <= nuse ; kk++ ){
       if( use_BBB[kk] < 0 || use_BBB[kk] >= shd_BBB->ndset )
         ERROR_exit("Index in -useBBB outside of range 0..%d",shd_BBB->ndset-1) ;
     }
     shd_BBB->nuse = nuse ;
     shd_BBB->use  = use_BBB + 1 ;
   } else if( use_BBB != NULL ){
     WARNING_message("-useB was given, but -setB wasn't given!") ;
   }
#endif

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

   /*-- Create NIML element to hold the output brick data --*/

   nelset = NI_new_data_element( "3dGroupInCorr_dataset" , nvec ) ;

   /*----- if no covariates, do it the olden style way -----*/

   if( mcov == 0 ){ /* create columns in nelset, 1 for each output sub-brick */

     NI_add_column( nelset, NI_FLOAT, NULL );
     NI_add_column( nelset, NI_FLOAT, NULL );
     neldar = (float *)nelset->vec[0];          /* neldar = delta  sub-brick */
     nelzar = (float *)nelset->vec[1];          /* nelzar = Zscore sub-brick */
     if( neldar == NULL || nelzar == NULL )
       ERROR_exit("Can't setup output dataset?") ; /* should never transpire */

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
       if( dtar[kk] == NULL )
         ERROR_exit("Can't setup output dataset [#%d]?!",kk) ;
     }
     if( shd_BBB != NULL ){   /* bit masks for which tests to compute */
       testAB = (UINT32)(-1) ; testA  = testB = (dosix) ? testAB : 0 ;
     } else {
       testAB = testB = 0 ; testA = (UINT32)(-1) ;
     }

   }

   /* add columns for the individual dataset arctanh(corr) results */

   if( do_sendall ){
     int qq = nelset->vec_num ;                  /* # of columns at the start */
     nsaar = ndset_AAA + ndset_BBB ;      /* number of -sendall arrays to add */
     saar  = (float **)malloc(sizeof(float *)*nsaar) ;
     for( kk=0 ; kk < nsaar ; kk++ ){
       NI_add_column( nelset , NI_FLOAT , NULL ) ;
       saar[kk] = (float *)nelset->vec[kk+qq] ;
       if( saar[kk] == NULL )
         ERROR_exit("Can't setup output dataset for -sendall [#%d]?!",kk) ;
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

   /*========= this stuff is one-time-only setup of the I/O to AFNI =========*/

   atexit(GI_exit) ;             /* call this when program ends */

   signal(SIGINT ,GI_sigfunc) ;  /* setup signal handler */
   signal(SIGBUS ,GI_sigfunc) ;  /* for fatal errors */
   signal(SIGSEGV,GI_sigfunc) ;
   signal(SIGTERM,GI_sigfunc) ;

   /* name of NIML stream (socket) to open */

                    pname = (TalkToAfni) ? "AFNI"         : "SUMA" ;
   if( nport <= 0 ) nport = (TalkToAfni) ? AFNI_NIML_PORT : SUMA_GICORR_PORT ;
   sprintf( nsname , "tcp:%s:%d" , afnihost , nport ) ;

   /* open the socket (i.e., dial the telephone call) */

   fprintf(stderr,"++ Opening NIML socket '%s' to %s",nsname,pname) ;
   GI_stream = NI_stream_open( nsname , "w" ) ;

   /* loop until AFNI connects (answers the call),
      printing a '.' every so often to keep the user happy */

   for( nn=0 ; nn < 234 ; nn++ ){
     fprintf(stderr,".") ;
     kk = NI_stream_writecheck( GI_stream , 999 ) ;
     if( kk == 1 ){ fprintf(stderr," Connected!\n") ; break ; }
     if( kk <  0 ){ fprintf(stderr," ** Connection fails :-(\n") ; exit(1) ; }
   }
   if( kk <= 0 ){ fprintf(stderr," ** Connection times out :-(\n") ; exit(1) ; }

   /** store some info about the dataset we are constructing **/

   nx = DSET_NX(shd_AAA->tdset) ; dx = fabsf(DSET_DX(shd_AAA->tdset)) ;
   ny = DSET_NY(shd_AAA->tdset) ; dy = fabsf(DSET_DY(shd_AAA->tdset)) ;
   nz = DSET_NZ(shd_AAA->tdset) ; dz = fabsf(DSET_DZ(shd_AAA->tdset)) ;
   dmin = MIN(dx,dy) ; dmin = MIN(dmin,dz) ; nxy = nx*ny ;

   /** now send our setup info to AFNI **/

   if( shd_AAA->nvec == shd_AAA->nvox ){
     nelcmd = NI_new_data_element( "3dGroupInCorr_setup" , 0 ) ;  /* no data */
   } else {
     nelcmd = NI_new_data_element( "3dGroupInCorr_setup" , nvec ) ;
     NI_add_column( nelcmd , NI_INT , shd_AAA->ivec ) ;    /* data = indexes */
   }

   /* set various attributes to let AFNI know what's up, doc */

   sprintf(buf,"%d",shd_AAA->ndset) ;
   NI_set_attribute( nelcmd , "ndset_A" , buf ) ;

   sprintf(buf,"%d",(shd_BBB != NULL) ? shd_BBB->ndset : 0 ) ;
   NI_set_attribute( nelcmd , "ndset_B" , buf ) ;

   sprintf(buf,"%d",nvec) ;
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

   NI_set_attribute_int( nelcmd , "target_nvals" , nout+nsaar ) ;

   bricklabels = (char *)calloc(sizeof(char),(5*MAX_LABEL_SIZE+16)*(nout+nsaar+1)) ;
   if( mcov == 0 ){

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

   } else {  /* labels for the myriad of covariates results [23 May 2010] */

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

     kk = strlen(bricklabels) ; bricklabels[kk-1] = '\0' ;  /* truncate last ';' */
   }

   /* add labels for the subject-level bricks, if needed */

   if( do_sendall ){
     char buf[32] ;
     for( kk=0 ; kk < ndset_AAA ; kk++ ){
       if( shd_AAA->dslab != NULL ) sprintf(buf,"A_%.10s",shd_AAA->dslab[kk]) ;
       else                         sprintf(buf,"%.9s#%02d",label_AAA,kk) ;
       sprintf( bricklabels+strlen(bricklabels) , "%s_zcorr ; " , buf ) ;
     }

     for( kk=0 ; kk < ndset_BBB ; kk++ ){
       if( shd_BBB->dslab != NULL ) sprintf(buf,"B_%.10s",shd_BBB->dslab[kk]) ;
       else                         sprintf(buf,"%.9s#%02d",label_BBB,kk) ;
       sprintf( bricklabels+strlen(bricklabels) , "%s_zcorr ; " , buf ) ;
     }

     kk = strlen(bricklabels) ; bricklabels[kk-1] = '\0' ;  /* truncate last ';' */
   }

   /* set the brick labels into the header being sent to AFNI/SUMA */

   NI_set_attribute( nelcmd , "target_labels" , bricklabels ) ;
   free(bricklabels) ;

   /* ZSS: set surface attributes [note Ziad's terrible use of spaces] */

   if (shd_AAA->nnode[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->nnode[0], shd_AAA->nnode[1]);
      NI_set_attribute( nelcmd , "LRpair_nnode", buf);
   }
   if (shd_AAA->ninmask[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->ninmask[0], shd_AAA->ninmask[1]);
      NI_set_attribute( nelcmd , "LRpair_ninmask", buf);
   }

   /* actually send the setup NIML element now */

   if( verb > 1 ) INFO_message("Sending setup information to %s",pname) ;
   nn = NI_write_element( GI_stream , nelcmd , NI_BINARY_MODE ) ;
   if( nn < 0 ){
     ERROR_exit("Can't send setup data to %s!?",pname) ;
   }
   NI_free_element(nelcmd) ;

   /** make neighborhood struct for seedrad usage **/

   if( seedrad >= dmin ){
     nbhd = MCW_spheremask( dx,dy,dz , seedrad ) ;
     if( nbhd != NULL && nbhd->num_pt < 2 ) KILL_CLUSTER(nbhd) ;
   }

   /** make space for seed vectors and arctanh(correlations) **/

   seedvec_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   dotprod_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   for( kk=0 ; kk < ndset_AAA ; kk++ ){
     seedvec_AAA[kk] = (float *)malloc(sizeof(float)*nvals_AAA[kk]) ;
     if( nsaar == 0 )
       dotprod_AAA[kk] = (float *)malloc(sizeof(float)*nvec) ;
     else
       dotprod_AAA[kk] = saar[kk] ;
   }

   if( shd_BBB != NULL ){
     seedvec_BBB = (float **)malloc(sizeof(float *)*ndset_BBB) ;
     dotprod_BBB = (float **)malloc(sizeof(float *)*ndset_BBB) ;
     for( kk=0 ; kk < ndset_BBB ; kk++ ){
       seedvec_BBB[kk] = (float *)malloc(sizeof(float)*nvals_BBB[kk]) ;
       if( nsaar == 0 )
         dotprod_BBB[kk] = (float *)malloc(sizeof(float)*nvec) ;
       else
         dotprod_BBB[kk] = saar[kk+ndset_AAA] ;
     }
   }

   if( verb ){
     INFO_message("3dGroupInCorr stands ready to do thy bidding :-) !!") ;
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

   while(1){  /* loop forever? */

     nelcmd = NI_read_element( GI_stream , 333 ) ;  /* get command? */

     /* nada?  check if something is bad */

     if( nelcmd == NULL ){
       kk = NI_stream_goodcheck( GI_stream , 1 ) ;
       if( kk < 1 ){
         NI_stream_close(GI_stream) ; GI_stream = (NI_stream)NULL ;
         WARNING_message("Connection to %s broken - trying to restart",pname) ;
         NI_sleep(111) ;                /* give AFNI a moment to do whatever */
         GI_stream = NI_stream_open( nsname , "w" ) ;
         kk = NI_stream_goodcheck( GI_stream , 9999 ) ; /* wait a little bit */
         if( kk == 1 ){
           ININFO_message("TCP/IP restart is good :-)") ; shm_active = 0 ;
         } else {
           ININFO_message("TCP/IP restart failed :-(") ;
           NI_stream_close(GI_stream) ; GI_stream = (NI_stream)NULL ;
           goto GetOutOfDodge ;  /* failed */
         }
       }
       continue ; /* loop back */
     }

     /* the following should never happen */

     if( NI_element_type(nelcmd) != NI_ELEMENT_TYPE ){
       WARNING_message("Badly formatted command from %s!",pname) ;
       NI_free_element(nelcmd) ; continue ;
     }

     /* do something with the command, based on the element name */

     /** Command = AFNI said 'TaTa for Now' **/

     if( strcmp(nelcmd->name,"AuRevoir") == 0 ){
       INFO_message("Message from %s: ** Au Revoir **",pname) ;
       NI_free_element(nelcmd) ;
       goto GetOutOfDodge ;  /* failed */
     }

     /** start timer **/

     if( verb > 1 || (verb==1 && nsend < NSEND_LIMIT) )
       INFO_message("Received command %s from %s",nelcmd->name,pname) ;

     atim = btim = NI_clock_time() ;

     /** Command = set seed voxel index **/

     if( strcmp(nelcmd->name,"SETREF_ijk") == 0 ){

       /* extract location of seed voxel from command */

                         atr = NI_get_attribute(nelcmd,"index") ;
       if( atr == NULL ) atr = NI_get_attribute(nelcmd,"node" ) ;
       if( atr == NULL ) atr = NI_get_attribute(nelcmd,"ijk"  ) ;
       if( atr == NULL ){   /* should never happen */
         WARNING_message("SETREF_ijk: no index given!?") ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }
       voxijk = (int)strtod(atr,NULL) ;
       voxind = IJK_TO_INDEX(shd_AAA,voxijk) ;
       if( verb > 2 )
         ININFO_message(" dataset index=%d  node index=%d",voxijk,voxind) ;
       if( voxind < 0 ){
         WARNING_message("SETREF_ijk: %d is not in mask!?",voxijk) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }

       /* radius over which to average */

       atr = NI_get_attribute(nelcmd,"seedrad") ;
       if( atr != NULL ){
         float nsr = (float)strtod(atr,NULL) ;
         if( nsr != seedrad ){
           seedrad = nsr ; KILL_CLUSTER(nbhd) ;
           if( seedrad >= dmin ){
             nbhd = MCW_spheremask( dx,dy,dz , seedrad ) ;
             if( nbhd != NULL && nbhd->num_pt < 2 ) KILL_CLUSTER(nbhd) ;
           }
           if( verb > 2 )
             ININFO_message(" seedrad set to %.2f mm",seedrad) ;
         }
       }

#if 0
       /* t-test method (for 2-sample case only) */

       atr = NI_get_attribute(nelcmd,"ttest_opcode") ;
       if( atr != NULL ){
         int nto = (int)strtod(atr,NULL) ;
         if( nto < 0 || nto > ttest_opcode_max ) ttest_opcode = 0 ;
         else                                    ttest_opcode = nto ;
         if( verb > 2 )
           ININFO_message(" ttest_opcode set to %d",ttest_opcode) ;
       }
#endif

     /** unknown command type **/

     } else {

       WARNING_message("Don't know what to do with command %s",nelcmd->name) ;
       NI_free_element(nelcmd) ;
       goto LoopBack ;

     }

     /** throw away the message from AFNI **/

     NI_free_element( nelcmd ) ;

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

#if 0
     if( verb > 4 ){
       float mm,ss ; int nf ;
       INFO_message("dotprod_AAA statistics") ;
       for( kk=0 ; kk < ndset_AAA ; kk++ ){
          nf = thd_floatscan( nvec , dotprod_AAA[kk] ) ;
          meansigma_float( nvec , dotprod_AAA[kk] , &mm,&ss ) ;
          ININFO_message(" #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
       }
       if( ndset_BBB > 0 ){
         INFO_message("dotprod_BBB statistics") ;
         for( kk=0 ; kk < ndset_BBB ; kk++ ){
            nf = thd_floatscan( nvec , dotprod_BBB[kk] ) ;
            meansigma_float( nvec , dotprod_BBB[kk] , &mm,&ss ) ;
            ININFO_message(" #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
         }
       }
     }
#endif

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

       if( verb > 3 )
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

       if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message(" finished t-test-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     } else {  /*-- covariates ==> regression analyses --*/

       if( verb > 3 )
         ININFO_message(" start %d-sample regression-izing" , (ndset_BBB > 0) ? 2 : 1 ) ;

       GRINCOR_many_regress( nvec , ndset_AAA , dotprod_AAA ,
                                    ndset_BBB , dotprod_BBB , nout , dtar ) ;

       if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message(" finished regression-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     }

     /** re-attach to AFNI using shared memory? **/

#ifndef DONT_USE_SHM
     if( do_shm > 0 && strcmp(afnihost,"localhost") == 0 && !shm_active ){
       char nsnew[128] ;
       kk = (nout+nsaar) / 2 ; if( kk < 1 ) kk = 1 ; else if( kk > 3 ) kk = 3 ;
       sprintf( nsnew , "shm:GrpInCorr_%d:%dM+4K" , nport , kk ) ;
       INFO_message("Reconnecting to %s with shared memory channel %s",pname,nsnew) ;
       kk = NI_stream_reopen( GI_stream , nsnew ) ;
       if( kk == 0 ){
         ININFO_message(" SHM reconnection *FAILED* :-( ???") ;
       }
       else {
         ININFO_message(" SHM reconnection *ACTIVE* :-) !!!") ; shm_active = 1 ;
       }
       do_shm-- ;
     }
#endif

     /*** send the result to AFNI ***/

#if 0
     if( verb > 4 ){
       float mm,ss ; int nf ;
       INFO_message("dotprod_AAA statistics") ;
       for( kk=0 ; kk < ndset_AAA ; kk++ ){
          nf = thd_floatscan( nvec , dotprod_AAA[kk] ) ;
          meansigma_float( nvec , dotprod_AAA[kk] , &mm,&ss ) ;
          ININFO_message(" #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
       }
       if( ndset_BBB > 0 ){
         INFO_message("dotprod_BBB statistics") ;
         for( kk=0 ; kk < ndset_BBB ; kk++ ){
           nf = thd_floatscan( nvec , dotprod_BBB[kk] ) ;
           meansigma_float( nvec , dotprod_BBB[kk] , &mm,&ss ) ;
           ININFO_message(" #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
         }
       }
     }
#endif

     if( verb > 3 ) ININFO_message(" sending results to %s",pname) ;
     kk = NI_write_element( GI_stream , nelset , NI_BINARY_MODE ) ;
     if( kk <= 0 ){
       ERROR_message("3dGroupInCorr: failure when writing to %s",pname) ;
     }

     ctim = NI_clock_time() ;
     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message(" sent results to %s: elapsed=%d ms  bytes=%s" ,
                      pname , ctim-btim , commaized_integer_string(kk) ) ;

     if( verb > 1 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message(" Total elapsed time = %d msec",ctim-atim) ;

     nsend++ ;  /* number of results sent back so far */

  LoopBack: ; /* loop back for another command from AFNI */
   }

   /*-- bow out gracefully --*/

GetOutOfDodge :
   /* NI_free_element(nelset) ; */

   INFO_message("Exeunt 3dGroupInCorr and its %s of data" ,
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

 AFNI_OMP_START ;
#pragma omp parallel
 { int ii,kk ; float *xar,*yar ; float_pair delzsc ;
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
 }
 AFNI_OMP_END ;

   } else {  /*--- 1 sample t-test ---*/

 AFNI_OMP_START ;
#pragma omp parallel
 { int kk,ii ; float *xar ; float_pair delzsc ;
   xar = (float *)malloc(sizeof(float)*numx) ;
#pragma omp for
     for( kk=0 ; kk < nvec ; kk++ ){
       for( ii=0 ; ii < numx ; ii++ ) xar[ii] = xxar[ii][kk] ;
       delzsc  = ttest_toz( numx , xar , 0 , NULL , ttest_opcode ) ;
       dar[kk] = delzsc.a ; zar[kk] = delzsc.b ;
     }
   free(xar) ;      /* oopsie [13 Sep 2010] */
 }
 AFNI_OMP_END ;

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
   AFNI_OMP_START ;
#pragma omp parallel
   { int ii,kk ; float *xar,*yar,*var,*wss ;
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

   }
   AFNI_OMP_END ;

   } else {  /*--- 1 sample ---*/

   AFNI_OMP_START ;
#pragma omp parallel
   { int ii,kk ; float *xar,*var,*wss ;
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
   }
   AFNI_OMP_END ;

   }

   return ;
}
