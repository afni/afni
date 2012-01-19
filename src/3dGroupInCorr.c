/*** 3dGroupIncorr == Group InstaCorr == GrpInCorr ***/

/***
  Ideas for making this program more cromulently embiggened:
   ++ 2-way case: produce 1-way result sub-bricks as well  -- DONE!
   ++ Rank or other robust analog to t-test (slow)         -- TBD
   ++ Send sub-brick data as scaled shorts                 -- TBD
   ++ Fix shm: bug in AFNI libray (but how?)               -- TBD
   ++ Have non-server modes:
    -- To input a 1D file as the seed vector set           -- DONE!
    -- To input a mask file to define the seed vector set  -- DONE!
    -- To output dataset(s) to disk                        -- DONE!
    -- 3dTcorrMap-like scan through whole brain as seed    -- TBD
   ++ Per-subject covariates                               -- DONE!
   ++ Send per-subject correlations to AFNI (as an option) -- DONE!
***/

#include "mrilib.h"
#include "suma_suma.h"

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

static unsigned short xran[3] = { 0x330e , 0x747a , 0x9754 } ;

#undef  UINT32
#define UINT32 unsigned int  /* 20 May 2010 */
#undef  MAXCOV
#define MAXCOV 31

#define CENTER_NONE 0        /* 15 Jul 2011 */
#define CENTER_DIFF 1
#define CENTER_SAME 2
#define CENTER_VALS 3

static int center_code = CENTER_DIFF ;
static MRI_IMAGE *center_valimA=NULL , *center_valimB=NULL ;

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
   int ii ; char *npt , *ttt , *sss ;

   if( targ == NULL || *targ == '\0' || str == NULL || nstr < 1 ) return -1 ;

   ttt = strdup(targ); npt = strstr(ttt,".nii"); if( npt != NULL ) *npt = '\0';
   for( ii=0 ; ii < nstr ; ii++ ){
     if( str[ii] != NULL ){
       sss = strdup(str[ii]); npt = strstr(sss,".nii"); if( npt != NULL ) *npt = '\0';
       if( strcmp(ttt,sss) == 0 ){ free(sss); free(ttt); return ii; }
       free(sss) ;
     }
   }

   free(ttt) ; return -1 ;
}

/*--------------------------------------------------------------------------*/

typedef struct {

  int nvec  ;     /* number of vectors in a dataset */
  int ndset ;     /* number of datasets */
  int *nvals ;    /* nvals[i] = number of values in a vector in i-th dataset */
  int nvals_max ; /* largest nvals[i] value */
  int nvals_tot ; /* sum of nvals[i] values */
  int datum ;     /* 1 for sbyte, 2 for short */

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

/*------ for creating output dataset internally (rather than in AFNI) ------*/

#if 0    /* will use GICOR_setup instead           ZSS: Jan 2012*/
typedef struct {               /* Feb 2011 */
  THD_3dim_dataset *dset ;
  int nvox , nivec , *ivec ;
} GRINCOR_setup ;
#endif

GICOR_setup * GRINCOR_setup_dataset( NI_element * ) ;
int GRINCOR_output_dataset( GICOR_setup *, NI_element *, char * ) ;

/*--------------------------------------------------------------------------*/

#undef  GQUIT
#define GQUIT(sss)                                                     \
 do{ if( tdset != NULL ) DSET_delete(tdset) ;                          \
     if( dfname != NULL ) free(dfname) ;                               \
     if( geometry_string != NULL ) free(geometry_string) ;             \
     NI_free_element(nel) ;                                            \
     if( sss != NULL ) ERROR_message("GIC: file %s: %s",fname,(sss)) ; \
     return(NULL) ;                                                    \
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
   void *var ; int ids , nvmax , nvtot ;
   int datum , datum_size ;

   char *geometry_string=NULL ;
   THD_3dim_dataset *tdset=NULL; int nvox;
   int no_ivec=0 , *ivec=NULL , *nvals=NULL , nvec,ndset ; float *fac=NULL ;
   NI_str_array *slabar=NULL ;

   if( fname == NULL || *fname == '\0' ) GQUIT(NULL) ;

   /* get data element */

   if (!THD_is_ondisk(fname)) {
      GQUIT("not on disk");
   }
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

   nvmax = nvtot = nvals[0] ;
   for( ids=1 ; ids < ndset ; ids++ ){             /* Feb 2011 */
     nvtot += nvals[ids] ;
     if( nvals[ids] > nvmax ) nvmax = nvals[ids] ;
   }

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
   if( nbytes_dfname <= 0 && strstr(dfname,"/") != NULL ){
     char *tnam = THD_trailname(atr,0) ;
     nbytes_dfname = THD_filesize(tnam) ;
     if( nbytes_dfname > 0 ){ free(dfname); dfname = strdup(tnam); }
   }
   if( nbytes_dfname <= 0 ){
     char mess[THD_MAX_NAME+256] ;
     sprintf(mess,"datafile is missing (%s)",dfname) ; GQUIT(mess) ;
   } else if( nbytes_dfname < nbytes_needed ){
     char mess[THD_MAX_NAME+1024] ;
     sprintf(mess,"datafile %s has %s bytes but needs at least %s",
              dfname , 
              commaized_integer_string(nbytes_dfname) ,
              commaized_integer_string(nbytes_needed) ) ;
     GQUIT(mess) ;
   }
   fdes = open( dfname , O_RDONLY ) ;
   if( fdes < 0 ){
     char mess[THD_MAX_NAME+256] ;
     sprintf(mess,"can't open datafile (%s)",dfname) ; GQUIT(mess) ;
   }

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

   shd->nvals = nvals ; shd->nvals_max = nvmax ; shd->nvals_tot = nvtot ;
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
       "GIC: file %s: can't mmap() datafile -- memory space exhausted?" , dfname ) ;
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
       if( nbad > 0 ) WARNING_message("GIC: %d bad correlations in dataset #%d",nbad,ids) ;
     }
   }
#endif

   return ;
}

/*----------------------------------------------------------------------------*/

int GRINCOR_extract_ijknbhd( MRI_shindss *shd , int dd ,
                             MCW_cluster *nbhd , int voxijk , float *tsar )
{
   int nvec,nvals, voxind,ii , isbyte=(shd->datum==1) ;
   short *sv=NULL , *svv=NULL ; float *vv ; sbyte *bv=NULL , *bvv=NULL ;

   voxind = IJK_TO_INDEX(shd,voxijk) ;
   nvals  = shd->nvals[dd] ;
   vv     = tsar ;

   if( isbyte ){
     bv = shd->bv[dd] ; bvv = bv + voxind*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)bvv[ii] ;
   } else {
     sv = shd->sv[dd] ; svv = sv + voxind*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)svv[ii] ;
   }

   nvec = 1 ;

   if( nbhd != NULL ){
     int aa,bb,cc,xx,yy,zz , qijk,qind , jj , nx,ny,nz,nxy ;
     nx = shd->nx ; ny = shd->ny ; nz = shd->nz ; nxy = nx*ny ;
     IJK_TO_THREE(voxijk,aa,bb,cc,nx,nxy) ;
     for( jj=1 ; jj < nbhd->num_pt ; jj++ ){
       xx = aa + nbhd->i[jj] ; if( xx < 0 || xx >= nx ) continue ;
       yy = bb + nbhd->j[jj] ; if( yy < 0 || yy >= ny ) continue ;
       zz = cc + nbhd->k[jj] ; if( zz < 0 || zz >= nz ) continue ;
       qijk = THREE_TO_IJK(xx,yy,zz,nx,nxy) ;
       qind = IJK_TO_INDEX(shd,qijk) ;
       if( qind >= 0 ){
         vv = tsar + nvec*nvals ; nvec++ ;
         if( isbyte ){
           bvv = bv + qind*nvals ;
           for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)bvv[ii] ;
         } else {
           svv = sv + qind*nvals ;
           for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)svv[ii] ;
         }
       }
     }
   }

   return nvec ;
}

/*----------------------------------------------------------------------------*/

void GRINCOR_average_ijknbhd( MRI_shindss *shd , int dd ,
                              MCW_cluster *nbhd , int voxijk , float *avar )
{
   int nvec , nvals , ii,jj ;
   float *tsar , *vv ;

   nvals = shd->nvals[dd] ;
   nvec  = (nbhd == NULL) ? 1 : nbhd->num_pt ;
   tsar  = (float *)malloc(sizeof(float)*nvals*nvec) ;
   nvec  = GRINCOR_extract_ijknbhd( shd , dd , nbhd , voxijk , tsar ) ;

   for( ii=0 ; ii < nvals ; ii++ ) avar[ii] = 0.0f ;

   for( jj=0 ; jj < nvec ; jj++ ){
     vv = tsar + jj*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) avar[ii] += vv[ii] ;
   }

   (void)THD_normalize( nvals , avar ) ;
   free(tsar) ; return ;
}

/*----------------------------------------------------------------------------*/

void GRINCOR_prinvec_ijknbhd( MRI_shindss *shd , int dd ,
                              MCW_cluster *nbhd , int voxijk , float *pvar )
{
   int nvec , nvals , ii,jj ;
   float *tsar , *avar , *vv ;

   nvals = shd->nvals[dd] ;
   nvec  = (nbhd == NULL) ? 1 : nbhd->num_pt ;
   tsar  = (float *)malloc(sizeof(float)*nvals*nvec) ;
   nvec  = GRINCOR_extract_ijknbhd( shd , dd , nbhd , voxijk , tsar ) ;

   avar = (float *)malloc(sizeof(float)*nvals) ;
   for( ii=0 ; ii < nvals ; ii++ ) avar[ii] = 0.0f ;
   for( jj=0 ; jj < nvec ; jj++ ){
     vv = tsar + jj*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) avar[ii] += vv[ii] ;
   }

   (void)principal_vector( nvals , nvec , 0 , tsar ,
                           pvar , avar , NULL , xran ) ;
   free(avar) ; free(tsar) ; return ;
}

/*----------------------------------------------------------------------------*/
/* Load the seed vectors from each dataset:
   seed 3D voxel index (not node) is voxijk, then average over nbhd.
*//*--------------------------------------------------------------------------*/

void GRINCOR_seedvec_ijk_aver( MRI_shindss *shd , MCW_cluster *nbhd ,
                               int voxijk       , float **seedvec    )
{
   int kk ;
   for( kk=0 ; kk < shd->ndset ; kk++ )
     GRINCOR_average_ijknbhd( shd , kk , nbhd , voxijk , seedvec[kk] ) ;
   return ;
}
/*----------------------------------------------------------------------------*/

void GRINCOR_seedvec_ijk_pvec( MRI_shindss *shd , MCW_cluster *nbhd ,
                               int voxijk       , float **seedvec    )
{
   int kk ;
   for( kk=0 ; kk < shd->ndset ; kk++ )
     GRINCOR_prinvec_ijknbhd( shd , kk , nbhd , voxijk , seedvec[kk] ) ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* extract, from the dd-th dataset, the nijk vectors node-indexed in vijk[],
   into array tsar (pre-malloc-ized, please, to nvals*nijk long).
*//*--------------------------------------------------------------------------*/

void GRINCOR_extract_ijklist( MRI_shindss *shd , int dd ,
                              int nijk , int *vijk , float *tsar )
{
   int nvals , ii,jj,qq , isbyte=(shd->datum==1) ;
   short *sv=NULL , *svv=NULL ; float *vv ; sbyte *bv=NULL , *bvv=NULL ;

   nvals = shd->nvals[dd] ;
   if( isbyte ) bv = shd->bv[dd] ;
   else         sv = shd->sv[dd] ;

   for( jj=0 ; jj < nijk ; jj++ ){   /* loop over nodes */
     qq = vijk[jj] ;                 /* node index */
     vv = tsar + jj*nvals ;          /* jj-th vector in output array */
     if( isbyte ){
       bvv = bv + qq*nvals ;
       for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)bvv[ii] ;
     } else {
       svv = sv + qq*nvals ;
       for( ii=0 ; ii < nvals ; ii++ ) vv[ii] = (float)svv[ii] ;
     }
   }

   return ;
}

/*----------------------------------------------------------------------------*/

void GRINCOR_average_ijklist( MRI_shindss *shd , int dd ,
                              int nijk , int *vijk , float *avar )
{
   int ii,jj , nvals ; float *vv , *tsar ;

   nvals = shd->nvals[dd] ;
   tsar  = (float *)malloc(sizeof(float)*nvals*nijk) ;
   GRINCOR_extract_ijklist( shd , dd , nijk , vijk , tsar ) ;

   for( ii=0 ; ii < nvals ; ii++ ) avar[ii] = 0.0f ;

   for( jj=0 ; jj < nijk ; jj++ ){
     vv = tsar + jj*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) avar[ii] += vv[ii] ;
   }

   (void)THD_normalize( nvals , avar ) ;
   free(tsar) ; return ;
}

/*----------------------------------------------------------------------------*/

void GRINCOR_prinvec_ijklist( MRI_shindss *shd , int dd ,
                              int nijk , int *vijk , float *pvar )
{
   int ii,jj , nvals ; float *vv , *tsar , *avar ;

   nvals = shd->nvals[dd] ;
   tsar  = (float *)malloc(sizeof(float)*nvals*nijk) ;
   GRINCOR_extract_ijklist( shd , dd , nijk , vijk , tsar ) ;

   avar = (float *)malloc(sizeof(float)*nvals) ;
   for( ii=0 ; ii < nvals ; ii++ ) avar[ii] = 0.0f ;
   for( jj=0 ; jj < nijk ; jj++ ){
     vv = tsar + jj*nvals ;
     for( ii=0 ; ii < nvals ; ii++ ) avar[ii] += vv[ii] ;
   }

   (void)principal_vector( nvals , nijk , 0 , tsar ,
                           pvar , avar , NULL , xran ) ;
   free(avar) ; free(tsar) ; return ;
}

/*----------------------------------------------------------------------------*/
/* Load the seed vectors from each dataset, given a mask of nodes */

void GRINCOR_seedvec_ijklist_aver( MRI_shindss *shd ,
                                   int nijk , int *vijk , float **seedvec )
{
   int kk ;
   for( kk=0 ; kk < shd->ndset ; kk++ )
     GRINCOR_average_ijklist( shd , kk , nijk , vijk , seedvec[kk] ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void GRINCOR_seedvec_ijklist_pvec( MRI_shindss *shd ,
                                   int nijk , int *vijk , float **seedvec )
{
   int kk ;
   for( kk=0 ; kk < shd->ndset ; kk++ )
     GRINCOR_prinvec_ijklist( shd , kk , nijk , vijk , seedvec[kk] ) ;
   return ;
}

/*-----------------------------------------------------------------------------*/

#if 0    /* ZSS June 2011 */
  #define AFNI_NIML_PORT   53212          /* TCP/IP port that AFNI uses */
  #define SUMA_GICORR_PORT 53224          /* TCP/IP port that SUMA uses */
  /*  Replace               With
      AFNI_NIML_PORT        get_port_named("AFNI_GroupInCorr_NIML");
      SUMA_GICORR_PORT      get_port_named("SUMA_GroupInCorr_NIML");
   ZSS. June 2011 */
#endif

static int nport = 0 ;                  /* 02 Aug 2010 */

NI_stream GI_stream = (NI_stream)NULL ;

/*=============================================================================*/

static char *pname = "AFNI" ;  /* name of partner program */

void GI_exit(void)                   /* Function to be called to make sure */
{                                    /* the AFNI data channel gets closed. */
   if( GI_stream != (NI_stream)NULL ){
     fprintf(stderr,"** 3dGroupInCorr exits: closing connection to %s\n",pname) ;
     NI_stream_close(GI_stream) ; GI_stream = (NI_stream)NULL ;
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

static int   oform = SUMA_NO_DSET_FORMAT; /* output format for surface-based */

#define MAX_LABEL_SIZE 12
#define NSEND_LIMIT     9

#undef COVTEST  /* this is for Cox ONLY -- for debugging */

#define IJK_MODE     1  /* for -batch */
#define XYZ_MODE     2
#define IJKPV_MODE   3
#define XYZPV_MODE   4
#define MASKAVE_MODE 5
#define MASKPV_MODE  6
#define VECTORS_MODE 7
#define NODE_MODE    8

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
   int nvals_AAA_tot=0 , nvals_BBB_tot=0 , nvals_tot=0 , ndset_tot=0 ;
   int nvals_AAA_max=0 , nvals_BBB_max=0 , nvals_max=0 ;
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
   MRI_IMAGE *bxxim=NULL, *bxxim_psinv , *bxxim_xtxinv ;
   float **dtar=NULL ;
   int no_ttest = 0 ;  /* 02 Nov 2010 */

   int do_sendall=0 , nsaar=0 ; /* 22 Jan 2011 */
   char *bricklabels=NULL ;
   float **saar=NULL ;

   int   bmode=0 , do_lpi=0;    /* 05 Feb 2011 -- stuff for batch mode */
   char *bname=NULL ;
   char *bfile=NULL , *bprefix=NULL ;
   FILE *bfp=NULL ;
   GICOR_setup *giset=NULL ;

#ifdef COVTEST
   float *ctarA=NULL , *ctarB=NULL ; char *ctnam ;
#endif

   /*-- enlighten the ignorant and brutish sauvages with our wisdom? --*/

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
      " (-: However,  the new [Feb 2011] '-batch' option,  described far below, :-)\n"
      " (-: lets you run 3dGroupInCorr by itself, without AFNI or SUMA, writing :-)\n"
      " (-: results to disk instead of transmitting them to the client program. :-)\n"
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
      " ++ A lot of computing can be required if there are a lot of datasets\n"
      "    in the input collections.  3dGroupInCorr is carefully written to\n"
      "    be fast.  For example, on a Mac Pro with 8 3GHz CPUs, running\n"
      "    with 1.2 GBytes of data (100 datasets each with 69K voxels), each\n"
      "    group correlation map takes about 0.3 seconds to calculate and\n"
      "    transmit to AFNI -- this speed is why it's called 'Insta'.\n"
      "\n"
      "* You must start AFNI with the '-niml' option to allow it to accept\n"
      "  incoming TCP/IP socket connections.\n"
      " ++ Or you can press the 'NIML+PO' button in the GUI, if you forgot\n"
      "    to type the AFNI command line correctly.\n"
      " ++ If you are running 3dGroupInCorr and AFNI on separate computers,\n"
      "    you also have to setup 'host trusting' correctly -- for details,\n"
      "    see the description of the '-ah' option, far below.\n"
      "\n"
      "* In the AFNI 'A' controller, once 3dGroupInCorr is connected to AFNI,\n"
      "  you don't have to switch to 'GrpInCorr' on the 'Cluster' menu to\n"
      "  use the 'InstaCorr Set' controls -- unlike the individual subject\n"
      "  InstaCorr, which requires setup inside AFNI.  For Group InstaCorr,\n"
      "  the setup is already done in 3dSetupGroupInCorr.  The only reason\n"
      "  for using the 'GrpInCorr' setup controls in AFNI is to change the\n"
      "  value of the '-seedrad' option' radius interactively.\n"
      "\n"
      "* More detailed outline of processing in 3dGroupInCorr:\n"
      " ++ For each 3D+time dataset in the input dataset collections:\n"
      "   -- Extract the seed voxel time series (averaging locally per 'seedrad')\n"
      "        [you could do this manually with 3dmaskave]\n"
      "   -- Correlate it with all other voxel time series in the same dataset\n"
      "        [you could do this manually with 3dDeconvolve or 3dfim]\n"
      "   -- Result is one 3D correlation map per input dataset\n"
      " ++ Then carry out the t-test between/among these 3D correlation maps,\n"
      "      possibly allowing for dataset-level covariates.\n"
      "   -- Actually, between the arctanh() of these maps:\n"
      "       cf. RA Fisher: http://en.wikipedia.org/Fisher_transformation\n"
      "       [you could do the arctanh() conversion manually via 3dcalc;]\n"
      "       [then do the t-tests manually with 3dttest++;  then convert]\n"
      "       [the t-statistics to Z-scores using yet another 3dcalc run.]\n"
      "   -- To be overly precise, if the correlation is larger than 0.999329,\n"
      "       then the arctanh is clipped to 4.0, to avoid singularities.\n"
      "       If you consider this to be a problem, please go away.\n"
      " ++ The dataset returned to AFNI converts the t-statistic maps\n"
      "    to Z-scores, for various reasons of convenience.\n"
      "   -- Conversion is done via the same mechanism used in program\n"
      "        cdf -t2z fitt TSTAT DOF\n"
      "   -- The individual correlation maps that were t-test-ed are discarded.\n"
      "   -- Unless you use the new [Jan 2011] '-sendall' option :-)\n"
      "\n"
      "* When 3dGroupInCorr starts up, it has to 'page fault' all the data\n"
      "  into memory.  This can take several minutes, if it is reading (say)\n"
      "  10 Gbytes of data from a slow disk.  After that, if your computer\n"
      "  has enough RAM, then the program should run pretty quickly.\n"
      " ++ If your computer DOESN'T have enough RAM to hold all the data,\n"
      "    then this program will be very slow -- buy more memory!\n"
      " ++ Note that the .data file(s) are mapped directly into memory (mmap),\n"
      "    rather than being read with standard input methods.  This operation\n"
      "    may not work well on network-mounted drives, in which case you\n"
      "    will have to run 3dGroupInCorr on the same computer with the data\n"
      "    files.  However, 3dGroupInCorr does NOT need to be run on the same\n"
      "    computer as AFNI or SUMA: see the '-ah' option (far below).\n"
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
      " ++ If at all possible, use a version/compilation of 3dGroupInCorr\n"
      "    with OpenMP; the speed difference is very appreciable!\n"
#endif
      ) ;

      printf(
      "\n"
      "==================================================================\n"
      "                       COMMAND LINE OPTIONS\n"
      "==================================================================\n"
      "\n"
      "-----------------------*** Input Files ***------------------------\n"
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
      "-----------------------*** Two-Sample Options ***-----------------------\n"
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
      " -nosix    = For a 2-sample situation, the program by default computes\n"
      "             not only the t-test for the difference between the samples,\n"
      "             but also the individual (setA and setB) 1-sample t-tests, giving\n"
      "             6 sub-bricks that are sent to AFNI.  If you don't want\n"
      "             these 4 extra 1-sample sub-bricks, use the '-nosix' option.\n"
      "            ++ See the Covariates discussion, below, for an example of how\n"
      "               '-nosix' affects which covariate sub-bricks are computed.\n"
      "\n"
      " **++ None of these 'two-sample' options means anything for a 1-sample\n"
      "      t-test (i.e., where you don't use -setB).\n"
#if 0
      "\n"
      "---------------** Special Option for Ziad Saad (and His Ilk) ***--------------\n"
      "\n"
      " -no_ttest = Don't do any t-tests at all.  Just compute the correlations\n"
      "             at each voxel for each dataset and transmit those to the\n"
      "             master program (AFNI or SUMA).\n"
      "            ++ This really is a special case, and not for the normal user.\n"
#endif
      ) ;

      printf(
      "\n"
      "-----------------*** Dataset-Level Covariates [May 2010] ***-----------------\n"
      "\n"
      " -covariates cf = Read file 'cf' that contains covariates values for each dataset\n"
      "                  input (in both -setA and -setB; there can only at most one\n"
      "                  -covariates option).  Format of the file\n"
      "     FIRST LINE  -->  subject IQ   age\n"
      "     LATER LINES -->  Elvis   143   42\n"
      "                      Fred     85   59\n"
      "                      Ethel   109   49\n"
      "                      Lucy    133   32\n"
      "        This file format should be compatible with 3dMEMA.\n"
      "\n"
      "        ++ The first column contains the labels that must match the dataset\n"
      "            labels stored in the input *.grpincorr.niml files, which are\n"
      "            either the dataset prefixes or whatever you supplied in the\n"
      "            3dSetupGroupInCorr program via '-labels'.\n"
      "            -- If you ran 3dSetupGroupInCorr before this update, its output\n"
      "               .grpincorr.niml file will NOT have dataset labels included.\n"
      "               Such a file cannot be used with -covariates -- Sorry.\n"
      "\n"
      "        ++ The later columns contain numbers: the covariate values for each\n"
      "            input dataset.\n"
      "            -- 3dGroupInCorr does not allow voxel-level covariates.  If you\n"
      "               need these, you will have to use 3dttest++ on the '-sendall'\n"
      "               output (of individual dataset correlations), which might best\n"
      "               be done using '-batch' mode (cf. far below).\n"
      "\n"
      "        ++ The first line contains column headers.  The header label for the\n"
      "            first column isn't used for anything.  The later header labels are\n"
      "            used in the sub-brick labels sent to AFNI.\n"
      "\n"
      "        ++ If you want to omit some columns in file 'cf' from the analysis,\n"
      "            you can do so with the standard AFNI column selector '[...]'.\n"
      "            However, you MUST include column #0 first (the dataset labels) and\n"
      "            at least one more numeric column.  For example:\n"
      "              -covariates Cov.table'[0,2..4]'\n"
      "            to skip column #1 but keep columns #2, #3, and #4.\n"
      "\n"
      "        ++ At this time, only the -paired and -pooled options can be used with\n"
      "            covariates.  If you use -unpooled, it will be changed to -pooled.\n"
      "            -unpooled still works with a pure t-test (no -covariates option).\n"
      "            -- This restriction might be lifted in the future.  Or it mightn't.\n"
      "\n"
      "        ++ If you use -paired, then the covariates for -setB will be the same\n"
      "            as those for -setA, even if the dataset labels are different!\n"
      "            -- This restriction may be lifted in the future.  Or maybe not.\n"
      "\n"
      "        ++ By default, each covariate column in the regression matrix will have\n"
      "            its mean removed (centered). If there are 2 sets of subjects, each\n"
      "            set's matrix will be centered separately.\n"
      "            -- See the '-center' option (below) to alter this default.\n"
      "\n"
      "        ++ For each covariate, 2 sub-bricks are produced:\n"
      "            -- The estimated slope of arctanh(correlation) vs covariate\n"
      "            -- The Z-score of the t-statistic of this slope\n"
      "\n"
      "        ++ If there are 2 sets of subjects, then each pair of sub-bricks is\n"
      "            produced for the setA-setB, setA, and setB cases, so that you'll\n"
      "            get 6 sub-bricks per covariate (plus 6 more for the mean, which\n"
      "            is treated as a special covariate whose values are all 1).\n"
      "            -- At present, there is no way to tell 3dGroupInCorr not to send\n"
      "               all this information back to AFNI/SUMA.\n"
      "\n"
      "        ++ EXAMPLE:\n"
      "           If there are 2 groups of datasets (with setA labeled 'Pat', and setB\n"
      "           labeled 'Ctr'), and one covariate (labeled IQ), then the following\n"
      "           sub-bricks will be produced:\n"
      "\n"
      "       # 0: Pat-Ctr_mean    = mean difference in arctanh(correlation)\n"
      "       # 1: Pat-Ctr_Zscr    = Z score of t-statistic for above difference\n"
      "       # 2: Pat-Ctr_IQ      = difference in slope of arctanh(correlation) vs IQ\n"
      "       # 3: Pat-Ctr_IQ_Zscr = Z score of t-statistic for above difference\n"
      "       # 4: Pat_mean        = mean of arctanh(correlation) for setA\n"
      "       # 5: Pat_Zscr        = Z score of t-statistic for above mean\n"
      "       # 6: Pat_IQ          = slope of arctanh(correlation) vs IQ for setA\n"
      "       # 7: Pat_IQ_Zscr     = Z score of t-statistic for above slope\n"
      "       # 8: Ctr_mean        = mean of arctanh(correlation) for setB\n"
      "       # 9: Ctr_Zscr        = Z score of t-statistic for above mean\n"
      "       #10: Ctr_IQ          = slope of arctanh(correlation) vs IQ for setB\n"
      "       #11: Ctr_IQ_Zscr     = Z score of t-statistic for above slope\n"
      "\n"
      "        ++ However, the single-set results (sub-bricks #4-11) will NOT be\n"
      "           computed if the '-nosix' option is used.\n"
      "\n"
      "        ++ If '-sendall' is used, the individual dataset arctanh(correlation)\n"
      "           maps (labeled with '_zcorr' at the end) will be appended to this\n"
      "           list.  These setA sub-brick labels will start with 'A_' and these\n"
      "           setB labels with 'B_'.\n"
      "\n"
      "        ++ If you are having trouble getting the program to read your covariates\n"
      "           table file, then set the environment variable AFNI_DEBUG_TABLE to YES\n"
      "           and run the program -- the messages may help figure out the problem.\n"
      "           For example:\n"
      "             3dGroupInCorr -DAFNI_DEBUG_TABLE=YES -covariates cfile.txt |& more\n"
      "\n"
      "  -->>**++ A maximum of 31 covariates are allowed.  If you need more, then please\n"
      "           consider the possibility that you are completely deranged or demented.\n"
      "\n"
      " *** CENTERING ***\n"
      " Covariates are processed using linear regression.  There is one column in the\n"
      " regression matrix for each covariate, plus a column of all 1s for the mean\n"
      " value.  'Centering' refers to the process of subtracting some value from each\n"
      " number in a covariate's column, so that the fitted model for the covariate's\n"
      " effect on the data is zero at this subtracted value; the model (1 covariate) is:\n"
      "   data[i] = mean + slope * ( covariate[i] - value )\n"
      " where i is the dataset index.  The standard (default) operation is that 'value'\n"
      " is the mean of the covariate[i] numbers.\n"
      "\n"
      " -center NONE = Do not remove the mean of any covariate.\n"
      "\n"
      " -center DIFF = Each set will have the means removed separately [default].\n"
      "\n"
      " -center SAME = The means across both sets will be computed and subtracted.\n"
      "                (This option only applies to a 2-sample unpaired test.)\n"
      "\n"
      " -center VALS A.1D [B.1D]\n"
      "                This option (for Gang Chen) allows you to specify the\n"
      "                values that will be subtracted from each covariate before\n"
      "                the regression analysis.  If you use this option, then\n"
      "                you must supply a 1D file that gives the values to be\n"
      "                subtracted from the covariates; if there are 3 covariates,\n"
      "                then the 1D file for the setA datasets should have 3 numbers,\n"
      "                and the 1D file for the setB datasets (if present) should\n"
      "                also have 3 numbers.\n"
      "              * For example, to put these values directly on the command line,\n"
      "                you could do something like this:\n"
      "                  -center VALS '1D: 3 7 9' '1D: 3.14159 2.71828 0.91597'\n"
      "              * As a special case, if you want the same values used for\n"
      "                the B.1D file as in the A.1D file, you can use the word\n"
      "                'DITTO' in place of repeating the A.1D filename.\n"
      "              * Of course, you only have to give the B.1D filename if there\n"
      "                is a setB collection of datasets, and you are not doing a\n"
      "                paired t-test.\n"
      "\n"
      " Please see the discussion of CENTERING in the 3dttest++ help output.  If\n"
      " you change away from the default 'DIFF', you should really understand what\n"
      " you are doing, or an elephant may sit on your head, which no one wants.\n"
      "\n"
      "---------------------------*** Other Options ***---------------------------\n"
      "\n"
      " -seedrad r = Before performing the correlations, average the seed voxel time\n"
      "              series for a radius of 'r' millimeters.  This is in addition\n"
      "              to any blurring done prior to 3dSetupGroupInCorr.  The default\n"
      "              radius is 0, but the AFNI user can change this interactively.\n"
      "\n"
      " -sendall   = Send all individual subject results to AFNI, as well as the\n"
      "              various group statistics.\n"
      "             ++ These extra sub-bricks will be labeled like 'xxx_zcorr', where\n"
      "                'xxx' indicates which dataset the results came from; 'zcorr'\n"
      "                denotes that the values are the arctanh of the correlations.\n"
      "             ++ If there are a lot of datasets, then the results will be VERY\n"
      "                large and take up a lot of memory in AFNI.\n"
      "           **++ Use this option with some judgment and wisdom, or bad things\n"
      "                might happen! (e.g., your computer runs out of memory.)\n"
      "             ++ This option is also known as the 'Tim Ellmore special'.\n"
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
/*      " -np port = Connect to AFNI/SUMA using the TCP/IP port number given here,\n"
      "            rather than the default port number [53212 for AFNI, 53224 for\n"
      "            SUMA].  You must give the corresponding option to AFNI to\n"
      "            get proper communication going.  Using '-np' properly is the\n"
      "            only way to have multiple copies of 3dGroupInCorr and AFNI\n"
      "            talking to each other!\n"  ZSS June 2011*/
      "%s"
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
      " -suma = Talk to suma instead of afni, using surface-based i/o data.\n"
      " -sdset_TYPE = Set the output format in surface-based batch mode to\n"
      "               TYPE. For allowed values of TYPE, search for option\n"
      "               called -o_TYPE in ConvertDset -help.\n"
      "               Typical values would be: \n"
      "                  -sdset_niml, -sdset_1D, or -sdset_gii\n"
      " -quiet = Turn off the 'fun fun fun in the sun sun sun' informational messages.\n"
      " -verb  = Print out extra informational messages for more fun!\n"
      " -VERB  = Print out even more informational messages for even more fun fun!!\n"
#ifdef isfinite
      " -debug = Do some internal testing (slows things down a little)\n"
#endif
      "\n"
      "---------------*** Talairach (+trlc) vs. Original (+orig) ***---------------\n"
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
      "-----------*** Group InstaCorr and AFNI's Clusterize function ***-----------\n"
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
      "      and Clusterize, which quickly becomes very annoying.\n"
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
     , get_np_help()) ;

     printf(
     "\n"
     "--------------------------*** BATCH MODE [Feb 2011] ***-----------------------\n"
"\n"
"* In batch mode, instead of connecting AFNI or SUMA to get commands on\n"
"  what to compute, 3dGroupInCorr computes correlations (etc.) based on\n"
"  commands from an input file.\n"
"  ++ Batch mode works to produce 3D (AFNI, or NIfTI) or 2D surface-based \n"
"     (SUMA or GIFTI format) datasets. \n"
"\n"
"* Each line in the command file specifies the prefix for the output dataset\n"
"  to create, and then the set of seed vectors to use.\n"
"  ++ Each command line produces a distinct dataset.\n"
"  ++ If you want to put results from multiple commands into one big dataset,\n"
"     you will have to do that with something like 3dbucket or 3dTcat after\n"
"     running this program.\n"
"  ++ If an error occurs with one command line (e.g., a bad seed location is\n"
"     given), the program will not produce an output dataset, but will try\n"
"     to continue with the next line in the command file.\n"
"  ++ Note that I say 'seed vectors', since a distinct one is needed for\n"
"     each dataset comprising the inputs -setA (and -setB, if used).\n"
"\n"
"* Batch mode is invoked with the following option:\n"
"\n"
"   -batch METHOD COMMANDFILENAME\n"
"\n"
"  where METHOD specifies how the seed vectors are to be computed, and\n"
"  where COMMANDFILENAME specifies the file with the commands.\n"
"  ++ As a special case, if COMMANDFILENAME contains a space character,\n"
"     then instead of being interpreted as a filename, it will be used\n"
"     as the contents of a single line command file; for example:\n"
"       -batch IJK 'something.nii 33 44 55'\n"
"     could be used to produce a single output dataset named 'something.nii'.\n"
"  ++ Only one METHOD can be used per batch mode run of 3dGroupInCorr!\n"
"     You can't mix up 'IJK' and 'XYZ' modes, for example.\n"
"  ++ Note that this program WILL overwrite existing datasets, unlike most\n"
"     AFNI programs, so be careful.\n"
"\n"
"* METHOD must be one of the following strings (not case sensitive):\n"
"\n"
"  ++ IJK     ==> the 3D voxel grid index triple (i,j,k) is given in FILENAME,\n"
" or  IJKAVE      which tells the program to extract the time series from\n"
"                 each input dataset at that voxel and use that as the seed\n"
"                 vector for that dataset (if '-seedrad' is given, then the\n"
"                 seed vector will be averaged as done in interactive mode).\n"
"              ** This is the same mode of operation as the interactive seed\n"
"                 picking via AFNI's 'InstaCorr Set' menu item.\n"
"             -- FILE line format:  prefix i j k\n"
"\n"
"  ++ XYZ     ==> very similar to 'IJK', but instead of voxel indexes being\n"
" or  XYZAVE      given to specify the seed vectors, the RAI (DICOM) (x,y,z)\n"
"                 coordinates are given ('-seedrad' also applies).\n"
"              ** If you insist on using LPI (neurological) coordinates, as\n"
"                 Some other PrograMs (which are Fine Software tooLs) do,\n"
"                 set environmentment variable AFNI_INSTACORR_XYZ_LPI to YES,\n"
"                 before running this program.\n"
"             -- FILE line format:  prefix x y z\n"
"\n"
"  ++ NODE    ==> the index of the surface node where the seed is located.\n"
"                 A simple line would contain a prefix and a node number.\n"
"                 The prefix sets the output name and the file format, \n"
"                 if you include the extension. See also -sdset_TYPE option.\n"
"                 for controlling output format.\n"
"                 The node number specifies the seed node. Because you might\n"
"                 have two surfaces (-LRpairs option in 3dSetupGroupInCorr)\n"  
"                 you can add 'L', or 'R' to the node index to specify its\n"
"                 hemisphere.\n"
"                 For example:\n"
"                     OccipSeed1 L720\n"
"                     OccipSeed2 R2033\n"
"                 If you don't specify the side in instances where you are\n"
"                 working with two hemispheres, the default is 'L'.\n"
"\n"
"  ++ MASKAVE ==> each line on the command file specifies a mask dataset;\n"
"                 the nonzero voxels in that dataset are used to define\n"
"                 the list of seed voxels that will be averaged to give\n"
"                 the set of seed vectors.\n"
"              ** You can use the usual '[..]' and '<..>' sub-brick and value\n"
"                 range selectors to modify the dataset on input.  Do not\n"
"                 put these selectors inside quotes in the command file!\n"
"             -- FILE line format:  prefix maskdatasetname\n"
"\n"
"  ++ IJKPV   ==> very similar to IJKAVE, XYZAVE, and MASKAVE (in that order),\n"
"  ++ XYZPV       but instead of extracting the average over the region\n"
"  ++ MASKPV      indicated, extracts the Principal Vector (in the SVD sense;\n"
"                 cf. program 3dLocalPV).\n"
"              ** Note that IJKPV and XYZPV modes only work if seedrad > 0.\n"
"              ** In my limited tests, the differences between the AVE and PV\n"
"                 methods are very small.  YMMV.\n"
"\n"
"  ++ VECTORS ==> each line on the command file specifies an ASCII .1D\n"
"                 file which contains the set of seed vectors to use.\n"
"                 There must be as many columns in the .1D file as there\n"
"                 are input datasets in -setA and -setB combined.  Each\n"
"                 column must be as long as the maximum number of time\n"
"                 points in the longest dataset in -setA and -setB.\n"
"              ** This mode is for those who want to construct their own\n"
"                 set of reference vectors in some clever way.\n"
"              ** N.B.: This method has not yet been tested!\n"
"             -- FILE line format:  prefix 1Dfilename\n"
     ) ;

     PRINT_AFNI_OMP_USAGE("3dGroupInCorr",NULL) ;
     printf("++ Authors: Bob Cox and Ziad Saad\n") ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dGroupInCorr"); machdep();
   AFNI_logger("3dGroupInCorr",argc,argv);
   PRINT_VERSION("3dGroupInCorr"); AUTHOR("Cox, the Mad Correlator");

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

     if( strcasecmp(argv[nopt],"-suma") == 0 ){
       TalkToAfni = 0 ; nopt++ ; continue ;
     }

     if( strncasecmp(argv[nopt],"-sdset_",7) == 0 ){
       oform = SUMA_FormatFromFormString(argv[nopt]+7); 
       if (oform != SUMA_ERROR_DSET_FORMAT) {
         nopt++ ; continue ;
       } else {
         ERROR_message("Surface dataset type %s in %s unknown.\n"
                       "See option -o_TYPE in ConvertDset -help \n"
                       "for allowed values of TYPE.\n",
                       argv[nopt]+7, argv[nopt]);
         exit(1);
       }
     }

     if( strcasecmp(argv[nopt],"-nosix") == 0 ){
       nosix = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-batch") == 0 ){  /* Feb 2011 */
       if( bmode )            ERROR_exit("GIC: can't use '%s' twice!",argv[nopt]) ;
       if( ++nopt >= argc+1 ) ERROR_exit("GIC: need 2 arguments after option '%s'",argv[nopt-1]) ;
            if( strcasecmp(argv[nopt],"IJK")     == 0 ) bmode = IJK_MODE ;
       else if( strcasecmp(argv[nopt],"IJKAVE")  == 0 ) bmode = IJK_MODE ;
       else if( strcasecmp(argv[nopt],"XYZ")     == 0 ) bmode = XYZ_MODE ;
       else if( strcasecmp(argv[nopt],"XYZAVE")  == 0 ) bmode = XYZ_MODE ;
       else if( strcasecmp(argv[nopt],"IJKPV")   == 0 ) bmode = IJKPV_MODE ;
       else if( strcasecmp(argv[nopt],"XYZPV")   == 0 ) bmode = XYZPV_MODE ;
       else if( strcasecmp(argv[nopt],"MASKAVE") == 0 ) bmode = MASKAVE_MODE ;
       else if( strcasecmp(argv[nopt],"MASKPV")  == 0 ) bmode = MASKPV_MODE ;
       else if( strcasecmp(argv[nopt],"VECTORS") == 0 ) bmode = VECTORS_MODE ;
       else if( strcasecmp(argv[nopt],"NODE")    == 0 ) bmode = NODE_MODE ;
       else ERROR_exit("GIC: don't understand '-batch' method '%s'",argv[nopt]) ;
       bname = strdup(argv[nopt]) ;
       bfile = strdup(argv[++nopt]) ;
       if( strchr(bfile,' ') == NULL ){   /* if no blank inside filename */
         bfp = fopen( bfile , "r" ) ;
         if( bfp == NULL )
           ERROR_exit("GIC: can't open '-batch' file '%s'",bfile) ;
       }
       THD_force_ok_overwrite(1) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-seedrad") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
       seedrad = (float)strtod(argv[nopt],NULL) ;
       if( seedrad < 0.0f ){
         WARNING_message("GIC: Negative -seedrad being set back to zero!?") ;
         seedrad = 0.0f ;
       }
       nopt++ ; continue ;
     }

#if 0 /* This is now handled in AFNI_prefilter_args(). ZSS, June 2011
         Delete soon.                                                */
     if( strcasecmp(argv[nopt],"-np") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
       nport = (int)strtod(argv[nopt],NULL) ;
       if( nport < 1024 || nport > 65535 ){
         WARNING_message("GIC: Illegal port after '-np': should be in range 1024..65535") ;
         nport = -1 ;
       }
       nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-ah") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
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

     if( strcmp(argv[nopt],"-center") == 0 ){  /* 15 Jul 2011 */
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       switch( argv[nopt][0] ){
         case 'n': case 'N': center_code = CENTER_NONE ; break ;
         case 'd': case 'D': center_code = CENTER_DIFF ; break ;
         case 's': case 'S': center_code = CENTER_SAME ; break ;

         case 'v': case 'V':
           center_code = CENTER_VALS ;
           if( ++nopt > argc )
             ERROR_exit("Need a second argument after '%s %s'",argv[nopt-2],argv[nopt-1]) ;
           center_valimA = mri_read_1D( argv[nopt] ) ;
           if( center_valimA == NULL )
             ERROR_exit("Can't read 1D file '%s'",argv[nopt]) ;
           if( ++nopt < argc && argv[nopt][0] != '-' ){
             if( strcasecmp(argv[nopt],"DITTO") == 0 ){
               center_valimB = center_valimA ;
             } else {
               center_valimB = mri_read_1D( argv[nopt] ) ;
               if( center_valimB == NULL )
                 ERROR_exit("Can't read 1D file '%s'",argv[nopt]) ;
             }
           }
         break ;

         default:
           WARNING_message(
             "Unknown -center option '%s' -- using 'DIFF'",argv[nopt]) ;
           center_code = CENTER_DIFF ;
         break ;
       }
       nopt++ ; continue ;
     }


     if( strcasecmp(argv[nopt],"-covariates") == 0 ){  /* 20 May 2010 */
       char *lab ; float sig ; int nbad ;
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]);
       if( covnel != NULL ) ERROR_exit("GIC: can't use -covariates twice!") ;
       covnel = THD_simple_table_read( argv[nopt] ) ;
       if( covnel == NULL ){
         ERROR_message("GIC: Can't read table from -covariates file '%s'",argv[nopt]) ;
         ERROR_message("GIC: Try re-running this program with the extra option -DAFNI_DEBUG_TABLE=YES") ;
         ERROR_exit(   "GIC: Can't continue after the above error!") ;
       }
       mcov = covnel->vec_num - 1 ;
       INFO_message("GIC: Covariates file: %d columns (%d covariates), each with %d rows",
                    covnel->vec_num , mcov , covnel->vec_len ) ;
       if( mcov < 1 )
         ERROR_exit("GIC: Need at least 2 columns in -covariates file!") ;
       else if( mcov > MAXCOV )
         ERROR_exit("GIC: %d covariates in file, more than max allowed (%d)",mcov,MAXCOV) ;
       lab = NI_get_attribute( covnel , "Labels" ) ;
       if( lab != NULL ){
         ININFO_message("GIC: Covariate column labels: %s",lab) ;
         covlab = NI_decode_string_list( lab , ";," ) ;
         if( covlab == NULL || covlab->num < mcov+1 )
           ERROR_exit("GIC: can't decode labels properly?!") ;
       } else {
         ERROR_exit("GIC: Can't get labels from -covariates file '%s'",argv[nopt]) ;
       }
       for( nbad=0,kk=1 ; kk <= mcov ; kk++ ){
         meansigma_float(covnel->vec_len,(float *)covnel->vec[kk],NULL,&sig) ;
         if( sig <= 0.0f ){
           ERROR_message("GIC: Covariate '%s' is constant; how can this be used?!" ,
                         covlab->str[kk] ) ;
           nbad++ ;
         }
         if( strlen(covlab->str[kk]) > MAX_LABEL_SIZE )  /* truncate labels to fit */
           covlab->str[kk][MAX_LABEL_SIZE] = '\0' ;
       }
       if( nbad > 0 ) ERROR_exit("GIC: Cannot continue :-(") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelA") == 0 || strcasecmp(argv[nopt],"-labA") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]);
       if( argv[nopt][0] != '\0' ){
         NI_strncpy(label_AAA,argv[nopt],MAX_LABEL_SIZE) ;
         THD_filename_fix(label_AAA) ; lset_AAA = 1 ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-labelB") == 0 || strcasecmp(argv[nopt],"-labB") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]);
       if( argv[nopt][0] != '\0' ){
         NI_strncpy(label_BBB,argv[nopt],MAX_LABEL_SIZE) ;
         THD_filename_fix(label_BBB) ; lset_BBB = 1 ;
       }
       nopt++ ; continue ;
     }

#if 0
     if( strcasecmp(argv[nopt],"-useA") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
       if( use_AAA != NULL ) ERROR_exit("GIC: you can't use -useA twice!") ;
       use_AAA = MCW_get_intlist( 999999 , argv[nopt] ) ;
       if( use_AAA == NULL || use_AAA[0] <= 0 )
         ERROR_exit("GIC: can't decode argument after -useA") ;
     }

     if( strcasecmp(argv[nopt],"-useB") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
       if( use_BBB != NULL ) ERROR_exit("GIC: you can't use -useB twice!") ;
       use_BBB = MCW_get_intlist( 999999 , argv[nopt] ) ;
       if( use_BBB == NULL || use_BBB[0] <= 0 )
         ERROR_exit("GIC: can't decode argument after -useB") ;
     }
#endif

     if( strcasecmp(argv[nopt],"-setA") == 0 ){
       char *fname , *cpt ;
       if( shd_AAA != NULL ) ERROR_exit("GIC: can only use '-setA' once!") ;
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]);
       fname = strdup(argv[nopt]) ;
       if( STRING_HAS_SUFFIX(fname,".data") ){
         strcpy(fname+strlen(fname)-5,".niml") ;
         WARNING_message("GIC: Replaced '.data' with '.niml' in -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,".niml") ;
         if( verb ) INFO_message("GIC: Added '.niml' to end of -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr.") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,"niml") ;
         if( verb ) INFO_message("GIC: Added 'niml' to end of -setA filename") ;
       }
       shd_AAA = GRINCOR_read_input( fname ) ;
       if( shd_AAA == NULL ) ERROR_exit("GIC: Cannot continue after -setA input error") ;
       if( verb ) INFO_message("GIC: -setA opened, contains %d datasets, %d time series, %s bytes",
                               shd_AAA->ndset , shd_AAA->nvec , commaized_integer_string(shd_AAA->nbytes));
       qlab_AAA = fname ;
       cpt = strchr(qlab_AAA,'.') ; if( cpt != NULL && cpt != qlab_AAA ) *cpt = '\0' ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-setB") == 0 ){
       char *fname , *cpt ;
       if( shd_BBB != NULL ) ERROR_exit("GIC: can only use '-setB' once!") ;
       if( ++nopt >= argc ) ERROR_exit("GIC: need 1 argument after option '%s'",argv[nopt-1]) ;
       fname = strdup(argv[nopt]) ;
       if( STRING_HAS_SUFFIX(fname,".data") ){
         strcpy(fname+strlen(fname)-5,".niml") ;
         if( verb ) WARNING_message("GIC: Replaced '.data' with '.niml' in -setA filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,".niml") ;
         if( verb ) INFO_message("GIC: Added '.niml' to end of -setB filename") ;
       } else if( STRING_HAS_SUFFIX(fname,".grpincorr.") ){
         fname = (char *)realloc(fname,strlen(fname)+16) ;
         strcat(fname,"niml") ;
         if( verb ) INFO_message("GIC: Added 'niml' to end of -setB filename") ;
       }
       shd_BBB = GRINCOR_read_input( fname ) ;
       if( shd_BBB == NULL ) ERROR_exit("GIC: Cannot continue after -setB input error") ;
       if( verb ) INFO_message("GIC: -setB opened, contains %d datasets, %d time series, %s bytes",
                               shd_BBB->ndset , shd_BBB->nvec , commaized_integer_string(shd_BBB->nbytes));
       qlab_BBB = fname ;
       cpt = strchr(qlab_BBB,'.') ; if( cpt != NULL && cpt != qlab_BBB ) *cpt = '\0' ;
       nopt++ ; continue ;
     }

     ERROR_message("GIC: Unknown option: '%s'",argv[nopt]) ;
     suggest_best_prog_option(argv[0], argv[nopt]);
     exit(1);
   }

   /*-- check inputs for OK-ness --*/

   do_lpi = AFNI_yesenv("AFNI_INSTACORR_XYZ_LPI") ;  /* 07 Feb 2011 */

   if( 0 && bmode && !TalkToAfni ) /* Now wait a doggone minute! */
     ERROR_exit("GIC: Alas, -batch and -suma are not compatible :-(") ;

   if( seedrad == 0.0f && (bmode == IJKPV_MODE || bmode == XYZPV_MODE) ){
     char *bold=bname ;
     if( bmode == IJKPV_MODE ){ bname = "IJKAVE" ; bmode = IJK_MODE ; }
     else                     { bname = "XYZAVE" ; bmode = XYZ_MODE ; }
     WARNING_message("GIC: seedrad=0 means -batch %s is changed to %s",bold,bname) ;
   }
   
   if (bmode == NODE_MODE && seedrad != 0.0f) {
      WARNING_message("GIC: seedrad must be 0 with NODE mode.\n"
                      "     Resetting seedrad of %f to 0.0\n", seedrad);
      seedrad = 0.0;
   }  

   if( shd_AAA == NULL ) ERROR_exit("GIC:  !! You must use the '-setA' option !!") ;

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
   ndset_tot = ndset_AAA + ndset_BBB ;

   if( shd_BBB != NULL && shd_AAA->nvec != shd_BBB->nvec )
     ERROR_exit("GIC: -setA and -setB don't have same number of voxels") ;

   if( shd_BBB != NULL && strcmp(shd_AAA->dfname,shd_BBB->dfname) == 0 )
     ERROR_exit("GIC: -setA and -setB can't use the same datafile!") ;

        if( shd_BBB        == NULL           ) ttest_opcode_max = 0 ;
   else if( shd_BBB->ndset != shd_AAA->ndset ) ttest_opcode_max = 1 ;

   if( ttest_opcode < 0 || ttest_opcode > ttest_opcode_max ){
     if( shd_BBB != NULL && verb > 2 )
       INFO_message("GIC: Setting t-test option to default value of 'pooled'") ;
     ttest_opcode = 0 ;
   }

   if( ttest_opcode == 1 && mcov > 0 ){
     WARNING_message("GIC: -covariates does not support unpooled variance (yet)") ;
     ttest_opcode = 0 ;
   }

   if( shd_BBB == NULL && center_code == CENTER_SAME )  /* 15 Jul 2011 */
     center_code = CENTER_DIFF ;
   else if( shd_BBB != NULL && ttest_opcode == 2 && center_code == CENTER_SAME )
     center_code = CENTER_DIFF ;

#if 0
   /*-- attach use list to dataset collections [07 Apr 2010] --*/

   if( use_AAA != NULL ){
     int nuse = use_AAA[0] ;
     for( kk=1 ; kk <= nuse ; kk++ ){
       if( use_AAA[kk] < 0 || use_AAA[kk] >= shd_AAA->ndset )
         ERROR_exit("GIC: Index in -useAAA outside of range 0..%d",shd_AAA->ndset-1) ;
     }
     shd_AAA->nuse = nuse ;
     shd_AAA->use  = use_AAA + 1 ;
   }

   if( use_BBB != NULL && shd_BBB != NULL ){
     int nuse = use_BBB[0] ;
     for( kk=1 ; kk <= nuse ; kk++ ){
       if( use_BBB[kk] < 0 || use_BBB[kk] >= shd_BBB->ndset )
         ERROR_exit("GIC: Index in -useBBB outside of range 0..%d",shd_BBB->ndset-1) ;
     }
     shd_BBB->nuse = nuse ;
     shd_BBB->use  = use_BBB + 1 ;
   } else if( use_BBB != NULL ){
     WARNING_message("GIC: -useB was given, but -setB wasn't given!") ;
   }
#endif

   /*---------- Process covariates into matrices [23 May 2010] ----------*/

#undef  AXX
#define AXX(i,j) axx[(i)+(j)*(nA)]    /* i=0..nA-1 , j=0..mcov */
#undef  BXX
#define BXX(i,j) bxx[(i)+(j)*(nB)]    /* i=0..nB-1 , j=0..mcov */

   if( mcov > 0 ){
     int nbad=0 , nA , nB=0;
     float *ctrA=NULL , *ctrB=NULL ;
     MRI_IMARR *impr ;

     /* simmple tests to gaurd against stoopid users [is there any other kind?] */

     if( shd_AAA->dslab == NULL ){
       ERROR_message("GIC: Can't use covariates, since setA doesn't have dataset labels!") ;
       nbad++ ;
     if( shd_BBB != NULL && shd_BBB->dslab == NULL && ttest_opcode != 2 )
       ERROR_message("GIC: Can't use covariates, since setB doesn't have dataset labels!") ;
       nbad++ ;
     }

     if( ndset_AAA < mcov+3 ){
       nbad++ ;
       ERROR_message(
         "GIC: -setA has %d datasets, but you have %d covariates (max would be %d)",
         ndset_AAA,mcov,ndset_AAA-3) ;
     }
     if( ndset_BBB > 0 && ndset_BBB < mcov+3 ){
       nbad++ ;
       ERROR_message(
         "GIC: -setB has %d datasets, but you have %d covariates (max would be %d)",
         ndset_BBB,mcov,ndset_BBB-3) ;
     }

     if( nbad ) ERROR_exit("GIC: Can't continue after such simple misteaks :-(") ;

     if( verb ) INFO_message("GIC: Setting up regression matrices for covariates") ;

     /*--- setup the setA regression matrix (uncentered) ---*/

     nA    = shd_AAA->ndset ;
     axxim = mri_new( nA , mcov+1 , MRI_float ) ;
     axx   = MRI_FLOAT_PTR(axxim) ;
     for( kk=0 ; kk < shd_AAA->ndset ; kk++ ){  /* loop over datasets */
       ii = string_search( shd_AAA->dslab[kk] , /* find which covariate */
                           covnel->vec_len , (char **)covnel->vec[0] ) ;
       if( ii < 0 ){
         ERROR_message("GIC: Can't find setA dataset label '%s' in covariates file" ,
                       shd_AAA->dslab[kk] ) ;
         nbad++ ;
       } else {                   /* ii-th row of covariates == kk-th dataset */
         AXX(kk,0) = 1.0f ; /* first element in kk-th row is 1 == mean effect */
         for( jj=1 ; jj <= mcov ; jj++ )     /* later elements are covariates */
           AXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
       }
     }

     /*--- ditto for the setB matrix (uncentered), if any ---*/

     if( shd_BBB != NULL && ttest_opcode != 2 ){  /* un-paired case */
       nB    = shd_BBB->ndset ;
       bxxim = mri_new( nB , mcov+1 , MRI_float ) ;
       bxx   = MRI_FLOAT_PTR(bxxim) ;
       for( kk=0 ; kk < shd_BBB->ndset ; kk++ ){  /* loop over datasets */
         ii = string_search( shd_BBB->dslab[kk] , /* find which covariate */
                             covnel->vec_len , (char **)covnel->vec[0] ) ;
         if( ii < 0 ){
           ERROR_message("GIC: Can't find setB dataset label '%s' in covariates file" ,
                         shd_BBB->dslab[kk] ) ;
           nbad++ ;
         } else {             /* ii-th row of covariates == kk-th dataset */
           BXX(kk,0) = 1.0f ;
           for( jj=1 ; jj <= mcov ; jj++ )
             BXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
         }
       }
     }

     if( nbad )
       ERROR_exit("GIC: Can't continue past the above covariates errors :-((") ;

     /*--- setup for centering: create 1D images of the values to subtract ---*/

     switch( center_code ){

       case CENTER_VALS:
         if( center_valimA == NULL )  /* should never happenstance */
           ERROR_exit("Can't do -center VALS without a valid input image") ;
         if( center_valimA->nx < mcov )
           ERROR_exit("-center VALS setA 1D file has %d rows, but need at least %d",
                      center_valimA->nx , mcov ) ;
         if( shd_BBB != NULL && ttest_opcode != 2 ){  /* unpaired */
           if( center_valimB == NULL ){
             center_valimB = center_valimA ;
             WARNING_message("Don't have setB 1D file for -center VALS; using setA's file") ;
           } else if( center_valimB->nx < mcov ){
             ERROR_exit("-center VALS setB 1D file has %d rows, but need at least %d",
                        center_valimB->nx , mcov ) ;
           }
         }
       break ;

       case CENTER_NONE:
         center_valimA = center_valimB = mri_new(mcov,1,MRI_float) ; /* zeros */
       break ;

       case CENTER_DIFF:{
         float sum ;
         center_valimA = mri_new(mcov,1,MRI_float) ; ctrA = MRI_FLOAT_PTR(center_valimA) ;
         for( jj=1 ; jj <= mcov ; jj++ ){  /* average the columns */
           for( sum=0.0f,kk=0 ; kk < shd_AAA->ndset ; kk++ ) sum += AXX(kk,jj) ;
           ctrA[jj-1] = sum / shd_AAA->ndset ;
         }
         if( shd_BBB != NULL && ttest_opcode != 2 ){  /* unpaired */
           center_valimB = mri_new(mcov,1,MRI_float) ; ctrB = MRI_FLOAT_PTR(center_valimB) ;
           for( jj=1 ; jj <= mcov ; jj++ ){  /* average the columns */
             for( sum=0.0f,kk=0 ; kk < shd_BBB->ndset ; kk++ ) sum += BXX(kk,jj) ;
             ctrB[jj-1] = sum / shd_BBB->ndset ;
           }
         }
       }
       break ;

       case CENTER_SAME:{  /* only possible in 2 sample unpaired case */
         float sum ;
         center_valimA = mri_new(mcov,1,MRI_float) ; ctrA = MRI_FLOAT_PTR(center_valimA) ;
         for( jj=1 ; jj <= mcov ; jj++ ){  /* average the columns */
           for( sum=0.0f,kk=0 ; kk < shd_AAA->ndset ; kk++ ) sum += AXX(kk,jj) ;
           for(          kk=0 ; kk < shd_BBB->ndset ; kk++ ) sum += BXX(kk,jj) ;
           ctrA[jj-1] = sum / (shd_AAA->ndset + shd_BBB->ndset) ;
         }
         center_valimB = center_valimA ;
       }
       break ;

     } /* end of switch on center_code */

     /*--- process the matrix for setA ---*/

     ctrA = MRI_FLOAT_PTR(center_valimA) ;
     for( jj=1 ; jj <= mcov ; jj++ ){  /* center the columns */
       for( kk=0 ; kk < shd_AAA->ndset ; kk++ ) AXX(kk,jj) -= ctrA[jj-1] ;
     }
     /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
     impr = mri_matrix_psinv_pair( axxim , 0.0f ) ;
     if( impr == NULL ) ERROR_exit("GIC: Can't process setA covariate matrix?! :-(") ;
     axxim_psinv  = IMARR_SUBIM(impr,0) ; axx_psinv  = MRI_FLOAT_PTR(axxim_psinv ) ;
     axxim_xtxinv = IMARR_SUBIM(impr,1) ; axx_xtxinv = MRI_FLOAT_PTR(axxim_xtxinv) ;

     /*--- process the setB matrix ---*/

     if( shd_BBB != NULL && ttest_opcode != 2 ){  /* un-paired case */
       ctrB = MRI_FLOAT_PTR(center_valimB) ;
       for( jj=1 ; jj <= mcov ; jj++ ){  /* center the columns */
         for( kk=0 ; kk < shd_BBB->ndset ; kk++ ) BXX(kk,jj) -= ctrB[jj-1] ;
       }
       /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
       impr = mri_matrix_psinv_pair( bxxim , 0.0f ) ;
       if( impr == NULL ) ERROR_exit("GIC: Can't process setB covariate matrix?! :-(") ;
       bxxim_psinv  = IMARR_SUBIM(impr,0) ; bxx_psinv  = MRI_FLOAT_PTR(bxxim_psinv ) ;
       bxxim_xtxinv = IMARR_SUBIM(impr,1) ; bxx_xtxinv = MRI_FLOAT_PTR(bxxim_xtxinv) ;

     } else if( shd_BBB != NULL && ttest_opcode == 2 ){  /* paired case */

       bxx = axx ; bxx_psinv = axx_psinv ; bxx_xtxinv = axx_xtxinv ;

     }

   } /*---------- covariates regression matrices now setup ----------*/

   /*--- scan through all the data, which will make it be page faulted
         into RAM, which will make the correlation-izing process faster;
         the downside is that this may take quite a while, which is boring ---*/

#undef  BSTEP
#define BSTEP 256
   { long long pp , vstep=9 ; char *qv ; float sum=0.0f ;
     if( verb ) INFO_message("GIC: page faulting (reading) data into memory") ;
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
     INFO_message("GIC: total bytes input = %s (about %s)" ,
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
       ERROR_exit("GIC: Can't setup output dataset?") ; /* should never transpire */

     /* for a 2-sample test, create arrays for the 1-sample results as well */

     if( dosix ){
       NI_add_column( nelset, NI_FLOAT, NULL ); neldar_AAA = (float *)nelset->vec[2];
       NI_add_column( nelset, NI_FLOAT, NULL ); nelzar_AAA = (float *)nelset->vec[3];
       NI_add_column( nelset, NI_FLOAT, NULL ); neldar_BBB = (float *)nelset->vec[4];
       NI_add_column( nelset, NI_FLOAT, NULL ); nelzar_BBB = (float *)nelset->vec[5];
       if( neldar_AAA == NULL || nelzar_AAA == NULL ||
           neldar_BBB == NULL || nelzar_BBB == NULL   )
        ERROR_exit("GIC: Can't setup output dataset?") ; /* should never transpire */
     }

     nout = (dosix) ? 6 : 2 ;

   } else {  /* alternative setup for covariates results */

     nout = (dosix) ? 6*(mcov+1) : 2*(mcov+1) ;
     dtar = (float **)malloc(sizeof(float *)*nout) ;
     for( kk=0 ; kk < nout ; kk++ ){
       NI_add_column( nelset , NI_FLOAT , NULL ) ;
       dtar[kk] = (float *)nelset->vec[kk] ;
       if( dtar[kk] == NULL )
         ERROR_exit("GIC: Can't setup output dataset [#%d]?!",kk) ;
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
         ERROR_exit("GIC: Can't setup output dataset for -sendall [#%d]?!",kk) ;
     }
   }

   /*========= message for the user =========*/

   pname = (bmode) ? "Myself" : (TalkToAfni) ? "AFNI" : "SUMA" ;
   if( verb && !bmode ){
     INFO_message    ("GIC: --- Be sure to start %s with the '-niml' command line option",pname) ;
     if( TalkToAfni )
       ININFO_message("     ---  [or press the NIML+PO button if you forgot '-niml']") ;
   }

   /*========= this stuff is one-time-only setup of the I/O to AFNI =========*/

   atexit(GI_exit) ;             /* call this when program ends */

   signal(SIGINT ,GI_sigfunc) ;  /* setup signal handler */
   signal(SIGBUS ,GI_sigfunc) ;  /* for fatal errors */
   signal(SIGSEGV,GI_sigfunc) ;
   signal(SIGTERM,GI_sigfunc) ;

   /* name of NIML stream (socket) to open */

   if( !bmode ){
     if( nport <= 0 ) {
      nport = (TalkToAfni) ?  get_port_named("AFNI_GroupInCorr_NIML") :
                              get_port_named("SUMA_GroupInCorr_NIML") ;
     }
     sprintf( nsname , "tcp:%s:%d" , afnihost , nport ) ;

     /* open the socket (i.e., dial the telephone call) */

     fprintf(stderr,"++ Opening NIML socket '%s' to %s",nsname,pname) ;
     GI_stream = NI_stream_open( nsname , "w" ) ;

     /* loop until AFNI connects (answers the call),
        printing '.' every so often to keep the user mollified and distracted */

     for( nn=0 ; nn < 234 ; nn++ ){  /* don't loop forever, though */
       fprintf(stderr,".") ;
       kk = NI_stream_writecheck( GI_stream , 999 ) ; /* check for connection */
       if( kk == 1 ){ fprintf(stderr," Connected!\n") ; break ; }    /* good! */
       if( kk <  0 ){ fprintf(stderr," ** Connection fails :-(\n") ; exit(1) ; }
     }
     if( kk <= 0 ){ fprintf(stderr," ** Connection times out :-(\n"); exit(1); }
   }

   /** store some info about the dataset we are constructing **/

   nx = DSET_NX(shd_AAA->tdset) ; dx = fabsf(DSET_DX(shd_AAA->tdset)) ;
   ny = DSET_NY(shd_AAA->tdset) ; dy = fabsf(DSET_DY(shd_AAA->tdset)) ;
   nz = DSET_NZ(shd_AAA->tdset) ; dz = fabsf(DSET_DZ(shd_AAA->tdset)) ;
   dmin = MIN(dx,dy) ; dmin = MIN(dmin,dz) ; nxy = nx*ny ;

   /** now create an element to describe our setup info to AFNI **/

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
     strcat(bricklabels,";") ;           /* 01 Feb 2011: oopsie */
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

   /* ZSS: set surface attributes [note Ziad's TERRIBLE use of spaces] 
            Perhaps, but   at least      he wraps at 80             . */

   if (shd_AAA->nnode[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->nnode[0], shd_AAA->nnode[1]);
      NI_set_attribute( nelcmd , "LRpair_nnode", buf);
   }
   if (shd_AAA->ninmask[0] >= 0) {
      sprintf(buf,"%d, %d", shd_AAA->ninmask[0], shd_AAA->ninmask[1]);
      NI_set_attribute( nelcmd , "LRpair_ninmask", buf);
   }

   /*-- either tell AFNI what's going on, or setup stuff ourselves --*/

   if( !bmode ){  /* actually send the setup NIML element now */

     if( verb > 1 ) INFO_message("GIC: Sending setup information to %s",pname) ;
     nn = NI_write_element( GI_stream , nelcmd , NI_BINARY_MODE ) ;
     if( nn < 0 ){
       ERROR_exit("GIC: Can't send setup data to %s!?",pname) ;
     }

   } else {       /* batch mode ==> setup internally */
     if (bmode != NODE_MODE) { 
      giset = GRINCOR_setup_dataset( nelcmd ) ;
      if( giset == NULL )
         ERROR_exit("Can't setup batch mode dataset for some reason :-(") ;
     } else {
      giset = (GICOR_setup*)calloc(1,sizeof(GICOR_setup)) ;
      if (!SUMA_init_GISET_setup(NULL , nelcmd, giset)) {
         ERROR_exit("Failed to setup batch mode dataset for some reason >:-(") ; 
      }
     }  
   }

   NI_free_element(nelcmd) ;  /* setup is done (here or there) */
   nelcmd = NULL ;

   /** make neighborhood struct for seedrad usage **/

   if( seedrad >= dmin ){
     nbhd = MCW_spheremask( dx,dy,dz , seedrad ) ;
     if( nbhd != NULL && nbhd->num_pt < 2 ) KILL_CLUSTER(nbhd) ;
     if( nbhd != NULL && verb > 1 )
       INFO_message("GIC: seedrad=%g neighborhood contains %d voxels" ,
                    seedrad , nbhd->num_pt ) ;
   }

   /** make space for seed vectors and arctanh(correlations) **/

   seedvec_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   dotprod_AAA = (float **)malloc(sizeof(float *)*ndset_AAA) ;
   nvals_AAA_max = shd_AAA->nvals_max ;
   nvals_AAA_tot = shd_AAA->nvals_tot ;
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
     nvals_BBB_max = shd_BBB->nvals_max ;
     nvals_BBB_tot = shd_BBB->nvals_tot ;
     for( kk=0 ; kk < ndset_BBB ; kk++ ){
       seedvec_BBB[kk] = (float *)malloc(sizeof(float)*nvals_BBB[kk]) ;
       if( nsaar == 0 )
         dotprod_BBB[kk] = (float *)malloc(sizeof(float)*nvec) ;
       else
         dotprod_BBB[kk] = saar[kk+ndset_AAA] ;
     }
   }
   nvals_tot = nvals_AAA_tot + nvals_BBB_tot ;
   nvals_max = MAX( nvals_AAA_max , nvals_BBB_max ) ;

   if( verb ){
     INFO_message("3dGroupInCorr stands ready to do thy bidding, O Master :-) !!") ;
#ifdef USE_OMP
#pragma omp parallel
 {
  if( omp_get_thread_num() == 0 )
    ININFO_message("GIC: OpenMP thread count = %d",omp_get_num_threads()) ;
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

     if( !bmode ){   /* read command from AFNI or SUMA */
       nelcmd = NI_read_element( GI_stream , 333 ) ;

       atim = btim = NI_clock_time() ;  /* start timer, for user info */

       /* nada?  check if something is bad */

       if( nelcmd == NULL ){
         kk = NI_stream_goodcheck( GI_stream , 1 ) ;
         if( kk < 1 ){
           NI_stream_close(GI_stream) ; GI_stream = (NI_stream)NULL ;
           WARNING_message("GIC: Connection to %s broken - trying to restart",pname) ;
           NI_sleep(111) ;                /* give AFNI a moment to do whatever */
           GI_stream = NI_stream_open( nsname , "w" ) ;
           kk = NI_stream_goodcheck( GI_stream , 9999 ) ; /* wait a little bit */
           if( kk == 1 ){
             ININFO_message("GIC: TCP/IP restart is good :-)") ; shm_active = 0 ;
           } else {
             ININFO_message("GIC: TCP/IP restart failed :-(") ;
             NI_stream_close(GI_stream) ; GI_stream = (NI_stream)NULL ;
             goto GetOutOfDodge ;  /* failed */
           }
         }
         continue ; /* loop back */
       }

     } else {   /* create command internally in batch mode [Feb 2011] */
       char cline[6666], buf[666] , *cpt ; static int nbatch=0 ;
       static NI_str_array *bsar=NULL ;
       static int nwarn=0;
       
       if( bfp == NULL && nbatch > 0 ) goto GetOutOfDodge ;  /* done */

       atim = btim = NI_clock_time() ;  /* start timer, for user info */

       if( bfp == NULL ){   /* extract command from FILENAME itself */
         strcpy( cline , bfile ) ;
       } else {             /* read next line in file */
         cpt = afni_fgets( cline , 6666 , bfp ) ;
         if( cpt == NULL ){
           fclose(bfp) ; goto GetOutOfDodge ;  /* end of file (or error) */
         }
       }

       if( bsar != NULL ) NI_delete_str_array(bsar) ;
       bsar = NI_decode_string_list( cline , "`" ) ;
       if( bsar == NULL || bsar->num < 2 ){
         ERROR_message("GIC: bad batch command line: too short") ;
         goto LoopBack ;
       }

       bprefix = bsar->str[0] ;
       if( !THD_filename_ok(bprefix) ){
         ERROR_message("GIC: bad batch command line: bad prefix") ;
         goto LoopBack ;
       }

       /* process the rest of the command line (broken up into bsar) */

       switch( bmode ){  /* each mode must create the correct nelcmd NI_element */
         default:
           ERROR_message("GIC: you should never see this message!"); goto GetOutOfDodge;
         
         case NODE_MODE:
            if( bsar->num != 2) {
               ERROR_message("GIC: bad batch command line: "
                             "%s list must have exactly two strings",bname) ;
               goto LoopBack ;
            }
            nelcmd = NI_new_data_element( "SETREF_ijk" , 0 ) ;
            nn = strlen(bsar->str[1]);
                   if (bsar->str[1][0]=='r' || bsar->str[1][0]=='R') {
               qijk = (int)strtod(bsar->str[1]+1,NULL)+giset->nnode_domain[0];
            } else if (bsar->str[1][nn-1]=='r' || bsar->str[1][nn-1]=='R') {
               bsar->str[1][nn-1]='\0';
               qijk = (int)strtod(bsar->str[1],NULL)+giset->nnode_domain[0] ;
            } else if (bsar->str[1][0]=='l' || bsar->str[1][0]=='L') {
               qijk = (int)strtod(bsar->str[1]+1,NULL);
            } else if (bsar->str[1][nn-1]=='l' || bsar->str[1][nn-1]=='L') {
               bsar->str[1][nn-1]='\0';
               qijk = (int)strtod(bsar->str[1],NULL);
            } else {
               if (!nwarn && giset->nnode_domain[1] > 0) {
                  WARNING_message("You are using two surfaces but your\n"
                              "node index of %s does not specify to which\n"
                              "hemishphere it belongs. While the default\n"
                              "is always for the left hemisphere, you are\n"
                              "better off adding an 'L' to the index for\n"
                              "clarity so your line would read:\n"
                              "   %s %sL\n"
                              "or"
                              "   %s L%s\n"
                              "This message is only shown once.\n",
                               bsar->str[1], 
                               bsar->str[0], bsar->str[1],
                               bsar->str[0], bsar->str[1]);
                  
               }
               qijk = (int)strtod(bsar->str[1],NULL);
            }
            sprintf( buf , "%d" , qijk ) ;
            NI_set_attribute( nelcmd , "index" , buf ) ;
            break ;
            
         case XYZPV_MODE:   /* x y z */
         case XYZ_MODE:     /* x y z */
         case IJKPV_MODE:   /* i j k */
         case IJK_MODE:{    /* i j k */
           int i,j,k ; float x=0.0,y=0.0,z=0.0 ; char *cname ;
           if( bsar->num < 4 ){
             ERROR_message("GIC: bad batch command line: %s list too short",bname) ;
             goto LoopBack ;
           }
           if( bmode == IJK_MODE || bmode == IJKPV_MODE ){
             i = (int)strtod(bsar->str[1],NULL) ;
             j = (int)strtod(bsar->str[2],NULL) ;
             k = (int)strtod(bsar->str[3],NULL) ;
           } else {
             float fi,fj,fk ;
             x = (float)strtod(bsar->str[1],NULL) ; if( do_lpi ) x = -x ;
             y = (float)strtod(bsar->str[2],NULL) ; if( do_lpi ) y = -y ;
             z = (float)strtod(bsar->str[3],NULL) ;
             MAT44_VEC( giset->dset->daxes->dicom_to_ijk , x,y,z , fi,fj,fk ) ;
             i = (int)rintf(fi) ; j = (int)rintf(fj) ; k = (int)rintf(fk) ;
           }
           if( i < 0 || i >= nx ||
               j < 0 || j >= ny ||
               k < 0 || k >= nz   ){
             if( bmode == IJK_MODE || bmode == IJKPV_MODE )
               ERROR_message("GIC: bad batch command line: %s (%d,%d,%d) illegal",bname,i,j,k);
             else {
               if( do_lpi ){ x = -x ; y = -y ; }
               ERROR_message("GIC: bad batch command line: %s (%g,%g,%g) illegal",bname,x,y,z);
             }
             goto LoopBack ;
           }
           qijk = THREE_TO_IJK(i,j,k,nx,nxy) ;
           if( bmode == XYZPV_MODE || bmode == IJKPV_MODE ) cname = "SETREF_ijk_pv" ;
           else                                             cname = "SETREF_ijk"    ;
           nelcmd = NI_new_data_element( cname , 0 ) ;
           sprintf( buf , "%d" , qijk ) ;
           NI_set_attribute( nelcmd , "index" , buf ) ;
         }
         break ;

         case MASKPV_MODE:   /* maskdataset */
         case MASKAVE_MODE:{
           THD_3dim_dataset *mset; int nmask,*ijklist; byte *mask; char *cname;
           mset = THD_open_dataset( bsar->str[1] ) ;
           if( mset == NULL ){
             ERROR_message("GIC: bad batch command line: can't open mask dataset '%s'",bsar->str[1]) ;
             goto LoopBack ;
           }
           if( shd_AAA->nvox != DSET_NVOX(mset) ){
             ERROR_message("GIC: bad batch command line: mask dataset '%s' is wrong size",bsar->str[1]) ;
             DSET_delete(mset) ; goto LoopBack ;
           }
           DSET_load(mset) ;
           if( !DSET_LOADED(mset) ){
             ERROR_message("GIC: bad batch command line: can't load dataset '%s'",bsar->str[1]) ;
             DSET_delete(mset) ; goto LoopBack ;
           }
           mask  = THD_makemask( mset , 0 , 1.0f,-1.0f ) ; DSET_delete(mset) ;
           nmask = THD_countmask( shd_AAA->nvox , mask ) ;
           if( nmask == 0 ){
             ERROR_message("GIC: bad batch command line: dataset '%s' mask is all zero",bsar->str[1]) ;
             free(mask) ; goto LoopBack ;
           }
           if( bmode == MASKPV_MODE && nmask > 1 ) cname = "SETREF_listijk_pv" ;
           else                                    cname = "SETREF_listijk"    ;
           nelcmd = NI_new_data_element( cname , nmask ) ;
           NI_add_column( nelcmd, NI_INT, NULL );
           ijklist = (int *)nelcmd->vec[0] ;
           for( kk=ii=0 ; ii < shd_AAA->nvox ; ii++ )
             if( mask[ii] ) ijklist[kk++] = ii ;
           free(mask) ;
         }
         break ;

         case VECTORS_MODE:{   /* 1Dfilename */
           MRI_IMAGE *vim ; int ntim,nvec,qq ; float *var,*iar,*jar ;

           vim = mri_read_1D( bsar->str[1] ) ;
           if( vim == NULL ){
             ERROR_message("GIC: bad batch command line: can't open 1D file '%s'",bsar->str[1]) ;
             goto LoopBack ;
           }
           ntim = vim->nx ; nvec = vim->ny ; iar = MRI_FLOAT_PTR(vim) ;
           if( ntim < nvals_max || nvec < ndset_tot ){
             ERROR_message("GIC: bad batch command line: 1D file '%s' is %d X %d, but should be at least %d X %d",
                           bsar->str[1] , ntim,nvec , nvals_max , ndset_tot ) ;
             mri_free(vim) ; goto LoopBack ;
           }
           nelcmd = NI_new_data_element( "SETREF_vectors" , nvals_tot ) ;
           NI_add_column( nelcmd , NI_FLOAT , NULL ) ;
           var = (float *)nelcmd->vec[0] ;
           for( qq=kk=0 ; kk < ndset_AAA ; kk++ ){
             jar = iar + kk*ntim ;
             for( ii=0 ; ii < shd_AAA->nvals[kk] ; ii++ ) var[qq++] = jar[ii] ;
           }
           for( kk=0 ; kk < ndset_BBB ; kk++ ){
             jar = iar + (kk+ndset_AAA)*ntim ;
             for( ii=0 ; ii < shd_BBB->nvals[kk] ; ii++ ) var[qq++] = jar[ii] ;
           }
           mri_free(vim) ;
         }
         break ;

       } /* end of switch over the various batch modes */

       nbatch++ ;  /* keep track of how many we've done */

       if( verb > 2 ) INFO_message("GIC: generated %s command",nelcmd->name) ;

     }

     /* the following should never happen */

     if( NI_element_type(nelcmd) != NI_ELEMENT_TYPE ){
       WARNING_message("GIC: Badly formatted command from %s!",pname) ;
       NI_free_element(nelcmd) ; continue ;
     }

     /* do something with the command, based on the element name */

     /** Command = AFNI said 'TaTa for Now' **/

     if( strcmp(nelcmd->name,"AuRevoir") == 0 ){
       INFO_message("GIC: Message from %s: ** Au Revoir **",pname) ;
       NI_free_element(nelcmd) ;
       goto GetOutOfDodge ;  /* failed */
     }

     /**----- step 1: process command to get seed vectors -----**/

     if( verb > 1 || (verb==1 && nsend < NSEND_LIMIT) )
       INFO_message("GIC: Received command %s from %s",nelcmd->name,pname) ;

     /**----- Command = set seed voxel index (and maybe radius) -----**/

     if( strncmp(nelcmd->name,"SETREF_ijk",10) == 0 ){
       int do_pv = (strstr(nelcmd->name,"_pv") != NULL && nbhd != NULL) ;

       /* extract location of seed voxel from command */

                         atr = NI_get_attribute(nelcmd,"index") ;
       if( atr == NULL ) atr = NI_get_attribute(nelcmd,"node" ) ;
       if( atr == NULL ) atr = NI_get_attribute(nelcmd,"ijk"  ) ;
       if( atr == NULL ){   /* should never happen */
         ERROR_message("GIC: %s: no index given!?",nelcmd->name) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }
       voxijk = (int)strtod(atr,NULL) ;
       voxind = IJK_TO_INDEX(shd_AAA,voxijk) ;
       if( verb > 2 )
         ININFO_message("GIC:  dataset index=%d  node index=%d",voxijk,voxind) ;
       if( voxind < 0 ){
         ERROR_message("GIC: %s: %d is not in mask!?",nelcmd->name,voxijk) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }

       /* change radius over which to average? */

       atr = NI_get_attribute(nelcmd,"seedrad") ;
       if( atr != NULL ){
         float nsr = (float)strtod(atr,NULL) ;
         if( nsr != seedrad ){
           seedrad = MAX(0.0f,nsr) ; KILL_CLUSTER(nbhd) ;
           if( seedrad >= dmin ){
             nbhd = MCW_spheremask( dx,dy,dz , seedrad ) ;
             if( nbhd != NULL && nbhd->num_pt < 2 ) KILL_CLUSTER(nbhd) ;
           } else {
             seedrad = 0.0f ;
           }
           if( verb > 2 )
             ININFO_message("GIC:  seedrad set to %.2f mm",seedrad) ;
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
           ININFO_message("GIC:  ttest_opcode set to %d",ttest_opcode) ;
       }
#endif

       /* actually get the seed vectors from this voxel */

       if( do_pv ){
         GRINCOR_seedvec_ijk_pvec( shd_AAA , nbhd , voxijk , seedvec_AAA ) ;
         if( shd_BBB != NULL )
           GRINCOR_seedvec_ijk_pvec( shd_BBB , nbhd , voxijk , seedvec_BBB ) ;
       } else {
         GRINCOR_seedvec_ijk_aver( shd_AAA , nbhd , voxijk , seedvec_AAA ) ;
         if( shd_BBB != NULL )
           GRINCOR_seedvec_ijk_aver( shd_BBB , nbhd , voxijk , seedvec_BBB ) ;
       }

     /**----- command contains all the seed vectors directly [Feb 2011] -----**/

     } else if( strcmp(nelcmd->name,"SETREF_vectors") == 0 ){
       float *cv ;

       if( nelcmd->vec_num < 1 ){
         ERROR_message("GIC: SETREF_vectors: no vectors attached!?") ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }
       if( nelcmd->vec_typ[0] != NI_FLOAT ){
         ERROR_message("GIC: SETREF_vectors: not in float format!?") ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }

       /*--- load data from nelcmd to seedvec arrays ---*/

       if( nelcmd->vec_num == 1 ){  /*--- one long vector: split it up ---*/

         if( nelcmd->vec_len < nvals_tot ){
           ERROR_message("GIC: SETREF_vectors: 1 vector length=%d but should be %d",
                         nelcmd->vec_len , nvals_tot ) ;
           NI_free_element(nelcmd) ; goto LoopBack ;
         }

         cv = (float *)nelcmd->vec[0] ;
         for( jj=kk=0 ; kk < ndset_AAA ; kk++ ){
           for( ii=0 ; ii < nvals_AAA[kk] ; ii++,jj++ ) seedvec_AAA[kk][ii] = cv[jj] ;
         }
         for( kk=0 ; kk < ndset_BBB ; kk++ ){
           for( ii=0 ; ii < nvals_BBB[kk] ; ii++,jj++ ) seedvec_BBB[kk][ii] = cv[jj] ;
         }

       } else if( nelcmd->vec_num >= ndset_tot ){  /*--- multiple vectors ---*/

         if( nelcmd->vec_len < nvals_max ){
           ERROR_message("GIC: SETREF_vectors: vector length=%d but should be %d",
                         nelcmd->vec_len , nvals_max ) ;
           NI_free_element(nelcmd) ; goto LoopBack ;
         }

         for( kk=0 ; kk < ndset_AAA ; kk++ ){
           cv = (float *)nelcmd->vec[kk] ;
           for( ii=0 ; ii < nvals_AAA[kk] ; ii++ ) seedvec_AAA[kk][ii] = cv[ii] ;
         }
         for( kk=0 ; kk < ndset_BBB ; kk++ ){
           cv = (float *)nelcmd->vec[kk+ndset_AAA] ;
           for( ii=0 ; ii < nvals_BBB[kk] ; ii++ ) seedvec_BBB[kk][ii] = cv[ii] ;
         }

       } else {        /*--- badly formed data element ---*/

         ERROR_message("GIC: SETREF_vectors: have %d vectors but need at least %d",
                       nelcmd->vec_num , ndset_tot ) ;
         NI_free_element(nelcmd) ; goto LoopBack ;

       }

       /*--- normalize vectors for dot productization ---*/

       for( kk=0 ; kk < ndset_AAA ; kk++ )
         (void)THD_normalize( nvals_AAA[kk] , seedvec_AAA[kk] ) ;
       for( kk=0 ; kk < ndset_BBB ; kk++ )
         (void)THD_normalize( nvals_BBB[kk] , seedvec_BBB[kk] ) ;

     /**----- command contains a list of voxels to use [Feb 2011] -----**/

     } else if( strncmp(nelcmd->name,"SETREF_listijk",14) == 0 ){
       int *vijk , nijk , vv ;
       int do_pv = (strstr(nelcmd->name,"_pv") != NULL) ;

       if( nelcmd->vec_num < 1 || nelcmd->vec_len < 1 ){
         ERROR_message("GIC: %s: no list attached!?",nelcmd->name) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }
       if( nelcmd->vec_typ[0] != NI_INT ){
         ERROR_message("GIC: %s: not in int format!?",nelcmd->name) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }

       /* convert voxel indexes to node indexes (in place) */

       vijk = (int *)nelcmd->vec[0] ;
       for( nijk=ii=0 ; ii < nelcmd->vec_len ; ii++ ){
         vv = IJK_TO_INDEX(shd_AAA,vijk[ii]) ;
         if( vijk[ii] >= 0 ) vijk[nijk++] = vv ;
       }

       if( nijk == 0 ){
         ERROR_message("GIC: %s: no good indexes found!",nelcmd->name) ;
         NI_free_element(nelcmd) ; goto LoopBack ;
       }

       if( verb > 2 ) ININFO_message("GIC:  %s: %d good vectors in list",bname,nijk) ;

       /* actually get the seed vectors from this list */

       if( do_pv && nijk > 1 ){
         GRINCOR_seedvec_ijklist_pvec( shd_AAA , nijk , vijk , seedvec_AAA ) ;
         if( shd_BBB != NULL )
           GRINCOR_seedvec_ijklist_pvec( shd_BBB , nijk , vijk , seedvec_BBB ) ;
       } else {
         GRINCOR_seedvec_ijklist_aver( shd_AAA , nijk , vijk , seedvec_AAA ) ;
         if( shd_BBB != NULL )
           GRINCOR_seedvec_ijklist_aver( shd_BBB , nijk , vijk , seedvec_BBB ) ;
       }

     /**----- unknown command type -----**/

     } else {

       ERROR_message("GIC: Don't know command %s",nelcmd->name) ;
       NI_free_element(nelcmd) ;
       goto LoopBack ;

     }

     /**--- throw away the message from AFNI ---**/

     NI_free_element( nelcmd ) ;

     /***** compute the result *****/

     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
       ctim = NI_clock_time() ;
       ININFO_message("GIC:  loaded seed vectors: elapsed=%d ms",ctim-btim) ;
       btim = ctim ;
     }

     /* step 2: lots and lots of correlation-ization */

     if( verb > 3 ) ININFO_message("GIC:  start correlation-izing for %s",label_AAA) ;
     GRINCOR_many_dotprod( shd_AAA , seedvec_AAA , dotprod_AAA ) ;
     if( shd_BBB != NULL ){
       if( verb > 3 ) ININFO_message("GIC:  start correlation-izing for %s",label_BBB) ;
       GRINCOR_many_dotprod( shd_BBB , seedvec_BBB , dotprod_BBB ) ;
     }

#if 0
     if( verb > 4 ){
       float mm,ss ; int nf ;
       INFO_message("GIC: dotprod_AAA statistics") ;
       for( kk=0 ; kk < ndset_AAA ; kk++ ){
          nf = thd_floatscan( nvec , dotprod_AAA[kk] ) ;
          meansigma_float( nvec , dotprod_AAA[kk] , &mm,&ss ) ;
          ININFO_message("GIC:  #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
       }
       if( ndset_BBB > 0 ){
         INFO_message("GIC: dotprod_BBB statistics") ;
         for( kk=0 ; kk < ndset_BBB ; kk++ ){
            nf = thd_floatscan( nvec , dotprod_BBB[kk] ) ;
            meansigma_float( nvec , dotprod_BBB[kk] , &mm,&ss ) ;
            ININFO_message("GIC:  #%02d nf=%d mean=%g sigma=%g",kk,nf,mm,ss) ;
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
       ININFO_message("GIC:  finished correlation-izing: elapsed=%d ms",ctim-btim) ;
       btim = ctim ;
     }

     /* step 3: lots of t-test-ification */

     if( mcov == 0 ){   /*-- no covariates ==> pure t-tests --*/

       if( verb > 3 )
         ININFO_message("GIC:  start %d-sample t-test-izing" , (ndset_BBB > 0) ? 2 : 1 ) ;
       GRINCOR_many_ttest( nvec , ndset_AAA , dotprod_AAA ,
                                  ndset_BBB , dotprod_BBB , neldar,nelzar ) ;

       /* 1-sample results for the 2-sample case? */

       if( dosix ){
         if( verb > 3 ) ININFO_message("GIC:  start 1-sample t-test-izing for %s",label_AAA) ;
         GRINCOR_many_ttest( nvec , ndset_AAA , dotprod_AAA ,
                                    0         , NULL        , neldar_AAA,nelzar_AAA ) ;
         if( verb > 3 ) ININFO_message("GIC:  start 1-sample t-test-izing for %s",label_BBB) ;
         GRINCOR_many_ttest( nvec , ndset_BBB , dotprod_BBB ,
                                    0         , NULL        , neldar_BBB,nelzar_BBB ) ;
       }

       if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message("GIC:  finished t-test-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     } else {  /*-- covariates ==> regression analyses --*/

       if( verb > 3 )
         ININFO_message("GIC:  start %d-sample regression-izing" , (ndset_BBB > 0) ? 2 : 1 ) ;

       GRINCOR_many_regress( nvec , ndset_AAA , dotprod_AAA ,
                                    ndset_BBB , dotprod_BBB , nout , dtar ) ;

       if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) ){
         ctim = NI_clock_time() ;
         ININFO_message("GIC:  finished regression-izing: elapsed=%d ms",ctim-btim) ;
         btim = ctim ;
       }

     }

     /*** test results to see if they are all zero! [18 Oct 2011] ***/

     if( verb > 1 || nsend < NSEND_LIMIT ){
       int nv = nelset->vec_num ;  /* # of columns */
       int nr = nelset->vec_len ;  /* # of rows */
       float *vv ;
       for( kk=0 ; kk < nv ; kk++ ){
         vv = (float *)nelset->vec[kk] ;
         for( ii=0 ; ii < nr && vv[ii] == 0.0f ; ii++ ) ; /*nada*/
         if( ii == nr )
           WARNING_message("GIC: sub-brick #%d of output is all zero!",kk) ;
       }
     }

#ifndef DONT_USE_SHM
     /** re-attach to AFNI using shared memory? **/

     if( !bmode && do_shm > 0 && strcmp(afnihost,"localhost") == 0 && !shm_active ){
       char nsnew[128] ;
       kk = (nout+nsaar) / 2 ; if( kk < 1 ) kk = 1 ; else if( kk > 3 ) kk = 3 ;
             /* using nport in nsnew below is no longer necessary,
                but it does not hurt. ZSS June 2011               */
       sprintf( nsnew , "shm:GrpInCorr_%d:%dM+4K" , nport , kk ) ;
       INFO_message("GIC: Reconnecting to %s with shared memory channel %s",pname,nsnew) ;
       kk = NI_stream_reopen( GI_stream , nsnew ) ;
       if( kk == 0 ){
         ININFO_message("GIC:  SHM reconnection *FAILED* :-( ???") ;
       }
       else {
         ININFO_message("GIC:  SHM reconnection *ACTIVE* :-) !!!") ; shm_active = 1 ;
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

     if( !bmode ){
       if( verb > 3 ) ININFO_message("GIC:  sending results to %s",pname) ;
       kk = NI_write_element( GI_stream , nelset , NI_BINARY_MODE ) ;
       if( kk <= 0 ){
         ERROR_message("3dGroupInCorr: failure when writing to %s",pname) ;
       }
     } else {
       kk = GRINCOR_output_dataset( giset, nelset , bprefix ) ;
     }

     ctim = NI_clock_time() ;
     if( verb > 2 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message("GIC:  sent results to %s: elapsed=%d ms  bytes=%s" ,
                      pname , ctim-btim , commaized_integer_string(kk) ) ;

     if( verb > 1 || (verb==1 && nsend < NSEND_LIMIT) )
       ININFO_message("GIC:  Total elapsed time = %d msec",ctim-atim) ;

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

/*---------------------------------------------------------------------------*/
/* Setup for creating AFNI datasets here, instead of in AFNI.     [Feb 2011] */

GICOR_setup * GRINCOR_setup_dataset( NI_element *nel )
{
   char *atr , *pre ;
   THD_3dim_dataset *dset ; GICOR_setup *giset ;
   int nvals=2 , vv ;
   static char *blab[6] = { "GIC_Delta" , "GIC_Zscore" ,
                            "AAA_Delta" , "AAA_Zscore" ,
                            "BBB_Delta" , "BBB_Zscore"  } ;
   NI_str_array *labar=NULL ;

   atr = NI_get_attribute( nel , "geometry_string" ); if( atr  == NULL ) return NULL;
   pre = NI_get_attribute( nel , "target_name" ) ;
   if( pre == NULL || *pre == '\0' ) pre = "X_GRP_ICORR" ;
   dset = EDIT_geometry_constructor( atr , pre ) ;    if( dset == NULL ) return NULL;

   giset = (GICOR_setup *)calloc(1, sizeof(GICOR_setup)) ;
   giset->dset = dset ;
   giset->nvox = DSET_NVOX(dset) ;

   atr = NI_get_attribute( nel , "target_nvals" ) ;
   if( atr != NULL ){ nvals = (int)strtod(atr,NULL); nvals = MAX(1,nvals); }
   vv = AFNI_yesenv("AFNI_GROUPINCORR_ORIG") ;
   EDIT_dset_items( dset , ADN_nvals     , nvals ,
                           ADN_view_type , (vv) ? VIEW_ORIGINAL_TYPE
                                                : VIEW_TALAIRACH_TYPE ,
                           ADN_brick_fac , NULL ,
                    ADN_none ) ;

   atr = NI_get_attribute( nel , "target_labels" ) ;
   if( atr != NULL )
     labar = NI_decode_string_list( atr , ";" ) ;

   /* for each sub-brick in the dataset-to-be */

   for( vv=0 ; vv < nvals ; vv++ ){
     EDIT_substitute_brick( dset, vv, MRI_float, NULL ) ; /* calloc sub-brick */
     if( labar != NULL && vv < labar->num )               /* and label-ize it */
       EDIT_BRICK_LABEL( dset , vv , labar->str[vv] ) ;
     else if( vv < 6 )
       EDIT_BRICK_LABEL( dset , vv , blab[vv] ) ;
     if( strstr( DSET_BRICK_LAB(dset,vv) , "_Zsc" ) != NULL )
       EDIT_BRICK_TO_FIZT(dset,vv) ;                     /* mark as a Z score */
   }
   NI_delete_str_array(labar) ;

   if( nel->vec_len == 0 || nel->vec_num == 0 || nel->vec == NULL ){  /* all */
     giset->ivec = NULL ; giset->nivec = 0 ;
   } else {                                     /* make index list of voxels */
     int ii , nn=nel->vec_len , *iv=(int *)nel->vec[0] ;
     atr = NI_get_attribute( nel , "nvec" ) ;
     if( atr != NULL ){ ii = (int)strtod(atr,NULL) ; nn = MIN(nn,ii) ; }
     giset->nivec = nn ;
     giset->ivec  = (int *)calloc(sizeof(int),nn) ;
     for( ii=0 ; ii < nn ; ii++ ) giset->ivec[ii] = iv[ii] ;
   }

   return giset ;
}

/*---------------------------------------------------------------------------*/
int GRINCOR_output_srf_dataset(GICOR_setup *giset, NI_element *nel, 
                                char *target_name )
{

   static char FuncName[]={"GRINCOR_output_srf_dataset"};
   int i, ii, *Ti=NULL, ovind = 0, nvals=0, vv=0;
   char *atr=NULL;
   char *targetv[2]={NULL, NULL}, *oname=NULL;
   static SUMA_DSET *sdsetv[2]={NULL,NULL};
   static char *blab[6] = { "GIC_Delta" , "GIC_Zscore" ,
                            "AAA_Delta" , "AAA_Zscore" ,
                            "BBB_Delta" , "BBB_Zscore"  } ;
   NI_str_array *labar=NULL ;
   long sszz=0;
   int LocalHead = NOPE;
   
   if( nel == NULL || nel->vec_num < 2 ) return(-1) ;

   if (giset->nnode_domain[0] <= 0 && giset->nnode_domain[1] <= 0) {
      ERROR_message("Bad values");
      return;
   }
   
   if (giset->nnode_domain[1] > 0) {
      targetv[0] = SUMA_ModifyName(target_name, "append", ".lh", NULL);
      targetv[1] = SUMA_ModifyName(target_name, "append", ".rh", NULL);
   } else {
      targetv[0] = SUMA_copy_string(target_name);
   }
   
   if (sdsetv[0] == NULL) { /* create output sets */
      for (i=0; i<2; ++i) {
         if (targetv[i]) {
            SUMA_LHv("Working %s\n", targetv[i]);
            /* dset names */
            sdsetv[i] = SUMA_CreateDsetPointer (targetv[i], 
                                        SUMA_NODE_BUCKET,
                                        NULL,
                                        NULL,
                                        giset->nnode_domain[i]);
            sprintf(giset->sdset_ID[i],"%s", SDSET_ID(sdsetv[i]));
            
            SUMA_LHv("Adding columns %d\n", i);
            /* add the columns */
            Ti = (int *) SUMA_calloc(SDSET_VECLEN(sdsetv[i]), sizeof(int));
            for (ii=0; ii <SDSET_VECLEN(sdsetv[i]); ++ii) Ti[ii]=ii;
            SUMA_AddDsetNelCol (sdsetv[i], "node index", 
                                SUMA_NODE_INDEX, Ti, NULL, 1);
            SUMA_free(Ti); Ti=NULL;

            if (!(atr = NI_get_attribute( nel , "target_nvals" ))) {
               nvals = giset->nvals;
            } else {
               nvals = (int)strtod(atr,NULL) ;  nvals = MAX(1,nvals);     
            }
         
            if (nvals < 1) {
               WARNING_message("Bad nvals %d. Going with 6\n",nvals);
               nvals = 6;
            }
            
            if (!(atr = NI_get_attribute( nel , "target_labels" ))) {
               atr = giset->brick_labels;
            }
            if( atr != NULL )
               labar = NI_decode_string_list( atr , ";" ) ;
            
            for( vv=0 ; vv < nvals ; vv++ ){
               if (labar != NULL && vv < labar->num) atr = labar->str[vv];
               else if (vv < 6) {
                  atr = blab[vv];
               } else {
                  atr = "What the hell is this?";
               }
               SUMA_LHv("Adding data column %d/%d (%s)\n", vv,nvals, atr);
               if (vv%2 == 0) { /* beta */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_FLOAT, NULL, NULL, 1);
               } else { /* zscore */
                  SUMA_AddDsetNelCol (sdsetv[i], atr,
                                SUMA_NODE_ZSCORE, NULL, NULL, 1);  
               }
            }
            if (labar) SUMA_free_NI_str_array(labar); labar=NULL;
         }
      }
   }
   
   /* have dsets, populate them */
   if (!SUMA_PopulateDsetsFromGICORnel(nel, giset, sdsetv)) {
      SUMA_S_Err("Failed to populate. Not fun.");
      return;
   }
   
   /* write them and quit */
   for (i=0; i<2; ++i) {
      if (targetv[i]) {
         SUMA_NewDsetID2(sdsetv[i],targetv[i]);
         if (!(oname = SUMA_WriteDset_ns (targetv[i], sdsetv[i], 
                                       oform, 1, 1))) {
            ERROR_message("Failed to write %s\n", targetv[i]);
         } else {
            SUMA_free(oname); oname = NULL;
         }
         SUMA_free(targetv[i]); targetv[i]=NULL;
         sszz += SUMA_sdset_dnel_size(sdsetv[i]);
      }
   }
   return;
}

int GRINCOR_output_dataset( GICOR_setup *giset, NI_element *nel, char *pref )
{
   float *nelar , *dsdar ;
   int nvec,nn,vv , vmul ; float thr ;

   if( nel == NULL || nel->vec_num < 2 ) return(-1) ;

   if (giset->nnode_domain[0] != 0 || giset->nnode_domain[1] != 0) {
      /* go to surfac output */
      return(GRINCOR_output_srf_dataset(giset, nel, pref));
   }
   
   /* copy NIML data into dataset */

   nvec = nel->vec_len ;

   for( vv=0 ; vv < DSET_NVALS(giset->dset) ; vv++ ){
     nelar = (float *)nel->vec[vv] ;                /* NIML array */
     dsdar = (float *)DSET_ARRAY(giset->dset,vv) ;  /* dataset array */

     if( giset->ivec == NULL ){               /* all voxels */
       nn = MIN( giset->nvox , nvec ) ;
       memcpy(dsdar,nelar,sizeof(float)*nn) ;
     } else {                                 /* some voxels */
       int *ivec=giset->ivec , kk ;
       nn = MIN( giset->nivec , nvec ) ;
       for( kk=0 ; kk < nn ; kk++ ) dsdar[ivec[kk]] = nelar[kk] ;
     }
   }

   if( pref != NULL && *pref != '\0' )
     EDIT_dset_items( giset->dset , ADN_prefix,pref , ADN_none ) ;

   giset->dset->dblk->diskptr->allow_directwrite = 1 ;
   DSET_write(giset->dset) ;
   if( verb ) WROTE_DSET(giset->dset) ;
   return ((int)DSET_TOTALBYTES(giset->dset));
}

