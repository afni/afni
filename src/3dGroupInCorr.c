/* Group InstaCorr == GrpInCorr */

#include "mrilib.h"

typedef struct {

  int nvec  ;  /* number of vectors in a dataset */
  int ndset ;  /* number of datasets */
  int *nvals ; /* nvals[i] = number of values in a vector in i-th dataset */

  char *geometry_string ;
  THD_3dim_dataset *tdset ; /* template dataset */

  char *dfname ; /* data file name */
  int  *ivec   ;  /* ivec[i] = spatial index of i-th vector, i=0..nvec-1 */
  float *fac   ;  /* fac[i] = scale factor for i-th dataset, i=0..ndset-1 */
  short **sv   ;  /* sv[i] = array [nvals[i]*nvec] for i-th dataset */

} MRI_shindss ;  /* short indexed datasets */

/*--------------------------------------------------------------------------*/

#undef  GQUIT
#define GQUIT                                              \
 do{ if( tdset != NULL ) DSET_delete(tdset) ;              \
     if( dfname != NULL ) free(dfname) ;                   \
     if( geometry_string != NULL ) free(geometry_string) ; \
     NI_free_element(nel) ;                                \
     return(NULL) ;                                        \
 } while(0)

MRI_shindss * GRINCOR_read_input( char *fname )
{
   NI_element *nel=NULL ;
   char *dfname=NULL , *atr ;
   NI_float_array *facar ; NI_int_array *nvar ;
   MRI_shindss *shd ;
   long long nbytes_needed , nbytes_dfname ; int fdes ;
   void *var ; int ids ;

   char *geometry_string=NULL ;
   THD_3dim_dataset *tdset=NULL;
   int need_ivec=0 , *ivec=NULL , *nvals=NULL , nvec,ndset ; float *fac=NULL ;

   if( fname == NULL || *fname == '\0' ) GQUIT ;

   /* get data element */

   nel = NI_read_element_fromfile(fname) ;
   if( nel == NULL || nel->type != NI_ELEMENT_TYPE ) GQUIT ;
   if( strcmp(nel->name,"3dGroupInCorr") != 0 ) GQUIT ;

   /* headerless ==> using all voxels */

   need_ivec = ( nel->vec_num < 1 ||
                 nel->vec_len < 1 || nel->vec_typ[0] != NI_INT ) ;

   /* number of vectors in each dataset */

   atr = NI_get_attribute(nel,"nvec"); if( atr == NULL ) GQUIT ;
   nvec = (int)strtod(atr,NULL) ;
   if( nvec < 2 || (!need_ivec && nel->vec_len != nvec) ) GQUIT ;

   /* number of datasets */

   atr = NI_get_attribute(nel,"ndset"); if( atr == NULL ) GQUIT ;
   ndset = (int)strtod(atr,NULL) ; if( ndset < 1 ) GQUIT ;

   /* number of time points in each dataset (varies with dataset) */

   atr = NI_get_attribute(nel,"nvals"); if( atr == NULL ) GQUIT ;
   nvar = NI_decode_int_list(atr,",") ;
   if( nvar == NULL || nvar->num < ndset ) GQUIT ;
   nvals = nvar->ar ; nvar->ar = NULL ; NI_delete_int_array(nvar) ;

   /* number of bytes needed:
        sizeof(short) * number of vectors per dataset
                      * number of datasets
                      * sum of per dataset vector lengths */

   nbytes_needed = 0 ;
   for( ids=0 ; ids < ndset ; ids++ ) nbytes_needed += nvals[ids] ;
   nbytes_needed *= ((long long)nvec) * ((long long)ndset) * sizeof(short) ;

   /* scale factor for each dataset */

   atr = NI_get_attribute(nel,"fac") ; if( atr == NULL ) GQUIT ;
   facar = NI_decode_float_list(atr,",") ;
   if( facar == NULL || facar->num < ndset ) GQUIT ;
   fac = facar->ar ; facar->ar = NULL ; NI_delete_float_array(facar) ;

   for( ids=0 ; ids < ndset ; ids++ ) if( fac[ids] <= 0.0f ) fac[ids] = 1.0f ;

   /* grid definition */

   atr = NI_get_attribute(nel,"geometry") ; if( atr == NULL ) GQUIT ;
   geometry_string = strdup(atr) ;
   tdset = EDIT_geometry_constructor( geometry_string , "GrpInCorr" ) ;
   if( tdset == NULL ) GQUIT ;
   if(  need_ivec && DSET_NVOX(tdset) != nvec ) GQUIT ;
   if( !need_ivec && DSET_NVOX(tdset) <  nvec ) GQUIT ;

   /* name of data file: check its size against what's needed */

   atr = NI_get_attribute(nel,"datafile") ; if( atr == NULL ) GQUIT ;
   dfname = strdup(atr) ; nbytes_dfname = THD_filesize(dfname) ;
   if( nbytes_dfname < nbytes_needed ) GQUIT ;
   fdes = open( dfname , O_RDONLY ) ; if( fdes < 0 ) GQUIT ;

   /* ivec[i] is the voxel spatial index of the i-th vector */

   if( need_ivec ){
     ivec = (int *)malloc(sizeof(int)*nvec) ;
     for( ids=0 ; ids < nvec ; ids++ ) ivec[ids] = ids ;
   } else {
     ivec = (int *)nel->vec[0] ;
     nel->vec[0] = NULL ;
   }

   NI_free_element(nel) ;

   /* create output struct */

   shd = (MRI_shindss *)malloc(sizeof(MRI_shindss)) ;

   shd->nvals = nvals ;
   shd->nvec  = nvec  ;
   shd->ndset = ndset ;

   shd->geometry_string = geometry_string ;
   shd->tdset           = tdset ;
   shd->dfname          = dfname ;

   shd->ivec = ivec ;
   shd->fac  = fac  ;

   /*--- now have to map data from disk ---*/

   var = mmap( 0 , (size_t)nbytes_needed ,
                   PROT_READ , THD_MMAP_FLAG , fdes , 0 ) ;
   close(fdes) ;  /* close file descriptor does not unmap data */

   if( var == (void *)(-1) ){ free(shd) ; return NULL ; } /* this is bad */

   /*-- create array of pointers to each dataset's data array --*/

   shd->sv    = (short **)malloc(sizeof(short *)*ndset) ;
   shd->sv[0] = (short *)var ;
   for( ids=1 ; ids < ndset ; ids++ )
     shd->sv[ids] = shd->sv[ids-1] + nvals[ids-1]*nvec ;

   return shd ;
}

#undef GQUIT

/*--------------------------------------------------------------------------*/
/* This cute little function consumes a lot of CPU time. */

void GRINCOR_dotprod( MRI_shindss *shd, int ids, float *vv, float *dp )
{
   int nvec = shd->nvec , nvals = shd->nvals[ids] , iv,ii ;
   float sum , fac = shd->fac[ids] ;
   short *sv = shd->sv[ids] , *ssv ;

   for( iv=0 ; iv < nvec ; iv++ ){
     ssv = sv + iv*nvals ;
     for( sum=0.0f,ii=0 ; ii < nvals ; ii++ ) sum += vv[ii]*ssv[ii] ;
     dp[iv] = sum * fac ;
   }

   return ;
}
