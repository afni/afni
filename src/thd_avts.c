#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/*! For each cluster of points in clustar, extract the average time
    series from dataset dset.  Put the results into an nt X nc float
    image, where nt = number of dataset bricks, nc = number of clusters.
-------------------------------------------------------------------------*/

MRI_IMAGE * THD_average_timeseries( MCW_cluster_array *clustar ,
                                    THD_3dim_dataset *dset      )
{
   int nt,nc , ii,jj , npt,kk , nx,ny,nxy , ijk,nav ;
   MRI_IMAGE *flim ;
   float     *flar , *tsar , *avar , fac ;
   MCW_cluster *clust ;

ENTRY("THD_average_timeseries") ;

   if( clustar == NULL || clustar->num_clu == 0 || !ISVALID_DSET(dset) )
     RETURN(NULL) ;

   nt = DSET_NVALS(dset) ;
   nc = clustar->num_clu ;
   tsar = (float *) malloc(nt*sizeof(float)) ;
   avar = (float *) malloc(nt*sizeof(float)) ;

   flim = mri_new( nt,nc , MRI_float ) ;
   flar = MRI_FLOAT_PTR(flim) ;

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nxy = nx*ny ;

   for( jj=0 ; jj < nc ; jj++ ){
     clust = clustar->clar[jj] ;
     if( clust == NULL || clust->num_pt == 0 ) continue ;
     npt = clust->num_pt ;
     for( ii=0 ; ii < nt ; ii++ ) avar[ii] = 0.0 ;
     for( nav=kk=0 ; kk < npt ; kk++ ){
       ijk = THREE_TO_IJK(clust->i[kk],clust->j[kk],clust->k[kk],nx,nxy) ;
       ii  = THD_extract_array( ijk , dset , 0 , tsar ) ;
       if( ii < 0 ) continue ;
       for( ii=0 ; ii < nt ; ii++ ) avar[ii] += tsar[ii] ;
       nav++ ;
     }
     if( nav > 0 ){
       fac = 1.0 / nav ;
       for( ii=0 ; ii < nt ; ii++ ) flar[ii+jj*nt] = fac*avar[ii] ;
     }
   }

   free(avar) ; free(tsar) ; RETURN(flim) ;
}

/*------------------------------------------------------------------------*/
/*! Extract a single average time series.
--------------------------------------------------------------------------*/

MRI_IMAGE * THD_average_one_timeseries( MCW_cluster *clust ,
                                        THD_3dim_dataset *dset )
{
   MRI_IMAGE *im ;
   MCW_cluster_array *clustar ;

ENTRY("THD_average_one_timeseries") ;

   if( clust == NULL || !ISVALID_DSET(dset) ) RETURN(NULL) ;

   INIT_CLARR(clustar) ;
   ADDTO_CLARR(clustar,clust) ;

   im = THD_average_timeseries( clustar , dset ) ;

   clustar->clar[0] = NULL ; DESTROY_CLARR(clustar) ;
   RETURN(im) ;
}
