#include "mrilib.h"

/*---------------------------------------------------------------------*/
static char *report = NULL ;
char * mri_clusterize_report(void){ return report; }
/*---------------------------------------------------------------------*/

#if 0
MCW_cluster_array *find_clusters_NN1( MRI_IMAGE *cim ) ;
MCW_cluster_array *find_clusters_NN2( MRI_IMAGE *cim ) ;
MCW_cluster_array *find_clusters_NN3( MRI_IMAGE *cim ) ;
#endif

/*---------------------------------------------------------------------*/
static MCW_cluster_array *clarout=NULL ;

MCW_cluster_array * mri_clusterize_array(int clear)
{
  MCW_cluster_array *cc = clarout ;
  if( clear ) clarout = NULL ;
  return cc ;
}

/*---------------------------------------------------------------------*/
/*! Cluster-edit volume bim, possibly thresholding with tim, and
    produce a new output image.  [05 Sep 2006]
-----------------------------------------------------------------------*/

MRI_IMAGE * mri_clusterize( float rmm , float vmul , MRI_IMAGE *bim ,
                            float thb , float tht  , MRI_IMAGE *tim ,
                            int posonly )
{
   float dx,dy,dz , dbot , vmin ;
   int   nx,ny,nz , ptmin,iclu , nkeep,nkill,ncgood , nbot,ntop , ii ;
   MRI_IMAGE *cim ; void *car ;
   MCW_cluster *cl , *dl ; MCW_cluster_array *clar ;

ENTRY("mri_clusterize") ;

   if( report  != NULL ){ free((void *)report);  report  = NULL; }
   if( clarout != NULL ){ DESTROY_CLARR(clarout); }

   if( bim == NULL || mri_data_pointer(bim) == NULL ) RETURN(NULL) ;

   nx = bim->nx; ny = bim->ny; nz = bim->nz;
   dx = bim->dx; dy = bim->dy; dz = bim->dz;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   dbot = MIN(dx,dy) ; dbot = MIN(dbot,dz) ;

   if( rmm < dbot ){ dx = dy = dz = 1.0f; rmm = 1.01f; }
   vmin = 2.0f*dx*dy*dz ; if( vmul < vmin ) vmul = vmin ;

   /* create copy of input image (this will be edited below) */

   cim = mri_copy(bim) ; car = mri_data_pointer(cim) ;
   if( car == NULL ){ mri_free(cim) ; RETURN(NULL) ; }

   /* threshold it, if so ordered (note that tim==bim is legal) */

   if( tht > thb && tim != NULL )
     mri_threshold( thb , tht , tim , cim ) ;

   if( posonly )
     mri_threshold( -1.e9 , 0.0 , cim , cim ) ;

   /* smallest cluster to keep */

   ptmin = (int)( vmul/(dx*dy*dz) + 0.99f ) ;

   /* find all clusters */

   clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz , cim->kind,car , rmm ) ;

   /* put all big clusters back into the image */

   if( clar != NULL ){
     ncgood = nkeep = nkill = 0 ; nbot = 999999 ; ntop = 0 ;
     for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
       cl = clar->clar[iclu] ;
       if( cl->num_pt >= ptmin ){  /* put back into array, get some stats */

         MCW_cluster_to_vol( nx,ny,nz , cim->kind,car , cl ) ;

         nkeep += cl->num_pt ; ncgood++ ;
         nbot = MIN(nbot,cl->num_pt); ntop = MAX(ntop,cl->num_pt);

         if( clarout == NULL ) INIT_CLARR(clarout) ;
         COPY_CLUSTER(dl,cl) ; ADDTO_CLARR(clarout,dl) ;
       } else {
         nkill += cl->num_pt ;
       }
     }
     DESTROY_CLARR(clar) ;
     report = THD_zzprintf( report ,
                            " Voxels survived clustering =%6d\n"
                            " Voxels edited out          =%6d\n" ,
                            nkeep , nkill ) ;
     if( ntop >= nbot )
       report = THD_zzprintf( report ,
                            " Min cluster size (voxels)  =%6d\n"
                            " Max cluster size           =%6d\n"
                            " Number of clusters kept    =%6d\n" ,
                            nbot , ntop , ncgood ) ;

   }

   RETURN(cim) ;
}

/*---------------------------------------------------------------------*/

mri_cluster_detail mri_clusterize_detailize( MCW_cluster *cl )
{
   mri_cluster_detail cld ;
   float xcm,ycm,zcm , xpk,ypk,zpk , vpk,vvv,vsum ;
   int ii ;

ENTRY("mri_clusterize_detailize") ;

   memset( &cld , 0 , sizeof(mri_cluster_detail) ) ;
   if( cl == NULL || cl->num_pt <= 0 ) RETURN(cld) ;

   cld.nvox   = cl->num_pt ;
   cld.volume = cl->num_pt ;
   xcm = ycm = zcm = 0.0f ; xpk = ypk = zpk = vpk = 0.0f ;
   for( vsum=ii=0 ; ii < cl->num_pt ; ii++ ){
     vvv = fabsf(cl->mag[ii]) ; vsum += vvv ;
     xcm += vvv*cl->i[ii] ; ycm += vvv*cl->j[ii] ; zcm += vvv*cl->k[ii] ;
     if( vvv > vpk ){
       xpk = cl->i[ii]; ypk = cl->j[ii]; zpk = cl->k[ii]; vpk = vvv;
     }
   }
   if( vsum > 0.0f ){
     cld.xcm = xcm / vsum; cld.ycm = ycm / vsum; cld.zcm = zcm / vsum;
   }
   cld.xpk = xpk; cld.ypk = ypk; cld.zpk = zpk;

   RETURN(cld) ;
}

#if 0
/*---------------------------------------------------------------------------*/
/** The stuff below is added Jul 2010, for the new AFNI Clusterize methods. **/
/*---------------------------------------------------------------------------*/

/* Put (i,j,k) into the current cluster, maybe */

#undef  CPUT
#define CPUT(i,j,k)                            \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;       \
      if( car[ijk] != 0.0f ){                  \
        ADDTO_CLUSTER(clust,i,j,k,car[ijk]) ;  \
        car[ijk] = 0.0f ;                      \
      } } while(0)

/*----------------------------------------------------------------------------*/

MCW_cluster_array *find_clusters_NN1( MRI_IMAGE *cim )
{
   MCW_cluster_array *clust_arr ;
   MCW_cluster       *clust ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nx,ny,nz,nxy,nxyz ;
   float *car = MRI_FLOAT_PTR(cim) ;

   nx = cim->nx ; ny = cim->ny ; cim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   INIT_CLARR(clust_arr) ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( car[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     INIT_CLUSTER(clust) ;
     IJK_TO_THREE(ijk,ii,jj,kk,nx,nxy) ;
     ADDTO_CLUSTER( clust , ii,jj,kk , car[ijk] ) ;
     car[ijk] = 0.0f ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < clust->num_pt ; icl++ ){
       ii = clust->i[icl] ; jj = clust->j[icl] ; kk = clust->k[icl] ;
       im = ii-1          ; jm = jj-1          ; km = kk-1          ;
       ip = ii+1          ; jp = jj+1          ; kp = kk+1          ;

       if( im >= 0 ) CPUT(im,jj,kk) ;
       if( ip < nx ) CPUT(ip,jj,kk) ;
       if( jm >= 0 ) CPUT(ii,jm,kk) ;
       if( jp < ny ) CPUT(ii,jp,kk) ;
       if( km >= 0 ) CPUT(ii,jj,km) ;
       if( kp < nz ) CPUT(ii,jj,kp) ;
     }
     ADDTO_CLARR(clust_arr,clust) ;
   }

   if( clust_arr->num_clu <= 0 ){ DESTROY_CLARR(clust_arr) ; }
   return clust_arr ;
}

/*----------------------------------------------------------------------------*/

MCW_cluster_array *find_clusters_NN2( MRI_IMAGE *cim )
{
   MCW_cluster_array *clust_arr ;
   MCW_cluster       *clust ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nx,ny,nz,nxy,nxyz ;
   float *car = MRI_FLOAT_PTR(cim) ;

   nx = cim->nx ; ny = cim->ny ; cim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   INIT_CLARR(clust_arr) ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( car[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     INIT_CLUSTER(clust) ;
     IJK_TO_THREE(ijk,ii,jj,kk,nx,nxy) ;
     ADDTO_CLUSTER( clust , ii,jj,kk , car[ijk] ) ;
     car[ijk] = 0.0f ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < clust->num_pt ; icl++ ){
       ii = clust->i[icl] ; jj = clust->j[icl] ; kk = clust->k[icl] ;
       im = ii-1          ; jm = jj-1          ; km = kk-1          ;
       ip = ii+1          ; jp = jj+1          ; kp = kk+1          ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;
         if( jm >= 0 ) CPUT(im,jm,kk) ;  /* 2NN */
         if( jp < nx ) CPUT(im,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(im,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(im,jj,kp) ;  /* 2NN */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;
         if( jm >= 0 ) CPUT(ip,jm,kk) ;  /* 2NN */
         if( jp < nx ) CPUT(ip,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(ip,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ip,jj,kp) ;  /* 2NN */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;
         if( km >= 0 ) CPUT(ii,jm,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jm,kp) ;  /* 2NN */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;
         if( km >= 0 ) CPUT(ii,jp,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jp,kp) ;  /* 2NN */
       }
       if( km >= 0 ) CPUT(ii,jj,km) ;
       if( kp < nz ) CPUT(ii,jj,kp) ;
     }
     ADDTO_CLARR(clust_arr,clust) ;
   }

   if( clust_arr->num_clu <= 0 ){ DESTROY_CLARR(clust_arr) ; }
   return clust_arr ;
}

/*----------------------------------------------------------------------------*/

MCW_cluster_array *find_clusters_NN3( MRI_IMAGE *cim )
{
   MCW_cluster_array *clust_arr ;
   MCW_cluster       *clust ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nx,ny,nz,nxy,nxyz ;
   float *car = MRI_FLOAT_PTR(cim) ;

   nx = cim->nx ; ny = cim->ny ; cim->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   INIT_CLARR(clust_arr) ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( car[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     INIT_CLUSTER(clust) ;
     IJK_TO_THREE(ijk,ii,jj,kk,nx,nxy) ;
     ADDTO_CLUSTER( clust , ii,jj,kk , car[ijk] ) ;
     car[ijk] = 0.0f ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < clust->num_pt ; icl++ ){
       ii = clust->i[icl] ; jj = clust->j[icl] ; kk = clust->k[icl] ;
       im = ii-1          ; jm = jj-1          ; km = kk-1          ;
       ip = ii+1          ; jp = jj+1          ; kp = kk+1          ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;
         if( jm >= 0 ) CPUT(im,jm,kk) ;  /* 2NN */
         if( jp < nx ) CPUT(im,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(im,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(im,jj,kp) ;  /* 2NN */
         if( jm >= 0 && km >= 0 ) CPUT(im,jm,km) ;  /* 3NN */
         if( jm >= 0 && kp < nz ) CPUT(im,jm,kp) ;  /* 3NN */
         if( jp < ny && km >= 0 ) CPUT(im,jp,km) ;  /* 3NN */
         if( jp < ny && kp < nz ) CPUT(im,jp,kp) ;  /* 3NN */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;
         if( jm >= 0 ) CPUT(ip,jm,kk) ;  /* 2NN */
         if( jp < nx ) CPUT(ip,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(ip,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ip,jj,kp) ;  /* 2NN */
         if( jm >= 0 && km >= 0 ) CPUT(ip,jm,km) ;  /* 3NN */
         if( jm >= 0 && kp < nz ) CPUT(ip,jm,kp) ;  /* 3NN */
         if( jp < ny && km >= 0 ) CPUT(ip,jp,km) ;  /* 3NN */
         if( jp < ny && kp < nz ) CPUT(ip,jp,kp) ;  /* 3NN */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;
         if( km >= 0 ) CPUT(ii,jm,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jm,kp) ;  /* 2NN */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;
         if( km >= 0 ) CPUT(ii,jp,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jp,kp) ;  /* 2NN */
       }
       if( km >= 0 )   CPUT(ii,jj,km) ;
       if( kp < nz )   CPUT(ii,jj,kp) ;

     }
     ADDTO_CLARR(clust_arr,clust) ;
   }

   if( clust_arr->num_clu <= 0 ){ DESTROY_CLARR(clust_arr) ; }
   return clust_arr ;
}
#endif
