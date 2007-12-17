#include "mrilib.h"

static char *report = NULL ;
char * mri_clusterize_report(void){ return report; }

static int                 clu_num = 0 ;
static mri_cluster_detail *clu_rep = NULL ;

#undef  CV
#define CV(p,q) ( ((p)<(q)) ? 1 : ((p)>(q)) ? -1 : 0 )

static int compar_clu_detail( const void *a , const void *b )
{
  mri_cluster_detail *ca = (mri_cluster_detail *)a ;
  mri_cluster_detail *cb = (mri_cluster_detail *)b ;
  return CV(ca->nvox,cb->nvox) ;
}

int mri_clusterize_details( mri_cluster_detail **cld ){
  if( cld != NULL ) *cld = clu_rep ;
  return clu_num ;
}

/*---------------------------------------------------------------------*/
/*! Cluster-edit volume bim, possibly thresholding with tim, and
    produce a new output image.  [05 Sep 2006]
-----------------------------------------------------------------------*/

MRI_IMAGE * mri_clusterize( float rmm , float vmul , MRI_IMAGE *bim ,
                                        float thr  , MRI_IMAGE *tim  )
{
   float dx,dy,dz , dbot , xcm,ycm,zcm , xpk,ypk,zpk , vpk,vvv ;
   int   nx,ny,nz , ptmin,iclu , nkeep,nkill,ncgood , nbot,ntop , ii ;
   MRI_IMAGE *cim ; void *car ;
   MCW_cluster *cl ; MCW_cluster_array*clar ;

ENTRY("mri_clusterize") ;

   if( report  != NULL ){ free((void *)report);  report  = NULL; }
   if( clu_rep != NULL ){ free((void *)clu_rep); clu_rep = NULL; clu_num = 0; }

   if( bim == NULL || mri_data_pointer(bim) == NULL ) RETURN(NULL) ;

   nx = bim->nx; ny = bim->ny; nz = bim->nz;
   dx = bim->dx; dy = bim->dy; dz = bim->dz;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;
   dbot = MIN(dx,dy) ; dbot = MIN(dbot,dz) ;

   if( rmm < dbot ){ dx = dy = dz = 1.0f; rmm = 1.01f; }
   if( vmul <= 2.0*dx*dy*dz ) RETURN(NULL) ;

   /* create copy of input image (this will be edited below) */

   cim = mri_copy(bim) ; car = mri_data_pointer(cim) ;
   if( car == NULL ){ mri_free(cim) ; RETURN(NULL) ; }

   /* threshold it, if so ordered (note that tim==bim is legal) */

   if( thr > 0.0f && tim != NULL )
     mri_threshold( -thr , thr , tim , cim ) ;

   /* smallest cluster to keep */

   ptmin = (int)( vmul/(dx*dy*dz) + 0.99f ) ;

   /* find all clusters */

   clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz , cim->kind,car , rmm ) ;

   /* put all big clusters back into the image */

   if( clar != NULL ){
     ncgood = nkeep = nkill = 0 ; nbot = 999999 ; ntop = 0 ;
     for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
       cl = clar->clar[iclu] ;
       if( cl->num_pt >= ptmin ){  /* put back into array */
          MCW_cluster_to_vol( nx,ny,nz , cim->kind,car , cl ) ;
          nkeep += cl->num_pt ; ncgood++ ;
          nbot = MIN(nbot,cl->num_pt); ntop = MAX(ntop,cl->num_pt);
          clu_num++ ;
          clu_rep = (mri_cluster_detail *)realloc((void *)clu_rep,
                                                  sizeof(mri_cluster_detail)*clu_num) ;
          clu_rep[clu_num-1].nvox   = cl->num_pt ;
          clu_rep[clu_num-1].volume = cl->num_pt ;
          xcm = ycm = zcm = 0.0f ; vpk = 0.0f ;
          for( ii=0 ; ii < cl->num_pt ; ii++ ){
            xcm += cl->i[ii] ; ycm += cl->j[ii] ; zcm += cl->k[ii] ;
            vvv = fabsf(cl->mag[ii]) ;
            if( vvv > vpk ){
              xpk = cl->i[ii] ; ypk = cl->j[ii] ; zpk = cl->k[ii] ; vpk = vvv ;
            }
          }
          clu_rep[clu_num-1].xcm = xcm / cl->num_pt ;
          clu_rep[clu_num-1].ycm = ycm / cl->num_pt ;
          clu_rep[clu_num-1].zcm = zcm / cl->num_pt ;
          clu_rep[clu_num-1].xpk = xpk ;
          clu_rep[clu_num-1].ypk = ypk ;
          clu_rep[clu_num-1].zpk = zpk ;
       } else {
          nkill += cl->num_pt ;
       }
     }
     DESTROY_CLARR(clar) ;
     report = THD_zzprintf( report ,
                            "Voxels survived clustering = %5d\n"
                            "Voxels edited out          = %5d\n" ,
                            nkeep , nkill ) ;
     if( ntop >= nbot )
      report = THD_zzprintf( report ,
                            "Min cluster size (voxels)  = %5d\n"
                            "Max cluster size           = %5d\n"
                            "Number of clusters kept    = %5d\n" ,
                            nbot , ntop , ncgood ) ;

     if( clu_num > 1 )
       qsort( clu_rep , (size_t)clu_num ,
              sizeof(mri_cluster_detail) , compar_clu_detail ) ;
   }

   RETURN(cim) ;
}
