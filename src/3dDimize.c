#include "mrilib.h"

/*--------------------------------------------------------------------------*/

short * mri_mask_localcount( MRI_IMAGE *maskim , float rad )
{
   byte *mmm , *mar ;
   short *sar ;
   MCW_cluster *cl ;
   int ii,jj,kk , ncl , nx,ny,nz,nxy,nxyz , ijk ;
   int ip,jp,kp , im,jm,km , icl , ic,jc,kc ;
   float dx,dy,dz , val , di,dj,dk , rsq ;

ENTRY("mri_mask_localcount") ;
   if( maskim == NULL || maskim->kind != MRI_byte ){
     WARNING_message("bad inputs to mri_mask_localcount()"); RETURN(NULL);
   }

   nx = maskim->nx; ny = maskim->ny; nz = maskim->nz; nxy = nx*ny; nxyz = nxy*nz;
   dx = maskim->dx; dy = maskim->dy; dz = maskim->dz;
   val = 1.001f*sqrt(dx*dx+dy*dy+dz*dz) ; if( rad < val ) rad = val ;
   mmm = MRI_BYTE_PTR(maskim) ;
   sar = (short *)calloc(sizeof(short),nxyz) ;
   mar = (byte  *)malloc(sizeof(byte) *nxyz) ; memcpy(mar,mmm,sizeof(byte)*nxyz) ;

#undef  DCLUST
#define DCLUST(p,q,r)                                                  \
 do{ int pqr=(p)+(q)*nx+(r)*nxy ;                                      \
     if( mar[pqr] ){                                                   \
       di = (ii-(p))*dx; dj = (jj-(q))*dy; dk = (kk-(r))*dz;           \
       if( di*di+dj*dj+dk*dk <= rsq ) ADDTO_CLUSTER(cl,(p),(q),(r),0); \
       mar[pqr] = 0 ;                                                  \
    }                                                                  \
 } while(0)

   rsq = rad*rad ;
INFO_message("rsq=%g  val=%g  nx=%d ny=%d nz=%d",rsq,val,nx,ny,nz) ;
   for( kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = ii + jj*nx + kk*nxy ; if( mmm[ijk] == 0 ) continue ;
       INIT_CLUSTER(cl) ;  ADDTO_CLUSTER(cl,ii,jj,kk,0) ; mar[ijk] = 0 ;
       for( icl=0 ; icl < cl->num_pt ; icl++ ){
         ic = cl->i[icl]; jc = cl->j[icl]; kc = cl->k[icl];
#if 0
printf("[%d,%d,%d] ",ic,jc,kc);fflush(stdout);
#endif
         im = ic-1 ; jm = jc-1 ; km = kc-1 ;
         ip = ic+1 ; jp = jc+1 ; kp = kc+1 ;
         if( im >= 0 && im < nx ) DCLUST(im,jc,kc) ;
         if( ip >= 0 && ip < nx ) DCLUST(ip,jc,kc) ;
         if( jm >= 0 && jm < ny ) DCLUST(ic,jm,kc) ;
         if( jp >= 0 && jp < ny ) DCLUST(ic,jp,kc) ;
         if( km >= 0 && km < nz ) DCLUST(ic,jc,km) ;
         if( kp >= 0 && kp < nz ) DCLUST(ic,jc,kp) ;
       }
#if 0
printf(" = %d\n",cl->num_pt) ;
#endif
       memcpy(mar,mmm,sizeof(byte)*nxyz) ;
       sar[ijk] = (short)cl->num_pt ;
       KILL_CLUSTER(cl) ;
   }}}

   RETURN(sar) ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *medim , *mim ;
   byte *ccc , *mmm ; float *mar ;
   short *dar05,*dar10,*dar15,*dar20 ;
   float *dim10,*dim15,*dim20 ;
   int nx,ny,nz , nxy,nxyz , ii,jj,kk , nmm , icm,jcm,kcm , ijk ;
   int ibot,itop , jbot,jtop , kbot,ktop ;
   float isum,jsum,ksum , clip_val , lg10,lg15,lg20 ;
   int id,jd,kd , dijk , di,dj,dk ; float ff ;
   char *prefix = "dimize" ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("3dDimize dataset\n"); exit(0);
   }

   mainENTRY("3dDimize"); machdep();

   inset = THD_open_dataset(argv[1]); if( inset == NULL ) ERROR_exit("Can't open");
   DSET_load(inset); if( !DSET_LOADED(inset) ) ERROR_exit("Can't load");
   medim = THD_median_brick(inset); if( medim == NULL ) ERROR_exit("Can't median");
   DSET_unload(inset) ;
   clip_val = THD_cliplevel(medim,0.50f) ;

   /* create mask of values above clip value */

   nx   = medim->nx; ny = medim->ny; nz = medim->nz; nxy = nx*ny; nxyz = nxy*nz;
   mar  = MRI_FLOAT_PTR(medim) ;
   mim  = mri_new_conforming(medim,MRI_byte); mmm = MRI_BYTE_PTR(mim);
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (mar[ii] >= clip_val) ;
   mri_free(medim) ;
   MRI_COPY_AUX(mim,DSET_BRICK(inset,0)) ;

   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                      ADN_nvals  , 3 ,
                      ADN_ntt    , 3 ,
                      ADN_prefix , prefix ,
                    ADN_none ) ;

   dar05 = mri_mask_localcount( mim ,  5.0 ) ;
   dar10 = mri_mask_localcount( mim , 10.0 ) ;
   dar15 = mri_mask_localcount( mim , 15.0 ) ;
   dar20 = mri_mask_localcount( mim , 20.0 ) ;

   dim10 = (float *)calloc(sizeof(float),nxyz) ;
   dim15 = (float *)calloc(sizeof(float),nxyz) ;
   dim20 = (float *)calloc(sizeof(float),nxyz) ;

   lg10 = 1.0 / log(2.0) ;
   lg15 = 1.0 / log(1.5) ;
   lg20 = 1.0 / log(20.0/15.0) ;
   for( ii=0 ; ii < nxyz ; ii++ ){
     if( dar05[ii] ){
       dim10[ii] = log( dar10[ii]/(double)dar05[ii] ) * lg10 ;
       dim15[ii] = log( dar15[ii]/(double)dar10[ii] ) * lg15 ;
       dim20[ii] = log( dar20[ii]/(double)dar15[ii] ) * lg20 ;
     }
   }

   EDIT_substitute_brick( outset , 0 , MRI_float , dim10 ) ;
   EDIT_substitute_brick( outset , 1 , MRI_float , dim15 ) ;
   EDIT_substitute_brick( outset , 2 , MRI_float , dim20 ) ;

   DSET_write( outset ) ;
   WROTE_DSET( outset ) ;
   exit(0) ;
}
