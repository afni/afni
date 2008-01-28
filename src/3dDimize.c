#include "mrilib.h"

/*--------------------------------------------------------------------------*/

#undef  IJK
#define IJK(p,q,r) ((p)+(q)*nx+(r)*nxy)

#undef  NRMAX
#define NRMAX 19

int mri_mask_localcount( MRI_IMAGE *maskim , int iv, int jv, int kv ,
                         int nrad, float *rad, short *ccount, short *vcount )
{
   byte *mmm ;
   MCW_cluster *cl ;
   int ii,jj,kk , ncl , nx,ny,nz,nxy,nxyz , ijk ;
   int ip,jp,kp , im,jm,km , icl , ic,jc,kc , rr ;
   float dx,dy,dz , val , di,dj,dk , rsq , rq[NRMAX];

ENTRY("mri_mask_localcount") ;

   nx = maskim->nx; ny = maskim->ny; nz = maskim->nz; nxy = nx*ny; nxyz = nxy*nz;
   mmm = MRI_BYTE_PTR(maskim) ;
   ijk = IJK(iv,jv,kv) ; if( mmm[ijk] == 0 ) RETURN(0) ;

   ii = iv ; jj = jv ; kk = kv ;
   dx = maskim->dx; dy = maskim->dy; dz = maskim->dz;

#undef  CCLUST
#define CCLUST(p,q,r)                                               \
 do{ int pqr=IJK(p,q,r) ;                                           \
     if( mmm[pqr] ){                                                \
       di = (ii-(p))*dx; dj = (jj-(q))*dy; dk = (kk-(r))*dz;        \
       val = di*di+dj*dj+dk*dk ;                                    \
       if( val <= rsq ){ ADDTO_CLUSTER(cl,p,q,r,val); mmm[pqr]=0; } \
    }                                                               \
 } while(0)

   INIT_CLUSTER(cl) ; ADDTO_CLUSTER(cl,ii,jj,kk,0) ; mmm[ijk] = 0 ;
   for( rr=0 ; rr < nrad ; rr++ ){
     rq[rr] = rsq = SQR(rad[rr]) ;
     for( icl=0 ; icl < cl->num_pt ; icl++ ){
       ic = cl->i[icl]; jc = cl->j[icl]; kc = cl->k[icl];
       im = ic-1 ; jm = jc-1 ; km = kc-1 ;
       ip = ic+1 ; jp = jc+1 ; kp = kc+1 ;
       if( im >= 0 && im < nx ) CCLUST(im,jc,kc) ;
       if( ip >= 0 && ip < nx ) CCLUST(ip,jc,kc) ;
       if( jm >= 0 && jm < ny ) CCLUST(ic,jm,kc) ;
       if( jp >= 0 && jp < ny ) CCLUST(ic,jp,kc) ;
       if( km >= 0 && km < nz ) CCLUST(ic,jc,km) ;
       if( kp >= 0 && kp < nz ) CCLUST(ic,jc,kp) ;
     }
     ccount[rr] = (short)cl->num_pt ;
   }
   for( icl=0 ; icl < cl->num_pt ; icl++ )
     mmm[IJK(cl->i[icl],cl->j[icl],cl->k[icl])] = 1 ;  /* restore mmm */
   KILL_CLUSTER(cl) ;

   val = rad[nrad-1] / dx ;
   im  = (int)(ii-val)     ; if( im <  0  ) im = 0 ;
   ip  = (int)(ii+val+1.0) ; if( ip >= nx ) ip = nx-1 ;
   val = rad[nrad-1] / dy ;
   jm  = (int)(jj-val)     ; if( jm <  0  ) jm = 0 ;
   jp  = (int)(jj+val+1.0) ; if( jp >= ny ) jp = ny-1 ;
   val = rad[nrad-1] / dz ;
   km  = (int)(kk-val)     ; if( km <  0  ) km = 0 ;
   kp  = (int)(kk+val+1.0) ; if( kp >= nz ) kp = nz-1 ;
   for( rr=0 ; rr < nrad ; rr++ ) vcount[rr] = 0 ;
   icl = 0 ;
   for( kc=km ; kc <= kp ; kc++ ){
     dk = (kk-kc)*dz; dk = dk*dk ;
     for( jc=jm ; jc <= jp ; jc++ ){
      dj = (jj-jc)*dy; dj = dj*dj + dk ;
      for( ic=im ; ic <= ip ; ic++ ){
        di = (ii-ic)*dx; val = di*di+dj ;
        for( rr=0 ; rr < nrad ; rr++ ) if( val <= rq[rr] ) vcount[rr]++ ;
   }}}

   RETURN(1) ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset , *outset ;
   MRI_IMAGE *medim , *mim ;
   byte *mmm ; float *mar ;
   short *dar05,*dar10,*dar15,*dar20 ;
   float *dim10,*dim15,*dim20 ;
   int nx,ny,nz , nxy,nxyz , ii,jj,kk , nmm , icm,jcm,kcm , ijk ;
   int ibot,itop , jbot,jtop , kbot,ktop ;
   float isum,jsum,ksum , clip_val , lg10,lg15,lg20 ;
   int id,jd,kd , dijk , di,dj,dk ; float ff ;
   char *prefix = "dimize" ;
#define NRAD 6
   float rad[NRAD] = { 5.0f , 8.0f , 11.0f , 14.0f , 17.0f , 20.0f } ;
   float *bb[NRAD] ;
   short ccc[NRAD] , vvv[NRAD] ; int rr ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("3dDimize dataset\n"); PRINT_COMPILE_DATE ; exit(0);
   }

   mainENTRY("3dDimize"); machdep();

   inset = THD_open_dataset(argv[1]); CHECK_OPEN_ERROR((inset,argv[1]);
   DSET_load(inset); CHECK_LOAD_ERROR(inset) ;
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
                      ADN_nvals  , NRAD-1 ,
                      ADN_ntt    , NRAD-1 ,
                      ADN_prefix , prefix ,
                    ADN_none ) ;
   for( rr=0 ; rr < NRAD-1 ; rr++ ){
     bb[rr] = (float *)calloc(sizeof(float),nxyz) ;
     EDIT_substitute_brick( outset , rr , MRI_float , bb[rr] ) ;
   }

   for( kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = IJK(ii,jj,kk) ; if( mmm[ijk] == 0 ) continue ;
       rr = mri_mask_localcount( mim , ii,jj,kk , NRAD,rad , ccc,vvv ) ;
       if( rr > 0 ){
         for( rr=0 ; rr < NRAD-1 ; rr++ )
           bb[rr][ijk] =  3.0f * logf( ccc[rr+1]/(float)ccc[rr] )
                               / logf( vvv[rr+1]/(float)vvv[rr] ) ;
       }
   }}}

   DSET_write( outset ) ;
   WROTE_DSET( outset ) ;
   exit(0) ;
}
