#include "afni_warp.h"

#define NPER 10

static int nper = NPER ;

int_triple THD_orient_guess( MRI_IMAGE *imm )
{
   int nvox , ii , nx,ny,nxy,nz , ix,jy,kz , icm,jcm,kcm , nbar ;
   byte *bar , bp,bm ;
   float xcm , ycm , zcm , ff , dx,dy,dz ;
   float xx,yy,zz ;
   int ni,nj,nk , itop,jtop,ktop , im,ip , jm,jp , km,kp ;
   float axx,ayy,azz , clip  , qx,qy,qz , arr[3] ;
   int d_LR , d_AP , d_IS ;

   int_triple it = {-1,-1,-1} ;

   /*-- check for bad input --*/

   if( imm == NULL || imm->nx < 5 || imm->ny < 5 || imm->nz < 5 ) return it ;

   nx = imm->nx; ny = imm->ny; nz = imm->nz; nxy = nx*ny; nvox = nx*ny*nz;

   dx = fabs(imm->dx) ; if( dx == 0.0 ) dx = 1.0 ;
   dy = fabs(imm->dy) ; if( dy == 0.0 ) dy = 1.0 ;
   dz = fabs(imm->dz) ; if( dz == 0.0 ) dz = 1.0 ;

   /*-- make mask of NPER levels --*/

   bar  = (byte *) malloc( sizeof(byte) * nvox ) ;
   clip = THD_cliplevel( imm , 0.5 ) ;

   /* start with a binary mask */

   switch( imm->kind ){
     case MRI_float:{
       float *ar = MRI_FLOAT_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_short:{
       short *ar = MRI_SHORT_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
     case MRI_byte:{
       byte *ar = MRI_BYTE_PTR(imm) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (ar[ii] >= clip);
     }
     break ;
   }

   nbar = THD_countmask(nvox,bar) ;
   printf("%d voxels in initial binary mask\n",nbar) ;
   if( nbar == 0 ){ free(bar); return it; }  /* bad */

   THD_mask_clust( nx,ny,nz , bar ) ;      /* take biggest cluster */

   nbar = THD_countmask(nvox,bar) ;
   printf("%d voxels in final binary mask\n",nbar) ;

#ifdef NPER
 if( nper > 1 ){
   float per[NPER+1] ; MRI_IMAGE *qim ; int jj ;
   qim = mri_new( nbar , 1 , imm->kind ) ;
   switch(imm->kind){
     case MRI_float:{
      float *ar=MRI_FLOAT_PTR(imm) , *qar=MRI_FLOAT_PTR(qim) ;
      for( jj=ii=0 ; ii < nvox ; ii++ ) if( bar[ii] ) qar[jj++] = ar[ii] ;
     }
     break ;
     case MRI_short:{
      short *ar=MRI_SHORT_PTR(imm) , *qar=MRI_SHORT_PTR(qim) ;
      for( jj=ii=0 ; ii < nvox ; ii++ ) if( bar[ii] ) qar[jj++] = ar[ii] ;
     }
     break ;
     case MRI_byte:{
      byte *ar=MRI_BYTE_PTR(imm) , *qar=MRI_BYTE_PTR(qim) ;
      for( jj=ii=0 ; ii < nvox ; ii++ ) if( bar[ii] ) qar[jj++] = ar[ii] ;
     }
     break ;
   }
   printf("call mri_percents\n") ;
   mri_percents( qim , nper , per ) ;  /* compute nper break points */
   mri_free(qim) ;
   printf("per:") ;
   for( ii=0 ; ii <= nper ; ii++ ) printf(" %g",per[ii]) ;
   printf("\n") ;
   switch( imm->kind ){
     case MRI_float:{
       float *ar = MRI_FLOAT_PTR(imm) , val ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( bar[ii] ){
           val = ar[ii] ;
           for( jj=1 ; jj <= nper && val >= per[jj] ; jj++ ) ; /*spin*/
           bar[ii] = jj ;
         }
       }
     }
     break ;
     case MRI_short:{
       short *ar = MRI_SHORT_PTR(imm) , val ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( bar[ii] ){
           val = ar[ii] ;
           for( jj=1 ; jj <= nper && val >= per[jj] ; jj++ ) ; /*spin*/
           bar[ii] = jj ;
         }
       }
     }
     break ;
     case MRI_byte:{
       byte *ar = MRI_BYTE_PTR(imm) , val ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( bar[ii] ){
           val = ar[ii] ;
           for( jj=1 ; jj <= nper && val >= per[jj] ; jj++ ) ; /*spin*/
           bar[ii] = jj ;
         }
       }
     }
     break ;
   }
  }
#endif  /* NPER */

   /* find center of mass of mask */

   xcm = ycm = zcm = ff = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( bar[ii] ){
       ix = (ii % nx)      ; xx = ix*dx ; xcm += xx*bar[ii] ;
       jy = (ii / nx) % ny ; yy = jy*dy ; ycm += yy*bar[ii] ;
       kz = (ii /nxy)      ; zz = kz*dz ; zcm += zz*bar[ii] ;
       ff += bar[ii] ;
     }
   }
   xcm /= ff ; ycm /= ff ; zcm /= ff ;

   icm = rint(xcm/dx) ;
   itop = 2*icm ; if( itop >= nx ) itop = nx-1 ;
   ni  = itop-icm ;

   jcm = rint(ycm/dy) ;
   jtop = 2*jcm ; if( jtop >= ny ) jtop = ny-1 ;
   nj  = jtop-jcm ;

   kcm = rint(zcm/dz) ;
   ktop = 2*kcm ; if( ktop >= nz ) ktop = nz-1 ;
   nk  = ktop-kcm ;

   printf("Mask count = %d\n"
          "icm = %d  jcm = %d  kcm = %d\n"
          "ni  = %d  nj  = %d  nk  = %d\n",
          (int)ff , icm,jcm,kcm , ni,nj,nk ) ;

   /** compute asymmetry measures about CM voxel **/

#define BAR(i,j,k) bar[(i)+(j)*nx+(k)*nxy]

   axx = 0.0 ;
   for( ix=1 ; ix <= ni ; ix++ ){
     im = icm-ix ; ip = icm+ix ;
     for( kz=kcm-nk ; kz <= kcm+nk ; kz++ ){
       for( jy=jcm-nj ; jy <= jcm+nj ; jy++ )
         axx += abs(BAR(ip,jy,kz) - BAR(im,jy,kz)) ;
     }
   }
   axx /= (ni*nj*nk) ; printf("axx = %g\n",axx) ;

   ayy = 0.0 ;
   for( jy=1 ; jy <= nj ; jy++ ){
     jm = jcm-jy ; jp = jcm+jy ;
     for( kz=kcm-nk ; kz <= kcm+nk ; kz++ ){
       for( ix=icm-ni ; ix <= icm+ni ; ix++ )
         ayy += abs(BAR(ix,jp,kz) - BAR(ix,jm,kz)) ;
     }
   }
   ayy /= (ni*nj*nk) ; printf("ayy = %g\n",ayy) ;

   azz = 0.0 ;
   for( kz=1 ; kz <= nk ; kz++ ){
     km = kcm-kz ; kp = kcm+kz ;
     for( jy=jcm-nj ; jy <= jcm+nj ; jy++ ){
       for( ix=icm-ni ; ix <= icm+ni ; ix++ )
         azz += abs(BAR(ix,jy,kp) - BAR(ix,jy,km)) ;
     }
   }
   azz /= (ni*nj*nk) ; printf("azz = %g\n",azz) ;

   /** least asymettric is L-R direction **/

   if( axx < ayy ){
     if( axx < azz ) d_LR = 1 ;
     else            d_LR = 3 ;
   } else {
     if( ayy < azz ) d_LR = 2 ;
     else            d_LR = 3 ;
   }
   printf("axis %d is L-R\n",d_LR) ;

   arr[0] = axx ; arr[1] = ayy ; arr[2] = azz ; ff = arr[d_LR-1] ;
   arr[0] /= ff ;
   arr[1] /= ff ;
   arr[2] /= ff ;
   printf("a ratios = %g  %g  %g\n",arr[0],arr[1],arr[2]) ;

   /** find asymmetry measures in 1/2 spaces perp to L-R **/

   switch( d_LR ){

     case 3:{  /* L-R is z-axis */
       float axx_jp=0.0, axx_jm=0.0, ayy_ip=0.0, ayy_im=0.0 ;
       for( ix=1 ; ix <= ni ; ix++ ){
         im = icm-ix ; ip = icm+ix ;
         for( kz=kcm-nk ; kz <= kcm+nk ; kz++ ){
           for( jy=1 ; jy <= nj ; jy++ ){
             axx_jp += abs(BAR(ip,jcm+jy,kz) - BAR(im,jcm+jy,kz)) ;
             axx_jm += abs(BAR(ip,jcm-jy,kz) - BAR(im,jcm-jy,kz)) ;
           }
         }
       }
       for( jy=1 ; jy <= nj ; jy++ ){
         jm = jcm-jy ; jp = jcm+jy ;
         for( kz=kcm-nk ; kz <= kcm+nk ; kz++ ){
           for( ix=1 ; ix <= ni ; ix++ ){
             ayy_ip += abs(BAR(icm+ix,jp,kz) - BAR(icm+ix,jm,kz)) ;
             ayy_im += abs(BAR(icm-ix,jp,kz) - BAR(icm-ix,jm,kz)) ;
           }
         }
       }
       axx_jp /= (ni*nj*nk) ; axx_jm /= (ni*nj*nk) ;
       ayy_ip /= (ni*nj*nk) ; ayy_im /= (ni*nj*nk) ;

       printf("axx_jp=%g  axx_jm=%g  ayy_ip=%g  ayy_im=%g\n",
               axx_jp,axx_jm , ayy_ip,ayy_im ) ;
     } /* end of case 3 */
     break ;

   }

   return it ;
}

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   MRI_IMAGE *im ;
   int iarg=1 ;

   if( strcmp(argv[1],"-nper") == 0 ){
      nper = strtol( argv[2] , NULL , 10 ) ;
      iarg = 3 ;
   }

   for( ; iarg < argc ; iarg++ ){
     printf("======= dataset %s  nper=%d ========\n",argv[iarg],nper) ;
     dset = THD_open_dataset(argv[iarg]) ;
     if( dset == NULL ) continue ;
     DSET_load(dset) ;
     im = DSET_BRICK(dset,0) ;
     im->dx = DSET_DX(dset) ;
     im->dy = DSET_DY(dset) ;
     im->dz = DSET_DZ(dset) ;
     (void) THD_orient_guess( im ) ;

     printf( "Data Axes Orientation:\n"
             "  first  (x) = %s\n"
             "  second (y) = %s\n"
             "  third  (z) = %s\n" ,
        ORIENT_typestr[dset->daxes->xxorient] ,
        ORIENT_typestr[dset->daxes->yyorient] ,
        ORIENT_typestr[dset->daxes->zzorient]  ) ;
     DSET_delete(dset) ;
   }

   exit(0) ;
}
