#include "mrilib.h"

typedef struct {
   int nx, ny, nz ;
   byte * vol ;
   byte * xyhit , * yzhit , * zxhit ;
} BYTE_vol ;

void free_BYTE_vol( BYTE_vol * bv , int freedata )
{
   if( bv != NULL ){
      if( bv->vol != NULL && freedata ) free(bv->vol) ;
      free(bv->xyhit) ; free(bv->yzhit) ; free(bv->zxhit) ;
      free(bv) ;
   }
   return ;
}

BYTE_vol * new_BYTE_vol( int nx, int ny, int nz, byte * vol, int copydata )
{
   BYTE_vol * bv ;
   int ii,jj,kk,vv , nxy ;
   byte * bz , *bxy,*byz,*bzx ;

   bv = (BYTE_vol *) malloc(sizeof(BYTE_vol)) ;
   bv->nx = nx ; bv->ny = ny ; bv->nz = nz ; nxy = nx*ny ;
   if( copydata ){
      bv->vol = (byte *) malloc(sizeof(byte)*nxy*nz) ;
      memcpy( bv->vol , vol , sizeof(byte)*nxy*nz ) ;
   } else {
      bv->vol = vol ;
   }

   bv->xyhit = (byte *) calloc(1,sizeof(byte)*nxy) ;
   bv->yzhit = (byte *) calloc(1,sizeof(byte)*ny*nz) ;
   bv->zxhit = (byte *) calloc(1,sizeof(byte)*nz*nx) ;

   for( byz=bv->yzhit,kk=0 ; kk < nz ; kk++,byz+=ny ){
      bz = vol + kk*nxy ;
      for( bxy=bv->xyhit,jj=0 ; jj < ny ; jj++,bz+=nx,bxy+=nx ){
         for( bzx=bv->zxhit,ii=0 ; ii < nx ; ii++,bzx+=nz ){
            if( bz[ii] ){ bxy[ii] = byz[jj] = bzx[kk] = 1 ; }
         }
      }
   }

   return bv ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * in_dset ;
   int nx,ny,nz , pp,ploop ;
   BYTE_vol * bv ;
   double ct ;

   if( argc < 3 ){ printf("Usage: qqq loops bytedset\n") ; exit(0) ; }

   ploop = strtol(argv[1],NULL,10) ;
   if( ploop < 1 ){ fprintf(stderr,"loop=%d?\n",ploop);exit(1);}

   in_dset = THD_open_dataset( argv[2] ) ;
   DSET_load(in_dset) ;
   if( in_dset == NULL ){fprintf(stderr,"can't open dataset?\n");exit(1);}
   if( DSET_NVALS(in_dset) > 1 ){fprintf(stderr,"nvals > 1?\n");exit(1);}
   if( DSET_BRICK_TYPE(in_dset,0) != MRI_byte ){fprintf(stderr,"not byte?\n");exit(1);}

   nx = DSET_NX(in_dset) ;
   ny = DSET_NY(in_dset) ;
   nz = DSET_NZ(in_dset) ;

   ct = COX_cpu_time() ;

   for( pp=0 ; pp < ploop ; pp++ ){
      bv = new_BYTE_vol( nx,ny,nz , DSET_ARRAY(in_dset,0) , 0 ) ;
      if( pp < ploop-1 ) free_BYTE_vol(bv,0) ;
   }

   ct = (COX_cpu_time() - ct)/ploop ;
   fprintf(stderr,"CPU time per loop = %g\n",ct) ;

   { MRI_IMAGE * bim ; int ii , nn , ntot , n2 ;
     bim = mri_new_vol_empty( nx , ny , 1 , MRI_byte ) ;
     for( nn=ii=0 ; ii < bim->nvox ; ii++ ) nn += (bv->xyhit[ii] == 0) ;
     mri_fix_data_pointer( bv->xyhit , bim ) ;
     mri_write( "qqq_xy.pgm" , bim ) ; mri_free(bim) ;
     fprintf(stderr,"N xy = %d\n",nn) ; ntot = nn ;

     bim = mri_new_vol_empty( ny , nz , 1 , MRI_byte ) ;
     for( nn=ii=0 ; ii < bim->nvox ; ii++ ) nn += (bv->yzhit[ii] == 0) ;
     mri_fix_data_pointer( bv->yzhit , bim ) ;
     mri_write( "qqq_yz.pgm" , bim ) ; mri_free(bim) ;
     fprintf(stderr,"N yz = %d\n",nn) ; ntot += nn ;

     bim = mri_new_vol_empty( nz , nx , 1 , MRI_byte ) ;
     for( nn=ii=0 ; ii < bim->nvox ; ii++ ) nn += (bv->zxhit[ii] == 0) ;
     mri_fix_data_pointer( bv->zxhit , bim ) ;
     mri_write( "qqq_zx.pgm" , bim ) ; mri_free(bim) ;
     fprintf(stderr,"N zx = %d\n",nn) ; ntot += nn ;
     n2 = nx*ny + ny*nz + nz*nx ;
     fprintf(stderr,"N total = %d / %d = %g\n",
             ntot,n2, ((double)(ntot))/((double)(n2)) ) ;
   }

   exit(0) ;
}
