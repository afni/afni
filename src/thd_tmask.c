/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/* a Tmask shows which 1D lines through a 3D volume have nonzero entries */
/*-----------------------------------------------------------------------*/

void free_Tmask( Tmask * tm )
{
   if( tm != NULL ){
      free(tm->mask[0]) ; free(tm->mask[1]) ; free(tm->mask[2]) ; free(tm) ;
   }
   return ;
}

/*-----------------------------------------------------------------------*/

Tmask * create_Tmask_byte( int nx, int ny, int nz, byte * vol )
{
   Tmask * tm ;
   int ii,jj,kk,vv , nxy,nyz,nzx ;
   byte * bz , *xym,*yzm,*zxm , *bxy,*byz,*bzx ;

   tm = (Tmask *) malloc(sizeof(Tmask)) ;
   tm->nmask[TM_IXY] = nxy = nx*ny ;
   tm->nmask[TM_IYZ] = nyz = ny*nz ;
   tm->nmask[TM_IZX] = nzx = nz*nx ;

   tm->mask[TM_IXY] = xym = (byte *) calloc(1,sizeof(byte)*nxy) ;
   tm->mask[TM_IYZ] = yzm = (byte *) calloc(1,sizeof(byte)*nyz) ;
   tm->mask[TM_IZX] = zxm = (byte *) calloc(1,sizeof(byte)*nzx) ;

   for( byz=yzm,kk=0 ; kk < nz ; kk++,byz+=ny ){
      bz = vol + kk*nxy ;
      for( bxy=xym,jj=0 ; jj < ny ; jj++,bz+=nx,bxy+=nx ){
         for( bzx=zxm,ii=0 ; ii < nx ; ii++,bzx+=nz ){
            if( bz[ii] ){ bxy[ii] = byz[jj] = bzx[kk] = 1 ; }
         }
      }
   }

   return tm ;
}
