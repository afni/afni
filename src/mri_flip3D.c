#include "mrilib.h"

/** NOT 7D SAFE **/

/*----------------------------------------------------------------------*/
/*! Flip a 3D image.  The (outx,outy,outz) parameters specify the
    direction of the output axes relative to the input axes:
     - +1 => +x
     - -1 => -x
     - +2 => +y
     - -2 => -y
     - +3 => +z
     - -3 => -z

    Among (outx,outy,outz), exactly 1 must be either +1 or -1, one must
    be +2 or -2, and one must be +3 or -3.  Bad inputs result in a NULL
    return.
------------------------------------------------------------------------*/

MRI_IMAGE * mri_flip3D( int outx, int outy, int outz, MRI_IMAGE *inim )
{
   MRI_IMAGE *outim ;
   int ii,jj,kk , dsiz , nxin,nyin,nzin , nxout,nyout,nzout ;
   int nxyin , nxyout , ijk_out , ijk_in ;
   int ax,bx,cx,dx , ay,by,cy,dy , az,bz,cz,dz , aa,bb,cc,dd ;
   char *inar , *outar ;
   float delx,dely,delz ;

   if( inim == NULL || outx == 0 || outy == 0 || outz == 0 ) return NULL ;
   ii = abs(outx) ; jj = abs(outy) ; kk = abs(outz) ;
   if( ii > 3 || jj > 3 || kk > 3 )                          return NULL ;
   if( ii == jj || ii == kk || jj == kk )                    return NULL ;
   if( ii+jj+kk != 6 )                                       return NULL ;

   if( outx==1 && outy==2 && outz==3 ) return mri_copy(inim) ;  /* easy case */

   nxin = inim->nx ;
   nyin = inim->ny ; nxyin = nxin*nyin ;
   nzin = inim->nz ;

   /* setup so that i_out = ax + bx*i_in + cx*j_in + dx*k_in,
       for i_in=0..nxin-1, j_in=0..nyin-1, k_in=0..nzin-1,
      and then similarly for y and z axes                    */

   switch( outx ){
     case  1:  ax=0     ; bx= 1; cx= 0; dx= 0; nxout=nxin; delx=inim->dx; break;
     case -1:  ax=nxin-1; bx=-1; cx= 0; dx= 0; nxout=nxin; delx=inim->dx; break;
     case  2:  ax=0     ; bx= 0; cx= 1; dx= 0; nxout=nyin; delx=inim->dy; break;
     case -2:  ax=nyin-1; bx= 0; cx=-1; dx= 0; nxout=nyin; delx=inim->dy; break;
     case  3:  ax=0     ; bx= 0; cx= 0; dx= 1; nxout=nzin; delx=inim->dz; break;
     case -3:  ax=nzin-1; bx= 0; cx= 0; dx=-1; nxout=nzin; delx=inim->dz; break;
     default: return NULL ;
   }
   switch( outy ){
     case  1:  ay=0     ; by= 1; cy= 0; dy= 0; nyout=nxin; dely=inim->dx; break;
     case -1:  ay=nxin-1; by=-1; cy= 0; dy= 0; nyout=nxin; dely=inim->dx; break;
     case  2:  ay=0     ; by= 0; cy= 1; dy= 0; nyout=nyin; dely=inim->dy; break;
     case -2:  ay=nyin-1; by= 0; cy=-1; dy= 0; nyout=nyin; dely=inim->dy; break;
     case  3:  ay=0     ; by= 0; cy= 0; dy= 1; nyout=nzin; dely=inim->dz; break;
     case -3:  ay=nzin-1; by= 0; cy= 0; dy=-1; nyout=nzin; dely=inim->dz; break;
     default: return NULL ;
   }
   switch( outz ){
     case  1:  az=0     ; bz= 1; cz= 0; dz= 0; nzout=nxin; delz=inim->dx; break;
     case -1:  az=nxin-1; bz=-1; cz= 0; dz= 0; nzout=nxin; delz=inim->dx; break;
     case  2:  az=0     ; bz= 0; cz= 1; dz= 0; nzout=nyin; delz=inim->dy; break;
     case -2:  az=nyin-1; bz= 0; cz=-1; dz= 0; nzout=nyin; delz=inim->dy; break;
     case  3:  az=0     ; bz= 0; cz= 0; dz= 1; nzout=nzin; delz=inim->dz; break;
     case -3:  az=nzin-1; bz= 0; cz= 0; dz=-1; nzout=nzin; delz=inim->dz; break;
     default: return NULL ;
   }
   nxyout = nxout*nyout ;

   /* 3D index ijk_out = i_out + nxout*j_out + nxyout*k_out

                       = (ax + bx*i_in + cx*j_in + dx*k_in)
                        +(ay + by*i_in + cy*j_in + dy*k_in)*nxout
                        +(az + bz*i_in + cz*j_in + dz*k_in)*nxyout

                       = (ax+ay*nxout+az*nxyout)
                        +(bx+by*nxout+bz*nxyout)*i_in
                        +(cx+cy*nxout+cz*nxyout)*j_in
                        +(dx+dy*nxout+dz*nxyout)*k_in

                       = aa + bb*i_in + cc*j_in + dd*k_in  */

   inar  = mri_data_pointer( inim ) ;
   outim = mri_new_vol( nxout,nyout,nzout , inim->kind ) ;

   outim->dx = delx ; outim->dy = dely ; outim->dz = delz ;

   outar = mri_data_pointer( outim ) ;

   if( inar == NULL ){       /* empty input ==> empty output */
     free(outar) ; mri_fix_data_pointer(NULL,outim ); return outim;
   }

   dsiz  = outim->pixel_size ;          /* size of each voxel in bytes */

   aa = (ax+ay*nxout+az*nxyout)*dsiz ;  /* same as aa, etc. in above */
   bb = (bx+by*nxout+bz*nxyout)*dsiz ;  /* comment, but scaled to be */
   cc = (cx+cy*nxout+cz*nxyout)*dsiz ;  /* address offset in bytes   */
   dd = (dx+dy*nxout+dz*nxyout)*dsiz ;

   for( ijk_in=kk=0 ; kk < nzin ; kk++ ){
     for( jj=0 ; jj < nyin ; jj++ ){
       for( ii=0 ; ii < nxin ; ii++,ijk_in+=dsiz ){
         ijk_out = aa + bb*ii + cc*jj + dd*kk ;
         memcpy( outar+ijk_out , inar+ijk_in , dsiz ) ;
       }
     }
   }

   return outim ;
}
