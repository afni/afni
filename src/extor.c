#include "mrilib.h"

/*---------------------------------------------------------------------------
   im[i,j] = vol[i-dz,j-dj,kz]
-----------------------------------------------------------------------------*/

void extract_byte_ij_nn( int nx , int ny , int nz , byte * vol ,
                         int kz , float di , float dj ,
                         int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jbot,jtop ;
   register int ii , kzoff , joff ;

   memset( im , mi*mj , 0 ) ;  /* initialize output to zero */

   di += 0.5 ; idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di+0.5) */
   dj += 0.5 ; jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj+0.5) */

   ibot = 0       ; if( ibot < idel ) ibot = idel ;       /* range in im[] */
   itop = nx+idel ; if( itop > mi   ) itop = mi ;

   jbot = 0       ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel - (jbot-jdel)*nx ;             /* offset into vol */
   joff  = jbot*mi ;                                      /* offset into im  */

   for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ ) im[ii+joff] = vol[ii+kzoff] ;
      joff  += mi ;
      kzoff += nx ;
   }

   return ;
}

/*------------------------------------------------------------------------*/

void extract_byte_ij_li( int nx , int ny , int nz , byte * vol ,
                         int kz , float di , float dj ,
                         int mi , int mj , byte * im )
{
   int kzoff , idel,jdel , ibot,itop , jbot,jtop ;
   register int ii , jj , joff ;
   float fi , fj ;
   float f_i_j , f_ip_j , f_i_jp , f_ip_jp ;
#ifdef LI_BYTE
   byte  b_i_j , b_ip_j , b_i_jp , b_ip_jp ;
#endif

   memset( im , mi*mj , 0 ) ;  /* initialize output to zero */

   idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di) */
   jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj) */

   fi = di - idel ;               /* fractional part of di */
   fj = dj - jdel ;               /* fractional part of dj */

   f_i_j   = (1.0-fi)*(1.0-fj) ;
   f_ip_j  = fi      *(1.0-fj) ;
   f_i_jp  = (1.0-fi)*fj ;
   f_ip_jp = fi      *fj ;

#ifdef LI_BYTE
   b_i_j   = (byte)(256*f_i_j  + 0.499) ;
   b_ip_j  = (byte)(256*f_ip_j + 0.499) ;
   b_i_jp  = (byte)(256*f_i_jp + 0.499) ;
   ii      = (256-(b_i_j+b_ip_j+b_i_jp)) ;
   b_ip_jp = (ii > 0) ? (byte) ii : 0 ;
#endif

   ibot = 0         ; if( ibot < idel ) ibot = idel ;    /* range in im[] */
   itop = nx+idel-1 ; if( itop > mi   ) itop = mi ;

   jbot = 0         ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel-1 ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel - (jbot-jdel)*nx ;             /* offset into vol */
   joff  = jbot*mi ;                                      /* offset into im  */

   for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ )
#ifdef LI_BYTE
         im[ii+joff] = (byte)((  b_i_j   * vol[ii+kzoff]
                              + b_ip_j  * vol[ii+(kzoff+1)]
                              + b_i_jp  * vol[ii+(kzoff+nx)]
                              + b_ip_jp * vol[ii+(kzoff+nx+1)] ) >> 8);
#else
         im[ii+joff] = (byte)(  f_i_j   * vol[ii+kzoff]
                              + f_ip_j  * vol[ii+(kzoff+1)]
                              + f_i_jp  * vol[ii+(kzoff+nx)]
                              + f_ip_jp * vol[ii+(kzoff+nx+1)] ) ;
#endif
      joff  += mi ;
      kzoff += nx ;
   }

   return ;
}
