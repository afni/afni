#include "mrilib.h"

/*---------------------------------------------------------------------------
   Functions to extract a plane of shifted bytes from a 3D volume.
     nx, ny, nz = dimensions of vol
     vol        = input 3D volume of bytes

     kz     = z-plane index in vol to extract (0 <= kz < nz)
     di     = shift in x
     dj     = shift in y
     mi, mj = dimensions of im
     im     = output 2D image

   The goal is im[i,j] = vol[i-dz,j-dj,kz], for i=0..mi-1, j=0..mj-1,
   where this makes sense.  For [i,j] that would be outside of vol,
   im[i,j] is set to zero.

   The five routines that follow are:
      _nn   = nearest neigbhor interpolation
      _lifl = linear interpolation, with floating point arithmetic
      _liby = linear interpolation, with byte arithmetic
      _ts   = two-step interpolation
      _fs   = four-step interpolation
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
    Nearest neighbor "interpolation"
-----------------------------------------------------------------------------*/

void extract_byte_ij_nn( int nx , int ny , int nz , byte * vol ,
                         int kz , float di , float dj ,
                         int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jj,jbot,jtop ;
   register int ii , kzoff , joff ;

   memset( im , 0 , mi*mj ) ;  /* initialize output to zero */

   di += 0.5 ; idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di+0.5) */
   dj += 0.5 ; jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj+0.5) */

   ibot = 0       ; if( ibot < idel ) ibot = idel ;       /* range in im[] */
   itop = nx+idel ; if( itop > mi   ) itop = mi ;

   jbot = 0       ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel + (jbot-jdel)*nx ;           /* offset into vol */
   joff  = jbot*mi ;                                    /* offset into im  */

   for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ ) im[ii+joff] = vol[ii+kzoff] ;
      joff  += mi ;
      kzoff += nx ;
   }

   return ;
}

/*---------------------------------------------------------------------------
    Linear interpolation with floating point arithmetic
-----------------------------------------------------------------------------*/

void extract_byte_ij_lifl( int nx , int ny , int nz , byte * vol ,
                           int kz , float di , float dj ,
                           int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jj,jbot,jtop ;
   register int ii , kzoff , joff ;
   float fi , fj ;
   float f_i_j , f_ip_j , f_i_jp , f_ip_jp ;

   memset( im , 0 , mi*mj ) ;  /* initialize output to zero */

   idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di) */
   jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj) */

   fi = di - idel ;               /* fractional part of di */
   fj = dj - jdel ;               /* fractional part of dj */

   idel++ ; jdel++ ;

   f_i_j   = fi      * fj      ;
   f_ip_j  = (1.0-fi)* fj      ;
   f_i_jp  = fi      *(1.0-fj) ;
   f_ip_jp = (1.0-fi)*(1.0-fj) ;

   ibot = 0         ; if( ibot < idel ) ibot = idel ;    /* range in im[] */
   itop = nx+idel-1 ; if( itop > mi   ) itop = mi ;

   jbot = 0         ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel-1 ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel + (jbot-jdel)*nx ;          /* offset into vol */
   joff  = jbot*mi ;                                   /* offset into im  */

   for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ )
         im[ii+joff] = (byte)(  f_i_j   * vol[ii+kzoff]
                              + f_ip_j  * vol[ii+(kzoff+1)]
                              + f_i_jp  * vol[ii+(kzoff+nx)]
                              + f_ip_jp * vol[ii+(kzoff+nx+1)] ) ;
      joff  += mi ;
      kzoff += nx ;
   }

   return ;
}

/*---------------------------------------------------------------------------
    Linear interpolation with fixed point arithmetic
-----------------------------------------------------------------------------*/

void extract_byte_ij_liby( int nx , int ny , int nz , byte * vol ,
                           int kz , float di , float dj ,
                           int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jj,jbot,jtop ;
   register int ii , kzoff , joff ;
   float fi , fj ;
   float f_i_j , f_ip_j , f_i_jp , f_ip_jp ;
   byte  b_i_j , b_ip_j , b_i_jp , b_ip_jp ;

   memset( im , 0 , mi*mj ) ;  /* initialize output to zero */

   idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di) */
   jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj) */

   fi = di - idel ;               /* fractional part of di */
   fj = dj - jdel ;               /* fractional part of dj */

   idel++ ; jdel++ ;

   f_i_j   = fi      * fj      ;
   f_ip_j  = (1.0-fi)* fj      ;
   f_i_jp  = fi      *(1.0-fj) ;
   f_ip_jp = (1.0-fi)*(1.0-fj) ;

   b_i_j   = (byte)(256*f_i_j  + 0.499) ;
   b_ip_j  = (byte)(256*f_ip_j + 0.499) ;
   b_i_jp  = (byte)(256*f_i_jp + 0.499) ;
   ii      = (256-(b_i_j+b_ip_j+b_i_jp)) ;
   b_ip_jp = (ii > 0) ? (byte) ii : 0 ;

   ibot = 0         ; if( ibot < idel ) ibot = idel ;    /* range in im[] */
   itop = nx+idel-1 ; if( itop > mi   ) itop = mi ;

   jbot = 0         ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel-1 ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel + (jbot-jdel)*nx ;          /* offset into vol */
   joff  = jbot*mi ;                                   /* offset into im  */

   for( jj=jbot ; jj < jtop ; jj++ ){
      for( ii=ibot ; ii < itop ; ii++ )
         im[ii+joff] = (byte)(( b_i_j   * vol[ii+kzoff]
                              + b_ip_j  * vol[ii+(kzoff+1)]
                              + b_i_jp  * vol[ii+(kzoff+nx)]
                              + b_ip_jp * vol[ii+(kzoff+nx+1)] ) >> 8);
      joff  += mi ;
      kzoff += nx ;
   }

   return ;
}

/*---------------------------------------------------------------------------
    Two-step interpolation
-----------------------------------------------------------------------------*/

#define TSBOT 0.3
#define TSTOP 0.7

void extract_byte_ij_ts( int nx , int ny , int nz , byte * vol ,
                         int kz , float di , float dj ,
                         int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jj,jbot,jtop ;
   register int ii , kzoff , joff ;
   float fi , fj ;
   int nts=0 , dts1,dts2 ;

   memset( im , 0 , mi*mj ) ;  /* initialize output to zero */

   idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di) */
   jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj) */

   fi = di - idel ;               /* fractional part of di */
   fj = dj - jdel ;               /* fractional part of dj */

   idel++ ; jdel++ ; fi = 1.0-fi ; fj = 1.0-fj ;

   if( fi < TSBOT ){                      /*- left 30% -*/
      if( fj < TSBOT ){                   /*- lower 30% -*/
        nts = 1 ; dts1 = 0 ;                /* [0,0] */
      } else if( fj > TSTOP ){            /*- upper 30% -*/
        nts = 1 ; dts1 = nx ;               /* [0,1] */
      } else {                            /*- middle 40% -*/
        nts = 2 ; dts1 = 0 ; dts2 = nx ;    /* mid of [0,0] and [0,1] */
      }
   } else if( fi > TSTOP ){               /*- right 30% -*/
      if( fj < TSBOT ){                   /*- lower 30% -*/
        nts = 1 ; dts1 = 1 ;                /* [1,0] */
      } else if( fj > TSTOP ){            /*- upper 30% -*/
        nts = 1 ; dts1 = nx+1 ;             /* [1,1] */
      } else {
        nts = 2 ; dts1 = 1 ; dts2 = nx+1 ;  /* mid of [1,0] and [1,1] */
      }
   } else {                               /*- middle 40% -*/
      if( fj < TSBOT ){                   /*- lower 30% -*/
        nts = 2 ; dts1 = 0 ; dts2 = 1 ;     /* mid of [0,0] and [1,0] */
      } else if( fj > TSTOP ){            /*- upper 30% -*/
        nts = 2 ; dts1 = nx ; dts2 = nx+1 ; /* mid of [0,1] and [1,1] */
      } else {                            /*- middle 40% -*/
        nts = 4 ;                           /* mid of all 4 points */
      }
   }

   ibot = 0         ; if( ibot < idel ) ibot = idel ;    /* range in im[] */
   itop = nx+idel-1 ; if( itop > mi   ) itop = mi ;

   jbot = 0         ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel-1 ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel + (jbot-jdel)*nx ;          /* offset into vol */
   joff  = jbot*mi ;                                   /* offset into im  */

   switch( nts ){

      case 1:
         kzoff += dts1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ ) im[ii+joff] = vol[ii+kzoff] ;
            joff  += mi ;
            kzoff += nx ;
         }
      break ;

      case 2:
         kzoff += dts1 ;
         dts2   = dts2 - dts1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = (vol[ii+kzoff] + vol[ii+(kzoff+dts2)]) >> 1 ;
            joff  += mi ;
            kzoff += nx ;
         }
      break ;

      case 4:
      for( jj=jbot ; jj < jtop ; jj++ ){
         for( ii=ibot ; ii < itop ; ii++ )
            im[ii+joff] = (   vol[ii+kzoff]      + vol[ii+(kzoff+1)]
                            + vol[ii+(kzoff+nx)] + vol[ii+(kzoff+nx+1)] ) >> 2 ;
         joff  += mi ;
         kzoff += nx ;
      }
   }

   return ;
}

/*---------------------------------------------------------------------------
    Four-step interpolation
-----------------------------------------------------------------------------*/

#define FSA 0.175
#define FSB 0.400
#define FSC 0.600
#define FSD 0.825

void extract_byte_ij_fs( int nx , int ny , int nz , byte * vol ,
                         int kz , float di , float dj ,
                         int mi , int mj , byte * im )
{
   int idel,jdel , ibot,itop , jj,jbot,jtop ;
   register int ii , kzoff , joff ;
   float fi , fj ;
   int nfs , dfs1,dfs2,dfs3,dfs4 , xp,yp ;

   memset( im , 0 , mi*mj ) ;  /* initialize output to zero */

   idel = (int) di ; if( di < 0.0 ) idel-- ;  /* floor(di) */
   jdel = (int) dj ; if( dj < 0.0 ) jdel-- ;  /* floor(dj) */

   fi = di - idel ;               /* fractional part of di */
   fj = dj - jdel ;               /* fractional part of dj */

   idel++ ; jdel++ ;

   fi = 1.0-fi ; fj = 1.0-fj ;   /* weights for right/upper sides */

        if( fi < FSA ) xp = 0 ;  /* left-right position */
   else if( fi < FSB ) xp = 1 ;
   else if( fi < FSC ) xp = 2 ;
   else if( fi < FSD ) xp = 3 ;
   else                xp = 4 ;

        if( fj < FSA ) yp = 0 ;  /* down-up position */
   else if( fj < FSB ) yp = 1 ;
   else if( fj < FSC ) yp = 2 ;
   else if( fj < FSD ) yp = 3 ;
   else                yp = 4 ;

   /*----- 5x5 grid of possible interpolation cases (nfs): -----------------

                   yp = 4|  1 3 2 3 1     04 14 24 34 44 <- grid of
                        3|  3 4 5 4 3     03 13 23 33 43 <- 10*xp + yp
                        2|  2 5 6 5 2     02 12 22 32 42 <- values
                        1|  3 4 5 4 3     01 11 21 31 41
                        0|  1 3 2 3 1     00 10 20 30 40
                           -----------
                       xp = 0 1 2 3 4

     ----- The indices and nfs cases are assigned in the switch below. -----*/


   switch( 10*xp + yp ){

      default: fprintf(stderr,"** extract_byte_ij_fs: xp=%d yp=%d\n",xp,yp);exit(1);

      case 00: nfs = 1 ; dfs1 = 0    ; break ;                /* 1 point */
      case 04: nfs = 1 ; dfs1 = nx   ; break ;
      case 40: nfs = 1 ; dfs1 = 1    ; break ;
      case 44: nfs = 1 ; dfs1 = nx+1 ; break ;

      case 20: nfs = 2 ; dfs1 = 0  ; dfs2 = 1    ; break ;    /* 2 points:  */
      case 02: nfs = 2 ; dfs1 = 0  ; dfs2 = nx   ; break ;    /* 1/2 = dfs1 */
      case 24: nfs = 2 ; dfs1 = nx ; dfs2 = nx+1 ; break ;    /* 1/2 = dfs2 */
      case 42: nfs = 2 ; dfs1 = 1  ; dfs2 = nx+1 ; break ;

      case 10: nfs = 3 ; dfs1 = 0    ; dfs2 = 1    ; break ;  /* 2 points:  */
      case 30: nfs = 3 ; dfs1 = 1    ; dfs2 = 0    ; break ;  /* 3/4 = dfs1 */
      case 01: nfs = 3 ; dfs1 = 0    ; dfs2 = nx   ; break ;  /* 1/4 = dfs2 */
      case 03: nfs = 3 ; dfs1 = nx   ; dfs2 = 0    ; break ;
      case 14: nfs = 3 ; dfs1 = nx   ; dfs2 = nx+1 ; break ;
      case 34: nfs = 3 ; dfs1 = nx+1 ; dfs2 = nx   ; break ;
      case 41: nfs = 3 ; dfs1 = 1    ; dfs2 = nx+1 ; break ;
      case 43: nfs = 3 ; dfs1 = nx+1 ; dfs2 = 1    ; break ;

      case 11: nfs = 4 ; dfs1 = 0    ; dfs2 = 1    ;          /* 4 points:   */
                         dfs3 = nx   ; dfs4 = nx+1 ; break ;  /* 9/16 = dfs1 */
      case 13: nfs = 4 ; dfs1 = nx   ; dfs2 = nx+1 ;          /* 3/16 = dfs2 */
                         dfs3 = 0    ; dfs4 = 1    ; break ;  /* 3/16 = dfs3 */
      case 31: nfs = 4 ; dfs1 = 1    ; dfs2 = 0    ;          /* 1/16 = dfs4 */
                         dfs3 = nx+1 ; dfs4 = nx   ; break ;
      case 33: nfs = 4 ; dfs1 = nx+1 ; dfs2 = nx   ;
                         dfs3 = 1    ; dfs4 = 0    ; break ;

      case 12: nfs = 5 ; dfs1 = 0    ; dfs2 = nx   ;          /* 4 points:  */
                         dfs3 = 1    ; dfs4 = nx+1 ; break ;  /* 3/8 = dfs1 */
      case 21: nfs = 5 ; dfs1 = 0    ; dfs2 = 1    ;          /* 3/8 = dfs2 */
                         dfs3 = nx   ; dfs4 = nx+1 ; break ;  /* 1/8 = dfs3 */
      case 23: nfs = 5 ; dfs1 = nx   ; dfs2 = nx+1 ;          /* 1/8 = dfs4 */
                         dfs3 = 0    ; dfs4 = 1    ; break ;
      case 32: nfs = 5 ; dfs1 = 1    ; dfs2 = nx+1 ;
                         dfs3 = 0    ; dfs4 = nx   ; break ;

      case 22: nfs = 6 ; dfs1 = 0    ; dfs2 = 1    ;          /* 4 points: */
                         dfs3 = nx   ; dfs4 = nx+1 ; break ;  /* 1/4 = all */
   }

   ibot = 0         ; if( ibot < idel ) ibot = idel ;    /* range in im[] */
   itop = nx+idel-1 ; if( itop > mi   ) itop = mi ;

   jbot = 0         ; if( jbot < jdel ) jbot = jdel ;
   jtop = ny+jdel-1 ; if( jtop > mj   ) jtop = mj ;

   kzoff = kz*nx*ny - idel + (jbot-jdel)*nx ;          /* offset into vol */
   joff  = jbot*mi ;                                   /* offset into im  */

   switch( nfs ){

      case 1:                                          /* 1 point (NN copy) */
         kzoff += dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ ) im[ii+joff] = vol[ii+kzoff] ;
            joff  += mi ;
            kzoff += nx ;
         }
      break ;

      case 2:                                          /* 2 points (1/2+1/2) */
         kzoff += dfs1 ;
         dfs2   = dfs2 - dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = ( vol[ii+kzoff] + vol[ii+(kzoff+dfs2)] ) >> 1 ;
            joff  += mi ;
            kzoff += nx ;
         }
      break ;

      case 3:                                          /* 2 points (3/4+1/4) */
         kzoff += dfs1 ;
         dfs2   = dfs2 - dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = (  (vol[ii+kzoff] << 1)
                              +  vol[ii+kzoff]
                              + vol[ii+(kzoff+dfs2)] ) >> 2 ;
            joff  += mi ;
            kzoff += nx ;
         }
      break ;

      case 4:                                          /* 4 points (9/16+3/16+3/16+1/16) */
         kzoff += dfs1 ;
         dfs2   = dfs2 - dfs1 ;
         dfs3   = dfs3 - dfs1 ;
         dfs4   = dfs4 - dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = (  (vol[ii+kzoff] << 3)
                              +  vol[ii+kzoff]
                              + (( vol[ii+(kzoff+dfs2)] + vol[ii+(kzoff+dfs3)] ) << 1)
                              +  ( vol[ii+(kzoff+dfs2)] + vol[ii+(kzoff+dfs3)] )
                              + vol[ii+(kzoff+dfs4)]                           ) >> 4 ;
         joff  += mi ;
         kzoff += nx ;
      }
      break ;

      case 5:                                          /* 4 points (3/8+3/8+1/8+1/8) */
         kzoff += dfs1 ;
         dfs2   = dfs2 - dfs1 ;
         dfs3   = dfs3 - dfs1 ;
         dfs4   = dfs4 - dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = (  (( vol[ii+kzoff] + vol[ii+(kzoff+dfs2)] ) << 1)
                              +  ( vol[ii+kzoff] + vol[ii+(kzoff+dfs2)] )
                              + vol[ii+(kzoff+dfs3)]
                              + vol[ii+(kzoff+dfs4)] ) >> 3 ;
         joff  += mi ;
         kzoff += nx ;
      }
      break;

      case 6:                                          /* 4 points (1/4+1/4+1/4+1/4) */
         kzoff += dfs1 ;
         dfs2   = dfs2 - dfs1 ;
         dfs3   = dfs3 - dfs1 ;
         dfs4   = dfs4 - dfs1 ;
         for( jj=jbot ; jj < jtop ; jj++ ){
            for( ii=ibot ; ii < itop ; ii++ )
               im[ii+joff] = (  vol[ii+kzoff]
                              + vol[ii+(kzoff+dfs2)]
                              + vol[ii+(kzoff+dfs3)]
                              + vol[ii+(kzoff+dfs4)] ) >> 2 ;
         joff  += mi ;
         kzoff += nx ;
      }
      break;
   }

   return ;
}

/*---------------------------------------------------------------------------
    Test the speeds of the above routines:
      nrep = number of repetitions to execute
      ct   = float [5] array (must be allocated by caller)
             ct[0] = CPU time for _nn
             ct[1] = CPU time for _lifl
             ct[2] = CPU time for _liby
             ct[3] = CPU time for _ts
             ct[4] = CPU time for _fs
-----------------------------------------------------------------------------*/

void extract_byte_ij_speedtest( int nrep , float * ct )
{
   double cputim ;
   int pp , nx=100,ny=100,nz=100,nxy=nx*ny ,
       kk , mx,my,mxy , xpad,ypad ;
   float aa=0.347 , bb=-0.521 , da,db ;
   byte * vin , * vout ;

   /* setup bricks */

   da = fabs( 0.5*aa*(nz-1.0) ) ; db = fabs( 0.5*bb*(nz-1.0) ) ;
   xpad = (int)(2.0+da)         ; ypad = (int)(2.0+db) ;
   mx   = nx + 2*xpad           ; my   = ny + 2*ypad   ; mxy = mx*my ;

   vin = (byte *) malloc( sizeof(byte) * (nx*ny*nz) ) ;
   if( vin == NULL ) return ;

   vout = (byte *) malloc( sizeof(byte) * (mx*my*nz) ) ;
   if( vout == NULL ){ free(vin) ; return ; }

   vin[0] = 1 ;
   for( kk=1 ; kk < nx*ny*nz ; kk++ ) vin[kk] = (byte)((3*vin[kk-1]+7) % 256) ;

#undef BTEST
#define BTEST(func) do{ cputim = COX_cpu_time() ;                    \
                        for( pp=0 ; pp < nrep ; pp++ ){              \
                          for( kk=0 ; kk < nz ; kk++ ){              \
                             da = aa*(kk - 0.5*(nz-1.0)) + xpad ;    \
                             db = bb*(kk - 0.5*(nz-1.0)) + ypad ;    \
                             func( nx,ny,nz , vin ,                  \
                                   kk , da , db ,                    \
                                   mx , my , vout + kk*mxy ) ;       \
                          }                                          \
                        }                                            \
                        cputim = COX_cpu_time() - cputim ; } while(0)

   BTEST(extract_byte_ij_nn)   ; ct[0] = cputim ;
   BTEST(extract_byte_ij_lifl) ; ct[1] = cputim ;
   BTEST(extract_byte_ij_liby) ; ct[2] = cputim ;
   BTEST(extract_byte_ij_ts)   ; ct[3] = cputim ;
   BTEST(extract_byte_ij_fs)   ; ct[4] = cputim ;

#undef BTEST

   free(vin) ; free(vout) ; return ;
}

/******************************************************************************/

typedef void gfun( int , int , int , byte * ,
                   int , float , float , int , int , byte * ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * in_dset , * out_dset ;
   int nx,ny,nz,nxy , kk , mx,my,mxy , xpad,ypad , pp,ploop=1;
   float aa , bb , da,db ;
   THD_ivec3 iv ;
   byte * vin , * vout ;
   double cputim ;
   gfun * func = extract_byte_ij_nn ;

   if( argc < 2 ){
      printf("Usage 1: extor A B bytedset [loops [suffix]]\n") ;
      printf("Usage 2: extor loops\n") ;
      exit(0) ;
   }

   if( argc == 2 ){
      float ct[5] ;
      ploop = strtol(argv[1],NULL,10) ;
      if( ploop < 1 ){ fprintf(stderr,"loop=%d?\n",ploop);exit(1); }
      extract_byte_ij_speedtest( ploop , ct ) ;
      printf("_nn   = %g\n"
             "_lifl = %g\n"
             "_liby = %g\n"
             "_ts   = %g\n"
             "_fs   = %g\n" , ct[0],ct[1],ct[2],ct[3],ct[4] ) ;
      exit(1) ;
   }

   aa = strtod(argv[1],NULL) ;
   bb = strtod(argv[2],NULL) ;
   if( aa == 0.0 && bb == 0.0 ){fprintf(stderr,"A=B=0?\n");exit(1);}

   if( argc > 4 ){
      ploop = strtol(argv[4],NULL,10) ;
      if( ploop < 1 ){ fprintf(stderr,"loop=%d?\n",ploop);exit(1); }
   }

   if( argc > 5 ){
      if( strstr(argv[5],"nn") != NULL )
         func = extract_byte_ij_nn ;
      else if( strstr(argv[5],"lifl") != NULL )
         func = extract_byte_ij_lifl ;
      else if( strstr(argv[5],"liby") != NULL )
         func = extract_byte_ij_liby ;
      else if( strstr(argv[5],"ts") != NULL )
         func = extract_byte_ij_ts ;
      else if( strstr(argv[5],"fs") != NULL )
         func = extract_byte_ij_fs ;
      else {
         fprintf(stderr,"Unknown func suffix\n");exit(1);
      }
   }


   in_dset = THD_open_dataset( argv[3] ) ;
   if( in_dset == NULL ){fprintf(stderr,"can't open dataset?\n");exit(1);}
   if( DSET_NVALS(in_dset) > 1 ){fprintf(stderr,"nvals > 1?\n");exit(1);}
   if( DSET_BRICK_TYPE(in_dset,0) != MRI_byte ){fprintf(stderr,"not byte?\n");exit(1);}

   out_dset = EDIT_empty_copy(in_dset) ;
   EDIT_dset_items( out_dset , ADN_prefix , "extor" , NULL ) ;
   tross_Copy_History( in_dset , out_dset ) ;
   tross_Make_History( "extor" , argc,argv , out_dset ) ;

   nx = DSET_NX(out_dset) ; ny = DSET_NY(out_dset) ; nz = DSET_NZ(out_dset) ; nxy = nx*ny ;

   da = fabs( 0.5*aa*(nz-1.0) ) ; db = fabs( 0.5*bb*(nz-1.0) ) ;
   if( da < 1.0 || db < 1.0 ){fprintf(stderr,"da=%g db=%g ?\n",da,db);exit(1);}

   xpad = (int)(2.0+da) ; ypad = (int)(2.0+db) ;
   mx   = nx + 2*xpad   ; my   = ny + 2*ypad   ; mxy = mx*my ;

   LOAD_IVEC3(iv,mx,my,nz) ;
   EDIT_dset_items( out_dset , ADN_nxyz , iv , NULL ) ;

   DSET_load(in_dset) ;
   vin = DSET_BRICK_ARRAY(in_dset,0) ;
   vout = (byte *) malloc( sizeof(byte) * DSET_NVOX(out_dset) ) ;
   EDIT_substitute_brick( out_dset , 0 , MRI_byte , vout ) ;

   cputim = COX_cpu_time() ;

   for( pp=0 ; pp < ploop ; pp++ ){
     for( kk=0 ; kk < nz ; kk++ ){
        da = aa*(kk - 0.5*(nz-1.0)) + xpad ; db = bb*(kk - 0.5*(nz-1.0)) + ypad ;

        func( nx,ny,nz , vin ,
              kk , da , db ,
              mx , my , vout + kk*mxy ) ;
     }
   }

   cputim = (COX_cpu_time() - cputim)/ploop ;
   fprintf(stderr,"CPU time per loop = %g\n",cputim) ;

   DSET_write(out_dset) ;
   exit(1) ;
}
