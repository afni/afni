#include "mrilib.h"

typedef struct {
   int   nmask[3] ;
   byte * mask[3] ;
} Tmask ;

void free_Tmask( Tmask * tm )
{
   if( tm != NULL ){
      free(tm->mask[0]) ; free(tm->mask[1]) ; free(tm->mask[2]) ; free(tm) ;
   }
   return ;
}

#define IXY 2  /* fixdir-1 for each plane */
#define IYZ 0
#define IZX 1

Tmask * create_Tmask( int nx, int ny, int nz, byte * vol )
{
   Tmask * tm ;
   int ii,jj,kk,vv , nxy,nyz,nzx ;
   byte * bz , *xym,*yzm,*zxm , *bxy,*byz,*bzx ;

   tm = (Tmask *) malloc(sizeof(Tmask)) ;
   tm->nmask[IXY] = nxy = nx*ny ;
   tm->nmask[IYZ] = nyz = ny*nz ;
   tm->nmask[IZX] = nzx = nz*nx ;

   tm->mask[IXY] = xym = (byte *) calloc(1,sizeof(byte)*nxy) ;
   tm->mask[IYZ] = yzm = (byte *) calloc(1,sizeof(byte)*nyz) ;
   tm->mask[IZX] = zxm = (byte *) calloc(1,sizeof(byte)*nzx) ;

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

/*===========================================================================
   Functions to extract a plane of shifted bytes from a 3D volume.
     nx, ny, nz = dimensions of vol
     vol        = input 3D volume of bytes

     fixdir = fixed direction (1=x, 2=y, 3=z)
     fixijk = fixed index
     da, db = shift in planar coordinaes (non-fixed directions)
     ma, mb = dimensions of im
     im     = output 2D image

   Goal is im[a,b] = vol[ P(a-da,b-db,c=fixijk) ] for a=0..ma-1, b=0..mb-1,
   where P(a,b,c) is the permutation of (a,b,c) that goes with fixdir:
     P(x,y,z) = (y,z,x) for fixdir == 1
     P(x,y,z) = (z,x,y) for fixdir == 2
     P(x,y,z) = (x,y,z) for fixdir == 3
   For values outside the range of vol[], im[] is set to 0.

   The five interpolation routines that follow are:
     _nn   = nearest neigbhor "interpolation"
     _lifl = linear interpolation, with floating point arithmetic
     _liby = linear interpolation, with byte arithmetic
     _ts   = two-step interpolation
     _fs   = four-step interpolation
=============================================================================*/

  /* macros for offsets in vol[] to corners of the interpolation square */

#undef LL
#undef LR
#undef UL
#undef UR

#define LL 0                /* lower left  */
#define LR astep            /* lower right */
#define UL bstep            /* upper left  */
#define UR (astep+bstep)    /* upper right */

#define ASSIGN_DIRECTIONS                                       \
 do{ switch( fixdir ){                                          \
      default:                                                  \
      case 1:            /* x-direction: (a,b,c) = (y,z,x) */   \
         astep = nx ; bstep = nxy ; cstep = 1  ;                \
         na    = ny ; nb    = nz  ; nc    = nx ;                \
      break ;                                                   \
                                                                \
      case 2:            /* y-direction: (a,b,c) = (z,x,y) */   \
         astep = nxy ; bstep = 1  ; cstep = nx ;                \
         na    = nz  ; nb    = nx ; nc    = ny ;                \
      break ;                                                   \
                                                                \
      case 3:            /* z-direction: (a,b,c) = (x,y,z) */   \
         astep = 1  ; bstep = nx ; cstep = nxy ;                \
         na    = nx ; nb    = ny ; nc    = nz  ;                \
      break ;                                                   \
    } } while(0)

/*-----------------------------------------------------------------------*/

void extract_assign_directions( int nx, int ny, int nz, int fixdir ,
                                int *Astep, int *Bstep, int *Cstep ,
                                int *Na   , int *Nb   , int *Nc     )
{
   int astep,bstep,cstep , na,nb,nc , nxy=nx*ny ;

   ASSIGN_DIRECTIONS ;

   *Astep = astep ; *Bstep = bstep ; *Cstep = cstep ;
   *Na    = na    ; *Nb    = nb    ; *Nc    = nc    ; return ;
}

/*-----------------------------------------------------------------------
   NN "interpolation"
-------------------------------------------------------------------------*/

void extract_byte_nn( int nx , int ny , int nz , byte * vol ,
                      Tmask * tm ,
                      int fixdir , int fixijk , float da , float db ,
                      int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   da += 0.5 ; adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da+0.5) */
   db += 0.5 ; bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db+0.5) */

   abot = 0       ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel ; if( atop > ma   ) atop = ma ;

   bbot = 0       ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
      if( mask == NULL || mask[bb] )
         for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
            im[aa+boff] = vol[aoff+ijkoff] ;

   return ;
}

/*---------------------------------------------------------------------------
    Linear interpolation with floating point arithmetic
-----------------------------------------------------------------------------*/

void extract_byte_lifl( int nx , int ny , int nz , byte * vol ,
                        Tmask * tm ,
                        int fixdir , int fixijk , float da , float db ,
                        int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;
   float fa , fb ;
   float f_a_b , f_ap_b , f_a_bp , f_ap_bp ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of dj */
   fb = db - bdel ;               /* fractional part of dk */

   adel++ ; bdel++ ;

   f_a_b   = fa      * fb      ;
   f_ap_b  = (1.0-fa)* fb      ;
   f_a_bp  = fa      *(1.0-fb) ;
   f_ap_bp = (1.0-fa)*(1.0-fb) ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
      if( mask == NULL || mask[bb] || mask[bb+1] )
         for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
            im[aa+boff] = (byte)(  f_a_b   * vol[aoff+ijkoff]
                                 + f_ap_b  * vol[aoff+(ijkoff+LR)]
                                 + f_a_bp  * vol[aoff+(ijkoff+UL)]
                                 + f_ap_bp * vol[aoff+(ijkoff+UR)] ) ;
   return ;
}

/*---------------------------------------------------------------------------
    Linear interpolation with fixed point arithmetic
-----------------------------------------------------------------------------*/

void extract_byte_liby( int nx , int ny , int nz , byte * vol ,
                        Tmask * tm ,
                        int fixdir , int fixijk , float da , float db ,
                        int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;
   float fa , fb ;
   float f_a_b , f_ap_b , f_a_bp , f_ap_bp ;
   byte  b_a_b , b_ap_b , b_a_bp , b_ap_bp ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of dj */
   fb = db - bdel ;               /* fractional part of dk */

   adel++ ; bdel++ ;

   f_a_b   = fa      * fb      ;
   f_ap_b  = (1.0-fa)* fb      ;
   f_a_bp  = fa      *(1.0-fb) ;
   f_ap_bp = (1.0-fa)*(1.0-fb) ;

   bb = (int)(256*f_a_b  + 0.499) ; if( bb == 256 ) bb-- ; b_a_b  = (byte) bb ;
   bb = (int)(256*f_ap_b + 0.499) ; if( bb == 256 ) bb-- ; b_ap_b = (byte) bb ;
   bb = (int)(256*f_a_bp + 0.499) ; if( bb == 256 ) bb-- ; b_a_bp = (byte) bb ;
   bb = (int)(256*f_ap_bp+ 0.499) ; if( bb == 256 ) bb-- ; b_ap_bp= (byte) bb ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
      if( mask == NULL || mask[bb] || mask[bb+1] )
        for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
           im[aa+boff] = (byte)((  b_a_b   * vol[aoff+ijkoff]
                                 + b_ap_b  * vol[aoff+(ijkoff+LR)]
                                 + b_a_bp  * vol[aoff+(ijkoff+UL)]
                                 + b_ap_bp * vol[aoff+(ijkoff+UR)] ) >> 8 ) ;
   return ;
}

/*---------------------------------------------------------------------------
    Two-step interpolation
-----------------------------------------------------------------------------*/

#if 0
# define TSBOT 0.3
# define TSTOP 0.7
#else
# define TSBOT 0.25
# define TSTOP 0.75
#endif

void extract_byte_ts( int nx , int ny , int nz , byte * vol ,
                      Tmask * tm ,
                      int fixdir , int fixijk , float da , float db ,
                      int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nts,dts1,dts2 ;
   float fa , fb ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of dj */
   fb = db - bdel ;               /* fractional part of dk */

   fa = 1.0-fa ; fb = 1.0-fb ;

   if( fa < TSBOT ){                      /*- Left 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LL ;               /* [0,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UL ;               /* [0,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LL ; dts2 = UL ;   /* mid of [0,0] and [0,1] */
      }
   } else if( fa > TSTOP ){               /*- Right 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LR ;               /* [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UR ;               /* [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LR ; dts2 = UR ;   /* mid of [1,0] and [1,1] */
      }
   } else {                               /*- Middle 40% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 2 ; dts1 = LL ; dts2 = LR ;   /* mid of [0,0] and [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 2 ; dts1 = UL ; dts2 = UR ;   /* mid of [0,1] and [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 4 ;                           /* mid of all 4 points */
      }
   }

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   switch( nts ){

      case 1:
         ijkoff += dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = vol[aoff+ijkoff] ;
      break ;

      case 2:
         ijkoff += dts1 ; dts2 -= dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = (vol[aoff+ijkoff] + vol[aoff+(ijkoff+dts2)]) >> 1;
      break ;

      case 4:
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( vol[aoff+ijkoff]     +vol[aoff+(ijkoff+LR)]
                              +vol[aoff+(ijkoff+UL)]+vol[aoff+(ijkoff+UR)]) >> 2;
      break ;
   }

   return ;
}

/*---------------------------------------------------------------------------
    Four-step interpolation
-----------------------------------------------------------------------------*/

#if 0
# define FSA 0.175
# define FSB 0.400
# define FSC 0.600
# define FSD 0.825
#else
# define FSA 0.125
# define FSB 0.375
# define FSC 0.625
# define FSD 0.875
#endif

void extract_byte_fs( int nx , int ny , int nz , byte * vol ,
                      Tmask * tm ,
                      int fixdir , int fixijk , float da , float db ,
                      int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nfs,dfs1,dfs2,dfs3,dfs4 , ap,bp ;
   float fa , fb ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of dj */
   fb = db - bdel ;               /* fractional part of dk */

   fa = 1.0-fa ; fb = 1.0-fb ;   /* weights for right/upper sides */

        if( fa < FSA ) ap = 0 ;  /* left-right position */
   else if( fa < FSB ) ap = 1 ;
   else if( fa < FSC ) ap = 2 ;
   else if( fa < FSD ) ap = 3 ;
   else                ap = 4 ;

        if( fb < FSA ) bp = 0 ;  /* down-up position */
   else if( fb < FSB ) bp = 1 ;
   else if( fb < FSC ) bp = 2 ;
   else if( fb < FSD ) bp = 3 ;
   else                bp = 4 ;

   /*----- 5x5 grid of possible interpolation cases (nfs): -----------------

                   bp = 4|  1 3 2 3 1     04 14 24 34 44 <- grid of
                        3|  3 4 5 4 3     03 13 23 33 43 <- 10*ap + bp
                        2|  2 5 6 5 2     02 12 22 32 42 <- values
                        1|  3 4 5 4 3     01 11 21 31 41
                        0|  1 3 2 3 1     00 10 20 30 40
                           -----------
                       ap = 0 1 2 3 4

     ----- The indices and nfs cases are assigned in the switch below. -----*/


   dfs2=dfs3=dfs4=-1 ;
   switch( 10*ap + bp ){

      default: return ;  /* should never be executed */

      case 00: nfs = 1 ; dfs1 = LL ; break ;              /* 1 point */
      case 04: nfs = 1 ; dfs1 = UL ; break ;
      case 40: nfs = 1 ; dfs1 = LR ; break ;
      case 44: nfs = 1 ; dfs1 = UR ; break ;

      case 20: nfs = 2 ; dfs1 = LL ; dfs2 = LR ; break ;  /* 2 points:  */
      case 02: nfs = 2 ; dfs1 = LL ; dfs2 = UL ; break ;  /* 1/2 = dfs1 */
      case 24: nfs = 2 ; dfs1 = UL ; dfs2 = UR ; break ;  /* 1/2 = dfs2 */
      case 42: nfs = 2 ; dfs1 = LR ; dfs2 = UR ; break ;

      case 10: nfs = 3 ; dfs1 = LL ; dfs2 = LR ; break ;  /* 2 points:  */
      case 30: nfs = 3 ; dfs1 = LR ; dfs2 = LL ; break ;  /* 3/4 = dfs1 */
      case 01: nfs = 3 ; dfs1 = LL ; dfs2 = UL ; break ;  /* 1/4 = dfs2 */
      case 03: nfs = 3 ; dfs1 = UL ; dfs2 = LL ; break ;
      case 14: nfs = 3 ; dfs1 = UL ; dfs2 = UR ; break ;
      case 34: nfs = 3 ; dfs1 = UR ; dfs2 = UL ; break ;
      case 41: nfs = 3 ; dfs1 = LR ; dfs2 = UR ; break ;
      case 43: nfs = 3 ; dfs1 = UR ; dfs2 = LR ; break ;

      case 11: nfs = 4 ; dfs1 = LL ; dfs2 = LR ;          /* 4 points:   */
                         dfs3 = UL ; dfs4 = UR ; break ;  /* 9/16 = dfs1 */
      case 13: nfs = 4 ; dfs1 = UL ; dfs2 = UR ;          /* 3/16 = dfs2 */
                         dfs3 = LL ; dfs4 = LR ; break ;  /* 3/16 = dfs3 */
      case 31: nfs = 4 ; dfs1 = LR ; dfs2 = LL ;          /* 1/16 = dfs4 */
                         dfs3 = UR ; dfs4 = UL ; break ;
      case 33: nfs = 4 ; dfs1 = UR ; dfs2 = UL ;
                         dfs3 = LR ; dfs4 = LL ; break ;

      case 12: nfs = 5 ; dfs1 = LL ; dfs2 = UL ;          /* 4 points:  */
                         dfs3 = LR ; dfs4 = UR ; break ;  /* 3/8 = dfs1 */
      case 21: nfs = 5 ; dfs1 = LL ; dfs2 = LR ;          /* 3/8 = dfs2 */
                         dfs3 = UL ; dfs4 = UR ; break ;  /* 1/8 = dfs3 */
      case 23: nfs = 5 ; dfs1 = UL ; dfs2 = UR ;          /* 1/8 = dfs4 */
                         dfs3 = LL ; dfs4 = LR ; break ;
      case 32: nfs = 5 ; dfs1 = LR ; dfs2 = UR ;
                         dfs3 = LL ; dfs4 = UL ; break ;

      case 22: nfs = 6 ; dfs1 = LL ; dfs2 = LR ;          /* 4 points: */
                         dfs3 = UL ; dfs4 = UR ; break ;  /* 1/4 = all */
   }

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

#if 0
printf("fixijk=%3d  nfs=%d  dfs1=%d  dfs2=%d  dfs3=%d  dfs4=%d\n",
        fixijk,nfs,dfs1,dfs2,dfs3,dfs4);
#endif

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   switch( nfs ){

      case 1:                                          /* 1 point (NN copy) */
         ijkoff += dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = vol[aoff+ijkoff] ;
      break ;

      case 2:                                          /* 2 points (1/2+1/2) */
         ijkoff += dfs1 ; dfs2 -= dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = (vol[aoff+ijkoff] + vol[aoff+(ijkoff+dfs2)]) >> 1 ;
      break ;

      case 3:                                          /* 2 points (3/4+1/4) */
         ijkoff += dfs1 ; dfs2 -= dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( (vol[aoff+ijkoff] << 1) + vol[aoff+ijkoff]
                              + vol[aoff+(ijkoff+dfs2)]                  ) >> 2 ;
      break ;

      case 4:                                          /* 4 points (9/16+3/16+3/16+1/16) */
         ijkoff += dfs1 ; dfs2 -= dfs1 ; dfs3 -= dfs1 ; dfs4 -= dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( (vol[aoff+ijkoff] << 3)
                              + vol[aoff+ijkoff]
                              +((vol[aoff+(ijkoff+dfs2)] + vol[aoff+(ijkoff+dfs3)]) << 1)
                              + (vol[aoff+(ijkoff+dfs2)] + vol[aoff+(ijkoff+dfs3)])
                              + vol[aoff+(ijkoff+dfs4)]                                ) >> 4 ;
      break ;

      case 5:                                          /* 4 points (3/8+3/8+1/8+1/8) */
         ijkoff += dfs1 ; dfs2 -= dfs1 ; dfs3 -= dfs1 ; dfs4 -= dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( ((vol[aoff+ijkoff] + vol[aoff+(ijkoff+dfs2)]) << 1)
                              + (vol[aoff+ijkoff] + vol[aoff+(ijkoff+dfs2)])
                              + vol[aoff+(ijkoff+dfs3)] + vol[aoff+(ijkoff+dfs4)] ) >> 3 ;
      break;

      case 6:                                          /* 4 points (1/4+1/4+1/4+1/4) */
         ijkoff += dfs1 ; dfs2 -= dfs1 ; dfs3 -= dfs1 ; dfs4 -= dfs1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
            for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = (  vol[aoff+ijkoff]        + vol[aoff+(ijkoff+dfs2)]
                              + vol[aoff+(ijkoff+dfs3)] + vol[aoff+(ijkoff+dfs4)] ) >> 2 ;
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

void extract_byte_speedtest( int nrep , int fixdir , float * ct )
{
   double cputim ;
   int pp , nx=161,ny=191,nz=141,nxy=nx*ny ,
       kk , ma,mb,mab , apad,bpad ;
   float aa=0.347 , bb=-0.521 , da,db ;
   byte * vin , * vout ;
   int astep,bstep,cstep , na,nb,nc ;

   ASSIGN_DIRECTIONS ;

   /* setup bricks */

   da = fabs( 0.5*aa*(nc-1.0) ) ; db = fabs( 0.5*bb*(nc-1.0) ) ;
   apad = (int)(2.0+da)         ; bpad = (int)(2.0+db) ;
   ma   = na + 2*apad           ; mb   = nb + 2*bpad   ; mab = ma*mb ;

   vin = (byte *) malloc( sizeof(byte) * (na*nb*nc) ) ;
   if( vin == NULL ) return ;

   vout = (byte *) malloc( sizeof(byte) * (ma*mb*nc) ) ;
   if( vout == NULL ){ free(vin) ; return ; }

   vin[0] = 1 ;
   for( kk=1 ; kk < na*nb*nc ; kk++ ) vin[kk] = (byte)((3*vin[kk-1]+7) % 256) ;

#undef BTEST
#define BTEST(func) do{ cputim = COX_cpu_time() ;                    \
                        for( pp=0 ; pp < nrep ; pp++ ){              \
                          for( kk=0 ; kk < nc ; kk++ ){              \
                             da = aa*(kk - 0.5*(nc-1.0)) + apad ;    \
                             db = bb*(kk - 0.5*(nc-1.0)) + bpad ;    \
                             func( nx,ny,nz , vin ,                  \
                                   NULL ,                            \
                                   fixdir , kk , da , db ,           \
                                   ma , mb , vout + kk*mab ) ;       \
                          }                                          \
                        }                                            \
                        cputim = COX_cpu_time() - cputim ; } while(0)

   BTEST(extract_byte_nn)   ; ct[0] = cputim ;
   BTEST(extract_byte_lifl) ; ct[1] = cputim ;
   BTEST(extract_byte_liby) ; ct[2] = cputim ;
   BTEST(extract_byte_ts)   ; ct[3] = cputim ;
   BTEST(extract_byte_fs)   ; ct[4] = cputim ;

#undef BTEST

   free(vin) ; free(vout) ; return ;
}

/*-----------------------------------------------------------------------
   Simple get/put of a fixed plane (no shifting, zero padding).
-------------------------------------------------------------------------*/

void getplane_byte( int nx , int ny , int nz , byte * vol ,
                    int fixdir , int fixijk , byte * im )
{
   int bb , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   ijkoff = fixijk*cstep ;

   for( bb=0,boff=0 ; bb < nb ; bb++,boff+=na,ijkoff+=bstep )
      for( aa=0,aoff=0 ; aa < na ; aa++,aoff+=astep )
         im[aa+boff] = vol[aoff+ijkoff] ;

   return ;
}

void putplane_byte( int nx , int ny , int nz , byte * vol ,
                    int fixdir , int fixijk , byte * im )
{
   int bb , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   ijkoff = fixijk*cstep ;

   for( bb=0,boff=0 ; bb < nb ; bb++,boff+=na,ijkoff+=bstep )
      for( aa=0,aoff=0 ; aa < na ; aa++,aoff+=astep )
         vol[aoff+ijkoff] = im[aa+boff] ;

   return ;
}

/******************************************************************************
 ******************************************************************************
 ******************************************************************************/

typedef void gfun( int , int , int , byte * , Tmask * ,
                   int , int , float , float , int , int , byte * ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * in_dset ;
   Tmask * tmask ;
   int nx,ny,nz,nxy , kk,ii , ma,mb,mab ,
       apad,bpad , pp,ploop=1,fixdir;
   float aa , bb , da,db ;
   THD_ivec3 iv ;
   byte * vin , * vout , * vmax ;
   MRI_IMAGE * imout , * immax ;
   double cputim ;
   gfun * func = extract_byte_nn ;
   char * cfun = "nn" ;
   int astep,bstep,cstep , na,nb,nc , use_tmask ;

   if( argc < 3 ){
      printf("Usage 1: extor fixdir A B bytedset [loops [suffix]]\n") ;
      printf("Usage 2: extor fixdir loops\n") ;
      exit(0) ;
   }

   fixdir = strtol(argv[1],NULL,10) ;
   if( fixdir < 0 ){ use_tmask = 1 ; fixdir = -fixdir ; }
   if( fixdir<1 || fixdir>3 ){fprintf(stderr,"fixdir=%d?\n",fixdir);exit(1);}

   if( argc == 3 ){
      float ct[5] ;
      ploop = strtol(argv[2],NULL,10) ;
      if( ploop < 1 ){ fprintf(stderr,"loop=%d?\n",ploop);exit(1);}
      extract_byte_speedtest( ploop , fixdir , ct ) ;
      printf("Speed test with fixdir=%d\n"
             "_nn   = %g (%g/rep)\n"
             "_lifl = %g (%g/rep)\n"
             "_liby = %g (%g/rep)\n"
             "_ts   = %g (%g/rep)\n"
             "_fs   = %g (%g/rep)\n" ,
             fixdir ,
             ct[0],ct[0]/ploop, ct[1],ct[1]/ploop,
             ct[2],ct[2]/ploop, ct[3],ct[3]/ploop, ct[4],ct[4]/ploop ) ;
      exit(1) ;
   }

   aa = strtod(argv[2],NULL) ;
   bb = strtod(argv[3],NULL) ;
   if( aa == 0.0 && bb == 0.0 ){fprintf(stderr,"A=B=0?\n");exit(1);}

   if( argc > 5 ){
      ploop = strtol(argv[5],NULL,10) ;
      if( ploop < 1 ){ fprintf(stderr,"loop=%d?\n",ploop);exit(1); }
   }

   if( argc > 6 ){
      cfun = argv[6] ;
      if( strstr(argv[6],"nn") != NULL )
         func = extract_byte_nn ;
      else if( strstr(argv[6],"lifl") != NULL )
         func = extract_byte_lifl ;
      else if( strstr(argv[6],"liby") != NULL )
         func = extract_byte_liby ;
      else if( strstr(argv[6],"ts") != NULL )
         func = extract_byte_ts ;
      else if( strstr(argv[6],"fs") != NULL )
         func = extract_byte_fs ;
      else {
         fprintf(stderr,"Unknown func suffix\n");exit(1);
      }
   }

   in_dset = THD_open_dataset( argv[4] ) ;
   if( in_dset == NULL ){fprintf(stderr,"can't open dataset?\n");exit(1);}
   if( DSET_NVALS(in_dset) > 1 ){fprintf(stderr,"nvals > 1?\n");exit(1);}
   if( DSET_BRICK_TYPE(in_dset,0) != MRI_byte ){fprintf(stderr,"not byte?\n");exit(1);}

   nx = DSET_NX(in_dset) ;
   ny = DSET_NY(in_dset) ;
   nz = DSET_NZ(in_dset) ; nxy = nx*ny ;

   ASSIGN_DIRECTIONS ;

   da = fabs( 0.5*aa*(nc-1.0) ) ; db = fabs( 0.5*bb*(nc-1.0) ) ;
   if( da < 1.0 && db < 1.0 ){fprintf(stderr,"da=%g db=%g ?\n",da,db);exit(1);}

   apad = (int)(2.0+da) ; bpad = (int)(2.0+db) ;
   ma   = na + 2*apad   ; mb   = nb + 2*bpad   ; mab = ma*mb ;

   DSET_load(in_dset) ;
   vin = DSET_BRICK_ARRAY(in_dset,0) ;

   imout = mri_new( ma,mb , MRI_byte ) ; vout = MRI_BYTE_PTR(imout) ;
   immax = mri_new( ma,mb , MRI_byte ) ; vmax = MRI_BYTE_PTR(immax) ;

   tmask = (use_tmask) ? create_Tmask(nx,ny,nz,vin) : NULL ;

   cputim = COX_cpu_time() ;

   for( pp=0 ; pp < ploop ; pp++ ){
     memset( vmax , 0 , mab ) ;
     for( kk=0 ; kk < nc ; kk++ ){
        da = aa*(kk - 0.5*(nc-1.0)) + apad ;
        db = bb*(kk - 0.5*(nc-1.0)) + bpad ;

        func( nx,ny,nz , vin,tmask , fixdir,kk , da,db , ma,mb , vout ) ;

        for( ii=0 ; ii < mab ; ii++ )
          if( vout[ii] > vmax[ii] ) vmax[ii] = vout[ii] ;
     }
   }

   cputim = (COX_cpu_time() - cputim)/ploop ;
   fprintf(stderr,"CPU time per loop = %g [%s]\n",cputim,cfun) ;

   { char fname[128] = "exim_" ;
     strcat(fname,cfun) ; strcat(fname,".pgm") ;
     mri_write( fname , immax ) ;
   }

   exit(0) ;
}
