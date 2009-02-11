/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*===========================================================================
  Routines to rotate and shift a 3D volume using a 4 way shear decomposition,
  coupled with an FFT-based shifting of the rows -- RWCox [October 1998].
=============================================================================*/

#include "thd_shear3d.h"  /* 23 Oct 2000: moved shear funcs to thd_shear3d.c */

/********** 28 Oct 1999: the shifting routines that were here   **********
 **********              have been removed to file thd_shift2.c **********/

/*---------------------------------------------------------------------------
   Set the interpolation method for shifting:
   input is one of MRI_NN, MRI_LINEAR, MRI_CUBIC, or MRI_FOURIER.
-----------------------------------------------------------------------------*/

typedef void (*shift_func)(int,int,float,float *,float,float *) ;
static  shift_func shifter      = fft_shift2 ;
static  int        shift_method = MRI_FOURIER ;

void THD_rota_method( int mode )
{
   shift_method = mode ;
   switch( mode ){
      case MRI_NN:      shifter = nn_shift2    ; break ;
      case MRI_TSSHIFT: shifter = ts_shift2    ; break ;  /* Dec 1999 */

      case MRI_LINEAR:  shifter = lin_shift2   ; break ;
      case MRI_FOURIER: shifter = fft_shift2   ; break ;
      default:
      case MRI_CUBIC:   shifter = cub_shift2   ; break ;
      case MRI_QUINTIC: shifter = quint_shift2 ; break ;  /* Nov 1998 */
      case MRI_HEPTIC:  shifter = hept_shift2  ; break ;  /* Nov 1998 */

      case MRI_FOURIER_NOPAD: shifter = fft_shift2   ; break ;  /* 13 May 2003 */
   }
   return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,y) axes:
    i <--> nx-1-i    j <--> ny-1-j
-----------------------------------------------------------------------------*/

#define VV(i,j,k) v[(i)+(j)*nx+(k)*nxy]
#define SX(i)     (nx1-(i))
#define SY(j)     (ny1-(j))
#define SZ(k)     (nz1-(k))

static void flip_xy( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*nx) ;  /* save 1 row */

   for( kk=0 ; kk < nz ; kk++ ){              /* for each slice */
      for( jj=0 ; jj < ny2 ; jj++ ){          /* first 1/2 of rows */

         /* swap rows jj and ny1-jj, flipping them in ii as well */

         for( ii=0 ; ii < nx ; ii++ ) r1[ii]           = VV(SX(ii),SY(jj),kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,SY(jj),kk) = VV(SX(ii),jj    ,kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj    ,kk) = r1[ii] ;
      }
      if( ny%2 == 1 ){                                             /* central row? */
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]       = VV(SX(ii),jj,kk) ; /* flip it */
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk) = r1[ii] ;           /* restore */
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (y,z) axes:
     j <--> ny-1-j   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_yz( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*ny) ;

   for( ii=0 ; ii < nx ; ii++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         for( jj=0 ; jj < ny ; jj++ ) r1[jj]           = VV(ii,SY(jj),SZ(kk)) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,SZ(kk)) = VV(ii,SY(jj),kk    ) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,kk    ) = r1[jj] ;
      }
      if( nz%2 == 1 ){
         for( jj=0 ; jj < ny ; jj++ ) r1[jj]       = VV(ii,SY(jj),kk) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,kk) = r1[jj] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,z) axes:
     i <--> nx-1-i   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_xz( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*nx) ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]           = VV(SX(ii),jj,SZ(kk)) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,SZ(kk)) = VV(SX(ii),jj,kk    ) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk    ) = r1[ii] ;
      }
      if( nz%2 == 1 ){
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]       = VV(SX(ii),jj,kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk) = r1[ii] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Apply an x-axis shear to a 3D array: x -> x + a*y + b*z + s
   (dilation factor "f" assumed to be 1.0)
-----------------------------------------------------------------------------*/

static void apply_xshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup=0,nst ;
   float a0 , a1=0 , st ;

ENTRY("apply_xshear") ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*ny2 + fabs(b)*nz2 + fabs(s) ; if( st < 1.e-3 ) EXRETURN ;

   if( shift_method == MRI_FOURIER ){
      nst = nx + 0.5*st ;
      nup = csfft_nextup_one35(nst) ;
   } else if( shift_method == MRI_FOURIER_NOPAD ){
      nup = csfft_nextup_even(nx) ;
   }

   for( kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj+=2 ){
         fj0 = v + (jj*nx + kk*nxy) ;
         fj1 = (jj < ny1) ? (fj0 + nx) : NULL ;   /* allow for odd ny */
         a0  = a*(jj-ny2) + b*(kk-nz2) + s ;
         a1  = a0 + a ;
         shifter( nx , nup , a0 , fj0 , a1 , fj1 ) ;
      }
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Apply a y-axis shear to a 3D array: y -> y + a*x + b*z + s
-----------------------------------------------------------------------------*/

static void apply_yshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup=0,nst ;
   float a0 , a1=0 , st ;

ENTRY("apply_yshear") ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*nx2 + fabs(b)*nz2 + fabs(s) ; if( st < 1.e-3 ) EXRETURN ;

   if( shift_method == MRI_FOURIER ){
      nst = ny + 0.5*st ;
      nup = csfft_nextup_one35(nst) ;
   } else if( shift_method == MRI_FOURIER_NOPAD ){
      nup = csfft_nextup_even(ny) ;
   }

   fj0 = (float *) malloc( sizeof(float) * 2*ny ) ; fj1 = fj0 + ny ;

   for( kk=0 ; kk < nz ; kk++ ){
      for( ii=0 ; ii < nx1 ; ii+=2 ){
         for( jj=0; jj < ny; jj++ ){ fj0[jj] = VV(ii,jj,kk) ; fj1[jj] = VV(ii+1,jj,kk) ; }
         a0 = a*(ii-nx2) + b*(kk-nz2) + s ;
         a1 = a0 + a ;
         shifter( ny , nup , a0 , fj0 , a1 , fj1 ) ;
         for( jj=0; jj < ny; jj++ ){ VV(ii,jj,kk) = fj0[jj] ; VV(ii+1,jj,kk) = fj1[jj] ; }
      }

      if( ii == nx1 ){                                       /* allow for odd nx */
         for( jj=0; jj < ny; jj++ ) fj0[jj] = VV(ii,jj,kk) ;
         a0 = a*(ii-nx2) + b*(kk-nz2) + s ;
         shifter( ny , nup , a0 , fj0 , a1 , NULL ) ;
         for( jj=0; jj < ny; jj++ ) VV(ii,jj,kk) = fj0[jj] ;
      }
   }

   free(fj0) ; EXRETURN ;
}

/*---------------------------------------------------------------------------
   Apply a z-axis shear to a 3D array: z -> z + a*x + b*y + s
-----------------------------------------------------------------------------*/

static void apply_zshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup=0,nst ;
   float a0 , a1=0 , st ;

ENTRY("apply_zshear") ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*nx2 + fabs(b)*ny2 + fabs(s) ; if( st < 1.e-3 ) EXRETURN ;

   if( shift_method == MRI_FOURIER ){
      nst = nz + 0.5*st ;
      nup = csfft_nextup_one35(nst) ;
   } else if( shift_method == MRI_FOURIER_NOPAD ){
      nup = csfft_nextup_even(nz) ;
   }

   fj0 = (float *) malloc( sizeof(float) * 2*nz ) ; fj1 = fj0 + nz ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx1 ; ii+=2 ){
         for( kk=0; kk < nz; kk++ ){ fj0[kk] = VV(ii,jj,kk) ; fj1[kk] = VV(ii+1,jj,kk) ; }
         a0 = a*(ii-nx2) + b*(jj-ny2) + s ;
         a1 = a0 + a ;
         shifter( nz , nup , a0 , fj0 , a1 , fj1 ) ;
         for( kk=0; kk < nz; kk++ ){ VV(ii,jj,kk) = fj0[kk] ; VV(ii+1,jj,kk) = fj1[kk] ; }
      }

      if( ii == nx1 ){                                       /* allow for odd nx */
         for( kk=0; kk < nz; kk++ ) fj0[kk] = VV(ii,jj,kk) ;
         a0 = a*(ii-nx2) + b*(jj-ny2) + s ;
         shifter( nz , nup , a0 , fj0 , a1 , NULL ) ;
         for( kk=0; kk < nz; kk++ ) VV(ii,jj,kk) = fj0[kk] ;
      }
   }

   free(fj0) ; EXRETURN ;
}

/*---------------------------------------------------------------------------
   Apply a set of shears to a 3D array of floats.
   Note that we assume that the dilation factors ("f") are all 1.
-----------------------------------------------------------------------------*/

static void apply_3shear( MCW_3shear shr ,
                          int   nx   , int   ny   , int   nz   , float * vol )
{
   int qq ;
   float a , b , s ;

ENTRY("apply_3shear") ;

   if( ! ISVALID_3SHEAR(shr) ) EXRETURN ;

   /* carry out a preliminary 180 flippo ? */

   if( shr.flip0 >= 0 ){
      switch( shr.flip0 + shr.flip1 ){
         case 1: flip_xy( nx,ny,nz,vol ) ; break ;
         case 2: flip_xz( nx,ny,nz,vol ) ; break ;
         case 3: flip_yz( nx,ny,nz,vol ) ; break ;
        default:                           EXRETURN ;  /* should not occur */
      }
   }

   /* apply each shear */

   for( qq=0 ; qq < 4 ; qq++ ){
      switch( shr.ax[qq] ){
         case 0:
            a = shr.scl[qq][1] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_xshear( a,b,s , nx,ny,nz , vol ) ;
         break ;

         case 1:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_yshear( a,b,s , nx,ny,nz , vol ) ;
         break ;

         case 2:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][1] ;
            s = shr.sft[qq]    ;
            apply_zshear( a,b,s , nx,ny,nz , vol ) ;
         break ;
      }
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Set zero padding size for rotations:
   padding is done before rotate, then stripped off afterwards.
   02 Feb 2001 -- RWCox
-----------------------------------------------------------------------------*/

static int rotpx=0 , rotpy=0 , rotpz = 0 ;
static int rotpset=0 ;

void THD_rota_setpad( int px , int py , int pz )
{
   rotpx = (px > 0) ? px : 0 ;
   rotpy = (py > 0) ? py : 0 ;
   rotpz = (pz > 0) ? pz : 0 ;
   rotpset = 1 ;               /* 05 Feb 2001 */
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_rota_clearpad(void)   /* 05 Feb 2001 */
{
   rotpx=rotpy=rotpz=0; rotpset=1; return;
}

static void THD_rota_envpad(void)
{
   char * eee = my_getenv("AFNI_ROTA_ZPAD") ;
   int ppp ;

   if( rotpset ) return ;
   eee = my_getenv("AFNI_ROTA_ZPAD") ;
   if( eee != NULL ){
      ppp = strtol( eee , NULL , 10 ) ;
      THD_rota_setpad(ppp,ppp,ppp) ;
   }
   rotpset = 1 ; return ;
}

/*---------------------------------------------------------------------------
  Rotate and translate a 3D volume.
-----------------------------------------------------------------------------*/

#undef CLIPIT

void THD_rota_vol( int   nx   , int   ny   , int   nz   ,
                   float xdel , float ydel , float zdel , float * vol ,
                   int ax1,float th1, int ax2,float th2, int ax3,float th3,
                   int dcode , float dx , float dy , float dz )
{
   MCW_3shear shr ;

#ifdef CLIPIT
   register float bot , top ;
   register int   nxyz=nx*ny*nz , ii ;
#endif

ENTRY("THD_rota_vol") ;

   if( nx < 2 || ny < 2 || nz < 2 || vol == NULL ) EXRETURN ;

   if( xdel == 0.0 ) xdel = 1.0 ;
   if( ydel == 0.0 ) ydel = 1.0 ;
   if( zdel == 0.0 ) zdel = 1.0 ;

   if( th1 == 0.0 && th2 == 0.0 && th3 == 0.0 ){  /* nudge rotation */
      th1 = 1.e-6 ; th2 = 1.1e-6 ; th3 = 0.9e-6 ;
   }

#if 0
fprintf(stderr,"THD_rota_vol:\n") ;
fprintf(stderr,"  th1=%g  th2=%g  th3=%g\n",th1,th2,th3) ;
fprintf(stderr,"  dx=%g  dy=%g  dz=%g\n",dx,dy,dz) ;
fprintf(stderr,"  xdel=%g  ydel=%g  zdel=%g\n",xdel,ydel,zdel) ;
#endif

   shr = rot_to_shear( ax1,-th1 , ax2,-th2 , ax3,-th3 ,
                       dcode,dx,dy,dz , xdel,ydel,zdel ) ;

   if( ! ISVALID_3SHEAR(shr) ){
      fprintf(stderr,"*** THD_rota_vol: can't compute shear transformation!\n") ;
      EXRETURN ;
   }

#if 0
   if( MRILIB_verbose )
     DUMP_3SHEAR("Computed shear",shr) ;
#endif

#ifdef CLIPIT
   bot = top = vol[0] ;
   for( ii=1 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) bot = vol[ii] ;
      else if( vol[ii] > top ) top = vol[ii] ;
   }
   if( bot >= top ) EXRETURN ;
#endif

   /********************************/
   /* 02 Feb 2001: include padding */

   { float * vvv , *www ;
     int nxp , nyp , nzp ;

     THD_rota_envpad() ;  /* 05 Feb 2001 */

     nxp=nx+2*rotpx ; nyp=ny+2*rotpy ; nzp=nz+2*rotpz ;

     if( rotpx > 0 && rotpy > 0 && rotpz > 0 )
        vvv = EDIT_volpad_even( rotpx,rotpy,rotpz , nx,ny,nz , MRI_float,vol ) ;
     else
        vvv = vol ;

     apply_3shear( shr , nxp,nyp,nzp , vvv ) ;  /*-- do the actual rotation! --*/

     if( vvv != vol ){
        www = EDIT_volpad_even( -rotpx,-rotpy,-rotpz , nxp,nyp,nzp , MRI_float,vvv ) ;
        free(vvv) ;
        memcpy( vol , www , sizeof(float)*nx*ny*nz ) ; free(www) ;
     }
   }

   /********************************/

#ifdef CLIPIT
   for( ii=0 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) vol[ii] = bot ;
      else if( vol[ii] > top ) vol[ii] = top ;
   }
#endif

   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Like the above, but with geometrical information about the volume
   given from the image header
-----------------------------------------------------------------------------*/

MRI_IMAGE * THD_rota3D( MRI_IMAGE * im ,
                        int ax1,float th1, int ax2,float th2, int ax3,float th3,
                        int dcode , float dx , float dy , float dz )
{
   MRI_IMAGE * jm ;
   float * jvol ;

   if( ! MRI_IS_3D(im) ){
      fprintf(stderr,"\n*** THD_rota3D: non-3D image input!\n") ;
      return NULL ;
   }

   jm = mri_new_vol( im->nx , im->ny , im->nz , MRI_float ) ;
   MRI_COPY_AUX(jm,im) ;
   jvol = MRI_FLOAT_PTR(jm) ;

   EDIT_coerce_type( im->nvox ,
                     im->kind , mri_data_pointer(im) , MRI_float , jvol ) ;

   THD_rota_vol(      im->nx ,       im->ny ,       im->nz  ,
                 fabs(im->dx) , fabs(im->dy) , fabs(im->dz) , jvol ,
                 ax1,th1 , ax2,th2 , ax3,th3 , dcode , dx,dy,dz ) ;

   return jm ;
}

/****************************************************************************
  Alternative entries, with rotation specified via a 3x3 matrix
  and shift as a 3-vector -- RWCox - 16 July 2000
*****************************************************************************/

/*---------------------------------------------------------------------------
  Rotate and translate a 3D volume
-----------------------------------------------------------------------------*/

#undef CLIPIT

void THD_rota_vol_matvec( int   nx   , int   ny   , int   nz   ,
                          float xdel , float ydel , float zdel , float * vol ,
                          THD_dmat33 rmat , THD_dfvec3 tvec )
{
   MCW_3shear shr ;
   int dcode ;

#ifdef CLIPIT
   register float bot , top ;
   register int   nxyz=nx*ny*nz , ii ;
#endif

   if( nx < 2 || ny < 2 || nz < 2 || vol == NULL ) return ;

   if( xdel == 0.0 ) xdel = 1.0 ;
   if( ydel == 0.0 ) ydel = 1.0 ;
   if( zdel == 0.0 ) zdel = 1.0 ;

   shr = rot_to_shear_matvec( rmat , tvec , xdel,ydel,zdel ) ;

   if( ! ISVALID_3SHEAR(shr) ){
      fprintf(stderr,"*** THD_rota_vol: can't compute shear transformation!\n") ;
      return ;
   }

#if 0
   DUMP_3SHEAR("Computed shear",shr) ;
   DUMP_MAT33("rmat",rmat) ;
   DUMP_FVEC3("tvec",tvec) ;
   fprintf(stderr,"---- xdel = %f, ydel = %f, zdel = %f\n",
           xdel, ydel, zdel);
#endif

#ifdef CLIPIT
   bot = top = vol[0] ;
   for( ii=1 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) bot = vol[ii] ;
      else if( vol[ii] > top ) top = vol[ii] ;
   }
   if( bot >= top ) return ;
#endif

   /********************************/
   /* 02 Feb 2001: include padding */

   { float * vvv , *www ;
     int nxp , nyp , nzp ;

     THD_rota_envpad() ;  /* 05 Feb 2001 */

     nxp=nx+2*rotpx ; nyp=ny+2*rotpy ; nzp=nz+2*rotpz ;

     if( rotpx > 0 && rotpy > 0 && rotpz > 0 )
        vvv = EDIT_volpad_even( rotpx,rotpy,rotpz , nx,ny,nz , MRI_float,vol ) ;
     else
        vvv = vol ;

     apply_3shear( shr , nxp,nyp,nzp , vvv ) ;  /*-- do the actual rotation! --*/

     if( vvv != vol ){
        www = EDIT_volpad_even( -rotpx,-rotpy,-rotpz , nxp,nyp,nzp , MRI_float,vvv ) ;
        free(vvv) ;
        memcpy( vol , www , sizeof(float)*nx*ny*nz ) ; free(www) ;
     }
   }

   /********************************/

#ifdef CLIPIT
   for( ii=0 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) vol[ii] = bot ;
      else if( vol[ii] > top ) vol[ii] = top ;
   }
#endif

   return ;
}

/*------------------------------------------------------------------------------
   14 Feb 2001:
   Like the above, but with geometrical information about the volume
   given from the image header
--------------------------------------------------------------------------------*/

MRI_IMAGE * THD_rota3D_matvec( MRI_IMAGE * im, THD_dmat33 rmat,THD_dfvec3 tvec )
{
   MRI_IMAGE * jm ;
   float * jvol ;

   if( ! MRI_IS_3D(im) ){
      fprintf(stderr,"\n*** THD_rota3D_matvec: non-3D image input!\n") ;
      return NULL ;
   }

   jm = mri_new_vol( im->nx , im->ny , im->nz , MRI_float ) ;
   MRI_COPY_AUX(jm,im) ;
   jvol = MRI_FLOAT_PTR(jm) ;

   EDIT_coerce_type( im->nvox ,
                     im->kind , mri_data_pointer(im) , MRI_float , jvol ) ;

   THD_rota_vol_matvec(      im->nx ,       im->ny ,       im->nz  ,
                       fabs(im->dx) , fabs(im->dy) , fabs(im->dz) , jvol ,
                       rmat , tvec ) ;
   return jm ;
}
