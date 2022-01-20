#include "mrilib.h"
#include "extrema.h"
#include <signal.h>
#include <setjmp.h>

/** Intended to be #include-d into another source file **/

/*-------------------------------------------------------------------------*/
/* This stuff is to catch and ignore SEGV errors that can happen
   in Extract_Gradient_Maxima_3D -- that is, the function just ends
   there -- no harm, no foul, no edge enhancement.
*//*-----------------------------------------------------------------------*/

typedef void (*sig_t) (int);
sig_t   old_handler ;          /* to restore the old signal handler */
jmp_buf return_to_edgeize ;    /* how to jump out of signal handler below */

void intedge_sig( int sss )    /* called for SIGSEGV = seg fault */
{
   ERROR_message("Extract_Gradient_Maxima_3D had a seg fault SIGSEGV :(" ) ;
   longjmp(return_to_edgeize,666) ;  /* jump back to mri_interior_edgeize */
}

/*-------------------------------------------------------------------------*/
/* Enhance the interior edges of a 3D volume (brain image).
   Positive return value = number of input voxels altered.
   Negative return value = error code
*//*-----------------------------------------------------------------------*/

int mri_interior_edgeize( MRI_IMAGE *inim , int erode , float frac )
{
   MRI_IMAGE *outim ;
   float *inar , *outar , inmax , outmax , sfac ;
   byte *mask ;
   int indims[3] , border[3] , qq,pp , nx,ny,nz , nxy,nxyz , nadd,nmm ;
   float filterCoefs[3] = {1.0, 1.0, 1.0} ;
   recursiveFilterType filterType = ALPHA_DERICHE ;
   int xm,xp,ym,yp,zm,zp , nxi,nyi,nzi , nxyi,nxyzi ;
   int ic,jc,kc , ii,jj,kk , cropped=0 ;;
   MRI_IMAGE *cim=NULL ; float *car ;

ENTRY("mri_interior_edgeize") ;

   if( inim == NULL || inim->kind != MRI_float ) RETURN( -1 ) ;
   inar = MRI_FLOAT_PTR(inim) ;
   if( inar == NULL )                            RETURN( -101 );

   /* size of input image */

   nxi = inim->nx ; if( nxi < 32 )               RETURN( -2 ) ;
   nyi = inim->ny ; if( nyi < 32 )               RETURN( -3 ) ;
   nzi = inim->nz ; if( nzi < 32 )               RETURN( -4 ) ;
   nxyi = nxi*nyi ; nxyzi = nxyi*nzi ;

   /*-- Find autobox and make a cropped image [30 Mar 2021] --*/

   MRI_autobbox( inim , &xm,&xp , &ym,&yp, &zm,&zp ) ;
   if( xm > 1 || ym > 1 || zm > 1 || xp < nxi-2 || yp < nyi-2 || zp <= nzi-2 ){
     int xc = nxi - (xm + ((nxi-1)-xp)) ;
     int yc = nyi - (ym + ((nyi-1)-yp)) ;
     int zc = nzi - (zm + ((nzi-1)-zp)) ;
     int nnn = xc*yc*zc ;
     if( nxyzi > (int)(1.5f*nnn) ){
       cim = mri_zeropad_3D( -xm , xp-(nxi-1) ,
                             -ym , yp-(nyi-1) ,
                             -zm , zp-(nzi-1) , inim ) ;
       ININFO_message("   mri_interior_edgeize: cropping %d..%d %d..%d %d..%d",xm,xp,ym,yp,zm,zp) ;
     }
   }
   if( cim == NULL ){
     cim = inim ;  /* if crop did nothing */
   } else {
     cropped = 1 ; /* flag that cropping happened */
   }

   car = MRI_FLOAT_PTR(cim) ;
   if( car == NULL ){
     if( cropped ) mri_free(cim) ;               RETURN( -19 );
   }

   /* size of cropped image */

   nx = cim->nx ; if( nx < 32 )                  RETURN( -21 );
   ny = cim->ny ; if( ny < 32 )                  RETURN( -31 );
   nz = cim->nz ; if( nz < 32 )                  RETURN( -41 );
   nxy = nx*ny ; nxyz = nxy*nz ;

   if( cropped ){
     ININFO_message("   mri_interior_edgeize: input=%dx%dx%d cropped=%dx%dx%d",nxi,nyi,nzi,nx,ny,nz) ;
   }

        if( erode <  0    ) erode = 4 ;
        if( frac  <= 0.0f ) frac  = 0.1f ;
   else if( frac  >  1.0f ) frac  = 1.0f ;

   /* setup for 3D edge processing code below */

   indims[0] = nx ;
   indims[1] = ny ;
   indims[2] = nz ;
   border[0] = nx / 5 ; if( border[0] < 10 ) border[0] = 10 ;
   border[1] = ny / 5 ; if( border[1] < 10 ) border[1] = 10 ;
   border[2] = nz / 5 ; if( border[2] < 10 ) border[2] = 10 ;

   /* largest input intensity */

   inmax = mri_maxabs(cim) ;
     ININFO_message("   mri_interior_edgeize: maximum of input image = %g",inmax) ;
   if( inmax == 0.0f ){
     if( cropped ) mri_free(cim) ;               RETURN( -51 ) ;
   }

   /* output from 3D edge processing code below */

   outim = mri_new_conforming( cim , MRI_float ) ;
   if( outim == NULL ){
     if( cropped ) mri_free(cim) ;               RETURN( -70 );
   }
   outar = MRI_FLOAT_PTR(outim) ;
   if( outar == NULL ){
     if( cropped ) mri_free(cim) ;               RETURN( -72 );
   }

   /* "edge" detection */

STATUS("call Extract_Gradient_Maxima_3D") ;

     /* Step 0: Set up signal handler to be called if there's a seg fault;
                save the old signal handler so it can be restored afterwards;
                Note that the default signal handler in AFNI or otherwise
                causes the program to die die die, which I'm trying to avoid */

   old_handler = signal( SIGSEGV , intedge_sig ) ;

     /* Step 1: setjmp() sets up to return back here, and returns 0
                when it is called all innocently here */

   if( setjmp(return_to_edgeize) == 0 ){
     qq = Extract_Gradient_Maxima_3D( (void *)car  , FLOAT ,
                                      (void *)outar, FLOAT ,
                                      indims , border ,
                                      filterCoefs , filterType ) ;
   } else {
     /* Step 2: If setjmp() returns a nonzero value, so control gets
                to here, setjmp() was actually just being longjmp()
                leaping back here, which happens if the signal handler was
                invoked -- if instead we just normally returned from the
                signal handler, Extract_Gradient_Maxima_3D would take control
                back and do the seg fault over again, and an infinite loop
                would take over the galaxy, which is bad for everybody concerned */

     ININFO_message("   mri_interior_edgeize: longjmp from signal handler :(") ;
     signal( SIGSEGV , old_handler ) ;  /* restore old signal handler */
     if( cropped ) mri_free(cim) ;
     mri_free(outim) ;
     RETURN(-666) ;
   }

   /* don't need cropped image any more */

   if( cropped ){ mri_free(cim) ; cim = NULL ; }

   /* something bad happened above :( */

   if( qq == 0 ){ mri_free(outim);               RETURN( -16 ) ; }

   /* largest edge intensity */

   outmax = mri_maxabs(outim) ;
     ININFO_message("   mri_interior_edgeize: maximum of output image = %g",outmax) ;
   if( outmax == 0.0f ){ mri_free(outim);        RETURN( -17 ) ; }

   /* make eroded mask (to keep only interior edges) */

   mask = mri_automask_image(inim) ;
   if( mask == NULL ){ mri_free(outim);          RETURN( -18 ) ; }

   nmm = 1 ;
   qq  = rint(0.032*nx) ; nmm = MAX(nmm,qq) ;
   qq  = rint(0.032*ny) ; nmm = MAX(nmm,qq) ;
   qq  = rint(0.032*nz) ; nmm = MAX(nmm,qq) ;

   for( qq=0 ; qq < erode ; qq++ ){
     THD_mask_erode            ( nx,ny,nz , mask, 0, 2 ) ;
     THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
   }

   /* throw away non-automask edge voxels */

   outmax = 0.0f ;
   for( qq=0 ; qq < nxyz ; qq++ ){
          if( !mask[qq]          ) outar[qq] = 0.0f ;
     else if( outar[qq] > outmax ) outmax = outar[qq] ;
   }
   free(mask) ;

     ININFO_message("   mri_interior_edgeize: maximum of eroded output image = %g",outmax) ;
   if( outmax == 0.0f ){ mri_free(outim);        RETURN( -99 ) ; }

   /* add scaled edge values to input image */

   sfac = frac * inmax / outmax ;  /* how much to add */

   for( nadd=qq=0 ; qq < nxyz ; qq++ ){
     if( outar[qq] > 0.0f ){ /* something to add? */
       if( ! cropped ){      /* direct from outar to inar */
         inar[ii] += sfac*outar[qq] ;
       } else {              /* adjust indexes for cropping */
         ic = qq % nx; kc = qq / nxy; jc = (qq-kc*nxy) / nx; /* 3D in outar */
         ii = ic + xm; jj = jc + ym ; kk = kc + zm;          /* 3D in inar */
         pp = ii + jj*nxi + kk*nxyi ;                        /* 1D in inar */
         inar[pp] += sfac*outar[qq] ;
       }
       nadd++ ;
     }
   }

   /* I'm done */

   free(outim) ; RETURN( nadd  ) ;
}
