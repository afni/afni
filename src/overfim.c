/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "overfim.h"

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this routine creates a blank overlay ***/

short * RWC_create_overlay( nxover , nyover )
   int nxover , nyover ;
{
   short * ov ;
   register int ii , npix = nxover * nyover ;

   ov = (short *) malloc( sizeof(short) * npix ) ;

   for( ii=0 ; ii < npix ; ii++ ) ov[ii] = RWC_OVFLAG ;
   return ov ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this routine modifies an array of shorts by
     overlaying values that are NOT the flag value ***/

int RWC_short_overlay( nxim,nyim,image , nxover,nyover ,
                       flag,dont_overlay,checker , overlay )
  short * image ;
  int     nxim,nyim , nxover,nyover , checker ;
  short   flag , dont_overlay ;
  short * overlay ;
{
   int rowsize = nxim ;
   static int old_rowsize = -1 ; /* remember last entry */

   int nxdup , nydup ;           /* duplication factors: overlay -> image */
   static int old_nxdup = -1 ,
              old_nydup = -1 ;   /* remember last time in */

   static int old_checker = -1 ;

   int ir , jc , xx,yy ;        /* various loop indices */
   register int jj ;

   static int * jump = NULL ;     /* jump table, for duplicating overlays */
   static int   jump_size = -1 ;  /* allocated space for table */
   static int   jump_count ;      /* number of actual entries in table */

   short * ovp = overlay ;        /* for scanning through overlay array */

   register short * imbase ;      /* base of points in image to be overlaid */
   int     xfac , yfac ;   /* scale factors for mapping overlay to image */

#ifdef OV_DEBUG1
   int nov = 0 ;
#endif

/*** check if input image satisfies restrictions ***/

#ifdef OV_DEBUG1
   if( jump_size == -1 )
     fprintf(stderr,"RWC: overlay called nxim=%d nxover=%d\n",nxim,nxover) ;
#endif

   if( overlay == NULL || image  == NULL ||
       nxover  == 0    || nyover == 0      ) return 1 ;

/*** overlay and image dimensions must conform,
  in the sense that the image must be an exact multiple of the overlay ***/

   if( (nxim % nxover) !=0  ||  (nyim % nyover) != 0 ) return 2 ;

   nxdup = nxim / nxover ;  /* duplication factors for each overlay pixel */
   nydup = nyim / nyover ;

#ifdef OV_DEBUG1
   if( nxdup != old_nxdup || nydup != old_nydup )
     fprintf(stderr,"RWC: overlay nxdup=%d nydup=%d\n",nxdup,nydup) ;
#endif

/*** create the jump table, if not already present from last call ***/

   if( nxdup * nydup > jump_size ){
      if( jump != NULL ) free(jump) ;
      jump_size = nxdup * nydup ;
      jump      = (int *) malloc( sizeof(int) * jump_size ) ;
      if( jump == NULL ){
         jump_size = -1 ;
         return 3 ;
      }
   }

   if( rowsize != old_rowsize ||
       nxdup   != old_nxdup   || nydup != old_nydup ||
       checker != old_checker                         ){

      jj = 0 ;
      for( yy=0 ; yy < nydup ; yy++ ){
         for( xx=0 ; xx < nxdup ; xx++ ){
            if( !checker || (xx+yy)%2 == 0 )    /* checkerboard pattern */
               jump[jj++] = ( xx + yy * rowsize ) ;
         }
      }
      jump_count  = jj ;
      old_rowsize = rowsize ;  /* remember these for next time */
      old_nxdup   = nxdup ;
      old_nydup   = nydup ;
      old_checker = checker ;

#ifdef OV_DEBUG1
      fprintf(stderr,"RWC_short_overlay: new jump array:\n") ;
      for( jj=0 ; jj < jump_size ; jj++ ) fprintf(stderr,"%d ",jump[jj]) ;
      fprintf(stderr,"\ncomputed from nxdup=%d nydup=%d rowsize=%d \n",
              nxdup,nydup,rowsize ) ;
#endif
   }

/*** scale factors for mapping overlay to image ***/

   xfac = nxdup ;           /* overlay(x,y) --> image(xfac*x,yfac*y) */
   yfac = rowsize * nydup ;

   for( jc=0 ; jc < nyover ; jc++ ){    /* loop over all overlay pixels */
      for( ir=0 ; ir < nxover ; ir++,ovp++ ){

         if( *ovp != flag ){            /* overlay unless hit flag */

            imbase = &image[ir * xfac + jc * yfac] ; /* start overlay here */
            for( jj=0 ; jj < jump_count ; jj++ ){
               if( imbase[jump[jj]] != dont_overlay ){

                 imbase[jump[jj]] = *ovp ;
#ifdef OV_DEBUG1
                 nov++ ;
#endif
               }   /* end if */
            }  /* end for jj */
         }  /* end if */
      }  /* end for ir */
   }  /* end for jc */

#ifdef OV_DEBUG1
   fprintf(stderr,"RWC: number points overlaid = %d\n",nov);
#endif
   return 0 ;   /* nonzero return codes are an error */
}
