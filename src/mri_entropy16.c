#include "mrilib.h"

/*-----------------------------------------------------------------------*/

#undef  SNUM
#define SNUM 65536  /* one for every possible 16 bit pattern */

double mri_entropy16( MRI_IMAGE *im )
{
   register int *scount , snum , ii ;
   register unsigned short *sar ;
   register double sum ;

ENTRY("mri_entropy16") ;

   if( im == NULL ) RETURN(0.0l) ;

   sar = (unsigned short *) mri_data_pointer( im ) ;
   if( sar == NULL ) RETURN(0.0l) ;

   snum   = (im->nvox * im->pixel_size) / 2 ; if( snum < 2 ) RETURN(0.0l) ;
   scount = (int *) calloc( sizeof(int),SNUM ) ;

   for( ii=0 ; ii < snum ; ii++ ) scount[sar[ii]]++ ;

   sum = 0.0 ;
   for( ii=0 ; ii < SNUM ; ii++ )
     if( scount[ii] > 0 ) sum += scount[ii] * log((double)scount[ii]) ;

   free( (void *)scount ) ;

   sum = -(sum - snum*log((double)snum)) / ( log(2.0l) * snum ) ;
   RETURN(sum) ;
}

/*-----------------------------------------------------------------------*/

#undef  BNUM
#define BNUM 256  /* one for every possible 8 bit pattern */

double mri_entropy8( MRI_IMAGE *im )
{
   register int *bcount , bnum , ii ;
   register byte *bar ;
   register double sum ;

ENTRY("mri_entropy8") ;

   if( im == NULL ) RETURN(0.0l) ;

   bar = (byte *) mri_data_pointer( im ) ;
   if( bar == NULL ) RETURN(0.0l) ;

   bnum   = (im->nvox * im->pixel_size) ; if( bnum < 2 ) RETURN(0.0l) ;
   bcount = (int *) calloc( sizeof(int),BNUM ) ;

   for( ii=0 ; ii < bnum ; ii++ ) bcount[bar[ii]]++ ;

   sum = 0.0 ;
   for( ii=0 ; ii < BNUM ; ii++ )
     if( bcount[ii] > 0 ) sum += bcount[ii] * log((double)bcount[ii]) ;

   free( (void *)bcount ) ;

   sum = -(sum - bnum*log((double)bnum)) / ( log(2.0l) * bnum ) ;
   RETURN(sum) ;
}
