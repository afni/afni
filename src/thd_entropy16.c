/*--------------------------------------------------------------------*/
/*----- 02 Mar 2001 - RWCox - Used for AUTOGZIP decision -------------*/

#include "mrilib.h"

#undef SNUM
#define SNUM 65536  /* one for every possible 16 bit pattern */

static int * scount=NULL ;  /* holds count of unsigned shorts */
static int   snum=0 ;       /* total number of unsigned shorts processed */

/*-----------------------------------------------------------------------*/

void ENTROPY_setup(void)
{
   if( scount == NULL ) scount = (int *) malloc(sizeof(int)*SNUM) ;
   memset( scount , 0 , sizeof(int)*SNUM ) ;
   snum = 0 ;
}

/*-----------------------------------------------------------------------*/

void ENTROPY_setdown(void)
{
   if( scount != NULL ){ free(scount); scount = NULL; snum = 0; }
}

/*-----------------------------------------------------------------------*/

void ENTROPY_accumulate( int nbytes , void * var )
{
   int nn = nbytes/2 , ii ;
   unsigned short * sar = (unsigned short *) var ;

   if( scount == NULL ) ENTROPY_setup() ;

   for( ii=0 ; ii < nn ; ii++ ) scount[sar[ii]]++ ;
   snum += nn ;
}

/*-----------------------------------------------------------------------
  Value returned is in bits per 16 bit unsigned short
-------------------------------------------------------------------------*/

double ENTROPY_compute(void)
{
   register int ii ;
   register double sum ;

   if( scount == NULL || snum == 0 ) return 0.0 ;

   sum = 0.0 ;
   for( ii=0 ; ii < SNUM ; ii++ )
      if( scount[ii] > 0 ) sum += scount[ii] * log((double)scount[ii]) ;

   sum = -(sum - snum*log((double)snum)) / ( log(2.0) * snum ) ;
   return sum ;
}

/*-----------------------------------------------------------------------*/

double ENTROPY_dataset( THD_3dim_dataset * dset )
{
   if( !ISVALID_DSET(dset) ) return(0.0) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) return(0.0) ;
   return ENTROPY_datablock( dset->dblk ) ;
}

/*-----------------------------------------------------------------------*/

double ENTROPY_datablock( THD_datablock * blk )
{
   int iv ;
   double sum ;

ENTRY("ENTROPY_datablock") ;

   ENTROPY_setup() ;

   for( iv=0 ; iv < blk->nvals ; iv++ )
      ENTROPY_accumulate( DBLK_BRICK_BYTES(blk,iv) , DBLK_ARRAY(blk,iv) ) ;

   sum = ENTROPY_compute() ;
   ENTROPY_setdown() ;
   RETURN(sum) ;
}
