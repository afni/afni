/*--------------------------------------------------------------------*/
/*----- 02 Mar 2001 - RWCox - Used for AUTOGZIP decision -------------*/

#include "mrilib.h"

#undef SNUM
#define SNUM 65536  /* one for every possible 16 bit pattern */

static int64_t *scount=NULL ;  /* holds count of unsigned shorts */
static int64_t  snum=0 ;       /* total number of unsigned shorts processed */

static int do_zskip  = 0 ;     /* skip zero? */
static int do_perbin = 0 ;
static int do_permax = 0 ;

/*-----------------------------------------------------------------------*/

void ENTROPY_setup(void)
{
   if( scount == NULL ) scount = (int64_t *) malloc(sizeof(int64_t)*SNUM) ;
   memset( scount , 0 , sizeof(int64_t)*SNUM ) ;
   snum = 0 ;
}

/*-----------------------------------------------------------------------*/

void ENTROPY_setdown(void)
{
   if( scount != NULL ){ free(scount); scount = NULL; snum = 0; }
}

/*-----------------------------------------------------------------------*/

void ENTROPY_accumulate( int64_t nbytes , void * var )
{
   int64_t nn = nbytes/2 , ii ;
   unsigned short * sar = (unsigned short *) var ;

   if( scount == NULL ) ENTROPY_setup() ;

   if( do_zskip )
     for( ii=0 ; ii < nn ; ii++ ){
       if( sar[ii] != 0 ){ scount[sar[ii]]++ ; snum++ ; }
   } else {
     for( ii=0 ; ii < nn ; ii++ ) scount[sar[ii]]++ ;
     snum += nn ;
   }
   return ;
}

/*-----------------------------------------------------------------------
  Value returned is in bits per 16 bit unsigned short
-------------------------------------------------------------------------*/

double ENTROPY_compute(void)
{
   int64_t ii ;
   double sum , bfac=1.0 ;

   if( scount == NULL || snum == 0 ) return 0.0 ;

   sum = 0.0 ;
   for( ii=0 ; ii < SNUM ; ii++ )
      if( scount[ii] > 0 ) sum += scount[ii] * log((double)scount[ii]) ;

   if( do_perbin ){
     int64_t nbin = 0 ;
     for( ii=0 ; ii < SNUM ; ii++ ){ if( scount[ii] > 0 ) nbin++ ; }
     if( nbin > 1 ) bfac = 1.0 / log((double)nbin) ;
   } else if( do_permax ){
     int64_t nmax = 0 ;
     for( ii=0 ; ii < SNUM ; ii++ ){ if( scount[ii] > 0 ) nmax = ii ; }
     if( nmax > 1 ) bfac = 1.0 / log((double)nmax) ;
   }

   sum = -bfac * (sum - snum*log((double)snum)) / ( log(2.0) * snum ) ;
   return sum ;
}

/*-----------------------------------------------------------------------*/

double ENTROPY_dataset( THD_3dim_dataset *dset )
{
   if( !ISVALID_DSET(dset) ) return(0.0) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) return(0.0) ;
   return ENTROPY_datablock( dset->dblk ) ;
}

/*-----------------------------------------------------------------------*/

double ENTROPY_datablock( THD_datablock *blk )
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
