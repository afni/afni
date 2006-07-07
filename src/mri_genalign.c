#include "mrilib.h"

/*---------------------------------------------------------------------------*/
static int gcd( int m , int n )
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n;
  }
  return n;
}
static int find_relprime_fixed( int n )
{
   int dj , n5=n/5 ;

   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}
static int find_relprime_random( int n )
{
   int dj , n5=n/5 , n2=n/2 ;

   if( n5 < 2 ) return 1 ;
   do{ dj = n5 + lrand48()%n2 ; } while( gcd(n,dj) > 1 ) ;
   return dj ;
}
/*---------------------------------------------------------------------------*/

static GA_setup *stup = NULL ;

#undef  ERREX
#define ERREX(s) \
 do{ ERROR_message("mri_genalign_scalar: %s",(s)); RETURN(NULL); } while(0)

MRI_IMAGE * mri_genalign_scalar( MRI_IMAGE *basim  ,
                                 MRI_IMAGE *maskim ,
                                 MRI_IMAGE *targim ,
                                 GA_params *parm    )
{
   int nspad ;

ENTRY("mri_genalign_scalar") ;

   /*-- basic checks of input for rationality --*/

   if( basim  == NULL ) ERREX("basim is NULL") ;
   if( targim == NULL ) ERREX("targim is NULL") ;

   nspad = MRI_DIMENSIONALITY(basim) ;
   if( nspad < 2 || nspad > 3 )
     ERREX("basim dimensionality is not 2 or 3") ;
   if( nspad != MRI_DIMENSIONALITY(targim) )
     ERREX("basim & targim dimensionalities differ") ;
   if( maskim != NULL && maskim->nvox != basim->nvox )
     ERREX("basim and maskim grids differ") ;

   FREE_GA_setup(stup) ;

   stup->bsim   = mri_to_float(basim ) ;
   stup->ajim   = mri_to_float(targim) ;
   stup->maskim = mri_to_byte (maskim) ;
