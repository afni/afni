#include "mrilib.h"

/**** Byte ordering routines ***/

/*---------------------------------------------------------------*/

int mri_short_order(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = 0 ;

   return (fred.ss == 1) ? LSB_FIRST : MSB_FIRST ;
}

/*---------------------------------------------------------------*/

int mri_int_order(void)
{
   union { unsigned char bb[4] ;
           int           ii ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = fred.bb[2] = fred.bb[3] = 0 ;

   return (fred.ii == 1) ? LSB_FIRST : MSB_FIRST ;
}

/*---------------------------------------------------------------*/

typedef struct { unsigned char a,b ; } twobytes ;

void mri_swap2( int n , short * ar )
{
   register int ii ;
   register twobytes * tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].b ;
      tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

void mri_swap4( int n , int * ar )
{
   register int ii ;
   register fourbytes * tb = (fourbytes *) ar ;
   register unsigned char tt , uu ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].d ;
      tb[ii].d = tt ;

      uu       = tb[ii].b ;
      tb[ii].b = tb[ii].c ;
      tb[ii].c = uu ;
   }
   return ;
}
