#include "niml_private.h"

/*************************************************************************/
/************************ Byte ordering functions ************************/
/*************************************************************************/

/*---------------------------------------------------------------*/
/*! Find the byte order on this system.
    Return is either NI_LSB_FIRST or NI_MSB_FIRST.
    We are assuming that there are only 2 possible order, which
    is of course false.
-----------------------------------------------------------------*/

int NI_byteorder(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = 0 ;

   return (fred.ss == 1) ? NI_LSB_FIRST : NI_MSB_FIRST ;
}

/*---------------------------------------------------------------*/
/*! Struct defined for use in NI_swap2(). */

typedef struct { unsigned char a,b ; } twobytes ;

/*---------------------------------------------------------------*/
/*! Swap arrays of 2 bytes (shorts).
-----------------------------------------------------------------*/

void NI_swap2( int n , void *ar )
{
   register int ii ;
   register twobytes *tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].b ; tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Struct defined for use in NI_swap4(). */

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

/*---------------------------------------------------------------*/
/*! Swap arrays of 4 bytes (ints or floats).
-----------------------------------------------------------------*/

void NI_swap4( int n , void *ar )
{
   register int ii ;
   register fourbytes *tb = (fourbytes *) ar ;
   register unsigned char tt , uu ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].d ; tb[ii].d = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].c ; tb[ii].c = uu ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Struct defined for use in NI_swap8(). */

typedef struct { unsigned char a,b,c,d , e,f,g,h ; } eightbytes ;

/*---------------------------------------------------------------*/
/*! Swap arrays of 8 bytes (doubles or 64 bit ints).
-----------------------------------------------------------------*/

void NI_swap8( int n , void *ar )
{
   register int ii ;
   register eightbytes *tb = (eightbytes *) ar ;
   register unsigned char tt , uu , vv , ww ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].h ; tb[ii].h = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].g ; tb[ii].g = uu ;
      vv = tb[ii].c ; tb[ii].c = tb[ii].f ; tb[ii].f = vv ;
      ww = tb[ii].d ; tb[ii].d = tb[ii].e ; tb[ii].e = ww ;
   }
   return ;
}
