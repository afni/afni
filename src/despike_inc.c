#ifndef __DESKPIKE_INCLUDE__

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+8 >= num ) jj = num-9;     \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }

/*-------------------------------------------------------------------------*/
/*! Remove spikes from a time series, in a very simplistic way.
    Return value is the number of spikes that were squashed [RWCox].
*//*-----------------------------------------------------------------------*/

static int DES_despike9( int num , float *vec , float *wks )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return 0 ;

   if( wks != NULL ) zme = wks ;
   else              zme = (float *)malloc(sizeof(float)*(2*num)) ;
   zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ){ if( wks == NULL ) free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   if( wks == NULL ) free(zme) ;
   return nsp ;
}
#undef mead9

/*----------------------------------------------------------------------------*/
/* Similar code to the above, but for spans of length 25 instead of 9 */

static INLINE float median25f(float *p)
{
    register float temp ;
    SORT2(p[0], p[1]) ;   SORT2(p[3], p[4]) ;   SORT2(p[2], p[4]) ;
    SORT2(p[2], p[3]) ;   SORT2(p[6], p[7]) ;   SORT2(p[5], p[7]) ;
    SORT2(p[5], p[6]) ;   SORT2(p[9], p[10]) ;  SORT2(p[8], p[10]) ;
    SORT2(p[8], p[9]) ;   SORT2(p[12], p[13]) ; SORT2(p[11], p[13]) ;
    SORT2(p[11], p[12]) ; SORT2(p[15], p[16]) ; SORT2(p[14], p[16]) ;
    SORT2(p[14], p[15]) ; SORT2(p[18], p[19]) ; SORT2(p[17], p[19]) ;
    SORT2(p[17], p[18]) ; SORT2(p[21], p[22]) ; SORT2(p[20], p[22]) ;
    SORT2(p[20], p[21]) ; SORT2(p[23], p[24]) ; SORT2(p[2], p[5]) ;
    SORT2(p[3], p[6]) ;   SORT2(p[0], p[6]) ;   SORT2(p[0], p[3]) ;
    SORT2(p[4], p[7]) ;   SORT2(p[1], p[7]) ;   SORT2(p[1], p[4]) ;
    SORT2(p[11], p[14]) ; SORT2(p[8], p[14]) ;  SORT2(p[8], p[11]) ;
    SORT2(p[12], p[15]) ; SORT2(p[9], p[15]) ;  SORT2(p[9], p[12]) ;
    SORT2(p[13], p[16]) ; SORT2(p[10], p[16]) ; SORT2(p[10], p[13]) ;
    SORT2(p[20], p[23]) ; SORT2(p[17], p[23]) ; SORT2(p[17], p[20]) ;
    SORT2(p[21], p[24]) ; SORT2(p[18], p[24]) ; SORT2(p[18], p[21]) ;
    SORT2(p[19], p[22]) ; SORT2(p[8], p[17]) ;  SORT2(p[9], p[18]) ;
    SORT2(p[0], p[18]) ;  SORT2(p[0], p[9]) ;   SORT2(p[10], p[19]) ;
    SORT2(p[1], p[19]) ;  SORT2(p[1], p[10]) ;  SORT2(p[11], p[20]) ;
    SORT2(p[2], p[20]) ;  SORT2(p[2], p[11]) ;  SORT2(p[12], p[21]) ;
    SORT2(p[3], p[21]) ;  SORT2(p[3], p[12]) ;  SORT2(p[13], p[22]) ;
    SORT2(p[4], p[22]) ;  SORT2(p[4], p[13]) ;  SORT2(p[14], p[23]) ;
    SORT2(p[5], p[23]) ;  SORT2(p[5], p[14]) ;  SORT2(p[15], p[24]) ;
    SORT2(p[6], p[24]) ;  SORT2(p[6], p[15]) ;  SORT2(p[7], p[16]) ;
    SORT2(p[7], p[19]) ;  SORT2(p[13], p[21]) ; SORT2(p[15], p[23]) ;
    SORT2(p[7], p[13]) ;  SORT2(p[7], p[15]) ;  SORT2(p[1], p[9]) ;
    SORT2(p[3], p[11]) ;  SORT2(p[5], p[17]) ;  SORT2(p[11], p[17]) ;
    SORT2(p[9], p[17]) ;  SORT2(p[4], p[10]) ;  SORT2(p[6], p[12]) ;
    SORT2(p[7], p[14]) ;  SORT2(p[4], p[6]) ;   SORT2(p[4], p[7]) ;
    SORT2(p[12], p[14]) ; SORT2(p[10], p[14]) ; SORT2(p[6], p[7]) ;
    SORT2(p[10], p[12]) ; SORT2(p[6], p[10]) ;  SORT2(p[6], p[17]) ;
    SORT2(p[12], p[17]) ; SORT2(p[7], p[17]) ;  SORT2(p[7], p[10]) ;
    SORT2(p[12], p[18]) ; SORT2(p[7], p[12]) ;  SORT2(p[10], p[18]) ;
    SORT2(p[12], p[20]) ; SORT2(p[10], p[20]) ; SORT2(p[10], p[12]) ;
    return (p[12]);
}

/*--- get the local median and MAD of values vec[j-12 .. j+12] ---*/

#undef  mead25
#define mead25(j)                                              \
 { float qqq[25] ; int jj=(j)-12 ; register int pp;            \
   if( jj < 0 ) jj = 0; else if( jj+24 >= num ) jj = num-24;   \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = vec[jj+pp] ;         \
   med = median25f(qqq) ;                                      \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = fabsf(qqq[pp]-med) ; \
   mad = median25f(qqq); }

static int DES_despike25( int num , float *vec , float *wks )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( vec == NULL ) return 0 ;
   if( num <  25   ) return DES_despike9(num,vec,wks) ;

   if( wks != NULL ) zme = wks ;
   else              zme = (float *)malloc(sizeof(float)*(2*num)) ;
   zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead25(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ){ if( wks == NULL ) free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   if( wks == NULL ) free(zme) ;
   return nsp ;
}
#undef mead25
#undef SORT2
#undef SWAP

#endif
