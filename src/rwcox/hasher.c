#undef  UINT
#define UINT unsigned int

#if 1
# define SLOTS 8191    /* prime */
#else
# define SLOTS 27449   /* prime */
#endif

#include <stdio.h>

static UINT hasher( char *str )
{
   UINT hh , jj,kk ; char *cpt ;

   if( str    == NULL ) return 0 ;
   if( str[0] == '\0' ) return 1 ;

   hh = 0 ;
   for( cpt=str ; *cpt != '\0' ; cpt++ ){
      jj = (65537*(UINT)(*cpt)) ; cpt++ ;
      kk = 17389*(UINT)(*cpt) ;
      hh = 13*hh + (jj << 8) + kk ;
      if( *cpt == '\0' ) break ;
   }

   hh =  ((hh & 0xaaaaaaaa) >> 1)   /* swap bits */
       | ((hh & 0x55555555) << 1) ;

   return (hh % SLOTS) ;
}

int main( int argc , char *argv[] )
{
   int ii ; UINT hh ;
   for( ii=1 ; ii < argc ; ii++ )
     printf("%s -> %u\n",argv[ii],hasher(argv[ii])) ;
   exit(0);
}
