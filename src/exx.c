#include "mrilib.h"

#define ASSIGN_DIRECTIONS                                       \
 do{ switch( fixdir ){                                          \
      default:                                                  \
      case 1:            /* x-direction: (a,b,c) = (y,z,x) */   \
         astep = nx ; bstep = nxy ; cstep = 1  ;                \
         na    = ny ; nb    = nz  ; nc    = nx ;                \
      break ;                                                   \
                                                                \
      case 2:            /* y-direction: (a,b,c) = (z,x,y) */   \
         astep = nxy ; bstep = 1  ; cstep = nx ;                \
         na    = nz  ; nb    = nx ; nc    = ny ;                \
      break ;                                                   \
                                                                \
      case 3:            /* z-direction: (a,b,c) = (x,y,z) */   \
         astep = 1  ; bstep = nx ; cstep = nxy ;                \
         na    = nx ; nb    = ny ; nc    = nz  ;                \
      break ;                                                   \
    } } while(0)

/*-----------------------------------------------------------------------
   Simple get/put of a fixed plane (no shifting, zero padding).
-------------------------------------------------------------------------*/

void getplane_byte( int nx , int ny , int nz , byte * vol ,
                    int fixdir , int fixijk , byte * im )
{
   int bb , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   ijkoff = fixijk*cstep ;

   for( bb=0,boff=0 ; bb < nb ; bb++,boff+=na,ijkoff+=bstep )
      for( aa=0,aoff=0 ; aa < na ; aa++,aoff+=astep )
         im[aa+boff] = vol[aoff+ijkoff] ;

   return ;
}

void putplane_byte( int nx , int ny , int nz , byte * vol ,
                    int fixdir , int fixijk , byte * im )
{
   int bb , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   ijkoff = fixijk*cstep ;

   for( bb=0,boff=0 ; bb < nb ; bb++,boff+=na,ijkoff+=bstep )
      for( aa=0,aoff=0 ; aa < na ; aa++,aoff+=astep )
         vol[aoff+ijkoff] = im[aa+boff] ;

   return ;
}

void swaptest( int nrep , int fixdir )
{
   double cputim ;
   int pp , nx=100,ny=100,nz=100,nxy=nx*ny , kk ;
   byte * vin , * vout  ;
   int astep,bstep,cstep , na,nb,nc ;

   if( nrep <= 0 ) nrep = 1 ;

   ASSIGN_DIRECTIONS ;

   /* setup bricks */

   vin = (byte *) malloc( sizeof(byte) * (nx*ny*nz) ) ;
   if( vin == NULL ) return ;

   vout = (byte *) malloc( sizeof(byte) * (na*nb*nc) ) ;
   if( vout == NULL ){ free(vin) ; return ; }

   for( kk=0 ; kk < nx*ny*nz ; kk++ ) vin[kk] = (byte) kk ;

   cputim = COX_cpu_time() ;

   for( pp=0 ; pp < nrep ; pp++ ){
#if 0
      for( kk=0 ; kk < nc ; kk++ ){
         getplane_byte( nx,ny,nz , vin , fixdir , kk , vout + kk*na*nb ) ;
      }
#else
      for( kk=0 ; kk < nz ; kk++ ){
         putplane_byte( nx,ny,nz , vout , fixdir , kk , vin + kk*nx*ny ) ;
      }
#endif
   }
   cputim = COX_cpu_time() - cputim ;
   printf("fixdir = %d CPU time = %g (%g/rep)\n",fixdir,cputim,cputim/nrep) ;
   return ;
}

int main( int argc , char * argv[] )
{
   int nrep ;

   if( argc < 2 ){printf("Usage: exx nrep\n");exit(0);}

   nrep = strtol(argv[1],NULL,10) ;
   swaptest( nrep , 1 ) ;
   swaptest( nrep , 2 ) ;
   swaptest( nrep , 3 ) ;
   exit(0) ;
}
