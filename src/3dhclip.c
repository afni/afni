/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------------------*/

#define NPMAX 128

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset * dset ;
   float bper = 60.0 , bmin = 1 ;

   int nxyz , ii , kk , nbin , sval , sum , nbot , a,b,c , nbase,npeak,ntop , nvox ;
   int * fbin ;
   short * sfim ;
   int   kmin[NPMAX] , kmax[NPMAX] ;
   int   nmin        , nmax        ;

   /*-- help? --*/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      fprintf(stderr,
             "Find the clipping point on a histogram of a 3D brick\n"
             "Usage: 3dhclip [options] dataset\n"
             "\n"
             "Options:\n"
             "  -bper bb   Means to take the base percentage point\n"
             "              on the cumulative histogram as 'bb'\n"
             "              [default bb = 60]\n"
             "  -bmin cc   Means to take the minimum value to consider\n"
             "              as 'cc' [default cc = 1]\n"
         ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      exit(0) ;
   }

   /*-- process options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-bper") == 0 ){
         bper = strtod( argv[++iarg] , NULL ) ;
         if(bper<1 || bper>99){fprintf(stderr,"** Illegal -bper value\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-bmin") == 0 ){
         bmin = strtod( argv[++iarg] , NULL ) ;
         if(bmin<0){fprintf(stderr,"** Illegal -bmin value\n");exit(1);}
         iarg++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- load dataset --*/

   if( iarg >=  argc ){fprintf(stderr,"** No dataset?!\n");exit(1);}

   dset = THD_open_dataset(argv[iarg]) ;
   if( dset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]) ;
      exit(1) ;
   }

   if( DSET_BRICK_TYPE(dset,0) != MRI_short || DSET_BRICK_FACTOR(dset,0) != 0.0 ){
     fprintf(stderr,"** Program only works with short-value datasets!\n") ;
     exit(1) ;
   }

   DSET_load(dset) ;
   sfim = DSET_ARRAY(dset,0) ;
   if( sfim == NULL ){fprintf(stderr,"** Can't load dataset brick\n");exit(1);}

   /*-- make histogram of shorts --*/


   fbin = (int *) malloc( sizeof(int) * 32768 ) ;
   for( kk=0 ; kk < 32768 ; kk++ ) fbin[kk] = 0 ;

   nvox = 0 ; nxyz = DSET_NVOX(dset) ;

   for( ii=0 ; ii < nxyz ; ii++ ){
      kk = sfim[ii] ; if( kk >= 0 ){ fbin[kk]++ ; nvox++ ; }
   }

   DSET_unload(dset) ;

   /*-- find largest value --*/

   for( kk=32767 ; kk > 0 ; kk-- ) if( fbin[kk] > 0 ) break ;
   if( kk == 0 ){fprintf(stderr,"** All voxels are zero!\n");exit(1);}
   nbin = kk+1 ;

   /*-- find bper point in cumulative distribution --*/

   sval = 0.01 * bper * nvox ;
   sum  = 0 ;
   for( kk=0 ; kk < nbin ; kk++ ){
      sum += fbin[kk] ; if( sum >= sval ) break ;
   }
   nbot = kk ; if( nbot == 0 ) nbot = 1 ; if( bmin > nbot ) nbot = bmin ;
   if( nbot >= nbin-9 ){fprintf(stderr,"** Base point on histogram too high\n");exit(1);}

   /*-- smooth histogram --*/

   b = fbin[nbot-1] ; c = fbin[nbot] ;
   for( kk=nbot ; kk < nbin ; kk++ ){
      a = b ; b = c ; c = fbin[kk+1] ; fbin[kk] = 0.25*(a+c+2*b) ;
   }

   /*-- find minima and maxima above bper point --*/

   nmin = nmax = 0 ;
   for( kk=nbot+1 ; kk < nbin ; kk++ ){
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] && nmin < NPMAX ){
         kmin[nmin++] = kk ;
      } else if( fbin[kk] > fbin[kk-1] && fbin[kk] > fbin[kk+1] && nmax < NPMAX ){
         kmax[nmax++] = kk ;
      }
   }

#if 0
   for( kk=0 ; kk < nmin ; kk++ )
      printf("Min: %d has %d\n",kmin[kk],fbin[kmin[kk]]) ;

   for( kk=0 ; kk < nmax ; kk++ )
      printf("Max: %d has %d\n",kmax[kk],fbin[kmax[kk]]) ;
#endif

   /*-- find the largest two maxima --*/

   if( nmax == 0 ){fprintf(stderr,"** No maxima above base point\n");exit(1);}

   if( nmax <= 2 ){
      npeak = kmax[0] ; ntop = 0 ;
   } else {
      int f1,f2 , k1,k2 , fk , klow,kup ;

      k1 = 0 ; f1 = fbin[kmax[0]] ;
      k2 = 1 ; f2 = fbin[kmax[1]] ;
      if( f1 < f2 ){
         k1 = 1 ; f1 = fbin[kmax[1]] ;
         k2 = 0 ; f2 = fbin[kmax[0]] ;
      }

      for( kk=2 ; kk < nmax ; kk++ ){
         fk = fbin[kmax[kk]] ;
         if( fk > f1 ){
            f2 = f1 ; k2 = k1 ;
            f1 = fk ; k1 = kk ;
         } else if( fk > f2 ){
            f2 = fk ; k2 = kk ;
         }
      }
      npeak = MIN( kmax[k1] , kmax[k2] ) ;  /* smaller bin of the 2 top peaks */

      /* find valley between 2 peaks */

      ntop  = MAX( kmax[k1] , kmax[k2] ) ;

      fk = fbin[ntop] ; klow = ntop ;
      for( kk=ntop-1 ; kk >= npeak ; kk-- ){
         if( fbin[kk] < fk ){ fk = fbin[kk] ; klow = kk ; }
      }
      fk = 0.16 * fk ; kup = MIN( nbin-1 , ntop+3*(ntop-klow+2) ) ;
      for( kk=ntop+1 ; kk <= kup ; kk++ ) if( fbin[kk] < fk ) break ;
      ntop = kk ;
   }

   for( kk=npeak-1 ; kk > 0 ; kk-- )
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] ) break ;
   nbase = kk ;

   printf("dataset: %s -- ",argv[iarg]) ;
   printf("base = %d  peak = %d  top = %d\n",nbase,npeak,ntop) ;

   exit(0) ;
}
