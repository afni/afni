#include "mrilib.h"

#define NBIN 32768
static int histo[NBIN] ;

void init_histo(void)
{
   int ii ;
   for( ii=0 ; ii < NBIN ; ii++ ) histo[ii] = 0 ;
   return ;
}

int load_histo( THD_3dim_dataset * dset , int iv )
{
   int ii , nvox ;
   MRI_IMAGE * bim ;
   short * bar ;

   if( !ISVALID_DSET(dset) )                   return 0 ;
   if( iv < 0 || iv >= DSET_NVALS(dset) )      return 0 ;
   if( DSET_BRICK_TYPE(dset,iv) != MRI_short ) return 0 ;

   DSET_load(dset) ; bar = DSET_ARRAY(dset,iv) ;
   if( bar == NULL )                           return 0 ;

   nvox = DSET_NVOX(dset) ;
   for( ii=0 ; ii < nvox ; ii++ )
      if( bar[ii] < 0 )                        return 0 ;

   for( ii=0 ; ii < nvox ; ii++ ) histo[bar[ii]]++ ;
   return nvox ;
}

double chfit( double mu )
{
   int ntop = (2.5 *  mu + 0.5) ;
   double ftop , ntot , ccc , eee ;
   int ii ;

   if( ntop <= 1 || ntop >= NBIN/2 ) return -1.0 ;

   ntot = 0 ;
   for( ii=0 ; ii <= ntop ; ii++ ) ntot += histo[ii] ;

   ftop = 1.0 - exp(-(ntop/mu)*(ntop/mu)) ;
   ntot *= ftop ;

   ccc = 0.0 ;
   for( ii=0 ; ii <= ntop ; ii++ ){
      eee = ntot * ( exp(-(ii/mu)*(ii/mu)) -
                     exp(-((ii+1)/mu)*((ii+1)/mu)) ) ;

      if( eee < 1.0 ) eee = 1.0 ;

      ccc += (histo[ii] - eee) * (histo[ii] - eee) / eee ;
   }

   ccc /= (ntop+1) ; return ccc ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   double mu , ccc , mbest,cbest , perc , snr=2.5 ;
   int ii , narg=1 , blast=0 , iv , ncut , nnn , nvox ;
   int vmax ;
   short * bar ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dnoise [-blast] [-snr fac] datasets ...\n"
             "Estimates noise level in 3D datasets, and optionally\n"
             "set voxels below the noise threshold to zero.\n"
             "This only works on datasets that are stored as shorts,\n"
             "and whose elements are all nonnegative.\n"
             "  -blast   = Set values at or below the cutoff to zero.\n"
             "  -snr fac = Set cutoff to 'fac' times the estimated\n"
             "               noise level.  Default fac = 2.5.\n"
            ) ;
      exit(0) ;
   }

   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-blast") == 0 ){
         blast = 1 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-snr") == 0 ){
         narg++ ;
         snr = strtod( argv[narg] , NULL ) ;
         if( snr <= 0.0 ){fprintf(stderr,"Illegal snr value!\n");exit(1);}
         narg++ ; continue ;
      }

      fprintf(stderr,"Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }
   if( narg >= argc ){fprintf(stderr,"No datasets?\n");exit(1);}

   for( ; narg < argc ; narg++ ){
      dset = THD_open_one_dataset(argv[narg]) ;
      if( dset == NULL ){
         printf("*** Can't open %s\n",argv[narg]) ;
         continue ;
      }

      printf("%s",argv[narg]) ; fflush(stdout) ;

      init_histo() ;
      if( blast )
         THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      vmax = 0 ;
      nvox = DSET_NVOX(dset) ;
      for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
         ii = load_histo(dset,iv) ;
         if( ii <= 0 ){ printf(": Can't load data, or illegal data!\n"); break; }
         bar = DSET_ARRAY(dset,iv) ;
         for( ii=0 ; ii < nvox ; ii++ ) if( vmax < bar[ii] ) vmax = bar[ii] ;
      }
      if( iv < DSET_NVALS(dset) ) continue ;
      if( vmax < 40 ){ printf(": Didn't fit noise model!\n"); continue; }

      printf(":") ; fflush(stdout) ;

#define DMU 0.5
      mu = 1.0 ; mbest = mu ; cbest = chfit(mu) ; ii = 0 ;
      do {
         mu += DMU ; ccc = chfit(mu) ;
         if( ccc > cbest ) break ;
         cbest = ccc ; mbest = mu ; ii++ ;
         if( mu > 0.05 * vmax ){ ii=0 ; break; }
      } while( 1 ) ;

      if( ii <= 0 ){ printf(" Didn't fit noise model!\n"); continue; }

      ncut = (int) (snr * mbest) ;
      nnn  = 0 ;
      for( ii=0 ; ii <= ncut ; ii++ ) nnn += histo[ii] ;
      perc = (100.0*nnn) / (double)(DSET_NVOX(dset)*DSET_NVALS(dset)) ;
      printf(" Cutoff=%d  Count=%d [%4.1f%%]",ncut,nnn,perc) ; fflush(stdout) ;

      if( blast && nnn > 0 ){
         for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
            bar = DSET_ARRAY(dset,iv) ;
            for( ii=0 ; ii < nvox ; ii++ ) if( bar[ii] <= ncut ) bar[ii] = 0 ;
         }
         DSET_write(dset) ;
         printf("--blasted") ; fflush(stdout) ;
      }

      printf("\n") ;
      THD_delete_3dim_dataset( dset , False ) ;
   }

   exit(0) ;
}
