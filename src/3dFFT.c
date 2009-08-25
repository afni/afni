
#include "mrilib.h"

#define FFT_ABS     1
#define FFT_PHASE   2
#define FFT_COMPLEX 3

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_in , *dset_out ;
   int Lxx=-1 , Lyy=-1 , Lzz=-1 , Mode=FFT_ABS , Sign=-1 ;
   char *prefix = "FFT" ;
   int iarg ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dFFT [options] dataset\n"
       "Does the FFT of the input dataset in 3 directions (x,y,z) and\n"
       "produces the output dataset.\n"
       "\n"
       "Options\n"
       "=======\n"
       " -abs       = Outputs the magnitude of the FFT [default]\n"
       " -phase     = Outputs the phase of the FFT (-PI..PI)\n"
       " -complex   = Outputs the complex FFT\n"
       " -inverse   = Does the inverse FFT\n"
       " -Lx xx     = Use FFT of length 'xx' in the x-direction\n"
       " -Ly yy     = Use FFT of length 'yy' in the y-direction\n"
       " -Lz zz     = Use FFT of length 'zz' in the z-direction\n"
       "              * Set a length to 0 to skip the FFT in that direction\n"
       " -prefix pp = Use 'pp' for the output dataset prefix.\n"
       "\n"
       "Notes\n"
       "=====\n"
       " * The program can only do FFT lengths that are factorable\n"
       "    into a product of powers of 2, 3, and 5.\n"
       " * For -abs and -phase, the output dataset is in float format.\n"
       " * Forward FFT = sum_k[ exp(-2*PI*i*k/N) * data(k) ]\n"
       " * Inverse FFT = sum_k[ exp(+2*PI*i*k/N) * data(k) ] / N\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dFFT main") ; machdep() ; AUTHOR("RW Cox") ;

   /*--- scan args ---*/

   iarg = 1 ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-inverse") == 0 ){
       Sign = +1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-abs") == 0 ){
       Mode = FFT_ABS ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-phase") == 0 ){
       Mode = FFT_PHASE ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-complex") == 0 ){
       Mode = FFT_COMPLEX ; iarg++ ; continue ;
     }

     if( strlen(argv[iarg]) == 3 && strncmp(argv[iarg],"-L",2) == 0 ){
       int lll=-1 ; char *ept ;
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("need an argument after option %s",argv[iarg-1]) ;

       lll = strtol( argv[iarg] , &ept , 10 ) ;
       if( *ept != '\0' ){
         ERROR_exit("bad argument after option %s",argv[iarg-1]) ;
         exit(1) ;
       }
       if( lll < 0 ) ERROR_exit("negative argument after option %s",argv[iarg-1]) ;
       if( lll > 0 && csfft_nextup(lll) != lll )
         ERROR_exit("argument after '%s' is not a legal FFT length in this program!",argv[iarg-1]) ;
       switch( argv[iarg-1][2] ){
         case 'x': Lxx = lll ; break ;
         case 'y': Lyy = lll ; break ;
         case 'z': Lzz = lll ; break ;
         default:  ERROR_exit("unknown option '%s'",argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("need an argument after %s\n",argv[iarg-1]) ;
       prefix = strdup( argv[iarg] ) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("bad argument after %s\n",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     ERROR_exit("unknown option '%s'\n",argv[iarg]) ;
   }

   if( iarg >= argc ) ERROR_exit("no input dataset on command line?!\n") ;

   if( Lxx == 0 && Lyy == 0 && Lzz == 0 )
     ERROR_exit("-Lx, -Ly, -Lz all given as zero?!\n") ;

   /* open input dataset */

   dset_in = THD_open_dataset(argv[iarg]); CHECK_OPEN_ERROR(dset_in,argv[iarg]);

   nx = DSET_NX(dset_in) ; ny = DSET_NY(dset_in) ; nz = DSET_NZ(dset_in) ;

   if( Lxx < 0 ) Lxx = csfft_nextup(nxx) ;
   if( Lyy < 0 ) Lxx = csfft_nextup(nyy) ;
   if( Lzz < 0 ) Lxx = csfft_nextup(nzz) ;
   INFO_message("x-axis length = %d ;  FFT length = %d",nx,Lxx) ;
   INFO_message("y-axis length = %d ;  FFT length = %d",ny,Lyy) ;
   INFO_message("z-axis length = %d ;  FFT length = %d",nz,Lzz) ;
   if( Lxx > 0 && Lxx < nx ) ERROR_exit("x-axis length mismatch!") ;
   if( Lyy > 0 && Lyy < ny ) ERROR_exit("y-axis length mismatch!") ;
   if( Lzz > 0 && Lzz < nz ) ERROR_exit("z-axis length mismatch!") ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_fft_3D( int Sign, MRI_IMAGE *inim , int Lxx,int Lyy,int Lzz )
{
   MRI_IMAGE *iim , *oim ;
   int ii,jj,kk , nx,ny,nxy,nz , nbig , ioff,joff,koff , fx,fy,fz,fxy ;
   complex *cbig , *car , *far ;

   if( inim->kind != MRI_complex ) return NULL ;
   car = MRI_COMPLEX_PTR(inim) ;

   nx = inim->nx ; ny = inim->ny ; nz = inim->nz ; nxy = nx*ny ;
   nbig = MAX(nx,ny) ; nbig = MAX(nbig,nz) ;
   nbig = MAX(nbig,Lxx) ; nbig = MAX(nbig,Lyy) ; nbig = MAX(nbig,Lzz) ;
   nbig *= 3 ; cbig = (complex *)malloc(sizeof(complex)*nbig) ;

   fx = (Lxx == 0) ? nx : (Lxx >= nx) ? Lxx : csfft_nextup(nx) ;
   fy = (Lyy == 0) ? ny : (Lyy >= ny) ? Lyy : csfft_nextup(ny) ;
   fz = (Lzz == 0) ? nz : (Lzz >= nz) ? Lzz : csfft_nextup(nz) ;
   fxy = fx*fy ;

   oim = mri_new_volume( fx,fy,fz , MRI_complex ) ;
   far = MRI_COMPLEX_PTR(oim) ;

   /* x-direction FFTs */

   if( fx > 1 ){
     for( kk=0 ; kk < nz ; kk++ ){
       koff_i = kk*nxy ; koff_o = kk*fxy ;
       for( jj=0 ; jj < nz ; jj++ ){
         joff_i = koff_i + jj*nx ; joff_o = koff_o + jj*fx ;
         for( ii=0 ; ii < nx ; ii++ ) cbig[ii] = car[ii+joff_i] ;
         for(      ; ii < fx ; ii++ ) cbig[ii].r = cbig[ii].i = 0.0f ;
         csfft_cox( Sign , fx , cbig ) ;
         for( ii=0 ; ii < fx ; ii++ ) car[ii+joff_o] = cbig[ii] ;
       }
     }
   }

}
