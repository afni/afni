
#include "mrilib.h"

#define FFT_ABS     1
#define FFT_PHASE   2
#define FFT_COMPLEX 3

static MRI_IMAGE * mri_fft_3D( int Sign, MRI_IMAGE *inim,
                               int Lxx,int Lyy,int Lzz, int alt ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_in=NULL , *dset_out ;
   int Lxx=-1 , Lyy=-1 , Lzz=-1 , Mode=FFT_ABS , Sign=-1 , do_alt=0 ;
   char *prefix = "FFTout" ;
   int iarg ;
   MRI_IMAGE *inim , *outim ; float fac ; int nx,ny,nz ;
   THD_ivec3 iv ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dFFT [options] dataset\n"
       "\n"
       "* Does the FFT of the input dataset in 3 directions (x,y,z) and\n"
       "   produces the output dataset.\n"
       "\n"
       "* Why you'd want to do this is an interesting question.\n"
       "\n"
       "* Program 3dcalc can operate on complex-valued datasets, but\n"
       "   only on one component at a time (cf. the '-cx2r' option).\n"
       "\n"
       "* Most other AFNI programs can only operate on real-valued\n"
       "   datasets.\n"
       "\n"
       "* You could use 3dcalc (twice) to split a complex-valued dataset\n"
       "   into two real-valued datasets, do your will on those with other\n"
       "   AFNI programs, then merge the results back into a complex-valued\n"
       "   dataset with 3dTwotoComplex.\n"
       "\n"
       "Options\n"
       "=======\n"
       " -abs       = Outputs the magnitude of the FFT [default]\n"
       " -phase     = Outputs the phase of the FFT (-PI..PI == no unwrapping!)\n"
       " -complex   = Outputs the complex-valued FFT\n"
       " -inverse   = Does the inverse FFT instead of the forward FFT\n"
       "\n"
       " -Lx xx     = Use FFT of length 'xx' in the x-direction\n"
       " -Ly yy     = Use FFT of length 'yy' in the y-direction\n"
       " -Lz zz     = Use FFT of length 'zz' in the z-direction\n"
       "              * Set a length to 0 to skip the FFT in that direction\n"
       "\n"
       " -altIN     = Alternate signs of input data before FFT, to bring\n"
       "               zero frequency from edge of FFT-space to center of grid\n"
       "               for cosmetic purposes.\n"
       " -altOUT    = Alternate signs of output data after FFT.  If you\n"
       "               use '-altI' on the forward transform, then you should\n"
       "               use '-altO' an the inverse transform, to get the\n"
       "               signs of the recovered image correct.\n"
       "      **N.B.: You cannot use '-altIN' and '-altOUT' in the same run!\n"
       "\n"
       " -input dd  = Read the input dataset from 'dd', instead of\n"
       "               from the last argument on the command line.\n"
       "\n"
       " -prefix pp = Use 'pp' for the output dataset prefix.\n"
       "\n"
       "Notes\n"
       "=====\n"
       " * In the present avatar, only 1 sub-brick will be processed.\n"
       "\n"
       " * The program can only do FFT lengths that are factorable\n"
       "    into a product of powers of 2, 3, and 5, and are even.\n"
       "   + The largest power of 3 that is allowed is 3^3 = 27.\n"
       "   + The largest power of 5 that is allowed is 5^3 = 125.\n"
       "   + e.g., FFT of length 3*5*8=120 is possible.\n"
       "   + e.g., FFT of length 4*31 =124 is not possible.\n"
       "\n"
       " * The 'x', 'y', and 'z' axes here refer to the order the\n"
       "    data is stored, not DICOM coordinates; cf. 3dinfo.\n"
       "\n"
       " * If you force (via '-Lx' etc.) an FFT length that is not\n"
       "    allowed, the program will stop with an error message.\n"
       "\n"
       " * If you force an FFT length that is shorter than an dataset\n"
       "    axis dimension, the program will stop with an error message.\n"
       "\n"
       " * If you don't force an FFT length along a particular axis,\n"
       "    the program will pick the smallest legal value that is\n"
       "    greater than or equal to the corresponding dataset dimension.\n"
       "   + e.g., 124 would be increased to 128.\n"
       "\n"
       " * If an FFT length is longer than an axis length, then the\n"
       "    input data in that direction is zero-padded at the end.\n"
       "\n"
       " * For -abs and -phase, the output dataset is in float format.\n"
       "\n"
       " * If you do the forward and inverse FFT, then you should get back\n"
       "    the original dataset, except for roundoff error and except that\n"
       "    the new dataset axis dimensions may be longer than the original.\n"
       "\n"
       " * Forward FFT = sum_{k=0..N-1} [ exp(-2*PI*i*k/N) * data(k) ]\n"
       "\n"
       " * Inverse FFT = sum_{k=0..N-1} [ exp(+2*PI*i*k/N) * data(k) ] / N\n"
       "\n"
       " * Started a long time ago, but only finished in Aug 2009 at the\n"
       "    request of John Butman, because he asked so nicely.  (Now pay up!)\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   PRINT_VERSION("3dFFT") ;
   mainENTRY("3dFFT main") ; machdep() ; AUTHOR("RW Cox") ;
   AFNI_logger("3dFFT",argc,argv) ;

   /*--- scan args ---*/

   iarg = 1 ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strncasecmp(argv[iarg],"-altI",5) == 0 ){
       do_alt = 1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-altOUT",5) == 0 ){
       do_alt = -1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-inverse",4) == 0 ){
       Sign = +1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-abs",4) == 0 ){
       Mode = FFT_ABS ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-phase",4) == 0 ){
       Mode = FFT_PHASE ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-complex",4) == 0 ){
       Mode = FFT_COMPLEX ; iarg++ ; continue ;
     }

     if( strlen(argv[iarg]) == 3 && strncmp(argv[iarg],"-L",2) == 0 ){
       int lll=-1 , mmm ; char *ept ;
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("need an argument after option %s",argv[iarg-1]) ;

       lll = strtol( argv[iarg] , &ept , 10 ) ;
       if( *ept != '\0' )
         ERROR_exit("bad argument after option %s",argv[iarg-1]) ;
       if( lll > 0 && (mmm = csfft_nextup_even(lll)) != lll )
         ERROR_exit(
          "'%s %d' is not a legal FFT length here: next largest legal value = %d" ,
          argv[iarg-1] , lll , mmm ) ;
       switch( argv[iarg-1][2] ){
         case 'x': case 'X': Lxx = lll ; break ;
         case 'y': case 'Y': Lyy = lll ; break ;
         case 'z': case 'Z': Lzz = lll ; break ;
         default:  ERROR_exit("unknown option '%s'",argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-prefix",4) == 0 ){
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("need an argument after %s\n",argv[iarg-1]) ;
       prefix = strdup( argv[iarg] ) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("bad argument after %s\n",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-input",4) == 0 ){
       iarg++ ;
       if( iarg >= argc )
         ERROR_exit("need an argument after %s\n",argv[iarg-1]) ;
       dset_in = THD_open_dataset(argv[iarg]); CHECK_OPEN_ERROR(dset_in,argv[iarg]);
       iarg++ ; continue ;
     }

     ERROR_exit("unknown option '%s'\n",argv[iarg]) ;
   }

   /* check for simple errors */

   if( Lxx == 0 && Lyy == 0 && Lzz == 0 )
     ERROR_exit("-Lx, -Ly, -Lz all given as zero?!") ;

   /* open input dataset */

   if( dset_in == NULL ){
     if( iarg >= argc ) ERROR_exit("no input dataset on command line?!\n") ;
     dset_in = THD_open_dataset(argv[iarg]); CHECK_OPEN_ERROR(dset_in,argv[iarg]);
   }

   nx = DSET_NX(dset_in) ; ny = DSET_NY(dset_in) ; nz = DSET_NZ(dset_in) ;

   /* establish actual FFT lengths now (0 ==> no FFT) */

   if( nx == 1 ) Lxx = 0 ;  /* can't FFT if dataset is shrimpy! */
   if( ny == 1 ) Lyy = 0 ;
   if( nz == 1 ) Lzz = 0 ;

   if( Lxx < 0 ) Lxx = csfft_nextup_even(nx) ;  /* get FFT length from */
   if( Lyy < 0 ) Lyy = csfft_nextup_even(ny) ;  /* dataset dimensions */
   if( Lzz < 0 ) Lzz = csfft_nextup_even(nz) ;

   INFO_message("x-axis length = %d ; FFT length = %d %s",nx,Lxx,(Lxx==0)?"==> none":"\0") ;
   INFO_message("y-axis length = %d ; FFT length = %d %s",ny,Lyy,(Lyy==0)?"==> none":"\0") ;
   INFO_message("z-axis length = %d ; FFT length = %d %s",nz,Lzz,(Lzz==0)?"==> none":"\0") ;

   if( Lxx > 0 && Lxx < nx ) ERROR_exit("x-axis FFT length too short!") ;
   if( Lyy > 0 && Lyy < ny ) ERROR_exit("y-axis FFT length too short!") ;
   if( Lzz > 0 && Lzz < nz ) ERROR_exit("z-axis FFT length too short!") ;

   /* extract sub-brick #0 */

   DSET_load(dset_in) ; CHECK_LOAD_ERROR(dset_in) ;

   inim = mri_to_complex( DSET_BRICK(dset_in,0) ) ;
   fac  = DSET_BRICK_FACTOR(dset_in,0) ;
   if( fac > 0.0f && fac != 1.0f ){
     int ii , nvox = nx*ny*nz ; complex *car = MRI_COMPLEX_PTR(inim) ;
     for( ii=0 ; ii < nvox ; ii++ ){ car[ii].r *= fac ; car[ii].i *= fac ; }
   }

   DSET_unload(dset_in) ;

   /* FFT to get output image */

   csfft_scale_inverse(1) ;  /* scale by 1/N for inverse FFTs */

   outim = mri_fft_3D( Sign , inim , Lxx,Lyy,Lzz , do_alt ) ;

   mri_free(inim) ;

   /* post-process output? */

   switch( Mode ){
     case FFT_ABS:{
       MRI_IMAGE *qim = mri_complex_abs(outim) ;
       mri_free(outim) ; outim = qim ;
     }
     break ;

     case FFT_PHASE:{
       MRI_IMAGE *qim = mri_complex_phase(outim) ;
       mri_free(outim) ; outim = qim ;
     }
     break ;
   }

   /* create output dataset */

   dset_out = EDIT_empty_copy( dset_in ) ;
   LOAD_IVEC3( iv , outim->nx , outim->ny , outim->nz ) ;
   EDIT_dset_items( dset_out ,
                      ADN_nxyz   , iv ,
                      ADN_prefix , prefix ,
                      ADN_nvals  , 1 ,
                      ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_BRICK_FACTOR( dset_out , 0 , 0.0 ) ;
   EDIT_substitute_brick( dset_out , 0 , outim->kind , mri_data_pointer(outim) ) ;
   DSET_write(dset_out) ; WROTE_DSET(dset_out) ; DSET_unload(dset_out) ;

   exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* FFT lengths are in Lxx, Lyy, Lzz; however,
     Lxx = 0 ==> no FFT in that direction (etc.).
*//*--------------------------------------------------------------------------*/

#undef  ALTERN
#define ALTERN(nn)                                                                        \
 do{ register int qq;                                                                     \
     for( qq=1; qq < (nn); qq+=2 ){ cbig[qq].r = -cbig[qq].r; cbig[qq].i = -cbig[qq].i; } \
 } while(0)

static MRI_IMAGE * mri_fft_3D( int Sign, MRI_IMAGE *inim, int Lxx,int Lyy,int Lzz, int alt )
{
   MRI_IMAGE *outim ;
   int ii,jj,kk , nx,ny,nxy,nz , nbig , fx,fy,fz,fxy , joff,koff ;
   complex *cbig , *car , *far ;

   if( inim->kind != MRI_complex ) return NULL ;

   /* input data and its dimensions */

   car = MRI_COMPLEX_PTR(inim) ;
   nx = inim->nx ; ny = inim->ny ; nz = inim->nz ; nxy = nx*ny ;

   /* output dimensions and data */

   fx = (Lxx == 0) ? nx : (Lxx > nx) ? csfft_nextup_even(Lxx) : csfft_nextup_even(nx) ;
   fy = (Lyy == 0) ? ny : (Lyy > ny) ? csfft_nextup_even(Lyy) : csfft_nextup_even(ny) ;
   fz = (Lzz == 0) ? nz : (Lzz > nz) ? csfft_nextup_even(Lzz) : csfft_nextup_even(nz) ;
   fxy = fx*fy ;

   outim = mri_new_vol( fx,fy,fz , MRI_complex ) ;  /* zero filled */
   far   = MRI_COMPLEX_PTR(outim) ;

   /* buffer space */

   nbig = MAX(fx,fy) ; nbig = MAX(nbig,fz) ; nbig = 4*nbig + 512 ;
   cbig = (complex *)malloc(sizeof(complex)*nbig) ;

   /* copy input data into output image */

   for( kk=0 ; kk < nz ; kk++ )
     for( jj=0 ; jj < ny ; jj++ )
       memcpy( far + jj*fx + kk*fxy, car + jj*nx + kk*nxy, sizeof(complex)*nx );

   /* x-direction FFTs */

   if( Lxx > 1 ){
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
       for( jj=0 ; jj < fy ; jj++ ){
         joff = koff + jj*fx ;
         for( ii=0 ; ii < fx ; ii++ ) cbig[ii] = far[ii+joff] ;
         if( alt > 0 ) ALTERN(fx) ;
         csfft_cox( Sign , fx , cbig ) ;
         if( alt < 0 ) ALTERN(fx) ;
         for( ii=0 ; ii < fx ; ii++ ) far[ii+joff] = cbig[ii] ;
       }
     }
   }

   /* y-direction FFTs */

   if( Lyy > 1 ){
     for( kk=0 ; kk < fz ; kk++ ){
       koff = kk*fxy ;
       for( ii=0 ; ii < fx ; ii++ ){
         joff = koff + ii ;
         for( jj=0 ; jj < fy ; jj++ ) cbig[jj] = far[jj*fx+joff] ; /* copy data */
         if( alt > 0 ) ALTERN(fy) ;
         csfft_cox( Sign , fy , cbig ) ;                       /* FFT in buffer */
         if( alt < 0 ) ALTERN(fy) ;
         for( jj=0 ; jj < fy ; jj++ ) far[jj*fx+joff] = cbig[jj] ; /* copy back */
       }
     }
   }

   /* z-direction FFTs */

   if( Lzz > 1 ){
     for( jj=0 ; jj < fy ; jj++ ){
       joff = jj*fx ;
       for( ii=0 ; ii < fx ; ii++ ){
         koff = joff + ii ;
         for( kk=0 ; kk < fz ; kk++ ) cbig[kk] = far[kk*fxy+koff] ;
         if( alt > 0 ) ALTERN(fz) ;
         csfft_cox( Sign , fz , cbig ) ;
         if( alt < 0 ) ALTERN(fz) ;
         for( kk=0 ; kk < fz ; kk++ ) far[kk*fxy+koff] = cbig[kk] ;
       }
     }
   }

   free(cbig) ; MRI_COPY_AUX(outim,inim) ; return outim ;
}
