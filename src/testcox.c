#include "cox_render.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;
   char *cc1="x",*cc2="y",*cc3="z" ;
   float th1=0.0, th2=0.0, th3=0.0 ;
   float thx,thy,thz ;
   int   axx,ayy,azz ;
   char *fname="testcox.ppm" , fn[128] ;
   void * rhand ;
   int bot=1 , ii , nim=0 ;
   float omap[128] , bfac ;
   MRI_IMAGE * im , * brim ;
   int hbr[256] , nperc,ibot,itop,sum ;
   byte * bar ;
   double ctim ;
   int imode=CREN_TWOSTEP ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: testcox [-rotate a b c] [-out f] [-bot b] [-nn|-ts|-li] dset\n") ;
      exit(0) ;
   }

   enable_mcw_malloc() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-nn") == 0 ){
         imode = CREN_NN ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ts") == 0 ){
         imode = CREN_TWOSTEP ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-li") == 0 ){
         imode = CREN_LINEAR ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-bot") == 0 ){
         bot = strtod( argv[++iarg] , NULL ) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-rotate") == 0 ){
         th1 = (PI/180.0) * strtod( argv[++iarg] , &cc1 ) ;
         th2 = (PI/180.0) * strtod( argv[++iarg] , &cc2 ) ;
         th3 = (PI/180.0) * strtod( argv[++iarg] , &cc3 ) ;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-out") == 0 ){
         fname = argv[++iarg] ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"Illegal option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg >= argc ){fprintf(stderr,"No dataset?\n"); exit(1); }

   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){fprintf(stderr,"Can't open dataset!\n");exit(1);}
   if( DSET_BRICK_TYPE(dset,0) != MRI_byte ){
      fprintf(stderr,"Non-byte dataset input!\n");exit(1);
   }
   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"Can't load dataset!\n");exit(1);
   }

   rhand = new_CREN_renderer() ;

#if 0
   THD_rotangle_user_to_dset( dset ,
                              th1,*cc1  , th2,*cc2  , th3,*cc3 ,
                              &thx,&axx , &thy,&ayy , &thz,&azz ) ;
   CREN_set_viewpoint( rhand , axx,thx,ayy,thy,azz,thz ) ;
#else
   CREN_set_angles( rhand , th1,th2,th3 ) ;
#endif

   for( ii=0 ; ii < 128 ; ii++ )
      omap[ii] = (ii <= bot) ? 0.0
                             : (ii-bot)/(127.0-bot) ;

   CREN_set_opamap( rhand , omap , 1.0 ) ;

   brim = DSET_BRICK(dset,0) ; bar = MRI_BYTE_PTR(brim) ;
   mri_histobyte( brim , hbr ) ;
   nperc = 0.02 * brim->nvox ;
   for( sum=0,ibot=0   ; ibot < 128  && sum < nperc ; ibot++ ) sum += hbr[ibot] ;
   for( sum=0,itop=255 ; itop > ibot && sum < nperc ; itop-- ) sum += hbr[itop] ;
   if( ibot >= itop ){ ibot = 64 ; itop = 192 ; }
   bfac = 127.5 / (itop-ibot) ;
   for( ii=0 ; ii < brim->nvox ; ii++ )
           if( bar[ii] <= ibot ) bar[ii] = 0 ;
      else if( bar[ii] >= itop ) bar[ii] = 127 ;
      else                       bar[ii] = bfac * (bar[ii]-ibot) ;


   ctim = COX_cpu_time() ;

   CREN_set_databytes( rhand , brim->nx,brim->ny,brim->nz , bar ) ;
   CREN_dset_axes( rhand , dset ) ;

#if 0
   CREN_set_render_mode( rhand , CREN_MIP_OPA ) ;
#endif

   CREN_set_interp( rhand , imode ) ;

   for( th3=0 ; th3 < 360.0 ; th3+=5.0 ){
      CREN_set_angles( rhand , th1,th2,(PI/180.0)*th3 ) ;
      im = CREN_render( rhand, NULL ) ;  /* added NULL   2002.08.28 - rickr */
      if( im == NULL ){
        fprintf(stderr,"renderer fails!\n") ; exit(1) ;
      }

      sprintf(fn,"tc%03d.ppm",(int)rint(th3)) ;
      mri_write_pnm( fn, im ) ;
      fprintf(stderr,"+++ Output to file %s\n",fn);
      mri_free(im) ; nim++ ;
   }
   ctim = COX_cpu_time() - ctim ;
   fprintf(stderr,"+++ Rendering CPU time = %g s = %g/im\n",ctim,ctim/nim) ;

   exit(0) ;
}
