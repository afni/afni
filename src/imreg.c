/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

/******* global data *******/

/** define results of scanning the command line **/

static char RG_out_prefix[256] = "reg." ;
static char RG_out_suffix[256] = "\0"   ;
static int  RG_out_start       = 1      ;
static int  RG_out_step        = 1      ;
static int  RG_out_mode        = -666   ;

static char RG_dout_prefix[256] = "\0" ;

static int  RG_meth            = ALIGN_DFSPACE_TYPE ;
static int  RG_methcode        = 0 ;
static int  RG_verbose         = 1 ;
static int  RG_skip_output     = 0 ;
static int  RG_debug           = 0 ;

static int  RG_almode_coarse   = MRI_BICUBIC ;
static int  RG_almode_fine     = MRI_BICUBIC ;
static int  RG_almode_reg      = MRI_BICUBIC ;

static int  RG_use_cmass       = 0 ; /* 12 Nov 2001 */

static MRI_IMAGE * RG_imwt     = NULL ;

static int     Iarg = 1 ;
static int     Argc ;
static char ** Argv ;

static MRI_IMAGE * RG_baseimage = NULL ;
static MRI_IMARR * RG_imseq     = NULL ;

/******* prototypes *******/

void REG_syntax(void) ;
void REG_command_line(void) ;

/******* the program! *****/

/*-- 07 Apr 1998: for testing the new mri_2dalign_* routines --*/

#undef USE_2DALIGN
#ifdef USE_2DALIGN
#  undef ALLOW_DFTIME
#endif

int main( int argc , char *argv[] )
{
   MRI_IMAGE * qim , * tim ;
   MRI_IMARR * regar = NULL ;
   float * dx , * dy , * phi ;
   int kim , imcount ;
   char fname[666] ;
   float dxtop , dytop , phitop ;
   FILE * dxfil , * dyfil , * phifil ;

   /** handle command line options **/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){ REG_syntax() ; exit(0); }

   machdep() ;

   Argc = argc ;
   Argv = argv ;
   Iarg = 1 ;

   REG_command_line() ;

   imcount = RG_imseq->num ;
   dx      = (float *) malloc( sizeof(float) * imcount ) ;
   dy      = (float *) malloc( sizeof(float) * imcount ) ;
   phi     = (float *) malloc( sizeof(float) * imcount ) ;
   if( dx == NULL || dy == NULL || phi == NULL ){
     fprintf(stderr,"** malloc failure for mri_rotate parameters!\a\n") ; exit(1) ;
   }

   /* 12 Nov 2001: shift input images to align centers of mass */

   if( RG_use_cmass ){
      float xbase,ybase , xcm,ycm ;
      int ii,jj,kk , di,dj , nx,ny , sj,si  ;
      MRI_IMAGE *fim , *nim ;
      float     *far , *nar ;

      if( RG_verbose ) printf("-- doing -cmass adjustments\n") ;

      mri_get_cmass_2D( RG_baseimage , &xbase , &ybase ) ;

      for( kk=0 ; kk < imcount ; kk++ ){
         fim = mri_to_float( IMARR_SUBIM(RG_imseq,kk) ) ; /* copy image */
         mri_get_cmass_2D( fim , &xcm , &ycm ) ;
         di = (int) rint(xbase-xcm) ;                 /* integer shifts */
         dj = (int) rint(ybase-ycm) ;
         if( di != 0 || dj != 0 ){                 /* shift input image */

#if 0
fprintf(stderr,"Image %d: xbase=%g ybase=%g xcm=%g ycm=%g di=%d dj=%d\n",
        kk,xbase,ybase,xcm,ycm,di,dj ) ;
#endif

            nim = mri_new_conforming( fim , MRI_float ); /* zero filled */
            far = MRI_FLOAT_PTR(fim) ;
            nar = MRI_FLOAT_PTR(nim) ;
            nx  = fim->nx ; ny = fim->ny ;
            for( jj=0 ; jj < ny ; jj++ ){          /* copy fim into nim */
               sj = jj + dj ;
               if( sj < 0 || sj >= ny ) continue ;
               for( ii=0 ; ii < nx ; ii++ ){
                  si = ii + di ;
                  if( si < 0 || si >= nx ) continue ;
                  nar[si+nx*sj] = far[ii+nx*jj] ;
               }
            }
            mri_free( IMARR_SUBIM(RG_imseq,kk) ) ; /* replace image */
            IMARR_SUBIM(RG_imseq,kk) = nim ;       /* in the array */
         }
         mri_free(fim) ;
      }
   } /* end of RG_use_cmass */

   /* do the actual iterative alignments */

   if( RG_verbose ) printf("-- beginning alignment\n") ;
   switch( RG_meth ){

      default:
      case ALIGN_DFSPACE_TYPE:
#ifdef USE_2DALIGN
         regar = mri_2dalign_many( RG_baseimage,RG_imwt, RG_imseq, dx,dy,phi ) ;
#else
         if( ! RG_skip_output ) RG_methcode |= ALIGN_REGISTER_CODE ;
         regar = mri_align_dfspace( RG_baseimage,RG_imwt, RG_imseq, RG_methcode, dx,dy,phi ) ;
#endif
      break ;

#ifdef ALLOW_DFTIME
      case ALIGN_DFTIME_TYPE:
         if( ! RG_skip_output ) RG_methcode |= ALIGN_REGISTER_CODE ;
         regar = mri_align_dftime( RG_baseimage,RG_imwt, RG_imseq, RG_methcode, dx,dy,phi ) ;
      break ;
#endif
   }

   dxtop = dytop = phitop = 0.0 ;

   if( strlen(RG_dout_prefix) > 0 ){
      sprintf( fname , "%s%s" , RG_dout_prefix , "dx"  ) ; dxfil  = fopen( fname , "w" ) ;
      sprintf( fname , "%s%s" , RG_dout_prefix , "dy"  ) ; dyfil  = fopen( fname , "w" ) ;
      sprintf( fname , "%s%s" , RG_dout_prefix , "phi" ) ; phifil = fopen( fname , "w" ) ;
   } else {
      dxfil = dyfil = phifil = NULL ;
   }

   for( kim=0 ; kim < imcount ; kim++ ){

      if( fabs(dx[kim])  > fabs(dxtop)  ) dxtop  = dx[kim] ;
      if( fabs(dy[kim])  > fabs(dytop)  ) dytop  = dy[kim] ;
      if( fabs(phi[kim]) > fabs(phitop) ) phitop = phi[kim] ;

      if( ! RG_skip_output ){
         sprintf( fname , "%s%04d%s" , RG_out_prefix ,
                  RG_out_start + kim*RG_out_step , RG_out_suffix ) ;

         if( regar == NULL ){
            tim = mri_rota_variable( RG_almode_reg ,
                                     RG_imseq->imarr[kim] ,
                                     dx[kim],dy[kim],phi[kim] ) ;
         } else {
            tim = regar->imarr[kim] ;
         }
      } else {
         sprintf( fname , "%4d" , kim+1 ) ;
      }

      if( RG_verbose )
         printf("-- image %s: dx = %6.3f  dy = %6.3f  phi = %6.3f\n" ,
                fname,dx[kim],dy[kim],(180.0/PI)*phi[kim] ) ;

      if( dxfil  != NULL ) fprintf( dxfil  , "%6.3f\n" , dx[kim] ) ;
      if( dyfil  != NULL ) fprintf( dyfil  , "%6.3f\n" , dy[kim] ) ;
      if( phifil != NULL ) fprintf( phifil , "%6.3f\n" , (180.0/PI)*phi[kim] ) ;

      if( ! RG_skip_output ){
         switch( RG_out_mode ){

            default:
               mri_write( fname , tim ) ;
            break ;

            case MRI_short:
               qim = mri_to_short( 1.0 , tim ) ;
               mri_write( fname , qim ) ; mri_free( qim ) ;
            break ;

            case MRI_byte:
               qim = mri_to_byte( tim ) ;
               mri_write( fname , qim ) ; mri_free( qim ) ;
            break ;
         }
         mri_free( tim ) ; mri_free( RG_imseq->imarr[kim] ) ;
      }
   }

   if( RG_verbose )
      printf("-- MAX:  %*s  dx = %6.3f  dy = %6.3f  phi = %6.3f\n" ,
             (int)strlen(fname) , " " , dxtop,dytop,phitop*(180.0/PI) ) ;

   exit(0) ;
}

/*---------------------------------------------------------------------*/

void REG_syntax(void)
{
   printf(
    "Usage: imreg [options] base_image image_sequence ...\n"
    " * Registers each 2D image in 'image_sequence' to 'base_image'.\n"
    " * If 'base_image' = '+AVER', will compute the base image as\n"
    "   the average of the images in 'image_sequence'.\n"
    " * If 'base_image' = '+count', will use the count-th image in the\n"
    "   sequence as the base image.  Here, count is 1,2,3, ....\n"
    "\n"
    "OUTPUT OPTIONS:\n"
    "  -nowrite        Don't write outputs, just print progress reports.\n"
    "  -prefix pname   The output files will be named in the format\n"
    "  -suffix sname   'pname.index.sname' where 'pname' and 'sname'\n"
    "  -start  si      are strings given by the first 2 options.\n"
    "  -step   ss      'index' is a number, given by 'si+(i-1)*ss'\n"
    "                  for the i-th output file, for i=1,2,...\n"
    "                *** Default pname = 'reg.'\n"
    "                *** Default sname = nothing at all\n"
    "                *** Default si    = 1\n"
    "                *** Default ss    = 1\n"
    "\n"
    "  -flim           Write output in mrilib floating point format\n"
    "                  (which can be converted to shorts using program ftosh).\n"
    "                *** Default is to write images in format of first\n"
    "                    input file in the image_sequence.\n"
    "\n"
    "  -quiet          Don't write progress report messages.\n"
    "  -debug          Write lots of debugging output!\n"
    "\n"
    "  -dprefix dname  Write files 'dname'.dx, 'dname'.dy, 'dname'.phi\n"
    "                    for use in time series analysis.\n"
    "\n"
    "ALIGNMENT ALGORITHMS:\n"
    "  -bilinear       Uses bilinear interpolation during the iterative\n"
    "                    adjustment procedure, rather than the default\n"
    "                    bicubic interpolation. NOT RECOMMENDED!\n"
    "  -modes c f r    Uses interpolation modes 'c', 'f', and 'r' during\n"
    "                    the coarse, fine, and registration phases of the\n"
    "                    algorithm, respectively.  The modes can be selected\n"
    "                    from 'bilinear', 'bicubic', and 'Fourier'.  The\n"
    "                    default is '-modes bicubic bicubic bicubic'.\n"
    "  -mlcF           Equivalent to '-modes bilinear bicubic Fourier'.\n"
    "\n"
    "  -wtim filename  Uses the image in 'filename' as a weighting factor\n"
    "                    for each voxel (the larger the value the more\n"
    "                    importance is given to that voxel).\n"
    "\n"
    "  -dfspace[:0]    Uses the 'iterated diffential spatial' method to\n"
    "                    align the images.  The optional :0 indicates to\n"
    "                    skip the iteration of the method, and to use the\n"
    "                    simpler linear differential spatial alignment method.\n"
    "                    ACCURACY: displacments of at most a few pixels.\n"
    "                *** This is the default method (without the :0).\n"
    "\n"

#ifdef ALLOW_DFTIME
    "  -dftime[:0]     Similar to the above, but after determining the\n"
    "                    displacements, it modifies the images by using a\n"
    "                    local fit in each voxel.  The optional :0 has the\n"
    "                    same meaning as for -dfspace.\n"
#if 0
    "                    The optional :d means to also remove the mean and\n"
    "                    linear trend from each pixel in the image time series.\n"
#endif
    "                    ACCURACY: unevaluated.  This option is intended\n"
    "                    for FMRI use only!\n"
    "\n"
    "  -dfspacetime[:0]  Apply both algorithms: dfspace, then dftime.\n"
    "\n"
#endif /* ALLOW_DFTIME */

    "  -cmass            Initialize the translation estimate by aligning\n"
    "                    the centers of mass of the images.\n"
    "              N.B.: The reported shifts from the registration algorithm\n"
    "                    do NOT include the shifts due to this initial step.\n"
    "\n"
    "The new two options are used to play with the -dfspace algorithm,\n"
    "which has a 'coarse' fit phase and a 'fine' fit phase:\n"
    "\n"
    "  -fine blur dxy dphi  Set the 3 'fine' fit parameters:\n"
    "                         blur = FWHM of image blur prior to registration,\n"
    "                                  in pixels [must be > 0];\n"
    "                         dxy  = convergence tolerance for translations,\n"
    "                                  in pixels;\n"
    "                         dphi = convergence tolerance for rotations,\n"
    "                                  in degrees.\n"
    "\n"
    "  -nofine              Turn off the 'fine' fit algorithm. By default, the\n"
    "                         algorithm is on, with parameters 1.0, 0.07, 0.21.\n"
   ) ;

   return ;
}

/*---------------------------------------------------------------------*/

#define BASE_INPUT -1
#define BASE_AVER  -2

void REG_command_line(void)
{
   int ii , nxbase , nybase , nerr , basecode ;
   MRI_IMAGE * tim ;
   MRI_IMARR * tarr ;

   /*** look for options ***/

   while( Iarg < Argc-2 && Argv[Iarg][0] == '-' ){

      /** -cmass [12 Nov 2001] **/

      if( strcmp(Argv[Iarg],"-cmass") == 0 ){
         RG_use_cmass = 1 ;
         Iarg++ ; continue ;
      }

      /** -nowrite **/

      if( strncmp(Argv[Iarg],"-nowrite",5) == 0 ){
         RG_skip_output = 1 ;
         Iarg++ ; continue ;
      }

      /** -debug **/

      if( strncmp(Argv[Iarg],"-debug",5) == 0 ){
         RG_debug = 1 ;
         Iarg++ ; continue ;
      }

      /** -quiet **/

      if( strncmp(Argv[Iarg],"-quiet",5) == 0 ){
         RG_verbose = 0 ;
         Iarg++ ; continue ;
      }

      /** -flim **/

      if( strncmp(Argv[Iarg],"-flim",5) == 0 ){
         RG_out_mode = MRI_float ;
         Iarg++ ; continue ;
      }

      /** -wtim **/

      if( strncmp(Argv[Iarg],"-wtim",5) == 0 ){
         RG_imwt = mri_read_just_one( Argv[++Iarg] ) ;
         Iarg++ ; continue ;
      }

      /** -prefix **/

      if( strncmp(Argv[Iarg],"-prefix",5) == 0 ){
         strcpy( RG_out_prefix , Argv[++Iarg] ) ;
         ii = strlen(RG_out_prefix) ;
         if( ii > 0 && RG_out_prefix[ii-1] != '.' ){
            RG_out_prefix[ii]   = '.'  ;
            RG_out_prefix[ii+1] = '\0' ;
         }
         Iarg++ ; continue ;
      }

      /** -dprefix **/

      if( strncmp(Argv[Iarg],"-dprefix",5) == 0 ){
         strcpy( RG_dout_prefix , Argv[++Iarg] ) ;
         ii = strlen(RG_dout_prefix) ;
         if( ii > 0 && RG_dout_prefix[ii-1] != '.' ){
            RG_dout_prefix[ii]   = '.'  ;
            RG_dout_prefix[ii+1] = '\0' ;
         }
         Iarg++ ; continue ;
      }

      /** -suffix **/

      if( strncmp(Argv[Iarg],"-suffix",5) == 0 ){
         Iarg++ ;
         if( Argv[Iarg][0] == '.' ){
            strcpy( RG_out_suffix , Argv[Iarg] ) ;
         } else {
            RG_out_suffix[0] = '.' ;
            strcpy( RG_out_suffix + 1 , Argv[Iarg] ) ;
         }
         Iarg++ ; continue ;
      }

      /** -start **/

      if( strncmp(Argv[Iarg],"-start",5) == 0 ){
         char * ch ;
         RG_out_start = strtol( Argv[++Iarg] , &ch , 10 ) ;
         if( *ch != '\0' ){
            fprintf(stderr,"** value after -start is illegal!\a\n") ;
            exit(1) ;
         }
         Iarg++ ; continue ;
      }

      /** -step **/

      if( strncmp(Argv[Iarg],"-step",5) == 0 ){
         char * ch ;
         RG_out_step = strtol( Argv[++Iarg] , &ch , 10 ) ;
         if( *ch != '\0' ){
            fprintf(stderr,"** value after -step is illegal!\a\n") ;
            exit(1) ;
         }
         Iarg++ ; continue ;
      }

      /** -bilinear **/

      if( strncmp(Argv[Iarg],"-bilinear",5) == 0 ){
         RG_almode_coarse = RG_almode_fine = RG_almode_reg = MRI_BILINEAR ;
         mri_align_method( RG_almode_coarse,RG_almode_fine,RG_almode_reg ) ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-Fourier",5) == 0 ){
         RG_almode_coarse = RG_almode_fine = RG_almode_reg = MRI_FOURIER ;
         mri_align_method( RG_almode_coarse,RG_almode_fine,RG_almode_reg ) ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-bicubic",5) == 0 ){
         RG_almode_coarse = RG_almode_fine = RG_almode_reg = MRI_BICUBIC ;
         mri_align_method( RG_almode_coarse,RG_almode_fine,RG_almode_reg ) ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-modes",5) == 0 ){  /* 1 Oct 1998 */
         char * cpt ;

         cpt = Argv[++Iarg] ;
         if( strlen(cpt) > 2 ){
            switch( cpt[2] ){
               case 'l': RG_almode_coarse = MRI_BILINEAR ; break ;
               case 'c': RG_almode_coarse = MRI_BICUBIC  ; break ;
               case 'u': RG_almode_coarse = MRI_FOURIER  ; break ;
            }
         }

         cpt = Argv[++Iarg] ;
         if( strlen(cpt) > 2 ){
            switch( cpt[2] ){
               case 'l': RG_almode_fine = MRI_BILINEAR ; break ;
               case 'c': RG_almode_fine = MRI_BICUBIC  ; break ;
               case 'u': RG_almode_fine = MRI_FOURIER  ; break ;
            }
         }

         cpt = Argv[++Iarg] ;
         if( strlen(cpt) > 2 ){
            switch( cpt[2] ){
               case 'l': RG_almode_reg = MRI_BILINEAR ; break ;
               case 'c': RG_almode_reg = MRI_BICUBIC  ; break ;
               case 'u': RG_almode_reg = MRI_FOURIER  ; break ;
            }
         }

         mri_align_method( RG_almode_coarse,RG_almode_fine,RG_almode_reg ) ;
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-mlcF",5) == 0 ){  /* 1 Oct 1998 */
         RG_almode_coarse = MRI_BILINEAR ;
         RG_almode_fine   = MRI_BICUBIC  ;
         RG_almode_reg    = MRI_FOURIER  ;
         mri_align_method( RG_almode_coarse,RG_almode_fine,RG_almode_reg ) ;
         Iarg++ ; continue ;
      }

      /** 05 Nov 1997: -params maxite sig dxy dph fsig fdxy fdph **/

      if( strncmp(Argv[Iarg],"-params",6) == 0 ){
         int maxite ;
         float sig,dxy,dph,fsig,fdxy,fdph ;
         maxite = strtol( Argv[++Iarg] , NULL , 10 ) ;
         sig    = strtod( Argv[++Iarg] , NULL ) * 0.42466090 ;
         dxy    = strtod( Argv[++Iarg] , NULL ) ;
         dph    = strtod( Argv[++Iarg] , NULL ) ;
         fsig   = strtod( Argv[++Iarg] , NULL ) * 0.42466090 ;
         fdxy   = strtod( Argv[++Iarg] , NULL ) ;
         fdph   = strtod( Argv[++Iarg] , NULL ) ;
#ifdef USE_2DALIGN
         mri_2dalign_params( maxite,sig,dxy,dph,fsig,fdxy,fdph ) ;
#else
         mri_align_params( maxite,sig,dxy,dph,fsig,fdxy,fdph ) ;
#endif
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-nofine",6) == 0 ){
#ifdef USE_2DALIGN
         mri_2dalign_params( 0 , 0.0,0.0,0.0 , 0.0,0.0,0.0 ) ;
#else
         mri_align_params( 0 , 0.0,0.0,0.0 , 0.0,0.0,0.0 ) ;
#endif
         Iarg++ ; continue ;
      }

      if( strncmp(Argv[Iarg],"-fine",4) == 0 ){
         float fsig,fdxy,fdph ;
         fsig = strtod( Argv[++Iarg] , NULL ) * 0.42466090 ;
         fdxy = strtod( Argv[++Iarg] , NULL ) ;
         fdph = strtod( Argv[++Iarg] , NULL ) ;
#ifdef USE_2DALIGN
         mri_2dalign_params( 0 , 0.0,0.0,0.0 , fsig,fdxy,fdph ) ;
#else
         mri_align_params( 0 , 0.0,0.0,0.0 , fsig,fdxy,fdph ) ;
#endif
         Iarg++ ; continue ;
      }

      /** ALGORITHM: -dfspace **/

      if( strstr(Argv[Iarg],"-dfspace") != NULL && strstr(Argv[Iarg],"-dfspacetime") == NULL ){
         RG_meth     = ALIGN_DFSPACE_TYPE ;
         RG_methcode = 0 ;
         if( strstr(Argv[Iarg],":0") != NULL ) RG_methcode |= ALIGN_NOITER_CODE ;
         Iarg++ ; continue ;
      }

#ifdef ALLOW_DFTIME
      /** -dftime **/

      if( strstr(Argv[Iarg],"-dftime") != NULL ){
         RG_meth     = ALIGN_DFTIME_TYPE ;
         RG_methcode = 0 ;
         if( strstr(Argv[Iarg],":0") != NULL ) RG_methcode |= ALIGN_NOITER_CODE ;
         if( strstr(Argv[Iarg],":d") != NULL ) RG_methcode |= ALIGN_DETREND_CODE ;
         Iarg++ ; continue ;
      }

      /** ALGORITHM: -dfspacetime **/

      if( strstr(Argv[Iarg],"-dfspacetime") != NULL ){
         RG_meth     = ALIGN_DFTIME_TYPE ;
         RG_methcode = ALIGN_DOBOTH_CODE ;
         if( strstr(Argv[Iarg],":0") != NULL ) RG_methcode |= ALIGN_NOITER_CODE ;
         if( strstr(Argv[Iarg],":d") != NULL ) RG_methcode |= ALIGN_DETREND_CODE ;
         Iarg++ ; continue ;
      }
#endif

      /** get to here is bad news **/

      fprintf(stderr,"** Unknown option: %s\a\n",Argv[Iarg]) ;
      exit(1) ;

   }

   if( RG_verbose ) RG_methcode |= ALIGN_VERBOSE_CODE ;
   if( RG_debug   ) RG_methcode |= ALIGN_DEBUG_CODE ;

   /*** All options have been loaded.  Next, read base_image, if available ***/

   if( Iarg >= Argc ){
      fprintf(stderr,"** No baseimage specified on command line!\a\n") ;
      exit(1) ;
   }

   if( Argv[Iarg][0] != '+' ){
      tim = mri_read_just_one( Argv[Iarg] ) ;
      if( tim == NULL ){
         fprintf(stderr,"** Can't read base_image -- end of run!\a\n") ; exit(1) ;
      } else if ( ! MRI_IS_2D(tim) ){
         fprintf(stderr,"** Base image is not 2D!\a\n") ; exit(1) ;
      }
      nxbase = tim->nx ;
      nybase = tim->ny ;
      if( tim->kind == MRI_float )  RG_baseimage = tim ;
      else                        { RG_baseimage = mri_to_float(tim) ; mri_free(tim) ; }
      basecode = BASE_INPUT ;
      if( RG_verbose ) printf("-- read base image: file %s\n",Argv[Iarg]) ;
   } else if( strcmp(Argv[Iarg],"+AVER")==0 || strcmp(Argv[Iarg],"+aver")==0 ){
      basecode = BASE_AVER ;
      if( RG_verbose ) printf("-- will set base image to AVER\n") ;
   } else {
      char * cp ;
      basecode = strtol( Argv[Iarg]+1 , &cp , 10 ) ;
      if( *cp != '\0' || basecode < 1 ){
         fprintf(stderr,"** Can't interpret '+count' base_image input: %s\a\n",Argv[Iarg]) ;
         exit(1) ;
      }
      if( RG_verbose ) printf("-- will set base image to input # %d\n",basecode) ;
   }

   /*** Read entire image sequence ***/

   Iarg++ ;
   if( Iarg >= Argc ){
      fprintf(stderr,"** No image sequence specified on command line!\a\n") ;
      exit(1) ;
   }

   INIT_IMARR(RG_imseq) ;

   for( ; Iarg < Argc ; Iarg++ ){

      tarr = mri_read_file( Argv[Iarg] ) ;

      if( tarr == NULL || tarr->num == 0 ){
         fprintf(stderr,
                 "** Can't read image(s) from file %s -- end of run!\a\n", Argv[Iarg]) ;
         exit(1) ;
      }

      if( RG_out_mode < 0 ) RG_out_mode = tarr->imarr[0]->kind ;

      for( ii=0 ; ii < tarr->num ; ii++ ){
         if( ! MRI_IS_2D(tarr->imarr[ii]) ){
            fprintf(stderr,"** Some input image is not 2D\a\n") ; exit(1) ;
         }
         if( tarr->imarr[ii]->kind == MRI_float ){
            ADDTO_IMARR( RG_imseq , tarr->imarr[ii] ) ;
         } else {
            tim = mri_to_float( tarr->imarr[ii] ) ;
            ADDTO_IMARR( RG_imseq , tim ) ;
            mri_free( tarr->imarr[ii] ) ;
         }
      }

      FREE_IMARR( tarr ) ;  /* not DESTROY */
   }

   /*** if no base image, get dimensions from 1st sequence image ***/

   if( RG_baseimage == NULL ){
      if( RG_imseq->num <= 1 ){
         fprintf(stderr,
                 "** No base_image supplied and only 1 image in sequence?\n"
                 "** Makes no sense!  End of run!\a\n" ) ;
         exit(1) ;
      }
      nxbase  = RG_imseq->imarr[0]->nx ;
      nybase  = RG_imseq->imarr[0]->ny ;
   }

   /*** for each image in the sequence:
          check for conformant dimensions ***/

   nerr = 0 ;
   for( ii=0 ; ii < RG_imseq->num ; ii++ ){
      tim = RG_imseq->imarr[ii] ;
      if( tim->nx != nxbase || tim->ny != nybase ){
         fprintf(stderr,"** Image %d dimensions do not match base image!\a\n",ii+1) ;
         nerr++ ;
      }
   }
   if( nerr > 0 ) exit(1) ;

   /*** if needed, create base image from image sequence ***/

   if( RG_baseimage == NULL ){
      if( basecode == BASE_AVER ){
         register int pp , npix ;
         register float * flar , * flfl ;
         register float fac ;

         if( RG_verbose ) printf("-- computing AVER image now\n") ;

         RG_baseimage = mri_new( nxbase , nybase , MRI_float ) ;
         flar         = mri_data_pointer( RG_baseimage ) ;
         npix         = nxbase * nybase ;
         for( pp=0 ; pp < npix ; pp++ ) { flar[pp] = 0.0 ; }

         for( ii=0 ; ii < RG_imseq->num ; ii++ ){
            flfl = mri_data_pointer( RG_imseq->imarr[ii] ) ;
            for( pp=0 ; pp < npix ; pp++ ) flar[pp] += flfl[pp] ;
         }

         fac = 1.0 / RG_imseq->num ;
         for( pp=0 ; pp < npix ; pp++ ) flar[pp] *= fac ;

      } else if( basecode > 0 && basecode <= RG_imseq->num ){
         if( RG_verbose ) printf("-- setting base image now\n") ;
         RG_baseimage = mri_to_float( RG_imseq->imarr[basecode-1] ) ;  /* copy it */
      } else {
         fprintf(stderr,"** Can't make baseimage as specified!\a\n") ;
         exit(1) ;
      }
   }

   /*** done (I hope) ***/

   return ;
}
