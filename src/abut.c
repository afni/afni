/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994,1995 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"
#include <string.h>

void AB_interp( int , double ) ;

#define LINEAR 7
#define BLOCKY 8

void Syntax(void)
{
   printf(
     "ABUT:  put noncontiguous FMRI slices together [for to3d]\n"
     "       Copyright 1994,1995 Medical College of Wisconsin\n"
     "\n"
     "method: put zero valued slices in the gaps, then\n"
     "        replicate images to simulate thinner slices\n"
     "\n"
     "Usage:\n"
     "   abut [-dzin thickness] [-dzout thickness] [-root name]\n"
     "        [-linear | -blocky] [-verbose] [-skip n+gap] ... images ...\n"
     "\n"
     "   -dzin   the thickness value in mm;  if not given,\n"
     "             taken to be 1.0 (in which case, the output\n"
     "             thickness and gap sizes are simply relative\n"
     "             to the slice thickness, rather than absolute)\n"
     "\n"
     "   -dzout  the output slice thickness, usually smaller than\n"
     "             the input thickness;  if not given, the program\n"
     "             will compute a value (the smaller the ratio\n"
     "             dzout/dzin is, the more slices will be output)\n"
     "\n"
     "   -root   'name' is the root (or prefix) for the output\n"
     "             slice filename;  for example, '-root fred.'\n"
     "             will result in files fred.0001, fred.0002, ...\n"
     "\n"
     "   -linear if present, this flag indicates that subdivided slices\n"
     "             will be linearly interpolated rather than simply\n"
     "             replicated -- this will make the results smoother\n"
     "             in the through-slice direction (if dzout < dzin)\n"
     "\n"
     "   -blocky similar to -linear, but uses AFNI's 'blocky' interpolation\n"
     "             when possible to put out intermediate slices.\n"
     "             Both interpolation options only apply when dzout < dzin\n"
     "             and when an output slice has a non-gappy neighbor.\n"
     "\n"
     "   -skip   'n+gap' indicates that a gap is to be inserted\n"
     "             between input slices #n and #n+1, where n=1,2,...;\n"
     "             for example, -skip 6+5.5 means put a gap of 5.5 mm\n"
     "             between slices 6 and 7.\n"
     "\n"
     "   More than one -skip option is allowed.  They must all occur\n"
     "   before the list of input image filenames.\n"
   ) ;

   exit(0) ;
}

/*-------------------------------------------------------------------*/

#define SLICES_MAX 128

static MRI_IMAGE * imin[SLICES_MAX] , * zero_im ;
static double gap[SLICES_MAX] ;
static double zbot[SLICES_MAX] , ztop[SLICES_MAX] ;

static double dzin = 1.0 , dzout = 0.0 , gap_max , gap_sum ;
static char prefix[256] = "abut." ;
static int interp = 0 ;
static int verbose = 0 ;
static int iout , nfiles , nx , ny ;
static char fname[256] ;

int main( int argc , char * argv[] )
{
   int isl , nopt , ii ;
   double fff , zout ;
   short * zar ;

   /*----- initialize -----*/

   if( argc < 3 || strncmp(argv[1],"-help",2) == 0 ) Syntax() ;

   for( isl=0 ; isl < SLICES_MAX ; isl++ ){
      imin[isl] = NULL ;
      gap[isl]  = 0.0 ;
   }

   /*----- read command line options -----*/

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /*--- -verbose ---*/

      if( strncmp(argv[nopt],"-verbose",4) == 0 ){
         verbose = 1 ;
         nopt++ ; continue ;
      }

      /*--- -linear ---*/

      if( strncmp(argv[nopt],"-linear",4) == 0 ){
         interp = LINEAR ;
         nopt++ ; continue ;
      }

      /*--- -blocky ---*/

      if( strncmp(argv[nopt],"-blocky",4) == 0 ){
         interp = BLOCKY ;
         nopt++ ; continue ;
      }

      /*--- -dzin thickness ---*/

      if( strncmp(argv[nopt],"-dzin",4) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"\n*** no argument for -dzin?\n\n") ;
            Syntax() ;
         }
         fff = strtod( argv[nopt] , NULL ) ;
         if( fff <= 0.0 ){
            fprintf(stderr,"\n*** illegal argument for -dzin: %f\n\n",fff) ;
            Syntax() ;
         }
         dzin = fff ;
         nopt++ ; continue ;
      }

      /*--- -dzout thickness ---*/

      if( strncmp(argv[nopt],"-dzout",4) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"\n*** no argument for -dzout?\n\n") ;
            Syntax() ;
         }
         fff = strtod( argv[nopt] , NULL ) ;
         if( fff <= 0.0 ){
            fprintf(stderr,"\n*** illegal argument for -dzout: %f\n\n",fff) ;
            Syntax() ;
         }
         dzout = fff ;
         nopt++ ; continue ;
      }

      /*--- -root name ---*/

      if( strncmp(argv[nopt],"-root",4) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"\n*** no argument for -root?\n\n") ;
            Syntax() ;
         }
         strcpy( prefix , argv[nopt] ) ;
         ii = strlen(prefix) ;
         if( prefix[ii-1] != '.' ){  /* add a period? */
            prefix[ii]   = '.' ;
            prefix[ii+1] = '\0' ;
         }
         nopt++ ; continue ;
      }

      /*--- -skip n+gap ---*/

      if( strncmp(argv[nopt],"-skip",4) == 0 ){
         int nok , nnn = -1 ;
         double ggg = -1.0 ;
         if( ++nopt >= argc ){
            fprintf(stderr,"\n*** no argument for -skip?\n\n") ;
            Syntax() ;
         }

         nok = sscanf( argv[nopt] , "%d+%lf" , &nnn , &ggg ) ;

         if( nok != 2 || nnn <= 0 || nnn > SLICES_MAX || ggg < 0 ){
            fprintf(stderr,
                    "\n*** illegal argument for -skip: %s\n\n",argv[nopt]) ;
            Syntax() ;
         }

         gap[nnn-1] = ggg ;    /* external slice indices are 1 more */
         nopt++ ; continue ;   /* than the internal index system!   */
      }
   }

   /*----- read input images -----*/

   if( nopt >= argc ){
      fprintf(stderr,"\n*** no input image files!!!\n\n") ;
      Syntax() ;
   }

   nfiles = argc - nopt ;
   if( nfiles > SLICES_MAX ){
      fprintf(stderr,
              "\n*** You input %d files, but max allowed is %d!!!\n\n" ,
              nfiles , SLICES_MAX ) ;
      Syntax() ;
   } else if( nfiles < 1 ){
      fprintf(stderr,"\n*** no input image files!!!\n\n") ;
      Syntax() ;
   } else if( nfiles == 1 ){
      fprintf(stderr,"\n*** only one input image file!!!\n\n") ;
      Syntax() ;
   }

   imin[0] = mri_read_just_one( argv[nopt] ) ;
   if( imin[0] == NULL ) exit(-1) ;
   if( imin[0]->kind != MRI_short ){
      fprintf(stderr,"\n*** Cannot deal with non-short images!\n") ;
      exit(-1) ;
   }
   nx = imin[0]->nx ; ny = imin[0]->ny ;

   for( isl=1 ; isl < nfiles ; isl++ ){
      imin[isl] = mri_read_just_one( argv[nopt+isl] ) ;
      if( imin[isl] == NULL ) exit(-1) ;

      if( imin[isl]->nx != nx || imin[isl]->ny != ny ){
         fprintf(stderr,
                 "\n*** first file is %d x %d but file %s is %d x %d!\n\n",
                 nx,ny , argv[nopt+isl] , imin[isl]->nx , imin[isl]->ny ) ;
         exit(-1) ;
      }

      if( imin[isl]->kind != MRI_short ){
         fprintf(stderr,
                 "\n*** image file %s is not an image of shorts!\n",
                 argv[nopt+isl] ) ;
         exit(-1) ;
      }
   }  /* all images input when this loop exits */

   zero_im = mri_new( nx , ny , MRI_short ) ;
   zar     = mri_data_pointer( zero_im ) ;
   for( ii=0 ; ii < (nx*ny) ; ii++ ) zar[ii] = 0 ;

   /*----- adjust gaps to be relative to dzin -----*/

   gap_max = 0 ;
   for( isl=0 ; isl < nfiles-1 ; isl++ ){
      gap[isl] = gap[isl] / dzin ;
      gap_max  = (gap_max < gap[isl]) ? (gap[isl]) : (gap_max) ;
   }

   /*----- compute dzout (relative to dzin) if not already given -----*/

   if( dzout > 0.0 ){
      dzout = dzout / dzin ;
   } else if( gap_max == 0.0 ){
      dzout = 1.0 ;
   } else {
#define NBASE 5
      int ibase , ibest ;
      double fit , worst_fit[NBASE+1];

      for( ibase=1 ; ibase <= NBASE ; ibase++ ){   /* find worst error */
         worst_fit[ibase] = 0.0 ;                  /* for dzout = 1/ibase */

         for( isl=0 ; isl < nfiles-1 ; isl++ ){
            if( gap[isl] > 0.0 ){
               fit = gap[isl] * ibase ;
               fit = fabs( fit - floor(fit+0.5) ) ;
               if( fit > worst_fit[ibase] ) worst_fit[ibase] = fit ;
            }
         }
      }

      for( ibase=1 ; ibase <= NBASE ; ibase++ )
         if( worst_fit[ibase] < 0.05 ) break ;   /* first one < 5% */

      ibest = ibase ;
      if( ibest > NBASE ){                       /* otherwise take best */
         fit   = worst_fit[1] ;
         ibest = 1 ;
         for( ibase=2 ; ibase <= NBASE ; ibase++ )
            if( worst_fit[ibase] < fit ){
               ibest = ibase ;
               fit   = worst_fit[ibase] ;
            }
      }

      dzout = ((double) 1.0) / (double) ibest ;

      printf("-- computed dzout = %f\n" , dzout * dzin ) ;
   }

   /*----- adjust gaps to be integer multiples of dzout -----*/

   for( isl=0 ; isl < nfiles-1 ; isl++ ){

      fff      = gap[isl] ;
      iout     = (int)( fff / dzout + 0.5 ) ;
      gap[isl] = iout * dzout ;

      if( fabs(gap[isl]-fff) > 0.001 )
         printf("-- adjusted gap after slice %d to %f\n",
                isl+1 , gap[isl]*dzin ) ;
   }

   /*----- loop through output points and output stuff -----*/

   isl  = 0 ;
   zout = 0 ;

   gap_sum = 0.0 ;
   for( isl=0 ; isl < nfiles ; isl++ ){
      zbot[isl] = zout ;
      ztop[isl] = zout + 1.0 ;
      zout     += 1.0 + gap[isl] ;  /* start of next slice */
   }

   zout = 0.001 ;
   iout = 0 ;
   isl  = 0 ;

   do {

     iout++ ;
     sprintf(fname,"%s%04d",prefix,iout) ;

     if( zout >= zbot[isl] && zout < ztop[isl] ){  /* inside this slice */
        AB_interp( isl , zout ) ;
        continue ;
     }

     if( zout >= ztop[isl] ){  /* move to next slice */
        isl++ ;
     }

     if( zout < zbot[isl] ){  /* before slice ==> in a gap */
        mri_write( fname , zero_im ) ;
        if(verbose) printf("  -- new slice %d is all zeros\n",iout) ;
        continue ;
     }

     AB_interp( isl , zout ) ;  /* into the next slice now */

   } while ( (zout += dzout) < ztop[nfiles-1] ) ;

   /*----- DONE -----*/

   printf("-- wrote %d output slices\n",iout) ;
   exit(0) ;
}

void AB_interp( int isl , double zout )
{
   int nb , ii ;
   double wt , fnow,fnb;
   short * snow , * snb , * sint ;
   MRI_IMAGE * imint ;

   if( interp != LINEAR && interp != BLOCKY ){    /* this is easy! */
      mri_write( fname , imin[isl] ) ;
      if(verbose)
        printf("  -- new slice %d is a replica of input %d\n",iout,isl+1) ;
      return ;
   }

   /* must deal with possibility of interpolation */

   /* possible to interpolate below? */

   if( zout-zbot[isl] <= 0.5 &&               /* closer to below */
       isl > 0 &&                             /* there is a below */
       fabs(ztop[isl-1]-zbot[isl]) < 0.001 ){ /* below is adjacent */

      nb = isl-1 ;                 /* neighbor index */
      wt = zout-zbot[isl] + 0.5 ;  /* distance from below mid to zout */

   /* possible to interpolate above? */

   } else if( ztop[isl]-zout <= 0.5 &&               /* closer to above */
              isl < nfiles-1 &&                      /* there is an above */
              fabs(zbot[isl+1]-ztop[isl]) < 0.001 ){ /* above is adjacent */

      nb = isl+1 ;                  /* neighbor index */
      wt = ztop[isl]-zout + 0.5 ;   /* distance from above mid to zout */
   } else {
      mri_write( fname , imin[isl] ) ;  /* no interp possible */
      if(verbose)
        printf("  -- new slice %d is a replica of input %d\n",iout,isl+1) ;
      return ;
   }

   snow  = mri_data_pointer( imin[isl] ) ;
   snb   = mri_data_pointer( imin[nb]  ) ;
   imint = mri_new( nx , ny , MRI_short ) ;
   sint  = mri_data_pointer( imint ) ;

   if( interp == LINEAR ) fnow = wt ;  /* linear */
   else                   fnow = 1.0 - 8.0 * pow(1.0-wt,4.0) ;
   fnb = 1.0 - fnow ;

   for( ii=0 ; ii < (nx*ny) ; ii++ )
      sint[ii] = fnow * snow[ii] + fnb * snb[ii] ;

   if(verbose)
     printf(
       "  -- new slice %d interpolated from input %d(%6.3f) and %d(%6.3f)\n",
       iout , isl+1,fnow,nb+1,fnb ) ;

   mri_write(fname,imint) ;
   mri_free(imint) ;
   return ;
}
