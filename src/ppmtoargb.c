/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <string.h>

#define Amax   0
#define Asolid 1
#define Ainten 2

int main( int argc , char * argv[] )
{
   MRI_IMARR * ppmar ;
   MRI_IMAGE * rim , * gim , * bim ;
   byte      * rby , * gby , * bby , * argb ;
   byte aa,rr,gg,bb ;

   int Askip=0 , Rskip=0 , Gskip=0 , Bskip=0 ;
   int Aoff =0 , Roff =1 , Goff =2 , Boff =3 ;
   int Acalc = Amax ;
   int nx , ny , nz , nper=4 , iarg, ii,jj,kk , npix ;

   /** check if help is needed **/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      printf(
       "Usage: ppmtoargb [options] ppm_files\n"
       "-- Takes as input a bunch of raw PPM files and writes them\n"
       "-- out as a 3D brick of ARGB byte values, where\n"
       "-- A = max(r,g,b).\n"
       "-- Output is written to stdout and should be redirected!\n"
       "-- Dimensions of output brick are written to stderr.\n"
       "\n"
       "Options are:\n"
       "  -ARGB  = write out data in ARGB order [default].\n"
       "  -ABGR  = write out data in ABGR order [good for NCSA].\n"
       "  -????  = up to four letters to specify output byte order;\n"
       "           a missing letter means don't write that channel\n"
       "           (e.g., -RGB means write R then G then B, and no A).\n"
       "           In all cases, bytes for each voxel are written contiguously.\n"
       "  -solid = Flag for alternate calculation of A, using\n"
       "            A = r if (r==g && g==b) else A = 255 (max).\n"
       "  -inten = Flag for alternate calculation of A, using\n"
       "            A = .299*r + .587*g + .114*b (total intensity).\n"
      ) ;
      exit(0) ;
   }

   /** read options **/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-solid",5) == 0 ){
         Acalc = Asolid ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-inten",5) == 0 ){
         Acalc = Ainten ;
         iarg++ ; continue ;
      }

      if( argv[iarg][1] == 'A' || argv[iarg][1] == 'R' ||
          argv[iarg][1] == 'G' || argv[iarg][1] == 'B'    ){

         char * ch ;

         nper = strlen(argv[iarg]) - 1 ;
         if( nper < 1 || nper > 4 ){
            fprintf(stderr,"Illegal order option %s\n",argv[iarg]) ; exit(-1) ; }

         ch = strchr( argv[iarg] , 'A' ) ;
         if( ch == NULL ){ Askip = 1 ; }
         else            { Askip = 0 ; Aoff = (ch - argv[iarg]) - 1 ; }

         ch = strchr( argv[iarg] , 'B' ) ;
         if( ch == NULL ){ Bskip = 1 ; }
         else            { Bskip = 0 ; Boff = (ch - argv[iarg]) - 1 ; }

         ch = strchr( argv[iarg] , 'R' ) ;
         if( ch == NULL ){ Rskip = 1 ; }
         else            { Rskip = 0 ; Roff = (ch - argv[iarg]) - 1 ; }

         ch = strchr( argv[iarg] , 'G' ) ;
         if( ch == NULL ){ Gskip = 1 ; }
         else            { Gskip = 0 ; Goff = (ch - argv[iarg]) - 1 ; }

         if( nper + Askip + Bskip + Gskip + Rskip != 4 ){
            fprintf(stderr,"Illegal order option %s\n",argv[iarg]) ; exit(-1) ; }

#if 0
         fprintf(stderr,
                 "Askip = %d  Aoff = %d\n"
                 "Bskip = %d  Boff = %d\n"
                 "Gskip = %d  Goff = %d\n"
                 "Rskip = %d  Roff = %d\n" ,
                 Askip,Aoff, Bskip,Boff, Gskip,Goff, Rskip,Roff ) ;
#endif

         iarg++ ; continue ;
      }

      /** unknown option **/

      fprintf(stderr,"Unknown option %s\n",argv[iarg]) ; exit(-1) ;
   }

   /*** read and process input files ***/

   nz = argc - iarg ;
   if( nz < 1 ){
      fprintf(stderr,"No input images on command line!\n") ; exit(-1) ;
   }

   for( kk=0 ; kk < nz ; kk++ ){

      ppmar = mri_read_ppm3( argv[kk+iarg] ) ;
      if( ppmar == NULL || ppmar->num != 3 ){
         fprintf(stderr,"Cannot read input image file %s\n",argv[kk+iarg]) ;
         exit(-1) ;
      }

      rim = ppmar->imarr[0] ; rby = mri_data_pointer( rim ) ;
      gim = ppmar->imarr[1] ; gby = mri_data_pointer( gim ) ;
      bim = ppmar->imarr[2] ; bby = mri_data_pointer( bim ) ;

      FREE_IMARR(ppmar) ;

      if( kk == 0 ){
         int kbytes ;

         nx = rim->nx ; ny = rim->ny ; npix = nx*ny ;

         argb = (byte *) malloc( sizeof(byte) * nper * nx * ny ) ;
         if( argb == NULL ){
            fprintf(stderr,"Cannot malloc workspace in ppmtoargb!\n") ;
            exit(-1) ;
         }

         kbytes = 0.001 * nx * ny * nz * nper ;
         fprintf(stderr,
           "Output brick is %d x %d x %d (%d bytes per voxel) = %d Kbytes ",
           nx,ny,nz,nper , kbytes) ;
         fflush(stderr) ;

      } else if( rim->nx != nx || rim->ny != ny ){
         fprintf(stderr,"Image mismatch in input image file %s\n",argv[kk+iarg]) ;
         exit(-1) ;
      }

      jj = 0 ;
      for( ii=0 ; ii < npix ; ii++ ){
         rr = rby[ii] ; gg = gby[ii] ; bb = bby[ii] ;

         if( !Rskip ) argb[jj+Roff] = rr ;
         if( !Gskip ) argb[jj+Goff] = gg ;
         if( !Bskip ) argb[jj+Boff] = bb ;

         if( !Askip ){
            switch( Acalc ){
               default:
               case Amax:
                  aa = MAX(rr,gg) ; aa = MAX(aa,bb) ;
               break ;

               case Asolid:
                  aa = (rr==gg && gg==bb) ? (rr) : (255) ;
               break ;

               case Ainten:
                  aa = (byte) (0.299*rr + 0.587*gg + 0.114*bb) ;
               break ;
            }

            argb[jj+Aoff] = aa ;
         }

         jj += nper ;
      }

      fwrite( argb , sizeof(byte) , npix*nper , stdout ) ;

      mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;

      if( kk%10 == 5 ) { fprintf(stderr,".") ; fflush(stderr) ; }
   }

   fprintf(stderr,"\n") ;
   exit(0) ;
}
