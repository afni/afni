#include <math.h>
#include <stdio.h>
#include <unistd.h>

#if defined(SUN) || defined(SOLARIS) || defined(SGI)
# include <ieeefp.h>
# define USE_ISNANF
#endif

#if defined(HP)
# define USE_ISNANF
# define USE_FINITEF
#endif

#ifdef USE_ISNANF
# define IS_NAN(x) isnanf(x)
#else
# define IS_NAN(x) isnan(x)
#endif

#ifdef USE_FINITEF
# define IS_FINITE(x) finitef(x)
#else
# define IS_FINITE(x) finite(x)
#endif

#define FSET(nf) do{ fseek(fp,sizeof(float)*nf,SEEK_SET); fpos=nf; } while(0)

#define NBUF 1024

#define READ_BUF  do{ nbuf = fread(fbuf,sizeof(float),NBUF,fp); } while(0)
#define WRITE_BUF \
   do{ fwrite(fbuf,sizeof(float),nbuf,stdout); fflush(stdout); } while(0)

int main( int argc , char * argv[] )
{
   int iarg=1 , ii , typ ;
   int fix=0 , verb=0 , skip=0 ;
   int num_inf=0 , num_nan=0 , num_this ;

   int fsize , fpos ; FILE * fp ;

   int nbuf ; float fbuf[NBUF] ;

   /*-- help --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf( "Usage: float_scan [options] input_filename\n"
              "Scans the input file of IEEE floating point numbers for\n"
              "illegal values: infinities and not-a-number (NaN) values.\n"
              "\n"
              "Options:\n"
              "  -fix     = Writes a copy of the input file to stdout (which\n"
              "               should be redirected using '>'), replacing\n"
              "               illegal values with 0.  If this option is not\n"
              "               used, the program just prints out a report.\n"
              "  -v       = Verbose mode: print out index of each illegal value.\n"
              "  -skip n  = Skip the first n floating point locations\n"
              "               (i.e., the first %d*n bytes) in the file\n"
              "\n"
              "N.B.: This program does NOT work on compressed files, nor does it\n"
              "      work on byte-swapped files (e.g., files transferred between\n"
              "      Sun/SGI/HP and Intel platforms)!\n"
              "\n"
              "The program 'exit status' is 1 if any illegal values were\n"
              "found in the input file.  If no errors were found, then\n"
              "the exit status is 0. You can check the exit status by\n"
              "using the shell variable $status.  A C-shell example:\n"
              "   float_scan fff\n"
              "   if ( $status == 1 ) then\n"
              "      float_scan -fix fff > Elvis.Aaron.Presley\n"
              "      rm -f fff\n"
              "      mv Elvis.Aaron.Presley fff\n"
              "   endif\n"
             ,
             sizeof(float)
            ) ;
      exit(0) ;
   }

   /*-- parse options -- */

   while( argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-fix") == 0 ){
         fix = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-v") == 0 ){
         verb = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-skip") == 0 ){
         if( ++iarg >= argc ){
            fprintf(stderr,"*** no value after -skip?!\n") ; exit(0) ;
         }
         skip = strtol( argv[iarg] , NULL , 10 ) ;
         if( skip < 0 ){
            fprintf(stderr,"*** -skip value of %d is illegal!\n",skip) ; exit(0) ;
         } else if ( skip == 0 ){
            fprintf(stderr,"+++ -skip value of 0 is legal, but not required.\n") ;
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[iarg] ) ; exit(0) ;
   }

   if( iarg >= argc ){
      fprintf(stderr,"*** No input filename given!?\n") ; exit(0) ;
   }

   /*-- open file --*/

   fp = fopen( argv[iarg] , "r" ) ;
   if( fp == NULL ){
      fprintf(stderr,"*** Can't open input file: %s\n",argv[iarg]) ; exit(0) ;
   }

   fsize = THD_filesize( argv[iarg] ) ;
   if( fsize % sizeof(float) != 0 ){
      fprintf(stderr,"*** File %s is %d bytes long: not a multiple of %d!\n",
              argv[iarg] , fsize , sizeof(float) ) ;
      exit(0) ;
   }

   FSET(skip) ;

   /*-- loop, reading buffer, scanning for error, maybe rewriting --*/

   while(1){

      READ_BUF ; if( nbuf == 0 ) break ;  /* this is the loop exit */

      for( num_this=ii=0 ; ii < nbuf ; ii++ ){

         if( ! IS_FINITE(fbuf[ii]) ){

            typ = IS_NAN(fbuf[ii]) ;
            if( typ ) num_nan++ ; else num_inf++ ;

            if( verb )
               fprintf(stderr," [%d] is %s\n",
                       fpos+ii , (typ) ? "NaN" : "Infinite") ;

            fbuf[ii] = 0.0 ; num_this++ ;
         }
      }

      if( fix ) WRITE_BUF ;

      FSET(fpos+nbuf) ; /* set file position to start of next buffer */

   } /* end of "infinite" loop */

   /*-- done --*/

   fclose(fp) ;

   if( num_nan )
      fprintf(stderr,"+++ Found %d NaN values in %s\n"     ,num_nan,argv[iarg]) ;
   if( num_inf )
      fprintf(stderr,"+++ Found %d Infinite values in %s\n",num_inf,argv[iarg]) ;
   if( num_nan+num_inf == 0 )
      fprintf(stderr,"+++ No float errors found in %s\n",argv[iarg]) ;

   exit( (num_nan+num_inf) > 0 ) ;
}
