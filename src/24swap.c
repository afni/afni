#include <stdio.h>
#include <unistd.h>
#include "mrilib.h"

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

#define TEMP_FILE "Frodo.Lives"
#define BUFSIZE   64000

static byte bbuf[BUFSIZE] ;

#define MAXPAT 256

int main( int argc , char * argv[] )
{
   FILE * infil , * outfil ;
   int narg , nbyte , nbyt , quiet = 0 , ndone ;
   int npat,ipat,jpat,ii,typ,len,num , cpat_len ;
   int pattype[MAXPAT] , patsize[MAXPAT] ;
   char * cpat = NULL ;

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
     printf("Usage: 24swap [options] file ...\n"
            "Swaps bytes pairs and/or quadruples on the files listed.\n"
            "Options:\n"
            " -q            Operate quietly\n"
            " -pattern pat  'pat' determines the pattern of 2 and 4\n"
            "                 byte swaps.  Each element is of the form\n"
            "                 2xN  or 4xN, where N is the number of\n"
            "                 bytes to swap as pairs (for 2x) or\n"
            "                 as quadruples (for 4x).  For 2x, N must\n"
            "                 be divisible by 2; for 4x, N must be\n"
            "                 divisible by 4.  The whole pattern is\n"
            "                 made up of elements separated by colons,\n"
            "                 as in '-pattern 4x39984:2x0'.  If bytes\n"
            "                 are left over after the pattern is used\n"
            "                 up, the pattern starts over.  However,\n"
            "                 if a byte count N is zero, as in the\n"
            "                 example, then it means to continue until\n"
            "                 the end of file.\n"
            "\n"
            " N.B.: A default pattern can be stored in the Unix\n"
            "         environment variable AFNI_24SWAP_PATTERN.\n"
            "         If no -pattern option is given, the default\n"
            "         will be used.  If there is no default, then\n"
            "         nothing will be done.\n"
            " N.B.: If there are bytes 'left over' at the end of the file,\n"
            "         they are written out unswapped.  This will happen\n"
            "         if the file is an odd number of bytes long.\n"
            " N.B.: If you just want to swap pairs, see program 2swap.\n"
            "         For quadruples only, see program 4swap.\n"
            " N.B.: This program will overwrite the input file!\n"
            "         You might want to test it first.\n"
            "\n"
            " Example: 24swap -pat 4x8:2x0 fred\n"
            "          If fred contains 'abcdabcdabcdabcdabcd' on input,\n"
            "          then fred has    'dcbadcbabadcbadcbadc' on output.\n"
         ) ;
     exit(0) ;
   }

   /*-- scan arguments --*/

   narg = 1 ;

   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-q",2) == 0 ){
         quiet = 1 ; narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-pat",4) == 0 ){
         cpat = argv[++narg] ; narg++ ; continue ;
      }

      fprintf(stderr,"** 24swap: Unknown option %s\n",argv[narg]) ;
      exit(1) ;
   }

   if( narg == argc ){
     fprintf(stderr,"** 24swap: No input files?\n") ; exit(1) ;
   }

   if( cpat == NULL ) cpat = getenv("AFNI_24SWAP_PATTERN") ;
   if( cpat == NULL ){
     fprintf(stderr,"** 24swap: No pattern?  Exiting!\n") ; exit(1) ;
   }
   cpat_len = strlen(cpat) ;
   if( cpat_len < 3 ){
     fprintf(stderr,"** 24swap: Stupid pattern?  Exiting!\n") ; exit(1) ;
   }

   /*-- parse cpat --*/

   ipat = npat = 0 ; cpat_len = strlen(cpat) ;
   while( ipat < cpat_len ){
      ii = sscanf( cpat+ipat , "%dx%d%n" , &typ,&len,&num ) ;
      if( ii <= 0 ) break ;
      if( ii != 2 ){
         fprintf(stderr,"** 24swap: illegal pattern %s\n",cpat); exit(1);
      }
      if( len < 0 || (typ!=2 && typ!=4) ){
         fprintf(stderr,"** 24swap: illegal pattern %s\n",cpat); exit(1);
      }
      if( typ == 2 && len%2 != 0 ){
         fprintf(stderr,"** 24swap: illegal pattern %s\n",cpat); exit(1);
      }
      if( typ == 4 && len%4 != 0 ){
         fprintf(stderr,"** 24swap: illegal pattern %s\n",cpat); exit(1);
      }

      pattype[npat] = typ ;
      patsize[npat] = len ;
      npat++ ; ipat += num+1 ;
   }

   if( npat < 1 ){
      fprintf(stderr,"** 24swap: illegal pattern %s\n",cpat); exit(1);
   }

   /*-- loop over files --*/

   for( ; narg < argc ; narg++ ){
      infil = fopen( argv[narg] , "r" ) ;
      if( infil == NULL ){
         fprintf(stderr,"** 24swap: File %s not found - skipping it!\n",argv[narg]);
         continue ;
      }

      if( !quiet){ printf("-- opened %s",argv[narg]) ; fflush(stdout) ; }

      outfil = fopen( TEMP_FILE , "w" ) ;
      if( outfil == NULL ){
         printf("** 24swap: Cannot open temporary file - exiting!\n"); exit(1);
      }

      ndone = 0 ; ipat = 0 ;  /* loop over patterns and execute them */
      while(1){

         if( patsize[ipat] == 0 ){  /* do rest of file this way */
            while(1){
               nbyt = fread( bbuf , sizeof(byte) , BUFSIZE , infil ) ;
               if( nbyt <= 0 ) break ;  /* end of file */
               switch( pattype[ipat] ){
                  case 2: swap_twobytes ( nbyt/2 , bbuf ) ; break ;
                  case 4: swap_fourbytes( nbyt/4 , bbuf ) ; break ;
               }
               fwrite( bbuf , sizeof(byte) , nbyt , outfil ) ;
               ndone += nbyt ;
               if( !quiet && ndone > 1000000 ){
                  ndone -= 1000000 ; printf(".") ; fflush(stdout) ;
               }
            }
            break ; /* to end of outer loop */

         } else {                   /* do patsize[ipat] bytes */
            ii = 0 ;
            while(1){
               jpat = MIN( patsize[ipat] - ii , BUFSIZE ) ;
               nbyt = fread( bbuf , sizeof(byte) , jpat , infil ) ;
               if( nbyt <= 0 ) break ;  /* end of file */
               switch( pattype[ipat] ){
                  case 2: swap_twobytes ( nbyt/2 , bbuf ) ; break ;
                  case 4: swap_fourbytes( nbyt/4 , bbuf ) ; break ;
               }
               fwrite( bbuf , sizeof(byte) , nbyt , outfil ) ;
               ndone += nbyt ;
               if( !quiet && ndone > 1000000 ){
                  ndone -= 1000000 ; printf(".") ; fflush(stdout) ;
               }
               ii += nbyt ; if( ii >= patsize[ipat] ) break ;
            }
         }

         if( nbyt <= 0 ) break ; /* to end of outer loop */

         ipat++ ; if( ipat >= npat ) ipat = 0 ;  /* continue outer loop */

      } /* end of outer loop */

      fsync(fileno(outfil)) ; fclose(infil) ; fclose(outfil) ;

      unlink( argv[narg] ) ;
      rename( TEMP_FILE , argv[narg] ) ;

      if( !quiet ){ printf(".\n") ; fflush(stdout) ; }
   }
   exit(0) ;
}
