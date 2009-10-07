/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

int ranco(int,int, long int) ;
extern int *z_rand_order(int bot, int top, long int seed);

int main( int argc , char *argv[] )
{
   int ii , bot = -1 , top = -1 , step = -1 , rando_count = 0, rando_num, col ;
   int narg , ndig = 4 , iout ;
   long int seed = 0;
   static char root[6664] , fmt[128] , suffix[6664] ;
   float sclfac = 0.0 ;
   int comma=0 , quiet = 0;   /* 18 Jan 2007 */
   char sep=' ' ;
   int skipn = 0; /* skip numbers that modulus m = n  08 May 2007 */
   int skipm = 0;
   int skipout;

/*** Usage ***/

   if( argc < 3 || strncmp(argv[1],"-help",2) == 0 ){

     printf(
"Usage: count [options] bot top [step]\n"
"\n"
"* Produces many numbered copies of the root and/or suffix,\n"
"    counting from 'bot' to 'top' with stride 'step'.\n"
"* If 'bot' > 'top', counts backwards with stride '-step'.\n"
"* If step is of the form 'R#', then '#' random counts are produced\n"
"    in the range 'bot..top' (inclusive).\n"
"* If step is of the form 'S', then a random sequence of unique integers\n"
"    in the range 'bot..top' (inclusive) is output.\n"
"    A number after S ('S#') indicates the number of unique integers\n"
"    to output. If # exceeds the number of unique values, the shuffled\n"
"    sequence will simply repeat itself. (N.B.: 'S' is for 'Shuffle'.)\n"
"* 'bot' and 'top' must not be negative; step must be positive (defaults to 1).\n"
"\n"
"Options:\n"
"  -seed        seed number for random number generator (for S and R above)\n"
"  -sseed       seed string for random number generator (for S and R above)\n"
"  -column      writes output, one number per line (with root and suffix, if any)\n"
"  -digits n    prints numbers with 'n' digits [default=4]\n"
"  -root rrr    prints string 'rrr' before the number [default=empty]\n"
"  -sep s       prints single character 's' between the numbers [default=blank]\n"
"                 [normally you would not use '-sep' with '-column']\n"
"  -suffix sss  prints string 'sss' after the number [default=empty]\n"
"  -scale fff   multiplies each number by the factor 'fff';\n"
"                 if this option is used, -digits is ignored and\n"
"                 the floating point format '%%g' is used for output.\n"
"                 ('fff' can be a floating point number.)\n"
"  -comma       put commas between the outputs, instead of spaces\n"
"                 (same as '-sep ,')\n"
"  -skipnmodm n m   skip over numbers with a modulus of n with m\n"
"                  -skipnmodm 15 16 would skip 15, 31, 47, ...\n"
"               not valid with random number sequence options\n"
"\n"
"The main application of this program is for use in C shell programming:\n"
"  foreach fred ( `count 1 20` )\n"
"     mv wilma.${fred} barney.${fred}\n"
"  end\n"
"The backward quote operator in the foreach statement executes the\n"
"count program, captures its output, and puts it on the command line.\n"
"The loop body renames each file wilma.0001 to wilma.0020 to barney.0001\n"
"to barney.0020.  Read the man page for csh to get more information.  In\n"
"particular, the csh built-in command '@' can be useful.\n"
     ) ;

     exit(0) ;
   }

/*** read arguments ***/

   narg      = 1 ;
   root[0]   = '\0' ;
   suffix[0] = '\0' ;
   col = 0;
   rando_count = 0;
   seed = 0;
   quiet = 0;
   do {

   /*** switches ***/

      if( strncmp(argv[narg],"-digits",2) == 0 ){
         ndig = strtol( argv[++narg] , NULL , 10 ) ;
         if( ndig < 1 ) ERROR_exit("-digits value must be > 0") ;
         continue ;
      }
      
      if( strncmp(argv[narg],"-quiet",5) == 0 ){
         quiet = 1 ;
         continue ;
      }
      if( strncmp(argv[narg],"-seed",5) == 0 ){
         seed = strtol( argv[++narg] , NULL , 10 ) ;
         continue ;
      }
      if( strncmp(argv[narg],"-sseed",5) == 0 ){
         char *sseed=NULL;
         int kk;
         if (narg+1>= argc) ERROR_exit("Need argument after -sseed\n");
         sseed=argv[++narg];
         for (kk=0;kk<strlen(sseed);++kk) 
            seed += (int)(sseed[kk]) ;
         continue ;
      }
      if( strncmp(argv[narg],"-root",5) == 0 ){
         strcpy(root,argv[++narg]) ;
         continue ;
      }
      
      if( strncmp(argv[narg],"-column",4) == 0 ){
         col = 1 ; comma = 0 ; sep = ' ' ;
         continue ;
      }
      
      if( strncmp(argv[narg],"-sep",4) == 0 ){   /* 02 Mar 2007 [rickr] */
         sep = argv[++narg][0] ;
         if( !isprint(sep) )
           WARNING_message("-sep character is not printable?!") ;
         else if( argv[narg][1] != '\0' )
           WARNING_message("-sep string '%s' has more than one character!",argv[narg]) ;
         continue ;
      }

      if( strncmp(argv[narg],"-suffix",4) == 0 ){
         strcpy(suffix,argv[++narg]) ;
         continue ;
      }

      if( strncmp(argv[narg],"-scale",4) == 0 ){
         sclfac = strtod(argv[++narg],NULL) ;
         continue ;
      }

      if( strncmp(argv[narg],"-comma",4) == 0 ||
          strncmp(argv[narg],"-,",2) == 0 ){
        comma = 1 ; sep = ',' ; col = 0 ; continue ;
      }

      if( strncmp(argv[narg],"-skipnmodm",10) == 0 ){
         skipn = strtol(argv[++narg],NULL,10) ;
         skipm = strtol(argv[++narg],NULL,10) ;
         continue ;
      }


      if( strncmp(argv[narg],"-",1) == 0 ){
         fprintf( stderr , "unknown switch %s\n" , argv[narg] ) ;
         exit(1) ;
      }

   /*** numbers ***/

      if( bot < 0 ){
         bot = strtol( argv[narg] , NULL , 10 ) ;
         if( bot < 0 ){
            fprintf( stderr , "illegal value of bot %d\n" , bot ) ;
            exit(1) ;
         }
         continue ;
      }

      if( top < 0 ){
         top = strtol( argv[narg] , NULL , 10 ) ;
         if( top < 0 ){
            fprintf( stderr , "illegal value of top %d\n" , top ) ;
            exit(1) ;
         }
         continue ;
      }

      if( step < 0 ){
         if( argv[narg][0] == 'R' || argv[narg][0] == 'r' ){
            rando_count = 1 ;
            rando_num   = strtol( argv[narg]+1 , NULL , 10 ) ;
            if( rando_num <= 0 ){
               fprintf( stderr , "illegal value of random count %d\n" , rando_num ) ;
               exit(1) ;
            }
            continue ;
         } else if( argv[narg][0] == 'S' || argv[narg][0] == 's' ){
            rando_count = 2 ;
            if (strlen(argv[narg]) > 1) {
               rando_num   = strtol( argv[narg]+1 , NULL , 10 ) ;
               if( rando_num <= 0 ){
                  fprintf( stderr , "illegal value of shuffle count %d\n" , rando_num ) ;
                  exit(1) ;
               }
            } else {
               rando_num   = -1;
            }
            continue ;
         }
         step = strtol( argv[narg] , NULL , 10 ) ;
         if( step <= 0 ){
            fprintf( stderr , "illegal value of step %d\n" , step ) ;
            exit(1) ;
         }
         continue ;
      }

      fprintf( stderr , "too many arguments: %s\n" , argv[narg] ) ;
      exit(1) ;

   } while ( ++narg < argc ) ;

/*** set up to iterate ***/

   if( step <= 0 ) step = 1 ;

   if (col == 0) {
      if( sclfac == 0.0 ) sprintf( fmt , "%%s%%0%dd%%s" , ndig ) ;
      else                strcpy ( fmt , " %s%g%s" ) ;
      if( isspace(sep) )  strcat ( fmt," ") ;
   } else {
      if( sclfac == 0.0 ) sprintf( fmt , " %%s%%0%dd%%s\n" , ndig ) ;
      else                strcpy ( fmt , " %s%g%s\n" ) ;
   }
/*** iterate ***/
   /* fprintf(stderr,"bot=%d, top=%d, step=%d\n", bot, top, step); */
   
   if( rando_count == 0){
      if( bot <= top ){
         for( ii=bot ; ii <= top ; ii += step ) {
            if(skipm) {
              skipout = ii%skipm;
              if(skipout==skipn) continue;
            }
            /* if (ii==top) suffix[0] = '\0'; */  /* ZSS Dec 06 */
            if( sclfac == 0.0 ) printf( fmt , root , ii , suffix ) ;
            else                printf( fmt , root , sclfac*ii , suffix ) ;
            if( ii <= top-step && !isspace(sep) ) printf("%c",sep) ;
         }
      } else {
         for( ii=bot ; ii >= top ; ii -= step ) {
            if(skipm) {
              skipout = ii%skipm;
              if(skipout==skipn)
                 continue;
            }

            /* if (ii==top) suffix[0] = '\0'; */  /* ZSS Dec 06 */
            if( sclfac == 0.0 ) printf( fmt , root , ii , suffix ) ;
            else                printf( fmt , root , sclfac*ii , suffix ) ;
            if( ii >= top+step && !isspace(sep) ) printf("%c",sep) ;
         }
      }
   } else if (rando_count == 1) {
      for( ii=0 ; ii < rando_num ; ii++ ){
         iout = ranco( bot , top, seed) ;
         /* if (ii==rando_num-1) suffix[0]='\0'; */
         if( sclfac == 0.0 ) printf( fmt , root , iout , suffix ) ;
         else                printf( fmt , root , sclfac*iout , suffix ) ;
         if( ii < rando_num-1 && !isspace(sep) ) printf("%c",sep) ;
      }
   } else if (rando_count == 2) {
      int nmax, *ir = z_rand_order(bot, top, seed);
      if (top < bot) nmax = bot-top+1;
      else nmax = top-bot+1;
      if (rando_num == -1) rando_num = nmax;
      if (!quiet && nmax < rando_num) {
         fprintf(stderr,
            "Warning: "
            "requested %d numbers in a shuffled sequence of %d unique values.\n"
            "         Sequence will repeat until %d values are output.\n",
            rando_num, nmax, rando_num);
      }
      if (ir) {
         for( ii=0 ; ii < rando_num ; ii++ ){
            iout = ir[ii%nmax] ;
            /* if (ii==rando_num-1) suffix[0]='\0'; */
            if( sclfac == 0.0 ) printf( fmt , root , iout , suffix ) ;
            else                printf( fmt , root , sclfac*iout , suffix ) ;
            if( ii < rando_num-1 && !isspace(sep) ) printf("%c",sep) ;
         }
         free(ir); ir = NULL;
      } else {
         fprintf(stderr,"Failure to plan!\n");
      }
   } else {
      fprintf(stderr,"Should not be here!\n");
      exit(1);
   }

   if( !col ) printf( "\n" ) ;
   exit(0) ;
}

int ranco( int bot , int top , long int seed)
{
   static int first = 1 ;
   int ir , ii ;
   double dr ;

   if( first ){
      if (seed) srand48(seed);
      else      srand48((long)time(NULL)+(long)getpid()) ;
      dr = drand48() ;
      ir = (int)(dr*100) ;
      for( ii=0 ; ii < ir ; ii++ ) dr = drand48() ;  /* warmup */
      first = 0 ;
   }

   ir = bot + (top-bot+0.999999)*drand48() ;
   return ir ;
}
