#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

int ranco(int,int) ;

int main( int argc , char *argv[] )
{
   int ii , bot = -1 , top = -1 , step = -1 , rando_count = 0, rando_num ;
   int narg , ndig = 4 , iout ;
   static char root[6664] , fmt[128] , suffix[6664] ;
   float sclfac = 0.0 ;

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
        "* 'bot' and 'top' must not be negative; step must be positive.\n"
        "\n"
        "Options:\n"
        "  -digits n    prints numbers with 'n' digits [default=4]\n"
        "  -root rrr    prints string 'rrr' before the number [default=empty]\n"
        "  -suffix sss  prints string 'sss' after the number [default=empty]\n"
        "  -scale fff   multiplies each number by the factor 'fff';\n"
        "                 if this option is used, -digits is ignored and\n"
        "                 the floating point format '%%g' is used for output.\n"
        "                 ('fff' can be a floating point number.)\n"
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

   do {

   /*** switches ***/

      if( strncmp(argv[narg],"-digits",2) == 0 ){
         ndig = strtol( argv[++narg] , NULL , 10 ) ;
         if( ndig < 1 ){
            fprintf( stderr , "-digits illegal!\n" ) ;
            exit(1) ;
         }
         continue ;
      }

      if( strncmp(argv[narg],"-root",2) == 0 ){
         strcpy(root,argv[++narg]) ;
         continue ;
      }

      if( strncmp(argv[narg],"-suffix",3) == 0 ){
         strcpy(suffix,argv[++narg]) ;
         continue ;
      }

      if( strncmp(argv[narg],"-scale",3) == 0 ){
         sclfac = strtod(argv[++narg],NULL) ;
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

   if( sclfac == 0.0 )
      sprintf( fmt , " %%s%%0%dd%%s" , ndig ) ;
   else
      strcpy( fmt , " %s%g%s" ) ;

/*** iterate ***/

   if( ! rando_count ){
      if( bot <= top ){
         for( ii=bot ; ii <= top ; ii += step )
            if( sclfac == 0.0 )
               printf( fmt , root , ii , suffix ) ;
            else
               printf( fmt , root , sclfac*ii , suffix ) ;
      } else {
         for( ii=bot ; ii >= top ; ii -= step )
            if( sclfac == 0.0 )
               printf( fmt , root , ii , suffix ) ;
            else
               printf( fmt , root , sclfac*ii , suffix ) ;
      }
   } else {
      for( ii=0 ; ii < rando_num ; ii++ ){
         iout = ranco( bot , top ) ;
         if( sclfac == 0.0 )
            printf( fmt , root , iout , suffix ) ;
         else
            printf( fmt , root , sclfac*iout , suffix ) ;
      }
   }

   printf( "\n" ) ;
   exit(0) ;
}

int ranco( int bot , int top )
{
   static int first = 1 ;
   int ir , ii ;
   double dr ;

   if( first ){
      srand48( time(NULL) ) ;
      dr = drand48() ;
      ir = (int)(dr*100) ;
      for( ii=0 ; ii < ir ; ii++ ) dr = drand48() ;
      first = 0 ;
   }

   ir = bot + (top-bot+0.999999)*drand48() ;
   return ir ;
}
