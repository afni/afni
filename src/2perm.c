#include "mrilib.h"

int ** make_two_random_subsets( int bot , int top , int n1 , int n2 )
{
   int num=top-bot+1 , jj,nn , bad ;
   int *ar1 , *ar2 , *all , **arr ;

   if( num < 2 || n1 < 1 || n2 < 1 || n1+n2 > num ) return NULL ;

   arr = (int **)malloc(sizeof(int *)*2);
   ar1 = (int * )malloc(sizeof(int)*n1) ; arr[0] = ar1 ;
   ar2 = (int * )malloc(sizeof(int)*n2) ; arr[1] = ar2 ;
   all = (int * )malloc(sizeof(int)*num);
   for( jj=0 ; jj < num ; jj++ ) all[jj] = bot+jj ;

   bad = bot - 666 ;
   for( nn=0 ; nn < n1 ; nn++ ){
     while(1){
       jj = lrand48() % num ;
       if( all[jj] >= bot ){
         ar1[nn] = all[jj] ; all[jj] = bad ; break ;
       }
     }
   }

   if( n1+n2 == num ){  /* all the rest into ar2 */
     for( nn=jj=0 ; jj < num ; jj++ ){
       if( all[jj] >= bot ) ar2[nn++] = all[jj] ;
     }
   } else {             /* random subset of what's left */
     for( nn=0 ; nn < n2 ; nn++ ){
       while(1){
         jj = lrand48() % num ;
         if( all[jj] >= bot ){
           ar2[nn] = all[jj] ; all[jj] = bad ; break ;
         }
       }
     }
   }

   free(all) ; qsort_int(n1,ar1) ; qsort_int(n2,ar2) ; return arr ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int jj , bot , top , n1 , n2 , *ar1 , *ar2 , **arr , iarg , docomma=0 ;
   char *prefix = "AFNIroolz" , *fnam ; FILE *fp ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 2perm [-prefix PPP] [-comma] bot top [n1 n2]\n"
       "\n"
       "This program creates 2 random non-overlapping subsets of the set of\n"
       "integers from 'bot' to 'top' (inclusive).  The first subset is of\n"
       "length 'n1' and the second of length 'n2'.  If those values are not\n"
       "given, then equal size subsets of length (top-bot+1)/2 are used.\n"
       "\n"
       "This program is intended for use in various simulation and/or\n"
       "randomization scripts, or for amusement/hilarity.\n"
       "\n"
       "OPTIONS:\n"
       "========\n"
       " -prefix PPP == Two output files are created, with names PPP_A and PPP_B,\n"
       "                where 'PPP' is the given prefix.  If no '-prefix' option\n"
       "                is given, then the string 'AFNIroolz' will be used.\n"
       "                ++ Each file is a single column of numbers.\n"
       "                ++ Note that the filenames do NOT end in '.1D'.\n"
       "\n"
       " -comma      == Write each file as a single row of comma-separated numbers.\n"
       "\n"
       "EXAMPLE:\n"
       "========\n"
       "This illustration shows the purpose of 2perm -- for use in permutation\n"
       "and/or randomization tests of statistical significance and power.\n"
       "Given a dataset with 100 sub-bricks (indexed 0..99), split it into two\n"
       "random halves and do a 2-sample t-test between them.\n"
       "\n"
       "  2perm -prefix Q50 0 99\n"
       "  3dttest++ -setA dataset+orig\"[1dcat Q50_A]\" \\\n"
       "            -setB dataset+orig\"[1dcat Q50_B]\" \\\n"
       "            -no1sam -prefix Q50\n"
       "  \\rm -f Q50_?\n"
       "\n"
       "Alternatively:\n"
       "\n"
       "  2perm -prefix Q50 -comma 0 99\n"
       "  3dttest++ -setA dataset+orig\"[`cat Q50_A`]\" \\\n"
       "            -setB dataset+orig\"[`cat Q50_B`]\" \\\n"
       "            -no1sam -prefix Q50\n"
       "  \\rm -f Q50_?\n"
       "\n"
       "Note the combined use of the double quote \" and backward quote `\n"
       "shell operators in this second approach.\n"
       "\n"
       "AUTHOR: (no one want to admit they wrote this trivial code).\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("2perm: need an argument after option %s",argv[iarg-1]) ;
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("2perm: illegal prefix") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-comma") == 0 ){
       docomma = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   }

   fnam = (char *)malloc(sizeof(char)*(strlen(prefix)+8)) ;

   if( iarg+2 > argc )
     ERROR_exit("2perm: need at least 2 arguments for 'bot' and 'top'") ;

   bot = (int)strtod(argv[iarg++],NULL) ;
   top = (int)strtod(argv[iarg++],NULL) ;
   if( bot >= top )
     ERROR_exit("2perm: bot=%d is not less than top=%d",bot,top) ;

   if( iarg+2 <= argc ){
     n1 = (int)strtod(argv[iarg++],NULL) ;
     n2 = (int)strtod(argv[iarg++],NULL) ;
     if( n1 < 1 || n2 < 1 )
       ERROR_exit("2perm: n1=%d and/or n2=%d are not both positive",n1,n2) ;
     if( n1+n2 > top-bot+1 )
       ERROR_exit("2perm: n1=%d + n2=%d is bigger than number of points from %d to %d",
                  n1,n2 , bot,top ) ;
   } else {
     n2 = n1 = (top-bot+1)/2 ;
   }

   arr = make_two_random_subsets(bot,top,n1,n2) ;
   if( arr == NULL ) ERROR_exit("2perm: bad inputs -- can't create subsets") ;
   ar1 = arr[0] ; ar2 = arr[1] ;

   sprintf(fnam,"%s_A",prefix) ;
   fp = fopen(fnam,"w") ;
   if( fp == NULL ) ERROR_exit("2perm: Can't open file %s for output!",fnam) ;
   if( docomma ){
     for( jj=0 ; jj < n1-1 ; jj++ ) fprintf(fp,"%d,",ar1[jj]) ;
     fprintf(fp,"%d\n",ar1[n1-1]) ;
   } else {
     for( jj=0 ; jj < n1 ; jj++ ) fprintf(fp,"%d\n",ar1[jj]) ;
   }
   fclose(fp) ;

   sprintf(fnam,"%s_B",prefix) ;
   fp = fopen(fnam,"w") ;
   if( fp == NULL ) ERROR_exit("2perm: Can't open file %s for output!",fnam) ;
   if( docomma ){
     for( jj=0 ; jj < n2-1 ; jj++ ) fprintf(fp,"%d,",ar2[jj]) ;
     fprintf(fp,"%d\n",ar2[n2-1]) ;
   } else {
     for( jj=0 ; jj < n2 ; jj++ ) fprintf(fp,"%d\n",ar2[jj]) ;
   }
   fclose(fp) ;

   exit(0) ;
}
