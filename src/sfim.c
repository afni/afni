/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#include "mrilib.h"

#define MAX_NAME 64

typedef struct {
      int         count ;
      char        name[MAX_NAME] ;
      MRI_IMAGE * avim , * sdim ;
} SF_interval ;

/*** global data!!! ***/

static int           SF_numint = 0 ;
static SF_interval * SF_int = NULL ;
static char          SF_bname[MAX_NAME] = "rest"  ;
static char          SF_pname[MAX_NAME] = "sfim." ;
static char          SF_iname[MAX_NAME] = "sfint" ;
static MRI_IMARR *   SF_imts = NULL ;
static int           SF_localbase = 0 ;

/*** prototypes ***/

void SFIM_getopts( int , char * argv[] ) ;
SF_interval * SFIM_load_intervals( char * ) ;
void SFIM_syntax( char * ) ;
void SFIM_write_avs() ;

/**********************************************************************/

int main( int argc , char * argv[] )
{
   int lin , kim , kbot,ktop , nx,ny , npix , ii ,
       lbase , lup,ldown ;
   MRI_IMAGE ** stat_ret ;
   MRI_IMAGE * imb ;
   float     * bar , * bav ;

   printf(
    "MCW SFIM: Stepwise Functional IMages, by RW Cox\n") ;

   if( argc < 2 ) SFIM_syntax("type sfim -help for usage details") ;
   else if( strcmp(argv[1],"-help") == 0 ) SFIM_syntax(NULL) ;

   machdep() ;

   SFIM_getopts( argc , argv ) ;

   /*----- average over each interval -----*/

   nx = SF_imts->imarr[0]->nx ;
   ny = SF_imts->imarr[0]->ny ; npix = nx * ny ;

   lin = 0 ; kbot = 0 ;
   do {
      ktop = kbot + SF_int[lin].count ;
      if( ktop > SF_imts->num ) ktop = SF_imts->num ;

      if( isalpha(SF_int[lin].name[0]) ){     /* average if a good name */
         for( kim=kbot ; kim < ktop ; kim++ )
           (void) mri_stat_seq( SF_imts->imarr[kim] ) ;

         stat_ret         = mri_stat_seq( NULL ) ;
         SF_int[lin].avim = stat_ret[0] ;
         SF_int[lin].sdim = stat_ret[1] ;
      }

      kbot = ktop ; lin ++ ;
   } while( SF_int[lin].count > 0 && kbot < SF_imts->num ) ;
   SF_numint = lin ;

   /*----- find the number of base intervals -----*/

   lbase = 0 ;
   for( lin=0 ; lin < SF_numint ; lin++ )
      if( strcmp(SF_int[lin].name,SF_bname) == 0 ) lbase++ ;

   /* no bases --> write averages out now and quit */

   if( lbase <= 0 ){
      printf("** no 'base' intervals --> task means not adjusted\n") ;
      SFIM_write_avs() ;
      exit(0) ;
   }

   /* bases yes, but not localbase --> compute global average of bases */

   if( ! SF_localbase ){
      int knum = 0 ;

      kbot = 0 ;
      for( lin=0 ; lin < SF_numint ; lin++ ){
         ktop = kbot + SF_int[lin].count ;
         if( ktop > SF_imts->num ) ktop = SF_imts->num ;
         if( strcmp(SF_int[lin].name,SF_bname) == 0 ){  /* if a base */
            for( kim=kbot ; kim < ktop ; kim++ ){
               (void) mri_stat_seq( SF_imts->imarr[kim] ) ; /* average in */
               knum ++ ;
            }
         }
         kbot = ktop ;
      }
      stat_ret = mri_stat_seq( NULL ) ;
      imb      = stat_ret[0] ;           /* average of all bases */
      mri_free( stat_ret[1] ) ;          /* don't keep st. dev.  */

      printf("** global base = average of %d images\n",knum) ;
   }

   /*----- for each non-base interval,
           subtract the relevant base average -----*/

   for( lin=0 ; lin < SF_numint ; lin++ ){
      int free_imb = 0 ;

      if( !isalpha(SF_int[lin].name[0]) ||
          strcmp(SF_int[lin].name,SF_bname) == 0 ) continue ;  /* skip this */

      if( SF_localbase ){
         for( lup=lin+1 ; lup < SF_numint ; lup++ )  /* look for a base above */
            if( strcmp(SF_int[lup].name,SF_bname) == 0 ) break ;

         for( ldown=lin-1 ; ldown >=0 ; ldown-- )    /* look for a base below */
            if( strcmp(SF_int[ldown].name,SF_bname) == 0 ) break ;

         if( ldown < 0 && lup >= SF_numint ){  /* no base?  an error! */
            fprintf(stderr,"*** can't find base above or below at lin=%d\n",lin) ;
            SFIM_syntax("INTERNAL ERROR -- should not occur!") ;
         }

         /* if only have one neighbor, use it, otherwise make average */

         if( ldown <  0         ){
            imb = SF_int[lup].avim ; free_imb = 0 ;
            printf("** local base for %s = average of %d images above\n",
                   SF_int[lin].name , SF_int[lup].count ) ;
         }
         else if( lup   >= SF_numint ){
            imb = SF_int[ldown].avim ; free_imb = 0 ;
            printf("** local base for %s = average of %d images below\n",
                   SF_int[lin].name , SF_int[ldown].count ) ;
         }
         else {
            float * bup , * bdown ;
            bup   = mri_data_pointer( SF_int[lup].avim ) ;
            bdown = mri_data_pointer( SF_int[ldown].avim ) ;
            imb   = mri_new( nx , ny , MRI_float ) ; free_imb = 1 ;
            bar   = mri_data_pointer( imb ) ;
            for( ii=0 ; ii < npix ; ii++ )
               bar[ii] = 0.5 * ( bup[ii] + bdown[ii] ) ;

            printf("** local base for %s = average of %d below, %d above\n",
                   SF_int[lin].name, SF_int[ldown].count, SF_int[lup].count ) ;
         }
      }

      /* subtract imb (base average) from current interval average */

      bar = mri_data_pointer( imb ) ;
      bav = mri_data_pointer( SF_int[lin].avim ) ;
      for( ii=0 ; ii < npix ; ii++ ) bav[ii] -= bar[ii] ;

      if( SF_localbase && free_imb ) mri_free( imb ) ;
   }

   /*----- now write the averages out -----*/

   SFIM_write_avs() ;
   exit(0) ;
}

/**********************************************************************/

void SFIM_write_avs()
{
   char name[256] ;
   int lin , ldown , tnum ;

   for( lin=0 ; lin < SF_numint ; lin++ ){
      if( !isalpha(SF_int[lin].name[0]) ||
          SF_int[lin].avim == NULL         ) continue ;  /* skip */

      tnum = 1 ;
      for( ldown=lin-1 ; ldown >=0 ; ldown-- )
         if( strcmp(SF_int[lin].name,SF_int[ldown].name) == 0 ) tnum++ ;

      sprintf( name , "%s%s.%04d" , SF_pname , SF_int[lin].name , tnum ) ;

      printf("-- writing file %s\n",name) ;
      mri_write( name , SF_int[lin].avim ) ;
   }
}

/**********************************************************************/

void SFIM_getopts( int argc , char * argv[] )
{
   int nopt = 1 ;
   int kk , nx , ny , kount ;

   /*--- do options ---*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      /** -sfint iname **/

      if( strncmp(argv[nopt],"-sfint",5) == 0 ){
         if( ++nopt >= argc ) SFIM_syntax("-sfint needs a name after it!") ;
         strcpy(SF_iname,argv[nopt]) ;
         nopt++ ; continue ;
      }

      /** -base bname **/

      if( strncmp(argv[nopt],"-base",5) == 0 ){
         if( ++nopt >= argc ) SFIM_syntax("-base needs a name after it!") ;
         strcpy(SF_bname,argv[nopt]) ;
         nopt++ ; continue ;
      }

      /** -prefix pname **/

      if( strncmp(argv[nopt],"-prefix",5) == 0 ){
         if( ++nopt >= argc ) SFIM_syntax("-prefix needs a name after it!") ;
         strcpy(SF_pname,argv[nopt]) ;
         kk = strlen(SF_pname) ;
         if( SF_pname[kk-1] != '.' ){
            SF_pname[kk]   = '.' ;
            SF_pname[kk+1] = '\0' ;
         }
         nopt++ ; continue ;
      }

      /** -localbase **/

      if( strncmp(argv[nopt],"-localbase",5) == 0 ){
         SF_localbase = 1 ;
         nopt++ ; continue ;
      }

      /** illegal option **/

      fprintf(stderr,"*** illegal command line option: %s\n",argv[nopt]) ;
      SFIM_syntax("type sfim -help for more details") ;

   }

   /*--- do images ---*/

   SF_imts = mri_read_many_files( argc-nopt , argv+nopt ) ;
   if( SF_imts == NULL ) SFIM_syntax("cannot continue without input images!") ;

   /* check images for consistency */

   nx = SF_imts->imarr[0]->nx ;
   ny = SF_imts->imarr[0]->ny ;

   for( kk=1 ; kk < SF_imts->num ; kk++ ){
      if( nx != SF_imts->imarr[kk]->nx || ny != SF_imts->imarr[kk]->ny ){
         fprintf(stderr,"*** image %d not conformant to image 0\n",kk) ;
         SFIM_syntax("cannot continue with images whose sizes differ!") ;
      }
   }

   /*--- read intervals ---*/

   SF_int = SFIM_load_intervals( SF_iname ) ;
   if( SF_int[0].count <= 0 ) SFIM_syntax("cannot read interval definitions") ;

   kount = kk = 0 ;
   do {
      kount += SF_int[kk++].count ;
   } while( SF_int[kk].count > 0 ) ;

   if( kount < SF_imts->num ){
      fprintf(stderr,"*** # images = %d but only %d images in intervals!\n",
              SF_imts->num , kount ) ;
      SFIM_syntax("must have at least as many in intervals as actual images!") ;
   }

   return ;
}

/**********************************************************************/

void SFIM_syntax( char * str )
{
   if( str != NULL ){
      fprintf(stderr,"*** %s\n",str) ;
      exit(-1) ;
   }

   printf(
    "\n"
    "Usage: sfim [options] image_files ...\n"
    "\n"
    "  + image_files are in the same format AFNI accepts\n"
    "  + options are from the following:\n"
    "\n"
    "  -sfint iname:   'iname' is the name of a file which has\n"
    "                  the interval definitions; an example is\n"
    "                    3*# 5*rest 4*A 5*rest 4*B 5*rest 4*A 5*rest\n"
    "                  which says:\n"
    "                    - ignore the 1st 3 images\n"
    "                    - take the next 5 as being in task state 'rest'\n"
    "                    - take the next 4 as being in task state 'A'\n"
    "                    and so on;\n"
    "                  task names that start with a nonalphabetic character\n"
    "                  are like the '#' above and mean 'ignore'.\n"
    "              *** the default 'iname' is 'sfint'\n"
    "\n"
    "  -base bname:    'bname' is the task state name to use as the\n"
    "                  baseline; other task states will have the mean\n"
    "                  baseline state subtracted; if there are no task\n"
    "                  states from 'iname' that match 'bname', this\n"
    "                  subtraction will not occur.\n"
    "              *** the default 'bname' is 'rest'\n"
    "\n"
    "  -localbase:     if this option is present, then each non-base\n"
    "                  task state interval has the mean of the two\n"
    "                  nearest base intervals subtracted instead of the\n"
    "                  grand mean of all the base task intervals.\n"
    "\n"
    "  -prefix pname:  'pname' is the prefix for output image filenames for\n"
    "                  all states:  the i'th interval with task state name\n"
    "                  'fred' will be writen to file 'pname.fred.i'.\n"
    "              *** the default 'pname' is 'sfim'\n"
    "\n"
    "  Output files are the base-mean-removed averages for each non-base\n"
    "  task interval, and simply the mean for each base task interval.\n"
    "  Output images are in the 'flim' (floating pt. image) format, and\n"
    "  may be converted to 16 bit shorts using the program 'ftosh'.\n"
   ) ;

   exit(0) ;
}

/**********************************************************************/

SF_interval * SFIM_load_intervals( char * fname )
{
   SF_interval * sfint ;
   FILE * fd ;
   int num_int , num_all , ic , cc ;
   char nn[MAX_NAME] ;
#define INC_SFINT 8

   if( fname == NULL || strlen(fname) == 0 ){
      fprintf(stderr,"SFIM_load_intervals illegal filename\n") ;
      exit(-1) ;
   }
   fd = fopen( fname , "r" ) ;
   if( fd == NULL ){
      fprintf(stderr,"SFIM_load_intervals cannot open filename %s\n",fname) ;
      exit(-1) ;
   }

   /* malloc -> calloc   13 Feb 2009 [lesstif patrol] */
   sfint = (SF_interval *) calloc( INC_SFINT, sizeof(SF_interval) ) ;
   if( sfint == NULL ){
     fprintf(stderr,"SFIM_load_intervals fails to malloc!\n") ;
     exit(-1) ;
   }
   num_int = 0 ;
   num_all = INC_SFINT ;

   do {
      ic = fscanf( fd , "%d*%s" , &cc , nn ) ;
      if( num_int == num_all ){
         num_all += INC_SFINT ;
         sfint    = (SF_interval *) realloc( (void *) sfint ,
                                             sizeof(SF_interval) * num_all ) ;
         if( sfint == NULL ){
            fprintf(stderr,"SFIM_load_intervals fails to realloc!\n") ;
            exit(-1) ;
         }
      }
      if( ic != 2 ){ /* bad scanning */
         sfint[num_int].count = -1 ;
         break ;
      }
      sfint[num_int].count = cc ; /* put results into interval */
      sfint[num_int].avim  = NULL ;
      sfint[num_int].sdim  = NULL ;
      strcpy( sfint[num_int].name , nn ) ;

      num_int ++ ;
   } while( cc > 0 ) ;

   fclose( fd ) ;
   return sfint ;
}
