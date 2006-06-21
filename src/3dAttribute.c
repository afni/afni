#include "mrilib.h"

/*---------------------------------------------------------------------------
  01 Feb 2001: Program to print out the value of any given attribute
  from an AFNI header.
-----------------------------------------------------------------------------*/


static int do_all  = 0 ;
static int do_name = 0 ;

int main( int argc , char * argv[] )
{
   int nopt=1 , ia ;
   THD_3dim_dataset * dset ;
   char * aname ;
   ATR_any * atr ;
   char *ssep=NULL, *spsep = NULL;
   char ssep_def[] = {"~"};
   char quote = '\0';
   
   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAttribute [options] aname dset\n"
             "Prints (to stdout) the value of the attribute 'aname' from\n"
             "the header of dataset 'dset'.  If the attribute doesn't exist,\n"
             "prints nothing and sets the exit status to 1.\n"
             "\n"
             "Options:\n"
             "  -name = Include attribute name in printout\n"
             "  -all  = Print all attributes [don't put aname on command line]\n"
             "          Also implies '-name'.  Attributes print in whatever order\n"
             "          they are in the .HEAD file, one per line.  You may want\n"
             "          to do '3dAttribute -all elvis+orig | sort' to get them\n"
             "          in alphabetical order.\n"
             "  Special options for string attributes:\n"
             "    -ssep SSEP    Use string SSEP as a separator between strings for\n"
             "                  multiple sub-bricks. The default is '~', which is what\n"
             "                  is used internally in AFNI's .HEAD file. For tcsh,\n"
             "                  I recommend ' ' which makes parsing easy, assuming each\n"
             "                  individual string contains no spaces to begin with.\n" 
             "                  Try -ssep 'NUM'\n"
             "    -spsep SPSEP  Use string SPSEP to replace blank space in string \n"
             "                  attributes.\n"
             "    -quote        Use single quote around each string.\n"
             "    Examples:\n"
             "       3dAttribute -quote -ssep ' '  BRICK_LABS SomeStatDset+tlrc.BRIK\n"
             "       3dAttribute -quote -ssep 'NUM' -spsep '+' BRICK_LABS SomeStatDset+tlrc.BRIK\n"
            ) ;
      exit(0) ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-all") == 0 ){
         do_all = do_name = 1 ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-quote") == 0 ){
         quote = '\'' ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-ssep") == 0 ){
         nopt++ ;
         if (nopt >= argc) {
            fprintf(stderr,"*** Need string after -ssep\n");
            exit(1) ;
         }
         ssep = argv[nopt] ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-spsep") == 0 ){
         nopt++ ;
         if (nopt >= argc) {
            fprintf(stderr,"*** Need string after -spsep\n");
            exit(1) ;
         }
         spsep = argv[nopt] ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-name") == 0 ){
         do_name = 1 ;
         nopt++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[nopt]) ; exit(1) ;
   }

   if (!ssep) ssep = ssep_def;
   if( !do_all ) aname = argv[nopt++] ;

   dset  = THD_open_one_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   if( !do_all ){
      atr = THD_find_atr( dset->dblk , aname ) ;
      if( atr == NULL ) exit(1) ;                  /* failure */
      atr_print( atr, ssep, spsep, quote, do_name ) ;
      exit(0) ;
   }

   for( ia=0 ; ia < dset->dblk->natr ; ia++ ){
      atr = &(dset->dblk->atr[ia]) ;
      atr_print(atr, ssep, spsep, quote, do_name) ;
   }

   exit(0) ;
}

/*----------------------------------------------------------------------*/

