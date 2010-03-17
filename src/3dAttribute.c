#include "mrilib.h"

/*---------------------------------------------------------------------------
  01 Feb 2001: Program to print out the value of any given attribute
  from an AFNI header.
-----------------------------------------------------------------------------*/


static int do_all  = 0 ;
static int do_name = 0 ;
static int do_center = 0 ;
int main( int argc , char * argv[] )
{
   int nopt=1 , ia , HasSb=0;
   THD_3dim_dataset * dset ;
   char * aname =NULL;
   ATR_any * atr ;
   THD_fvec3 fv;
   char *ssep=NULL, *sprep = NULL, *cpt=NULL;
   char ssep_def[] = {"~"};
   char quote = '\0';
   
   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dAttribute [options] aname dset\n"
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
 "  -center = Center of volume in RAI coordinates.\n"
 "            Note that center is not itself an attribute in the \n"
 "           .HEAD file. It is calculated from other attributes.\n"
 "  Special options for string attributes:\n"
 "    -ssep SSEP    Use string SSEP as a separator between strings for\n"
 "                  multiple sub-bricks. The default is '~', which is what\n"
 "                  is used internally in AFNI's .HEAD file. For tcsh,\n"
 "                  I recommend ' ' which makes parsing easy, assuming each\n"
 "                  individual string contains no spaces to begin with.\n" 
 "                  Try -ssep 'NUM'\n"
 "    -sprep SPREP  Use string SPREP to replace blank space in string \n"
 "                  attributes.\n"
 "    -quote        Use single quote around each string.\n"
 "    Examples:\n"
 "       3dAttribute -quote -ssep ' '  BRICK_LABS SomeStatDset+tlrc.BRIK\n"
 "       3dAttribute -quote -ssep 'NUM' -sprep '+' BRICK_LABS SomeStatDset+tlrc.BRIK\n"
) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-all") == 0 ){
         do_all = do_name = 1 ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-center") == 0 ){
         do_center = 1 ;
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
      
      if( strcmp(argv[nopt],"-sprep") == 0 ){
         nopt++ ;
         if (nopt >= argc) {
            fprintf(stderr,"*** Need string after -sprep\n");
            exit(1) ;
         }
         sprep = argv[nopt] ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-name") == 0 ){
         do_name = 1 ;
         nopt++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[nopt]) ; exit(1) ;
   }

   if (!ssep) ssep = ssep_def;
   if( !do_all && !do_center) aname = argv[nopt++] ;

   /* Check to see if you have sub-brick selection */
   cpt = strstr(argv[nopt],"[") ;
   if( cpt == NULL ){
      HasSb = 0 ;
   } else if( cpt == argv[nopt] ){
      fprintf(stderr,"illegal dataset specifier: %s\n",argv[nopt]) ;
      exit(1) ;
   } else {
      HasSb = 1 ;
   }
   
   if (HasSb) {
      dset  = THD_open_dataset( argv[nopt] ) ;/* changed from open_one_ to allow 
                                                for getting sub-brick specific
                                                attributes ZSS-April 08 */
      if (ISVALID_DSET(dset)) {
         /* push hearder structure back to header attributes */
         /* NOTICE SOME ATTRIBUTES NOT LOADED INTO STRUCTS WILL VANISH */
         THD_set_dataset_attributes(dset);
      }
   } else {
      dset  = THD_open_one_dataset( argv[nopt] ) ;

      /* in either case (i.e. handle non-AFNI dsets)   16 Mar 2010 [rickr] */
      THD_set_dataset_attributes(dset);
   }
   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }
   
   if( !do_all && aname){
      atr = THD_find_atr( dset->dblk , aname ) ;
      if( atr == NULL ) {
         if (HasSb) {
            *cpt = '\0';
            /* see if attribute is present in parent dset but
            not preserved with sub-brick selection */
            dset = THD_open_one_dataset( argv[nopt] ) ; /* memory leak, 
                                                         but OK here */
            if (  ISVALID_DSET(dset) &&
                  THD_find_atr( dset->dblk , aname ) ) {
               fprintf(stderr,
                  "Failed to find attribute %s with sub-brick selection.\n"
                  "Since %s exists in %s then it is an attribute that \n"
                  "is not preserved with sub-brick selection.\n",
                  aname, aname, argv[nopt]);
            }
         }
         exit(1) ;                  /* failure */
      }
      atr_print( atr, ssep, sprep, quote, do_name ) ;
      exit(0) ;
   }

   if (do_all) {
      for( ia=0 ; ia < dset->dblk->natr ; ia++ ){
         atr = &(dset->dblk->atr[ia]) ;
         atr_print(atr, ssep, sprep, quote, do_name) ;
      }
   }

   if (do_center) {
      fv = THD_dataset_center( dset ) ;
      if (do_name) fprintf(stdout, "center = ");
      fprintf(stdout, "%f %f %f\n", fv.xyz[0], fv.xyz[1], fv.xyz[2]);
   }
   exit(0) ;
}

/*----------------------------------------------------------------------*/

