#include "mrilib.h"

/*---------------------------------------------------------------------------
  01 Feb 2001: Program to print out the value of any given attribute
  from an AFNI header.
-----------------------------------------------------------------------------*/

void atr_print( ATR_any * atr ) ;

static int do_all  = 0 ;
static int do_name = 0 ;

int main( int argc , char * argv[] )
{
   int nopt=1 , ia ;
   THD_3dim_dataset * dset ;
   char * aname ;
   ATR_any * atr ;

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
            ) ;
      exit(0) ;
   }

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-all") == 0 ){
         do_all = do_name = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-name") == 0 ){
         do_name = 1 ;
         nopt++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[nopt]) ; exit(1) ;
   }

   if( !do_all ) aname = argv[nopt++] ;

   dset  = THD_open_one_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   if( !do_all ){
      atr = THD_find_atr( dset->dblk , aname ) ;
      if( atr == NULL ) exit(1) ;                  /* failure */
      atr_print( atr ) ;
      exit(0) ;
   }

   for( ia=0 ; ia < dset->dblk->natr ; ia++ ){
      atr = &(dset->dblk->atr[ia]) ;
      atr_print(atr) ;
   }

   exit(0) ;
}

/*----------------------------------------------------------------------*/

void atr_print( ATR_any * atr )
{
   int ii ;

   switch( atr->type ){

      default:
         fprintf(stderr,"*** Illegal attribute type found: %d\n",atr->type);
      exit(1) ;

      case ATR_FLOAT_TYPE:{
         ATR_float * aa = (ATR_float *) atr ;
         if( do_name ) printf("%s = ",aa->name) ;
         for( ii=0 ; ii < aa->nfl ; ii++ )
            printf("%s ",MV_format_fval(aa->fl[ii])) ;
         printf("\n") ;
      }
      return ;

      case ATR_INT_TYPE:{
         ATR_int * aa = (ATR_int *) atr ;
         if( do_name ) printf("%s = ",aa->name) ;
         for( ii=0 ; ii < aa->nin ; ii++ )
            printf("%d ",aa->in[ii]) ;
         printf("\n") ;
      }
      return ;

      case ATR_STRING_TYPE:{
         ATR_string * aa = (ATR_string *) atr ;
         char *str = (char *)malloc(sizeof(char)*(aa->nch+1)) ;
         char *eee ;
         memcpy(str,aa->ch,aa->nch) ; str[aa->nch] = '\0' ;
         eee = tross_Expand_String(str) ;
         if( do_name ) printf("%s = ",aa->name) ;
         printf("%s\n",eee) ;
      }
      return ;
   }
}
