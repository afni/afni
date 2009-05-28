#include "mrilib.h"

/*----------------------------------------------------------------------
   history:

   ...         [rwcox]
     - Initial Version(s)

   24 Mar 2005 [rickr]
     - added options: -help, -new_char, -new_string, -unescape
  ----------------------------------------------------------------------
*/

   
void help_n_exit( void );
int  suck_file( char *fname , char **fbuf ) ;

int main( int argc , char * argv[] )
{
   int nbuf , ntarg , ii,jj, nfind , ff ;
   char *fbuf , *targ ;
   char *jstr=strdup("AFNI-rules-") ; int njstr=strlen(jstr) ;

   int   ac, unesc = 0; /* for -unescape option    24 Mar 2005 [rickr] */
   int   use_newstr = 0;
   char  newchar = 'x';

   int   nfname=0 ;    /* for globbing */
   char **fname=NULL ;

   /* help? */

   if( argc < 3 ) help_n_exit();

   /* Check for arguments.  If we don't get an exact match, continue, */
   /* allowing the user to start a target string with '-'.            */
   for( ac = 1; ac < argc && argv[ac][0] == '-'; ac++ ){
      if( strcmp(argv[ac], "-help") == 0 )
         help_n_exit();

      if( strcmp(argv[ac], "-new_char") == 0 ){
         ac++;
         if( ac >= argc ){
            fprintf(stderr,"** -new_char option requires an argument\n");
            exit(1);
         }
         
         newchar = argv[ac][0];
      }

      if( strcmp(argv[ac], "-new_string") == 0 ){
         ac++;
         if( ac >= argc ){
            fprintf(stderr,"** -new_string option requires an argument\n");
            exit(1);
         }
         
         jstr = strdup(argv[ac]);
         njstr = strlen(jstr);
         use_newstr = 1;
      }

      if( strcmp(argv[ac], "-unescape") == 0 )
         unesc = 1;
   }

   if( ac > argc-2 ){
      fprintf(stderr,"** missing target string or input files\n");
      fprintf(stderr,"   (please see 'strblast -help')\n");
      exit(1);
   }

   machdep() ;

   /* load the target */

   targ = argv[ac] ; ntarg = strlen(targ) ; ac++ ;
   if( ntarg < 1 ){
      fprintf(stderr,"** Can't enter an empty target string!\n") ;
      exit(1) ;
   }
   if( unesc ){ /* let's leave argv alone, so dup the string */
      char * tnew = strdup(targ);
      if( !tnew ){ fprintf(stderr,"** cannot dup targetstring?!\n"); exit(1); }
      for(ii=0, jj = 0; ii<ntarg; ii++, jj++){
              if(tnew[ii] == '\\' && tnew[ii+1] == 't'){tnew[jj] = '\t'; ii++;}
         else if(tnew[ii] == '\\' && tnew[ii+1] == 'n'){tnew[jj] = '\n'; ii++;}
         else if(tnew[ii] == '\\' && tnew[ii+1] == 'r'){tnew[jj] = '\r'; ii++;}
         else if(ii > jj) tnew[jj] = tnew[ii];
      }
      tnew[jj] = '\0';  /* and terminate */

      /* now for the ol' switcheroo... */
      targ = tnew;  ntarg = jj;
   }

   /* if the replacement string is too long, truncate it */
   if( ntarg < njstr ){
      if( use_newstr ){ jstr[ntarg] = '\0' ; njstr = ntarg; }
      else            { jstr[0] = newchar  ; njstr = 1;     }
   }

   /* get input filenames */

   MCW_warn_expand(1) ;
   MCW_file_expand( argc-ac , argv+ac , &nfname , &fname ) ;
   MCW_warn_expand(0) ;
   if( nfname == 0 ){
      fprintf(stderr,"** No files found from command line!\n") ;
      exit(1) ;
   }

   /* loop over files */

   for( ff=0 ; ff < nfname ; ff++ ){

      /* read it all into memory */

      fbuf = NULL ;
      nbuf = suck_file( fname[ff] , &fbuf ) ;
      if( nbuf < ntarg || fbuf == NULL ){
         fprintf(stderr,"** Can't read input file %s\n",fname[ff]) ;
         if( fbuf != NULL ) free(fbuf) ;
         continue ;
      }

      /* scan for start character */

      for( nfind=ii=0 ; ii < nbuf-ntarg ; ii++ ){

         if( fbuf[ii] == targ[0] ){  /* if find it, check rest of string */

            for( jj=1; jj < ntarg && fbuf[ii+jj]==targ[jj] ; jj++ ) ; /* nada */

            if( jj == ntarg ){  /* found it */
               nfind++ ;
               for( jj=0 ; jj < njstr ; jj++ ) fbuf[ii+jj] = jstr[jj] ;
               for(      ; jj < ntarg ; jj++ ) fbuf[ii+jj] = newchar ;
               /* only if found, noted by R Notestine  28 May 2009 [rickr] */
               ii += ntarg-1 ; /* increment past found word, shy by 1 */
            }
         }
      }

      if( nfind > 0 ){
         FILE *fp ;
         fprintf(stderr,"++ Found %d copies of target %s in file %s\n",
                 nfind,targ,fname[ff] ) ;
         fp = fopen( fname[ff] , "wb" ) ;
         if( fp == NULL ){
            fprintf(stderr,"** Can't open file %s for output!\n",fname[ff]) ;
            exit(1) ;
         }
         fwrite( fbuf , 1 , nbuf , fp ) ; fclose(fp) ;
      } else {
         fprintf(stderr,"++ Found no copies of target %s in file %s\n",
                 targ , fname[ff] ) ;
      }

      free(fbuf) ;

   } /* end of loop over files */

   exit(0) ;
}

/*------------------------------------------------------------------*/

int suck_file( char *fname , char **fbuf )
{
   int len , fd , ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' || fbuf == NULL ) return 0 ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return 0 ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   if( buf == NULL ) return 0 ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return 0 ;

   ii = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf) ; return 0; }
   *fbuf = buf ; return ii ;
}

void help_n_exit( void )
{
   printf("Usage: strblast [options] TARGETSTRING filename ...\n"
          "Finds exact copies of the target string in each of\n"
          "the input files, and replaces all characters with\n"
          "some junk string.\n"
          "\n"
          "options:\n"
          "\n"
          "  -help              : show this help\n"
          "\n"
          "  -new_char CHAR     : replace TARGETSTRING with CHAR (repeated)\n"
          "\n"
          "      This option is used to specify what TARGETSTRING is\n"
          "      replaced with.  In this case, replace it with repeated\n"
          "      copies of the character CHAR.\n"
          "\n"
          "  -new_string STRING : replace TARGETSTRING with STRING\n"
          "\n"
          "      This option is used to specify what TARGETSTRING is\n"
          "      replaced with.  In this case, replace it with the string\n"
          "      STRING.  If STRING is not long enough, then CHAR from the\n"
          "      -new_char option will be used to complete the overwrite\n"
          "      (or the character 'x', by default).\n"
          "\n"
          "  -unescape          : parse TARGETSTRING for escaped characters\n"
          "                       (includes '\\t', '\\n', '\\r')\n"
          "\n"
          "      If this option is given, strblast will parse TARGETSTRING\n"
          "      replacing any escaped characters with their encoded ASCII\n"
          "      values.\n"
          "\n"
          "Examples:\n"
          "  strings I.001 | more # see if Subject Name is present\n"
          "  strblast 'Subject Name' I.*\n"
          "\n"
          "  strblast -unescape \"END OF LINE\\n\"       infile.txt\n"
          "  strblast -new_char \" \" \"BAD STRING\"      infile.txt\n"
          "  strblast -new_string \"GOOD\" \"BAD STRING\" infile.txt\n"
          "\n"
          "Notes and Warnings:\n"
          "  * strblast will modify the input files irreversibly!\n"
          "      You might want to test if they are still usable.\n"
          "  * strblast reads files into memory to operate on them.\n"
          "      If the file is too big to fit in memory, strblast\n"
          "      will fail.\n"
          "  * strblast  will do internal wildcard expansion, so\n"
          "      if there are too many input files for your shell to\n"
          "      handle, you can do something like\n"
          "         strblast 'Subject Name' 'I.*'\n"
          "      and strblast will expand the 'I.*' wildcard for you.\n"
         ) ;
   exit(0) ;
}
