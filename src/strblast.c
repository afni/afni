#include "mrilib.h"

int suck_file( char *fname , char **fbuf ) ;

int main( int argc , char * argv[] )
{
   int nbuf , ntarg , ii,jj, nfind , ff ;
   char *fbuf , *targ ;
   char *jstr="AFNI-rules-" ; int njstr=strlen(jstr) ;

   int   nfname=0 ;    /* for globbing */
   char **fname=NULL ;

   /* help? */

   if( argc < 3 ){
      printf("Usage: strblast targetstring filename ...\n"
             "Finds exact copies of the target string in each of\n"
             "the input files, and replaces all characters with\n"
             "some junk string.\n"
             "Example:\n"
             "  strings I.001 | more # see if Subject Name is present\n"
             "  strblast 'Subject Name' I.*\n"
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

   /* load the target */

   targ = argv[1] ; ntarg = strlen(targ) ;
   if( ntarg < 1 ){
      fprintf(stderr,"** Can't enter an empty target string!\n") ;
      exit(1) ;
   }
   if( ntarg < njstr ){ jstr = "x" ; njstr = 1 ; }

   /* get input filenames */

   MCW_warn_expand(1) ;
   MCW_file_expand( argc-2 , argv+2 , &nfname , &fname ) ;
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
               for(      ; jj < ntarg ; jj++ ) fbuf[ii+jj] = 'x' ;
            }
            ii += ntarg-1 ;
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
