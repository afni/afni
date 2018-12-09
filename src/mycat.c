#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#undef  LF
#undef  CR

#define LF '\n'    /* Unix line ender      = ctrl-J = ASCII 0x0A */
#define CR '\r'    /* Microsoft line ender = ctrl-M = ASCII 0x0D */

/*----------------------------------------------------------------------------*/
/*! Like system fgets(), but allows LF, CR, CR+LF, and LF+CR as end of line
    markers.  Converts all of them to a single LF character, so that the
    results are as if the input was an honest-to-Allah Unix file, instead
    of some bastard child of Microsoft.  (Not that I have any prejudices.)
*//*--------------------------------------------------------------------------*/

char * afni_fgets( char *buf , int nbuf , FILE *fp )
{
   int nin=0 , cin , qin ;

   if( buf == NULL || nbuf <= 1 || fp == NULL ) return NULL ;

   /* use system fgets() if ordered to, or if reading from a terminal */

   if( isatty(fileno(fp)) ) return fgets(buf,nbuf,fp) ;

   /* read characters one at a time and process them */

   do{
     cin = getc(fp) ;           /* read next char */
     if( cin == EOF ) break ;   /* nuthin ==> quit */

     /* copy input to output, but convert CR to LF */

     if( cin != CR ) buf[nin++] = (char)cin ;
     else            buf[nin++] = (char)LF  ;  /* Microsoft coverup */

     if( cin == CR || cin == LF ){  /* end of line */
       qin = getc(fp) ;             /* check next character */
                                    /* if have a CR+LF or LF+CR combo, */
                                    /* skip next char, otherwise push it back */
       if( (cin==CR && qin!=LF) || (cin ==LF && qin!= CR) ) ungetc(qin,fp) ;
       break ;                      /* in either case, am done with loop */
     }
   } while( nin < nbuf-1 ) ;    /* don't run off the end of the world */
   if (nin >= nbuf -1) {
      fprintf(stderr,"** ERROR: Line too long for buffer of %d chars.\n", nbuf);
      return NULL;
   }
   if( nin == 0 ) return NULL ; /* nothing was read */

   buf[nin] = '\0' ;            /* Schwarznegger the string */
   return buf ;
}

/*----------------------------------------------------------------------------*/

#undef  NBUF
#define NBUF 131072
static char buf[NBUF] ;

int main( int argc , char *argv[] )
{
   char *bb ; FILE *fp ; int iarg=1 ;

   if( argc == 1 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
            "mycat fileA ...\n"
            "\n"
            "Copies text files to stdout, like the system 'cat', but with changes:\n"
            "* To copy stdin, you must use '-' for a filename\n"
            "* Microsoft end-of-line characters are changed to Unix format\n"
            "* Because of the above, mycat should only be used with text files!\n"
            "\n"
     ) ;
     exit(0) ;
   }

   while( iarg < argc ){

     if( strcmp(argv[iarg],"-") == 0 ){
       fp = stdin ; iarg++ ;
     } else {
       fp = fopen(argv[iarg++],"r") ; if( fp == NULL ) continue ;
     }

     while(1){
       bb = afni_fgets( buf , NBUF , fp ) ;
       if( bb == NULL ) break ;
       fwrite(buf,1,strlen(buf),stdout) ;
     }

     if( fp != stdin ) fclose(fp) ;

   }

   exit(0) ;
}
