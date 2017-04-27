#include <stdio.h>
#include <unistd.h>

#undef  LF
#undef  CR

#define LF '\n'    /* Unix line ender      = ctrl-J = ASCII 0x0A */
#define CR '\r'    /* Microsoft line ender = ctrl-M = ASCII 0x0D */

/*----------------------------------------------------------------------------*/
/* Allow user to skip the afni_fgets() function, probably for speedup.
   afni_fgets() is about 4-5 times slower than fgets(), at least on my Mac.
   Doesn't usually matter, since interpreting the text takes longer than
   reading it in most case.  (Also, cf machdep.h)
*//*--------------------------------------------------------------------------*/

static int use_fgets = 0 ;
void afni_fgets_setskip( int s ){ use_fgets = s ; }

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

   if( use_fgets || isatty(fileno(fp)) ) return fgets(buf,nbuf,fp) ;

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
       if( (cin==CR && qin!=LF) || (cin==LF && qin!=CR) ) ungetc(qin,fp) ;
       break ;                      /* in either case, am done with loop */
     }
   } while( nin < nbuf-1 ) ;    /* don't run off the end of the world */
   if (nin >= nbuf -1) {
      /* no current include for ERROR_message    24 Sep 2015 [rickr] */
      fprintf(stderr, "**Error: Line too long for buffer of %d chars.", nbuf);
      return NULL;
   }
   if( nin == 0 ) return NULL ; /* nothing was read */

   buf[nin] = '\0' ;            /* Schwarznegger the string */
   return buf ;
}

/*========================= main program to test speed of afni_fgets() =======*/
#if 0
#include "mrilib.h"

/* test speed of afni_fgets() */

int main( int argc , char *argv[] )
{
   char buf[99999] , *bb ; FILE *fp ; int iarg=1 ;

   machdep() ;  /* will set fgets usage from environment */

   while( iarg < argc ){  /* read all files */

     fp = fopen(argv[iarg++],"r") ; if( fp == NULL ) continue ;

     do{ bb = afni_fgets( buf , 99999 , fp ) ; } while( bb != NULL ) ;

     fclose(fp) ;
   }

   exit(0) ;
}
#endif
