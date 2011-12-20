#include <stdio.h>

#undef  LF
#undef  CR

#define LF '\n'
#define CR '\r'

char * afni_fgets( char *buf , int nbuf , FILE *fp )
{
   int nin=0 , cin , qin ;

   if( buf == NULL || nbuf <= 1 || fp == NULL ) return NULL ;

   do{
     cin = fgetc(fp) ;
     if( cin == EOF ) break ;

     if( cin != CR ) buf[nin++] = (char)cin ;  /* copy input to output */
     else            buf[nin++] = (char)LF  ;  /* but convert CR to LF */

     if( cin == CR || cin == LF ){  /* end of line */
       qin = fgetc(fp) ;  /* check next character */
                          /* if have a CR-LF or LF-CR combo, */
                          /* skip next char, otherwise push it back */
       if( (cin == CR && qin != LF) || (cin == LF && qin != CR) ) ungetc(qin,fp) ;
       break ;  /* in either case, am done with loop */
     }
   } while( nin < nbuf-1 ) ;

   if( nin == 0 ) return NULL ;  /* nothing was read */

   buf[nin] = '\0' ;  /* terminator */
   return buf ;
}
