#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define LBUF 262144

#define ISPMD(c)  ( (c)=='+' || (c)=='-' || (c)=='.' )
#define ISGOOD(c) ( isdigit(c) || ISPMD(c) )

int main( int argc , char **argv )
{
   size_t nbuf ; int ii ; char *buf ;

   buf = (char *)malloc(sizeof(char)*LBUF) ;

   if( argc > 1 && strcasecmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dAstrip < input > output\n"
            "\n"
            "This very simple program strips non-numeric characters\n"
            "from a file, so that it can be processed by other AFNI\n"
            "1d programs.  For example, if your input is\n"
            "  x=3.6 y=21.6 z=14.2\n"
            "then your output would be\n"
            "    3.6   21.6   14.2\n"
            "\n"
            "* Non-numeric characters are replaced with blanks.\n"
            "* The letter 'e' is preserved if it is preceeded\n"
            "  or followed by a numeric character.  This is\n"
            "  to allow for numbers like '1.2e-3'.\n"
            "* Numeric characters, for the purpose of this\n"
            "  program, are defined as the digits '0'..'9',\n"
            "  and '.', '+', '-'.\n"
            "* The program is simple and can easily end up leaving\n"
            "  undesired junk characters in the output.  Sorry.\n"
            "* This help string is longer than the rest of the\n"
            "  source code to this program!\n"
      ) ;
      exit(0) ;
    }

   while(1){
     nbuf = fread( buf , 1 , LBUF , stdin ) ;
     if( nbuf == 0 ) exit(0) ;
     for( ii=0 ; ii < nbuf ; ii++ ){
       if( isspace(buf[ii]) ) continue ;
       if( isdigit(buf[ii]) ) continue ;
       if( buf[ii] == 'e' || buf[ii] == 'E' ){
         if( ii <  nbuf-1 && ii   > 0 && ISGOOD(buf[ii+1]) && ISGOOD(buf[ii-1]) ) continue ;
         if( ii == 0      && nbuf > 1 && ISGOOD(buf[ii+1]) ) continue ;
         if( ii == nbuf-1 && nbuf > 1 && ISGOOD(buf[ii-1]) ) continue ;
       }
       if( ISPMD(buf[ii]) ){
         if( (ii < nbuf-1 && ii > 0)                    &&
             (isspace(buf[ii-1]) || buf[ii-1]==buf[ii]) &&
             (isspace(buf[ii+1]) || buf[ii+1]==buf[ii])   ) buf[ii] = ' ' ;
         continue ;
       }
       buf[ii] = ' ' ;
     }
     (void)fwrite( buf , 1 , LBUF , stdout ) ;
   }
}
