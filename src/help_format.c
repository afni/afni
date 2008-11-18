#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/*-------------------------------------------------------------------------*/

static int inside_pre = 0 ;

#define TURN_PRE_ON  \
  do{ if( !inside_pre ){ printf("<pre>\n") ; inside_pre = 1 ; } } while(0)

#define TURN_PRE_OFF \
  do{ if( inside_pre ){ printf("</pre>\n") ; inside_pre = 0 ; } } while(0)

/*-------------------------------------------------------------------------*/

#define CHK4(abcd)                                                    \
  ( tolower(buf[hend-4])==abcd[0] && tolower(buf[hend-3])==abcd[1] && \
    tolower(buf[hend-2])==abcd[2] && tolower(buf[hend-1])==abcd[3]   )

void echo_line( char *buf )
{
   int kend , hend , ii , np , is_img ;
   char *hpt ;

   kend = strlen(buf) ;
   if( kend == 0 ) return ;
   if( kend == 1 ){ TURN_PRE_ON ; printf("\n") ; return ; }

   if( buf[kend-1] == '\n' ) buf[--kend] = '\0' ;

   if( kend < 9 ){ TURN_PRE_ON ; printf("%s\n",buf) ; return ; }

   hpt = strstr(buf,"http://") ;
   if( hpt    == NULL || isspace(hpt[7]) || iscntrl(hpt[7]) ||
       hpt[7] == '\0' || hpt[7] == '*'   || hpt[7] == '.'     ){

     TURN_PRE_ON ; printf("%s\n",buf) ; return ;
   }

   TURN_PRE_OFF ; printf("<tt>") ;

 PUT_HYPERLINK:
   np = hpt - buf ;
   for( ii=0 ; ii < np ; ii++ ){
          if( isspace(buf[ii]) ) printf("&nbsp;") ;
     else if( buf[ii] == '&'   ) printf("&amp;") ;
     else if( buf[ii] == '<'   ) printf("&lt;") ;
     else if( buf[ii] == '>'   ) printf("&gt;") ;
     else                        printf("%c",buf[ii]) ;
   }

   for( hend=np ; buf[hend] != '\0' && !isspace(buf[hend]) ; hend++ ) ; /*nada*/

   is_img = ( CHK4(".jpg") || CHK4(".png") || CHK4(".gif") ) ;

   if( is_img ){
     printf("<center>\n") ;
     printf("<img src='") ;
     for( ii=np ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
     printf("' /><br />\n") ;
   }

   printf("<a href='") ;
   for( ii=np ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
   printf("'>") ;
   for( ii=np ; ii < hend ; ii++ ) printf("%c",buf[ii]) ;
   printf("</a>") ;

   if( is_img ) printf("</center>\n") ;

   buf += hend ;

   hpt = strstr(buf,"http://") ;
   if( hpt == NULL ){
     for( ii=0 ; buf[ii] != '\0' ; ii++ ){
            if( isspace(buf[ii]) ) printf("&nbsp;") ;
       else if( buf[ii] == '&'   ) printf("&amp;") ;
       else if( buf[ii] == '<'   ) printf("&lt;") ;
       else if( buf[ii] == '>'   ) printf("&gt;") ;
       else                        printf("%c",buf[ii]) ;
     }
     printf("</tt><br />\n") ; return ;
   }

   goto PUT_HYPERLINK ;
}

/*-------------------------------------------------------------------------*/

#define LBUF 1024

int main( int argc , char *argv[] )
{
   char buf[LBUF] , *cpt ;

   if( argc > 1 && strcmp(argv[1],"-help") == 0 ){
     printf("Reads from stdin; lines with http://... hyperlinks\n"
            "get converted into HTML <a href=...> hyperlinks.\n"
            "Purpose: formatting AFNI '-help' files for the Web.\n") ;
     exit(0) ;
   }

   do{
     cpt = fgets( buf , LBUF , stdin ) ;
     if( cpt == NULL || buf[0] == '\0' ) break ;
     echo_line(buf) ; buf[0] = '\0' ;
   } while(1) ;

   TURN_PRE_OFF ; exit(0) ;
}
