#include "niml.h"

/*--------------------------------------------------------------------------*/
/*! Allocate memory. */

void * NI_malloc( size_t len )
{
   void *p = calloc(1,len) ;
   if( p == NULL ){
      fprintf(stderr,"NI_malloc() fails.\n") ; exit(1) ;
   }
   return p ;
}

/*--------------------------------------------------------------------------*/
/*! Free memory. */

void NI_free( void *p )
{
   if( p != NULL ) free(p) ;
}

/*--------------------------------------------------------------------------*/
/*! Reallocate memory. */

void * NI_realloc( void *p , size_t len )
{
   void *q = realloc( p , len ) ;
   if( q == NULL ){
      fprintf(stderr,"NI_realloc() fails.\n"); exit(1);
   }
   return q ;
}

/*--------------------------------------------------------------------------*/
/*! Like strncpy, but better. */

char * NI_strncpy( char *dest , const char *src , size_t n )
{
   if( dest == NULL || n == 0 ) return NULL ;
   if( src  == NULL || n == 1 ){ dest[0] = '\0' ; return dest ; }
   strncpy( dest , src , n-1 ) ;
   dest[n] = '\0' ; return dest ;
}


/*--------------------------------------------------------------------------*/
/*! Deallocate a header_stuff struct. */

static void destroy_header_stuff( header_stuff *hs )
{
   int ii ;
   if( hs == NULL ) return ;
   NI_free(hs->name) ;
   for( ii=0 ; ii < hs->nattr ; ii++ ){
      if( hs->lhs != NULL ) NI_free( hs->lhs[ii] ) ;
      if( hs->rhs != NULL ) NI_free( hs->rhs[ii] ) ;
   }
   NI_free( hs ) ;
}

/*--------------------------------------------------------------------------*/
/*! Find an isolated string in the input array of char.

    nst = start position
    nch = total number of data bytes
    ch  = array of data bytes

    Return value is an intpair with the .i component indicating the
    start position of the string in the data and the .j indicating
    the byte after the end of the string.  If the .i component is
    negative, then no string was found.
*/

static intpair find_string( int nst, int nch, char *ch )
{
   intpair ans = {-1,-1} ;
   int ii,jj ;
   char quot ;

   if( nst >= nch || nch < 2 || ch == NULL ) return ans;        /* bad input */

   for( ii=nst ; ii<nch && !IS_START_CHAR(ch[ii]) ; ii++ ) ;/* skip to start */

   if( ii >= nch ) return ans ;                                 /* bad input */

   if( IS_QUOTE_CHAR(ch[ii]) ){                             /* quoted string */
      if( ii == nch-1 ) return ans ;                            /* bad input */
      quot = ch[ii] ; ii++ ;
      for( jj=ii ; jj<nch && ch[jj] != quot ; jj++ ) ;      /* skip to close */
   } else {
      for( jj=ii+1 ; jj<nch && IS_STRING_CHAR(ch[jj]) ; jj++ ) ; /* to blank */
   }

   ans.i = ii ; ans.j = jj ; /* answer starts at ch[ii] and goes to ch[jj-1] */
   return ans ;
}

/*--------------------------------------------------------------------------*/
/*! Parse into strings a <header and=its attributes="OK">.

    ndat  = number of data bytes
    dat   = data bytes

   *nused = number of bytes consumed (=index of byte after the closing '>').
    Return value is a pointer to a header_stuff struct;
    if NULL is returned, something real bad happened (and *nused won't
    be assigned).
*/

static header_stuff * parse_header_stuff( int ndat, char *dat, int *nused )
{
   header_stuff *hs ; /* return value */
   int id,jd , nn ;
   intpair ss ;

   if( ndat < 2 || dat == NULL ) return NULL ;        /* bad input */

   for( id=0 ; id < ndat && dat[id] != '<' ; id++ ) ; /* skip to opening */

   if( id >= ndat-1 ) return NULL ;                   /* bad input */

   hs = NI_malloc(sizeof(header_stuff)) ;             /* make output */
   hs->nattr = hs->empty = 0 ;
   hs->name  = NULL ;
   hs->lhs   = hs->rhs = NULL ;

   /* find and assign name string */

   ss = find_string( id+1 , ndat , dat ) ;

   if( ss.i < 0 || ss.j <= ss.i ){
      destroy_header_stuff( hs ) ; return NULL ;   /* no name string */
   }

   nn = ss.j - ss.i ;                               /* string length */
   hs->name = NI_malloc(nn+1) ;
   NI_strncpy( hs->name , dat+ss.i , nn+1 ) ;

   /* start scanning for next string at location id */

   id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   /* find and assign attribute strings */

   while(1){

      for( ; id < ndat && isspace(dat[id]) ; id++ ) ; /* skip blanks */

      if( id >= ndat ) break ;                 /* end of input found */

      if( dat[id] == '>' ) break ;                  /* ">" end found */

      if( dat[id] == '/' ){                        /* "/>" end found */
         if( id < ndat-1 ) id++ ;
         hs->empty = 1 ;                   /* mark header as 'empty' */
         break ;
      }

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

      /* extend size of attribute arrays */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      /* this is the LHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->lhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->lhs[hs->nattr] , dat+ss.i , nn+1 ) ;

      hs->rhs[hs->nattr] = NULL ;             /* in case there is no RHS */

      id = ss.j ; if( id >= ndat ) break ;    /* end of input ? */

      if( dat[id] != '=' ){                   /* no '=' means no RHS */
         (hs->nattr)++ ;
         continue ;                           /* so get next attribute */
      }

      id++ ; if( id >= ndat ) break ;         /* skip the '=' */

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

      /* this is the RHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->rhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->rhs[hs->nattr] , dat+ss.i , nn+1 ) ;

      (hs->nattr)++ ;                  /* increment attribute count */

      /* start scanning for next string at location id */

      id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   } /* end of loop over input */

   if( nused != NULL ){
      if( id >= ndat ) id = ndat-1 ;
      *nused = id+1 ;              /* number of bytes used from dat */
   }

   return hs ;                         /* the goal of all that work */
}

/*--------------------------------------------------------------------*/
/*! Decode a data type string into an array of integer codes.

  Returns NULL if the input is bad bad bad.
*/

static intarray * decode_type_string( char *ts )
{
   int num, typ, lts, id,jd,kd, nn,kk ;
   intarray *iar ;

   if( ts == NULL || ts[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(intarray)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan type string 1 time to get count */

   lts = strlen(ts) ;
   num = 0 ;            /* will be count of fields */

   for( id=0 ; id < lts ; id++ ){

      if( isdigit(ts[id]) ){   /* a count prefix */
         jd = nn = 0 ;
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         if( jd <= 0 || nn <= 0 ){
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
         }
         num += jd ;     /* this many fields so far */
         id  += nn ;     /* skip count prefix (plus next character) */

         if( !IS_DATUM_CHAR(ts[id]) ){
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
         }

         continue ;      /* try for something new */
      }

      switch( ts[id] ){  /* should be a type code */

        default: break ;   /* skip unknown characters */

        case 'b':
        case 's':
        case 'i':
        case 'f':
        case 'd':
        case 'c':
        case 'r':
        case 'S':
        case 'L':
           num++ ; break ; /* add a single field */
      }
   }

   if( num <= 0 ){
      NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
   }

   /* allocate space for result */

   iar->num = num ;
   iar->ar  = NI_malloc(sizeof(int)*num) ;

   /* rescan and assign result */

   for( id=kk=0 ; id < lts ; id++ ){

      if( isdigit(ts[id]) ){   /* a count prefix */
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         id += nn ;      /* skip count prefix */
      } else {
         jd = 1 ;        /* default is a single count */
      }

      switch( ts[id] ){  /* type code (or junk) */

        default: break ;   /* skip junk characters */

        case 'b':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_BYTE ;
           break ;

        case 's':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_SHORT ;
           break ;

        case 'i':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_INT ;
           break ;

        case 'f':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_FLOAT ;
           break ;

        case 'd':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_DOUBLE ;
           break ;

        case 'c':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_COMPLEX ;
           break ;

        case 'r':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_RGB ;
           break ;

        case 'S':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_STRING ;
           break ;

        case 'L':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_LINE ;
           break ;

      }
   }

   return iar ;
}

/*-------------------------------------------------------------------------*/
/*! Name for a given integer type code.  Return value is to static string. */

char * NI_type_name( int val )
{
   static char *NI_names[NI_NUM_TYPES] =
    { "byte"  , "short"  , "int"     ,
      "float" , "double" , "complex" ,
      "rgb"   , "String" , "Line"
    } ;

   if( val < 0 || val >= NI_NUM_TYPES ) return NULL ;
   return NI_names[val] ;
}

/*********************************************************************/

int main( int argc , char *argv[] )
{
   header_stuff *hs ;
   int           nused=0 , ii ;

   if( argc < 2 ) exit(1) ;
   hs = parse_header_stuff( strlen(argv[1]) , argv[1] , &nused ) ;
   if( hs == NULL ){
      printf("returned NULL") ; exit(1) ;
   }
   printf("nused = %d\n",nused) ;
   printf("hs->name = %s\n",hs->name) ;
   printf("  ->nattr = %d  ->empty = %d\n",hs->nattr,hs->empty) ;
   for( ii=0 ; ii < hs->nattr ; ii++ ){
      printf("  attrib[%d]:  %s = %s\n" ,
             ii , hs->lhs[ii] , hs->rhs[ii] ) ;

      if( strcmp(hs->lhs[ii],"ni_type") == 0 ){
         intarray *iar = decode_type_string( hs->rhs[ii] ) ;
         if( iar != NULL ){
            int qq ;
            for( qq=0 ; qq < iar->num ; qq++ )
               printf("  type code=%d  %s\n",iar->ar[qq],NI_type_name(iar->ar[qq])) ;
         }
      }
   }
   exit(0) ;
}
