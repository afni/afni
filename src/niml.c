#include "niml.h"

/****************************************************************************/
/*********************** Utility functions **********************************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Allocate memory (actually uses calloc); calls exit() if it fails. */

void * NI_malloc( size_t len )
{
   void *p = calloc(1,len) ;
   if( p == NULL ){
      fprintf(stderr,"NI_malloc() fails.\n") ; exit(1) ;
   }
   return p ;
}

/*--------------------------------------------------------------------------*/
/*! Free memory; NULL pointer is just ignored. */

void NI_free( void *p )
{
   if( p != NULL ) free(p) ;
}

/*--------------------------------------------------------------------------*/
/*! Reallocate memory; calls exit() if it fails. */

void * NI_realloc( void *p , size_t len )
{
   void *q = realloc( p , len ) ;
   if( q == NULL && len > 0 ){
      fprintf(stderr,"NI_realloc() fails.\n"); exit(1);
   }
   return q ;
}

/*--------------------------------------------------------------------------*/
/*! Like strncpy, but better (result always ends in NUL character). */

char * NI_strncpy( char *dest , const char *src , size_t n )
{
   if( dest == NULL || n == 0 ) return NULL ;
   if( src  == NULL || n == 1 ){ dest[0] = '\0' ; return dest ; }
   strncpy( dest , src , n-1 ) ;
   dest[n] = '\0' ; return dest ;
}

/*------------------------------------------------------------------------*/
/*! Find a string in an array of strings; return index (-1 if not found). */

static int string_index( char *targ , int nstr , char *str[] )
{
   int ii ;

   if( nstr < 1 || str == NULL || targ == NULL ) return -1 ;

   for( ii=0 ; ii < nstr ; ii++ )
      if( str[ii] != NULL && strcmp(str[ii],targ) == 0 ) return ii ;

   return -1 ;
}

/*-------------------------------------------------------------------*/
/*!  Sleep a given # of milliseconds (uses the Unix select routine). */

void NI_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;             /* can't wait into the past */
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Return the file length (-1 if file not found). */

long NI_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

/*---------------------------------------------------------------*/
/*! Return time elapsed since first call to this routine (msec).
    Note this will overflow an int after 24+ days.  You probably
    don't want to use this if the program will be running
    continuously for such a long time.
-----------------------------------------------------------------*/

int NI_clock_time(void)
{
   struct timeval  new_tval ;
   struct timezone tzone ;
   static struct timeval old_tval ;
   static int first = 1 ;

   gettimeofday( &new_tval , &tzone ) ;

   if( first ){
      old_tval = new_tval ;
      first    = 0 ;
      return 0.0 ;
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){
      new_tval.tv_usec += 1000000 ;
      new_tval.tv_sec -- ;
   }

   return (int)( (new_tval.tv_sec  - old_tval.tv_sec )*1000.0
                +(new_tval.tv_usec - old_tval.tv_usec)*0.001 + 0.5 ) ;
}

/*------------------------------------------------------------------------*/
/*! Un-escape a C string inplace.  (This can be done since the replacement
    is always smaller than the input.)  Escapes recognized are:
      *  &lt;   ->  <
      *  &gt;   ->  >
      *  &quot; ->  "
      *  &apos; ->  '
      *  &amp;  ->  &
    Return value is number of replacements made.
--------------------------------------------------------------------------*/

static int unescape_inplace( char *str )
{
   int ii,jj , nn,ll ;

   if( str == NULL ) return 0 ;                /* no string? */
   ll = strlen(str) ;  if( ll < 4 ) return 0 ; /* too short */
   if( memchr(str,'&',ll) == NULL ) return 0 ; /* no '&' in string */
   if( memchr(str,';',ll) == NULL ) return 0 ; /* no ';' in string */

   /* scan for escapes: &something; */

   for( ii=jj=nn=0 ; ii<ll ; ii++,jj++ ){ /* scan at ii; put results in at jj */

      if( str[ii] == '&' ){  /* start of escape? */

              if( ii+3 < ll        &&   /* &lt; */
                  str[ii+1] == 'l' &&
                  str[ii+2] == 't' &&
                  str[ii+3] == ';'   ){ str[jj] = '<' ; ii += 3 ; nn++ ; }

         else if( ii+3 < ll        &&   /* &gt; */
                  str[ii+1] == 'g' &&
                  str[ii+2] == 't' &&
                  str[ii+3] == ';'   ){ str[jj] = '>' ; ii += 3 ; nn++ ; }

         else if( ii+5 < ll        &&   /* &quot; */
                  str[ii+1] == 'q' &&
                  str[ii+2] == 'u' &&
                  str[ii+3] == 'o' &&
                  str[ii+4] == 't' &&
                  str[ii+5] == ';'   ){ str[jj] = '"' ; ii += 5 ; nn++ ; }

         else if( ii+5 < ll        &&   /* &apos; */
                  str[ii+1] == 'a' &&
                  str[ii+2] == 'p' &&
                  str[ii+3] == 'o' &&
                  str[ii+4] == 's' &&
                  str[ii+5] == ';'   ){ str[jj] = '\'' ; ii += 5 ; nn++ ; }

         else if( ii+4 < ll        &&  /* &amp; */
                  str[ii+1] == 'a' &&
                  str[ii+2] == 'm' &&
                  str[ii+3] == 'p' &&
                  str[ii+4] == ';'   ){ str[jj] = '&' ; ii += 4 ; nn++ ; }

         else if( ii+3 < ll        &&
                  str[ii+1] == '#' &&
                  isdigit(str[ii+2]) ){   /* &#dec; */

            unsigned int val='?' ; int kk=ii+3 ;
            while( kk < ll && kk != ';' ) kk++ ;
            sscanf( str+ii+2 , "%u" , &val ) ;
            str[jj] = (char) val ; ii = kk ; nn++ ;
         }

         else if( ii+4 < ll        &&
                  str[ii+1] == '#' &&
                  str[ii+2] == 'x' &&
                  isxdigit(str[ii+3]) ){   /* &#hex; */

            unsigned int val='?' ; int kk=ii+4 ;
            while( kk < ll && kk != ';' ) kk++ ;
            sscanf( str+ii+3 , "%x" , &val ) ;
            str[jj] = (char) val ; ii = kk ; nn++ ;
         }

         /* didn't start a recognized escape, so just copy as normal */

         else if( jj < ii )           { str[jj] = str[ii] ; }

      }  /* was a normal character */

         else if( jj < ii )           { str[jj] = str[ii] ; }

      /* at this point, ii=index of last character used up in scan
                        jj=index of last character written to (jj <= ii) */
   }

   if( jj < ll ) str[jj] = '\0' ; /* end string properly */

   return nn ;
}

/*************************************************************************/
/************************ Byte ordering functions ************************/
/*************************************************************************/

/*---------------------------------------------------------------*/
/*! Find the byte order on this system. */

static int NI_byteorder(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = 0 ;

   return (fred.ss == 1) ? NI_LSB_FIRST : NI_MSB_FIRST ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 2 bytes (shorts). */

typedef struct { unsigned char a,b ; } twobytes ;

static void NI_swap2( int n , void *ar )
{
   register int ii ;
   register twobytes *tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].b ; tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 4 bytes (ints or floats) */

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

static void NI_swap4( int n , void *ar )
{
   register int ii ;
   register fourbytes *tb = (fourbytes *) ar ;
   register unsigned char tt , uu ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].d ; tb[ii].d = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].c ; tb[ii].c = uu ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/*! Swap arrays of 8 bytes (doubles or 64 bit ints) */

typedef struct { unsigned char a,b,c,d , e,f,g,h ; } eightbytes ;

static void NI_swap8( int n , void *ar )
{
   register int ii ;
   register eightbytes *tb = (eightbytes *) ar ;
   register unsigned char tt , uu , vv , ww ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].h ; tb[ii].h = tt ;
      uu = tb[ii].b ; tb[ii].b = tb[ii].g ; tb[ii].g = uu ;
      vv = tb[ii].c ; tb[ii].c = tb[ii].f ; tb[ii].f = vv ;
      ww = tb[ii].d ; tb[ii].d = tb[ii].e ; tb[ii].e = ww ;
   }
   return ;
}

/****************************************************************************/
/****************** Functions to process a NIML header **********************/
/****************************************************************************/

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

/*-------------------------------------------------------------------------*/
/*! Characters allowed inside unquoted strings. */

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '='                  )

/*! Defines what we consider a quoting character. */

#define IS_QUOTE_CHAR(c)  ( (c) == '"' || (c) == '\'' )

/*-------------------------------------------------------------------------*/
/*! Find an isolated string in the input array of char.

    nst = start position
    nch = total number of data bytes
    ch  = array of data bytes

    Return value is an intpair with the .i component indicating the
    start position of the string in the data and the .j indicating
    the byte AFTER the end of the string.  If the .i component is
    negative, then no string was found.
---------------------------------------------------------------------------*/

static intpair find_string( int nst, int nch, char *ch )
{
   intpair ans = {-1,-1} ;  /* default answer ==> nothing found */
   int ii,jj ;
   char quot ;

   if( nst >= nch || nch < 2 || ch == NULL ) return ans;        /* bad input */

   for( ii=nst; ii<nch && !IS_STRING_CHAR(ch[ii]); ii++ ) ; /* skip to start */

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
/*! Parse into strings a <header and=its attributes="stuff">.

    ndat  = number of data bytes
    dat   = data bytes

   *nused = number of bytes consumed (=index of byte after the closing '>').
    Return value is a pointer to a header_stuff struct;
    if NULL is returned, something real bad happened (and *nused won't
    be assigned).
----------------------------------------------------------------------------*/

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
         if( id < ndat-1 ) id++ ;                  /* skip the '>'   */
         hs->empty = 1 ;                   /* mark header as 'empty' */
         break ;                /* done with scanning for attributes */
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
      unescape_inplace( hs->lhs[hs->nattr] ) ;

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
      unescape_inplace( hs->rhs[hs->nattr] ) ;

      (hs->nattr)++ ;                  /* increment attribute count */

      /* start scanning for next string at location id */

      id = ss.j ;
      if( IS_QUOTE_CHAR(dat[id]) ) id++ ;  /* skip closing quote */

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
----------------------------------------------------------------------*/

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
        case 'R':
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
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_BYTE ;
        break ;

        case 's':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_SHORT ;
        break ;

        case 'i':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_INT ;
        break ;

        case 'f':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_FLOAT ;
        break ;

        case 'd':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_DOUBLE ;
        break ;

        case 'c':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_COMPLEX ;
        break ;

        case 'r':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_RGB ;
        break ;

        case 'R':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_RGBA ;
        break ;

        case 'S':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_STRING ;
        break ;

        case 'L':
           for( kd=0 ; kd < jd ; kd++ ) iar->ar[kk++] = NI_LINE ;
        break ;
      }
   }

   return iar ;
}

/*-----------------------------------------------------------------------*/
/*! Construct an empty data element from a header.

    The data vectors will have space allocated, but they will be
    filled with all zero bytes.  If the header was "empty" (ended in
    "/>"), then no vectors will be allocated, and nel->vec_num=0.
-------------------------------------------------------------------------*/

static NI_element * make_empty_data_element( header_stuff *hs )
{
   NI_element *nel ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;

   /* move name and attributes from hs to new element */

   nel->name = hs->name ; hs->name = NULL ;

   nel->attr_num = hs->nattr ;

   if( nel->attr_num > 0 ){
      nel->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      nel->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      nel->attr_lhs = nel->attr_rhs = NULL ;
   }

   /* set default vector parameters [indicating no data] */

   nel->vec_num = 0 ;
   nel->vec_len = 0 ;
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   nel->vec_filled = 0 ;  /* no data has been filled into vectors */

   if( !hs->empty ){  /* find and process ni_* attributes about vectors */

     /* ni_type attribute */

     ii = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
       intarray *iar = decode_type_string( nel->attr_rhs[ii] ) ;
       if( iar != NULL ){
         nel->vec_num = iar->num ;  /* number of vectors */
         nel->vec_typ = iar->ar ;   /* vector types */
         NI_free(iar) ;             /* just the shell of the struct */
       }
     }

     /* ni_dimen attribute */

     ii = string_index( "ni_dimen" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
        int qq = strtol( nel->attr_rhs[ii] , NULL , 10 ) ;
        if( qq > 0 ) nel->vec_len = qq ;
     }

     /* supply vector parameters if none was given */

     if( nel->vec_len == 0 ) nel->vec_len = 1 ;  /* default length */

     if( nel->vec_num == 0 ){                    /* default type */
        nel->vec_num    = 1 ;
        nel->vec_typ    = NI_malloc(sizeof(int)) ;
        nel->vec_typ[0] = NI_BYTE ;
     }

     /* now allocate space for vectors defined above */

     nel->vec = NI_malloc( sizeof(void *)*nel->vec_num ) ;

     for( ii=0 ; ii < nel->vec_num ; ii++ )
       nel->vec[ii] = NI_malloc(NI_type_size(nel->vec_typ[ii]) * nel->vec_len) ;
   }

   return nel ;
}

/*-------------------------------------------------------------------------*/
/*! Make an empty group element from parsed header info.

    To be consistent with the NIML spec, the header name should
    be "ni_group", but nothing is done to check for this.  Also, the
    header should not be empty, but this is not checked at all.

    The attributes in the header are assigned to the group, and the group
    parts are initialized to nothing.
---------------------------------------------------------------------------*/

static NI_group * make_empty_group_element( header_stuff *hs )
{
   NI_group *ngr ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   /* move attributes from hs to new element */

   ngr->attr_num = hs->nattr ;

   if( ngr->attr_num > 0 ){
      ngr->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      ngr->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      ngr->attr_lhs = ngr->attr_rhs = NULL ;
   }

   /* have no pieces-parts yet */

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;

   return ngr ;
}

/*-------------------------------------------------------------------------*/
/*! Name for a given integer type code.  Return value is to static string. */

char * NI_type_name( int tval )
{
   static char *NI_names[NI_NUM_TYPES] =
    { "byte"  , "short"  , "int"     ,
      "float" , "double" , "complex" ,
      "rgb"   , "String" , "Line"    ,
      "Rgba"
    } ;

   if( tval < 0 || tval >= NI_NUM_TYPES ) return NULL ;
   return NI_names[tval] ;
}

/*-------------------------------------------------------------------------*/
/*! Byte size of a given integer type code. */

int NI_type_size( int tval )
{
   switch( tval ){
      case NI_BYTE:     return sizeof(byte)    ;
      case NI_SHORT:    return sizeof(short)   ;
      case NI_INT:      return sizeof(int)     ;
      case NI_FLOAT:    return sizeof(float)   ;
      case NI_DOUBLE:   return sizeof(double)  ;
      case NI_COMPLEX:  return sizeof(complex) ;
      case NI_RGB:      return sizeof(rgb)     ;
      case NI_RGBA:     return sizeof(rgba)    ;
      case NI_STRING:   return sizeof(char *)  ;
      case NI_LINE:     return sizeof(char *)  ;
   }
   return 0 ;
}

#if 0
/*-------------------------------------------------------------------------*/
/*! Number of component values of a given integer type code. */

static int NI_type_nval( int tval )
{
   switch( tval ){
      case NI_BYTE:     return 1 ;
      case NI_SHORT:    return 1 ;
      case NI_INT:      return 1 ;
      case NI_FLOAT:    return 1 ;
      case NI_DOUBLE:   return 1 ;
      case NI_COMPLEX:  return 2 ;
      case NI_RGB:      return 3 ;
      case NI_RGBA:     return 4 ;
      case NI_STRING:   return 0 ;
      case NI_LINE:     return 0 ;
   }
   return 0 ;
}
#endif

/*----------------------------------------------------------------------*/
/*! Return the size in bytes of one row in a data element. */

int NI_element_rowsize( NI_element *nel )
{
   int ii , nb ;

   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   for( ii=nb=0 ; ii < nel->vec_num ; ii++ )
      nb += NI_type_size( nel->vec_typ[ii] ) ;

   return nb ;
}

/*----------------------------------------------------------------------*/
/*! Return the size of all the rows in a data element. */

int NI_element_allsize( NI_element *nel )
{
   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_len < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   return (nel->vec_len * NI_element_rowsize(nel)) ;
}

/*************************************************************************/
/********** Functions to create NIML data and group elements *************/
/*************************************************************************/

/*-----------------------------------------------------------------------*/
/*! Return the type of something that points to a NI element.

    The input should be a pointer to a NI_element or a NI_group.
    The return value is NI_ELEMENT_TYPE, NI_GROUP_TYPE, or -1.
-------------------------------------------------------------------------*/

int NI_element_type( void *nini )
{
   NI_element *nel = (NI_element *) nini ;
   NI_group   *ngr = (NI_group *)   nini ;

   if( nini == NULL ) return -1 ;

   if( nel->type == NI_ELEMENT_TYPE ) return NI_ELEMENT_TYPE ;
   if( ngr->type == NI_GROUP_TYPE   ) return NI_GROUP_TYPE   ;

   return -1 ;
}

/*-----------------------------------------------------------------------*/
/*! Expunge a data or group element and its contents from the universe.  */

void NI_free_element( void *nini )
{
   int ii , tt=NI_element_type(nini)  ;

   if( tt < 0 ) return ; /* bad input */

   /*-- erase contents of data element --*/

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      NI_free(nel->name) ;
      for( ii=0 ; ii < nel->attr_num ; ii++ ){
         NI_free( nel->attr_lhs[ii] ) ;
         NI_free( nel->attr_rhs[ii] ) ;
      }
      NI_free( nel->attr_lhs ) ;
      NI_free( nel->attr_rhs ) ;
      NI_free( nel->vec_typ  ) ;
      for( ii=0 ; ii < nel->vec_num ; ii++ )
         NI_free( nel->vec[ii] ) ;
      NI_free( nel->vec ) ;
      NI_free( nel ) ;

   /*-- erase contents of group element --*/

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){
         NI_free( ngr->attr_lhs[ii] ) ;
         NI_free( ngr->attr_rhs[ii] ) ;
      }
      NI_free( ngr->attr_lhs ) ;
      NI_free( ngr->attr_rhs ) ;

      for( ii=0 ; ii < ngr->part_num ; ii++ )
         NI_free_element( ngr->part[ii] ) ;     /* recursion */

      NI_free( ngr->part_typ ) ;
      NI_free( ngr->part ) ;
      NI_free( ngr ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new data element.

    name   = string name for header.
    veclen = size of vectors (ni_dimen); set this to zero for "empty"
             elements (those with only headers, no data).

    Return is NULL if inputs are stupid.
-------------------------------------------------------------------------*/

NI_element * NI_new_data_element( char *name , int veclen )
{
   NI_element *nel ;

   if( name == NULL || name[0] == '\0' || veclen < 0 ) return NULL ;

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;  /* mark as being a data element */

   nel->name = strdup(name) ;
   nel->attr_num = 0 ;
   nel->attr_lhs = nel->attr_rhs = NULL ;

   nel->vec_num = 0 ;
   nel->vec_len = veclen ;

   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   return nel ;
}

/*-----------------------------------------------------------------------*/
/*! Add a vector (column) of data to a data element.

    nel = data element to modify
    typ = type code of data (e.g., NI_FLOAT)
    arr = pointer to data values - must be an array of length veclen
          from NI_new_data_element()

    The data array is copied into the element.  If the element was
    specified with veclen=0, then this function will do nothing.
    Since this function has no return value, the only way to check for
    such an error is to see if nel->vec_num was incremented.
-------------------------------------------------------------------------*/

void NI_add_vector( NI_element *nel , int typ , void *arr )
{
   int nn , ll ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len == 0 || arr == NULL ) return ;

   if( typ < 0 || typ >= NI_NUM_TYPES ) return ;

   if( nel->type != NI_ELEMENT_TYPE ) return ;

   /* get number of vectors currently in element */

   nn = nel->vec_num ;

   /* add 1 to the vec_typ array */

   nel->vec_typ     = NI_realloc( nel->vec_typ , sizeof(int)*(nn+1) ) ;
   nel->vec_typ[nn] = typ ;

   /* add 1 to the vec array, and copy data into it */

   nel->vec     = NI_realloc( nel->vec , sizeof(void *)*(nn+1) ) ;
   ll           = nel->vec_len * NI_type_size(typ) ;
   nel->vec[nn] = NI_malloc( ll ) ;
   memcpy( nel->vec[nn] , arr , ll ) ;  /* the real work is here */

   /* add 1 to the count of vectors */

   nel->vec_num = nn+1 ;
   return ;
}

/*------------------------------------------------------------------------*/
/*! Add an attribute to a data or group element. */

void NI_set_attribute( void *nini , char *attname , char *attvalue )
{
   int nn , tt=NI_element_type(nini) ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      nn = nel->attr_num ;

      nel->attr_lhs = NI_realloc( nel->attr_lhs , sizeof(char *)*(nn+1) ) ;
      nel->attr_rhs = NI_realloc( nel->attr_rhs , sizeof(char *)*(nn+1) ) ;

      nel->attr_lhs[nn] = strdup(attname) ;

      if( attvalue != NULL && attvalue[0] != '\0' )
         nel->attr_lhs[nn] = strdup(attvalue) ;
      else
         nel->attr_lhs[nn] = NULL ;

      nel->attr_num = nn+1 ;

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      nn = ngr->attr_num ;

      ngr->attr_lhs = NI_realloc( ngr->attr_lhs , sizeof(char *)*(nn+1) ) ;
      ngr->attr_rhs = NI_realloc( ngr->attr_rhs , sizeof(char *)*(nn+1) ) ;

      ngr->attr_lhs[nn] = strdup(attname) ;

      if( attvalue != NULL && attvalue[0] != '\0' )
         ngr->attr_lhs[nn] = strdup(attvalue) ;
      else
         ngr->attr_lhs[nn] = NULL ;

      ngr->attr_num = nn+1 ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new group element. */

NI_group * NI_new_group_element(void)
{
   NI_group *ngr ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   ngr->attr_num = 0 ;
   ngr->attr_lhs = ngr->attr_rhs = NULL ;

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;

   return ngr ;
}

/*-----------------------------------------------------------------------*/
/*! Add an element to a group element. */

void NI_add_to_group( NI_group *ngr , void *nini )
{
   int nn , tt=NI_element_type(nini) ;

   if( ngr == NULL || ngr->type != NI_GROUP_TYPE || tt < 0 ) return ;

   nn = ngr->part_num ;

   ngr->part_typ     = NI_realloc( ngr->part_typ , sizeof(int)*(nn+1) ) ;
   ngr->part_typ[nn] = tt ;

   ngr->part         = NI_realloc( ngr->part , sizeof(void *)*(nn+1) ) ;
   ngr->part[nn]     = nini ;

   ngr->part_num     = nn+1 ;

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Fill one row of an element with some data bytes (numeric only). */

static void NI_fill_vector_row( NI_element *nel , int row , char *buf )
{
   int bpos=0 , col ;
   char tmp[16] ;  /* We copy into here from buf, then into the vector. */
                   /* The reason for this is to ensure proper byte     */
                   /* alignment for the assignment into the vector.   */

   /* check inputs for stupidity */

   if( nel->type != NI_ELEMENT_TYPE ||
       row       <  0               ||
       row       >= nel->vec_len    || buf == NULL ) return ;

   /* loop over columns, taking the requisite number of
      bytes from buf and stuffing them into the vectors */

   for( col=0 ; col < nel->vec_num ; col++ ){
     switch( nel->vec_typ[col] ){
       default:                     /* unimplemented types */
       break ;                      /* (STRING and LINE)  */

       case NI_BYTE:{
         byte *vpt = (byte *) nel->vec[col] ;
         byte *bpt = (byte *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(byte)) ;
         vpt[row]  = *bpt ; bpos += sizeof(byte) ;
       }
       break ;

       case NI_SHORT:{
         short *vpt = (short *) nel->vec[col] ;
         short *bpt = (short *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(short)) ;
         vpt[row]  = *bpt ; bpos += sizeof(short) ;
       }
       break ;

       case NI_INT:{
         int *vpt = (int *) nel->vec[col] ;
         int *bpt = (int *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(int)) ;
         vpt[row]  = *bpt ; bpos += sizeof(int) ;
       }
       break ;

       case NI_FLOAT:{
         float *vpt = (float *) nel->vec[col] ;
         float *bpt = (float *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(float)) ;
         vpt[row]  = *bpt ; bpos += sizeof(float) ;
       }
       break ;

       case NI_DOUBLE:{
         double *vpt = (double *) nel->vec[col] ;
         double *bpt = (double *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(double)) ;
         vpt[row]  = *bpt ; bpos += sizeof(double) ;
       }
       break ;

       case NI_COMPLEX:{
         complex *vpt = (complex *) nel->vec[col] ;
         complex *bpt = (complex *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(complex)) ;
         vpt[row]  = *bpt ; bpos += sizeof(complex) ;
       }
       break ;

       case NI_RGB:{
         rgb *vpt = (rgb *) nel->vec[col] ;
         rgb *bpt = (rgb *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgb)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgb) ;
       }
       break ;

       case NI_RGBA:{
         rgba *vpt = (rgba *) nel->vec[col] ;
         rgba *bpt = (rgba *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgba)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgba) ;
       }
       break ;
     }
   }
   return ;
}

/*------------------------------------------------------------------*/
/*! Swap bytes for an array of type code tval. */

void NI_swap_vector( int tval , int nvec , void *vec )
{
   /* check inputs for stupidity */

   if( nvec <= 0 || vec == NULL ) return ;

   switch( tval ){

      default:  break ;   /* nothing to do */

      case NI_SHORT:    NI_swap2( nvec , vec ) ; break ;

      case NI_INT:
      case NI_FLOAT:    NI_swap4( nvec , vec ) ; break ;

      case NI_DOUBLE:   NI_swap8( nvec , vec ) ; break ;

      case NI_COMPLEX:  NI_swap4( 2*nvec, vec) ; break ;
   }
   return ;
}

/*************************************************************************/
/********************* Functions for NIML I/O ****************************/
/*** See http://www.manualy.sk/sock-faq/unix-socket-faq.html for info. ***/
/*************************************************************************/

/*! To print a system error message. */

#define PERROR(x) perror(x)

#include <signal.h>

/*! For tcp - indicates that SIGPIPE is ignored;
    will be set the first time tcp_send is called. */

static int nosigpipe = 0 ;

/*! How to close a socket, given the descriptor ss. */

#define CLOSEDOWN(ss) ( shutdown((ss),2) , close((ss)) )

/*! This is used to set the send/receive buffer size for sockets **/

#define SOCKET_BUFSIZE  NI_BUFSIZE

/*! This macro is used so I can replace recv() with something else if I want. */

#define tcp_recv recv

/*! This macro is used so I can replace send() with something else if I want. */

#define tcp_send send

#ifndef MIN
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

/*! Next delay in milliseconds, given current delay. */

#define NEXTDMS(dm) MIN(1.1*(dm)+1.01,999.0)

/********************************************************************
  Routines to manipulate TCP/IP stream sockets.
*********************************************************************/

/*-------------------------------------------------------------------*/
/*!  See if the given socket (sd) is ready to read.

   msec is the number of milliseconds to wait:
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to read
      1 = socket has data
---------------------------------------------------------------------*/

static int tcp_readcheck( int sd , int msec )
{
   int ii ;
   fd_set rfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&rfds) ; FD_SET(sd, &rfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, &rfds, NULL, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_readcheck(select)" ) ;
   return ii ;
}

/*-------------------------------------------------------------------*/
/*! See if the given socket is ready to write.

    msec = max amount of time to wait, in milliseconds.
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to write
      1 = OK to write to socket
---------------------------------------------------------------------*/

static int tcp_writecheck( int sd , int msec )
{
   int ii ;
   fd_set wfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&wfds) ; FD_SET(sd, &wfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, NULL , &wfds, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_writecheck(select)" ) ;
   return ii ;
}

/*------------------------------------------------------------------------*/
/*! Set a socket so that it will cutoff quickly when it is closed.

   See http://www.manualy.sk/sock-faq/unix-socket-faq.html for more
   information about this stuff.
--------------------------------------------------------------------------*/

static void tcp_set_cutoff( int sd )
{
   if( sd < 0 ) return ;  /* bad input */

#if 1
   /* Turn off "lingering". */

   { struct linger lg ;
     lg.l_onoff  = 1 ;
     lg.l_linger = 0 ;
     setsockopt(sd, SOL_SOCKET, SO_LINGER, (void *)&lg, sizeof(struct linger)) ;
   }
#endif

#if 1
   /* Let the address be reused quickly,
      in case of another connection from the same host on the same port. */

   { int optval = 1;
     setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;
   }
#endif

   return ;
}

/*-------------------------------------------------------------------*/
/*!  Check if an already active socket is still alive.

   If it is dead, then readcheck will say we can read, but we
   won't actually get any bytes when we try (using peek mode).
   Returns 1 if things are OK, 0 if not.
---------------------------------------------------------------------*/

static int tcp_alivecheck( sd )
{
   int ii ;
   char bbb[4] ;

   ii = tcp_readcheck(sd,0) ;                 /* can I read?          */
   if( ii == 0 ) return 1 ;                   /* can't read is OK     */
   if( ii <  0 ) return 0 ;                   /* some error is bad    */
   errno = 0 ;
   ii = tcp_recv( sd , bbb , 1 , MSG_PEEK ) ; /* try to read one byte */
   if( ii == 1 ) return 1 ;                   /* if we get it, good   */
   if( errno ) PERROR("tcp_alivecheck") ;
   return 0 ;                                 /* no data ==> death!   */
}

/*------------------------------------------------------------------------*/
/*!  Open a socket to the given host, to the given TCP port.

     This function is used to "reach out" to a server that is supposed
     to be listening on the same port.
     Returns socket id; if -1, some error occured (e.g., nobody listening).
--------------------------------------------------------------------------*/

static int tcp_connect( char * host , int port )
{
   int sd , l ;
   struct sockaddr_in sin ;
   struct hostent *   hostp ;

   if( host == NULL || port < 1 ) return -1 ;  /* bad inputs */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_connect(socket)"); return -1; }

   /** set socket options (no delays, large buffers) **/

   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family = AF_INET ;
   sin.sin_port   = htons(port) ;

   /** set remote computer IP address from its name **/

   hostp = gethostbyname(host) ;
   if( hostp == NULL ){
      PERROR("tcp_connect(gethostbyname)"); CLOSEDOWN(sd); return -1;
   }
   sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr ;

   if( connect(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("tcp_connect(connect)") ; CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Set up to listen for a connection on a given port.

   This is intended for use by a server, which will wait for some other
   program to actively connect to this port.  There is no security here -
   connections will be taken from any IP address.

   This function does not actually form the connection.  That must be done
   separately.  Whether someone is trying to connect can be checked for
   with the routine "tcp_readcheck" and then accepted with "tcp_accept".

   The return value is the descriptor for the listening socket.
----------------------------------------------------------------------------*/

static int tcp_listen( int port )
{
   int sd , l ;
   struct sockaddr_in sin ;

   if( port < 1 ) return -1 ; /* bad input */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_listen(socket)"); return -1; }

   /** set socket options (no delays, large buffers) **/

   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family      = AF_INET ;
   sin.sin_port        = htons(port) ;
   sin.sin_addr.s_addr = INADDR_ANY ;  /* reader reads from anybody */

   if( bind(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("tcp_listen(bind)"); CLOSEDOWN(sd); return -1;
   }

   if( listen(sd,1) == -1 ){
      PERROR("tcp_listen(listen)"); CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Accept incoming connection on a socket.

   Return value is the attached socket (which is not the original socket!).
   If -1 is returned, some error occured.  If the accept works, then the
   original socket is still open and listening for further attachments.
   Under many circumstances, you will want to close the original socket
   immediately.

   If hostname is not NULL, then the char * it points to will be filled
   with a pointer to the official name of the host that connected.

   If hostaddr is not NULL, then the char * it points to will be filled
   with a pointer to the Internet address (in 'dot' form) of the host that
   connected.

   Both the char * pointers returned are from malloc, and should be free()-d
   when no longer needed.  If they aren't needed at all, just pass in NULL
   for these arguments.

   Note that this routine will block until somebody connects.  You can
   use tcp_readcheck(sd,0) to see if anyone is waiting to connect before
   calling this routine.
---------------------------------------------------------------------------*/

static int tcp_accept( int sd , char ** hostname , char ** hostaddr )
{
   struct sockaddr_in pin ;
   int addrlen , sd_new ;
   struct hostent * hostp ;
   char * sout , * str ;

   /** accept the connection **/

   addrlen = sizeof(pin) ;
   sd_new = accept( sd , (struct sockaddr *)&pin , &addrlen ) ;
   if( sd_new == -1 ){ PERROR("tcp_accept"); return -1; }

   /** get name of connector **/

   if( hostname != NULL ){
      hostp = gethostbyaddr( (char *) (&pin.sin_addr) ,
                             sizeof(struct in_addr) , AF_INET ) ;
      if( hostp != NULL ){
         sout = (char *) malloc( strlen(hostp->h_name)+1 ) ;
         strcpy(sout,hostp->h_name) ;
      } else {
         sout = (char *) malloc( strlen("UNKNOWN")+1 ) ;
         strcpy(sout,"UNKNOWN") ;
      }
      *hostname = sout ;
   }

   /** get address of connector **/

   if( hostaddr != NULL ){
      str = inet_ntoa( pin.sin_addr ) ;
      sout = (char *) malloc( strlen(str)+1 ) ;
      strcpy(sout,str) ;
      *hostaddr = sout ;
   }

   return sd_new ;
}

/*******************************************************************/
/*** Functions to read/write from NI_streams (files or sockets). ***/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open a NIML input or output stream, and return a pointer to it.

    NULL is returned if an error occurs.

  name = "tcp:host:port" to connect a socket to system "host"
             on the given port number.

  name = "fil:filename" to open a file for I/O.

  name = "str:" to read/write data from a string

  mode = "w" to open a stream for writing
           * tcp: host must be specified ("w" is for a tcp client)
           * fil: filename is opened in write mode (and will be
                  overwritten if already exists)
           * str: data will be written to a buffer in the NI_stream
                  struct; you can later access this buffer with the
                  function NI_string_buf().

  mode = "r" to open a stream for reading
           * tcp: host is ignored (but must be present);
                  ("r" is for a tcp server)
           * fil: filename is opened in read mode
           * str: characters after the colon are the source of
                  the input data (will be copied to internal buffer)

  For a fil: or str: stream, you can either read from or write to the
  stream, but not both.  For a tcp: stream, once it is connected, you
  can both read and write.

  The inputs "host" (for tcp:) and "filename" (for fil:) are
  limited to a maximum of 127 bytes.  For str:, there is no
  limit for the "r" stream (but clearly you can't have any NUL
  bytes in there).

  Since opening a socket requires sychronizing two processes,
  you can't read or write to a tcp: stream immediately.  Instead
  you have to check if it is "good" first.  This can be done using
  the function NI_stream_goodcheck().

  After an tcp: "r" stream is good, then the string ns->name
  contains the IP address of the connecting host, in "dot" form
  (e.g., "201.202.203.204"); here, "ns" is the NI_stream returned
  by this routine.

  For a fil: stream, ns->name contains the filename.

  For a str: stream, ns->name contains a stupid string.
------------------------------------------------------------------------*/

NI_stream NI_stream_open( char *name , char *mode )
{
   NI_stream_type *ns ;
   int do_create , do_accept ;

   /** check if inputs are reasonable **/

   if( name == NULL || strlen(name) < 4 ) return NULL ;

   if( mode == NULL ) return NULL ;

   do_create = (*mode == 'w') ;
   do_accept = (*mode == 'r') ;

   if( !do_create && !do_accept ) return NULL ;

   /***** deal with TCP/IP sockets *****/

   if( strncmp(name,"tcp:",4) == 0 ){
      char host[128] , *hend ;
      int  port=-1 , ii , jj ;

      if( strlen(name) > 127 ) return NULL ;

      /** find "host" substring **/

      hend = strstr( name+4 , ":" ) ;
      if( hend == NULL || hend-name > 128 ) return NULL ;

      for( ii=4 ; name[ii] != ':' ; ii++ ) host[ii-4] = name[ii] ;
      host[ii-4] = '\0' ;

      /** get "port" number **/

      port = strtol( name+ii+1 , NULL , 10 ) ;
      if( port <= 0 ) return NULL ;

      /** initialize NI_stream_type output struct **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type = NI_TCP_TYPE;   /* what kind is this? */
      ns->port = port ;         /* save the port #    */
      ns->nbuf = 0 ;            /* buffer is empty    */
      ns->npos = 0 ;            /* scan starts at 0   */

      ns->buf     = NI_malloc(NI_BUFSIZE) ;
      ns->bufsize = NI_BUFSIZE ;

      ns->bin_thresh = -1 ;     /* write in text mode */

      /** attach to incoming call "r" **/

      if( do_accept ){
         ns->io_mode = NI_INPUT_MODE ;
         ns->sd = tcp_listen( port ) ;                   /* set up to listen  */
         if( ns->sd < 0 ){                               /* error? must die!  */
            NI_free(ns) ; return NULL ;
         }
         ns->bad = TCP_WAIT_ACCEPT ;                     /* not connected yet */
         ii = tcp_readcheck(ns->sd,1) ;                  /* see if ready      */
         if( ii > 0 ){                                   /* if socket ready:  */
            jj = tcp_accept( ns->sd , NULL,&hend ) ;     /* accept connection */
            if( jj >= 0 ){                               /* if accept worked  */
               CLOSEDOWN( ns->sd ) ;                     /* close old socket  */
               NI_strncpy(ns->name,hend,128) ;           /* put IP into name  */
               free(hend) ; ns->bad = 0 ; ns->sd = jj ;  /* and ready to go!  */
            }
         }
         return ns ;
      }

      /** place an outgoing call "w" **/

      if( do_create ){
         struct hostent *hostp ;
         ns->io_mode = NI_OUTPUT_MODE ;
         hostp = gethostbyname(host) ;                   /* lookup host on net */
         if( hostp == NULL ){                            /* fails? must die!   */
             NI_free(ns) ; return NULL ;
         }
         ns->sd  = tcp_connect( host , port ) ;          /* connect to host    */
         ns->bad = (ns->sd < 0) ? TCP_WAIT_CONNECT : 0 ; /* fails? must wait   */
         NI_strncpy(ns->name,host,128) ;                 /* save the host name */
         return ns ;
      }
      return NULL ;  /* should never be reached */
   }

   /***** deal with simple files *****/

   if( strncmp(name,"fil:",4) == 0 ){

      char *fname = name+4 ;
      FILE *fp ;

      if( strlen(name) > 127 || strlen(fname) < 1 ) return NULL ;

      fp = fopen( fname , do_create ? "wb"     /* always in binary mode */
                                    : "rb" ) ;

      if( fp == NULL ) return NULL ;

      /** initialize NI_stream_type output **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_FILE_TYPE;   /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->fp       = fp ;
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;

      ns->buf      = NI_malloc(NI_BUFSIZE) ;
      ns->bufsize  = NI_BUFSIZE ;

      NI_strncpy( ns->name , fname , 128 ) ;

      if( ns->io_mode == NI_INPUT_MODE )     /* save the file size */
         ns->fsize = NI_filesize( fname ) ;  /* if we are reading  */
      else
         ns->fsize = -1 ;

      return ns ;
   }

   /***** str: string array I/O *****/

   if( strncmp(name,"str:",4) == 0 ){

      int nn = strlen(name+4) ;

      if( do_accept && nn < 4 ) return NULL ; /* bad "r" string */

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_STRING_TYPE; /* what kind is this? */
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->npos     = 0 ;             /* scan starts at 0   */

      /* Note that bufsize == nbuf+1 for str:
         This is because we don't count the terminal NUL
         in nbuf (number of readable bytes),
         but do count it in bufsize (size of the buf array) */

      if( do_accept ){               /* read from stuff after str: */
         ns->nbuf    = nn ;
         ns->bufsize = nn+1 ;
         ns->buf     = NI_malloc(nn+1) ;
         strcpy(ns->buf,name+4) ;
      } else {                       /* write to a string */
         ns->nbuf    = 0 ;
         ns->bufsize = 1 ;
         ns->buf     = NI_malloc(1) ; /* 1 byte set to zero */
      }

      strcpy( ns->name , "Elvis" ) ;
      return ns ;
   }

   return NULL ;  /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*!  Check if the given NI_stream is properly opened for I/O.

   If not, wait up to msec milliseconds to establish the connection to
   the other end; if msec < 0, will wait nearly forever.
   Returns 1 if ready; 0 if not; -1 if an error occurs.
   Possible -1 errors are:
     * ns was connected to a socket, and now has become disconnected
     * ns is passed in as NULL (bad user, bad bad bad)
     * ns is reading a file, and we are already at its end
-------------------------------------------------------------------------*/

int NI_stream_goodcheck( NI_stream_type *ns , int msec )
{
   int ii , jj ;
   char * bbb ;

   /** check inputs for OK-osity **/

   if( ns == NULL ) return -1 ;

   switch( ns->type ){

      /** File I/O [there is never any waiting here] **/

      case NI_FILE_TYPE:
        if( ns->fp == NULL ) return -1 ;        /* should never happen */
        if( ns->io_mode == NI_INPUT_MODE )
           return NI_stream_readcheck(ns,0) ;   /* input mode */
        else
           return 1 ;                           /* output mode */

      /** String I/O **/

      case NI_STRING_TYPE:
        if( ns->io_mode == NI_INPUT_MODE )
           return NI_stream_readcheck(ns,0) ;   /* input mode */
        else
           return 1 ;                           /* output mode */

      /** Socket I/O **/

      case NI_TCP_TYPE:
        if( ns->bad == 0 ){  /** if good before, then check if is still good **/
           int ich = 1 ;
           ich = tcp_alivecheck(ns->sd) ;
           if( ich == 0 ) return -1 ;
           return 1 ;
        }

        /** wasn't good before, so check if that condition has changed **/

        /** TCP/IP waiting to accept call from another host **/

        if( ns->bad == TCP_WAIT_ACCEPT ){
           ii = tcp_readcheck(ns->sd,msec) ;            /* see if ready      */
           if( ii > 0 ){                                /* if socket ready:  */
              jj = tcp_accept( ns->sd , NULL,&bbb ) ;   /* accept connection */
              if( jj >= 0 ){                            /* if accept worked  */
                 CLOSEDOWN( ns->sd ) ;                  /* close old socket  */
                 NI_strncpy(ns->name,bbb,128) ;         /* put IP into name  */
                 free(bbb); ns->bad = 0; ns->sd = jj;   /* and ready to go!  */
              }
           }
        }

        /** TCP/IP waiting to connect call to another host **/

        else if( ns->bad == TCP_WAIT_CONNECT ){
           int dms=0 , ms ;

           if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */
           for( ms=0 ; ms < msec ; ms += dms ){
             ns->sd = tcp_connect( ns->name , ns->port );  /* try to connect  */
             if( ns->sd >= 0 ) break ;                     /* worked? get out */
             dms = NEXTDMS(dms); dms = MIN(dms,msec-ms); NI_sleep(dms);
           }
           if( ns->sd < 0 )                                  /* one last try? */
              ns->sd  = tcp_connect( ns->name , ns->port ) ;

           if( ns->sd >= 0 ) ns->bad = 0 ;                   /* succeeded?    */
        }

        /** see if it turned from bad to good **/

        return (ns->bad == 0) ;
   }

   return -1 ;  /* unreachable, I hope */
}

/*-----------------------------------------------------------------------*/
/*! Close a NI_stream.  Note that this will also free what ns points to.

    Use the NI_STREAM_CLOSE macro to call this function and then
    also set the pointer "ns" to NULL.
-------------------------------------------------------------------------*/

void NI_stream_close( NI_stream_type *ns )
{
   if( ns == NULL ) return ;

   switch( ns->type ){

      case NI_STRING_TYPE:   /* nothing to do */
      break ;

      case NI_FILE_TYPE:
        if( ns->fp != NULL ) fclose(ns->fp) ;
      break ;

      case NI_TCP_TYPE:
        if( ns->sd >= 0 ) CLOSEDOWN(ns->sd) ;
      break ;
   }

   NI_free(ns->buf); NI_free(ns); return;
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data read out of it.

  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data is ready, 0 if not;
  -1 will be returned if some unrecoverable error is detected:
    tcp: the socket connection was dropped
    fil: you have reached the end of the file, and are still trying to read.
-----------------------------------------------------------------------------*/

int NI_stream_readcheck( NI_stream_type *ns , int msec )
{
   int ii ;

   switch( ns->type ){

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = NI_stream_goodcheck(ns,0) ;       /* check if it is connected */
        if( ii == -1 ) return -1 ;             /* some error */
        if( ii == 0  ){                        /* not good yet */
           ii = NI_stream_goodcheck(ns,msec) ; /* so wait for it to get good */
           if( ii != 1 ) return ii ;           /* if still not good, exit */
        }
        ii = tcp_alivecheck( ns->sd ) ;        /* see if it is still open  */
        if( !ii ) return -1 ;                  /* if not open, error exit  */
        ii = tcp_readcheck( ns->sd , msec ) ;  /* see if any data is there */
        return ii ;

      /** fil: ==> check current file position and length of file **/

      case NI_FILE_TYPE:{
         long f_len , f_pos ;

         if( ns->fp == NULL                ||
             ns->io_mode == NI_OUTPUT_MODE   ) return -1 ; /* never? */

         f_len = ns->fsize ;               /* length of file      */
         if( f_len < 0 ) return -1 ;       /* file not found (?)  */

         f_pos = ftell( ns->fp ) ;         /* where are we now?   */
         if( f_pos < 0 ) return -1 ;       /* should never happen */

         return (f_pos < f_len) ? 1 : -1 ; /* is good or bad, but */
                                           /* never just neutral  */
      }

      /** str: ==> check current buffer position **/

      case NI_STRING_TYPE:{
         if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* never? */

         return (ns->npos < ns->nbuf) ? 1 : -1 ;  /* is data left? */
      }
   }

   return -1 ;  /* should never happen */
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data written into it.

  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data can be sent, 0 if not;
  -1 will be returned if some unrecoverable error is detected:
    tcp: the socket closed down at the other end
    fil: this should never happen, unless you try to write to
         a readonly NI_stream
-----------------------------------------------------------------------------*/

int NI_stream_writecheck( NI_stream_type *ns , int msec )
{
   int ii ;

   switch( ns->type ){

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = NI_stream_goodcheck(ns,0) ;
        if( ii == -1 ) return -1 ;             /* some error */
        if( ii == 0  ){                        /* not good yet */
           ii = NI_stream_goodcheck(ns,msec);  /* so wait for it to get good */
           if( ii != 1 ) return ii ;           /* if still not good, exit */
        }
        return tcp_writecheck(ns->sd,msec) ;   /* check if we can write bytes */

      /** fil: ==> if the file was opened in write mode **/

      case NI_FILE_TYPE:
        return ( (ns->fp != NULL && ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                                   : -1 ) ;

      /** str: ==> if the string was opened in write mode **/

      case NI_STRING_TYPE:
        return ( (ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                 : -1 ) ;
   }

   return -1 ;  /* should never be reached */
}

/*----------------------------------------------------------------------------*/
/*!  Send nbytes of data from buffer down the NI_stream.

  Return value is the number of bytes actually sent, or is -1 if some error
  occurs (which means that the NI_stream is bad).

  tcp: We use blocking sends, so that all the data should be sent properly
       unless the connection to the other end fails for some reason
       (e.g., the planet explodes in a fiery cataclysm of annihilation).

  fil: Everything should be written, unless the filesystem fills up.
       If nothing at all gets written, -1 is returned.
------------------------------------------------------------------------------*/

int NI_stream_write( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , nsent ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;  /* that was easy */

   ii = NI_stream_writecheck(ns,1) ; /* check if stream is writable */
   if( ii <= 0 ) return ii ;         /* if not, vamoose the ranch */

   switch( ns->type ){

     /** tcp: ==> just use send **/

     case NI_TCP_TYPE:

       /* turn off SIGPIPE signals, which will otherwise be
          raised if we send to a socket when the other end has crashed */

       if( !nosigpipe ){ signal(SIGPIPE,SIG_IGN); nosigpipe = 1; }

       nsent = tcp_send( ns->sd , buffer , nbytes , 0 ) ;
       if( nsent < nbytes ) PERROR("NI_stream_write(send)") ;
       if( nsent == 0 ) nsent = -1 ;
       return nsent ;

     /** fil: ==> just fwrite **/

     case NI_FILE_TYPE:
       nsent = fwrite( buffer , 1 , nbytes , ns->fp ) ;
       if( nsent < nbytes ) PERROR("NI_stream_write(fwrite)") ;
       if( nsent == 0 ) nsent = -1 ;
       return nsent ;

     /** str: ==> append to buffer in stream struct **/

     case NI_STRING_TYPE:
        ns->buf = NI_realloc( ns->buf , ns->bufsize+nbytes ) ;
        memcpy( ns->buf+ns->nbuf , buffer , nbytes ) ;
        ns->nbuf    += nbytes ; ns->buf[ns->nbuf] = '\0' ;
        ns->bufsize += nbytes ;
        return nbytes ;
   }

   return -1 ;  /* should not be reached */
}

/*-------------------------------------------------------------------------*/
/*!  Read up to nbytes of data from the NI_stream, into buffer.

   Returns the number of bytes actually read.  For both the case of
   sockets and files, this may be less than nbytes (may even be 0).
   If an error occurs and no data is read, -1 is returned.

   For tcp: streams, if no data is available, this function will wait until
   something can be read.  If this behavior is undesirable, then you should
   use NI_stream_readcheck() before calling this function in order to see
   if any data is available.

   For fil: streams, this function simply tries to read from the file.
   Whether or not it succeeds, it will return immediately. It should
   never return -1; if it returns 0, this means end-of-file.
---------------------------------------------------------------------------*/

int NI_stream_read( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;

   switch( ns->type ){

     /** tcp: just use recv **/

     case NI_TCP_TYPE:
       ii = NI_stream_goodcheck(ns,1) ;
       if( ii != 1 ) return ii ;
       ii = tcp_recv( ns->sd , buffer , nbytes , 0 ) ;
       if( ii == -1 ) PERROR("NI_stream_read(recv)") ;
       return ii ;

     /** fil: just use fread **/

     case NI_FILE_TYPE:
       if( ns->fp == NULL || ns->io_mode == NI_OUTPUT_MODE ) return -1 ;
       ii = fread( buffer , 1 , nbytes , ns->fp ) ;
       return ii ;

     /** str: copy bytes out of the buffer string **/

     case NI_STRING_TYPE:
       if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* bad stream */
       ii = ns->nbuf - ns->npos ;                      /* how much is left */
       if( ii <= 0                       ) return -1 ; /* no data left */
       if( ii > nbytes ) ii = nbytes ;                 /* amount to copy */
       memcpy( buffer , ns->buf+ns->npos , ii ) ;      /* copy it */
       ns->npos += ii ;                                /* advance position */
       return ii ;
   }

   return -1 ;  /* should not be reached */
}

/*-----------------------------------------------------------------------*/
/*! Try to fill up the stream's input buffer.
    Don't call this function until NI_stream_goodcheck() is 1!

    minread = Minimum number of bytes to read.
              Will wait until we get at least this many,
              until the stream is bad or the buffer is full.
              If minread=0, then may read nothing (but will try).

    msec    = Maximum amount of time to wait to satisfy minread,
              in milliseconds.  If msec<0, will wait nearly forever.
              If msec=0, will return after 1st read attempt, even
              if nothing was obtained.

    Returns number of bytes read (-1 if input stream goes bad before
    any data is read).  If the input stream goes bad AFTER some data
    is read, there is no indication of that (until the next time
    you call this, of course).
-------------------------------------------------------------------------*/

static int NI_stream_fillbuf( NI_stream_type *ns, int minread, int msec )
{
   int nn , ii , ntot=0 , ngood=0 , mwait=0 ;
   int start_msec = NI_clock_time() ;

   if( NI_stream_goodcheck(ns,0) <= 0 ) return -1 ; /* bad input */

   if( ns->type == NI_STRING_TYPE ) return 0 ;      /* goofy input */

   if( ns->nbuf >= ns->bufsize ) return 0 ; /* buffer already full */

   if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */

   /* read loop */

   while(1){

      ngood = NI_stream_readcheck(ns,mwait); /* check if data can be read */

      if( ngood < 0 ) break ;                /* data stream gone bad, so exit */

      if( ngood > 0 ){                       /* we can read! */

         ii = NI_stream_read( ns, ns->buf+ns->nbuf, ns->bufsize-ns->nbuf ) ;

         if( ii > 0 ){                 /* we got data! */
            ns->nbuf += ii ;           /* buffer is now longer */
            ntot     += ii ;           /* total bytes read here so far */

            /* if buffer is full,
               or we have all the data that was asked for, then exit */

            if( ns->nbuf >= ns->bufsize || ntot >= minread ) break ;

         } else if( ii < 0 ){          /* stream suddenly died horribly? */
            ngood = -1 ; break ;
         }
      }

      /* if we don't require data, then exit no matter what our status is */

      if( minread <= 0 ) break ;

      /* if the max time has elapsed, then exit */

      if( NI_clock_time()-start_msec >= msec ) break ;

      /* otherwise, sleep a little bit before trying again */

      mwait++ ; if( mwait > 999 ) mwait = 999 ;
   }

   /* if didn't get any data, and
      if the NI_stream was bad, return -1 as a flag of displeasure */

   if( ntot == 0 && ngood < 0 ) ntot = -1 ;

   return ntot ;  /* otherwise, return # of bytes read (may be 0) */
}

/*-----------------------------------------------------------------*/
/*! Set the binary threshold size for NI_write_element.

    If a data element takes up more than 'size' bytes, then it
    will be written in binary form, otherwise in text form.
    If size=0, then all elements are written in binary.
    If size<0, then all elements are written in text.
    This function only affects what happens when you write
    data elements.  Reading is controlled by the contents of
    each element header (i.e., the ni_form attribute).
-------------------------------------------------------------------*/

void NI_binary_threshold( NI_stream_type *ns , int size )
{
   if( ns == NULL ) return ;
   ns->bin_thresh = size ;
   return ;
}

/**********************************************************************/
/******* Functions to read and write data and group elements. *********/
/**********************************************************************/

/* internal prototypes */

static int decode_one_double( NI_stream_type *, double * ) ;
static int scan_for_angles( NI_stream_type *, int ) ;
static void reset_buffer( NI_stream_type * ) ;

#define clear_buffer(ns) ( (ns)->nbuf = (ns)->npos = 0 )

/*--------------------------------------------------------------------*/
/*! Read an element (maybe a group) from the stream.

   Return is NULL if nothing can be read at this time.  Otherwise,
   use NI_element_type(return value) to determine if the element
   read is a data element or a group element.

   Note that a header that is longer than ns->bufsize will
   never be read properly, since we must have the entire header in
   the buffer before processing it.  This should only be a problem
   for deranged users.  If such a vast header is encountered, it
   will be flushed.

   If header start '<' and stop '>' are encountered, then this
   function will read data until it can create an element, or until
   the data stream is bad (i.e., the file ends, or the socket closes).

   If NULL is returned, that can be because there is no data to
   read even in the buffer, or because the input data stream has gone
   bad (i.e., will return no more data ever).  To check for the latter
   case, use NI_stream_readcheck().

   This code does not yet support String or Line input - only the
   numeric input types are implemented!
----------------------------------------------------------------------*/

void * NI_read_element( NI_stream_type *ns )
{
   int ii,nn,nhs , num_restart ;
   char *cstart , *cstop ;
   header_stuff *hs ;

   if( ns == NULL ) return NULL ;  /* bad input */

   /*-- Try to find the element header --*/

   num_restart = 0 ;
HeadRestart:                            /* loop back here to retry */
   num_restart++ ;
   if( num_restart > 5 ) return NULL ;  /* don't allow too many loops */

   nn = scan_for_angles( ns , 1 ) ;     /* look for '<stuff>' */

   if( nn < 0 ) return NULL ;           /* found nothing */

   /* ns->buf[ns->npos] = opening '<' ; ns->buf[nn-1] = closing '>' */

   /* see if we found '<>', which is illegal,
      or a trailer '</stuff>', which is also illegal (here) */

   if( nn - ns->npos <= 2 || ns->buf[ns->npos+1] == '/' ){
      ns->npos = nn; reset_buffer(ns); /* toss the '<..>', try again */
      goto HeadRestart ;
   }

   /*----- Parse the header data and prepare to make an element! -----*/

   hs = parse_header_stuff( nn - ns->npos , ns->buf + ns->npos , &nhs ) ;

   if( hs == NULL ){  /* something bad happened there */
      ns->npos = nn; reset_buffer(ns); /* toss the '<..>', try again */
      goto HeadRestart ;
   }

   /*----- If here, have parsed a header (and will not HeadRestart).
           First, expunge the data bytes that were consumed to make
           the header; that is, we can then start reading data from
           ns->buf[ns->npos] .. ns->buf[ns->nbuf-1]                 --*/

   ns->npos = nn ;

   /*--------------- Now make an element of some kind ---------------*/

   if( strcmp(hs->name,"ni_group") == 0 ){  /*--- a group element ---*/

      NI_group *ngr ;
      void *nini ;

      ngr = make_empty_group_element( hs ) ;
      destroy_header_stuff( hs ) ;

      /* we now have to read the elements within the group */

      num_restart = 0 ;
      while(1){           /* loop to find an element */

         nn = scan_for_angles( ns , 10 ) ;  /* find header/trailer '<...>' */

         if( nn <= 0 ){  /* didn't find it */
            if( num_restart > 9 || NI_stream_readcheck(ns,0) < 0 ) break ;
            num_restart++ ;
            continue ;        /* try again (but not forever) */
         }

         /* check if we found a trailer element '</stuff>' */

         if( ns->buf[ns->npos+1] == '/' ){  /* trailer */
            ns->npos = nn ;                 /* so end the group */
            break ;
         }

         /* try to make an element out of it */

         nini = NI_read_element( ns ) ;
         if( nini != NULL ){
            NI_add_to_group( ngr , nini ) ;
            num_restart = 0 ;
         } else {
            if( num_restart > 9 || NI_stream_readcheck(ns,0) < 0 ) break ;
            num_restart++ ;
         }
      }

      /* and we are done */

      return ngr ;

   } /* end of reading group element */

   else { /*---------------------- a data element -------------------*/

      NI_element *nel ;
      int form, swap, nbrow , row,col ;

      nel = make_empty_data_element( hs ) ;
      destroy_header_stuff( hs ) ;

      if( nel == NULL          ||     /* nel == NULL should never happen. */
          nel->vec_len == 0    ||     /* These other cases are indication */
          nel->vec_num == 0    ||     /* that this is an 'empty' element. */
          nel->vec_typ == NULL ||     /* ==> The header is all there is.  */
          nel->vec     == NULL   ) return nel ;

      /*-- If here, must read data from the buffer into nel->vec --*/

      /* Find the form of the input */

      form = NI_TEXT_MODE ; /* default is text mode */
      swap = 0 ;            /* and (obviously) don't byte swap */

      ii = string_index( "ni_form" , nel->attr_num , nel->attr_lhs ) ;

      if( ii >= 0 && nel->attr_rhs[ii] != NULL ){ /* parse ni_form=rhs */

         /* at present, the only non-text mode is "binary" */

         if( strstr(nel->attr_rhs[ii],"bin") != NULL ){
            int order=NI_MSB_FIRST ; /* default input byteorder */

            form = NI_BINARY_MODE ;

            /* check byteorder in header vs. this CPU */

            if( strstr(nel->attr_rhs[ii],"lsb") != NULL ) order = NI_LSB_FIRST;

            swap = ( order != NI_byteorder() ) ;  /* swap bytes? */
         }
      }

      /*-- Now must actually read data and put it somewhere (oog). */

      row = 0 ;         /* next index in vectors to fill */

      nbrow = NI_element_rowsize( nel ) ; /* how many bytes for one row */

      switch( form ){

        default:  break ;   /* bad!!! should never ever happen */

        /*......................................................*/

        case NI_BINARY_MODE:
         while( row < nel->vec_len ){  /* loop over input rows */

           /* if not enough data left in buffer for 1 row
              of data, then try to read more data into the buffer */

           if( ns->nbuf-ns->npos < nbrow ){

             reset_buffer(ns) ;  /* discard used up data in buffer */

             /* now read at least enough to fill one data row,
                waiting forever if need be (or until the stream goes bad) */

             (void) NI_stream_fillbuf( ns , nbrow-ns->nbuf , -1 ) ;

             /* if still don't have a full row of data,
                something bad has happened (end-of-file? closed socket?);
                so put what pitiful data we have into the vectors and exit */

             if( ns->nbuf-ns->npos < nbrow ){
               if( ns->nbuf-ns->npos > 0 ){   /* if we have any data at all */
                 char *qbuf = NI_malloc( sizeof(char)*nbrow ) ;
                 memcpy( qbuf , ns->buf+ns->npos , ns->nbuf-ns->npos ) ;
                 NI_fill_vector_row( nel , row , qbuf ) ; row++ ;
                 NI_free(qbuf) ;
               }
               clear_buffer(ns) ;   /* buffer is used up now */
               goto BinaryDone ;    /* and break out of loop */
             }
           }

           /* normal case: have (at least) a full row of data bytes,
                           so put them into the vectors, and loop    */

           NI_fill_vector_row( nel , row , ns->buf+ns->npos ) ;
           ns->npos += nbrow ;  /* we used up this many bytes */
           row++ ;          /* we filled this row */

         } /* end of loop over vector rows */

BinaryDone:
         nel->vec_filled = row ;  /* how many rows were filled above */

        break ;  /* end of binary input */

        /*......................................................*/

        case NI_TEXT_MODE:{

fprintf(stderr,"starting text mode read\n") ;

         while( row < nel->vec_len ){  /* loop over input rows */
          for( col=0 ; col < nel->vec_num ; col++ ){ /* over input vectors */

fprintf(stderr,"  row=%d col=%d\n",row,col) ;

            /* decode one value from input, according to its type */

            switch( nel->vec_typ[col] ){
              default:                    /* unimplemented types */
              break ;                     /* (String and Line)   */

              /* only numeric types are implemented so far */

              case NI_BYTE:{
                 double val ;
                 byte *vpt = (byte *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (byte) val ;
              }
              break ;

              case NI_SHORT:{
                 double val ;
                 short *vpt = (short *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (short) val ;
              }
              break ;

              case NI_INT:{
                 double val ;
                 int *vpt = (int *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (int) val ;
              }
              break ;

              case NI_FLOAT:{
                 double val ;
                 float *vpt = (float *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (float) val ;
              }
              break ;

              case NI_DOUBLE:{
                 double val ;
                 double *vpt = (double *) nel->vec[col] ;
                 nn = decode_one_double( ns , &val ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (double) val ;
              }
              break ;

              case NI_COMPLEX:{
                 double v1,v2 ;
                 complex *vpt = (complex *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (float) v1 ;
                 vpt[row].i = (float) v2 ;
              }
              break ;

              case NI_RGB:{
                 double v1,v2,v3 ;
                 rgb *vpt = (rgb *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v3 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
              }
              break ;

              case NI_RGBA:{
                 double v1,v2,v3,v4 ;
                 rgba *vpt = (rgba *) nel->vec[col] ;
                 nn = decode_one_double( ns , &v1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v2 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v3 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = decode_one_double( ns , &v4 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
                 vpt[row].a = (byte) v4 ;
              }
              break ;
            } /* end of switch on type of this data value */

          } /* end of loop over vector columns */

          row++ ;
         } /* end of loop over vector rows */

TextDone:
         nel->vec_filled = row ;  /* how many rows were filled above */
        }
        break ;  /* end of text input */

      } /* end of reading data into the element */

      /*-- At this point, have finished reading into the element
           vectors, and the next character to process in the
           NI_stream input buffer is at location index ns->npos.  --*/

      /*-- Now swap bytes, if needed. --*/

      if( swap ){
        for( col=0 ; col < nel->vec_num ; col++ )
          NI_swap_vector( nel->vec_typ[col], nel->vec_len, nel->vec[col] ) ;
      }

      /*-- Now scan for the end-of-element marker '</something>' and
           skip all input bytes up to (and including) the final '>'. --*/

      num_restart = 0 ;
TailRestart:
      num_restart++ ;

      if( num_restart < 99 ){  /* don't loop forever, dude */
         int is_tail ;

         nn = scan_for_angles( ns , 10 ) ;  /* find '<...>' */

         /* if we didn't find '<...>' at all,
            then if the I/O stream is bad, just exit;
            otherwise, try scanning for '<...>' again */

         if( nn < 0 ){
           if( NI_stream_readcheck(ns,0) < 0 ) return nel ;
           goto TailRestart ;
         }

         /* we have '<...>', but make sure it starts with '</' */

         is_tail = ( ns->buf[ns->npos+1] == '/' ) ;

         if( !is_tail ){                       /* no '/'? */
           ns->npos = nn ; reset_buffer(ns) ;  /* skip '<...>' */
           goto TailRestart ;                  /* and try again */
         }
      }

      /*-- And are done with the input stream and the data element! --*/

      return nel ;

   } /* end of reading data element */

   return NULL ; /* should never be reached */
}

/*----------------------------------------------------------------------*/
/*! From the NI_stream ns, starting at buffer position ns->npos, decode
    one number into *val.  Return value of this function is 1 if
    we succeeded, 0 if not.  ns->npos will be altered to reflect the
    current buffer position (one after the last character processed)
    when all is done.
------------------------------------------------------------------------*/

#undef  NVBUF
#define NVBUF 127  /* max num chars for one number */

#define IS_USELESS(c) ( isspace(c) || iscntrl(c) )

static int decode_one_double( NI_stream_type *ns, double *val )
{
   int epos , num_restart, need_data, nn ;
   char vbuf[NVBUF+1] ;                    /* number string from buffer */

fprintf(stderr,"   enter decode_one_double\n") ;

   /*-- check inputs for stupidness --*/

   if( ns == NULL || val == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data for a number ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ;
fprintf(stderr,"    num_restart=%d\n",num_restart) ;
   if( num_restart > 19 ) return 0 ;  /*** give up ***/

   /*-- advance over useless characters in the buffer --*/

   while( ns->npos < ns->nbuf && IS_USELESS(ns->buf[ns->npos]) ) ns->npos++ ;

   /*-- check if we ran into the closing '<' prematurely
        (before any useful characters); if we did, then we are done --*/

fprintf(stderr,"    npos=%d  nbuf=%d\n",ns->npos,ns->nbuf) ;

   if( ns->npos < ns->nbuf && ns->buf[ns->npos] == '<' ) return 0 ;

   /*-- if we need some data, try to get some --*/

   need_data = (ns->nbuf-ns->npos < 2) ; /* need at least 2 unused bytes */

   /*-- An input value is decoded from a string of non-useless
        characters delimited by a useless character (or by the
        element closing '<').
        Note that the 1st character we are now at is non-useless.
        Scan forward to see if we have a useless character later. --*/

   if( !need_data ){  /* so have at least 2 characters */

      for( epos=ns->npos+1 ; epos < ns->nbuf ; epos++ )
        if( ns->buf[epos] == '<' || IS_USELESS(ns->buf[epos]) ) break ;

      /*- epos is either the delimiter position, or the end of data bytes -*/

      need_data = (epos == ns->nbuf) ; /* no delimiter ==> need more data */

      /*- If the string of characters we have is not delimited,
          and it is too long to be a number, throw out all the
          data in the buffer and quit.                         -*/

      if( need_data && epos-ns->npos > NVBUF ){ clear_buffer(ns); return 0; }
   }

   /*-- read more data now if it is needed --*/

   if( need_data ){

      reset_buffer(ns) ; /* discard used up data in buffer */

      /*- read at least 1 byte,
          waiting up to 66 ms (unless the data stream goes bad) -*/

      nn = NI_stream_fillbuf( ns , 1 , 66 ) ;

      if( nn >= 0 ) goto Restart ;  /* check if buffer is adequate now */

      /*- if here, the stream went bad.  If there are still
          data bytes in the stream, we can try to interpret them.
          Otherwise, must quit without success.                  -*/

      if( ns->nbuf == 0 ){ ns->npos=0; return 0; }  /* quitting */

      epos = ns->nbuf ;
   }

   /*-- if here, try to interpret data bytes ns->npos .. epos-1 --*/

   nn = epos-ns->npos ; if( nn > NVBUF ) nn = NVBUF ;     /* # bytes to read   */
   memcpy( vbuf, ns->buf+ns->npos, nn ); vbuf[nn] = '\0'; /* put bytes in vbuf */
   sscanf( vbuf , "%lf" , val ) ;                         /* interpret them    */
   ns->npos = epos ; return 1 ;                           /* retire undefeated */
}

/*----------------------------------------------------------------------*/
/*! Reset the unscanned bytes in the buffer to start at position 0
    instead of position ns->npos; then set ns->npos to 0.
------------------------------------------------------------------------*/

static void reset_buffer( NI_stream_type *ns )
{
   if( ns == NULL || ns->npos <= 0 || ns->nbuf <= 0 ) return ;

   if( ns->npos < ns->nbuf ){           /* haven't used up all data yet */
      memmove( ns->buf, ns->buf+ns->npos, ns->nbuf-ns->npos ) ;
      ns->nbuf -= ns->npos ;
   } else {
      ns->nbuf = 0 ;                   /* all data in buffer is used up */
   }
   ns->npos = 0 ;               /* further scanning starts at beginning */
}

/*----------------------------------------------------------------------*/
/*! Scan stream for an element header or trailer:'<characters>',
    starting at byte offset ns->npos, and waiting msec milliseconds.

    Returns with the stream buffer set so that the opening '<' is at
    ns->buf[ns->npos] and the closing '>' is at ns->buf[q-1], where q
    is this function's return value.  Note that read operations may
    change ns->npos from its input value.

    If the return value is -1, then we couldn't find a '<stuff>' string.
    This may be due to:
      * there is no '<...>' in the buffer, and we can't read from
         the input stream; call NI_readcheck(ns,0) to confirm this
      * time ran out (alas)
      * The '<...' part filled the entire buffer (64K).  In this case,
         all the input buffer is thrown away - we don't support
         headers or trailers this long!
------------------------------------------------------------------------*/

static int scan_for_angles( NI_stream_type *ns, int msec )
{
   int nn, epos, need_data, num_restart ;
   char goal ;
   int start_time = NI_clock_time() , mleft , nbmin ;

   if( ns == NULL ) return -1 ;  /* bad input */

   epos = ns->npos ;

        if( msec <  0 ) msec = 999999999 ;   /* a long time (11+ days) */
   else if( msec == 0 ) msec = 1 ;           /* allow a little waiting */

   /*-- Will loop back here if we have to re-read/re-scan --*/

   goal        = '<' ;  /* first goal is opening '<' (second goal is '>') */
   num_restart = 0   ;
Restart:                                       /* loop back here to retry */
   num_restart++ ;
   mleft = msec - (NI_clock_time()-start_time) ;             /* time left */

   if( num_restart > 3 && mleft <= 0 ){                        /* failure */
      reset_buffer(ns) ;                               /* and out of time */
      return -1 ;
   }

   /*-- skip ahead to find goal in the buffer --*/

   while( epos < ns->nbuf && ns->buf[epos] != goal ) epos++ ;

   /*-- if we found our goal, do something about it --*/

   if( epos < ns->nbuf ){

     /*-- if our goal was the closing '>', we are done! --*/

     if( goal == '>' ) return epos+1 ;  /* marks the character after '>' */

     /*-- if here, our goal was the opening '<';
          set the buffer position to this location,
          set the new goal, and scan for the new goal --*/

      ns->npos = epos ;  /* mark where we found '<' */
      goal     = '>'  ;  /* the new goal */
      goto Restart    ;  /* scan again! */
   }

   /*-- if we get to here, we didn't find our goal:
        (a) if the goal was the opening '<', then throw
             away all data in the buffer, and get some more data
        (b) if the goal was the closing '>', then we need more data
            in the buffer, but need to keep the existing data
        (c) UNLESS the buffer is full
             - in this case, the universe ends right here and now --*/

   if( goal == '<' ){                   /* case (a) */
      ns->nbuf = ns->npos = epos = 0 ;
   } else if( ns->nbuf < ns->bufsize ){  /* case (b) */
      reset_buffer(ns) ; epos = 0 ;
   } else {                             /* case (c) */
      ns->nbuf = 0 ; return -1 ;
   }

   /*-- if we are here, we need more data before scanning again --*/

   /*-- read at least nbmin bytes,
        waiting up to mleft ms (unless the data stream goes bad) --*/

   if( mleft <= 0 ) mleft = 1 ;
   nbmin = (goal == '<') ? 3 : 1 ;

   nn = NI_stream_fillbuf( ns , nbmin , mleft ) ;

   if( nn >= 0 ) goto Restart ; /* scan some more */

   /*-- if here, the stream went bad, so exit --*/

   ns->nbuf = ns->npos = 0 ; return -1 ;
}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

int main( int argc , char *argv[] )
{
   NI_stream ns ;
   char *str ;
   int nn , tt ;
   void *nini ;

   if( argc < 2 ) exit(0) ;
   nn = strlen(argv[1]) ; if( nn < 1 ) exit(0) ;
   str = NI_malloc(nn+8) ;
   strcpy(str,"str:") ; strcat(str,argv[1]) ;

   ns = NI_stream_open( str , "r" ) ;
   if( ns == NULL ){
      fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
   }

   nini = NI_read_element( ns ) ;
   if( nini == NULL ){
      fprintf(stderr,"NI_read_element fails\n") ; exit(1) ;
   }

   tt = NI_element_type( nini ) ;

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;
      fprintf(stderr,"Data element:\n"
                     "  name       = %s\n"
                     "  vec_num    = %d\n"
                     "  vec_len    = %d\n"
                     "  vec_filled = %d\n"
                     "  attr_num   = %d\n" ,
          nel->name,nel->vec_num,nel->vec_len,nel->vec_filled,nel->attr_num );
       for( nn=0 ; nn < nel->attr_num ; nn++ )
          fprintf(stderr,"  %2d: lhs=%s  rhs=%s\n",
                  nn , nel->attr_lhs[nn] , nel->attr_rhs[nn] ) ;
   } else {
      NI_group *ngr = (NI_group *) nini ;
      fprintf(stderr,"Group element:\n"
                     "  part_num = %d\n"
                     "  attr_num = %d\n" ,
              ngr->part_num , ngr->attr_num ) ;
       for( nn=0 ; nn < ngr->attr_num ; nn++ )
          fprintf(stderr,"  %2d: lhs=%s  rhs=%s\n",
                  nn , ngr->attr_lhs[nn] , ngr->attr_rhs[nn] ) ;
   }

   exit(0) ;
}
