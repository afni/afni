#include "niml_private.h"

/****************************************************************************/
/****************** Functions to process a NIML header **********************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Deallocate a header_stuff struct.
----------------------------------------------------------------------------*/

void destroy_header_stuff( header_stuff *hs )
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
/*! Find an isolated string in the input array of char.

    - nst = start position
    - nch = total number of data bytes
    - ch  = array of data bytes

    Return value is an intpair with the .i component indicating the
    start position of the string in the data and the .j indicating
    the byte AFTER the end of the string.  If the .i component is
    negative, then no string was found.
---------------------------------------------------------------------------*/

intpair find_string( int nst, int nch, char *ch )
{
   intpair ans = {-1,-1} ;  /* default answer ==> nothing found */
   int ii,jj ;
   char quot ;

#ifdef NIML_DEBUG
NI_dpr("  ENTER find_string: nst=%d nch=%d\n",nst,nch) ;
#endif

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

    - ndat   = number of data bytes input
    - dat    = data bytes input
    - *nused = output number of bytes consumed (=index of byte AFTER the closing '>').

    Return value is a pointer to a header_stuff struct;
    if NULL is returned, something real bad happened (and *nused won't
    be assigned).
----------------------------------------------------------------------------*/

header_stuff * parse_header_stuff( int ndat, char *dat, int *nused )
{
   header_stuff *hs ; /* return value */
   int id,jd , nn ;
   intpair ss ;

   if( ndat < 2 || dat == NULL ) return NULL ;        /* bad input */

#ifdef NIML_DEBUG
NI_dpr("ENTER parse_header_stuff: %.*s\n",ndat,dat) ;
#endif

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

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: name = %s\n",hs->name) ;
#endif

   /* start scanning for next string at location id */

   id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   /* find and assign attribute strings */

   while(1){

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: scan start at id=%d\n",id) ;
#endif

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

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

      /* extend size of attribute arrays */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      /* this is the LHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->lhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->lhs[hs->nattr] , dat+ss.i , nn+1 ) ;
      unescape_inplace( hs->lhs[hs->nattr] ) ;

      hs->rhs[hs->nattr] = NULL ;             /* in case there is no RHS */

      id = ss.j ;
      if( id >= ndat ) break ;                      /* end of input ? */
      if( IS_QUOTE_CHAR(dat[id]) ) id++ ;           /* skip close quote */
      while( id < ndat && isspace(dat[id]) ) id++ ; /* skip blanks */
      if( id >= ndat ) break ;                      /* end of input ? */

      if( dat[id] != '=' ){                   /* no '=' means no RHS */
         (hs->nattr)++ ;                      /* count the LHS and */
         continue ;                           /* go get next attribute */
      }

      id++ ;                                        /* skip the '=' */
      while( id < ndat && isspace(dat[id]) ) id++ ; /* skip blanks */
      if( id >= ndat ) break ;                      /* end of input ? */

      /* find next string (the RHS) */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

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
/*! Decode a single type field.  Return value is an intpair with
    the .i component being the type code and the .j component being
    the number of characters consumed.  If the .i component is -1,
    then no legal type was found (and .j will be 1).
----------------------------------------------------------------------*/

intpair decode_type_field( char *tf )
{
   intpair ans = {-1,1} ;  /* default answer */

   /* check input for goodness */

   if( tf == NULL ) return ans ;  /* should never happen */

   /* check if tf[0] starts a full datum name,
      or if it is just an initial              */

   switch( tf[0] ){

      default: break ;  /* not a legal datum character */

      case 'b':
        ans.i = NI_BYTE ;
        if( strncmp(tf,"byte"   ,4) == 0 ) ans.j = 4 ;
      break ;

      case 's':
        ans.i = NI_SHORT ;
        if( strncmp(tf,"short"  ,5) == 0 ) ans.j = 5 ;
      break ;

      case 'i':
        ans.i = NI_INT ;
        if( strncmp(tf,"int"    ,3) == 0 ) ans.j = 3 ;
      break ;

      case 'f':
        ans.i = NI_FLOAT ;
        if( strncmp(tf,"float"  ,5) == 0 ) ans.j = 5 ;
      break ;

      case 'c':
        ans.i = NI_COMPLEX ;
        if( strncmp(tf,"complex",7) == 0 ) ans.j = 7 ;
      break ;

      case 'd':
        ans.i = NI_DOUBLE ;
        if( strncmp(tf,"double" ,6) == 0 ) ans.j = 6 ;
      break ;

      case 'r':
        ans.i = NI_RGB ;
        if( strncmp(tf,"rgb"    ,3) == 0 ) ans.j = 3 ;
      break ;

      case 'R':
        ans.i = NI_RGBA ;
        if( strncmp(tf,"RGBA"   ,4) == 0 ) ans.j = 4 ;
      break ;

      case 'S':
        ans.i = NI_STRING ;
        if( strncmp(tf,"STRING" ,6) == 0 ) ans.j = 6 ;
      break ;
   }

   return ans ;
}

/*--------------------------------------------------------------------*/
/*! Decode a single string into a bunch of strings, separated
    by characters from the list in sep.
    - Passing sep in as NULL means to use "," as the separator.
    - In each sub-string, leading and trailing blanks will be excised.
    - This can result in 0 length strings (e.g., "1,,2," will result
    - in the second and fourth output strings having 0 length).
----------------------------------------------------------------------*/

str_array * decode_string_list( char *ss , char *sep )
{
   str_array *sar ;
   int num , nn,id,jd , lss ;

   if( ss == NULL || ss[0] == '\0' ) return NULL ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */

   sar = NI_malloc(sizeof(str_array)) ;  /* create output */
   sar->num = 0 ; sar->str = NULL ;

   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator [or a space - 10 Dec 2002] */

      while( id < lss && strchr(sep,ss[id]) == NULL && !isspace(ss[id]) ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */

      /* sub-string runs from ss[jd] to ss[id-1] */

      sar->str = NI_realloc( sar->str , sizeof(char)*(num+1) ) ;

      nn = id-jd ;                                   /* length of sub-string */
#if 0
      while( nn > 0 && isspace(ss[jd+nn-1]) ) nn-- ; /* clip trailing blanks */
#endif
      sar->str[num] = NI_malloc(nn+1) ;              /* make output string  */
      if( nn > 0 ) memcpy(sar->str[num],ss+jd,nn) ;  /* copy sub-string    */
      sar->str[num++][nn] = '\0' ;                   /* terminate output  */

      id++ ;                                         /* skip separator  */
   }

   sar->num = num ; return sar ;
}

/*--------------------------------------------------------------------*/
/*! Decode a ni_dimen string into an array of integers.
  Returns NULL if the input is bad bad bad.
----------------------------------------------------------------------*/

int_array * decode_dimen_string( char *ds )
{
   int num , dd,nn,id,jd , lds ;
   int_array *iar ;

   if( ds == NULL || ds[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(int_array)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan string for integers */

   num = id = 0 ;
   lds = NI_strlen(ds) ;
   do{
      /* skip ahead until ds[id] is a digit */

      while( id < lds && !isdigit(ds[id]) ) id++ ;
      if( id == lds ) break ;                      /* end of input */

      /* decode integer starting here */

      nn = jd = 0 ;
      sscanf( ds+id , "%d%n" , &jd , &nn ) ;       /* get the count */
      if( jd <= 0 || nn <= 0 ) break ;             /* something bad */
      id += nn ;                                   /* skip these chars */

      /* extend output array, store new dimension in it */

      iar->ar = NI_realloc( iar->ar , sizeof(int)*(num+1) ) ;
      iar->ar[num++] = jd ;
   } while(1) ;

   if( num == 0 ){ NI_free(iar); return NULL; }    /* bad */

   iar->num = num ; return iar ;
}

/*--------------------------------------------------------------------*/
/*! Decode a data type string into an array of integer codes.
  Returns NULL if the input is bad bad bad.
----------------------------------------------------------------------*/

int_array * decode_type_string( char *ts )
{
   int num, typ, lts, id,jd, nn,kk ;
   int_array *iar ;
   intpair dc ;

   if( ts == NULL || ts[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(int_array)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan type string to find counts/fields and add to output */

   lts = NI_strlen(ts) ;
   num = 0 ;            /* will be count of fields */

   for( id=kk=0 ; id < lts ; ){  /* loop over input string */

      if( isdigit(ts[id]) ){   /* a count prefix */
         jd = nn = 0 ;
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         if( jd <= 0 || nn <= 0 ){                /* shouldn't happen */
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ;
         }
         id += nn ;                  /* skip count prefix characters */
         if( ts[id] == '*' ) id++ ;  /* allow for "3*float" */
      } else {
         jd = 1 ;       /* default count of 1 */
      }

      dc = decode_type_field( ts+id ) ;

      /* dc.i = type code; dc.j = character count used to get type code */

      id += dc.j ;              /* skip these characters */
      if( dc.i < 0 ) continue ; /* bad type code */

      num += jd ;               /* this many fields so far */

      /* extend output array length */

      iar->ar = NI_realloc( iar->ar , sizeof(int)*num ) ;

      /* put values into output array */

      for( nn=0 ; nn < jd ; nn++ ) iar->ar[kk++] = dc.i ;

   } /* end of loop over input string */

   /* nothing found? */

   if( num <= 0 ){
      NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
   }

   iar->num = num ; return iar ;
}

/*-----------------------------------------------------------------------*/
/* Given a type code, return the character code.
-------------------------------------------------------------------------*/

char NI_type_char( int typ )
{
   switch( typ ){
        case NI_BYTE:     return 'b' ;
        case NI_SHORT:    return 's' ;
        case NI_INT:      return 'i' ;
        case NI_FLOAT:    return 'f' ;
        case NI_DOUBLE:   return 'd' ;
        case NI_COMPLEX:  return 'c' ;
        case NI_RGB:      return 'r' ;
        case NI_RGBA:     return 'R' ;
        case NI_STRING:   return 'S' ;
        default:          return '\0';
   }
}

/*--------------------------------------------------------------------*/

int     typedef_nib = 0    ; /*!< Block ni_ typedefs? */
int     typedef_num = 0    ; /*!< Number of typedefs. */
char ** typedef_nam = NULL ; /*!< Names of typedefs.  */
char ** typedef_typ = NULL ; /*!< Types of typedefs.  */
char ** typedef_dim = NULL ; /*!< Dimens of typedefs. */

/*--------------------------------------------------------------------*/
/*! Implement typedef-ing.
      - name = name string for new type
      - type = type string for new type
      - dimen= dimen string for new type (can be NULL)

    The routine will fail (silently) if name or type is NULL, or
    if name is the same as an existing typedef name, or if the
    type string is indecipherable.
----------------------------------------------------------------------*/

void NI_typedef( char *name , char *type , char *dimen )
{
   int nn ;
   int_array *ar ;

   if( name == NULL || name[0] == '\0' ||
       type == NULL || type[0] == '\0'   ) return ; /* bad input */

   /* check if we allow name to start with "ni_" */

   if( typedef_nib && strncmp(name,"ni_",3) == 0 ) return ;

   /* search for name in current list */

   for( nn=0 ; nn < typedef_num ; nn++ )
      if( strcmp(name,typedef_nam[nn]) == 0 ) break ;

   /* found it ==> error exit */

   if( nn < typedef_num ) return ;

   /* check type string for integrity */

   ar = decode_type_string( type ) ;
   if( ar == NULL || ar->num == 0 ){ NI_free(ar); return; } /* bad */
   NI_free(ar->ar) ; NI_free(ar) ;                          /* OK */

   /* check dimen string (if any) for integrity */

   if( dimen != NULL ){
     ar = decode_dimen_string( dimen ) ;
     if( ar == NULL || ar->num == 0 ){ NI_free(ar); return; } /* bad */
     NI_free(ar->ar) ; NI_free(ar) ;                          /* OK */
   }

   /* add to typedef_ arrays */

   typedef_num++ ;
   typedef_nam = NI_realloc( typedef_nam , sizeof(char *)*typedef_num ) ;
   typedef_typ = NI_realloc( typedef_typ , sizeof(char *)*typedef_num ) ;
   typedef_dim = NI_realloc( typedef_dim , sizeof(char *)*typedef_num ) ;

   typedef_nam[nn] = NI_strdup(name) ;
   typedef_typ[nn] = NI_strdup(type) ;
   typedef_dim[nn] = NI_strdup(dimen);

   return ;
}

/*--------------------------------------------------------------------*/
/*! Add attributes to a header_stuff struct if it has a special name.
----------------------------------------------------------------------*/

void enhance_header_stuff( header_stuff *hs )
{
   char *ntype , *ndimen ;
   int nn ;

   /* check for goofy inputs */

   if( hs == NULL || hs->name == NULL ) return ;
   if( strcmp(hs->name,"ni_group") == 0 ) return ;

   /* initialize pre-defined types */

   if( typedef_num == 0 ){
      NI_typedef( "ni_f1" , "f"  , NULL ) ;
      NI_typedef( "ni_f2" , "2f" , NULL ) ;
      NI_typedef( "ni_f3" , "3f" , NULL ) ;
      NI_typedef( "ni_f4" , "4f" , NULL ) ;

      NI_typedef( "ni_i1" , "i"  , NULL ) ;
      NI_typedef( "ni_i2" , "2i" , NULL ) ;
      NI_typedef( "ni_i3" , "3i" , NULL ) ;
      NI_typedef( "ni_i4" , "4i" , NULL ) ;

      NI_typedef( "ni_irgb"  , "i,r" , NULL ) ;
      NI_typedef( "ni_irgba" , "i,R" , NULL ) ;

      NI_typedef( "ni_string", "S"   , NULL ) ;  /* 03 Jun 2002 */

      typedef_nib = 1 ;  /* block further names starting with "ni_" */
   }

   /* Check for special names with predefined types */

   for( nn=0 ; nn < typedef_num ; nn++ )
      if( strcmp(hs->name,typedef_nam[nn]) == 0 ) break ;
   if( nn == typedef_num ) return ;

   ntype  = typedef_typ[nn] ;
   ndimen = typedef_dim[nn] ;

   /* see if we already have a ni_type attribute */

   nn = string_index( "ni_type" , hs->nattr , hs->lhs ) ;

   if( nn >= 0 ){  /* replace existing RHS */

      NI_free( hs->rhs[nn] ) ;
      hs->rhs[nn] = NI_strdup(ntype) ;

   } else {

      /* otherwise, append the ni_type attribute */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      hs->lhs[hs->nattr] = NI_strdup("ni_type") ;
      hs->rhs[hs->nattr] = NI_strdup(ntype) ;
      hs->nattr++ ;
   }

   /* see if we need to add a ni_dimen attribute:
      only if the header doesn't have one already,
      and also if the NI_typedef included a default dimen */

   nn = string_index( "ni_dimen" , hs->nattr , hs->lhs ) ;
   if( nn < 0 && ndimen != NULL ){
      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      hs->lhs[hs->nattr] = NI_strdup("ni_dimen") ;
      hs->rhs[hs->nattr] = NI_strdup(ndimen) ;
      hs->nattr++ ;
   }

   return ;
}
