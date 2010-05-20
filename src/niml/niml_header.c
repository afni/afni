#include "niml_private.h"

/****************************************************************************/
/****************** Functions to process a NIML header **********************/
/****************************************************************************/

/*! Macro to define skippable characters. */

#define SKIPABL(c) ((c)=='#' || isspace(c))

/*--------------------------------------------------------------------------*/
/*! Find an attribute in a header_stuff struct.  Returns the RHS or NULL.
----------------------------------------------------------------------------*/

char * get_header_attribute( header_stuff *hs , char *attname )
{
   int nn ;
   static char *zorkon = "\0" ;

   if( hs == NULL ) return NULL ;

   for( nn=0 ; nn < hs->nattr ; nn++ )
     if( strcmp(hs->lhs[nn],attname) == 0 ) break ;

   if( nn == hs->nattr ) return NULL ;

   if( hs->rhs[nn] == NULL ) return zorkon ;

   return hs->rhs[nn] ;
}

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

   for( id=0 ; id < ndat && dat[id] != '<' ; id++ ) ; /* skip to opening '<' */

   if( id >= ndat-1 ) return NULL ;                   /* bad input */

   hs = NI_malloc(header_stuff,sizeof(header_stuff)); /* make output struct */
   hs->nattr = hs->empty = 0 ;
   hs->name  = NULL ;
   hs->lhs   = hs->rhs = NULL ;

   /* find and assign name string (immediately after '<') */

   ss = find_string( id+1 , ndat , dat ) ;

   if( ss.i < 0 || ss.j <= ss.i ){
     destroy_header_stuff( hs ) ; return NULL ;   /* no name string */
   }

   nn = ss.j - ss.i ;                              /* string length */
   hs->name = NI_malloc(char, nn+1) ;
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

      for( ; id < ndat && SKIPABL(dat[id]) ; id++ ) ; /* skip blanks */

      if( id >= ndat ) break ;                 /* end of input found */

      if( dat[id] == '>' ) break ;                  /* ">" end found */

      if( dat[id] == '/' ){                        /* "/>" end found */
         if( id < ndat-1 ) id++ ;                  /* skip the '>'   */
         hs->empty = 1 ;                   /* mark header as 'empty' */
         break ;                /* done with scanning for attributes */
      }

      if( dat[id] == '?' ){                            /* "?>" found */
        if( id < ndat-1 ) id++ ;           /* ==> this is a procins! */
        hs->empty = 1 ;
        break ;
      }

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

      /* extend size of attribute arrays */

      hs->lhs = NI_realloc( hs->lhs , char*, sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , char*, sizeof(char *)*(hs->nattr+1) ) ;

      /* this is the LHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->lhs[hs->nattr] = NI_malloc(char, nn+1) ;
      NI_strncpy( hs->lhs[hs->nattr] , dat+ss.i , nn+1 ) ;
      unescape_inplace( hs->lhs[hs->nattr] ) ;

      hs->rhs[hs->nattr] = NULL ;             /* in case there is no RHS */

      id = ss.j ;
      if( id >= ndat ) break ;                      /* end of input ? */
      if( IS_QUOTE_CHAR(dat[id]) ) id++ ;           /* skip close quote */
      while( id < ndat && SKIPABL(dat[id]) ) id++ ; /* skip blanks */
      if( id >= ndat ) break ;                      /* end of input ? */

      if( dat[id] != '=' ){                   /* no '=' means no RHS */
         (hs->nattr)++ ;                      /* count the LHS and */
         continue ;                           /* go get next attribute */
      }

      id++ ;                                        /* skip the '=' */
      while( id < ndat && SKIPABL(dat[id]) ) id++ ; /* skip blanks */
      if( id >= ndat ) break ;                      /* end of input ? */

      /* find next string (the RHS) */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

#ifdef NIML_DEBUG
NI_dpr("   parse_header_stuff: next string = %.*s\n",ss.j-ss.i,dat+ss.i) ;
#endif

      /* this is the RHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->rhs[hs->nattr] = NI_malloc(char, nn+1) ;
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
    then no legal type was found (.j is still the number of chars
    used in the scan).
----------------------------------------------------------------------*/

intpair decode_type_field( char *tf )
{
   intpair ans = {-1,1} ;  /* default answer */
   char tname[256] ;
   int jj ;

   /* check input for goodness */

   if( tf == NULL || !isalpha(*tf) ) return ans ;  /* prima facie bad */

#if 1    /*** The new way! Look for full names, not initials - RWCcox ***/

   /* copy input into local string,
      as long as 'name' characters are found,
      then get the integer code for this type name */

   for( jj=0 ; jj < 255 && IS_NAME_CHAR(tf[jj]) ; jj++ ) tname[jj] = tf[jj];
   tname[jj] = '\0' ;
   ans.i = NI_rowtype_name_to_code( tname ) ;  /* where the names are stored */
   ans.j = jj ;

#else    /*** This is the old way! Replaced on 12 Feb 2003 - RWCox ***/

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
#endif

   return ans ;
}

/*--------------------------------------------------------------------*/
/*! Decode a single string into a bunch of strings, separated
    by characters from the list in sep.
    - Passing sep in as NULL means to use "," as the separator.
    - In each sub-string, leading and trailing blanks will be excised.
    - This can result in 0 length strings (e.g., "1,,2," will result
      in the second and fourth output strings having 0 length).
----------------------------------------------------------------------*/

NI_str_array * NI_decode_string_list( char *ss , char *sep )
{
   NI_str_array *sar ;
   int num , nn,id,jd , lss ;

   if( ss == NULL || ss[0] == '\0' ) return NULL ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */

   sar = NI_malloc(NI_str_array, sizeof(NI_str_array)) ;  /* create output */
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

      /* new sub-string runs from ss[jd] to ss[id-1] */

      sar->str = NI_realloc( sar->str , char*, sizeof(char *)*(num+1) ) ;

      nn = id-jd ;                                   /* length of sub-string */
#if 0
      while( nn > 0 && isspace(ss[jd+nn-1]) ) nn-- ; /* clip trailing blanks */
#endif
      sar->str[num] = NI_malloc(char, nn+1) ;        /* make output string  */
      if( nn > 0 ) memcpy(sar->str[num],ss+jd,nn) ;  /* copy sub-string    */
      sar->str[num++][nn] = '\0' ;                   /* terminate output  */

      id++ ;                                         /* skip separator  */
   }

   sar->num = num ; return sar ;
}

/*--------------------------------------------------------------------*/
/*! Return value = -1 if not found, otherwise is index [20 May 2010]. */

int NI_str_array_find( char *targ , NI_str_array *sar )
{
   int ii ;

   if( targ == NULL || *targ == '\0' || sar == NULL || sar->num < 1 ) return -1;

   for( ii=0 ; ii < sar->num ; ii++ )
     if( strcmp(targ,sar->str[ii]) == 0 ) return ii ;

   return -1 ;
}

/*--------------------------------------------------------------------*/
/*! Decode a string that gives a list of floats [10 Jun 2007]. */

NI_float_array * NI_decode_float_list( char *ss , char *sep )
{
   NI_float_array *far ; float *ar,val ; int num , ii,jj , nadd ;
   NI_str_array *sar ; char *cc, *dd ;

   sar = NI_decode_string_list( ss , sep ) ;
   if( sar == NULL ) return NULL ;

   far = NI_malloc(NI_float_array,sizeof(NI_float_array)) ;
   ar  = NULL ;
   num = 0 ;

   for( jj=0 ; jj < sar->num ; jj++ ){
     cc = sar->str[jj] ; dd = strstr(cc,"@") ;
     if( dd == NULL ){
       val = (float)strtod(cc,NULL) ; nadd = 1 ;
     } else {
       (void)sscanf(cc,"%d@%f",&nadd,&val) ;
       if( nadd <= 0 ) continue ;    /* bad */
     }
     ar = NI_realloc( ar , float , sizeof(float)*(num+nadd) ) ;
     for( ii=0 ; ii < nadd ; ii++ ) ar[num++] = val ;
   }

   NI_delete_str_array(sar) ;
   far->ar = ar ; far->num = num ; return far ;
}

/*--------------------------------------------------------------------*/

char * NI_encode_float_list( NI_float_array *far , char *sep )
{
   float *ar,val ; int num,jj,ii,ff ; char *car, cc='\0', fbuf[32] ;

   if( far == NULL || far->num < 1 ) return NULL ;
   if( sep != NULL ) cc = *sep ;
   if( cc  == '\0' ) cc = ',' ;

   num = far->num ; ar = far->ar ;
   car = NI_malloc(char,sizeof(char)*num*16) ; *car = '\0' ;

   for( jj=0 ; jj < num ; ){  /* jj will be incremented inside */

     /* encode ar[jj] value into fbuf */

     val = ar[jj] ; ff = (int)val ;
     if( val != (float)ff ) sprintf(fbuf,"%12.6g",val) ;
     else                   sprintf(fbuf,"%d",ff) ;

     /* strip trailing and leading blanks */

     for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
     for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;

     /** last one?  just print it and quit */

     if( jj == num-1 ){
       sprintf(car+strlen(car),"%s",fbuf+ff) ; break ;
     }

     /* scan for identical sequence of values */

     for( ii=jj+1 ; ii < num && ar[ii]==val ; ii++ ) ; /*nada*/

     if( ii > jj+1 )                             /* encode values [jj..ii-1] */
       sprintf(car+strlen(car),"%d@%s",ii-jj,fbuf+ff) ;
     else
       sprintf(car+strlen(car),"%s",fbuf+ff) ;   /* just the [jj] value */
     jj = ii ;                                   /* the next one to process */
     if( jj < num ) sprintf(car+strlen(car),"%c",cc) ; /* add the separator */

   }

   num = strlen(car) ;
   car = NI_realloc( car , char , sizeof(char)*(num+1) ) ;
   return car ;
}

/*--------------------------------------------------------------------*/
/*! Decode a string that gives a list of ints [21 Jun 2007]. */

NI_int_array * NI_decode_int_list( char *ss , char *sep )
{
   NI_int_array *iar ; int *ar, num,jj , vv,ww,nadd,aa,da,ii; char *cc,*dd  ;
   NI_str_array *sar ;

   sar = NI_decode_string_list( ss , sep ) ;
   if( sar == NULL ) return NULL ;

   iar = NI_malloc(NI_int_array,sizeof(NI_int_array)) ;
   num = 0 ;
   ar  = NULL ;

   for( jj=0 ; jj < sar->num ; jj++ ){
     cc = sar->str[jj] ; dd = strstr(cc,"..") ;
     if( dd == NULL ){
       dd = strstr(cc,"@") ;
       if( dd == NULL ){               /* a single number */
         vv = (int)strtol( cc , NULL , 10 ) ;
         nadd = 1 ; da = 0 ;
       } else {                        /* repetitions of the same number */
         aa = sscanf(cc,"%d@%d",&nadd,&vv) ;
         if( nadd <= 0 ) continue ;    /* bad */
         da = 0 ;
       }
     } else {                          /* a sequence of numbers */
       vv = (int)strtol( cc  , NULL , 10 ) ;
       ww = (int)strtol( dd+2, NULL , 10 ) ;
       nadd = ww-vv ; da = 1 ;
       if( nadd < 0 ){ nadd = -nadd; da = -1; }
       nadd++ ;
     }
     ar = NI_realloc( ar , int , sizeof(int)*(num+nadd) ) ;
     for( aa=vv,ii=0 ; ii < nadd ; ii++,aa+=da ) ar[num++] = aa ;
   }

   NI_delete_str_array(sar) ;
   iar->num = num ; iar->ar = ar ; return iar ;
}

/*--------------------------------------------------------------------*/

char * NI_encode_int_list( NI_int_array *iar , char *sep )
{
   int *ar ; int ii,num,jj ; char *car , cc='\0' , fbuf[32] ;

   if( iar == NULL || iar->num < 1 ) return NULL ;
   if( sep != NULL ) cc = *sep ;
   if( cc  == '\0' ) cc = ',' ;

   num = iar->num ; ar = iar->ar ;
   car = NI_malloc(char,sizeof(char)*num*9) ; *car = '\0' ;

   for( jj=0 ; jj < num ; ){

     /** last one?  just do it and quit */
     if( jj == num-1 ){
       sprintf(car+strlen(car),"%d",ar[jj]) ; break ;
     }

     /* scan for identical sequence */
     for( ii=jj+1 ; ii < num && ar[ii]-ar[ii-1]==0 ; ii++ ) ; /*nada*/

     if( ii > jj+1 ){                             /* encode values [jj..ii-1] */
       sprintf(fbuf,"%d@%d",ii-jj,ar[jj]) ;
     } else {                                     /* scan for increasing sequence */
       for( ii=jj+1 ; ii < num && ar[ii]-ar[ii-1]==1 ; ii++ ) ; /*nada*/

       if( ii == jj+1 )
         sprintf(fbuf,"%d",ar[jj]);                 /* encode one value */
       else if( ii == jj+2 )
         sprintf(fbuf,"%d%c%d",ar[jj],cc,ar[jj+1]); /* encode 2 values */
       else
         sprintf(fbuf,"%d..%d",ar[jj],ar[ii-1]);    /* encode values [jj..ii-1] */
     }
     jj = ii ;
     if( jj < num ) sprintf(car+strlen(car),"%s%c",fbuf,cc) ;
     else           sprintf(car+strlen(car),"%s"  ,fbuf   ) ;
   }

   num = strlen(car) ;
   car = NI_realloc( car , char , sizeof(char)*(num+1) ) ;
   return car ;
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

   iar = NI_malloc(int_array, sizeof(int_array)) ;  /* create output */
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
      if( jd < 0 || nn <= 0 ) break ;              /* something bad */
      id += nn ;                                   /* skip these chars */

      /* extend output array, store new dimension in it */

      iar->ar = NI_realloc( iar->ar , int, sizeof(int)*(num+1) ) ;
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

   iar = NI_malloc(int_array, sizeof(int_array)) ;  /* create output */
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

      } else if( isalpha(ts[id]) ){   /* start of a type name */
         jd = 1 ;                    /* default count of 1 */

      } else {
         id++ ; continue ;           /* skip this character */
      }

      dc = decode_type_field( ts+id ) ;

      /* dc.i = type code; dc.j = character count used to get type code */

      id += dc.j ;              /* skip these characters */
      if( dc.i < 0 ) continue ; /* bad type code */

      num += jd ;               /* this many fields so far */

      /* extend output array length */

      iar->ar = NI_realloc( iar->ar , int, sizeof(int)*num ) ;

      /* put values into output array */

      for( nn=0 ; nn < jd ; nn++ ) iar->ar[kk++] = dc.i ;

   } /* end of loop over input string */

   /* nothing found? */

   if( num <= 0 ){
      NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
   }

   iar->num = num ; return iar ;
}
