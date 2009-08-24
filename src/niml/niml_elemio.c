#include "niml_private.h"

/**********************************************************************/
/******* Functions to read and write data and group elements. *********/
/**********************************************************************/

static int scan_for_angles( NI_stream_type *, int ) ;

#define clear_buffer(ns) ( (ns)->nbuf = (ns)->npos = 0 )

/*--------------------------------------------------------------------*/
/*! Check if header_stuff marks this NIML element as a group.
----------------------------------------------------------------------*/

static int header_stuff_is_group( header_stuff *hs )  /* 24 Feb 2005 */
{
   char *atr ;
   if( hs == NULL ) return 0 ;
   if( strcmp(hs->name,"ni_group") == 0 ) return 1 ;
   atr = get_header_attribute( hs , "ni_form" ) ;
   if( atr != NULL && strcmp(atr,"ni_group") == 0 ) return 1 ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! Check if header_stuff marks NIML element as a processing instruction.
----------------------------------------------------------------------*/

static int header_stuff_is_procins( header_stuff *hs )
{
   if( hs == NULL ) return 0 ;
   if( hs->name != NULL && hs->name[0] == '?' ) return 1 ;
   return 0 ;
}

/*--------------------------------------------------------------------*/
/*! Write a simple processing instruction to the stream:
    - "<?str ?>\n" will be written
    - Return value is the number of bytes written
    - Return 0 means that the stream wasn't ready to write
    - Return -1 means an error happened, and nothing was written
    - 17 Mar 2005 - RWCox
----------------------------------------------------------------------*/

int NI_write_procins( NI_stream_type *ns , char *str )
{
   char *buf ; int jj ;

   /* check inputs for good-ositifulness */

   if( !NI_stream_writeable(ns)             ) return -1 ;  /* stupid user */
   if( str == NULL || !IS_STRING_CHAR(*str) ) return -1 ;

   /* check if stream is ready to take data */

   if( ns->bad ){                       /* socket that hasn't connected yet */
     jj = NI_stream_goodcheck(ns,666) ; /* try to connect it */
     if( jj < 1 ) return jj ;           /* 0 is nothing yet, -1 is death */
   } else {                             /* check if good ns has gone bad */
     jj = NI_stream_writecheck(ns,666) ;
     if( jj < 0 ) return jj ;
   }

   /* write the processing instruction: "<?str ?>\n" */

   buf = (char *)malloc(strlen(str)+16) ;
   sprintf( buf , "<?%s ?>\n" , str ) ;
   jj = NI_stream_writestring( ns , buf ) ;

   free((void *)buf) ; return jj ;
}


/*--------------------------------------------------------------------*/

static int read_header_only = 0 ;
void NI_read_header_only( int r ){ read_header_only=r ; } /* 23 Mar 2003 */

static int skip_procins = 0 ;
void NI_skip_procins( int r ){ skip_procins = r ; }       /* 03 Jun 2005 */

/*--------------------------------------------------------------------*/
/*! Read only the header part of the next element.
----------------------------------------------------------------------*/

void * NI_read_element_header( NI_stream_type *ns , int msec )
{
   void *nini ;
   read_header_only = 1 ;
   nini = NI_read_element( ns , msec ) ;
   read_header_only = 0 ;
   return nini ;
}

/*--------------------------------------------------------------------*/
/*! Read an element (maybe a group) from the stream, waiting up to
    msec milliseconds for the header to appear.  (After that, this
    function may wait a long time for the rest of the element to
    appear, unless the data stream comes to a premature end.)

   Return is NULL if nothing can be read at this time.  Otherwise,
   use NI_element_type(return value) to determine if the element
   read is a data element or a group element.

   Note that a header that is longer than ns->bufsize will
   never be read properly, since we must have the entire header in
   the buffer before processing it.  This should only be a problem
   for deranged users (e.g., Ziad).  If such a vast header is encountered,
   it will be flushed.

   If header start '<' and stop '>' are encountered, then this
   function will read data until it can create an element, or until
   the data stream is bad (i.e., the file ends, or the socket closes).

   If NULL is returned, that can be because there is no data to
   read even in the buffer, or because the input data stream has gone
   bad (i.e., will return no more data ever).  To check for the latter
   case, use NI_stream_readcheck().

   If a "<ni_do ... />" or "<?ni_do ... ?>" element is encountered,
   it will not be returned to the caller.  Instead, the actions it
   orders will be carried out in function NI_do(), and the function
   will loop back to find some other input.
----------------------------------------------------------------------*/

void * NI_read_element( NI_stream_type *ns , int msec )
{
   int ii,nn,nhs , num_restart ;
   char *cstart , *cstop ;
   header_stuff *hs ;
   int start_time=NI_clock_time() , mleft ;

   if( ns == NULL || ns->bad == MARKED_FOR_DEATH || ns->buf == NULL )
     return NULL ;  /* bad input stream */

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_read_element\n") ;
#endif

   if( msec < 0 ) msec = 999999999 ;  /* a long time (11+ days) */

   /* if we have a socket that hasn't connected,
      then see if it can connect now            */

   if( ns->bad ){
     nn = NI_stream_goodcheck( ns , msec ) ;
     if( nn < 1 ) return NULL ;              /* didn't connect */
   }

   /*-- Try to find the element header --*/

   num_restart = 0 ;
HeadRestart:                            /* loop back here to retry */
   num_restart++ ;
   mleft = msec - (NI_clock_time()-start_time) ;      /* time left */
   if( num_restart > 1 && mleft <= 0 ) return NULL ;  /* don't allow too many loops */

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: HeadRestart scan_for_angles; num_restart=%d\n" ,
               num_restart ) ;
#endif

   nn = scan_for_angles( ns , 0 ) ;     /* look for '<stuff>' */

   /* didn't find it */

   if( nn < 0 ){
     if( NI_stream_readcheck(ns,0) < 0 ) return NULL ;   /* connection lost */
     NI_sleep(2); goto HeadRestart;                      /* try again */
   }

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: found '<'\n") ;
#endif

   /* ns->buf[ns->npos] = opening '<' ; ns->buf[nn-1] = closing '>' */

   /* see if we found '<>', which is meaningless,
      or a trailer '</stuff>', which is illegal here */

   if( nn - ns->npos <= 2 || ns->buf[ns->npos+1] == '/' ){
      ns->npos = nn; NI_reset_buffer(ns); /* toss the '<..>', try again */
#ifdef NIML_DEBUG
NI_dpr("NI_read_element: illegal header found? skipping\n") ;
#endif
      goto HeadRestart ;
   }

   /*----- Parse the header data and prepare to make an element! -----*/

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: parsing putative header\n") ;
#endif

   hs = parse_header_stuff( nn - ns->npos , ns->buf + ns->npos , &nhs ) ;

   if( hs == NULL ){  /* something bad happened there */
     fprintf(stderr,"NI_read_element: bad element header found!\n") ;
     ns->npos = nn; NI_reset_buffer(ns); /* toss the '<..>', try again */
     goto HeadRestart ;
   }

   /*----- If here, have parsed a header (and will not HeadRestart).
           First, expunge the data bytes that were consumed to make
           the header; that is, we can then start reading data from
           ns->buf[ns->npos] .. ns->buf[ns->nbuf-1]                 --*/

   ns->npos = nn ;

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: header parsed successfully\n") ;
#endif

   /*--------------- Now make an element of some kind ---------------*/

   if( header_stuff_is_procins(hs) ){       /*--- a processing instruction ---*/

     NI_procins *npi ;

     if( strcmp(hs->name,"?ni_do") == 0 ){  /* 19 Apr 2005: special case! */
       NI_element *nel ;
       nel = make_empty_data_element( hs ) ;         /* temporary element */
       destroy_header_stuff( hs ) ;
       NI_do( ns , nel ) ;                        /* do the stuff it says */
       NI_free_element( nel ) ;                        /* then destroy it */
       if( ns->bad == MARKED_FOR_DEATH || ns->buf == NULL ) return NULL ;
       num_restart = 0 ; goto HeadRestart ;
     }

     /* 03 Jun 2005: if ordered to skip these things, do so */

     if( skip_procins ){
       destroy_header_stuff( hs ) ; num_restart = 0 ; goto HeadRestart ;
     }

     /* normal case: make a procins element and give it to the caller */

     npi       = NI_malloc(NI_procins,sizeof(NI_procins)) ;
     npi->type = NI_PROCINS_TYPE ;
     npi->name = NI_strdup( hs->name + 1 ) ; /* skip the '?' */

     npi->attr_num = hs->nattr ;
     if( npi->attr_num > 0 ){
       npi->attr_lhs = hs->lhs ; hs->lhs = NULL ;
       npi->attr_rhs = hs->rhs ; hs->rhs = NULL ;
     } else {
       npi->attr_lhs = npi->attr_rhs = NULL ;
     }

     destroy_header_stuff( hs ) ;

     return npi ;

   } /*--- end of reading a processing instruction ---*/

   else if( header_stuff_is_group(hs) ){           /*---- a group element ----*/

      NI_group *ngr ;
      void *nini ;
      int   empty=hs->empty ;

      read_header_only = 0 ;         /* 23 Mar 2003 */

      start_time = NI_clock_time() ; /* allow up to 10 sec for next */
      msec       = 9999 ;            /* element to appear, before giving up */

      ngr = make_empty_group_element( hs ) ;  /* copies name and attributes */
      destroy_header_stuff( hs ) ;
      if( empty ) return ngr ;  /* 03 Jun 2002: empty group is legal */

      /* we now have to read the elements within the group */

      num_restart = 0 ;
      while(1){           /* loop to find an element */

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: ni_group scan_for_angles; num_restart=%d\n",
               num_restart ) ;
#endif

         nn = scan_for_angles( ns , 10 ) ;  /* find header/trailer '<...>' */

         mleft = msec - (NI_clock_time()-start_time) ;
         if( mleft < 0 ) mleft = 0 ;

         if( nn <= 0 ){  /* didn't find it */
           if( NI_stream_readcheck(ns,0) < 0 ) break ;  /* real bad */
           if( num_restart > 1 && mleft == 0 ) break ;  /* time's up */
           num_restart++ ;
           continue ;        /* try again (but not forever) */
         }

         /* check if we found a trailer element '</stuff>' */

         if( ns->buf[ns->npos+1] == '/' ){  /* trailer */
           ns->npos = nn ;                 /* so end the group */
           break ;
         }

         /* not a trailer, so try to make an element out of it */

         nini = NI_read_element( ns , mleft ) ;   /* recursion! */
         if( nini != NULL ){
            NI_add_to_group( ngr , nini ) ;  /* this is good */
            num_restart = 0 ;
            start_time = NI_clock_time() ;   /* restart the wait clock */
         } else {                            /* this is bad */
            if( NI_stream_readcheck(ns,0) < 0 ) break ;    /* real bad */
            mleft = msec - (NI_clock_time()-start_time) ;
            if( num_restart > 1 && mleft <= 0 ) break ;    /* time's up */
            num_restart++ ;
         }
      }

      /* and we are done */

      return ngr ;

   } /* end of reading group element */

   else {      /*------------------------ a data element ---------------------*/

      NI_element *nel ;
      int form, swap, nbrow , row,col ;

      nel = make_empty_data_element( hs ) ;
      destroy_header_stuff( hs ) ;

      /*-- check if this is an empty element --*/

      if( nel           == NULL ||     /* nel == NULL should never happen. */
          nel->vec_rank == 0    ||     /* These other cases are indication */
          nel->vec_num  == 0    ||     /* that this is an 'empty' element. */
          nel->vec_typ  == NULL ||     /* ==> The header is all there is.  */
          nel->vec      == NULL ||
          nel->name[0]  == '!'  ||     /* Stupid XML declaration */
          read_header_only        ){

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: returning empty element\n") ;
#endif

        /*-- 23 Aug 2002: do something, instead of returning data? --*/

        if( nel != NULL && strcmp(nel->name,"ni_do") == 0 ){
          NI_do( ns , nel ) ;
          NI_free_element( nel ) ;
          if( ns->bad == MARKED_FOR_DEATH || ns->buf == NULL ) return NULL ;
          num_restart = 0 ; goto HeadRestart ;
        }

        if( read_header_only && nel->vec != NULL ){
          for( ii=0 ; ii < nel->vec_num ; ii++ ) NI_free(nel->vec[ii]) ;
          NI_free(nel->vec) ; nel->vec = NULL ;
        }

        return nel ;   /* default: return element */
      }

      /*-- If here, must read data from the buffer into nel->vec --*/

      /* Find the form of the input */

      form = NI_TEXT_MODE ; /* default is text mode */
      swap = 0 ;            /* and (obviously) don't byte swap */

      ii = string_index( "ni_form" , nel->attr_num , nel->attr_lhs ) ;

      if( ii >= 0 && nel->attr_rhs[ii] != NULL ){ /* parse ni_form=rhs */

         /* binary or base64 mode? */

         if( strstr(nel->attr_rhs[ii],"binary") != NULL )
            form = NI_BINARY_MODE ;
         else if( strstr(nel->attr_rhs[ii],"base64") != NULL ){
            form = NI_BASE64_MODE ;
            ns->b64_numleft = 0 ;    /* 21 Apr 2005: reset Base64 leftovers */
         }

         /* check byteorder in header vs. this CPU */

         if( form != NI_TEXT_MODE ){
            int order=NI_MSB_FIRST ; /* default input byteorder */
            if( strstr(nel->attr_rhs[ii],"lsb") != NULL ) order = NI_LSB_FIRST;
            swap = ( order != NI_byteorder() ) ;  /* swap bytes? */
         }
      }

      /*-- 13 Feb 2003: Use new NI_read_columns() function to get data. --*/

      if( form == NI_TEXT_MODE ) ii = NI_LTEND_MASK ;  /* end on '<' char  */
      else if( swap )            ii = NI_SWAP_MASK  ;  /* swap binary data */
      else                       ii = 0 ;              /* no special flag  */

      row = NI_read_columns( ns ,
                             nel->vec_num, nel->vec_typ,
                             nel->vec_len, nel->vec    , form, ii );

      nel->vec_filled = (row >= 0) ? row : 0 ;

      /* 27 Mar 2003: allow for case where vec_len is
                      inferred from how much data we read */

      if( nel->vec_len == 0 ){
        if( nel->vec_axis_len == NULL )
          nel->vec_axis_len = NI_malloc(int, sizeof(int)) ;

        nel->vec_axis_len[0] = nel->vec_len  = nel->vec_filled ;
        nel->vec_rank = 1 ;
      }

      /*-- Now scan for the end-of-element marker '</something>' and
           skip all input bytes up to (and including) the final '>'. --*/

      num_restart = 0 ;
TailRestart:
      num_restart++ ;

      if( num_restart < 99 ){  /* don't loop forever, dude */
         int is_tail ;

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: TailRestart scan_for_angles; num_restart=%d\n" ,
               num_restart ) ;
#endif

         nn = scan_for_angles( ns , 99 ) ;  /* find '<...>' */

         /* if we didn't find '<...>' at all,
            then if the I/O stream is bad, just exit;
            otherwise, try scanning for '<...>' again */

         if( nn < 0 ){
           if( NI_stream_readcheck(ns,0) < 0 ) return nel ;
           goto TailRestart ;
         }

         /* we have '<...>', but make sure it starts with '</' */

         is_tail = ( ns->buf[ns->npos+1] == '/' ) ;

         if( !is_tail ){                         /* no '/'? */
           ns->npos = nn ; NI_reset_buffer(ns) ; /* skip '<...>' */
           goto TailRestart ;                    /* and try again */
         }

         ns->npos = nn ; /* skip '</...>' and we are done here! */
      }

      /*-- And are done with the input stream and the data element! --*/

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: returning filled data element\n") ;
#endif

      /*-- 23 Aug 2002: do something, instead of returning data? --*/

      if( strcmp(nel->name,"ni_do") == 0 ){
        NI_do( ns , nel ) ;
        NI_free_element( nel ) ;
        num_restart = 0 ; goto HeadRestart ;
      }

      return nel ;

   } /* end of reading data element */

   return NULL ; /* should never be reached */
}

/*----------------------------------------------------------------------*/

#undef  NVBUF
#define NVBUF 127  /* max num chars for one number */

#define IS_USELESS(c) ( isspace(c) || iscntrl(c) )
#define IS_CRLF(c)    ( (c) == 0x0D || (c) == 0x0A )

/*----------------------------------------------------------------------*/
/*! From the NI_stream ns, starting at buffer position ns->npos, decode
    one number into *val.
    - Parameter ltend != 0 means to stop at '<' character [07 Jan 2003].
    - ltend != 0 also means to skip lines starting with '#' [20 Mar 2003].
    - ns->npos will be altered to reflect the current buffer position
      (one after the last character processed) when all is done.
    - Return value of this function is 1 if we succeeded, 0 if not.
------------------------------------------------------------------------*/

int NI_decode_one_double( NI_stream_type *ns, double *val , int ltend )
{
   int epos , num_restart, need_data, nn ;
   char vbuf[NVBUF+1] ;                    /* number string from buffer */

   /*-- check inputs for stupidness --*/

   if( ns == NULL || ns->bad == MARKED_FOR_DEATH || val == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data for a number ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ; need_data = 0 ;
   if( num_restart > 19 ) return 0 ;  /*** too much ==> give up ***/

#ifdef NIML_DEBUG
NI_dpr(" {restart: npos=%d nbuf=%d}",ns->npos,ns->nbuf) ;
#endif

   /*-- advance over useless characters in the buffer --*/

   while( ns->npos < ns->nbuf && IS_USELESS(ns->buf[ns->npos]) ) ns->npos++ ;

   /*-- check if we ran into the closing '<' prematurely
        (before any useful characters); if we did, then we are done --*/

   if( ltend && ns->npos < ns->nbuf && ns->buf[ns->npos] == '<' ) return 0 ;

   /*-- 20 Mar 2003: check if we ran into a comment character '#';
                     if we did, skip to the end of the line (or '<') --*/

   if( ltend && ns->npos < ns->nbuf && ns->buf[ns->npos] == '#' ){
     int npold = ns->npos ;
     while( ns->npos < ns->nbuf && !IS_CRLF(ns->buf[ns->npos]) ){
       if( ns->buf[ns->npos] == '<' ) return 0 ;  /* STOP HERE! */
       ns->npos++ ;
     }
     if( ns->npos < ns->nbuf ){ /* found end of line, so try again */
       num_restart = 0 ; goto Restart ;
     }
     /* if here, didn't find '<' or end of line in buffer */
     /* so reset pointer back to '#', then read more data */
     ns->npos = npold ; need_data = 1 ;
   }

   /*-- if we need some data, try to get some --*/

   if( !need_data )                        /* need at least 2 unused  */
     need_data = (ns->nbuf-ns->npos < 2) ; /* bytes to decode a number */

   /*-- An input value is decoded from a string of non-useless
        characters delimited by a useless character (or by the
        element closing '<').
        Note that the 1st character we are now at is non-useless.
        Scan forward to see if we have a useless character later. --*/

   if( !need_data ){  /* so have at least 2 characters */

#ifdef NIML_DEBUG
nn = ns->nbuf-ns->npos ; if( nn > 19 ) nn = 19 ;
NI_dpr(" {buf=%.*s}" , nn , ns->buf+ns->npos ) ;
#endif

      for( epos=ns->npos+1 ; epos < ns->nbuf ; epos++ )
        if( ns->buf[epos] == '<' || IS_USELESS(ns->buf[epos]) ) break ;

      /*- epos is either the delimiter position, or the end of data bytes -*/

      need_data = (epos == ns->nbuf) ; /* no delimiter ==> need more data */

#ifdef NIML_DEBUG
if( need_data ) NI_dpr(" {eob}") ;
#endif

      /*- If the string of characters we have is not yet
          delimited, and it is too long to be a number,
          throw out all the data in the buffer and quit. -*/

      if( need_data && epos-ns->npos > NVBUF ){ clear_buffer(ns); return 0; }
   }

   /*-- read more data now if it is needed --*/

   if( need_data ){

      NI_reset_buffer(ns) ; /* discard used up data in buffer */

      /*- read at least 1 byte,
          waiting up to 666 ms (unless the data stream goes bad) -*/

#ifdef NIML_DEBUG
NI_dpr(" {fill buf}") ;
#endif
      nn = NI_stream_fillbuf( ns , 1 , 666 ) ;

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
   *val = 0.0 ;                                           /* initialize val */
   sscanf( vbuf , "%lf" , val ) ;                         /* interpret them    */
   ns->npos = epos ; return 1 ;                           /* retire undefeated */
}

/*----------------------------------------------------------------------*/
/*! From the NI_stream ns, starting at buffer position ns->npos, decode
    one string into newly NI_malloc()-ed space pointed to by *str.
    - Parameter ltend !=0 means to stop at '<' character [07 Jan 2003].
    - ltend != 0 also means to skip lines starting with '#' [20 Mar 2003].
    - Return value of this function is 1 if we succeeded, 0 if not.
    - ns->npos will be altered to reflect the current buffer position
      (one after the last character processed) when all is done.
------------------------------------------------------------------------*/

int NI_decode_one_string( NI_stream_type *ns, char **str , int ltend )
{
   int epos , num_restart, need_data, nn , overbuf=0 ;
   intpair sp ;

   /*-- check inputs for stupidness --*/

   if( ns == NULL || ns->bad == MARKED_FOR_DEATH || str == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ; need_data = 0 ;
   if( num_restart > 19 ){
     if( overbuf ){         /* 21 Nov 2007: warn if buffer was too small */
       static int nov=0 ;
       if( ++nov < 7 )
         fprintf(stderr,"** ERROR: String runs past end of NIML buffer\n");
     }
     return 0 ;  /*** give up ***/
   }
   if( num_restart > 2 && overbuf ){  /* 23 Nov 2007: auto-expand buffer */
     nn = 2*NI_stream_getbufsize(ns) ;
     if( nn > 0 ){
#if 0
       static int nov=0 ;
       if( ++nov < 7 )
         fprintf(stderr,"** WARNING: long String expands NIML buffer to %d bytes\n",nn) ;
#endif
       nn = NI_stream_setbufsize( ns , nn ) ;       /* double down */
       if( nn < 0 ) return 0 ; /*** buffer expand fails? give up ***/
     }
   }
   overbuf = 0 ;

   /*-- advance over useless characters in the buffer --*/

   while( ns->npos < ns->nbuf && IS_USELESS(ns->buf[ns->npos]) ) ns->npos++ ;

   /*-- check if we ran into the closing '<' prematurely
        (before any useful characters); if we did, then we are done --*/

   if( ltend && ns->npos < ns->nbuf && ns->buf[ns->npos] == '<' ) return 0 ;

   /*-- 20 Mar 2003: check if we ran into a comment character '#';
                     if we did, skip to the end of the line (or '<') --*/

   if( ltend && ns->npos < ns->nbuf && ns->buf[ns->npos] == '#' ){
     int npold = ns->npos ;
     while( ns->npos < ns->nbuf && !IS_CRLF(ns->buf[ns->npos]) ){
       if( ns->buf[ns->npos] == '<' ) return 0 ;  /* STOP HERE! */
       ns->npos++ ;
     }
     if( ns->npos < ns->nbuf ){ /* found end of line, so try again */
       num_restart = 0 ; goto Restart ;
     }
     /* if here, didn't find '<' or end of line in buffer */
     /* so reset pointer back to '#', then read more data */
     ns->npos = npold ; need_data = 1 ;
   }

   /*-- if we need some data, try to get some --*/

   if( !need_data )                        /* need at least 2 unused  */
     need_data = (ns->nbuf-ns->npos < 2) ; /* bytes to decode a string */

   if( !need_data ){  /* so have at least 2 characters */

      /* search for the string from here forward */

      sp = find_string( ns->npos , ns->nbuf , ns->buf ) ;

      need_data = (sp.i < 0)        ||  /* didn't find a string */
                  (sp.j <= sp.i)    ||  /* ditto */
                  (sp.j == ns->nbuf)  ; /* hit end of data bytes */

      overbuf = (sp.j == ns->nbuf) ; /* 21 Nov 2007: flag buffer overrun */
   }

   /*-- read more data now if it is needed --*/

   if( need_data ){

      NI_reset_buffer(ns) ; /* discard used up data in buffer */

      /*- read at least 1 byte,
          waiting up to 666 ms (unless the data stream goes bad) -*/

      nn = NI_stream_fillbuf( ns , 1 , 666 ) ;

      if( nn >= 0 ) goto Restart ;  /* check if buffer is adequate now */

      /*- if here, the stream went bad.  If there are still
          data bytes in the stream, we can try to interpret them.
          Otherwise, must quit without success.                  -*/

      if( ns->nbuf == 0 ){ ns->npos=0; return 0; }  /* quitting */

      sp.i = 0 ; sp.j = ns->nbuf ;
   }

   /*-- if here, data bytes sp.i .. sp.j-1 are the string --*/

   nn = sp.j - sp.i ;                       /* length of string */
   *str = NI_malloc(char, nn+1) ;           /* make the string */
   memcpy( *str , ns->buf+sp.i , nn ) ;     /* copy data to string */
   (*str)[nn] = '\0' ;                      /* terminate string */

   /* skip close quote character, if present */

   if( sp.j < ns->nbuf && IS_QUOTE_CHAR(ns->buf[sp.j]) ) sp.j++ ;

   ns->npos = sp.j ; return 1 ;
}

/*----------------------------------------------------------------------*/
/*! Reset the unscanned bytes in the buffer to start at position 0
    instead of position ns->npos; then set ns->npos to 0.
------------------------------------------------------------------------*/

void NI_reset_buffer( NI_stream_type *ns )
{
   if( ns == NULL || ns->npos <= 0 || ns->nbuf <= 0 ) return ;
   if( ns->buf == NULL || ns->bad == MARKED_FOR_DEATH ) return ;

   if( ns->npos < ns->nbuf ){          /* haven't used up all data yet */
     memmove( ns->buf, ns->buf+ns->npos, ns->nbuf-ns->npos ) ;
     ns->nbuf -= ns->npos ;
   } else {
     ns->nbuf = 0 ;                   /* all data in buffer is used up */
   }
   ns->npos = 0 ;              /* further scanning starts at beginning */
}

/*----------------------------------------------------------------------*/
/*! Scan stream for an element header or trailer:'<characters>',
    starting at byte offset ns->npos, and waiting msec milliseconds.

    Returns with the stream buffer set so that the opening '<' is at
    ns->buf[ns->npos] and the closing '>' is at ns->buf[q-1], where q
    is this function's return value.  Note that read operations may
    change ns->npos from its value when this function was called.

    If the return value is -1, then we couldn't find a '<stuff>' string.
    This may be due to:
      - there is no '<...>' in the buffer, and we can't read from
         the input stream; call NI_readcheck(ns,0) to confirm this
      - time ran out (alas)
      - The '<...' part filled the entire buffer space.  In this case,
         all the input buffer is thrown away - we don't support
         headers or trailers this long!

    01 Jun 2005: skip XML comments, which are of the form
                 "<!-- arbitrary text -->".
------------------------------------------------------------------------*/

static int scan_for_angles( NI_stream_type *ns, int msec )
{
   int nn, epos, need_data, num_restart ;
   char goal ;
   int start_time = NI_clock_time() , mleft , nbmin ;
   int caseb=0 ;  /* 1 => force rescan even if time is up */

   if( ns == NULL ) return -1 ;  /* bad input */

   if( ns->buf == NULL || ns->bad == MARKED_FOR_DEATH ) return -1 ;

   epos = ns->npos ;

   if( msec < 0 ) msec = 999999999 ;   /* a long time (11+ days) */

   /*-- Will loop back here if we have to re-read/re-scan --*/

   goal        = '<' ;  /* first goal is opening '<' (second goal is '>') */
   num_restart = 0   ;
Restart:                                       /* loop back here to retry */
   num_restart++ ;
   mleft = msec - (NI_clock_time()-start_time) ;             /* time left */

   if( num_restart > 3 && mleft <= 0 && !caseb ){              /* failure */
      NI_reset_buffer(ns) ;                            /* and out of time */
      return -1 ;
   }

   /*-- scan ahead to find goal character in the buffer --*/

   while( epos < ns->nbuf && ns->buf[epos] != goal ) epos++ ;

   /*-- if we found our goal, do something about it --*/

   if( epos < ns->nbuf ){

     /*-- if our goal was the closing '>', we are done! (maybe) --*/

     if( goal == '>' ){

       /*- 01 Jun 2005: see if we are at a comment; if so, must start over -*/

       if( epos - ns->npos >= 4 && strncmp(ns->buf+ns->npos,"<!--",4) == 0 ){

         if( strncmp(ns->buf+epos-2,"-->",3) == 0 ){  /* got a full comment */

#if 0
{ int ncp = 1+epos-ns->npos ; char *cpt=malloc(10+ncp) ;
  memcpy(cpt,ns->buf+ns->npos,ncp) ; cpt[ncp] = '\0' ;
  fprintf(stderr, "\nSkipping NIML comment: '%s'\n",cpt); free(cpt);
}
#endif

           ns->npos = epos+1 ; NI_reset_buffer(ns) ;  /* skip it & try again */
           epos = 0 ; goal = '<' ;
         } else {                              /* '>' doesn't close comment! */
           epos++ ;                            /* so look for another one!!! */
         }
         caseb = 1 ; goto Restart ;
       }

       /*** not a comment, so we can exit triumphantly! ***/

       return epos+1 ;  /* marks the character after '>' */
     }

     /*-- if here, our goal was the opening '<';
          set the buffer position to this location,
          set the new goal, and scan for the new goal --*/

      ns->npos = epos ;  /* mark where we found '<' */
      goal     = '>'  ;  /* the new goal */
      caseb    = 1    ;  /* force rescan, even if time is up */
      goto Restart    ;  /* scan again! */
   }

   /*-- if we get to here, we didn't find our goal:
        (a) if the goal was the opening '<', then throw
             away all data in the buffer, and get some more data
        (b) if the goal was the closing '>', then we need more data
            in the buffer, but need to keep the existing data
        (c) UNLESS the buffer is full AND npos is zero
             - in this case, we expand the buffer size and hope --*/

   if( goal == '<' ){                    /* case (a) */

      ns->nbuf = ns->npos = epos = 0 ; caseb = 0 ;

   } else if( ns->nbuf < ns->bufsize || ns->npos > 0 ){  /* case (b) */

      NI_reset_buffer(ns) ; epos = ns->nbuf ; caseb = 1 ;

   } else {                              /* case (c) */

      epos = ns->nbuf ;
      nn = NI_stream_setbufsize(ns,2*ns->bufsize) ; /* expand buffer! */
      if( nn < 0 ){ ns->nbuf = ns->npos = 0 ; return -1 ; } /* fails? */

   }

   /*-- if we are here, we need more data before scanning again --*/

   /*-- read at least nbmin bytes,
        waiting up to mleft ms (unless the data stream goes bad) --*/

   if( mleft <= 0 ) mleft = 3 ;
   nbmin = (goal == '<') ? 4 : 1 ;

   nn = NI_stream_fillbuf( ns , nbmin , mleft ) ;

   if( nn >= nbmin ) caseb = 1 ;    /* got new data => force rescan */

   if( nn >= 0     ) goto Restart ; /* scan some more for the goal */

   /*-- if here, the stream went bad, so exit --*/

   ns->nbuf = ns->npos = 0 ; return -1 ;
}

/*------------------------------------------------------------------------*/
/*! Mode for writing names. */

static int name_mode = NI_NAMEMODE_NORMAL ;

/*------------------------------------------------------------------------*/
/*! Set the mode for writing type names:
     - NI_NAMEMODE_NORMAL => byte , short, int  , float  , double , ...
     - NI_NAMEMODE_ALIAS  => uint8, int16, int32, float32, float64, ...
--------------------------------------------------------------------------*/

void NI_set_typename_mode( int nmode )
{
   if( nmode > 0 && nmode <= NI_ATTMODE_LAST ) name_mode = nmode ;
   else                                        name_mode = NI_NAMEMODE_NORMAL;
}

/*------------------------------------------------------------------------*/
/*! Return the type name given the integer code. */

char * NI_type_name( int code )
{
   return (name_mode == NI_NAMEMODE_ALIAS) ? NI_rowtype_code_to_alias(code)
                                           : NI_rowtype_code_to_name (code) ;
}

/*------------------------------------------------------------------------*/
/*! Write an element (data or group) to a stream.
    Return value is number of bytes written to the stream.
    If return is -1, something bad happened.  You should then check
    the stream with NI_stream_goodcheck(), for example.

    If the stream is temporarily unable to write (e.g., the socket
    buffer is full), then this function will wait until it is ready.
    If you don't want that behavior, you should use NI_stream_writecheck()
    before calling this function.
--------------------------------------------------------------------------*/

int NI_write_element( NI_stream_type *ns , void *nini , int tmode )
{
   char *wbuf , *att=NULL , *qtt , *btt ;
   int  nwbuf , ii,jj,row,col , tt=NI_element_type(nini) , ntot=0,nout ;
   int  att_len , kk ;

   char *bbuf , *cbuf ;  /* base64 stuff */
   int   bb=0 ,  cc=0 ;

   char *att_prefix , *att_equals , *att_trail ;
   int header_only , header_sharp , outmode=-1 ;

   /*--- 09 Mar 2005: outmode overrides tmode, if outmode is present ---*/

   switch( tt ){
     default: return -1 ;    /* bad input! */

     case NI_GROUP_TYPE:{
       NI_group *ngr = (NI_group *) nini ;
       outmode = ngr->outmode ;
     }
     break ;

     case NI_ELEMENT_TYPE:{
       NI_element *nel = (NI_element *) nini ;
       outmode = nel->outmode ;
     }
     break ;

     case NI_PROCINS_TYPE:{       /* 16 Mar 2005 */
       outmode = NI_TEXT_MODE ;
     }
     break ;
   }
   if( outmode >= 0 ) tmode = outmode ;

   /*--- determine special cases from the flags above bit #7 ---*/

   header_only  = ((tmode & NI_HEADERONLY_FLAG ) != 0) ;  /* 20 Feb 2003 */
   header_sharp = ((tmode & NI_HEADERSHARP_FLAG) != 0) ;  /* 20 Mar 2003 */

   /* ADDOUT = after writing, add byte count if OK, else quit */
   /* AF     = thing to do if ADDOUT is quitting */

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_write_element\n") ;
#endif

#undef  AF
#define AF     0
#define ADDOUT if(nout<0){AF;fprintf(stderr,"NIML: write abort!\n");return -1;} else ntot+=nout

   if( !NI_stream_writeable(ns) ) return -1 ;  /* stupid user */

   if( ns->bad ){                        /* socket that hasn't connected yet */
#ifdef NIML_DEBUG
NI_dpr("NI_write_element: write socket not connected\n") ;
#endif
      jj = NI_stream_goodcheck(ns,666) ; /* try to connect it */
      if( jj < 1 ) return jj ;           /* 0 is nothing yet, -1 is death */
#ifdef NIML_DEBUG
NI_dpr("NI_write_element: write socket now connected\n") ;
#endif
   } else {                              /* check if good ns has gone bad */
      jj = NI_stream_writecheck(ns,666) ;
      if( jj < 0 ) return jj ;
   }

   tmode &= 255 ;
   if( ns->type == NI_STRING_TYPE )      /* string output only in text mode */
      tmode = NI_TEXT_MODE ;

   if( tmode != NI_TEXT_MODE ) header_sharp = 0 ;  /* 20 Mar 2003 */

   /*-- 15 Oct 2002: write attributes with lots of space, or little --*/
   /*-- 20 Mar 2003: modified for "#  lhs = rhs" type of header     --*/

   att_prefix = (header_sharp) ? (char *)"\n#  " /* write this before each attribute */
                               : (char *)"\n  " ;

   att_equals = (header_sharp) ? (char *)" = "   /* write this between lhs and rhs */
                               : (char *)"="    ;

   att_trail  = (header_sharp) ? (char *)"\n# "  /* write this before closing ">" */
                               : (char *)" "    ;

   /*------------------ write a processing instruction ------------------*/

   if( tt == NI_PROCINS_TYPE ){

     NI_procins *npi = (NI_procins *)nini ;

     if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }

     nout = NI_stream_writestring( ns , "<?"   )    ; ADDOUT ;
     nout = NI_stream_writestring( ns , npi->name ) ; ADDOUT ;

     /*- attributes -*/

     for( ii=0 ; ii < npi->attr_num ; ii++ ){

       jj = NI_strlen( npi->attr_lhs[ii] ) ; if( jj == 0 ) continue ;
       nout = NI_stream_writestring( ns , " " ) ; ADDOUT ;
       if( NI_is_name(npi->attr_lhs[ii]) ){
         nout = NI_stream_write( ns , npi->attr_lhs[ii] , jj ) ;
       } else {
         att = quotize_string( npi->attr_lhs[ii] ) ;
         nout = NI_stream_writestring( ns , att ) ; NI_free(att) ;
       }
       ADDOUT ;

       jj = NI_strlen( npi->attr_rhs[ii] ) ; if( jj == 0 ) continue ;
       nout = NI_stream_writestring( ns , "=" ) ; ADDOUT ;
       att = quotize_string( npi->attr_rhs[ii] ) ;
       nout = NI_stream_writestring( ns , att ) ; NI_free(att) ; ADDOUT ;
     }

     nout = NI_stream_writestring( ns , " ?>\n" ) ; ADDOUT ;

     return ntot ;   /*** done with processing instruction ***/

   /*------------------ write a group element ------------------*/

   } else if( tt == NI_GROUP_TYPE ){

      NI_group *ngr = (NI_group *) nini ;
      char *gname ;

      /* 24 Feb 2005: all group elements used to be named "ni_group",
                      but no more; now they have attribute ni_form="ni_group" */

      gname = ngr->name ;
      if( gname == NULL || *gname == '\0' ) gname = "ni_group" ;

      /*- group header -*/

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
#if 1
      nout = NI_stream_writestring( ns , "<"   ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , gname ) ; ADDOUT ;
#else
      nout = NI_stream_writestring( ns , "<ni_group" ) ; ADDOUT ;
#endif

      /*- attributes -*/

      NI_set_attribute( ngr , "ni_form" , "ni_group" ) ;  /* 24 Feb 2005 */

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){

        jj = NI_strlen( ngr->attr_lhs[ii] ) ; if( jj == 0 ) continue ;
        nout = NI_stream_writestring( ns , att_prefix ) ; ADDOUT ;
        if( NI_is_name(ngr->attr_lhs[ii]) ){
          nout = NI_stream_write( ns , ngr->attr_lhs[ii] , jj ) ;
        } else {
          att = quotize_string( ngr->attr_lhs[ii] ) ;
          nout = NI_stream_writestring( ns , att ) ; NI_free(att) ;
        }
        ADDOUT ;

        jj = NI_strlen( ngr->attr_rhs[ii] ) ; if( jj == 0 ) continue ;
        nout = NI_stream_writestring( ns , att_equals ) ; ADDOUT ;
        att = quotize_string( ngr->attr_rhs[ii] ) ;
        nout = NI_stream_writestring( ns , att ) ; NI_free(att) ; ADDOUT ;
      }

      /*- close group header -*/

      nout = NI_stream_writestring( ns , att_trail ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , ">\n" ) ; ADDOUT ;

      /*- write the group parts (recursively) -*/

      for( ii=0 ; ii < ngr->part_num ; ii++ ){
        if( outmode >= 0 ){
          if( NI_element_type(ngr->part[ii]) == NI_ELEMENT_TYPE ){
            NI_element *qel = (NI_element *)ngr->part[ii] ;
            qel->outmode = outmode ;
          } else if( NI_element_type(ngr->part[ii]) == NI_GROUP_TYPE ){
            NI_group *qgr = (NI_group *)ngr->part[ii] ;
            qgr->outmode = outmode ;
          }
        }
        nout = NI_write_element( ns , ngr->part[ii] , tmode ) ; ADDOUT ;
      }

      /*- group trailer -*/

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
#if 1
      nout = NI_stream_writestring( ns , "</"  ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , gname ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , ">\n" ) ; ADDOUT ;
#else
      nout = NI_stream_writestring( ns , "</ni_group>\n" ) ; ADDOUT ;
#endif

      return ntot ;   /*** done with group element ***/

   /*------------------ write a data element ------------------*/

   } else if( tt == NI_ELEMENT_TYPE ){

      NI_element *nel = (NI_element *) nini ;

      /*- sanity check (should never fail) -*/

      jj = NI_strlen(nel->name) ; if( jj == 0 ) return -1 ;

      /*- select the data output mode -*/

      /* Strings can only be written in text mode */

      if( tmode != NI_TEXT_MODE ){
        for( jj=0 ; jj < nel->vec_num ; jj++ ){
          if( NI_has_String(NI_rowtype_find_code(nel->vec_typ[jj])) ){
             tmode = NI_TEXT_MODE ; break ;
          }
        }
      }

      switch( tmode ){
         default: tmode = NI_TEXT_MODE ; break ;

         case NI_BINARY_MODE: break ;
         case NI_BASE64_MODE: break ;
      }

      /* space to hold attribute strings */

      att_len = 8192 + 64*nel->vec_num + 128*nel->vec_rank ;
      att     = NI_malloc(char, att_len ) ;

#undef  AF
#define AF NI_free(att)  /* free att if we have to quit early now */

      /* write start of header "<name" */

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
      strcpy(att,"<") ; strcat(att,nel->name) ;
      nout = NI_stream_writestring( ns , att ) ; ADDOUT ;

      /*- write "special" attributes, if not an empty element -*/

      if( nel->vec_len > 0 && nel->vec_num > 0 ){
         int ll , tt ;

         /* ni_form (depends on tmode) */

         switch( tmode ){
           default:
           case NI_TEXT_MODE:
             *att = '\0' ;   /* text form is default */
           break ;

           case NI_BINARY_MODE:
           case NI_BASE64_MODE:
             sprintf(att,"%sni_form%s\"%s.%s\"" ,
                    att_prefix , att_equals ,
                    (tmode == NI_BINARY_MODE)      ? "binary"   : "base64"  ,
                    (NI_byteorder()==NI_LSB_FIRST) ? "lsbfirst" : "msbfirst" );
            break ;
         }
         if( *att != '\0' ){
            nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
         }

         /** do ni_type **/

         sprintf(att,"%sni_type%s\"" , att_prefix , att_equals ) ;
         for( ll=-1,ii=0 ; ii < nel->vec_num ; ii++ ){
          if( nel->vec_typ[ii] != ll ){  /* not the previous type */
             if( ll >= 0 ){              /* write the previous type out now */
                btt = att + strlen(att) ;
                if( jj > 1 ) sprintf(btt,"%d*%s,",jj,NI_type_name(ll)) ;
                else         sprintf(btt,"%s,"   ,   NI_type_name(ll)) ;
             }
             ll = nel->vec_typ[ii] ;     /* save new type code */
             jj = 1 ;                    /* it now has count 1 */

          } else {                       /* same as previous type */
             jj++ ;                      /* so add 1 to its count */
          }
         }
         /* write the last type we found */
         btt = att + strlen(att) ;
         if( jj > 1 ) sprintf(btt,"%d*%s\"",jj,NI_type_name(ll)) ;
         else         sprintf(btt,"%s\""   ,   NI_type_name(ll)) ;

         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;

         /** do ni_dimen **/

         if( nel->vec_rank > 1 ){
           sprintf(att,"%sni_dimen%s" , att_prefix , att_equals ) ;
           qtt = quotize_int_vector( nel->vec_rank ,
                                   nel->vec_axis_len , ',' ) ;
           strcat(att,qtt) ; NI_free(qtt) ;
         } else {
           sprintf(att,"%sni_dimen%s\"%d\"",att_prefix,att_equals,nel->vec_len);
         }
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;

#if 0
         /** 26 Mar 2003: write number of bytes of data contained herein **/

         for( jj=ii=0 ; ii < nel->vec_num ; ii++ )
            jj += NI_size_column( NI_rowtype_find_code(nel->vec_typ[ii]) ,
                                  nel->vec_len , nel->vec[ii] ) ;
         sprintf(att,"%sni_datasize%s\"%d\"" , att_prefix , att_equals , jj ) ;
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
#endif

#if 0
         /* extras: ni_veclen and ni_vecnum attributes */

         sprintf(att,"%sni_veclen%s\"%d\"", att_prefix,att_equals,nel->vec_len) ;
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;

         sprintf(att,"%sni_vecnum%s\"%d\"", att_prefix,att_equals,nel->vec_num) ;
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
#endif
         /* ni_delta */

         if( nel->vec_axis_delta != NULL ){
            sprintf(att,"%sni_delta%s",att_prefix,att_equals) ;
            qtt = quotize_float_vector( nel->vec_rank ,
                                        nel->vec_axis_delta , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
         }

         /* ni_origin */

         if( nel->vec_axis_origin != NULL ){
            sprintf(att,"%sni_origin%s",att_prefix,att_equals) ;
            qtt = quotize_float_vector( nel->vec_rank ,
                                        nel->vec_axis_origin , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
         }

         /* ni_units */

         if( nel->vec_axis_unit != NULL ){
            sprintf(att,"%sni_units%s",att_prefix,att_equals) ;
            qtt = quotize_string_vector( nel->vec_rank ,
                                         nel->vec_axis_unit , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
         }

         /* ni_axes */

         if( nel->vec_axis_label != NULL ){
            sprintf(att,"%sni_axes%s",att_prefix,att_equals) ;
            qtt = quotize_string_vector( nel->vec_rank ,
                                         nel->vec_axis_label , ',' ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
            nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
         }

      }

      /*- other attributes -*/

      for( ii=0 ; ii < nel->attr_num ; ii++ ){

         jj = NI_strlen( nel->attr_lhs[ii] ) ; if( jj == 0 ) continue ;

         /* skip "special" attributes */

         if( strcmp(nel->attr_lhs[ii],"ni_form")     == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_type")     == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_dimen")    == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_veclen")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_vecnum")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_delta")    == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_origin")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_units")    == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_axes")     == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_datasize") == 0 ) continue ; /* 13 Apr 2004 */

         kk = NI_strlen( nel->attr_rhs[ii] ) ;

         /* do the work */

         if( jj+kk+128 > att_len ){                 /* 13 Jun 2003 */
           att_len = jj+kk+128 ;
           att     = NI_realloc( att , char, att_len ) ;
         }

         strcpy(att,att_prefix) ;

         if( NI_is_name(nel->attr_lhs[ii]) ){           /* the 'normal' case */
           strcat(att,nel->attr_lhs[ii]) ;
         } else {                                        /* not legal in XML */
           qtt = quotize_string( nel->attr_lhs[ii] ) ;
           strcat(att,qtt) ; NI_free(qtt) ;
         }

         if( kk > 0 ){
            strcat(att,att_equals) ;
            qtt = quotize_string( nel->attr_rhs[ii] ) ; /* RHS always quoted */
            kk = strlen(qtt)+strlen(att)+32 ;
            if( kk > att_len ){ att_len=kk; att=NI_realloc(att,char,att_len); }
            strcat(att,qtt) ; NI_free(qtt) ;
         }
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
      }

      NI_free(att) ; att = NULL ; /**** done with attributes ****/

#undef  AF
#define AF 0  /* nothing to do if we have to quit early */

      /*- close header -*/

      if( nel->vec_len == 0    ||     /* An 'empty' element (no data) */
          nel->vec_num == 0    ||
          nel->vec_typ == NULL ||
          nel->vec     == NULL   ){

        nout = NI_stream_writestring( ns , att_trail ) ; ADDOUT ;
        nout = NI_stream_writestring( ns , "/>\n" )    ; ADDOUT ;

#ifdef NIML_DEBUG
  NI_dpr("NI_write_element: empty element '%s' had %d total bytes\n",nel->name,ntot) ;
#endif
        return ntot ;                 /*** done with empty data element ***/
      }

      /*- if here, must write some data out -*/

      /* first, terminate the header,
         and allocate space for the write buffer (1 row at a time) */

      switch( tmode ){
         default:
         case NI_TEXT_MODE:
            btt = ">\n" ;                             /* add a newline */
         break ;

         case NI_BINARY_MODE:
            btt = ">" ;                               /* no newline   */
         break ;

         case NI_BASE64_MODE:
            btt = ">\n" ;                             /* add a newline */
         break ;
      }

      nout = NI_stream_writestring( ns , att_trail ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , btt ) ; ADDOUT ;

      /*-- 13 Feb 2003: data output is now done elsewhere --*/

      if( !header_only ){
        nout = NI_write_columns( ns, nel->vec_num, nel->vec_typ,
                                     nel->vec_len, nel->vec    , tmode ) ;
        ADDOUT ;
      }
#ifdef NIML_DEBUG
      else NI_dpr("NI_write_element: header_only case\n") ;
#endif

      /*- write element trailer -*/

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
      nout = NI_stream_writestring( ns , "</" ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , nel->name ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , ">\n\n" ) ; ADDOUT ;

#ifdef NIML_DEBUG
  NI_dpr("NI_write_element: data element '%s' had %d total bytes\n",nel->name,ntot) ;
#endif
      return ntot ;   /*** done with full data element ***/

   } /* end of write data element */

   return -1 ; /* should never be reachable */
}

/*------------------------------------------------------------------------*/
/*! Write an element (data or group) to a file.  [07 Mar 2007]
--------------------------------------------------------------------------*/

int NI_write_element_tofile( char *fname , void *nini , int tmode )
{
   NI_stream_type *ns ; char *nsname ; int vv ;

   if( fname == NULL || *fname == '\0' || nini == NULL ) return -1 ;

   nsname = (char *)malloc(strlen(fname)+9) ;
   if( strncmp(fname,"stdout:",7) == 0 || strcmp(fname,"-") == 0 ){
     strcpy(nsname,"stdout:") ;
   } else if( strncmp(fname,"stderr:",7) == 0 ){
     strcpy(nsname,"stderr:") ;
   } else {
     strcpy(nsname,"file:") ; strcat(nsname,fname) ;
   }
   ns = NI_stream_open( nsname , "w" ) ; free((void *)nsname) ;
   if( ns == NULL ) return -1 ;
   vv = NI_write_element( ns , nini , tmode ) ;
   NI_stream_close( ns ) ;
   return vv ;
}

/*------------------------------------------------------------------------*/
/*! Read one element from a file.  [12 Mar 2007]
--------------------------------------------------------------------------*/

void * NI_read_element_fromfile( char *fname )
{
   NI_stream_type *ns ; char *nsname ; void *nini ;

   if( fname == NULL || *fname == '\0' ) return NULL ;

   nsname = (char *)malloc(strlen(fname)+9) ;
   strcpy(nsname,"file:") ; strcat(nsname,fname) ;
   ns = NI_stream_open( nsname , "r" ) ; free((void *)nsname) ;
   if( ns == NULL ) return NULL ;
   nini = NI_read_element( ns , 999 ) ;
   NI_stream_close( ns ) ;
   return nini ;
}
