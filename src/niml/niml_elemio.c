#include "niml_private.h"

/**********************************************************************/
/******* Functions to read and write data and group elements. *********/
/**********************************************************************/

static int scan_for_angles( NI_stream_type *, int ) ;

#define clear_buffer(ns) ( (ns)->nbuf = (ns)->npos = 0 )

/*--------------------------------------------------------------------*/

static int read_header_only = 0 ;
void NI_read_header_only( int r ){ read_header_only=r ; } /* 23 Mar 2003 */

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
   for deranged users.  If such a vast header is encountered, it
   will be flushed.

   If header start '<' and stop '>' are encountered, then this
   function will read data until it can create an element, or until
   the data stream is bad (i.e., the file ends, or the socket closes).

   If NULL is returned, that can be because there is no data to
   read even in the buffer, or because the input data stream has gone
   bad (i.e., will return no more data ever).  To check for the latter
   case, use NI_stream_readcheck().

   If a "<ni_do ... />" element is encountered, it will not be
   returned to the caller.  Instead, the actions it orders will
   be carried out in function NI_do(), and NULL will be returned
   to the caller.
----------------------------------------------------------------------*/

void * NI_read_element( NI_stream_type *ns , int msec )
{
   int ii,nn,nhs , num_restart ;
   char *cstart , *cstop ;
   header_stuff *hs ;
   int start_time=NI_clock_time() , mleft ;

   if( ns == NULL ) return NULL ;  /* bad input */

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
      NI_sleep(1); goto HeadRestart;                      /* try again */
   }

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: found '<'\n") ;
#endif

   /* ns->buf[ns->npos] = opening '<' ; ns->buf[nn-1] = closing '>' */

   /* see if we found '<>', which is illegal,
      or a trailer '</stuff>', which is also illegal (here) */

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

   if( strcmp(hs->name,"ni_group") == 0 ){  /*--- a group element ---*/

      NI_group *ngr ;
      void *nini ;
      int   empty=hs->empty ;

      read_header_only = 0 ;         /* 23 Mar 2003 */

      start_time = NI_clock_time() ; /* allow up to 10 sec for next */
      msec       = 9999 ;            /* element to appear, before giving up */

      ngr = make_empty_group_element( hs ) ;
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

   else { /*---------------------- a data element -------------------*/

      NI_element *nel ;
      int form, swap, nbrow , row,col ;

#if 0
      enhance_header_stuff( hs ) ;  /* 12 Feb 2003: removed NI_typedef - RWC */
#endif

      nel = make_empty_data_element( hs ) ;
      destroy_header_stuff( hs ) ;

#if 0 /*** 12 Feb 2003: disabled by RWC ***/
      /*-- check if this is a ni_typedef element --*/

      if( strcmp(nel->name,"ni_typedef") == 0 ){
         int nn , nt , nd ;
         nn = string_index( "ni_name" , nel->attr_num , nel->attr_lhs ) ;
         nt = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;
         nd = string_index( "ni_dimen", nel->attr_num , nel->attr_lhs ) ;

         /* needs ni_name and ni_type attributes */

         if( nn >= 0 && nt >= 0 )
            NI_typedef( nel->attr_rhs[nn] ,
                        nel->attr_rhs[nt] ,
                        (nd >= 0) ? nel->attr_rhs[nd] : NULL ) ;

         /* try for another element now */

         goto HeadRestart ;
      }
#endif

      /*-- check if this is an empty element --*/

      if( nel == NULL          ||     /* nel == NULL should never happen. */
          nel->vec_len == 0    ||     /* These other cases are indication */
          nel->vec_num == 0    ||     /* that this is an 'empty' element. */
          nel->vec_typ == NULL ||     /* ==> The header is all there is.  */
          nel->vec     == NULL ||
          read_header_only       ){

#ifdef NIML_DEBUG
NI_dpr("NI_read_element: returning empty element\n") ;
#endif

        /*-- 23 Aug 2002: do something, instead of returning data? --*/

        if( nel != NULL && strcmp(nel->name,"ni_do") == 0 ){
           NI_do( ns , nel ) ;
           NI_free_element( nel ) ;
           return NULL ;
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
         else if( strstr(nel->attr_rhs[ii],"base64") != NULL )
            form = NI_BASE64_MODE ;

         /* check byteorder in header vs. this CPU */

         if( form != NI_TEXT_MODE ){
            int order=NI_MSB_FIRST ; /* default input byteorder */
            if( strstr(nel->attr_rhs[ii],"lsb") != NULL ) order = NI_LSB_FIRST;
            swap = ( order != NI_byteorder() ) ;  /* swap bytes? */
         }
      }

#ifdef USE_NEW_IOFUN

      if( form == NI_TEXT_MODE ) ii = NI_LTEND_MASK ;  /* end on '<' char  */
      else if( swap )            ii = NI_SWAP_MASK  ;  /* swap binary data */
      else                       ii = 0 ;              /* no special flag  */

      row = NI_read_columns( ns ,
                             nel->vec_num, nel->vec_typ,
                             nel->vec_len, nel->vec    , form, ii );

      nel->vec_filled = (row >= 0) ? row : 0 ;

#else                   /** OLD way of reading data; replaced 13 Feb 2003 **/

      /*-- Now must actually read data and put it somewhere (oog). */

      row = 0 ;         /* next index in vectors to fill */

      nbrow = NI_element_rowsize( nel ) ; /* how many bytes for one row */

      switch( form ){

        default:  break ;   /* bad!!! should never ever happen */

        /*......................................................*/

        case NI_BASE64_MODE:{
         char *bbuf = NI_malloc(nbrow+8) ;  /* binary results */
         byte  a,b,c,w,x,y,z ;              /* base64 stuff */
         int   bb=0, bpos, num_reread=0 , bdone ;

         /* Base64 encodes 3 bytes of binary in 4 character bytes;
            bbuf is the binary output of the conversion;
            bb is the number of bytes currently saved in bbuf */

         load_decode_table() ;         /* prepare for base64 decoding */

         while( row < nel->vec_len ){  /* loop over input rows */

           /* we may be forced back here before a row
              is finished, if we run out of input data */

           num_reread = 0 ;
Base64Reread:
#ifdef NIML_DEBUG
NI_dpr("b64: Reread at row=%d num_reread=%d\n",row,num_reread) ;
#endif
           num_reread++ ; if( num_reread > 4 ) goto Base64Done ;

           /* if not enough data left in buffer to fill up
              one quad of base64 bytes into cbuf,
              then try to read more data into the buffer */

           if( num_reread > 1 || ns->nbuf-ns->npos < 4 ){

             NI_reset_buffer(ns) ;  /* discard used up data in buffer */
                                    /* (so ns->npos == 0 now)         */

             /* now read at least enough to fill up one quad,
                waiting a long time if need be (or until the stream goes bad) */

             bpos = 4 - ns->nbuf ; if( bpos <= 0 ) bpos = 1 ;

#ifdef NIML_DEBUG
NI_dpr("b64: reading extra data\n") ;
#endif

             (void) NI_stream_fillbuf( ns , bpos , 6666 ) ;

             /* if still don't have a full quad of data
                something bad has happened (end-of-file? closed socket?);
                we don't try to recover a partial row; instead, just quit */

             if( ns->nbuf < 4 ) goto Base64Done ;
           }

           /* copy base64 bytes out of the stream buffer,
              converting them to binary as we get full quads,
              then putting the results into bbuf;
              when bbuf is full, then we've finished a row
              and can put it into the data element vectors */

           bdone = 0 ;  /* bdone==1 when we hit '=' at end of base64 input */
           do{
             bpos = ns->npos ;  /* scan forward in input buffer using bpos */

             /* try to load 4 valid base64 characters into w,x,y,z;
                we skip non-valid characters (e.g., line ends, whitespace) */

             /* get next valid base64 character into w;
                if we hit the end token '<' first, quit;
                if we hit the end of the buffer first, need more data */
#ifdef NIML_DEBUG
NI_dpr("b64: bpos=%d bb=%d\n",bpos,bb) ;
#endif
             w = ns->buf[bpos++] ;
             while( !B64_goodchar(w) && w != '<' && bpos < ns->nbuf )
               w = ns->buf[bpos++] ;
             ns->npos = bpos-1 ;  /* if we have to reread, will start here */
             if( w == '<' ){ goto Base64Done; }
             if( bpos == ns->nbuf ){ goto Base64Reread; }
#ifdef NIML_DEBUG
NI_dpr("b64: bpos=%d w=%c\n",bpos,w) ;
#endif
             /* repeat to fill x */

             x = ns->buf[bpos++] ;
             while( !B64_goodchar(x) && x != '<' && bpos < ns->nbuf )
               x = ns->buf[bpos++] ;
             if( x == '<' ){ ns->npos = bpos-1; goto Base64Done; }
             if( bpos == ns->nbuf ){ goto Base64Reread; }

#ifdef NIML_DEBUG
NI_dpr("b64: bpos=%d x=%c\n",bpos,x) ;
#endif

             /* repeat to fill y */

             y = ns->buf[bpos++] ;
             while( !B64_goodchar(y) && y != '<' && bpos < ns->nbuf )
               y = ns->buf[bpos++] ;
             if( y == '<' ){ ns->npos = bpos-1; goto Base64Done; }
             if( bpos == ns->nbuf ){ goto Base64Reread; }

#ifdef NIML_DEBUG
NI_dpr("b64: bpos=%d y=%c\n",bpos,y) ;
#endif

             /* repeat to fill z */

             z = ns->buf[bpos++] ;
             while( !B64_goodchar(z) && z != '<' && bpos < ns->nbuf )
               z = ns->buf[bpos++] ;
             if( z == '<' ){ ns->npos = bpos-1; goto Base64Done; }
             if( bpos == ns->nbuf ){ goto Base64Reread; }

#ifdef NIML_DEBUG
NI_dpr("b64: bpos=%d z=%c\n",bpos,z) ;
#endif

             /* at this point, have w,x,y,z to decode */

             ns->npos = bpos ;  /* scan continues at next place in buffer */

             B64_decode4(w,x,y,z,a,b,c) ;  /* decode 4 bytes into 3 */

             if( z == '=' ){                        /* got to the end? */
               int nn = B64_decode_count(w,x,y,z) ; /* see how many to save */
               if( nn > 0 ) bbuf[bb++] = a ;
               if( nn > 1 ) bbuf[bb++] = b ;
               bdone = 1 ; break ;                  /* end of base64 data */
             }

             /* not at the end => save all 3 outputs, loop back */

             bbuf[bb++] = a ; bbuf[bb++] = b ; bbuf[bb++] = c ;

           } while( bb < nbrow ) ;  /* loop to fill output buffer */

#ifdef NIML_DEBUG
NI_dpr("b64: decoded row=%d with bb=%d\n",row,bb) ;
#endif

           /* if we've not filled a full row,
              then we reached the end of the base64 input
              (that was the only break out of the loop above) */

           if( bb < nbrow ) goto Base64Done ;

           /* have a full row here ==> save it! */

           NI_fill_vector_row( nel , row , bbuf ) ;
           row++ ;
           if( bdone ) goto Base64Done ;

           /* if we had more data stored into bbuf than
              was needed for one row, move the extra data down now */

           if( bb > nbrow ){
              memmove( bbuf , bbuf+nbrow , bb-nbrow ) ;
              bb = bb - nbrow ;
           } else {
              bb = 0 ;
           }

         } /* end of loop over vector rows */

Base64Done:
         nel->vec_filled = row ;  /* how many rows were filled above */
         NI_free(bbuf) ;
        }
        break ; /* end of base64 input */

        /*......................................................*/

        case NI_BINARY_MODE:{
         while( row < nel->vec_len ){  /* loop over input rows */

           /* if not enough data left in buffer for 1 row
              of data, then try to read more data into the buffer */

           if( ns->nbuf-ns->npos < nbrow ){

             NI_reset_buffer(ns) ;  /* discard used up data in buffer */

             /* now read at least enough to fill one data row,
                waiting a long time if need be (or until the stream goes bad) */

             (void) NI_stream_fillbuf( ns , nbrow-ns->nbuf , 9999 ) ;

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
        }
        break ;  /* end of binary input */

        /*......................................................*/

        case NI_TEXT_MODE:{

         while( row < nel->vec_len ){  /* loop over input rows */
#ifdef NIML_DEBUG
NI_dpr("NI_read_element: ROW=%d",row) ;
#endif
          for( col=0 ; col < nel->vec_num ; col++ ){ /* over input vectors */

            /* decode one value from input, according to its type */

            switch( nel->vec_typ[col] ){
              default:                    /* Line is unimplemented */
              break ;

              case NI_STRING:{
                 char *val=NULL ;
                 char **vpt = (char **) nel->vec[col] ;
                 nn = NI_decode_one_string( ns , &val , 1 ) ;
                 if( nn == 0 || val == NULL ) goto TextDone ;
                 unescape_inplace(val) ;
                 vpt[row] = val ;
              }
              break ;

              /* numeric types below here */

              case NI_BYTE:{
                 double val ;
                 byte *vpt = (byte *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &val , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (byte) val ;
              }
              break ;

              case NI_SHORT:{
                 double val ;
                 short *vpt = (short *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &val , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (short) val ;
              }
              break ;

              case NI_INT:{
                 double val ;
                 int *vpt = (int *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &val , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (int) val ;
#ifdef NIML_DEBUG
NI_dpr(" [%d]=%d",col,vpt[row]) ;
#endif
              }
              break ;

              case NI_FLOAT:{
                 double val ;
                 float *vpt = (float *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &val , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (float) val ;
#ifdef NIML_DEBUG
NI_dpr(" [%d]=%f",col,vpt[row]) ;
#endif
              }
              break ;

              case NI_DOUBLE:{
                 double val ;
                 double *vpt = (double *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &val , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row] = (double) val ;
              }
              break ;

              case NI_COMPLEX:{
                 double v1,v2 ;
                 complex *vpt = (complex *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &v1 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v2 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (float) v1 ;
                 vpt[row].i = (float) v2 ;
              }
              break ;

              case NI_RGB:{
                 double v1,v2,v3 ;
                 rgb *vpt = (rgb *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &v1 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v2 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v3 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
              }
              break ;

              case NI_RGBA:{
                 double v1,v2,v3,v4 ;
                 rgba *vpt = (rgba *) nel->vec[col] ;
                 nn = NI_decode_one_double( ns , &v1 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v2 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v3 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 nn = NI_decode_one_double( ns , &v4 , 1 ) ;
                 if( nn == 0 ) goto TextDone ;
                 vpt[row].r = (byte) v1 ;
                 vpt[row].g = (byte) v2 ;
                 vpt[row].b = (byte) v3 ;
                 vpt[row].a = (byte) v4 ;
              }
              break ;
            } /* end of switch on type of this data value */

          } /* end of loop over vector columns */
#ifdef NIML_DEBUG
NI_dpr("\n") ;
#endif
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

#endif  /* USE_NEW_IOFUN */

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
         return NULL ;
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

   if( ns == NULL || val == NULL ) return 0 ;

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
   int epos , num_restart, need_data, nn ;
   intpair sp ;

   /*-- check inputs for stupidness --*/

   if( ns == NULL || str == NULL ) return 0 ;

   /*--- might loop back here to check if have enough data ---*/

   num_restart = 0 ;
Restart:
   num_restart++ ; need_data = 0 ;
   if( num_restart > 19 ) return 0 ;  /*** give up ***/

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
   *str = NI_malloc(nn+1) ;                 /* make the string */
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
    change ns->npos from its value when this function was called.

    If the return value is -1, then we couldn't find a '<stuff>' string.
    This may be due to:
      - there is no '<...>' in the buffer, and we can't read from
         the input stream; call NI_readcheck(ns,0) to confirm this
      - time ran out (alas)
      - The '<...' part filled the entire buffer space.  In this case,
         all the input buffer is thrown away - we don't support
         headers or trailers this long!
------------------------------------------------------------------------*/

static int scan_for_angles( NI_stream_type *ns, int msec )
{
   int nn, epos, need_data, num_restart ;
   char goal ;
   int start_time = NI_clock_time() , mleft , nbmin ;
   int caseb=0 ;  /* 1 => force rescan even if time is up */

   if( ns == NULL ) return -1 ;  /* bad input */

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
#ifdef NIML_DEBUG
NI_dpr("  scan_for_angles: out of time!\n") ;
#endif
      return -1 ;
   }

#ifdef NIML_DEBUG
if( ns->npos < ns->nbuf )
NI_dpr("  scan_for_angles: npos=%d epos=%d nbuf=%d buffer=%.*s\n",
        ns->npos,epos,ns->nbuf,ns->nbuf-ns->npos,ns->buf+ns->npos ) ;
#endif

   /*-- scan ahead to find goal in the buffer --*/

   while( epos < ns->nbuf && ns->buf[epos] != goal ) epos++ ;

   /*-- if we found our goal, do something about it --*/

   if( epos < ns->nbuf ){

#ifdef NIML_DEBUG
NI_dpr("  scan_for_angles: found goal=%c at epos=%d\n",goal,epos) ;
#endif

     /*-- if our goal was the closing '>', we are done! --*/

     if( goal == '>' ) return epos+1 ;  /* marks the character after '>' */

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
        (c) UNLESS the buffer is full
             - in this case, the universe ends right here and now --*/

   if( goal == '<' ){                    /* case (a) */
#ifdef NIML_DEBUG
NI_dpr("  scan_for_angles: case (a)\n") ;
#endif
      ns->nbuf = ns->npos = epos = 0 ; caseb = 0 ;

   } else if( ns->nbuf < ns->bufsize ){  /* case (b) */
#ifdef NIML_DEBUG
NI_dpr("  scan_for_angles: case (b)\n") ;
#endif
      NI_reset_buffer(ns) ; epos = 0 ; caseb = 1 ;

   } else {                              /* case (c) */
#ifdef NIML_DEBUG
NI_dpr("  scan_for_angles: case (c)\n") ;
#endif
      ns->nbuf = 0 ; return -1 ;         /* death of Universe! */
   }

   /*-- if we are here, we need more data before scanning again --*/

   /*-- read at least nbmin bytes,
        waiting up to mleft ms (unless the data stream goes bad) --*/

   if( mleft <= 0 ) mleft = 1 ;
   nbmin = (goal == '<') ? 3 : 1 ;

   nn = NI_stream_fillbuf( ns , nbmin , mleft ) ;

   if( nn >= nbmin ) caseb = 1 ;    /* got new data => force rescan */

   if( nn >= 0     ) goto Restart ; /* scan some more for the goal */

   /*-- if here, the stream went bad, so exit --*/

   ns->nbuf = ns->npos = 0 ; return -1 ;
}

#if 0
/*-----------------------------------------------------------------*/
/*! Set the binary threshold size for NI_write_element.

  - If a data element takes up more than 'size' bytes, then it
    will be written in binary form, otherwise in text form.
  - If size=0, then all elements are written in binary.
  - If size<0, then all elements are written in text.

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
#endif

#if 0
/*------------------------------------------------------------------------*/
/*! Mode for writing attributes. */

static int att_mode = NI_ATTMODE_NORMAL ;

/*------------------------------------------------------------------------*/
/*! Set the mode for writing attributes:
     - NI_ATTMODE_NORMAL =>  lhs="rhs"
     - NI_ATTMODE_SPACED =>  lhs = "rhs"  [one per line]
--------------------------------------------------------------------------*/

void NI_set_attribute_mode( int amode )
{
   if( amode > 0 && amode <= NI_ATTMODE_LAST ) att_mode = amode ;
   else                                        att_mode = NI_ATTMODE_NORMAL ;
}
#endif

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
    If return is -1, something bad happened.  You should check
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

   char *bbuf , *cbuf ;  /* base64 stuff */
   int   bb=0 ,  cc=0 ;

   char *att_prefix , *att_equals , *att_trail ;

   int header_only  = ((tmode & NI_HEADERONLY_FLAG ) != 0) ;  /* 20 Feb 2003 */
   int header_sharp = ((tmode & NI_HEADERSHARP_FLAG) != 0) ;  /* 20 Mar 2003 */

   /* ADDOUT = after writing, add byte count if OK, else quit */
   /* AF     = thing to do if ADDOUT is quitting */

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_write_element\n") ;
#endif

#undef  AF
#define AF     0
#define ADDOUT if(nout<0){AF;fprintf(stderr,"NIML: write abort!\n");return -1;} else ntot+=nout

   if( ns == NULL ) return -1 ;

   if( ns->bad ){                        /* socket that hasn't connected yet */
#ifdef NIML_DEBUG
NI_dpr("NI_write_element: write socket not connected\n") ;
#endif
      jj = NI_stream_goodcheck(ns,1) ;   /* try to connect it */
      if( jj < 1 ) return jj ;           /* 0 is nothing yet, -1 is death */
#ifdef NIML_DEBUG
NI_dpr("NI_write_element: write socket now connected\n") ;
#endif
   } else {                              /* check if good ns has gone bad */
      jj = NI_stream_writecheck(ns,1) ;
      if( jj < 1 ) return jj ;
   }

   tmode &= 255 ;
   if( ns->type == NI_STRING_TYPE )      /* string output only in text mode */
      tmode = NI_TEXT_MODE ;

   if( tmode != NI_TEXT_MODE ) header_sharp = 0 ;  /* 20 Mar 2003 */

   /*-- 15 Oct 2002: write attributes with lots of space, or little --*/
   /*-- 20 Mar 2003: modified for "#  lhs = rhs" type of header     --*/

   att_prefix = (header_sharp) ? "\n#  "      /* write this before each attribute */
                               : " "    ;

   att_equals = (header_sharp) ? " = "        /* write this between lhs and rhs */
                               : "="    ;

   att_trail  = (header_sharp) ? "\n# "       /* write this before closing ">" */
                               : " "    ;

   /*------------------ write a group element ------------------*/

   if( tt == NI_GROUP_TYPE ){

      NI_group *ngr = (NI_group *) nini ;

      /*- group header -*/

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
      nout = NI_stream_writestring( ns , "<ni_group" ) ; ADDOUT ;

      /*- attributes -*/

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
         nout = NI_write_element( ns , ngr->part[ii] , tmode ) ; ADDOUT ;
      }

      /*- group trailer -*/

      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
      nout = NI_stream_writestring( ns , "</ni_group>\n" ) ; ADDOUT ;

      return ntot ;

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

      att = NI_malloc( 4096 + 2*nel->vec_num + 128*nel->vec_rank ) ;

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

         sprintf(att,"%sni_dimen%s" , att_prefix , att_equals ) ;
         qtt = quotize_int_vector( nel->vec_rank ,
                                   nel->vec_axis_len , ',' ) ;
         strcat(att,qtt) ; NI_free(qtt) ;
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;

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

         if( strcmp(nel->attr_lhs[ii],"ni_form")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_type")   == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_dimen")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_veclen") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_vecnum") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_delta")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_origin") == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_units")  == 0 ) continue ;
         if( strcmp(nel->attr_lhs[ii],"ni_axes")   == 0 ) continue ;

         /* do the work */

         strcpy(att,att_prefix) ;

         if( NI_is_name(nel->attr_lhs[ii]) ){
           strcat(att,nel->attr_lhs[ii]) ;
         } else {
           qtt = quotize_string( nel->attr_lhs[ii] ) ;
           strcat(att,qtt) ; NI_free(qtt) ;
         }

         jj = NI_strlen( nel->attr_rhs[ii] ) ;
         if( jj > 0 ){
            strcat(att,att_equals) ;
            qtt = quotize_string( nel->attr_rhs[ii] ) ;
            strcat(att,qtt) ; NI_free(qtt) ;
         }
         nout = NI_stream_writestring( ns , att ) ; ADDOUT ;
      }

      NI_free(att) ; att = NULL ; /* done with attributes */

#undef  AF
#define AF 0  /* nothing to do if we have to quit early */

      /*- close header -*/

      if( nel->vec_len == 0    ||     /* An 'empty' element (no data) */
          nel->vec_num == 0    ||
          nel->vec_typ == NULL ||
          nel->vec     == NULL   ){

        nout = NI_stream_writestring( ns , att_trail ) ; ADDOUT ;
        nout = NI_stream_writestring( ns , "/>\n" ) ; ADDOUT ;
        return ntot ;                 /* done with empty element */
      }

      /*- if here, must write some data out -*/

      /* first, terminate the header,
         and allocate space for the write buffer (1 row at a time) */

      switch( tmode ){
         default:
         case NI_TEXT_MODE:
            btt = ">\n" ;                             /* add a newline */
#ifndef USE_NEW_IOFUN
            nwbuf = 5*NI_element_rowsize(nel) + 16 ;  /* text buffer  */
#endif
         break ;

         case NI_BINARY_MODE:
            btt = ">" ;                               /* no newline   */
#ifndef USE_NEW_IOFUN
            nwbuf = NI_element_rowsize(nel) ;         /* binary buffer */
#endif
         break ;

         case NI_BASE64_MODE:
            btt = ">\n" ;                             /* add a newline */
#ifndef USE_NEW_IOFUN
            nwbuf = NI_element_rowsize(nel) ;         /* binary buffer */
            load_encode_table() ;                     /* initialize B64 */
#endif
         break ;
      }

      nout = NI_stream_writestring( ns , att_trail ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , btt ) ; ADDOUT ;

#ifdef USE_NEW_IOFUN   /** 13 Feb 2003: output is now done elsewhere **/

      if( !header_only ){
        nout = NI_write_columns( ns, nel->vec_num, nel->vec_typ,
                                     nel->vec_len, nel->vec    , tmode ) ;
        ADDOUT ;
      }

#else                  /** OLD way of doing output **/

      /* allocate output buffer */

      wbuf = NI_malloc(nwbuf+128) ;  /* 128 for the hell of it */

      /* allocate buffer for base64 encoding  */

      if( tmode == NI_BASE64_MODE ){
         bbuf = NI_malloc(  nwbuf+128) ; bb = 0 ;  /* binary buffer */
         cbuf = NI_malloc(2*nwbuf+128) ; cc = 0 ;  /* base64 buffer */
      } else {
         bbuf = cbuf = NULL ;
      }

#undef  AF
#define AF NI_free(wbuf);NI_free(bbuf);NI_free(cbuf)

      /*- loop over output rows and write results -*/

      for( row=0 ; row < nel->vec_len ; row++ ){
#ifdef NIML_DEBUG
NI_dpr("  start write of row %d:",row) ;
#endif

        /* initialize this row's output */

        switch( tmode ){
           case NI_TEXT_MODE:    wbuf[0] = '\0'; break; /* clear buffer */

           case NI_BASE64_MODE:
           case NI_BINARY_MODE:  jj = 0 ;        break; /* clear byte count */
        }

        /* write data for this row into wbuf */

        for( col=0 ; col < nel->vec_num ; col++ ){
#ifdef NIML_DEBUG
NI_dpr(" %d[%d]",col,nel->vec_typ[col]) ;
#endif

         switch( tmode ){

          /*----- encode one value to output, according to its type -----*/
          case NI_TEXT_MODE:{
           jj = strlen(wbuf) ;
#ifdef NIML_DEBUG
NI_dpr("[jj=%d]",jj);
#endif
           switch( nel->vec_typ[col] ){
            default:                    /* Line is unimplemented */
            break ;

            case NI_STRING:{
              char **vpt = (char **) nel->vec[col] ;
              char *str = quotize_string(vpt[row]) ;  /* format for output */
              int nn = strlen(str) ;                  /* how much? */
              int nadd = jj+nn+8-nwbuf ;
              if( nadd > 0 ){                         /* too long for wbuf? */
                 nwbuf += nadd ;
                 wbuf = NI_realloc(wbuf,nwbuf+128) ;  /* extend wbuf size */
              }
              sprintf(wbuf+jj," %s",str); NI_free(str);  /* write output */
            }
            break ;

            /* numeric types below here */

            case NI_BYTE:{
              byte *vpt = (byte *) nel->vec[col] ;
              sprintf(wbuf+jj," %u",(unsigned int)vpt[row]) ;
            }
            break ;

            case NI_SHORT:{
              short *vpt = (short *) nel->vec[col] ;
              sprintf(wbuf+jj," %d",(int)vpt[row]) ;
            }
            break ;

            case NI_INT:{
              int *vpt = (int *) nel->vec[col] ;
#ifdef NIML_DEBUG
NI_dpr("[int=%d]",vpt[row]) ;
#endif
              sprintf(wbuf+jj," %d",vpt[row]) ;
            }
            break ;

            /* multiple byte structs */

            case NI_RGB:{
              rgb *vpt = (rgb *) nel->vec[col] ;
              sprintf(wbuf+jj,"  %u %u %u",vpt[row].r,vpt[row].g,vpt[row].b) ;
            }
            break ;

            case NI_RGBA:{
              rgba *vpt = (rgba *) nel->vec[col] ;
              sprintf(wbuf+jj,"  %u %u %u %u",
                      vpt[row].r,vpt[row].g,vpt[row].b,vpt[row].a) ;
            }
            break ;

            /* for floating point outputs,
               first print to a temp string,
               then clip trailing and leading blanks */

            case NI_FLOAT:{
              float *vpt = (float *) nel->vec[col] ;
              char fbuf[32] ; int ff ;
#ifdef NIML_DEBUG
NI_dpr("[float=%g]",vpt[row]) ;
#endif
              sprintf(fbuf,"%12.6g",vpt[row]) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(wbuf+jj," %s",fbuf+ff) ;
            }
            break ;

            case NI_DOUBLE:{
              double *vpt = (double *) nel->vec[col] ;
              char fbuf[32] ; int ff ;
              sprintf(fbuf,"%18.12g",vpt[row]) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(wbuf+jj," %s",fbuf+ff) ;
            }
            break ;

            case NI_COMPLEX:{
              complex *vpt = (complex *) nel->vec[col] ;
              char fbuf[32],gbuf[32] ; int ff,gg ;
              sprintf(fbuf,"%12.6g",vpt[row].r) ;
              for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
              for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
              sprintf(gbuf,"%12.6g",vpt[row].i) ;
              for( gg=strlen(gbuf) ; gbuf[gg]==' ' ; gg-- ) gbuf[gg] = '\0' ;
              for( gg=0 ; gbuf[gg] == ' ' ; gg++ ) ;
              sprintf(wbuf+jj,"  %s %s",fbuf+ff,gbuf+gg) ;
            }
            break ;

           } /* end of switch on datum type */
          }
          break ; /* end of NI_TEXT_MODE */

          /*----- put the binary form of this element into wbuf+jj -----*/
          case NI_BASE64_MODE:
          case NI_BINARY_MODE:{
           switch( nel->vec_typ[col] ){
            default: break ;              /* should not happen */

            case NI_BYTE:{
              byte *vpt = (byte *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(byte));
              jj += sizeof(byte);
            }
            break ;

            case NI_SHORT:{
              short *vpt = (short *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(short));
              jj += sizeof(short);
            }
            break ;

            case NI_INT:{
              int *vpt = (int *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(int));
              jj += sizeof(int);
            }
            break ;

            case NI_FLOAT:{
              float *vpt = (float *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(float));
              jj += sizeof(float);
            }
            break ;

            case NI_DOUBLE:{
              double *vpt = (double *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(double));
              jj += sizeof(double);
            }
            break ;

            case NI_COMPLEX:{
              complex *vpt = (complex *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(complex));
              jj += sizeof(complex);
            }
            break ;

            case NI_RGB:{
              rgb *vpt = (rgb *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(rgb));
              jj += sizeof(rgb);
            }
            break ;

            case NI_RGBA:{
              rgba *vpt = (rgba *) nel->vec[col] ;
              memcpy(wbuf+jj,(char *)(vpt+row),sizeof(rgba));
              jj += sizeof(rgba);
            }
            break ;

           } /* end of switch on datum type */
          }
          break ; /* end of NI_BINARY_MODE and NI_BASE64_MODE */

         } /* end of switch on tmode */
        } /* end of loop over columns */
#ifdef NIML_DEBUG
NI_dpr(" ! wbuf=%s[%d] tmode=%d\n",wbuf,strlen(wbuf),tmode) ;
#endif

        /*- actually write this row of data out -*/

        switch( tmode ){
          case NI_TEXT_MODE:     /* each row is on a separate line */
            strcat(wbuf,"\n") ;
#ifdef NIML_DEBUG
NI_dpr("  and writing it [%d]\n",strlen(wbuf) ) ;
#endif
            nout = NI_stream_writestring( ns , wbuf ) ;
            ADDOUT ;
          break ;

          case NI_BINARY_MODE:
            nout = NI_stream_write( ns , wbuf , nwbuf ) ;
            ADDOUT ;
          break ;

          /* must convert binary byte triples into base64 byte quads */
          case NI_BASE64_MODE:{
            int nb , nb3 , nb64 , pp,qq ;
            byte a,b,c,w,x,y,z ;

            /* bbuf = bb bytes of unprocessed data from last row
                      plus nwbuf bytes of data from new row      */

            memcpy(bbuf+bb,wbuf,nwbuf) ;    /* add wbuf to bbuf      */
            nb = nwbuf+bb ;                 /* number of bytes in bb */
            if( nb < 3 ){ bb = nb; break; } /* need at least 3 bytes */
            nb3 = 3*(nb/3) ;                /* will encode nb3 bytes */

            /* cbuf = base64 output buffer */
            /* cc   = # bytes written since last EOL */

            for( qq=pp=0 ; pp < nb3 ; ){
              a = bbuf[pp++] ; b = bbuf[pp++] ; c = bbuf[pp++] ;
              B64_encode3(a,b,c,w,x,y,z) ;
              cbuf[qq++] = w ; cbuf[qq++] = x ;
              cbuf[qq++] = y ; cbuf[qq++] = z ;
              cc += 4; if( cc > 64 ){ cbuf[qq++]=B64_EOL2; cc=0; }
            }

            /* write base64 bytes to output */

            nout = NI_stream_write( ns , cbuf , qq ) ;
            ADDOUT ;

            /* deal with leftover bytes in bbuf */

            bb = nb - nb3 ;  /* num leftover bytes = 0, 1, or 2 */
            if( bb > 0 ){
              bbuf[0] = bbuf[nb3] ;                /* copy leftovers   */
              if( bb > 1 ) bbuf[1] = bbuf[nb3+1] ; /* to front of bbuf */
            }
          }
          break ;
        }

      } /* end of loop over rows */

      /* In base64 mode, we might have to clean
         up if there are any leftover bytes in bbuf. */

      if( tmode == NI_BASE64_MODE && bb > 0 ){
        byte w,x,y,z ;
        if( bb == 2 ) B64_encode2(bbuf[0],bbuf[1],w,x,y,z) ;
        else          B64_encode1(bbuf[0],w,x,y,z) ;
        cbuf[0] = w ; cbuf[1] = x ;
        cbuf[2] = y ; cbuf[3] = z ; cbuf[4] = B64_EOL2 ;
        nout = NI_stream_write( ns , cbuf , 5 ) ;
        ADDOUT ;
      }

      NI_free(wbuf) ;  /* don't need output buffer no more no how */
      NI_free(bbuf) ;
      NI_free(cbuf) ;

#undef  AF
#define AF 0  /* nothing to do if we quit early now */

#endif  /* USE_NEW_IOFUN [13 Feb 2003] */

      /*- write element trailer -*/

#if 0
      nout = NI_stream_writestring( ns , att_trail ) ;
      ADDOUT ;
#endif
      if( header_sharp ){ nout = NI_stream_writestring(ns,"# "); ADDOUT; }
      nout = NI_stream_writestring( ns , "</" ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , nel->name ) ; ADDOUT ;
      nout = NI_stream_writestring( ns , ">\n\n" ) ; ADDOUT ;

      return ntot ;

   } /* end of write data element */

   return -1 ; /* should never be reachable */
}
