#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Expand a string like "Fred 2*Jed -Ned[1..3]" into a float vector.

    Each SYM_irange struct has 4 fields
      - name = string that names this field
      - nbot,ntop = range of indexes valid for this name (nbot <= ntop, please)
      - gbot = global index that maps to nbot

    The set of structs in rang[] should collectively span global indexes
    from 0..nlast (inclusive).  The returned floatvec will have nlast+1 entries.
------------------------------------------------------------------------------*/

floatvecvec * SYM_expand_ranges( int nlast, int nrang, SYM_irange *rang, char *str )
{
   floatvec *fv ;
   floatvecvec *fvv=NULL ;
   int rr , ii , ss , gg, *qlist , nvec=0 , iv ;
   NI_str_array *sar ;
   char qname[64] , *qstr , *qpt , *qls ;
   float fac ;

   if( nlast < 0 ) return NULL ;  /* bad input */

   /* check if have anything to scan for */

   if( nrang < 1 || rang == NULL || str == NULL || *str == '\0' ) return NULL ;

   /* check if input line is a comment */

   for( ii=0 ; str[ii] != '\0' && isspace(str[ii]) ; ii++ ) ;  /*nada*/

   if( str[ii] == '\0' ||                   /* all blank */
       str[ii] == '#'  ||                   /* starts with "#" */
      (str[ii] == '/' && str[ii+1] == '/')  /* starts with "//" */
   ) return NULL ;

   fv     = (floatvec *)malloc(sizeof(floatvec)) ;    /* create empty output */
   fv->nar = nlast+1 ;
   fv->ar  = (float *)calloc(sizeof(float),nlast+1) ;

   /* break input string into separate chunks */

   sar = NI_decode_string_list( str , "~" ) ;
   if( sar == NULL ){
     fvv  = (floatvecvec *)malloc(sizeof(floatvecvec)) ;
     fvv->nvec = 1 ;
     fvv->fvar = fv ;
     return fvv ;
   }

   /* scan each chunk */

   for( ss=0 ; ss < sar->num ; ss++ ){
     qstr = sar->str[ss] ;
     if( qstr == NULL || *qstr == '\0' ) continue ;          /* bad entry? */
     if( *qstr == '#' ||                              /* comment ends line */
        (*qstr == '/' && *(qstr+1) == '/') ) break ;

     qstr  = strdup(sar->str[ss]) ;               /* duplicate for surgery */
     qls   = strchr(qstr,'[') ;      /* find and decode "[...]" subscripts */
     qlist = NULL ;                        /* if they are present, that is */
     if( qls != NULL ){
       *qls  = '\0' ;                  /* cut string off at '[' subscripts */
       qls++ ;                      /* will scan for intlist starting here */
     }

     qpt = strchr(qstr,'*') ;           /* find and decode factor in front */
     if( qpt != NULL ){                       /* if it is present, that is */
       fac = (float)strtod(qstr,NULL) ;
       if( fac == 0.0 && *qstr != '0' ) fac = 1.0 ;
       qpt++ ;
     } else if( *qstr == '+' ){                  /* "+" is same as "+1.0*" */
       qpt = qstr+1 ; fac =  1.0 ;
     } else if( *qstr == '-' ){                  /* "-" is same as "-1.0*" */
       qpt = qstr+1 ; fac = -1.0 ;
     } else {                                            /* default is "+" */
       qpt = qstr   ; fac =  1.0 ;
     }

     for( rr=0 ; rr < nrang ; rr++ )                 /* match name in list */
       if( strcmp(qpt,rang[rr].name) == 0 ) break ;
     if( rr == nrang ){                                      /* no match!? */
       fprintf(stderr,"** ERROR: can't match symbolic name '%s'\n",qpt) ;
       free((void *)qstr) ; continue ;
     }
                                       /* now scan for intlist, if present */
     if( qls != NULL ){
       MCW_intlist_allow_negative( (rang[rr].nbot < 0) ) ;
       qlist = MCW_get_intlist( rang[rr].ntop , qls ) ;

       if( qlist != NULL && *qls == '[' ){  /** [[...]] type of subscript **/
         if( nvec == 0 ){
           nvec = qlist[0] ;
           fvv  = (floatvecvec *)malloc(sizeof(floatvecvec)) ;
           fvv->nvec = nvec ;
           fvv->fvar = (floatvec *)calloc(sizeof(floatvec),nvec) ;
           for( iv=0 ; iv < nvec ; iv++ ){
             fvv->fvar[iv].nar = nlast+1 ;
             fvv->fvar[iv].ar  = (float *)calloc(sizeof(float),nlast+1) ;
           }
         } else if( qlist[0] != nvec ){
           fprintf(stderr,"** ERROR: mismatch in use of -gltsym [[...]]: '%s'\n",
                   sar->str[ss] ) ;
           free((void *)qlist) ; free((void *)qstr) ;
           continue ;
         }
         for( iv=0 ; iv < nvec ; iv++ ){
           gg = qlist[iv+1] - rang[rr].nbot + rang[rr].gbot ;
           if( gg >= 0 && gg <= nlast ) fvv->fvar[iv].ar[gg] = fac ;
         }
         free((void *)qlist) ; free((void *)qstr) ;
         continue ;          /** skip to next one, since this was special **/
       }
     }
                                         /* make up a fake list, if needed */
     if( qlist == NULL ){
       qlist = (int *)malloc(sizeof(int)*(rang[rr].ntop-rang[rr].nbot+2)) ;
       qlist[0] = rang[rr].ntop-rang[rr].nbot+1 ;
       for( ii=0 ; ii < qlist[0] ; ii++ ) qlist[ii+1] = rang[rr].nbot+ii ;
     }
                                         /* insert values into output list */

     for( ii=0 ; ii < qlist[0] ; ii++ ){
       if( qlist[ii+1] < rang[rr].nbot || qlist[ii+1] > rang[rr].ntop ){
         fprintf(stderr,"** ERROR: subscript %s[%d] out of range %d..%d\n",
                 rang[rr].name , qlist[ii+1] , rang[rr].nbot,rang[rr].ntop ) ;
         continue ;
       }
       gg = qlist[ii+1] - rang[rr].nbot + rang[rr].gbot ;
       if( gg >= 0 && gg <= nlast ) fv->ar[gg] = fac ;
     }

     free((void *)qlist) ; free((void *)qstr) ;
   }
   MCW_intlist_allow_negative(0) ;

   NI_delete_str_array(sar);

   /* if had no [[...]] subscripts, only have 1 vector for output */

   if( nvec == 0 ){
     fvv  = (floatvecvec *)malloc(sizeof(floatvecvec)) ;
     fvv->nvec = 1 ;
     fvv->fvar = fv ;
   } else {              /* have multiple outputs */
     for( iv=0 ; iv < nvec ; iv++ ){
       for( gg=0 ; gg <= nlast ; gg++ ){
        if( fvv->fvar[iv].ar[gg] == 0.0f ) fvv->fvar[iv].ar[gg] = fv->ar[gg] ;
       }
     }
     KILL_floatvec(fv) ;
   }

   return fvv ;
}
