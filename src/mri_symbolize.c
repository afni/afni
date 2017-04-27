#include "mrilib.h"
#include "niml.h"

static int nerr=0 ;
int SYM_expand_errcount(void){ return nerr; }  /* 03 May 2007 */

/*----------------------------------------------------------------------------*/

#define INIT_OUTBUF                                                         \
 do{ if( outbuf == NULL ){                                                  \
       outbuf = THD_zzprintf(outbuf,"***** Scanned GLT messages *****\n") ; \
       outbuf = THD_zzprintf(outbuf,"++ INFO: -gltsym is: '%s'\n",gltsym) ; \
 } } while(0)

#define NLAST_TEST 999999

/*----------------------------------------------------------------------------*/
/* This funcion is for testing a gltsym string for basic validity.
   It returns a string that contains the messages generated during the tests.
    * varlist = space (or comma or semicolon) delimited list of variable names
    * gltsym  = expression for GLT
   If NULL is returned, there were no problems detected.  [01 May 2015]
*//*--------------------------------------------------------------------------*/

char * SYM_test_gltsym( char *varlist , char *gltsym )
{
   char *outbuf=NULL ; int nbad=0 , nerrtemp=0 ;
   NI_str_array *vsar=NULL ; int vv=0 ; char *vnam=NULL , *mbuf=NULL ;
   int nrang=0 ; SYM_irange *rang=NULL ;
   floatvecvec *fvv=NULL ;

ENTRY("SYM_test_gltsym") ;

   /* check for bad inputs */

   if( gltsym == NULL || *gltsym == '\0' ){
     gltsym = "<EMPTY STRING>" ;  /* must have something for INIT_OUTBUF */
     INIT_OUTBUF ;
     outbuf = THD_zzprintf(outbuf,"** ERROR: cannot continue\n") ; nbad++ ; nerr++ ;
     RETURN(outbuf) ;
   }

   INIT_OUTBUF ;
   if( varlist == NULL || *varlist == '\0' ){
     outbuf = THD_zzprintf(outbuf,"** ERROR: Allowed variable list is empty\n") ; nbad++ ; nerr++ ;
   } else {
     outbuf = THD_zzprintf(outbuf,"++ INFO: Allowed variable list is '%s'\n",varlist) ;
   }

   /* decode the variable list into individual names */

   STATUS("scanning varlist") ;
   vsar = NI_decode_string_list( varlist , ",;" ) ;
   if( vsar == NULL ){
     outbuf = THD_zzprintf(outbuf,"** ERROR: Cannot decode variable list names!\n") ; nbad++ ; nerr++ ;
     RETURN(outbuf) ;
   }

   /* make list of variable for parsing, with made up data ranges */

   STATUS("creating fictional SYM_irange") ;
   rang = (SYM_irange *)calloc(sizeof(SYM_irange),vsar->num) ;
   for( nrang=vv=0 ; vv < vsar->num ; vv++ ){
     vnam = vsar->str[vv] ;
     if( vnam == NULL || *vnam == '\0' ) continue ; /* bad ==> skip */
     if( !isalpha(vnam[0]) ){
       outbuf = THD_zzprintf(outbuf,"** ERROR: Variable name '%s' doesn't start with alphabetic character\n",vnam) ;
       nbad++ ; nerr++ ;
     } else if( strlen(vnam) > 63 ){
       outbuf = THD_zzprintf(outbuf,"** ERROR: Variable name '%s' is too long (> 63)\n",vnam) ;
       nbad++ ; nerr++ ;
     } else {
       rang[nrang].nbot = 0 ;
       rang[nrang].ntop = NLAST_TEST ;
       rang[nrang].gbot = 0 ;
       NI_strncpy(rang[nrang].name,vnam,64) ; nrang++ ;
     }
   }

   NI_delete_str_array(vsar);

   /* now parse gltsym to see if it works OKAY */

   nerrtemp = nerr ;
   SET_message_outbuf(1) ;

   fvv = SYM_expand_ranges( NLAST_TEST , nrang , rang , gltsym ) ;

   if( fvv != NULL ){
     int iv ; floatvec *fv ;
     STATUS("freeing fvv") ;
     for( iv=0 ; iv < fvv->nvec ; iv++ ){
       fv = fvv->fvar + iv ;
       if( fv->ar != NULL ) free(fv->ar) ;
     }
     free(fvv->fvar) ; free(fvv) ;
   }

   nbad += nerr - nerrtemp ;

   mbuf  = GET_message_outbuf() ;
   if( mbuf != NULL ){
     size_t ll = strlen(outbuf) + strlen(mbuf) + 32 ;
     outbuf = (char *)realloc(outbuf,ll) ; strcat(outbuf,mbuf) ;
   }
   SET_message_outbuf(0) ;

   if( nbad == 0 )
     outbuf = THD_zzprintf(outbuf,"++ INFO: This gltsym appears to be OKAY :-)\n") ;
   else
     outbuf = THD_zzprintf(outbuf,"** SORRY: This gltsym appears to be BAD :-(\n") ;

   RETURN(outbuf) ;
}

/*----------------------------------------------------------------------------*/
/*! Expand a string like "Fred 2*Jed -Ned[1..3]" into a float vector
    that comprises the weights for each beta element in 3dDeconvolve.

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

ENTRY("SYM_expand_ranges") ;

   if( nlast < 0 ) RETURN(NULL) ;  /* bad input */

   /* check if have anything to scan for */

   if( nrang < 1 || rang == NULL || str == NULL || *str == '\0' ) RETURN(NULL) ;

   /* check if input line is a comment */

   for( ii=0 ; str[ii] != '\0' && isspace(str[ii]) ; ii++ ) ;  /*nada*/

   if( str[ii] == '\0' ||                   /* all blank */
       str[ii] == '#'  ||                   /* starts with "#" */
      (str[ii] == '/' && str[ii+1] == '/')  /* starts with "//" */
   ) RETURN(NULL) ;

   fv      = (floatvec *)malloc(sizeof(floatvec)) ;    /* create empty output */
   fv->nar = nlast+1 ;
   fv->ar  = (float *)calloc(sizeof(float),nlast+1) ;

   /* break input string into separate chunks */

   sar = NI_decode_string_list( str , "~" ) ;
   if( sar == NULL ){
     fvv  = (floatvecvec *)malloc(sizeof(floatvecvec)) ;
     fvv->nvec = 1 ;
     fvv->fvar = fv ;
     ERROR_message("empty line in -gltsym?") ; nerr++ ;
     RETURN(fvv) ;
   }

   /* scan chunks and merge isolated '+' or '-' with next chunk [01 May 2015] */

#if 1
   for( ss=0 ; ss < sar->num ; ss++ ){
     qstr = sar->str[ss] ;
     if( qstr == NULL || *qstr == '\0' ) continue ;          /* bad entry? */
     if( *qstr == '#' ||                              /* comment ends line */
        (*qstr == '/' && *(qstr+1) == '/') ) break ;

     /* process a string of length=1 and is either "+" or "-" */

     if( strlen(qstr) == 1 && (*qstr == '+' || *qstr == '-') ){
       if( ss == sar->num-1 ){                     /* at end ==> mark to ignore this */
         ERROR_message("-gltsym: isolated '%s' at end of -gltsym? IGNORING!",qstr) ;
         qstr[0] = '\0' ; nerr++ ;
       } else if ( *qstr == '+' ){                 /* can just ignore this "+"! */
         INFO_message("INFO: -gltsym: isolated '+' is being ignored") ;
         qstr[0] = '\0' ;
       } else {                                    /* this is a "-" */
         qpt = sar->str[ss+1] ;                    /* the next string in the list */
         if( qpt == NULL || *qpt == '\0' ){        /* is it bad?? should never happen */
           ERROR_message("-gltsym: isolated '%s' is followed by NULL entry in? IGNORING!",qstr) ;
           qstr[0] = '\0' ; nerr++ ;
         } else if( *qpt == '+' || *qpt == '-' ){  /* next string should not already be signed! */
           ERROR_message("-gltsym: isolated '-' is followed by '%s' which is already signed? IGNORING!",qpt) ;
           qstr[0] = '\0' ; nerr++ ;
         } else {                                  /* everything is cool: attach sign to next string */
           /* alloc with NIML and do not lose '-'   25 Jul 2016 [rickr] */
           qls = (char *)NI_malloc(char, sizeof(char)*(strlen(qpt)+4)) ;
           strcpy(qls,qstr) ; strcat(qls,qpt) ;
           INFO_message("INFO: -gltsym: isolated '-' is merged with following '%s'",qpt) ;
           NI_free(qpt) ; sar->str[ss+1] = qls ; qstr[0] = '\0' ;
         }
       }
     }
   }
#endif

   /* process each chunk */

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
       char *ept ;
       fac = (float)strtod(qstr,&ept) ;
       if( fac == 0.0f && ept == qstr ){     /* bad factor interpretation? */
         fac = 1.0f ;            /* ==> replace with 1, and bitch about it */
         WARNING_message(
           "-gltsym: Can't interpret '*' scale factor in '%s' -- replaced by 1",
           qstr) ;
       }
       if( ept != qpt )  /* 27 May 2008 */
         WARNING_message(
           "-gltsym: '*' scale factor in '%s' not at start of string?",qstr ) ;
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
       ERROR_message("-gltsym: can't match symbolic name '%s'\n",qpt) ;
       nerr++ ; free((void *)qstr) ; continue ;
     }
                                       /* now scan for intlist, if present */
     if( qls != NULL ){
       MCW_intlist_allow_negative( (rang[rr].nbot < 0) ) ;
       qlist = MCW_get_intlist( rang[rr].ntop+1 , qls ) ;

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
           ERROR_message("mismatch in use of -gltsym [[...]]: '%s'\n",
                   sar->str[ss] ) ;
           nerr++ ;free((void *)qlist) ; free((void *)qstr) ;
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
         ERROR_message("-gltsym subscript %s[%d] out of range %d..%d\n",
                 rang[rr].name , qlist[ii+1] , rang[rr].nbot,rang[rr].ntop ) ;
         nerr++ ; continue ;
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

   RETURN(fvv) ;
}
