/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "multivector.h"

/*********************************************************************
  Routines to handle the multivector data type: a collection of
  1D arrays of strings or floats, all the same length.

  RWCox - May 1999
**********************************************************************/

static void MV_fval_to_char( float qval , char * buf ) ;

/*-------------------------------------------------------------------
   Check 2 strings for equivalence, regardless of case
---------------------------------------------------------------------*/

static int my_strequiv( char * s1 , char * s2 )
{
   int ii , ll ;

   if( s1 == NULL && s2 == NULL ) return 1 ;
   if( s1 == NULL || s2 == NULL ) return 0 ;
   ii = strlen(s1) ; ll = strlen(s2) ; if( ii != ll ) return 0 ;
   for( ii=0 ; ii < ll ; ii++ )
      if( toupper(s1[ii]) != toupper(s2[ii]) ) return 0 ;
   return 1 ;
}

/*--------------------------------------------------------------------
  Throw away a multivector
----------------------------------------------------------------------*/

void multivector_free( multivector * mv )
{
   int ii ;

   if( mv == NULL ) return ;

   if( mv->name != NULL ) free(mv->name) ;
   if( mv->type != NULL ) free(mv->type) ;
   if( mv->label != NULL )
      for( ii=0 ; ii < mv->nvec ; ii++ ) free(mv->label[ii]) ;
   if( mv->vec != NULL )
      for( ii=0 ; ii < mv->nvec ; ii++ ) free(mv->vec[ii]) ;

   free(mv) ; return ;
}

/*-------------------------------------------------------------------
  Read a multivector from disk
---------------------------------------------------------------------*/

#define NVMAX 128
#define LBUF  2048
#define SEPCH " \t\n"

#define MERR(ss) \
   fprintf(stderr,"*** multivector_read error; file=%s: %s\n",fname,ss)

multivector * multivector_read( char * fname )
{
   FILE * fp ;
   char buf[LBUF] ;
   char * ptr , * pp[NVMAX] ;
   multivector * mv ;
   int ii , ll , nvec,ndim , first=0 ;
   float val ;

   /*-- sanity check --*/

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   fp = fopen( fname , "r" ) ;
   if( fp == NULL ){ MERR("can't open file"); return NULL; }

   mv = (multivector *) malloc( sizeof(multivector) ) ;
   nvec = ndim = mv->nvec = mv->ndim = 0 ;
   mv->name = strdup(fname) ;
   mv->type = NULL ; mv->label = NULL ; mv->vec = NULL ;

   /*-- read and process any header comments --*/

   while(1){
      ptr = fgets( buf , LBUF , fp ) ;
      if( ptr == NULL ){
         fclose(fp); multivector_free(mv); MERR("no data"); return NULL;
      }

      ll = strlen(buf) ;
      for( ii=ll-1 ; ii >= 0 ; ii-- ) if( !isspace(buf[ii]) ) break ;
      if( ii < 0 ) continue ;       /* was all blanks; goto next line */

      if( buf[0] != '#' ){ first=1; break; }   /* not a header line ==> done */

      ptr = strtok( buf , SEPCH ) ;

      /* handle #NAME */

      if( my_strequiv(ptr,"#NAME") ){
         ptr = strtok( NULL , SEPCH ) ;
         if( ptr != NULL ){
            free(mv->name) ; mv->name = strdup(ptr) ;
         }
         continue ;  /* goto next line */
      }

      /* handle #TYPE */

      if( my_strequiv(ptr,"#TYPE") ){
         int ntyp=0 , typ[NVMAX] ;

         if( mv->type != NULL ){
            fclose(fp); multivector_free(mv); MERR("second #TYPE"); return NULL;
         }

         /* scan tokens for type strings */

         while(1){
            ptr = strtok( NULL , SEPCH ) ;
            if( ptr == NULL ) break ;

            if( ntyp >= NVMAX ){
               fclose(fp); multivector_free(mv); MERR("oversize #TYPE"); return NULL;
            }

                 if( my_strequiv(ptr,"STRING") ) typ[ntyp++] = MV_STRING ;
            else if( my_strequiv(ptr,"FLOAT")  ) typ[ntyp++] = MV_FLOAT  ;
            else {
               fclose(fp); multivector_free(mv); MERR("illegal #TYPE"); return NULL;
            }
         }

         if( ntyp == 0 ){
            fclose(fp); multivector_free(mv); MERR("illegal #TYPE"); return NULL;
         }

         if( mv->nvec > 0 && ntyp != mv->nvec ){
            fclose(fp); multivector_free(mv); MERR("illegal #TYPE count"); return NULL;
         }

         if( mv->nvec == 0 ) nvec = mv->nvec = ntyp ;
         mv->type = (int *) malloc( sizeof(int) * ntyp ) ;
         for( ii=0 ; ii < ntyp ; ii++ ) mv->type[ii] = typ[ii] ;
         continue ;  /* goto next line */
      }

      /* handle #LABEL */

      if( my_strequiv(ptr,"#LABEL") ){
         int nlab=0 ; char * lab[NVMAX] ;

         if( mv->label != NULL ){
            fclose(fp); multivector_free(mv); MERR("second #LABEL"); return NULL;
         }

         /* scan tokens for label strings */

         while(1){
            ptr = strtok( NULL , SEPCH ) ;
            if( ptr == NULL ) break ;

            if( nlab >= NVMAX ){
               for( ii=0 ; ii < nlab ; ii++ ) free( lab[ii] ) ;
               fclose(fp); multivector_free(mv); MERR("oversize #LABEL"); return NULL;
            }

            lab[nlab++] = strdup(ptr) ;
         }

         if( nlab == 0 ){
            fclose(fp); multivector_free(mv); MERR("illegal #LABEL"); return NULL;
         }

         if( mv->nvec > 0 && nlab != mv->nvec ){
            for( ii=0 ; ii < nlab ; ii++ ) free( lab[ii] ) ;
            fclose(fp); multivector_free(mv); MERR("illegal #LABEL count"); return NULL;
         }

         if( mv->nvec == 0 ) nvec = mv->nvec = nlab ;
         mv->label = (char **) malloc( sizeof(char *) * nlab ) ;
         for( ii=0 ; ii < nlab ; ii++ ) mv->label[ii] = lab[ii] ;
         continue ;  /* goto next line */
      }

      /* otherwise, just ignore the line (it's a comment, maybe) */

   } /* end of scan over header lines */

   /*-- read and store data lines --*/

   while(1){
      if( !first ) ptr = fgets( buf , LBUF , fp ) ;
      if( ptr == NULL ) break ;        /* end of input */
      first = 0 ;

      ll = strlen(buf) ;
      for( ii=ll-1 ; ii >= 0 ; ii-- ) if( !isspace(buf[ii]) ) break ;
      if( ii < 0 ) continue ;         /* was all blanks; goto next line */
      if( buf[0] == '#' ) continue ;  /* a comment line; goto next line */

      /* extract tokens from this line */

      pp[0] = strtok(buf,SEPCH) ; if( pp[0] == NULL ) continue ;
      ll = 1 ;
      while(1){
         pp[ll] = strtok(NULL,SEPCH) ; if( pp[ll] == NULL ) break ;
         ll++ ;
      }

      /* check count */

      if( nvec == 0 ){
          mv->nvec = nvec = ll ;
          if( nvec > NVMAX ) MERR("too many columns") ;
      }
      if( ll > nvec ) ll = nvec ;

      /* make type, if needed */

      if( mv->type == NULL ){
         mv->type = (int *) malloc( sizeof(int) * nvec ) ;
         for( ii=0 ; ii < ll ; ii++ ){
            val = strtod( pp[ii] , &ptr ) ;
            if( *ptr != '\0' ) mv->type[ii] = MV_STRING ;
            else               mv->type[ii] = MV_FLOAT  ;
         }
         for( ; ii < nvec ; ii++ )    /* this can only happen if #LABEL  */
            mv->type[ii] = MV_FLOAT ; /* is used and has too many labels */
      }

      /* initialize vector space, if needed */

      if( mv->vec == NULL ){
         mv->vec = (void **) malloc( sizeof(void *) * nvec ) ;
         for( ii=0 ; ii < nvec ; ii++ )
            mv->vec[ii] = (void *) malloc( sizeof(float)*16 ) ;
      }

      /* expand vector space for new row of data,
         convert tokens to values and store them in this space */

      for( ii=0 ; ii < nvec ; ii++ ){
         switch( mv->type[ii] ){
            case MV_FLOAT:{
               float * fpt ;
               mv->vec[ii] = (void *) realloc( mv->vec[ii], sizeof(float)*(ndim+1) );
               fpt = (float *) mv->vec[ii] ;
               fpt[ndim] = (ii < ll) ? strtod( pp[ii] , NULL ) : 0.0 ;
            }
            break ;

            case MV_STRING:{
               char ** cpt ;
               mv->vec[ii] = (void *) realloc( mv->vec[ii], sizeof(char *)*(ndim+1) );
               cpt = (char **) mv->vec[ii] ;
               cpt[ndim] = (ii < ll) ? strdup(pp[ii]) : strdup("\0") ;
            }
            break ;
          }
      }
      ndim++ ;   /* just added a new element! */

   } /* end of processing this line */

   /*-- done --*/

   mv->ndim = ndim ; return mv ;
}

/*-------------------------------------------------------------------
   (Re)set the name stored in a multivector.
   nname can be NULL t{o clear the name.
---------------------------------------------------------------------*/

void multivector_set_name( multivector * mv , char * nname )
{
   if( mv->name != NULL ){ free(mv->name); mv->name = NULL; }

   if( nname != NULL ) mv->name = strdup(nname) ;
   return ;
}


/*-------------------------------------------------------------------
  Write a multivector to disk.
  Returns 0 if it fails, 1 if it succeeds.
---------------------------------------------------------------------*/

int multivector_write( char * fname , multivector * mv )
{
   int nvec,ndim , ii,kk,ll , width[NVMAX] ;
   char buf[LBUF] , fbuf[32] ;
   FILE * fp ;
   float * fpt ;
   char ** cpt ;

   /*-- sanity checks --*/

   if( !THD_filename_ok(fname) || mv == NULL ) return 0 ;

   nvec = mv->nvec ; ndim = mv->ndim ;
   if( nvec < 1 || ndim < 1 ) return 0 ;

   if( mv->type == NULL || mv->vec == NULL ) return 0 ;

   /*-- open file, write headers --*/

   if( strcmp(fname,"-") == 0 ){
      fp = stdout ;
   } else {
      fp = fopen( fname , "w" ) ; if( fp == NULL ) return 0 ;
   }

   if( mv->name != NULL ) fprintf(fp,"#NAME %s\n",mv->name) ;

   if( mv->label != NULL ){
      sprintf(buf,"#LABEL") ;
      for( ii=0 ; ii < nvec ; ii++ ){
         ll = strlen(buf) ;
         if( mv->label[ii] != NULL )
            sprintf(buf+ll," %s",mv->label[ii]) ;
         else
            sprintf(buf+ll," -none-") ;
      }
      fprintf(fp,"%s\n",buf) ;
   }

   sprintf(buf,"#TYPE") ;
   for( ii=0 ; ii < nvec ; ii++ ){
      ll = strlen(buf) ;
      switch( mv->type[ii] ){
         case MV_FLOAT:  sprintf(buf+ll," FLOAT" ) ; break ;
         case MV_STRING: sprintf(buf+ll," STRING") ; break ;
      }
      width[ii] = 1 ;
   }
   fprintf(fp,"%s\n",buf) ;

   /*-- scan vectors to determine maximum column widths --*/

   for( kk=0 ; kk < ndim ; kk++ ){
      for( ii=0 ; ii < nvec ; ii++ ){
         switch( mv->type[ii] ){
            case MV_FLOAT:
               fpt = (float *) mv->vec[ii] ;
               MV_fval_to_char( fpt[kk] , fbuf ) ; ll = strlen(fbuf) ;
               width[ii] = MAX( width[ii] , ll ) ;
            break ;

            case MV_STRING:
               cpt = (char **) mv->vec[ii] ; ll = strlen(cpt[kk]) ;
               width[ii] = MAX( width[ii] , ll ) ;
            break ;
         }
      }
   }

   /*-- write data in columns --*/

   for( kk=0 ; kk < ndim ; kk++ ){
      buf[0] = '\0' ;
      for( ii=0 ; ii < nvec ; ii++ ){
         ll = strlen(buf) ;
         switch( mv->type[ii] ){
            case MV_FLOAT:
               fpt = (float *) mv->vec[ii] ;
               MV_fval_to_char( fpt[kk] , fbuf ) ;
               sprintf(buf+ll," %*s",width[ii],fbuf) ;
            break ;

            case MV_STRING:
               cpt = (char **) mv->vec[ii] ;
               sprintf(buf+ll," %*s",width[ii],cpt[kk]) ;
            break ;
         }
      }
      fprintf(fp,"%s\n",buf) ;
   }

   /*-- done --*/

   if( fp != stdout ) fclose(fp) ;
   return 1 ;
}

/*----------------------------------------------------------------
   Adapted from AV_fval_to_char
------------------------------------------------------------------*/

#define MV_NCOL 12

static void MV_fval_to_char( float qval , char * buf )
{
   float aval = fabs(qval) ;
   int lv ;
   char lbuf[32] ;
   int il ;

   /* special case if the value is an integer */

   if( qval == 0.0 ){ strcpy(buf,"0"); return; }

   lv = (fabs(qval) < 99999999.0) ? (int)qval : 100000001 ;

   if( qval == lv && abs(lv) < 100000000 ){
      sprintf( buf, "%d" , lv ) ; return ;
   }

/* macro to strip trailing zeros from output */

#undef  BSTRIP
#define BSTRIP for( il=strlen(lbuf)-1 ;                        \
                    il>1 && (lbuf[il]=='0' || lbuf[il]==' ') ; \
                    il-- ) lbuf[il] = '\0'

   /* noninteger: choose floating format based on magnitude */

   lv = (int) (10.0001 + log10(aval)) ;

   switch( lv ){

      default:
         if( qval > 0.0 ) sprintf( lbuf , "%-12.6e" , qval ) ;
         else             sprintf( lbuf , "%-12.5e" , qval ) ;
      break ;

      case  6:  /* 0.0001-0.001 */
      case  7:  /* 0.001 -0.01  */
      case  8:  /* 0.01  -0.1   */
      case  9:  /* 0.1   -1     */
      case 10:  /* 1     -9.99  */
         sprintf( lbuf , "%-9.6f" , qval ) ; BSTRIP ; break ;

      case 11:  /* 10-99.9 */
         sprintf( lbuf , "%-9.5f" , qval ) ; BSTRIP ; break ;

      case 12:  /* 100-999.9 */
         sprintf( lbuf , "%-9.4f" , qval ) ; BSTRIP ; break ;

      case 13:  /* 1000-9999.9 */
         sprintf( lbuf , "%-9.3f" , qval ) ; BSTRIP ; break ;

      case 14:  /* 10000-99999.9 */
         sprintf( lbuf , "%-9.2f" , qval ) ; BSTRIP ; break ;

      case 15:  /* 100000-999999.9 */
         sprintf( lbuf , "%-9.1f" , qval ) ; BSTRIP ; break ;

      case 16:  /* 1000000-9999999.9 */
         sprintf( lbuf , "%-9.0f" , qval ) ; break ;
   }

   strcpy(buf,lbuf) ; return ;
}

/*!
   \sa MV_format_fval2  ZSS May 28 04
*/
char * MV_format_fval( float fval )
{
   static char buf[32] ;
   MV_fval_to_char( fval , buf ) ;
   return buf ;
}

/*!
   \brief s = MV_format_fval2( fval, len);
   same as fval, but will attempt to keep
   the number len characters long. That's done
   by truncating digits to the right of the decimal 
   point, if one exists. 
   \sa MV_fval_to_char
   \sa MV_format_fval      ZSS, RickR May 28 04
*/
char * MV_format_fval2( float fval, int len)
{
   static char buf[32] ;
   int wid;
   char *pos = NULL;
   
   MV_fval_to_char( fval , buf ) ;
   if (len < 1) return (buf);
   if (strlen(buf) < len) return (buf);
   
   /* trim it down */
   pos = strchr (buf, '.');
   if (!pos) return(buf);  /* can't do no'in */
   wid = pos - buf;
   if (wid < len) buf[len] = '\0';
   if (buf[len-1] == '.') buf[len-1] = '\0'; /* remove trailing period */
   return buf ;

}
