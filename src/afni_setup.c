#include "afni.h"

#define PAL_FIGNORE 9753.1
#define PAL_IIGNORE -1
#define MAX_PALABEL 16

#ifndef MCW_strncpy
#define MCW_strncpy(dest,src,n) \
   ( (void) strncpy( (dest) , (src) , (n)-1 ) , (dest)[(n)-1] = '\0' )
#endif

/*-------------------------------------------------------------------------*/

typedef struct {
   int npane , mode ;
   float val[NPANE_MAX+1] ;
   int   col[NPANE_MAX] ;
} PBAR_palette ;

typedef struct {
   char label[32] ;
   PBAR_palette * psgn[NPANE_MAX+1] ;
   PBAR_palette * ppos[NPANE_MAX+1] ;
} PBAR_palette_array ;

#define INIT_PALARR(name)                          \
 do{ int qp ; (name) = XtNew(PBAR_palette_array) ; \
     (name)->label[0] = '\0' ;                     \
     for( qp=0 ; qp <= NPANE_MAX ; qp++ )          \
        (name)->psgn[qp] = (name)->ppos[qp] = NULL ; } while(0)

typedef struct {
   int num , nall ;
   PBAR_palette_array ** par ;
} PBAR_palette_table ;

#define INC_PALTAB 8

#define INIT_PALTAB(name)                 \
   ( (name) = XtNew(PBAR_palette_table) , \
     (name)->num = (name)->nall = 0 ,     \
     (name)->par = NULL )

#define ADDTO_PALTAB(name,pp)                                       \
 do { if( (name)->num == (name)->nall ){                             \
       (name)->nall += INC_PALTAB ;                                   \
       (name)->par   = (PBAR_palette_array **)                         \
                        XtRealloc( (char *) (name)->par ,               \
                         sizeof(PBAR_palette_array *) * (name)->nall ) ; \
      }                                                                 \
      if( (pp) != NULL ){                                              \
       (name)->par[(name)->num] = (PBAR_palette_array *) (pp) ;       \
       ((name)->num)++ ;                                             \
      } } while(0)

#define PALTAB_NUM(name)            ( (name)->num )
#define PALTAB_ARR(name,qq)         ( (name)->par[(qq)] )
#define PALTAB_ARR_LABEL(name,qq)   ( (name)->par[(qq)]->label )
#define PALTAB_ARR_PSGN(name,qq,ww) ( (name)->par[(qq)]->psgn[(ww)] )
#define PALTAB_ARR_PPOS(name,qq,ww) ( (name)->par[(qq)]->ppos[(ww)] )

/*-------------------------------------------------------------------------*/

int label_in_PALTAB( PBAR_palette_table * pt , char * lab )
{
   int ii ;
   if( pt == NULL || PALTAB_NUM(pt) == 0 || lab == NULL || lab[0] == '\0' )
      return -1 ;

   for( ii=0 ; ii < PALTAB_NUM(pt) ; ii++ )
      if( strcmp( PALTAB_ARR_LABEL(pt,ii) , lab ) == 0 ) return ii ;

   return -1 ;
}

/*-------------------------------------------------------------------------*/

PBAR_palette * read_PBAR_palette( char * fp , MCW_DC * dc , int * nused )
{
   PBAR_palette * pal ;
   int cnext , ii , pp , nu ;
   char str[32] ;
   char * cpt ;
   float vvv ;

   *nused = 0 ;
   if( fp == NULL || fp[0] == '\0' ) return NULL ;

   pal = (PBAR_palette *) malloc( sizeof(PBAR_palette) ) ;

   /** get first string **/

   nu=0 ; ii = sscanf( fp , "%31s%n" , str,&nu ); *nused+=nu;fp+=nu;
   if( ii == 0 ){ free(pal) ; return NULL ; }

   /** at this point, str should be of the form [npane] or [npane+] **/

   if( strstr(str,"+]") == NULL ){
      ii = sscanf( str , "[%d]" , &(pal->npane) ) ;
      if( ii == 0 ){ free(pal) ; return NULL ; }
      pal->mode = 0 ;
   } else {
      ii = sscanf( str , "[%d+]" , &(pal->npane) ) ;
      if( ii == 0 ){ free(pal) ; return NULL ; }
      pal->mode = 1 ;
   }
   if( pal->npane < NPANE_MIN || pal->npane > NPANE_MAX ){ free(pal) ; return NULL ; }

   /** initialize fields **/

   for( pp=0 ; pp < NPANE_MAX ; pp++ ){
      pal->val[pp] = PAL_FIGNORE ;
      pal->col[pp] = PAL_IIGNORE ;
   }
   pal->val[NPANE_MAX] = PAL_FIGNORE ;

   /** scan for fields: value color value color value ... **/

   for( pp=0 ; pp < pal->npane ; pp++ ){

      nu=0 ; ii = sscanf( fp , "%s%n" , str,&nu ); *nused+=nu;fp+=nu; /* next string */
      if( ii == 0 || str[0] == '[' ){ free(pal) ; return NULL ; }

      ii = sscanf( str , "%f" , &vvv ) ;          /* try to make a value */
      if( ii == 1 ) pal->val[pp] = vvv ;

      nu=0 ; ii = sscanf( fp , "%s%n" , str,&nu ); *nused+=nu;fp+=nu; /* next string */
      if( ii == 0 || str[0] == '[' ){ free(pal) ; return NULL ; }

      ii = DC_find_overlay_color( dc , str ) ;    /* try to find a color */
      if( ii >= 0 ) pal->col[pp] = ii ;
   }

   /*** it worked !!! ***/

   return pal ;
}

/*------------------------------------------------------------------------
   Read an entire file into a character string.  When you are
   done with the returned string, free it.  If the string pointer
   is returned as NULL, something bad happened.
--------------------------------------------------------------------------*/

char * suck_file( char * fname )
{
   int len , fd , ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return NULL ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return NULL ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;

   if( ii <= 0 ){ free(buf) ; return NULL; }
   buf[ii+1] = '\0' ;
   return buf ;
}

/*-----------------------------------------------------------------------
   Process an AFNI setup file.
-------------------------------------------------------------------------*/

#define GETSTR                                                              \
  do{ int nu=0,qq ; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu;  \
      if( qq==0 || nu==0 ){ free(fbuf) ; return ; }                         \
    } while(0)

void AFNI_process_setup( char * fname )
{
   int    nbuf , nused , ii  ;
   char * fbuf , * fptr ;
   char str[128] ;

   fbuf = suck_file( fname ) ; if( fbuf == NULL ) return ;
   nbuf = strlen(fbuf) ;       if( nbuf == 0    ) return ;

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/

   GETSTR ;

   while( nused < nbuf ){

      if( strstr(str,"***") != str ){
         fprintf(stderr,"\nExpected '***' in setup file %s starting here:\n%.64s\n",
                        fname , str ) ;
         free(fbuf) ; return ;
      }

      /** COLORS section **/

      if( strcmp(str,"***COLORS") == 0 ){
         char label[128] , defn[128] ;

         while(1){
            GETSTR ; if( strstr(str,"***") == str ) break ;  /* skip ahead */
            strcpy(label,str) ;

            GETSTR ;
            if( str[0] != '=' ){
               fprintf(stderr,"\nExpected '=' in setup file %s starting here:\n%.64s\n",
                              fname , str ) ;
               free(fbuf) ; return ;
            }

            GETSTR ; if( strstr(str,"***") == str ) break ;  /* skip ahead */
            strcpy(defn,str) ;

            /** must do something with label and defn now **/

         }
         continue ;
      } /* end of COLORS */

      /** PALETTES section **/

      if( strcmp(str,"***PALETTES") == 0 ){

         continue ;
      } /* end of PALETTES */

      /** END **/

      if( strcmp(str,"***END") == 0 ) break ;

      /** unknown section **/

      fprintf(stderr,"\nIn setup file %s, unknown section: %s\n",
                     fname , str ) ;
      break ;

   }  /* end of while loop */

   free(fbuf) ; return ;

}
