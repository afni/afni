
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef unsigned char           byte ;
typedef struct { byte r,g,b ; } rgb ;
typedef struct { float r,i ; }  complex ;

#define SERTYPE_NUM 8
typedef enum {
   SERTYPE_byte  , SERTYPE_short  , SERTYPE_int  ,
   SERTYPE_float , SERTYPE_double , SERTYPE_complex ,
   SERTYPE_rgb   , SERTYPE_string
} SERTYPE ;

static int SERTYPE_code[SERTYPE_NUM] = {
   SERTYPE_byte  , SERTYPE_short  , SERTYPE_int  ,
   SERTYPE_float , SERTYPE_double , SERTYPE_complex ,
   SERTYPE_rgb   , SERTYPE_string
} ;

static int SERTYPE_initialized = 0 ;

static int SERTYPE_sizeof[SERTYPE_NUM] ;

static const char *SERTYPE_name[SERTYPE_NUM] = {
   "byte"      , "short"      , "int"       ,
   "float"     , "double"     , "complex"   ,
   "rgb"       , "String"
} ;

static const char *SERTYPE_initial[SERTYPE_NUM] = {
   "b" , "s" , "i" , "f" , "d" , "c" , "r" , "S"
} ;

#define SER_MAXNAME 256
typedef struct {
  char fieldname[SER_MAXNAME] ;
  int numvec , veclen , vecall ;
  int *vtype ;
  void **vec ;
} SER_vector ;

/*--------------------------------------------------------------------*/

static void SER_setup_stuff(void)
{
   SERTYPE_sizeof[0] = sizeof(byte) ;
   SERTYPE_sizeof[1] = sizeof(short) ;
   SERTYPE_sizeof[2] = sizeof(int) ;
   SERTYPE_sizeof[3] = sizeof(float) ;
   SERTYPE_sizeof[4] = sizeof(double) ;
   SERTYPE_sizeof[5] = sizeof(complex) ;
   SERTYPE_sizeof[6] = sizeof(rgb) ;
   SERTYPE_sizeof[7] = sizeof(char *) ;

   SERTYPE_initialized = 1 ;
}

/*--------------------------------------------------------------------*/

SER_vector * SER_new_vector( char *fname , char *typelist )
{
   SER_vector *sv ;
   int ntyp , ii , ntl , tpos , jj , *vtyp=NULL ;
   char *tlist , tname[SER_MAXNAME] ;

   if( typelist == NULL ) return NULL ;    /* bad */

   if( !SERTYPE_initialized ) SER_setup_stuff() ;

   sv = AFMALL( SER_vector, sizeof(SER_vector)) ;

   /*-- set fieldname --*/

   if( fname == NULL ){                    /* not good, but OK */
     sv->fieldname[0] = '\0' ;
   } else {
     strncpy(sv->fieldname,fname,SER_MAXNAME) ;
     sv->fieldname[SER_MAXNAME-1] = '\0' ;
   }

   /*-- copy and mangle type list --*/

   tlist = strdup(typelist) ;
   ntl = strlen(tlist) ;                       /* replace separators */
   for( ii=0 ; ii < ntl ; ii++ )               /* with blanks */
      if( tlist[ii] == ',' || tlist[ii] == ':' ||
          tlist[ii] == ';' || tlist[ii] == '+' ||
          tlist[ii] == '-' || tlist[ii] == '/'   ) tlist[ii] = ' ' ;

   /*-- scan mangled type list and get type codes --*/

   tpos = ntyp = 0 ;
   vtyp = (int *) malloc(sizeof(int)) ;

   do{
      ii = sscanf( tlist+tpos , "%s%n" , tname , &jj ) ;    /* next name */
      if( ii < 1 ) break ;                                /* end of work */
      tpos += jj ;                            /* char after last scanned */

      ii = strlen(tname) ;                     /* find name in type list */
      for( jj=0 ; jj < SERTYPE_NUM ; jj++ )
         if( strncmp(SERTYPE_name[jj],tname,ii) == 0 ) break ;

      if( jj == SERTYPE_NUM ){ free(tlist); free(vtyp); return NULL; }

      ntyp++ ;                                           /* add new type */
      vtyp = (int *) realloc( vtyp , sizeof(int)*ntyp ) ;
      vtyp[ntyp-1] = SERTYPE_code[jj] ;
   } while( tpos < ntl ) ;

   free(tlist) ;                              /* don't need copy no more */

   if( ntyp < 1 ){ free(vtyp); return NULL; }            /* got no types */

   /*-- now can setup rest of vector --*/

   sv->numvec = ntyp ;
   sv->veclen = 0 ;
   sv->vecall = 0 ;
   sv->vtype  = vtyp ;
   sv->vec    = (void **) malloc( sizeof(void *)*ntyp ) ;
   for( ii=0 ; ii < ntyp ; ii++ ) sv->vec[ii] = NULL ;

   return sv ;
}

/*--------------------------------------------------------------------*/

void SER_destroy_vector( SER_vector *sv )
{
   int ii , jj ;

   if( sv == NULL ) return ;

   if( sv->vec != NULL ){                    /* destroy each sub-vector */
     for( ii=0 ; ii < sv->numvec ; ii++ ){
        if( sv->vec[ii] != NULL ){
           if( sv->vtype[ii] == SERTYPE_string ){       /* if strings,  */
              char **SSp = (char **) sv->vec[ii] ;      /* must destroy */
              for( jj=0 ; jj < sv->numvec ; jj++ )      /* each string  */
                 if( SSp[jj] != NULL ) free(SSp[jj]) ;
           }
           free(sv->vec[ii]) ;
        }
     }
     free(sv->vec) ;
   }

   if( sv->vtype != NULL ) free(sv->vtype) ;
   free(sv) ;
}

/*--------------------------------------------------------------------*/

void SER_free_vector( SER_vector *sv )
{
   int ii ;
   if( sv == NULL ) return ;
   if( sv->vtype != NULL ) free(sv->vtype) ;
   if( sv->vec != NULL ) free(sv->vec) ;
   free(sv) ;
}

/*--------------------------------------------------------------------*/

int SER_addto_vector_textmode( SER_vector *sv , int nstr , char *str )
{
   int jj , err=0 , spos=0 , snum , ss ;
   void **val ;
   byte         *bbp ;
   short        *ssp ;
   int     ii , *iip ;
   float   ff , *ffp ;
   double  dd , *ddp ;
   complex cc , *ccp ;
   rgb          *rrp ; int rr,gg,bb ;
   char   *SS ,**SSp ;

   if( sv  == NULL ||
       str == NULL || str[0] == '\0' || nstr < sv->numvec ) return 0 ;

   /*-- loop over sub-vectors --*/

   val = (void **) calloc( sizeof(void *) , sv->numvec ) ;

   for( jj=0 ; jj < sv->numvec && spos < nstr ; jj++ ){

      switch( sv->vtype[jj] ){

         case SERTYPE_byte:
            ss = sscanf(str+spos,"%d%n",&ii,&snum) ;
            if( ss < 1 ){ err++ ; break ; }
            spos += snum ;
            bbp = (byte *) malloc( sizeof(byte) ) ; *bbp = (byte) ii ;
            val[jj] = (void *) bbp ;
         break ;

         case SERTYPE_short:
            ss = sscanf(str+spos,"%d%n",&ii,&snum) ;
            if( ss < 1 ){ err++ ; break ; }
            spos += snum ;
            ssp = (short *) malloc( sizeof(short) ) ; *ssp = (short) ii ;
            val[jj] = (void *) ssp ;
         break ;

         case SERTYPE_int:
            ss = sscanf(str+spos,"%d%n",&ii,&snum) ;
            if( ss < 1 ){ err++ ; break ; }
            spos += snum ;
            iip = (int *) malloc( sizeof(int) ) ; *iip = ii ;
            val[jj] = (void *) iip ;
         break ;

         case SERTYPE_float:
            ss = sscanf(str+spos,"%f%n",&ff,&snum) ;
            if( ss < 1 ){ err++ ; break ; }
            spos += snum ;
            ffp = (float *) malloc( sizeof(float) ) ; *ffp = ff ;
            val[jj] = (void *) ffp ;
         break ;

         case SERTYPE_double:
            ss = sscanf(str+spos,"%lf%n",&dd,&snum) ;
            if( ss < 1 ){ err++ ; break ; }
            spos += snum ;
            ddp = (double *) malloc( sizeof(double) ) ; *ddp = dd ;
            val[jj] = (void *) ddp ;
         break ;

         case SERTYPE_complex:
            ss = sscanf(str+spos,"%f%f%n",&(cc.r),&(cc.i),&snum) ;
            if( ss < 2 ){ err++ ; break ; }
            spos += snum ;
            ccp = (complex *) malloc( sizeof(complex) ) ; *ccp = cc ;
            val[jj] = (void *) ccp ;
         break ;

         case SERTYPE_rgb:
            ss = sscanf(str+spos,"%d%d%d%n",&rr,&gg,&bb,&snum) ;
            if( ss < 2 ){ err++ ; break ; }
            spos += snum ;
            rrp = (rgb *) malloc( sizeof(rgb) ) ;
            rrp->r = (byte)rr ; rrp->g = (byte)gg ; rrp->b = (byte)bb ;
            val[jj] = (void *) rrp ;
         break ;

#define QUOTE '"'  /* string quoting character */

         case SERTYPE_string:{
            int quote=0 , kk,mm , ll ;
            for( kk=spos ; kk < nstr && iswhite(str[kk]) ; kk++ ) ; /* nada */
            if( kk == nstr ){ err++ ; break ; }
            if( str[kk] == QUOTE ){ quote=1; kk++; if(kk==nstr){err++;break;} }
            if( quote ){
               mm = kk ;
               while( mm < nstr ){
                  if( str[mm] == QUOTE && str[mm-1] != '\\' ) break ;
                  mm++ ;
               }
            } else {
               for( mm=kk ; mm < nstr && !iswhite(str[kk]) ; mm++ ) ; /* nada */
            }
            /* string runs from str[kk] to str[mm-1] */
            spos = mm+1 ;
            ll   = mm-kk ;
         }
         break ;

      }
   }

   if( jj < sv->numvec || err > 0 ){
      for( jj=0 ; jj < sv->numvec ; jj++ )
         if( val[jj] != NULL ) free(val[jj]) ;
      free(val) ; return 0 ;
   }
}
