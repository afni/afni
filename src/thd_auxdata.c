/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"

#if 0
#  define DQQ(s) fprintf(stderr,"THD_copy_datablock_auxdata %s\n",(s))
#else
#  define DQQ(s) /* nada */
#endif

void THD_copy_datablock_auxdata( THD_datablock * old_dblk, THD_datablock * new_dblk )
{
   int new_nvals , old_nvals , min_nvals , iv,kv , ibr ;

DQQ("entry") ;

   if( ! ISVALID_DATABLOCK(new_dblk) ) return ;

   new_nvals = new_dblk->nvals ;

   if( new_dblk->brick_lab != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myXtFree( new_dblk->brick_lab[ibr] ) ;
      myXtFree( new_dblk->brick_lab ) ;
   }

   if( new_dblk->brick_keywords != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myXtFree( new_dblk->brick_keywords[ibr] ) ;
      myXtFree( new_dblk->brick_keywords ) ;
   }

   if( new_dblk->brick_statcode != NULL )
      myXtFree( new_dblk->brick_statcode ) ;

   if( new_dblk->brick_stataux != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myXtFree( new_dblk->brick_stataux[ibr] ) ;
      myXtFree( new_dblk->brick_stataux ) ;
   }

   new_dblk->brick_lab      = NULL ;
   new_dblk->brick_keywords = NULL ;
   new_dblk->brick_statcode = NULL ;
   new_dblk->brick_stataux  = NULL ;

DQQ("finish nulling") ;

   if( ! ISVALID_DATABLOCK(old_dblk) ) return ;

   old_nvals = old_dblk->nvals ;
   min_nvals = (old_nvals < new_nvals) ? old_nvals : new_nvals ;

DQQ("starting copy") ;

   if( old_dblk->brick_lab != NULL ){
DQQ("copy labels") ;
      THD_init_datablock_labels( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ )
         THD_store_datablock_label( new_dblk , iv , old_dblk->brick_lab[iv] ) ;
   }

   if( old_dblk->brick_keywords != NULL ){
DQQ("copy keywords") ;
      THD_init_datablock_keywords( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ )
         THD_store_datablock_keywords( new_dblk , iv , old_dblk->brick_keywords[iv] ) ;
   }

   if( old_dblk->brick_statcode != NULL ){
DQQ("copy statcode and stataux") ;
      THD_init_datablock_stataux( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ ){
         kv = old_dblk->brick_statcode[iv] ;
         THD_store_datablock_stataux( new_dblk , iv , kv ,
                                      FUNC_need_stat_aux[kv] ,
                                      old_dblk->brick_stataux[iv] ) ;
      }
   }

DQQ("exit") ;
   return ;
}

/*----------------------------------------------------------------
  30 Nov 1997: Initialize the brick fields in a datablock.
------------------------------------------------------------------*/

void THD_init_datablock_labels( THD_datablock * dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_lab != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myXtFree( dblk->brick_lab[ibr] ) ;
      myXtFree( dblk->brick_lab ) ;
   }

   dblk->brick_lab = (char **) XtMalloc( sizeof(char *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_lab[ibr] = (char *) XtMalloc(sizeof(char)*8) ;
      sprintf( dblk->brick_lab[ibr] , "#%d" , ibr ) ;
   }

   return ;
}

void THD_store_datablock_label( THD_datablock * dblk , int iv , char * str )
{
   if( ! ISVALID_DATABLOCK(dblk) || iv < 0 || iv >= dblk->nvals  ) return ;

   if( dblk->brick_lab == NULL ) THD_init_datablock_labels( dblk ) ;

   myXtFree( dblk->brick_lab[iv] ) ;
   if( str != NULL && str[0] != '\0' ){
      dblk->brick_lab[iv] = XtNewString( str ) ;
   } else {
      dblk->brick_lab[iv] = (char *) XtMalloc(sizeof(char)*8) ;
      sprintf( dblk->brick_lab[iv] , "#%d" , iv ) ;
   }
   return ;
}

void THD_init_datablock_keywords( THD_datablock * dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_keywords != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myXtFree( dblk->brick_keywords[ibr] ) ;
      myXtFree( dblk->brick_keywords ) ;
   }

   dblk->brick_keywords = (char **) XtMalloc( sizeof(char *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_keywords[ibr]    = (char *) XtMalloc(sizeof(char)*4) ;
      dblk->brick_keywords[ibr][0] = '\0' ;
   }

   return ;
}

void THD_store_datablock_keywords( THD_datablock * dblk , int iv , char * str )
{
   if( ! ISVALID_DATABLOCK(dblk) || iv < 0 || iv >= dblk->nvals ) return ;

   if( dblk->brick_keywords == NULL ) THD_init_datablock_keywords( dblk ) ;

   myXtFree( dblk->brick_keywords[iv] ) ;
   if( str != NULL && str[0] != '\0' )
      dblk->brick_keywords[iv] = XtNewString( str ) ;
   return ;
}

void THD_append_datablock_keywords( THD_datablock * dblk , int iv , char * str )
{
   if( ! ISVALID_DATABLOCK(dblk)   ||
       iv < 0 || iv >= dblk->nvals || str == NULL ) return ;

   if( dblk->brick_keywords == NULL ) THD_init_datablock_keywords( dblk ) ;

   if( dblk->brick_keywords[iv] == NULL || dblk->brick_keywords[iv][0] == '\0' ){
      THD_store_datablock_keywords( dblk , iv , str ) ;
   } else if( str[0] != '\0' ){
      int ll = strlen(dblk->brick_keywords[iv]) + strlen(str) + 6 ;
      char * cc = XtMalloc( sizeof(char) * ll ) ;
      strcpy(cc,dblk->brick_keywords[iv]) ; strcat(cc," ; ") ; strcat(cc,str) ;
      myXtFree( dblk->brick_keywords[iv] ) ;
      dblk->brick_keywords[iv] = cc ;
   }
   return ;
}

void THD_init_datablock_stataux( THD_datablock * dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_statcode != NULL )
      myXtFree( dblk->brick_statcode ) ;

   if( dblk->brick_stataux != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myXtFree( dblk->brick_stataux[ibr] ) ;
      myXtFree( dblk->brick_stataux ) ;
   }

   /* initialize to emptinesss */

   dblk->brick_statcode = (int *)    XtMalloc( sizeof(int)     * nvals ) ;
   dblk->brick_stataux  = (float **) XtMalloc( sizeof(float *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_statcode[ibr] = 0 ;
      dblk->brick_stataux[ibr]  = NULL ;
   }

   return ;
}

void THD_store_datablock_stataux( THD_datablock * dblk ,
                                  int iv , int scode , int npar , float * par )
{
   int kv , jv ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;
   if( iv < 0 || iv >= dblk->nvals || npar < 0 ) return ;

   if( dblk->brick_statcode == NULL ) THD_init_datablock_stataux( dblk ) ;

   dblk->brick_statcode[iv] = scode ;  /* save statcode */

   if( ! FUNC_IS_STAT(scode) ) return ;  /* do nothing else */

   /* make space, then copy data into it */

   kv = FUNC_need_stat_aux[scode] ;    /* how many params we need */
   if( npar > kv ) npar = kv ;         /* how many params we have */

   myXtFree( dblk->brick_stataux[iv] ) ;  /* if any old stuff, toss it */

   if( kv > 0 ){
      dblk->brick_stataux[iv] = (float *) XtMalloc( sizeof(float) * kv ) ;
      for( jv=0 ; jv < npar ; jv++ )
         dblk->brick_stataux[iv][jv] = par[jv] ; /* copy in */
      for( ; jv < kv ; jv++ )
         dblk->brick_stataux[iv][jv] = 0.0 ;     /* 0 fill */
   }

   return ;
}

int THD_string_has( char * strbig , char * strlit )
{
   if( strbig == NULL || strbig[0] == '\0' ||
       strlit == NULL || strlit[0] == '\0'   ) return 0 ;

   return ( strstr(strbig,strlit) != NULL ) ;
}

void THD_store_dataset_keywords( THD_3dim_dataset * dset , char * str )
{
   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   myXtFree( dset->keywords ) ;
   if( str != NULL && str[0] != '\0' )
      dset->keywords = XtNewString( str ) ;
   return ;
}

void THD_append_dataset_keywords( THD_3dim_dataset * dset , char * str )
{
   if( ! ISVALID_3DIM_DATASET(dset) || str == NULL ) return ;

   if( dset->keywords == NULL || dset->keywords[0] == '\0' ){
      THD_store_dataset_keywords( dset , str ) ;
   } else if( str[0] != '\0' ){
      int ll = strlen(dset->keywords) + strlen(str) + 6 ;
      char * cc = XtMalloc( sizeof(char) * ll ) ;
      strcpy(cc,dset->keywords) ; strcat(cc," ; ") ; strcat(cc,str) ;
      myXtFree( dset->keywords ) ;
      dset->keywords = cc ;
   }
   return ;
}
