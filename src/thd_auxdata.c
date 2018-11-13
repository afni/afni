/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------------*/

void THD_copy_datablock_auxdata( THD_datablock *old_dblk, THD_datablock *new_dblk )
{
   int new_nvals , old_nvals , min_nvals , iv,kv , ibr ;

ENTRY("THD_copy_datablock_auxdata") ;

   if( ! ISVALID_DATABLOCK(new_dblk) ) EXRETURN ;

   new_nvals = new_dblk->nvals ;

   if( new_dblk->brick_lab != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myRwcFree( new_dblk->brick_lab[ibr] ) ;
      myRwcFree( new_dblk->brick_lab ) ;
   }

   if( new_dblk->brick_keywords != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myRwcFree( new_dblk->brick_keywords[ibr] ) ;
      myRwcFree( new_dblk->brick_keywords ) ;
   }

   if( new_dblk->brick_statcode != NULL )
      myRwcFree( new_dblk->brick_statcode ) ;

   if( new_dblk->brick_stataux != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ ) myRwcFree( new_dblk->brick_stataux[ibr] ) ;
      myRwcFree( new_dblk->brick_stataux ) ;
   }

   new_dblk->brick_lab      = NULL ;
   new_dblk->brick_keywords = NULL ;
   new_dblk->brick_statcode = NULL ;
   new_dblk->brick_stataux  = NULL ;

STATUS("finish nulling") ;

   if( ! ISVALID_DATABLOCK(old_dblk) ) EXRETURN ;

   old_nvals = old_dblk->nvals ;
   min_nvals = (old_nvals < new_nvals) ? old_nvals : new_nvals ;

STATUS("starting copy") ;

   if( old_dblk->brick_lab != NULL ){
STATUS("copy labels") ;
      THD_init_datablock_labels( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ )
         THD_store_datablock_label( new_dblk , iv , old_dblk->brick_lab[iv] ) ;
   }

   if( old_dblk->brick_keywords != NULL ){
STATUS("copy keywords") ;
      THD_init_datablock_keywords( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ )
         THD_store_datablock_keywords( new_dblk , iv , old_dblk->brick_keywords[iv] ) ;
   }

   if( old_dblk->brick_statcode != NULL ){
STATUS("copy statcode and stataux") ;
      THD_init_datablock_stataux( new_dblk ) ;
      for( iv=0 ; iv < min_nvals ; iv++ ){
         kv = old_dblk->brick_statcode[iv] ;
         THD_store_datablock_stataux( new_dblk , iv , kv ,
                                      FUNC_need_stat_aux[kv] ,
                                      old_dblk->brick_stataux[iv] ) ;
      }
   }

   /* we may need to copy the node_list        12 Jul 2006 [rickr] */
   if( (DBLK_IS_NI_SURF_DSET(old_dblk) || DBLK_IS_GIFTI(old_dblk)) &&
        old_dblk->nnodes > 0 && old_dblk->node_list ){
STATUS("copy surface node_list") ;
      iv = old_dblk->nnodes * sizeof(int) ;
      new_dblk->node_list = (int *)RwcMalloc(iv) ;
      if( new_dblk->node_list ){
         new_dblk->nnodes = old_dblk->nnodes ;
         memcpy( new_dblk->node_list, old_dblk->node_list, iv ) ;
      }
   }

   #if 0               /* Overkill, you don't want 3dcalc preserving. 
                          such things for example ...              */
   /* preserve label tables                        ZSS: Jan 2012 */
   STATUS("copying labeltables and Atlas PointLists, if any");
   if (!THD_copy_labeltable_atr( new_dblk,  old_dblk)) {
      WARNING_message("Failed trying to preserve labeltables");
   } 
   #endif                                                    

   EXRETURN ;
}

/*----------------------------------------------------------------
  30 Nov 1997: Initialize the brick fields in a datablock.
------------------------------------------------------------------*/

void THD_init_datablock_labels( THD_datablock *dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_lab != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myRwcFree( dblk->brick_lab[ibr] ) ;
      myRwcFree( dblk->brick_lab ) ;
   }

   dblk->brick_lab = (char **) RwcMalloc( sizeof(char *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_lab[ibr] = (char *) RwcMalloc(sizeof(char)*8) ;
      sprintf( dblk->brick_lab[ibr] , "#%d" , ibr ) ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

void THD_store_datablock_label( THD_datablock *dblk , int iv , char *str )
{
   char *sss ;  /* 02 Sep 2004 */

   if( ! ISVALID_DATABLOCK(dblk) || iv < 0 || iv >= dblk->nvals  ) return ;

   if( dblk->brick_lab == NULL ) THD_init_datablock_labels( dblk ) ;

   myRwcFree( dblk->brick_lab[iv] ) ;
   if( str != NULL && str[0] != '\0' ){
      sss = strdup(str) ;
      /* mod 10/27/2011 - drg */
      if( strlen(sss) >= THD_MAX_SBLABEL ) sss[THD_MAX_SBLABEL-1] = '\0' ;
      dblk->brick_lab[iv] = RwcNewString( sss ) ;
      free((void *)sss) ;
   } else {
      dblk->brick_lab[iv] = (char *) RwcMalloc(sizeof(char)*8) ;
      sprintf( dblk->brick_lab[iv] , "#%d" , iv ) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_init_datablock_keywords( THD_datablock *dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_keywords != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myRwcFree( dblk->brick_keywords[ibr] ) ;
      myRwcFree( dblk->brick_keywords ) ;
   }

   dblk->brick_keywords = (char **) RwcMalloc( sizeof(char *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_keywords[ibr]    = (char *) RwcMalloc(sizeof(char)*4) ;
      dblk->brick_keywords[ibr][0] = '\0' ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

void THD_store_datablock_keywords( THD_datablock *dblk, int iv, char *str )
{
   if( ! ISVALID_DATABLOCK(dblk) || iv < 0 || iv >= dblk->nvals ) return ;

   if( dblk->brick_keywords == NULL ) THD_init_datablock_keywords( dblk ) ;

   myRwcFree( dblk->brick_keywords[iv] ) ;
   if( str != NULL && str[0] != '\0' )
      dblk->brick_keywords[iv] = RwcNewString( str ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_append_datablock_keywords( THD_datablock *dblk, int iv , char *str )
{
   if( ! ISVALID_DATABLOCK(dblk)   ||
       iv < 0 || iv >= dblk->nvals || str == NULL ) return ;

   if( dblk->brick_keywords == NULL ) THD_init_datablock_keywords( dblk ) ;

   if( dblk->brick_keywords[iv] == NULL || dblk->brick_keywords[iv][0] == '\0' ){
      THD_store_datablock_keywords( dblk , iv , str ) ;
   } else if( str[0] != '\0' ){
      int ll = strlen(dblk->brick_keywords[iv]) + strlen(str) + 6 ;
      char *cc = (char*)RwcMalloc( sizeof(char) * ll ) ;
      strcpy(cc,dblk->brick_keywords[iv]) ; strcat(cc," ; ") ; strcat(cc,str) ;
      myRwcFree( dblk->brick_keywords[iv] ) ;
      dblk->brick_keywords[iv] = cc ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_init_datablock_stataux( THD_datablock *dblk )
{
   int ibr , nvals ;

   if( ! ISVALID_DATABLOCK(dblk) ) return ;

   nvals = dblk->nvals ;

   if( dblk->brick_statcode != NULL )
      myRwcFree( dblk->brick_statcode ) ;

   if( dblk->brick_stataux != NULL ){
      for( ibr=0 ; ibr < nvals ; ibr++ ) myRwcFree( dblk->brick_stataux[ibr] ) ;
      myRwcFree( dblk->brick_stataux ) ;
   }

   /* initialize to emptinesss */

   dblk->brick_statcode = (int *)    RwcMalloc( sizeof(int)     * nvals ) ;
   dblk->brick_stataux  = (float **) RwcMalloc( sizeof(float *) * nvals ) ;
   for( ibr=0 ; ibr < nvals ; ibr++ ){
      dblk->brick_statcode[ibr] = 0 ;
      dblk->brick_stataux[ibr]  = NULL ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

void THD_store_datablock_stataux( THD_datablock *dblk ,
                                  int iv , int scode , int npar , float *par )
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

   myRwcFree( dblk->brick_stataux[iv] ) ;  /* if any old stuff, toss it */

   if( kv > 0 ){
     dblk->brick_stataux[iv] = (float *) RwcMalloc( sizeof(float) * kv ) ;
     for( jv=0 ; jv < npar ; jv++ )
       dblk->brick_stataux[iv][jv] = par[jv] ; /* copy in */
     for( ; jv < kv ; jv++ )
       dblk->brick_stataux[iv][jv] = 0.0 ;     /* 0 fill */
   }

   return ;
}

/*---------------------------------------------------------------------------*/

int THD_string_has( char *strbig , char *strlit )
{
   if( strbig == NULL || strbig[0] == '\0' ||
       strlit == NULL || strlit[0] == '\0'   ) return 0 ;

   return ( strstr(strbig,strlit) != NULL ) ;
}

/*---------------------------------------------------------------------------*/

void THD_store_dataset_keywords( THD_3dim_dataset *dset , char *str )
{
   if( ! ISVALID_3DIM_DATASET(dset) ) return ;

   myRwcFree( dset->keywords ) ;
   if( str != NULL && str[0] != '\0' )
      dset->keywords = RwcNewString( str ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_append_dataset_keywords( THD_3dim_dataset *dset , char *str )
{
   if( ! ISVALID_3DIM_DATASET(dset) || str == NULL ) return ;

   if( dset->keywords == NULL || dset->keywords[0] == '\0' ){
      THD_store_dataset_keywords( dset , str ) ;
   } else if( str[0] != '\0' ){
      int ll = strlen(dset->keywords) + strlen(str) + 6 ;
      char *cc = (char*)RwcMalloc( sizeof(char) * ll ) ;
      strcpy(cc,dset->keywords) ; strcat(cc," ; ") ; strcat(cc,str) ;
      myRwcFree( dset->keywords ) ;
      dset->keywords = cc ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

void THD_patch_dxyz_one( THD_3dim_dataset *dset , int iv )
{
   float dx,dy,dz ;
   MRI_IMAGE *qm ;

   dx = fabsf(DSET_DX(dset)) ;
   dy = fabsf(DSET_DY(dset)) ;
   dz = fabsf(DSET_DZ(dset)) ;
   qm = DSET_BRICK(dset,iv) ;
   qm->dx = dx ; qm->dy = dy ; qm->dz = dz ;

   return ;
}

void THD_patch_dxyz_all( THD_3dim_dataset *dset )
{
   int ii , nvals ;
   if( !ISVALID_DSET(dset) ) return ;
   nvals = DSET_NVALS(dset) ;
   for( ii=0 ; ii < nvals ; ii++ ) THD_patch_dxyz_one(dset,ii) ;
   return ;
}
