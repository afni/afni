
/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/* prototypes */

static int * get_intlist( int , char * ) ;
static void THD_setup_mastery( THD_3dim_dataset * , int * ) ;

/*-----------------------------------------------------------------
   11 Jan 1999: Open a dataset, allowing for possible mastering.
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_dataset( char * pathname )
{
   THD_3dim_dataset * dset ;
   char dname[THD_MAX_NAME] , subv[THD_MAX_NAME] ;
   char * cpt ;
   int  * ivlist ;
   int    ii ;

   /*-- sanity check --*/

   if( pathname == NULL            ||
       (ii=strlen(pathname)) == 0  ||
       pathname[ii-1]        == '/'  ) return NULL ;

   /*-- find the opening "[" --*/

   cpt = strstr(pathname,"[") ;

   if( cpt == NULL ){                            /* no "["   */
      dset = THD_open_one_dataset( pathname ) ;  /* ==> open */
      return dset ;                              /* normally */
   }

   if( cpt == pathname ) return NULL ;  /* error */

   /* copy dataset filename to dname and selector string to subv */

   ii = cpt - pathname ;
   memcpy(dname,pathname,ii) ; dname[ii] = '\0' ;
   strcpy(subv,cpt) ;

   /* open the dataset */

   dset = THD_open_one_dataset( dname ) ;
   if( dset == NULL ) return NULL ;

   /* parse the selector string */

   ivlist = get_intlist( DSET_NVALS(dset) , subv ) ;
   if( ivlist == NULL ) return dset ;

   /* modify the dataset according to the selector string */

   THD_setup_mastery( dset , ivlist ) ;
   free(ivlist) ;
   return dset ;
}

/*-----------------------------------------------------------------
   Set up a dataset for being mastered; that is, reading only
   a subset of sub-bricks from the master .BRIK file.
-------------------------------------------------------------------*/

static void THD_setup_mastery( THD_3dim_dataset * dset , int * ivlist )
{
   int ibr , old_nvals , new_nvals ;
   THD_datablock * dblk ;
   int * btype , * ivl ;

   float *  old_brick_fac  ;
   int *    old_brick_bytes ;
   char **  old_brick_lab  ;
   char **  old_brick_keywords ;
   int *    old_brick_statcode ;
   float ** old_brick_stataux ;

   /** sanity checks **/

   if( ! ISVALID_DSET(dset) || ivlist == NULL || ivlist[0] <= 0 ) return ;

   new_nvals = ivlist[0] ;
   ivl       = ivlist + 1 ;
   dblk      = dset->dblk ;
   old_nvals = dblk->nvals ;

   ibr = THD_count_databricks(dblk) ; if( ibr > 0 ) return ;

   for( ibr=0 ; ibr < new_nvals ; ibr++ )
      if( ivl[ibr] < 0 || ivl[ibr] >= old_nvals ) return ;

   /** save pointers to old datablock stuff **/

   old_brick_fac      = dblk->brick_fac      ; dblk->brick_fac      = NULL ;
   old_brick_bytes    = dblk->brick_bytes    ; dblk->brick_bytes    = NULL ;
   old_brick_lab      = dblk->brick_lab      ; dblk->brick_lab      = NULL ;
   old_brick_keywords = dblk->brick_keywords ; dblk->brick_keywords = NULL ;
   old_brick_statcode = dblk->brick_statcode ; dblk->brick_statcode = NULL ;
   old_brick_stataux  = dblk->brick_stataux  ; dblk->brick_stataux  = NULL ;

   /** setup new dataset brick structure **/

   dblk->diskptr->nvals = dblk->nvals = new_nvals ;
   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   if( dset->taxis != NULL ){                /* must fix time axis */
      if( new_nvals == 1 ){                  /* no time dependence */
         myXtFree( dset->taxis->toff_sl ) ;
         myXtFree( dset->taxis ) ;
      } else {                               /* different number of times */
         dset->taxis->ntt = new_nvals ;
      }
   }

   /* redo brick_fac */

   dblk->brick_fac = (float *) XtMalloc( sizeof(float) * new_nvals ) ;
   for( ibr=0 ; ibr < new_nvals ; ibr++ )
      dblk->brick_fac[ibr] = old_brick_fac[ivl[ibr]] ;

   /* redo brick and brick_bytes */

   btype = (int *) malloc( sizeof(int) * new_nvals ) ;
   for( ibr=0 ; ibr < new_nvals ; ibr++ )
      btype[ibr] = DBLK_BRICK_TYPE(dblk,ivl[ibr]) ;
   THD_init_datablock_brick( dblk , new_nvals , btype ) ;
   free(btype) ;

   /* redo brick_lab */

   if( old_brick_lab != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ )
         THD_store_datablock_label( dblk , ibr , old_brick_lab[ivl[ibr]] ) ;
   }

   /* redo brick_keywords */

   if( old_brick_keywords != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ )
         THD_store_datablock_keywords( dblk , ibr , old_brick_keywords[ivl[ibr]] ) ;
   }

   /* redo brick_statcode and brick_stataux */

   if( old_brick_statcode != NULL ){
      for( ibr=0 ; ibr < new_nvals ; ibr++ )
         THD_store_datablock_stataux( dblk, ibr, old_brick_statcode[ivl[ibr]] ,
                                           999 , old_brick_stataux [ivl[ibr]]  ) ;
   }

   /** setup master stuff now **/

   dblk->master_nvals = old_nvals ;
   dblk->master_bytes = old_brick_bytes ;
   dblk->master_ival  = (int *) XtMalloc( sizeof(int) * new_nvals ) ;
   for( ibr=0 ; ibr < new_nvals ; ibr++ ) dblk->master_ival[ibr] = ivl[ibr] ;

   /** destroy old datablock stuff now **/

   myXtFree( old_brick_fac ) ;

   if( old_brick_lab != NULL ){
      for( ibr=0 ; ibr < old_nvals ; ibr++ ) myXtFree( old_brick_lab[ibr] ) ;
      myXtFree( old_brick_lab ) ;
   }

   if( old_brick_keywords != NULL ){
      for( ibr=0 ; ibr < old_nvals ; ibr++ ) myXtFree( old_brick_keywords[ibr] ) ;
      myXtFree( old_brick_keywords ) ;
   }

   if( old_brick_statcode != NULL ) myXtFree( old_brick_statcode ) ;
   if( old_brick_stataux  != NULL ){
      for( ibr=0 ; ibr < old_nvals ; ibr++ ) myXtFree( old_brick_stataux[ibr] ) ;
      myXtFree( old_brick_stataux ) ;
   }

   /** if dataset has statistics, rearrange them **/

   if( ISVALID_STATISTIC(dset->stats) ){
      THD_statistics * new_stats , * old_stats ;
      THD_brick_stats * bsold , * bsnew ;

      old_stats = dset->stats ;
      new_stats = myXtNew( THD_statistics ) ;
      new_stats->type   = STATISTICS_TYPE ;
      new_stats->parent = (XtPointer) dset ;
      new_stats->bstat  = NULL ;

      bsold = old_stats->bstat ;
      bsnew = new_stats->bstat =
         (THD_brick_stats *) XtCalloc( new_nvals , sizeof(THD_brick_stats) ) ;

      new_stats->nbstat = new_nvals ;

      for( ibr=0 ; ibr < new_nvals ; ibr++ ){
         if( ibr < old_stats->nbstat ) bsnew[ibr] = bsold[ivl[ibr]] ;
         else                          INVALIDATE_BSTAT( bsnew[ibr] ) ;
      }

      REPLACE_KILL( dset->kl , bsold     , bsnew     ) ;
      REPLACE_KILL( dset->kl , old_stats , new_stats ) ;
      dset->stats = new_stats ;

      myXtFree(bsold) ; myXtFree(old_stats) ;
   }

   return ;
}

/*-----------------------------------------------------------------
   Get an integer list in the range 0..(nvals-1), from the
   character string str.  If we call the output pointer fred,
   then fred[0] = number of integers in the list (> 0), and
        fred[i] = i-th integer in the list for i=1..fred[0].
   If fred == NULL or fred[0] == 0, then something is wrong.
-------------------------------------------------------------------*/

static int * get_intlist( int nvals , char * str )
{
   int * subv = NULL ;
   int ii , ipos , nout , slen ;
   int ibot,itop,istep , nused ;
   char * cpt ;

   /* Meaningless input? */

   if( nvals < 1 ) return NULL ;

   /* No selection list? */

   if( str == NULL || str[0] == '\0' ) return NULL ;

   /* skip initial '[' */

   subv    = (int *) XtMalloc( sizeof(int) * 2 ) ;
   subv[0] = nout = 0 ;

   ipos = 0 ;
   if( str[ipos] == '[' ) ipos++ ;

   /*** loop through each sub-selector until end of input ***/

   slen = strlen(str) ;
   while( ipos < slen && str[ipos] != ']' ){

      /** get starting value **/

      if( str[ipos] == '$' ){  /* special case */
         ibot = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         ibot = strtol( str+ipos , &cpt , 10 ) ;
         if( ibot < 0 ){ myXtFree(subv) ; return NULL ; }
         if( ibot >= nvals ) ibot = nvals-1 ;
         nused = (cpt-(str+ipos)) ;
         if( ibot == 0 && nused == 0 ){ myXtFree(subv) ; return NULL ; }
         ipos += nused ;
      }

      /** if that's it for this sub-selector, add one value to list **/

      if( str[ipos] == ',' || str[ipos] == '\0' || str[ipos] == ']' ){
         nout++ ;
         subv = (int *) XtRealloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ibot ;
         ipos++ ; continue ;  /* re-start loop at next sub-selector */
      }

      /** otherwise, must have '..' or '-' as next inputs **/

      if( str[ipos] == '-' ){
         ipos++ ;
      } else if( str[ipos] == '.' && str[ipos+1] == '.' ){
         ipos++ ; ipos++ ;
      } else {
         myXtFree(subv) ; return NULL ;
      }

      /** get ending value for loop now **/

      if( str[ipos] == '$' ){  /* special case */
         itop = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         itop = strtol( str+ipos , &cpt , 10 ) ;
         if( itop < 0 ){ myXtFree(subv) ; return NULL ; }
         if( itop >= nvals ) itop = nvals-1 ;
         nused = (cpt-(str+ipos)) ;
         if( itop == 0 && nused == 0 ){ myXtFree(subv) ; return NULL ; }
         ipos += nused ;
      }

      /** set default loop step **/

      istep = (ibot <= itop) ? 1 : -1 ;

      /** check if we have a non-default loop step **/

      if( str[ipos] == '(' ){  /* decode an integer */
         ipos++ ;
         istep = strtol( str+ipos , &cpt , 10 ) ;
         if( istep == 0 ){ myXtFree(subv) ; return NULL ; }
         nused = (cpt-(str+ipos)) ;
         ipos += nused ;
         if( str[ipos] == ')' ) ipos++ ;
      }

      /** add values to output **/

      for( ii=ibot ; (ii-itop)*istep <= 0 ; ii += istep ){
         nout++ ;
         subv = (int *) XtRealloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ii ;
      }

      /** check if we have a comma to skip over **/

      if( str[ipos] == ',' ) ipos++ ;

   }  /* end of loop through selector string */

   return subv ;
}
