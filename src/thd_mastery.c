/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/* prototypes */

static void THD_setup_mastery( THD_3dim_dataset * , int * ) ;
static THD_3dim_dataset * THD_open_3dcalc( char * ) ;

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

   /*-- 17 Mar 2000: check if this is a 3dcalc() run --*/

   if( strncmp(pathname,"3dcalc(",7)  == 0 ){
      dset = THD_open_3dcalc( pathname ) ;
      return dset ;
   }

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

   ivlist = MCW_get_intlist( DSET_NVALS(dset) , subv ) ;
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

/*----------------------------------------------------------------------
   Run 3dcalc to create a dataset and read it in.
   -- RWCox - 17 Mar 2000
------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/wait.h>

static THD_3dim_dataset * THD_open_3dcalc( char * pname )
{
   int    Argc=1               ,    newArgc=0 , ii,ll  ;
   char * Argv[1]={ "3dcalc" } , ** newArgv=NULL ;
   char * qname , * tdir , prefix[16] ;
   pid_t  child_pid ;
   THD_3dim_dataset * dset ;
   static int ibase=1 ;

   /*-- remove the "3dcalc(" and the ")" from the input string --*/

   qname = (char *) malloc(sizeof(char)*(strlen(pname)+1024)) ;
   strcpy(qname,pname+7) ;
   ll = strlen(qname) ;
   for( ii=ll-1 ; ii > 0 && qname[ii] != ')' ; ii++ ) ; /* nada */
   if( ii == 0 ){ free(qname) ; return NULL ; }
   qname[ii] = '\0' ;

   /*-- add -session to command string --*/

   tdir = my_getenv("TMPDIR") ;
   if( tdir == NULL || strlen(tdir) > 512 ) tdir = "/tmp" ;
   strcat(qname," -session ") ; strcat(qname,tdir) ; ll = strlen(tdir) ;

   /*-- add -prefix to command string --*/

   for( ii=ibase ; ii < 9999 ; ii++ ){                    /* dataset name   */
      sprintf(prefix,"3dcalc#%04d",ii) ;
      if( THD_is_dataset(tdir,prefix,-1) == -1 ) break ;
   }
   if( ii > 9999 ){
      fprintf(stderr,"*** Can't find unused 3dcalc# dataset name in %s!\n",tdir) ;
      free(qname) ; return NULL ;
   }
   ibase = ii+1 ;

   strcat(qname," -prefix ") ; strcat(qname,prefix) ;

   strcat(qname," -verbose") ;

   /*-- add a placeholder to be the last argument --*/

   strcat(qname," Zork") ;

   /*-- create the arg list for 3dcalc, starting with program name --*/

   append_string_to_args( qname , Argc , Argv , &newArgc , &newArgv ) ;

   free(qname) ; /* not needed no more */

   /*-- check if arg list was created OK --*/

   if( newArgv == NULL ) return NULL ;  /* something bad? */

   if( newArgc < 3 ){                   /* too few args to 3dcalc */
      for( ii=0 ; ii < newArgc ; ii++ ) free(newArgv[ii]) ;
      free(newArgv) ; return NULL ;
   }

   /*-- replace placeholder in arg list with NULL pointer --*/

   free( newArgv[newArgc-1] ) ; newArgv[newArgc-1] = NULL ;

   /*-- fork and exec --*/

   fprintf(stderr,"+++ Executing 3dcalc()\n") ;
#if 0
for(ii=0; ii< newArgc-1; ii++) fprintf(stderr," argv[%d]=%s\n",ii,newArgv[ii]);
#endif
   child_pid = fork() ;

   if( child_pid == (pid_t)(-1) ){
      perror("*** Can't fork 3dcalc()") ;
      for( ii=0 ; ii < newArgc-1 ; ii++ ) free(newArgv[ii]) ;
      free(newArgv) ; return NULL ;
   }

   if( child_pid == 0 ){  /*-- I'm the child --*/

      execvp( "3dcalc" , newArgv ) ;        /* should not return */
      perror("*** Can't execvp 3dcalc()") ;
      _exit(1) ;

   }

   /*-- I'm the parent --*/

   (void) wait(NULL) ;  /* wait for child to exit */

   ii = THD_is_dataset( tdir , prefix , -1 ) ;
   if( ii == -1 ){
      fprintf(stderr,"*** 3dcalc() failed - no dataset created\n") ;
      return NULL ;
   }
   qname = THD_dataset_headname( tdir , prefix , ii ) ;
   dset = THD_open_one_dataset( qname ) ;  /* try to read result */

   for( ii=0 ; ii < newArgc-1 ; ii++ ) free(newArgv[ii]) ;  /* toss trash */
   free(newArgv) ; free(qname) ;

   if( dset == NULL ){                          /* read failed */
      fprintf(stderr,"*** 3dcalc() failed - can't read dataset\n") ;
      return NULL ;
   }

   /* read dataset into memory */

   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){                   /* can't read it? */
      THD_delete_3dim_dataset( dset , True ) ; /* kill it dead */
      fprintf(stderr,"*** 3dcalc() failed - can't load dataset\n") ;
      return NULL ;
   }

   /* lock dataset into memory, delete its files */

   DSET_lock(dset) ;
   unlink( dset->dblk->diskptr->header_name ) ;
   COMPRESS_unlink( dset->dblk->diskptr->brick_name ) ;

   return dset ;
}
