/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/* prototypes */

static int THD_setup_mastery( THD_3dim_dataset * , int * ) ;
static THD_3dim_dataset * THD_open_3dcalc( char * ) ;

/*-----------------------------------------------------------------
   11 Jan 1999: Open a dataset, allowing for possible mastering.
   21 Feb 2001: Allow for <a..b> sub-ranging as well.
   26 Jul 2004: Change THD_setup_mastery to return int.    [rickr]
                Add THD_copy_dset_subs() function.
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_dataset( char *pathname )
{
   THD_3dim_dataset *dset ;
   char dname[THD_MAX_NAME] , subv[THD_MAX_NAME] ;
   char *cpt , *bpt ;
   int  *ivlist=NULL ;
   int    ii , jj , kk ;
   float  bot=1.0 , top=0.0 ;

ENTRY("THD_open_dataset") ;

   /*-- sanity check --*/

   if( pathname == NULL            ||
       (ii=strlen(pathname)) == 0  ||
       pathname[ii-1]        == '/'  ) RETURN(NULL) ;

   /*-- 23 Mar 2001: perhaps get from across the Web --*/

   if( strncmp(pathname,"http://",7) == 0 ||
       strncmp(pathname,"ftp://" ,6) == 0   ){

      dset = THD_fetch_dataset( pathname ) ;
      RETURN(dset) ;
   }

   /*-- 17 Mar 2000: check if this is a 3dcalc() run --*/

   if( strncmp(pathname,"3dcalc(",7)  == 0 ){
     dset = THD_open_3dcalc( pathname ) ;
     RETURN(dset) ;
   }

   /*-- 04 Mar 2003: allow input of .1D files   --*/
   /*--              which deals with [] itself --*/

   if( strstr(pathname,".1D") != NULL ){
     dset = THD_open_1D( pathname ) ;
     if( dset != NULL ) RETURN(dset) ;
   }

   /*-- 04 Aug 2004: allow input of a list of dataset, separated by spaces --*/

   if( strchr(pathname,' ') != NULL ){
     dset = THD_open_tcat( pathname ) ;
     RETURN(dset) ;
   }

   /*-- find the opening "[" and/or "<" --*/

   cpt = strstr(pathname,"[") ;
   bpt = strstr(pathname,"<") ;  /* 21 Feb 2001 */

   if( cpt == NULL && bpt == NULL ){            /* no "[" or "<"  */
     dset = THD_open_one_dataset( pathname ) ;  /* ==> open      */
     RETURN(dset) ;                             /*     normally */
   }

   if( cpt == pathname || bpt == pathname ) RETURN(NULL);  /* error */

   /* copy dataset filename to dname and selector string to subv */

   ii = (cpt != NULL ) ? cpt - pathname : 999999 ;
   jj = (bpt != NULL ) ? bpt - pathname : 999999 ;
   kk = MIN(ii,jj) ;
   memcpy(dname,pathname,kk) ; dname[kk] = '\0' ;

   if( STRING_HAS_SUFFIX(dname,".mnc") ||
       STRING_HAS_SUFFIX(dname,".hdr") ||
       STRING_HAS_SUFFIX(dname,".nia") ||
       STRING_HAS_SUFFIX(dname,".nii") ||
       STRING_HAS_SUFFIX(dname,".mri") ||
       STRING_HAS_SUFFIX(dname,".svl")   ){

     fprintf(stderr,"** Can't use selectors on dataset: %s\n",pathname) ;
     RETURN(NULL) ;
   }

   /* open the dataset */

   dset = THD_open_one_dataset( dname ) ;
   if( dset == NULL ) RETURN(NULL) ;

   /* parse the sub-brick selector string (if any) */

   if( cpt != NULL ){
     char *qpt ;
     strcpy(subv,cpt) ;
     qpt = strstr(subv,"<") ; if( qpt != NULL ) *qpt = '\0' ;
     ivlist = MCW_get_intlist( DSET_NVALS(dset) , subv ) ;
   }
   if( ivlist == NULL ){
     if( cpt != NULL )
       fprintf(stderr,"** WARNING: bad sub-brick selector => using [0..%d]\n",
               DSET_NVALS(dset)-1) ;
     ivlist = (int *) malloc(sizeof(int)*(DSET_NVALS(dset)+1)) ;
     ivlist[0] = DSET_NVALS(dset) ;
     for( kk=0 ; kk < ivlist[0] ; kk++ ) ivlist[kk+1] = kk ;
   }

   /* 21 Feb 2001: if present, load the sub-range data */

   if( bpt != NULL ){
      char *dpt = strstr(bpt,"..") ;
#if 0
fprintf(stderr,"bpt=%s\n",bpt) ;
#endif
      if( dpt != NULL ){
#if 0
fprintf(stderr,"dpt=%s\n",dpt) ;
#endif
         kk  = sscanf( bpt+1 , "%f" , &bot ) ;
         kk += sscanf( dpt+2 , "%f" , &top ) ;
         if( kk == 2 && bot <= top ){
            dset->dblk->master_bot = bot ;
            dset->dblk->master_top = top ;
         } else {
            dset->dblk->master_bot = 1.0 ;
            dset->dblk->master_top = 0.0 ;
         }
      }
   }

   /* modify the dataset according to the selector string */

   THD_setup_mastery( dset , ivlist ) ;
   free(ivlist) ;

   RETURN(dset) ;
}

/*-----------------------------------------------------------------
   Set up a dataset for being mastered; that is, reading only
   a subset of sub-bricks from the master .BRIK file.
-------------------------------------------------------------------*/

static int THD_setup_mastery( THD_3dim_dataset *dset , int *ivlist )
{
   int ibr , old_nvals , new_nvals ;
   THD_datablock *dblk ;
   int *btype , *ivl ;

   float * old_brick_fac  ;
   int *   old_brick_bytes ;
   char ** old_brick_lab  ;
   char ** old_brick_keywords ;
   int *   old_brick_statcode ;
   float **old_brick_stataux ;

ENTRY("THD_setup_mastery") ;

   /** sanity checks **/

   if( ! ISVALID_DSET(dset) || ivlist == NULL || ivlist[0] <= 0 ) RETURN(1) ;

   new_nvals = ivlist[0] ;
   ivl       = ivlist + 1 ;
   dblk      = dset->dblk ;
   old_nvals = dblk->nvals ;

   ibr = THD_count_databricks(dblk) ; if( ibr > 0 ) RETURN(2) ;

   for( ibr=0 ; ibr < new_nvals ; ibr++ )
      if( ivl[ibr] < 0 || ivl[ibr] >= old_nvals ) RETURN(3) ;

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
   } else {                                  /* 21 Feb 2001: change to bucket type */

      if( ISANAT(dset) && !ISANATBUCKET(dset) )
         EDIT_dset_items( dset , ADN_func_type,ANAT_BUCK_TYPE , ADN_none ) ;
      else if( ISFUNC(dset) && !ISFUNCBUCKET(dset) )
         EDIT_dset_items( dset , ADN_func_type,FUNC_BUCK_TYPE , ADN_none ) ;

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
      float bot,top ;

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

      /* 21 Feb 2001: mangle statistics if sub-ranging is used */

      bot = dset->dblk->master_bot ; top = dset->dblk->master_top ;
      if( bot <= top ){
              if( bot > 0.0 ) bot = 0.0 ;
         else if( top < 0.0 ) top = 0.0 ;

         for( ibr=0 ; ibr < new_nvals ; ibr++ ){
            if( ISVALID_BSTAT(bsnew[ibr]) ){
               if( bsnew[ibr].min < bot ) bsnew[ibr].min = bot ;
               if( bsnew[ibr].max > top ) bsnew[ibr].max = top ;
            }
         }
      }
   }

   RETURN(0) ;
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

ENTRY("THD_open_3dcalc") ;

   /*-- remove the "3dcalc(" and the ")" from the input string --*/

   qname = (char *) malloc(sizeof(char)*(strlen(pname)+1024)) ;
   strcpy(qname,pname+7) ;
   ll = strlen(qname) ;
   for( ii=ll-1 ; ii > 0 && qname[ii] != ')' ; ii++ ) ; /* nada */
   if( ii == 0 ){ free(qname) ; RETURN(NULL) ; }
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
     free(qname) ; RETURN(NULL) ;
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

   if( newArgv == NULL ) RETURN(NULL) ;  /* something bad? */

   if( newArgc < 3 ){                   /* too few args to 3dcalc */
     for( ii=0 ; ii < newArgc ; ii++ ) free(newArgv[ii]) ;
     free(newArgv) ; RETURN(NULL) ;
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
     free(newArgv) ; RETURN(NULL) ;
   }

   if( child_pid == 0 ){  /*-- I'm the child --*/

     execvp( "3dcalc" , newArgv ) ;        /* should not return */
     perror("*** Can't execvp 3dcalc()") ;
     _exit(1) ;

   }

   /*-- I'm the parent --*/

   (void) waitpid( child_pid , NULL , 0 ) ; /* wait for child to exit */

   ii = THD_is_dataset( tdir , prefix , -1 ) ;
   if( ii == -1 ){
     fprintf(stderr,"*** 3dcalc() failed - no dataset created\n") ;
     RETURN(NULL) ;
   }
   qname = THD_dataset_headname( tdir , prefix , ii ) ;
   dset = THD_open_one_dataset( qname ) ;  /* try to read result */

   for( ii=0 ; ii < newArgc-1 ; ii++ ) free(newArgv[ii]) ;  /* toss trash */
   free(newArgv) ; free(qname) ;

   if( dset == NULL ){                          /* read failed */
     fprintf(stderr,"*** 3dcalc() failed - can't read dataset\n") ;
     RETURN(NULL) ;
   }

   /* read dataset into memory */

   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){                   /* can't read it? */
     THD_delete_3dim_dataset( dset , True ) ; /* kill it dead */
     fprintf(stderr,"*** 3dcalc() failed - can't load dataset\n") ;
     RETURN(NULL) ;
   }

   /* lock dataset into memory, delete its files */

   DSET_lock(dset) ;
   unlink( dset->dblk->diskptr->header_name ) ;
   COMPRESS_unlink( dset->dblk->diskptr->brick_name ) ;

   /* 30 Jul 2003: changes its directory to cwd */

   EDIT_dset_items( dset , ADN_directory_name , "./" , ADN_none ) ;

   RETURN(dset) ;
}

/*-----------------------------------------------------------------
   Copy a list of sub-bricks from a dataset.    26 Jul 2004 [rickr]
   The first element of dlist is the number of sub-bricks to copy.
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_copy_dset_subs( THD_3dim_dataset * din, int * dlist )
{
    THD_3dim_dataset * dout;
    MRI_TYPE           kind;
    char             * newdata;
    int                sub, subs;
    int                dsize, nxyz, rv;

ENTRY("THD_copy_dset_subs");

    /* validate inputs */
    if ( !din || !dlist )
    {
	fprintf(stderr, "** THD_copy_dset_subs: bad input (%p,%p)\n",
		din,dlist);
	RETURN(NULL);
    }

    if ( dlist[0] <= 0 )
    {
	fprintf(stderr,"** THD_copy_dset_subs: invalid dlist length %d\n",
		dlist[0]);
	RETURN(NULL);
    }

    dout = EDIT_empty_copy(din);
    rv = THD_setup_mastery(dout, dlist);
    if ( rv != 0 )
    {
	fprintf(stderr, "** failure: THD_setup_mastery() returned %d\n", rv);
	RETURN(NULL);
    }

    /* be sure that we have some data to copy */
    DSET_load(din);
    if ( ! DSET_LOADED(din) )
    {
	fprintf(stderr,"** THD_copy_dset_subs: cannot load input dataset\n");
	RETURN(NULL);
    }

    /* a basic warp is needed if header is written out - PLUTO_add_dset() */
    dout->warp  = myXtNew( THD_warp );
    *dout->warp = IDENTITY_WARP;
    ADDTO_KILL( dout->kl, dout->warp );

    dout->dblk->diskptr->byte_order   = mri_short_order();
    dout->dblk->diskptr->storage_mode = STORAGE_BY_BRICK;

    /* now copy all of the sub-bricks */
    nxyz = dout->daxes->nxx * dout->daxes->nyy * dout->daxes->nzz;
    subs = dlist[0];
    for ( sub = 0; sub < subs; sub++ )
    {
	kind = DSET_BRICK_TYPE(dout, sub);
	dsize = mri_datum_size( kind );
	if ( (newdata = (char *)malloc( nxyz * dsize )) == NULL )
        {
            fprintf( stderr, "r frdb: alloc failure: %d bytes!\n",
                     nxyz * dsize );
	    DSET_delete(dout);
            RETURN(NULL);
        }

	memcpy(newdata,DSET_ARRAY(din,dlist[sub+1]), nxyz*dsize);
	EDIT_substitute_brick(dout, sub, kind, (void *)newdata);
    }

    dout->dblk->malloc_type = DATABLOCK_MEM_MALLOC;
    dout->wod_flag = False;             /* since data is now in memory */

    RETURN(dout);
}
