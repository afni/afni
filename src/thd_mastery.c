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
   THD_3dim_dataset *dset=NULL ;
   char dname[THD_MAX_NAME]="\0" , *subv=NULL ;  /* 8 May 2007 */
   char *cpt=NULL , *bpt=NULL ;
   int  *ivlist=NULL ;
   int    ii=-1, jj=-1, kk=-1;
   float  bot=1.0 , top=0.0 ;
   static char qname[THD_MAX_NAME+222] ;

ENTRY("THD_open_dataset") ;

   /*-- sanity check --*/

   if( pathname == NULL            ||
       (ii=strlen(pathname)) == 0  ||
       pathname[ii-1]        == '/'  ) RETURN(NULL) ;

   if( pathname[0] == '~' && pathname[1] == '/' ){  /* 13 Feb 2008 */
     char *eee = getenv("HOME") ;
     if( eee != NULL ){
       strcpy(qname,eee); strcat(qname,pathname+1);
       pathname = qname ;
     }
   }

   /*-- 23 Mar 2001: perhaps get from across the Web --*/

   if( strncmp(pathname,"http://",7) == 0 ||
       strncmp(pathname,"ftp://" ,6) == 0   ){

     dset = THD_fetch_dataset( pathname ) ;
     if( ISVALID_DSET(dset) &&
        !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(dset->daxes) ;
     THD_patch_brickim(dset) ;                       /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 17 Mar 2000: check if this is a 3dcalc() run --*/

   if( strncmp(pathname,"3dcalc(",7) == 0 ||
       strncmp(pathname,"3dcalc ",7) == 0   ){
     dset = THD_open_3dcalc( pathname ) ;
     if( ISVALID_DSET(dset) &&
        !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(dset->daxes) ;
     THD_patch_brickim(dset) ;                       /* 20 Oct 2006 */
     RETURN(dset) ;
   }

   /*-- 04 Mar 2003: allow input of .1D files   --*/
   /*--              which deals with [] itself --*/

   if( strstr(pathname,".1D") != NULL || strncmp(pathname,"1D:",3) == 0 ){
     dset = THD_open_1D( pathname ) ;
     if( ISVALID_DSET(dset) &&
        !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(dset->daxes) ;
     if( dset != NULL ){ THD_patch_brickim(dset); RETURN(dset); }
   }

   /*-- 04 Aug 2004: allow input of a list of dataset, separated by spaces --*/
   /*  unless a count command is used inside the brackets 9 May 2007 drg*/
   if((strchr(pathname,' ') != NULL ) && 
      (strstr(pathname,"[count ")==NULL)){
     dset = THD_open_tcat( pathname ) ;
     if( ISVALID_DSET(dset) &&
        !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(dset->daxes) ;
     THD_patch_brickim(dset); RETURN(dset) ;
   }

   /*-- find the opening "[" and/or "<" --*/

   cpt = strstr(pathname,"[") ;
   bpt = strstr(pathname,"<") ;  /* 21 Feb 2001 */
   if( cpt == NULL && bpt == NULL ){            /* no "[" or "<"  */
     dset = THD_open_one_dataset( pathname ) ;  /* ==> open      */
     if( ISVALID_DSET(dset) &&
        !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(dset->daxes) ;
     THD_patch_brickim(dset); RETURN(dset);     /*     normally */
   }

   if( cpt == pathname || bpt == pathname ) RETURN(NULL);  /* error */

   /* copy dataset filename to dname and selector string to subv */

   ii = (cpt != NULL ) ? cpt - pathname : 999999 ;
   jj = (bpt != NULL ) ? bpt - pathname : 999999 ;
   kk = MIN(ii,jj) ;
   memcpy(dname,pathname,kk) ; dname[kk] = '\0' ;

   /* allow NIfTI extensions here                14 Apr 2006 [rickr] */
   if( STRING_HAS_SUFFIX(dname,".mnc")    ||
       STRING_HAS_SUFFIX(dname,".mri")    ||
       STRING_HAS_SUFFIX(dname,".svl")      ){
     ERROR_message("Can't use selectors on dataset: %s",pathname) ;
     RETURN(NULL) ;
   }

   /* open the dataset */

   dset = THD_open_one_dataset( dname ) ;
   if( dset == NULL ) RETURN(NULL) ;

   /* parse the sub-brick selector string (if any) */

   if( cpt != NULL ){
     char *qpt ;
     subv = strdup(cpt);
     /* strcpy(subv,cpt) ;  don't assume length   8 May 2007 [rickr,dglen] */
     qpt = strstr(subv,"<") ; if( qpt != NULL ) *qpt = '\0' ;
     ivlist = MCW_get_intlist( DSET_NVALS(dset) , subv ) ;
     free(subv) ;
   }
   if( ivlist == NULL ){
     if( cpt != NULL )
       WARNING_message("bad sub-brick selector => using [0..%d]",
                       DSET_NVALS(dset)-1) ;
     ivlist = (int *) malloc(sizeof(int)*(DSET_NVALS(dset)+1)) ;
     ivlist[0] = DSET_NVALS(dset) ;
     for( kk=0 ; kk < ivlist[0] ; kk++ ) ivlist[kk+1] = kk ;
   }

   /* ************************************************ */
   /* 21 Feb 2001: if present, load the sub-range data */

   /* THD_init_one_datablock() was not called   14 Apr 2006 [rickr] */
   if( STRING_HAS_SUFFIX(dname,".hdr") || STRING_HAS_SUFFIX(dname,".nia")    ||
       STRING_HAS_SUFFIX(dname,".nii") || STRING_HAS_SUFFIX(dname,".nii.gz") ||
       STRING_HAS_SUFFIX(dname,".niml.dset")                                 ||
       STRING_HAS_SUFFIX(dname,".gii") || STRING_HAS_SUFFIX(dname,".gii.dset")){
      dset->dblk->master_bot = 1.0 ;
      dset->dblk->master_top = 0.0 ;
   }

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
      } else {/* ZSS: Why not allow for <val> ? */
         kk  = sscanf( bpt+1 , "%f" , &bot ) ;
         dset->dblk->master_bot = bot ;
         dset->dblk->master_top = bot ;
      }
   }

   /* modify the dataset according to the selector string */

   THD_setup_mastery( dset , ivlist ) ;
   free(ivlist) ;

   if( ISVALID_DSET(dset) &&
      !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
     THD_daxes_to_mat44(dset->daxes) ;

   THD_patch_brickim(dset); RETURN(dset);
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
   floatvec **old_brick_fdrcurve ;  /* 23 Jan 2008 */
   floatvec **old_brick_mdfcurve ;  /* 22 Oct 2008 */

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
   old_brick_fdrcurve = dblk->brick_fdrcurve ; dblk->brick_fdrcurve = NULL ;
   old_brick_mdfcurve = dblk->brick_mdfcurve ; dblk->brick_mdfcurve = NULL ;

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

   /* redo brick_fdrcurve now */

   if( old_brick_fdrcurve != NULL ){  /* 23 Jan 2008 */
     floatvec *fv , *nv ;
     dblk->brick_fdrcurve = (floatvec **)calloc(sizeof(floatvec *),new_nvals) ;
     for( ibr=0 ; ibr < new_nvals ; ibr++ ){
       fv = old_brick_fdrcurve[ivl[ibr]] ;
       if( fv == NULL ){ nv = NULL; } else { COPY_floatvec(nv,fv); }
       dblk->brick_fdrcurve[ibr] = nv ;
     }
   }

   if( old_brick_mdfcurve != NULL ){  /* 22 Oct 2008 */
     floatvec *fv , *nv ;
     dblk->brick_mdfcurve = (floatvec **)calloc(sizeof(floatvec *),new_nvals) ;
     for( ibr=0 ; ibr < new_nvals ; ibr++ ){
       fv = old_brick_mdfcurve[ivl[ibr]] ;
       if( fv == NULL ){ nv = NULL; } else { COPY_floatvec(nv,fv); }
       dblk->brick_mdfcurve[ibr] = nv ;
     }
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

   if( old_brick_fdrcurve != NULL ){               /* 24 Jan 2008 */
     for( ibr=0 ; ibr < old_nvals ; ibr++ )
       KILL_floatvec( old_brick_fdrcurve[ibr] ) ;
     free(old_brick_fdrcurve) ;
   }
   if( old_brick_mdfcurve != NULL ){               /* 22 Oct 2008 */
     for( ibr=0 ; ibr < old_nvals ; ibr++ )
       KILL_floatvec( old_brick_mdfcurve[ibr] ) ;
     free(old_brick_mdfcurve) ;
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
   -- Modified 24 Jul 2009 to use unique name each time, fer shur
------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/wait.h>

static THD_3dim_dataset * THD_open_3dcalc( char *pname )
{
   int    Argc=1               ,   newArgc=0 , ii,ll  ;
   char  *Argv[1]={ "3dcalc" } , **newArgv=NULL ;
   char  *qname , *tdir , prefix[128] , *uuid ;
   pid_t  child_pid ;
   THD_3dim_dataset *dset ;

ENTRY("THD_open_3dcalc") ;

   /*-- remove the "3dcalc(" and the ")" from the input string --*/

   qname = (char *) malloc(sizeof(char)*(strlen(pname)+4096)) ;
   strcpy(qname,pname+7) ;
   ll = strlen(qname) ;
   for( ii=ll-1 ; ii > 0 && isspace(qname[ii]) ; ii-- ) ; /*nada*/
   if( ii == 0 ){ free(qname) ; RETURN(NULL) ; }  /* all blanks? */
   if( qname[ii] == ')' && isspace(qname[ii-1]) )
     qname[ii] = '\0' ;                   /* remove trailing ' )' */

   /*-- add -session to command string --*/

   tdir = my_getenv("TMPDIR") ;
   if( tdir == NULL || strlen(tdir) > 512 ) tdir = "/tmp" ;
   strcat(qname," -session ") ; strcat(qname,tdir) ; ll = strlen(tdir) ;

   /*-- add -prefix to command string --*/

   for( ii=0 ; ii < 9999 ; ii++ ){  /* create dataset name */
     uuid = UNIQ_idcode() ;
     sprintf(prefix,"3dcalc_%s",uuid) ; free(uuid) ;
     if( THD_is_dataset(tdir,prefix,-1) == -1 ) break ;
   }
   if( ii >= 9999 ){  /* should never happen */
     ERROR_message("Can't find unused 3dcalc_ dataset name in %s!",tdir) ;
     free(qname) ; RETURN(NULL) ;
   }

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

   INFO_message("Executing 3dcalc()") ;
#if 1
   for(ii=0; ii< newArgc-1; ii++)
     fprintf(stderr," argv[%d]=%s",ii,newArgv[ii]);
   fprintf(stderr,"\n") ;
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

   STATUS("Waiting for 3dcalc() process to run") ;

   (void) waitpid( child_pid , NULL , 0 ) ; /* wait for child to exit */

   ii = THD_is_dataset( tdir , prefix , -1 ) ;
   if( ii == -1 ){
     ERROR_message("3dcalc() failed - no dataset created!") ;
     RETURN(NULL) ;
   }
   qname = THD_dataset_headname( tdir , prefix , ii ) ;
   dset = THD_open_one_dataset( qname ) ;  /* try to read result */

   for( ii=0 ; ii < newArgc-1 ; ii++ ) free(newArgv[ii]) ;  /* toss trash */
   free(newArgv) ; free(qname) ;

   if( dset == NULL ){                          /* read failed */
     ERROR_message("3dcalc() failed - can't read dataset!") ;
     RETURN(NULL) ;
   }

   /* read dataset into memory */

   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){                   /* can't read it? */
     THD_delete_3dim_dataset( dset , True ) ; /* kill it dead */
     ERROR_message("3dcalc() failed - can't load dataset!") ;
     RETURN(NULL) ;
   }

   /* lock dataset into memory, delete its files */

   DSET_lock(dset) ;
   unlink( dset->dblk->diskptr->header_name ) ;
   COMPRESS_unlink( dset->dblk->diskptr->brick_name ) ;

   /* 30 Jul 2003: changes its directory to cwd */

   EDIT_dset_items( dset , ADN_directory_name , "./" , ADN_none ) ;

   if( ISVALID_DSET(dset) &&
      !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
     THD_daxes_to_mat44(dset->daxes) ;

   RETURN(dset) ;
}

/*-----------------------------------------------------------------
   Convenience function, for copying only a single sub-brick.
   Wrapper for THD_copy_dset_subs().
-------------------------------------------------------------------*/
THD_3dim_dataset * THD_copy_one_sub( THD_3dim_dataset * din, int sub )
{
    int sublist[2] = {1, sub};
    return THD_copy_dset_subs(din, sublist);
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

    /* verify that the input sub-brick list hold valid indices */
    subs = dlist[0];
    for ( sub = 0; sub < subs; sub++ )
    {
        if( dlist[sub+1] < 0 || dlist[sub+1] >= din->dblk->nvals )
        {
            fprintf(stderr,
            "** THD_copy_dset_subs: index %d outside sub-brick range [0,%d]\n",
                    dlist[sub+1], din->dblk->nvals);
            RETURN(NULL);
        }
    }

    /* create initial dataset */
    dout = EDIT_empty_copy(din);

    /* use mastery to copy selected labels, statistics, etc. */
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

    /* do not create any warp structure here, since data will be inserted */

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

    /* clear mastery information, since data is already stored */
    if( DBLK_IS_MASTERED(dout->dblk) ){
        dout->dblk->master_nvals = 0;
        myXtFree( dout->dblk->master_ival );
        myXtFree( dout->dblk->master_bytes );
    }

    dout->dblk->malloc_type = DATABLOCK_MEM_MALLOC;
    dout->wod_flag = False;             /* since data is now in memory */

    RETURN(dout);
}

/*------------------------------------------------------------------------*/
/* Given a dataset selector like "name[1..3]", return individual selectors
   like "name[1]", "name[2]", "name[3]".  The reason for this is for older
   programs like 3dttest that operate on a list of single brick datasets.
   [19 Jul 2007 - RWCox]
--------------------------------------------------------------------------*/

THD_string_array * THD_multiplex_dataset( char *pathname )
{
   char *cpt , *pname , *bname ;
   int  *ivlist=NULL , ii ;
   THD_string_array *sar ;

ENTRY("THD_multiplex_dataset") ;
   if( pathname == NULL ) RETURN(NULL) ;
   cpt = strstr(pathname,"[") ; if( cpt == NULL ) RETURN(NULL) ;

   bname = strdup(pathname) ; cpt = strstr(bname,"[") ; *cpt = '\0' ;
   ivlist = MCW_get_intlist( 999999 , cpt+1 ) ;
   if( ivlist == NULL || ivlist[0] == 0 ){ free(bname); RETURN(NULL); }

   INIT_SARR(sar) ;
   pname = malloc(sizeof(char)*(strlen(bname)+16)) ;
   for( ii=1 ; ii <= ivlist[0] ; ii++ ){
     sprintf(pname,"%s[%d]",bname,ivlist[ii]) ;
     ADDTO_SARR(sar,pname) ;  /* makes a copy */
   }

   free(pname); free(ivlist); free(bname); RETURN(sar);
}
