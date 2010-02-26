/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/***** This program makes a "duplicate" of a dataset, which is
       nothing more than something that is warped from the parent
       with the identity warp.

       Why is this useful?  Because you can then overwrite this
       warped-on-demand dataset in AFNI.

       RWCox, June 1995
*****/

static char DUP_session[THD_MAX_NAME]  = "./" ;
static char DUP_prefix[THD_MAX_PREFIX] = "dup" ;
static char DUP_label[THD_MAX_LABEL]   = "\0" ;
static char DUP_dname[THD_MAX_NAME]    = "\0" ;

static int anatomy_type  = ILLEGAL_TYPE ;
static int function_type = ILLEGAL_TYPE ;
static int dataset_type  = ILLEGAL_TYPE ;

THD_3dim_dataset * duplicate_dataset( THD_3dim_dataset * parent ) ;

int main( int argc , char * argv[] )
{
   int nopt , ii ;
   THD_3dim_dataset * dset_in , * dset_out ;

   /** check for help **/

   if( argc < 2 || strncmp(argv[1],"-help",3) == 0 ){
      printf(
       "Usage: dsetdup [options] dataset\n"
       " 'Duplicates' a dataset by making a warp-on-demand copy.\n"
       " Applications:\n"
       "   - allows AFNI to resample a dataset to a new grid without\n"
       "       destroying an existing data .BRIK\n"
       "   - change a functional dataset to anatomical, or vice-versa\n"
       "\n"
       "OPTIONS:\n"
       "  -type             = convert to the given 'type', which must be\n"
       "                       chosen from the same list as in to3d\n"
       "  -session dirname  = write output into given directory (default=./)\n"
       "  -prefix  pname    = use 'pname' for the output directory prefix\n"
       "                       (default=dup)\n"
       "  -label   string   = use 'string' for the label in the output\n"
       "                       dataset (default = pname)\n"
       "  -dname   name     = will make 3D dataset's name = 'name'\n"
       "\n"
       "N.B.: Even if the new dataset is anatomical, it will not contain\n"
       "      any markers, duplicated from the original, or otherwise.\n"
     ) ;
     exit(0) ;
   }

   /** scan command line options **/

#define DUPERR(str) \
   do{ fprintf(stderr,"ERROR: %s\n",(str)) ; exit(1) ; } while(1)

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*** the following code is borrowed from to3d ***/

      /* -type from the anatomy prefixes */

      for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ )
         if( strncmp( &(argv[nopt][1]) ,
                      ANAT_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      if( ii <= LAST_ANAT_TYPE ){
         anatomy_type = ii ;
         dataset_type = HEAD_ANAT_TYPE ;
         nopt++ ; continue ;
      }

      /* -type from the function prefixes */

      for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ )
         if( strncmp( &(argv[nopt][1]) ,
                      FUNC_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      if( ii <= LAST_FUNC_TYPE ){
         function_type = ii ;
         dataset_type  = HEAD_FUNC_TYPE ;
         nopt++ ; continue ;
      }

      /**** -session dirname ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) DUPERR("need argument after -session!") ;
         MCW_strncpy( DUP_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) DUPERR("need argument after -prefix!") ;
         MCW_strncpy( DUP_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -label string ****/

      if( strncmp(argv[nopt],"-label",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) DUPERR("need argument after -label!") ;
         MCW_strncpy( DUP_label , argv[nopt++] , THD_MAX_LABEL ) ;
         continue ;
      }

      /**** -dname string ****/

      if( strncmp(argv[nopt],"-dname",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ) DUPERR("need argument after -dname!") ;
         MCW_strncpy( DUP_dname , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;
   }  /* end of loop over options */

   if( strlen(DUP_label) == 0 ){
      MCW_strncpy(DUP_label,DUP_prefix,THD_MAX_LABEL) ;
   }

   if( nopt >= argc ) DUPERR("no dataset name given!") ;

   /*** read input dataset ***/

   dset_in = THD_open_one_dataset( argv[nopt] ) ;
   if( ! ISVALID_3DIM_DATASET(dset_in) ) DUPERR("cannot read dataset!\n") ;

   /*** copy header info ***/

   dset_out = duplicate_dataset( dset_in ) ;
   if( ! ISVALID_3DIM_DATASET(dset_out) ) DUPERR("cannot make duplicate!\n") ;

   /*** rewrite some strings ***/

   strcpy( dset_out->label1 , DUP_label ) ;
   if( strlen(DUP_dname) > 0 ) strcpy( dset_out->self_name , DUP_dname ) ;

   /*** change of type? ***/

   if( dataset_type >= FIRST_3DIM_TYPE && dataset_type <= LAST_3DIM_TYPE ){

      int isfunc , new_nvals ;

      isfunc    = ISFUNCTYPE(dataset_type) ;
      new_nvals = (isfunc) ? FUNC_nvals[function_type]
                           : ANAT_nvals[anatomy_type]  ;

      if( new_nvals > dset_in->dblk->nvals ){
         fprintf(stderr,
                 "ERROR: new dataset type has %d values per voxel, but old has %d!\n"
                 "       ==> cannot make duplicate!\n" ,
                 new_nvals , dset_in->dblk->nvals ) ;
         exit(1) ;

      } else if( new_nvals < dset_in->dblk->nvals ){
         fprintf(stderr,
                 "WARNING: new dataset type has %d values per voxel, but old has %d!\n"
                 "         ==> new dataset will not access all data in old!\n",
                 new_nvals , dset_in->dblk->nvals ) ;
      }

      dset_out->type      = dataset_type ;
      dset_out->func_type = ISANAT(dset_out) ? (anatomy_type)
                                             : (function_type) ;
   }

   /*** done! ***/

   THD_write_3dim_dataset( DUP_session , DUP_prefix , dset_out , False ) ;

   exit(0) ;
}

/*---------------------------------------------------------------------
   Adapted from AFNI_follower_dataset:
     Make a warped dataset whose grid corresponds to the parent and
     whose warp is just the identity warp.
-----------------------------------------------------------------------*/

THD_3dim_dataset * duplicate_dataset( THD_3dim_dataset * parent )
{
   THD_3dim_dataset * new_dset ;
   int ii ;

   /* sanity checks */

   if( ! ISVALID_3DIM_DATASET(parent) ) return NULL ;

   /* make new dataset, copying appropriate fields from its various parents */

   new_dset = myXtNew( THD_3dim_dataset ) ; INIT_KILL( new_dset->kl ) ;

   new_dset->type      = parent->type ;
   new_dset->func_type = parent->func_type ;
   new_dset->view_type = parent->view_type ;
   new_dset->Label_Dtable = NULL;                  /* ZSS Feb 26 2010 */

   new_dset->anat_parent         = NULL ;   /* no anat parent */
   new_dset->anat_parent_name[0] = '\0' ;

   new_dset->warp_parent =  parent ;        /* yes warp parent */
   MCW_strncpy( new_dset->warp_parent_name , parent->self_name , THD_MAX_NAME ) ;

   /* make the actual warp from the warp_parent to this dataset */

   new_dset->vox_warp       = myXtNew( THD_warp ) ;
   new_dset->vox_warp->type = ILLEGAL_TYPE ;        /* created when needed */
   new_dset->warp           = myXtNew( THD_warp ) ;
   *(new_dset->warp)        = IDENTITY_WARP ;

   /* make up some names for this new dataset */

   MCW_strncpy( new_dset->self_name  , parent->self_name , THD_MAX_NAME-5 ) ;
   ii = strlen( new_dset->self_name ) ;
   MCW_strncpy( &(new_dset->self_name[ii]) , "%duplicate" , THD_MAX_NAME-ii ) ;

   MCW_strncpy( new_dset->label1 , parent->label1 , THD_MAX_LABEL ) ;
   MCW_strncpy( new_dset->label2 , parent->label2 , THD_MAX_LABEL ) ;

   /* set the axes for this new dataset
      (same as parent, since that's the meaning of this routine) */

   new_dset->daxes         = myXtNew( THD_dataxes ) ;  /* copy data axes of */
   *(new_dset->daxes)      = *(parent->daxes) ;      /* parent */
   new_dset->daxes->parent = (XtPointer) new_dset ;  /* reset parent */

   new_dset->wod_daxes     = myXtNew( THD_dataxes ) ;  /* warp-on-demand */
   *(new_dset->wod_daxes)  = *(new_dset->daxes) ;
   new_dset->wod_flag      = True ;

   /* create a datablock and diskptr, in case the data is ever
      filled into memory (instead of wod) and written to disk */

   new_dset->dblk = myXtNew( THD_datablock ) ; INIT_KILL( new_dset->dblk->kl ) ;

   new_dset->dblk->type        = DATABLOCK_TYPE ;
   new_dset->dblk->nvals       = parent->dblk->nvals ;
   new_dset->dblk->brick       = NULL ;
   new_dset->dblk->malloc_type = DATABLOCK_MEM_UNDEFINED ;
   new_dset->dblk->total_bytes = 0 ;
   new_dset->dblk->brick_bytes = 0 ;
   new_dset->dblk->natr        = new_dset->dblk->natr_alloc  = 0 ;
   new_dset->dblk->atr         = NULL ;
   new_dset->dblk->parent      = (XtPointer) new_dset ;

   DBLK_unlock(new_dset->dblk) ;

   new_dset->dblk->diskptr               = myXtNew( THD_diskptr ) ;
   new_dset->dblk->diskptr->type         = DISKPTR_TYPE ;
   new_dset->dblk->diskptr->nvals        = parent->dblk->nvals ;
   new_dset->dblk->diskptr->rank         = 3 ;
   new_dset->dblk->diskptr->storage_mode = STORAGE_UNDEFINED ;
   new_dset->dblk->diskptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */
   new_dset->dblk->diskptr->dimsizes[0]  = new_dset->daxes->nxx ;
   new_dset->dblk->diskptr->dimsizes[1]  = new_dset->daxes->nyy ;
   new_dset->dblk->diskptr->dimsizes[2]  = new_dset->daxes->nzz ;

   /* create the names for storage on disk (if ever)
      -- note we put it in the same directory as the parent */

   THD_init_diskptr_names( new_dset->dblk->diskptr ,
                           parent->dblk->diskptr->directory_name , NULL ,
                           parent->dblk->diskptr->prefix ,
                           new_dset->view_type , True ) ;

   ADDTO_KILL( new_dset->dblk->kl , new_dset->dblk->diskptr ) ;

   /* oh yeah, set the new_dset kill list,
      copy statistics if available, and NULL out any unused stuff */

   ADDTO_KILL( new_dset->kl , new_dset->warp ) ;
   ADDTO_KILL( new_dset->kl , new_dset->vox_warp ) ;
   ADDTO_KILL( new_dset->kl , new_dset->daxes ) ;
   ADDTO_KILL( new_dset->kl , new_dset->wod_daxes ) ;
   ADDTO_KILL( new_dset->kl , new_dset->dblk ) ;

   new_dset->self_warp = NULL ;  /* 26 Aug 2002 */

   if( parent->stats != NULL ){
      new_dset->stats         = myXtNew( THD_statistics ) ;  /* copy statistics */
      *(new_dset->stats)      = *(parent->stats) ;         /* of parent */
      new_dset->stats->parent = (XtPointer) new_dset ;
      ADDTO_KILL( new_dset->kl , new_dset->stats ) ;
   } else {
      new_dset->stats = NULL ;
   }

   new_dset->markers     = NULL ;  /* no markers */
   new_dset->death_mark  = 0 ;     /* don't kill me! */
   new_dset->tcat_list   = 0 ;
   new_dset->tcat_num    = 0 ;
   new_dset->tcat_len    = NULL ;

   return(new_dset) ;
}
