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

       RWCox, June 1995, March 1996, April 1999
*****/

static char DUP_session[THD_MAX_NAME]  = "./" ;
static char DUP_prefix[THD_MAX_PREFIX] = "dup" ;

static int anatomy_type  = ILLEGAL_TYPE ;
static int function_type = ILLEGAL_TYPE ;
static int dataset_type  = ILLEGAL_TYPE ;

THD_3dim_dataset * duplicate_dataset( THD_3dim_dataset * parent ) ;

int main( int argc , char * argv[] )
{
   int nopt , ii ;
   THD_3dim_dataset * dset_in , * dset_out ;
   THD_warp * warp , * twarp ;

   /** check for help **/

WARNING_message("This program (3ddup) is old, not maintained, and probably useless!") ;

   if( argc < 2 || strncmp(argv[1],"-help",3) == 0 ){
      printf(
       "Usage: 3ddup [options] dataset\n"
       " 'Duplicates' a 3D dataset by making a warp-on-demand copy.\n"
       " Applications:\n"
       "   - allows AFNI to resample a dataset to a new grid without\n"
       "       destroying an existing data .BRIK\n"
       "   - change a functional dataset to anatomical, or vice-versa\n"
       "   - THIS PROGRAM IS BASICALLY OBSOLETE !!\n"
       "\n"
       "OPTIONS:\n"
       "  -'type'           = Convert to the given 'type', which must be\n"
       "                       chosen from the same list as in to3d\n"
       "  -session dirname  = Write output into given directory (default=./)\n"
       "  -prefix  pname    = Use 'pname' for the output directory prefix\n"
       "                       (default=dup)\n"
       "N.B.: Even if the new dataset is anatomical, it will not contain\n"
       "      any markers, duplicated from the original, or otherwise.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /** scan command line options **/

#define DUPERR(str) \
   do{ fprintf(stderr,"ERROR: %s\n",(str)) ; exit(1) ; } while(0)

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*** the following code is stolen from to3d ***/

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

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;
   }  /* end of loop over options */

   if( nopt >= argc ) DUPERR("no input dataset name given!") ;

   /*** read input dataset ***/

   dset_in = THD_open_one_dataset( argv[nopt] ) ;
   if( ! ISVALID_3DIM_DATASET(dset_in) ) DUPERR("cannot read dataset!\n") ;

   /*** copy header info ***/

   dset_out = EDIT_empty_copy( dset_in ) ;
   if( !ISVALID_3DIM_DATASET(dset_out) ) DUPERR("duplication fails!\n");

   EDIT_dset_items( dset_out ,
                      ADN_prefix         , DUP_prefix ,
                      ADN_label1         , DUP_prefix ,   /* label = prefix */
                      ADN_directory_name , DUP_session ,
                      ADN_self_name      , DUP_prefix ,
                    ADN_none ) ;

   tross_Copy_History( dset_in , dset_out ) ;
   tross_Make_History( "3ddup" , argc , argv , dset_out ) ;

   /*** change of type? ***/

   if( dataset_type>=FIRST_3DIM_TYPE && dataset_type<=LAST_3DIM_TYPE ){

      int isfunc , new_nvals , old_ntimes ;

      old_ntimes = DSET_NUM_TIMES(dset_in) ;

      isfunc    = ISFUNCTYPE(dataset_type) ;
      new_nvals = (isfunc) ? FUNC_nvals[function_type]
                           : ANAT_nvals[anatomy_type]  ;

      if( ( isfunc && function_type == FUNC_BUCK_TYPE) ||
          (!isfunc && anatomy_type  == ANAT_BUCK_TYPE)   )  /* 30 Nov 1997 */
         new_nvals = dset_in->dblk->nvals ;
      

      if( new_nvals > dset_in->dblk->nvals ){
         fprintf(stderr,
                 "ERROR: new dataset type has %d values per voxel,"
                 " but old has %d!\n"
                 "  ==> cannot make duplicate!\n" ,
                 new_nvals , dset_in->dblk->nvals ) ;
         exit(1) ;

      } else if( new_nvals < dset_in->dblk->nvals ){
         if( old_ntimes == 1 )
            fprintf(stderr,
                 "WARNING: new dataset type has %d values per voxel,"
                 " but old has %d!\n"
                 "  ==> new dataset will not access all data in old!\n",
                 new_nvals , dset_in->dblk->nvals ) ;
         else if( new_nvals > 1){
            fprintf(stderr,
                 "ERROR: new dataset type has %d values per voxel per time,"
                 " but old has %d!\n"
                 "  *** this conversion is not legal for time-dependent data!\n",
                 new_nvals , DSET_NVALS_PER_TIME(dset_in) ) ;
            exit(1) ;
         }
      }

      EDIT_dset_items( dset_out ,
                          ADN_type , dataset_type ,
                          ADN_func_type ,
                          ISANATTYPE(dataset_type) ? (anatomy_type)
                                                   : (function_type) ,
                       ADN_none ) ;
   }

   warp = myXtNew( THD_warp ) ; *warp = IDENTITY_WARP ;

   EDIT_dset_items( dset_out ,
                       ADN_warp        , warp    ,
                       ADN_warp_parent , dset_in ,
                    ADN_none ) ;

   /*** done! ***/

   THD_write_3dim_dataset( NULL , NULL , dset_out , False ) ;

   exit(0) ;
}
