#include "mrilib.h"
#include "mri_threshX.c"

static int verb=1 ;

void mri_apply_mask( MRI_IMAGE *inim , MRI_IMAGE *bim ) ; /* prototype */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *mset=NULL , *iset=NULL ;
   char *prefix = "mthresh.nii" ;
   int nopt , ii , tindex=-1 , ival,nval,nvox ;
   int nnlev,nnsid,nzthr ; float *zthr=NULL ;
   MRI_IMAGE *fim , *bfim ; int nhits=0 , do_nozero=0 ;

   /*----- help, I'm trapped in an instance of vi and can't get out -----*/

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Program to apply a multi-threshold (mthresh) dataset\n"
      "to an input dataset.\n"
      "\n"
      "Usage:\n"
      "  3dMultiThresh OPTIONS\n"
      "\n"
      "OPTIONS (in any order)\n"
      "----------------------\n"
      "\n"
      " -mthresh mmm    = multi-threshold dataset from 3dClustSimX\n"
      " -input   ddd    = dataset to threshold\n"
      " -1tindex iii    = index (sub-brick) on which to threshold\n"
      " -prefix  ppp    = prefix for output dataset\n"
      "                   ++ Can be 'NULL' to get no output dataset\n"
      " -nozero         = this option prevents the output of a\n"
      "                   dataset if it would be all zero\n"
      " -quiet          = turn off progress report messages\n"
      "\n"
      "The number of surviving voxels will be written to stdout.\n"
      "It can be captured in a csh script by a command such as\n"
      "   set nhits = `3dMultiThresh [options]`\n"
     ) ;
     exit(0) ;
   }

   /*----- scan options -----*/

   mainENTRY("3dMultiThresh"); machdep();

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-nozero") == 0 ){
       do_nozero = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-quiet") == 0 ){
       verb = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("prefix '%s' is illegal (and funny looking)",prefix) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       if( iset != NULL )
         ERROR_exit("you can't use '-input' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       iset = THD_open_dataset(argv[nopt]) ;
       if( iset == NULL )
         ERROR_exit("can't open -input dataset '%s'",argv[nopt]) ;
       DSET_load(iset) ; CHECK_LOAD_ERROR(iset) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-1tindex") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       tindex = (int)strtod(argv[nopt],NULL) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-mthresh") == 0 ){
       ATR_float *atr ; float *afl ;
       if( mset != NULL )
         ERROR_exit("you can't use '-mthresh' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       mset = THD_open_dataset(argv[nopt]) ;
       if( mset == NULL )
         ERROR_exit("can't open -mthresh dataset '%s'",argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;

       atr = THD_find_float_atr( mset->dblk , "MULTI_THRESHOLDS" ) ;
       if( atr == NULL )
         ERROR_exit("-mthresh dataset does not have MULTI_THRESHOLDS attribute :(") ;
       afl = atr->fl ;

       nnlev = (int)afl[0] ;
       nnsid = (int)afl[1] ;
       nzthr = (int)afl[2] ;
       zthr  = (float *)malloc(sizeof(float)*nzthr) ;
       for( ii=0 ; ii < nzthr ; ii++ ) zthr[ii] = afl[3+ii] ;

       if( verb ){
           INFO_message("-mthresh dataset parameters") ;
         ININFO_message("  clustering NN=%d  thresholding=%d-sided  %d thresholds",
                        nnlev,nnsid,nzthr ) ;
       }

       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[nopt]) ;
   }

   /*-- check for errors --*/

   if( iset == NULL ) ERROR_exit("-input is a mandatory option") ;
   if( mset == NULL ) ERROR_exit("-mthresh is a mandatory option") ;

   /*----- do the work (oog) -----*/

   /* find the threshold sub-brick, if not forced upon us */

   nval = DSET_NVALS(iset) ;
   nvox = DSET_NVOX(iset) ;

   if( tindex < 0 || tindex >= nval ){
     for( ival=0 ; ival < nval ; ival++ ){
       if( DSET_BRICK_STATCODE(iset,ival) == FUNC_ZT_TYPE ) break ;
     }
     if( ival >= nval ){
       for( ival=0 ; ival < nval ; ival++ ){
         if( DSET_BRICK_STATCODE(iset,ival) == FUNC_TT_TYPE ) break ;
       }
     }
     tindex = (ival < nval) ? ival : 0 ;
     if( verb ) INFO_message("threshold index set to %d",ival) ;
   }


   /* get the threshold brick image */

   fim = THD_extract_float_brick( tindex , iset ) ;
   if( fim == NULL ) ERROR_exit("Can't get sub-brick %d to threshold",tindex) ;

   /* get the mask of values above the multi-thresholds */

   mri_multi_threshold_setup() ;

   bfim = mri_multi_threshold_Xcluster( fim ,
                                        nzthr , zthr , nnsid , nnlev ,
                                        mset->dblk->brick ,
                                        XTHRESH_OUTPUT_MASK , &nhits ) ;

   mri_multi_threshold_unsetup() ;

   mri_free(fim) ; DSET_unload(mset) ;

   /* nothing survived? */

   if( bfim == NULL && do_nozero ){ printf("0\n") ; exit(0) ; }

   /* apply the mask and produce the output dataset */

   if( strcmp(prefix,"NULL") != 0 ){
     THD_3dim_dataset *oset ;
     if( bfim == NULL )
       bfim = mri_new_conforming( fim , MRI_byte ) ; /* zero filled */
     oset = EDIT_full_copy(iset,prefix) ;
     DSET_unload(iset) ;
     tross_Copy_History( iset , oset ) ;
     tross_Make_History( "3dMultiThresh" , argc,argv , oset ) ;
     THD_copy_datablock_auxdata( iset->dblk , oset->dblk ) ;
     THD_copy_labeltable_atr(oset->dblk,iset->dblk);
     for( ival=0 ; ival < nval ; ival++ ){
       mri_apply_mask( DSET_BRICK(oset,ival) , bfim ) ;
     }
     DSET_write(oset) ;
     if( verb ) WROTE_DSET(oset) ;
     DSET_delete(oset) ;
   }

   mri_free(bfim) ;
   printf("%d\n",nhits) ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

void mri_apply_mask( MRI_IMAGE *inim , MRI_IMAGE *bim )
{
   byte *msk ; int ii,nvox ;

ENTRY("mri_apply_mask") ;

   if( inim == NULL || bim == NULL || bim->kind != MRI_byte ) EXRETURN ;

   msk = MRI_BYTE_PTR(bim) ;
   nvox = inim->nvox ; if( bim->nvox != nvox ) EXRETURN ;

   switch( inim->kind ){

     default: ERROR_message("Can't handle data of type %s",MRI_TYPE_name[inim->kind]) ;
              break ;

     case MRI_byte:{
       byte *iar = MRI_BYTE_PTR(inim) ;
       for( ii=0 ; ii < nvox ; ii++ ) if( msk[ii] == 0 ) iar[ii] = 0 ;
     }
     break ;

     case MRI_short:{
       short *iar = MRI_SHORT_PTR(inim) ;
       for( ii=0 ; ii < nvox ; ii++ ) if( msk[ii] == 0 ) iar[ii] = 0 ;
     }
     break ;

     case MRI_float:{
       float *iar = MRI_FLOAT_PTR(inim) ;
       for( ii=0 ; ii < nvox ; ii++ ) if( msk[ii] == 0 ) iar[ii] = 0.0f ;
     }
     break ;

   }

   EXRETURN ;
}
