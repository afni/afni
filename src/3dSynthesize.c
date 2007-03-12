#include "mrilib.h"

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   NI_element *nelmat=NULL ;
   int nrow,ncol , iv , nxyz , ii,jj,kk , iarg ;
   char *prefix  = "Synthesize" ;
   int   nselect = 0 ;
   char **select = NULL ;
   char *cdt , *cgrp , *clab , *ccc ;
   float  dt=0.0f ;
   NI_str_array *clab_sar , *cgrp_sar ;
   int                      *cgrp_val ;
   int *ilist, nadd , nilist , ll ;
   float **clist , *tsar , *cfar ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
       "Usage: 3dSynthesize options\n"
       "Reads a '-cbucket' dataset and a '.x1D' matrix from 3dDeconvolve,\n"
       "and synthesizes a fit dataset using selected sub-bricks and\n"
       "matrix columns.\n"
       "\n"
       "Options (actually, these are mandatory)\n"
       "---------------------------------------\n"
       " -cbucket ccc  = Read the dataset 'ccc', which should have been\n"
       "                 output from 3dDeconvolve via the '-cbucket' option.\n"
       " -matrix mmm   = Read the matrix 'mmm', which should have been\n"
       "                 output from 3dDeconvolve via the '-x1D' option.\n"
       " -prefix ppp   = Output result into dataset with name 'ppp'.\n"
       " -select sss   = Selects specific columns from the matrix (and the\n"
       "                 corresponding coefficient sub-bricks from the\n"
       "                 cbucket).  The string 'sss' can be of the forms:\n"
       "                   StimLabel = all columns/coefficients that match\n"
       "                               this -stim_label from 3dDeconvolve\n"
       "                   baseline  = all baseline coefficients\n"
       "                   polort    = all polynomial baseline coefficients\n"
       "                               (skipping -stim_base coefficients)\n"
       "                 More than one '-select sss' option can be used.\n"
       "\n"
       "-- Zhark the All Powerful (when his Prozac is working) -- March 2007\n"
      ) ;
      exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dSynthesize main"); machdep(); AFNI_logger("3dSynthesize",argc,argv);
   PRINT_VERSION("3dSynthesize") ; AUTHOR("RW Cox") ;

   /** parse options **/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /** output dataset prefix **/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) ) ERROR_exit("-prefix is not good!");
        iarg++ ; continue ;
      }

      /** -cbucket **/

      if( strcmp(argv[iarg],"-cbucket") == 0 ){
        if( inset != NULL ) ERROR_exit("More than 1 -cbucket option!");
        inset = THD_open_dataset(argv[++iarg]) ;
        if( inset == NULL ) ERROR_exit("Can't open -cbucket dataset!");
        DSET_load(inset) ;
        if( !DSET_LOADED(inset) ) ERROR_exit("Can't load -cbucket dataset!");
        iarg++ ; continue ;
      }

      /** -matrix **/

      if( strcmp(argv[iarg],"-matrix") == 0 ){
        if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!");
        nelmat = NI_read_element_fromfile( argv[++iarg] ) ;
        if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
          ERROR_exit("Can't process -matrix file!");
        iarg++ ; continue ;
      }

      /** -select **/

      if( strcmp(argv[iarg],"-select") == 0 ){
        for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
          select = (char **)realloc(select,sizeof(char *)*(nselect+1)) ;
          select[nselect++] = strdup(argv[iarg]) ;
        }
        continue ;
      }

      /** bozo-ific user **/

      ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   if( nelmat == NULL || inset == NULL || nselect == 0 || select == NULL )
     ERROR_exit("Missing -matrix and/or -inset and/or -select options!") ;

   /*-- look at matrix, get it's pieces --*/

   ncol = DSET_NVALS(inset) ;
   nrow = nelmat->vec_len ;
   if( ncol != nelmat->vec_num )
     ERROR_exit("-matrix has %d columns but -cbucket has %d sub-bricks!?",
                nelmat->vec_num , ncol ) ;

   clab = NI_get_attribute( nelmat , "ColumnLabels" ) ;
   if( clab == NULL )
     ERROR_exit("-matrix is missing 'ColumnLabels' attribute!?") ;
   clab_sar = NI_decode_string_list( clab , ";" ) ;
   if( clab_sar == NULL || clab_sar->num < ncol )
     ERROR_exit("-matrix 'ColumnLabels' badly formatted!?") ;

   cgrp = NI_get_attribute( nelmat , "ColumnGroups" ) ;
   if( cgrp == NULL )
     ERROR_exit("-matrix is missing 'ColumnGroups' attribute!?") ;
   cgrp_sar = NI_decode_string_list( cgrp , ";" ) ;
   if( cgrp_sar == NULL || cgrp_sar->num < ncol )
     ERROR_exit("-matrix 'ColumnGroups' badly formatted!?") ;
   cgrp_val = (int *)malloc(sizeof(int)*ncol) ;
   for( ii=0 ; ii < ncol ; ii++ )
     cgrp_val[ii] = (int)strtod(cgrp_sar->str[ii],NULL) ;

   cdt = NI_get_attribute( nelmat , "RowTR" ) ;
   if( cdt != NULL ) dt = (float)strtod(cdt,NULL) ;
   if( dt <= 0.0f ){
     dt = 1.0f ; /*** WARNING_message("Using default TR = 1.0") ; ***/
   }

   clist = (float **)malloc(sizeof(float *)*ncol) ;
   for( ii=0 ; ii < ncol ; ii++ ) clist[ii] = nelmat->vec[ii] ;

   /*-- process the -select options to build a column list --*/

   ilist = (int *)calloc(sizeof(int),ncol) ;
   for( kk=0 ; kk < nselect ; kk++ ){
     nadd = 0 ;
     if( strcasecmp(select[kk],"baseline") == 0 ){  /* all baselines */
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] <= 0 ){ ilist[ii]++ ; nadd++ ; }
     } else if( strcasecmp(select[kk],"polort") == 0 ){ /* polort baselines */
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] < 0 ){ ilist[ii]++ ; nadd++ ; }
     } else if( strcasecmp(select[kk],"allfunc") == 0 ){ /* non-baselines */
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] > 0 ){ ilist[ii]++ ; nadd++ ; }
     } else if( strcasecmp(select[kk],"allstim") == 0 ){ /* non-polorts */
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] >= 0 ){ ilist[ii]++ ;
     } else if( strcasecmp(select[kk],"all") == 0 ){  /* all */
       for( ii=0 ; ii < ncol ; ii++ ) ilist[ii]++ ; nadd++ ; }

     } else {           /*--- a stim label --*/
       for( ii=0 ; ii < ncol ; ii++ )
         if( strstr(clab_sar->str[ii],select[kk]) == clab_sar->str[ii] ){
           ilist[ii]++ ; nadd++ ;
         }
     }

     if( nadd == 0 )
       WARNING_message("-select '%s' didn't match any matrix columns!",
                       select[kk]) ;
   }

   for( nilist=ii=0 ; ii < ncol ; ii++ ) if( ilist[ii] > 0 ) nilist++ ;

   if( nilist == 0 )
     ERROR_exit("No columns selected for dataset synthesis!") ;

   /*-- create empty output 3D+time dataset --*/

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , MRI_float ,
                      ADN_nvals     , nrow ,
                      ADN_ntt       , nrow ,
                      ADN_ttdel     , dt ,
                      ADN_tunits    , UNITS_SEC_TYPE ,
                    ADN_none ) ;

   if( THD_is_file(DSET_HEADNAME(outset)) )
     ERROR_exit("Output dataset already exists: %s\n",DSET_HEADNAME(outset)) ;

   tross_Copy_History( outset , inset ) ;
   tross_Make_History( "3dSynthesize" , argc , argv , outset ) ;

   nxyz = DSET_NVOX(inset) ;

   /* create bricks (will be filled with zeros) */

   for( iv=0 ; iv < nrow ; iv++ )
     EDIT_substitute_brick( outset , iv , MRI_float , NULL ) ;

   tsar = (float *)calloc(sizeof(float),nrow+1) ;
   cfar = (float *)calloc(sizeof(float),ncol+1) ;

   for( kk=0 ; kk < nxyz ; kk++ ){   /* kk = voxel index */

      /* get kk-th coefficient array into cfar */

      (void)THD_extract_array( kk , inset , 0 , cfar ) ;

      for( jj=0 ; jj < nrow ; jj++ ){
        tsar[jj] = 0.0f ;
        for( ii=0 ; ii < ncol ; ii++ )
          if( ilist[ii] ) tsar[jj] += cfar[ii]*clist[ii][jj] ;
      }

      /* put result into output dataset */

      THD_insert_series( kk , outset ,
                         ncol , MRI_float , tsar , 1 ) ;

   }

   DSET_delete(inset) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
