#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   MRI_IMAGE *imat=NULL ;
   float     *iar=NULL , *dval , *oval , sum ;
   int       nrow=0,ncol=0,nvals=0 , iarg , nvox=0,ivox , ii,jj ;
   char *prefix="matcalc" ;
   byte *mask=NULL ; int nmask=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dmatcalc [options]\n"
            "Apply a matrix to a dataset, voxel-by-voxel, to produce a new\n"
            "dataset.\n"
            "\n"
            "* If the input dataset has 'N' sub-bricks, and the input matrix\n"
            "   is 'MxN', then the output dataset will have 'M' sub-bricks; the\n"
            "   results in each voxel will be the result of extracting the N\n"
            "   values from the input at that voxel, multiplying the resulting\n"
            "   N-vector by the matrix, and output the resulting M-vector.\n"
            "\n"
            "* If the input matrix has 'N+1' columns, then it will be applied\n"
            "   to an (N+1)-vector whose first N elements are from the dataset\n"
            "   and the last value is 1.  This convention allows the addition\n"
            "   of a constant vector (the last row of the matrix) to each voxel.\n"
            "* The output dataset is always stored in float format.\n"
            "* Useful applications are left to your imagination.  The example\n"
            "   below is pretty fracking hopeless.  Something more useful might\n"
            "   be to project a 3D+time dataset onto some subspace, then run\n"
            "   3dpc on the results.\n"
            "\n"
            "\n"
            "OPTIONS:\n"
            "-------\n"
            " -input ddd  = read in dataset 'ddd'  [required option]\n"
            " -matrix eee = specify matrix, which can be done as a .1D file\n"
            "                or as an expression in the syntax of 1dmatcalc\n"
            "                [required option]\n"
            " -prefix ppp = write to dataset with prefix 'ppp'\n"
            " -mask mmm   = only apply to voxels in the mask; other voxels\n"
            "                will be set to all zeroes\n"
            "\n"
            "EXAMPLE:\n"
            "-------\n"
            "Assume dataset 'v+orig' has 50 sub-bricks:\n"
            " 3dmatcalc -input v+orig -matrix '&read(1D:50@1,\\,50@0.02) &transp' -prefix w\n"
            "The -matrix option computes a 2x50 matrix, whose first row is all 1's\n"
            "and whose second row is all 0.02's.  Thus, the output dataset w+orig has\n"
            "2 sub-bricks, the first of which is the voxel-wise sum of all 50 inputs,\n"
            "and the second is the voxel-wise average (since 0.02=1/50).\n"
            "\n"
            "-- Zhark, Emperor -- April 2006\n"
      ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dmatcalc main"); machdep(); AFNI_logger("3dmatcalc",argc,argv);
   PRINT_VERSION("3dmatcalc"); AUTHOR("Zhark");

   /** scan args **/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL ) ERROR_exit("Can't have two '-mask' options") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL ) ERROR_exit("Can't open mask dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       nmask = DSET_NVOX(mset) ;
       mask  = THD_makemask( mset , 0 , 1.0f,-1.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( nmask , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }


     if( strcmp(argv[iarg],"-prefix") == 0 ){
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-prefix'");
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal name after '-prefix'");
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL ) ERROR_exit("Can't use 2 '-input' options") ;
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-input'");
       inset = THD_open_dataset( argv[iarg] ) ;
       if( inset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       INFO_message("Loading dataset '%s'",argv[iarg]) ;
       DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
       nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-matrix") == 0 ){
       char *mop ;
       if( imat != NULL ) ERROR_exit("Can't use 2 '-matrix' options") ;
       iarg++ ; if( iarg >= argc ) ERROR_exit("Need argument after '-matrix'");
       if( strncmp(argv[iarg],"1D:",3) == 0 ||
           (strchr(argv[iarg],' ') == NULL && argv[iarg][0] != '&') ){
         imat = mri_read_1D( argv[iarg] ) ;
         if( imat == NULL ) ERROR_exit("Can't read matrix file '%s'",argv[iarg]);
         mop = "Read in" ;
       } else {
         imat = mri_matrix_evalrpn( argv[iarg] ) ;
         if( imat == NULL ) ERROR_exit("Can't evaluate matrix expression");
         mop = "Calculated" ;
       }
       nrow = imat->nx ; ncol = imat->ny ; iar = MRI_FLOAT_PTR(imat) ;
       INFO_message("%s %d X %d matrix\n",mop,nrow,ncol) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   }  /** end of scan args **/

   /** check for other errors **/

   if( inset == NULL ) ERROR_exit("No -input dataset?") ;
   if( imat  == NULL ) ERROR_exit("No -matrix option?") ;

   if( ncol < nvals || ncol > nvals+1 )
     ERROR_exit("Nonconforming matrix has %d columns, but should have %d or %d",
                ncol , nvals , nvals+1 ) ;

   if( nmask > 0 && nmask != nvox )
     ERROR_exit("-input and -mask datasets don't have same number of voxels") ;

   INFO_message("Creating %d sub-bricks with %d voxels each",nrow,nvox) ;

   /** create output dataset **/

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset ,
                      ADN_datum_all , MRI_float ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , nrow ,
                      ADN_ntt       , HAS_TIMEAXIS(inset) ? nrow : 0 ,
                    ADN_none ) ;

   if( THD_is_ondisk(DSET_HEADNAME(outset)) )
     ERROR_exit("Can't overwrite existing dataset '%s'",DSET_HEADNAME(outset));

   if( ISFUNC(outset) && ! ISFUNCBUCKET(outset) && outset->taxis != NULL )
     EDIT_dset_items( outset , ADN_func_type , FUNC_FIM_TYPE , ADN_none ) ;
   else if( ISANATBUCKET(outset) )
     EDIT_dset_items( outset , ADN_func_type , ANAT_EPI_TYPE , ADN_none ) ;

   THD_init_datablock_labels( outset->dblk ) ;
   THD_init_datablock_keywords( outset->dblk ) ;
   THD_init_datablock_stataux( outset->dblk ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dmatcalc", argc,argv, outset ) ;

   /* create sub-brick arrays (will be filled with zero) */

   for( ii=0 ; ii < nrow ; ii++ )
     EDIT_substitute_brick( outset , ii , MRI_float , NULL ) ;

   dval = (float *)malloc(sizeof(float)*(nvals+1)) ; dval[nvals] = 1.0f ;
   oval = (float *)malloc(sizeof(float)*nrow) ;

   /** actually do the work! **/

   INFO_message("Beginning calculations") ;

#define A(i,j) iar[(i)+(j)*nrow]

   for( ivox=0 ; ivox < nvox ; ivox++ ){
     if( mask != NULL && mask[ivox] == 0 ) continue ;  /* skip voxel */
     THD_extract_array( ivox , inset , 0 , dval ) ;
     for( ii=0 ; ii < nrow ; ii++ ){
       sum = 0.0f ;
       for( jj=0 ; jj < ncol ; jj++ ) sum += A(ii,jj) * dval[jj] ;
       oval[ii] = sum ;
     }
     THD_insert_series( ivox , outset , ncol , MRI_float , oval , 1 ) ;
   }

   /** save the work **/

   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
