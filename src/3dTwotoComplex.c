#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset1,*dset2=NULL , *mset=NULL , *oset ;
   MRI_IMAGE *dbr1,*dbr2;
   float      fac1, fac2;
   char *prefix = "cmplx" ;
   byte *mask=NULL ; float *rr,*gg ; complex *cmx ;
   int iarg=1 , dofim=0 , ii,nvox , mode=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage #1: 3dTwotoComplex [options] dataset\n"
             "Usage #2: 3dTwotoComplex [options] dataset1 dataset2\n"
             "\n"
             "Converts 2 sub-bricks of input to a complex-valued dataset.\n"
             "* If you have 1 input dataset, then sub-bricks [0..1] are\n"
             "    used to form the 2 components of the output.\n"
             "* If you have 2 input datasets, then the [0] sub-brick of\n"
             "    each is used to form the components.\n"
             "* Complex datasets have two 32-bit float components per voxel.\n"
             "\n"
             "Options:\n"
             "  -prefix ppp = Write output into dataset with prefix 'ppp'.\n"
             "                  [default='cmplx']\n"
             "  -RI         = The 2 inputs are real and imaginary parts.\n"
             "                  [this is the default]\n"
             "  -MP         = The 2 inputs are magnitude and phase.\n"
             "                  [phase is in radians, please!]\n"
             "  -mask mset  = Only output nonzero values where the mask\n"
             "                  dataset 'mset' is nonzero.\n"
             "Notes:\n"
             "* Input datasets must be byte-, short-, or float-valued.\n"
             "* You might calculate the component datasets using 3dcalc.\n"
             "* At present, there is limited support for complex datasets.\n"
             "    About the only thing you can do is display them in 2D\n"
             "    slice windows in AFNI.\n"
             "\n"
             "-- RWCox - March 2006\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dTwotoComplex main"); machdep();
   AFNI_logger("3dTwotoComplex",argc,argv);

   /*-- options --*/

#define GOOD_TYPE(tt) ((tt)==MRI_short || (tt)==MRI_byte || (tt)==MRI_float)

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) )
          ERROR_exit("-prefix %s is illegal!\n",prefix) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
        if( mset != NULL )
          ERROR_exit("can't have 2 -mask options!\n");
        mset = THD_open_dataset( argv[++iarg] ) ;
        if( !ISVALID_DSET(mset) )
          ERROR_exit("can't open -mset %s\n",argv[iarg]);
        if( !GOOD_TYPE( DSET_BRICK_TYPE(mset,0) ) )
          ERROR_exit("-mset %s has invalid data type\n",argv[iarg]);
        DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-RI") == 0 ){
        mode = 0 ; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-MP") == 0 ){
        mode = 1 ; iarg++ ; continue ;
      }

      ERROR_exit("ILLEGAL option: %s\n",argv[iarg]) ;
   }

   if( iarg >= argc )
     ERROR_exit("No datasets on command line!?\n");

   /*-- read input datasets --*/

#define OPENIT(ds,aa)                                                  \
 do{ ds = THD_open_dataset(aa) ;                                       \
     if( !ISVALID_DSET(ds) ) ERROR_exit("Can't open dataset %s\n",aa); \
     DSET_load(ds) ; CHECK_LOAD_ERROR(ds) ;                            \
 } while(0)

   if( iarg == argc-1 ){   /* one dataset */

     OPENIT(dset1,argv[iarg]) ;

     if( DSET_NVALS(dset1) < 2 )
       ERROR_exit("Dataset %s needs at least 2 sub-bricks!\n",argv[iarg]);
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) ||
         !GOOD_TYPE( DSET_BRICK_TYPE(dset1,1) )   )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg]);
     dbr1 = DSET_BRICK(dset1,0) ; fac1 = DSET_BRICK_FACTOR(dset1,0) ;
     dbr2 = DSET_BRICK(dset1,1) ; fac2 = DSET_BRICK_FACTOR(dset1,1) ;

   } else if( iarg+1 < argc ){  /* two datasets */

     OPENIT(dset1,argv[iarg])   ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg]) ;

     OPENIT(dset2,argv[iarg+1]) ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset2,0) ) )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg+1]);

     dbr1 = DSET_BRICK(dset1,0) ; fac1 = DSET_BRICK_FACTOR(dset1,0) ;
     dbr2 = DSET_BRICK(dset2,0) ; fac2 = DSET_BRICK_FACTOR(dset2,0) ;

     if( dbr1->nx != dbr2->nx ) ERROR_exit("Dataset x-axes don't match!\n") ;
     if( dbr1->ny != dbr2->ny ) ERROR_exit("Dataset y-axes don't match!\n") ;
     if( dbr1->nz != dbr2->nz ) ERROR_exit("Dataset z-axes don't match!\n") ;

     if( iarg+1 < argc-1 )
       WARNING_message("extra arguments on command line: %s ...\n",
                       argv[iarg+2] ) ;

   } else {                     /* this is bad */

     ERROR_exit("Need either 1 or 2 datasets on command line!?\n");
   }

   /* make a mask, if any */

   if( mset != NULL ){
     if( dbr1->nx != DSET_NX(mset) )
       ERROR_exit("Mask and dataset x-axes don't match!\n");
     if( dbr1->ny != DSET_NY(mset) )
       ERROR_exit("Mask and dataset y-axes don't match!\n");
     if( dbr1->nz != DSET_NZ(mset) )
       ERROR_exit("Mask and dataset z-axes don't match!\n");
     mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
     if( mask == NULL )
       ERROR_exit("Can't make mask!\n");
     DSET_unload(mset) ;
   }

   /* convert input sub-bricks to floats */

   nvox = dbr1->nvox ;

   rr   = (float *) calloc(sizeof(float),nvox) ;
   if( fac1 == 0.0 ) fac1 = 1.0 ;
   EDIT_coerce_scale_type( nvox , fac1 ,
                           dbr1->kind , mri_data_pointer(dbr1) ,
                           MRI_float  , rr                      ) ;

   DSET_unload(dset1) ;

   gg   = (float *) calloc(sizeof(float),nvox) ;
   if( fac2 == 0.0 ) fac2 = 1.0 ;
   EDIT_coerce_scale_type( nvox , fac2 ,
                           dbr2->kind , mri_data_pointer(dbr2) ,
                           MRI_float  , gg                      ) ;

   if( dset2 != NULL ) DSET_unload(dset2) ;

   /* merge inputs to output */

   cmx = (complex *) calloc(sizeof(complex),nvox) ;

#define T2C(a,b) \
  (mode==0) ? CMPLX((a),(b)) : CMPLX((a)*cos(b),(a)*sin(b))

   if( mask != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       if( mask[ii] ) cmx[ii] = T2C(rr[ii],gg[ii]) ;
   } else {
     for( ii=0 ; ii < nvox ; ii++ )
       cmx[ii] = T2C(rr[ii],gg[ii]) ;
   }

   free(mask) ; free(rr) ; free(gg) ;

   /* make output dataset */

   oset = EDIT_empty_copy( dset1 ) ;
   EDIT_dset_items( oset ,
                     ADN_prefix   , prefix      ,
                     ADN_datum_all, MRI_complex ,
                     ADN_nvals    , 1           ,
                     ADN_ntt      , 0           ,
                     ADN_type     , (dofim) ? HEAD_FUNC_TYPE : HEAD_ANAT_TYPE,
                     ADN_func_type, (dofim) ? FUNC_FIM_TYPE  : ANAT_SPGR_TYPE,
                    ADN_none ) ;
   EDIT_substitute_brick( oset , 0 , MRI_complex , cmx ) ;

   /* make history */

   tross_Copy_History( oset , dset1 ) ;
   tross_Make_History( "3dTwotoComplex", argc,argv, oset ) ;

   DSET_write( oset ) ;
   WROTE_DSET( oset ) ;
   exit(0) ;
}
