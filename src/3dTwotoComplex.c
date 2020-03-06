#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset1=NULL,*dset2=NULL , *mset=NULL , *oset ;
   MRI_IMAGE *dbr1=NULL,*dbr2=NULL;
   float      fac1=0.0, fac2=0.0;
   char *prefix = "cmplx" ;
   byte *mask=NULL ; float *rr,*gg ; complex *cmx ;
   int iarg=1 , dofim=0 , ii,i2,nvox , mode=0 ;
   int ivol, nvol=0 ;  /* 16 Oct 2019 rickr */
   int ninput=0 ;      /* have 1 or 2 datasets as input */

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
      PRINT_COMPILE_DATE ; exit(0) ;
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

     nvol = DSET_NVALS(dset1) ;
     if( (nvol % 2) || nvol < 2 )
       ERROR_exit("Dataset %s needs an even number (>0) of sub-bricks!\n",
                  argv[iarg]);
     nvol /= 2; /* convert to the number of output volumes */

     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) ||
         !GOOD_TYPE( DSET_BRICK_TYPE(dset1,1) )   )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg]);

     /* use dset2 in any case, to generalize in loop */
     dset2 = dset1 ;

     /* only 1 input */
     ninput = 1 ;

   } else if( iarg+1 < argc ){  /* two datasets */

     OPENIT(dset1,argv[iarg])   ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg]) ;

     OPENIT(dset2,argv[iarg+1]) ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset2,0) ) )
       ERROR_exit("ILLEGAL dataset type in %s\n",argv[iarg+1]);

     nvol = DSET_NVALS(dset1) ;
     if( nvol != DSET_NVALS(dset2) )
       ERROR_exit("Datasets must have the same number of volumes");

     if( ! EQUIV_GRIDS(dset1, dset2) )
        ERROR_exit("Dataset grids do not match\n") ;

     if( iarg+1 < argc-1 )
       WARNING_message("extra arguments on command line: %s ...\n",
                       argv[iarg+2] ) ;

     /* have 2 inputs */
     ninput = 2 ;
   } else {                     /* this is bad */
     ERROR_exit("Need either 1 or 2 datasets on command line!?\n");
   }

   /* make a mask, if any */

   if( mset != NULL ){
     if( ! EQUIV_GRIDS_NXYZ(dset1, mset) )
       ERROR_exit("Mask and dataset nx/y/z don't match!\n");
     mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
     if( mask == NULL )
       ERROR_exit("Can't make mask!\n");
     DSET_unload(mset) ;
   }

   /* convert input sub-bricks to floats */

   nvox = DSET_NVOX(dset1) ;

   /* allocate computational memory */
   rr  = (float *) calloc(sizeof(float),nvox) ;
   gg  = (float *) calloc(sizeof(float),nvox) ;

   /* make output dataset */

   oset = EDIT_empty_copy( dset1 ) ;
   EDIT_dset_items( oset ,
                     ADN_prefix   , prefix      ,
                     ADN_datum_all, MRI_complex ,
                     ADN_ntt      , nvol        ,
                     ADN_type     , (dofim) ? HEAD_FUNC_TYPE : HEAD_ANAT_TYPE,
                     ADN_func_type, (dofim) ? FUNC_FIM_TYPE  : ANAT_SPGR_TYPE,
                    ADN_none ) ;

   for( ivol=0; ivol < nvol; ivol++ ) {

      /* choose input indices, and note that if ninput==1, dset2==dset1 */
      if( ninput == 2 ) {
         ii = ivol;
         i2 = ivol;
      } else {
         ii = 2*ivol;
         i2 = ii + 1;
      }

      /* set each data block and scale factor */
      dbr1 = DSET_BRICK(dset1,ii) ; fac1 = DSET_BRICK_FACTOR(dset1,ii) ;
      dbr2 = DSET_BRICK(dset2,i2) ; fac2 = DSET_BRICK_FACTOR(dset2,i2) ;

      /* explicitly clear memory, though it shouldn't really matter */
      memset(rr, '\0', nvox*sizeof(float));
      memset(gg, '\0', nvox*sizeof(float));

      /* convert to float */
      if( fac1 == 0.0 ) fac1 = 1.0 ;
      EDIT_coerce_scale_type( nvox , fac1 ,
                              DSET_BRICK_TYPE(dset1,ii), mri_data_pointer(dbr1),
                              MRI_float  , rr                      ) ;

      if( fac2 == 0.0 ) fac2 = 1.0 ;
      EDIT_coerce_scale_type( nvox , fac2 ,
                              DSET_BRICK_TYPE(dset2,i2), mri_data_pointer(dbr2),
                              MRI_float  , gg                      ) ;

      /* allocate space for the new volume, to be attached to new dset */
      cmx = (complex *) calloc(sizeof(complex),nvox) ;

#define T2C(a,b) \
  (mode==0) ? CMPLX((a),(b)) : CMPLX((a)*cos(b),(a)*sin(b))

      /* merge inputs to output */
      if( mask != NULL ){
        for( ii=0 ; ii < nvox ; ii++ )
          if( mask[ii] ) cmx[ii] = T2C(rr[ii],gg[ii]) ;
      } else {
        for( ii=0 ; ii < nvox ; ii++ )
          cmx[ii] = T2C(rr[ii],gg[ii]) ;
      }

      EDIT_substitute_brick( oset , ivol , MRI_complex , cmx ) ;
   }

   /* fly, be free! */
   DSET_unload(dset1) ;
   if( ninput == 2 ) DSET_unload(dset2) ;
   free(mask) ; free(rr) ; free(gg) ;

   /* make history */

   tross_Copy_History( oset , dset1 ) ;
   tross_Make_History( "3dTwotoComplex", argc,argv, oset ) ;

   DSET_write( oset ) ;
   WROTE_DSET( oset ) ;
   DSET_unload( oset ) ;

   exit(0) ;
}
