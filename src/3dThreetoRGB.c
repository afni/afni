#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset1,*dset2=NULL,*dset3=NULL , *mset=NULL , *oset ;
   MRI_IMAGE *dbr1,*dbr2,*dbr3 ;
   float      fac1, fac2, fac3 ;
   char *prefix = "rgb" ;
   byte *mask=NULL , *rr,*gg,*bb , *rgb ;
   float fac=1.0 ;
   int iarg=1 , dofim=1 , ii,nvox ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage #1: 3dThreetoRGB [options] dataset\n"
             "Usage #2: 3dThreetoRGB [options] dataset1 dataset2 dataset3\n"
             "\n"
             "Converts 3 sub-bricks of input to an RGB-valued dataset.\n"
             "* If you have 1 input dataset, then sub-bricks [0..2] are\n"
             "   used to form the RGB components of the output.\n"
             "* If you have 3 input datasets, then the [0] sub-brick of\n"
             "   each is used to form the RGB components, respectively.\n"
             "* RGB datasets have 3 bytes per voxel, with values ranging\n"
             "   from 0..255.\n"
             "\n"
             "Options:\n"
             "  -prefix ppp = Write output into dataset with prefix 'ppp'.\n"
             "                 [default='rgb']\n"
             "  -scale fac  = Multiply input values by 'fac' before using\n"
             "                 as RGB [default=1].  If you have floating\n"
             "                 point inputs in range 0..1, then using\n"
             "                 '-scale 255' would make a lot of sense.\n"
             "  -mask mset  = Only output nonzero values where the mask\n"
             "                 dataset 'mset' is nonzero.\n"
             "  -fim        = Write result as a 'fim' type dataset.\n"
             "                 [this is the default]\n"
             "  -anat       = Write result as a anatomical type dataset.\n"
             "Notes:\n"
             "* Input datasets must be byte-, short-, or float-valued.\n"
             "* You might calculate the component datasets using 3dcalc.\n"
             "* You can also create RGB-valued datasets in to3d, using\n"
             "   2D raw PPM image files as input, or the 3Dr: format.\n"
             "* RGB fim overlays are transparent in AFNI in voxels where all\n"
             "   3 bytes are zero - that is, it won't overlay solid black.\n"
             "* At present, there is limited support for RGB datasets.\n"
             "   About the only thing you can do is display them in 2D\n"
             "   slice windows in AFNI.\n"
             "\n"
             "-- RWCox - April 2002\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dThreetoRGB main"); machdep(); AFNI_logger("3dThreetoRGB",argc,argv);

   /*-- options --*/

#define GOOD_TYPE(tt) ((tt)==MRI_short || (tt)==MRI_byte || (tt)==MRI_float)

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
           fprintf(stderr,"** -prefix %s is illegal!\n",prefix) ;
           exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-scale") == 0 ){
         fac = strtod( argv[++iarg] , NULL ) ;
         if( fac <= 0.0 ){
           fprintf(stderr,"** -scale %s is illegal!\n",argv[iarg]) ;
           exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-fim") == 0 ){
         dofim = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-anat") == 0 ){
         dofim = 0 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         if( mset != NULL ){
           fprintf(stderr,"** can't have 2 -mask options!\n"); exit(1);
         }
         mset = THD_open_dataset( argv[++iarg] ) ;
         if( !ISVALID_DSET(mset) ){
           fprintf(stderr,"** can't open -mset %s\n",argv[iarg]); exit(1);
         }
         if( !GOOD_TYPE( DSET_BRICK_TYPE(mset,0) ) ){
           fprintf(stderr,"** -mset %s has invalid data type\n",argv[iarg]);
           exit(1) ;
         }
         DSET_load(mset) ;
         if( !DSET_LOADED(mset) ){
           fprintf(stderr,"** can't load -mset %s\n",argv[iarg]); exit(1);
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   if( iarg >= argc ){
     fprintf(stderr,"** No datasets on command line!?\n"); exit(1);
   }

   /*-- read data --*/

#define OPENIT(ds,aa)                                               \
 do{                                                                \
     ds = THD_open_dataset(aa) ;                                    \
     if( !ISVALID_DSET(ds) ){                                       \
       fprintf(stderr,"** Can't open dataset %s\n",aa); exit(1);    \
     }                                                              \
     DSET_load(ds) ;                                                \
     if( !DSET_LOADED(ds) ){                                        \
       fprintf(stderr,"** Can't load dataset %s\n",aa); exit(1);    \
     }                                                              \
 } while(0)

   if( iarg == argc-1 ){   /* one dataset */

     OPENIT(dset1,argv[iarg]) ;

     if( DSET_NVALS(dset1) < 3 ){
       fprintf(stderr,"** Dataset %s needs at least 3 sub-bricks!\n",argv[iarg]);
       exit(1) ;
     }
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) ||
         !GOOD_TYPE( DSET_BRICK_TYPE(dset1,1) ) ||
         !GOOD_TYPE( DSET_BRICK_TYPE(dset1,2) )   ){
       fprintf(stderr,"** ILLEGAL dataset type in %s\n",argv[iarg]); exit(1);
     }
     dbr1 = DSET_BRICK(dset1,0) ; fac1 = DSET_BRICK_FACTOR(dset1,0) ;
     dbr2 = DSET_BRICK(dset1,1) ; fac2 = DSET_BRICK_FACTOR(dset1,1) ;
     dbr3 = DSET_BRICK(dset1,2) ; fac3 = DSET_BRICK_FACTOR(dset1,2) ;

   } else if( iarg+2 < argc ){  /* three datasets */

     OPENIT(dset1,argv[iarg])   ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) ){
       fprintf(stderr,"** ILLEGAL dataset type in %s\n",argv[iarg]) ; exit(1);
     }

     OPENIT(dset2,argv[iarg+1]) ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset2,0) ) ){
       fprintf(stderr,"** ILLEGAL dataset type in %s\n",argv[iarg+1]); exit(1);
     }

     OPENIT(dset3,argv[iarg+2]) ;
     if( !GOOD_TYPE( DSET_BRICK_TYPE(dset3,0) ) ){
       fprintf(stderr,"** ILLEGAL dataset type in %s\n",argv[iarg+2]); exit(1);
     }

     dbr1 = DSET_BRICK(dset1,0) ; fac1 = DSET_BRICK_FACTOR(dset1,0) ;
     dbr2 = DSET_BRICK(dset2,0) ; fac2 = DSET_BRICK_FACTOR(dset2,0) ;
     dbr3 = DSET_BRICK(dset3,0) ; fac3 = DSET_BRICK_FACTOR(dset3,0) ;

     if( dbr1->nx != dbr2->nx || dbr1->nx != dbr3->nx ){
       fprintf(stderr,"** Dataset x-axes don't match!\n") ; exit(1) ;
     }
     if( dbr1->ny != dbr2->ny || dbr1->ny != dbr3->ny ){
       fprintf(stderr,"** Dataset y-axes don't match!\n") ; exit(1) ;
     }
     if( dbr1->nz != dbr2->nz || dbr1->nz != dbr3->nz ){
       fprintf(stderr,"** Dataset z-axes don't match!\n") ; exit(1) ;
     }

     if( iarg+2 < argc-1 ){
       fprintf(stderr,"++ Warning: extra arguments on command line: %s ...\n",
               argv[iarg+3] ) ;
     }

   } else {                     /* this is bad */

     fprintf(stderr,"** Need either 1 or 3 datasets on command line!\n");
     exit(1) ;
   }

   /* make a mask, if any */

   if( mset != NULL ){
     if( dbr1->nx != DSET_NX(mset) ){
       fprintf(stderr,"** Mask and dataset x-axes don't match!\n"); exit(1);
     }
     if( dbr1->ny != DSET_NY(mset) ){
       fprintf(stderr,"** Mask and dataset y-axes don't match!\n"); exit(1);
     }
     if( dbr1->nz != DSET_NZ(mset) ){
       fprintf(stderr,"** Mask and dataset z-axes don't match!\n"); exit(1);
     }
     mask = THD_makemask( mset , 0 , 1.0,0.0 ) ;
     if( mask == NULL ){
       fprintf(stderr,"** Can't mask mask!\n"); exit(1);
     }
     DSET_unload(mset) ;
   }

   /* convert input sub-bricks to bytes */

   nvox = dbr1->nvox ;

   rr   = (byte *) calloc(1,nvox) ;
   if( fac1 == 0.0 ) fac1 = 1.0 ;
   EDIT_coerce_scale_type( nvox , fac*fac1 ,
                           dbr1->kind , mri_data_pointer(dbr1) ,
                           MRI_byte   , rr                      ) ;

   gg   = (byte *) calloc(1,nvox) ;
   if( fac2 == 0.0 ) fac2 = 1.0 ;
   EDIT_coerce_scale_type( nvox , fac*fac2 ,
                           dbr2->kind , mri_data_pointer(dbr2) ,
                           MRI_byte   , gg                      ) ;

   bb   = (byte *) calloc(1,nvox) ;
   if( fac3 == 0.0 ) fac3 = 1.0 ;
   EDIT_coerce_scale_type( nvox , fac*fac3 ,
                           dbr3->kind , mri_data_pointer(dbr3) ,
                           MRI_byte   , bb                      ) ;

                       DSET_unload(dset1) ;
   if( dset2 != NULL ) DSET_unload(dset2) ;
   if( dset3 != NULL ) DSET_unload(dset3) ;

   /* merge inputs to output */

   rgb = (byte *) calloc(1,3*nvox) ;

   if( mask != NULL ){
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mask[ii] ){
         rgb[3*ii  ] = rr[ii] ;
         rgb[3*ii+1] = gg[ii] ;
         rgb[3*ii+2] = bb[ii] ;
       }
     }
   } else {
     for( ii=0 ; ii < nvox ; ii++ ){
       rgb[3*ii  ] = rr[ii] ;
       rgb[3*ii+1] = gg[ii] ;
       rgb[3*ii+2] = bb[ii] ;
     }
   }

   free(mask) ; free(rr) ; free(gg) ; free(bb) ;

   /* make output dataset */

   oset = EDIT_empty_copy( dset1 ) ;
   EDIT_dset_items( oset ,
                     ADN_prefix   , prefix   ,
                     ADN_datum_all, MRI_rgb  ,
                     ADN_nvals    , 1        ,
                     ADN_ntt      , 0        ,
                     ADN_type     , (dofim) ? HEAD_FUNC_TYPE : HEAD_ANAT_TYPE,
                     ADN_func_type, (dofim) ? FUNC_FIM_TYPE  : ANAT_SPGR_TYPE,
                    ADN_none ) ;
   EDIT_substitute_brick( oset , 0 , MRI_rgb , rgb ) ;

   /* make history */

   tross_Copy_History( oset , dset1 ) ;
   tross_Make_History( "3dThreetoRGB", argc,argv, oset ) ;

   DSET_write( oset ) ;
   exit(0) ;
}
