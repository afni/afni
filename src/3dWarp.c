/*------------------------------------------------------------------------
     ***  This program does something, but nobody is sure what.  ***
     ***  14 Apr 2003 - RWCox.                                   ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/

#define MATVEC_FOR 1
#define MATVEC_BAC 2

static THD_vecmat dicom_in2out , dicom_out2in ;  /* coordinate warps */

/*--------------------------------------------------------------------------*/

void warp_dicom_in2out( float  xin , float  yin , float  zin ,
                        float *xout, float *yout, float *zout )
{
   THD_fvec3 xxx ;
   LOAD_FVEC3( xxx , xin,yin,zin ) ;
   xxx = VECMAT_VEC( dicom_in2out , xxx ) ;
   UNLOAD_FVEC3( xxx , *xout , *yout , *zout ) ;
}

/*--------------------------------------------------------------------------*/

void warp_dicom_out2in( float  xin , float  yin , float  zin ,
                        float *xout, float *yout, float *zout )
{
   THD_fvec3 xxx ;
   LOAD_FVEC3( xxx , xin,yin,zin ) ;
   xxx = VECMAT_VEC( dicom_out2in , xxx ) ;
   UNLOAD_FVEC3( xxx , *xout , *yout , *zout ) ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset , *outset , *newgset=NULL ;
   int nxin,nyin,nzin,nxyzin,nvals , ival ;
   int nxout,nyout,nzout,nxyzout ;
   char * prefix = "warped" ;
   int nopt=1 , verb=0 , zpad=0 ;
   int use_matvec=0 ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;
   int use_newgrid=0 ;
   float ddd_newgrid=0.0 , fac ;
   MRI_IMAGE *inim , *outim , *wim ;
   void *newggg=NULL ; int gflag=0 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
            "Usage: 3dWarp [options] dataset\n"
            "Warp (spatially transform) a 3D dataset.\n"
            "--------------------------\n"
            "Transform Defining Options: [exactly one of these must be used]\n"
            "--------------------------\n"
            "  -matvec_in2out mmm  = Read a 3x4 affine transform matrix+vector\n"
            "                         from file 'mmm':\n"
            "                          x_out = Matrix x_in + Vector\n"
            "\n"
            "  -matvec_out2in mmm  = Read a 3x4 affine transform matrix+vector\n"
            "                          from file 'mmm':\n"
            "                          x_in = Matrix x_out + Vector\n"
            "\n"
            "              ** N.B.: The coordinate vectors described above are\n"
            "                       defined in DICOM ('RAI') coordinate order.\n"
            "-----------------------\n"
            "Other Transform Options:\n"
            "-----------------------\n"
            "  -linear     }\n"
            "  -cubic      } = Chooses spatial interpolation method.\n"
            "  -NN         }     [default = linear]\n"
            "  -quintic    }\n"
            "\n"
            "  -newgrid ddd  = Tells program to compute new dataset on a\n"
            "                    new 3D grid, with spacing of 'ddd' mmm.\n"
            "                  * If this option is given, then the new\n"
            "                    3D region of space covered by the grid\n"
            "                    is computed by warping the 8 corners of\n"
            "                    the input dataset, then laying down a\n"
            "                    regular grid with spacing 'ddd'.\n"
            "                  * If this option is NOT given, then the\n"
            "                    new dataset is computed on the old\n"
            "                    dataset's grid.\n"
            "\n"
            "  -gridset ggg  = Tells program to compute new dataset on the\n"
            "                    same grid as dataset 'ggg'.\n"
            "\n"
            "  -zpad N       = Tells program to pad input dataset with 'N'\n"
            "                    planes of zeros on all sides before doing\n"
            "                    transformation.\n"
            "---------------------\n"
            "Miscellaneous Options:\n"
            "---------------------\n"
#if 0
            "  -verb         = Print out some information along the way.\n"
#endif
            "  -prefix ppp   = Sets the prefix of the output dataset.\n"
            "\n"
           ) ;
     exit(0) ;
   }

   /*-- startup mechanics --*/

   mainENTRY("3dWarp main"); machdep(); AFNI_logger("3dWarp",argc,argv);

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*-----*/

     if( strcmp(argv[nopt],"-gridset") == 0 ){
       if( use_newgrid ){
         fprintf(stderr,"** Can't use -gridset with -newgrid!\n"); exit(1);
       }
       if( newgset != NULL ){
         fprintf(stderr,"** Can't use -gridset twice!\n"); exit(1);
       }
       if( ++nopt >= argc ){
         fprintf(stderr,"** Need argument after -gridset!\n"); exit(1);
       }
       newgset = THD_open_dataset( argv[nopt] ) ;
       if( newgset == NULL ){
         fprintf(stderr,"** Can't open -gridset %s\n",argv[nopt]); exit(1);
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-zpad") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** Need argument after -zpad!\n"); exit(1);
       }
       zpad = (int) strtod( argv[nopt] , NULL ) ;
       if( zpad < 0 ){
         fprintf(stderr,"** Illegal negative argument after -zpad!\n"); exit(1);
       } else if( zpad == 0 ){
         fprintf(stderr,"++ NOTA BENE: -zpad is 0 ==> ignored\n") ;
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-newgrid") == 0 ){
       if( newgset != NULL ){
         fprintf(stderr,"** Can't use -newgrid with -gridset!\n"); exit(1);
       }
       if( use_newgrid ){
         fprintf(stderr,"** Can't use -newgrid twice!\n"); exit(1);
       }
       if( ++nopt >= argc ){
         fprintf(stderr,"** Need argument after -newgrid!\n"); exit(1);
       }
       ddd_newgrid = strtod( argv[nopt] , NULL ) ;
       if( ddd_newgrid <= 0.0 ){
         fprintf(stderr,"** Illegal argument after -newgrid!\n"); exit(1);
       }
       use_newgrid = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-NN")     == 0 ){
       mri_warp3D_method( MRI_NN )     ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-linear") == 0 ){
       mri_warp3D_method( MRI_LINEAR ) ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-cubic")  == 0 ){
       mri_warp3D_method( MRI_CUBIC )  ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-quintic") == 0 ){
       mri_warp3D_method( MRI_QUINTIC ); nopt++ ; continue ;  /* 06 Aug 2003 */
     }

     /*-----*/

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -prefix!\n"); exit(1);
       }
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** ERROR: -prefix argument is invalid!\n"); exit(1);
       }
       prefix = argv[nopt] ; nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-verbose",5) == 0 ){
       verb++ ; nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-matvec_",8) == 0 ){
       MRI_IMAGE *matim ; float *matar , dm ;

       if( use_matvec ){
         fprintf(stderr,"** Can't have two -matvec options!\n"); exit(0);
       }
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -matvec!\n"); exit(1);
       }
       matim = mri_read_ascii( argv[nopt] ) ;
       if( matim == NULL ){
         fprintf(stderr,"** Can't read -matvec file %s\n",argv[nopt]); exit(1);
       }
       if( matim->nx != 4 || matim->ny != 3 ){
         fprintf(stderr,"** -matvec file not 3x4!\n"); exit(1);
       }

       matar = MRI_FLOAT_PTR(matim) ;
       use_matvec = (strstr(argv[nopt-1],"_in2out") != NULL) ? MATVEC_FOR : MATVEC_BAC ;

       switch( use_matvec ){

         case MATVEC_FOR:
           LOAD_MAT  ( dicom_in2out.mm, matar[0],matar[1],matar[2],
                                        matar[4],matar[5],matar[6],
                                        matar[8],matar[9],matar[10] ) ;
           LOAD_FVEC3( dicom_in2out.vv, matar[3],matar[7],matar[11] ) ;

           dm = MAT_DET( dicom_in2out.mm ) ;
           if( dm == 0.0 ){
             fprintf(stderr,"** Determinant of matrix is 0\n"); exit(1);
           }

           dicom_out2in = INV_VECMAT( dicom_in2out ) ;
         break ;

         case MATVEC_BAC:
           LOAD_MAT  ( dicom_out2in.mm, matar[0],matar[1],matar[2],
                                        matar[4],matar[5],matar[6],
                                        matar[8],matar[9],matar[10] ) ;
           LOAD_FVEC3( dicom_out2in.vv, matar[3],matar[7],matar[11] ) ;

           dm = MAT_DET( dicom_out2in.mm ) ;
           if( dm == 0.0 ){
             fprintf(stderr,"** Determinant of matrix is 0\n"); exit(1);
           }

           dicom_in2out = INV_VECMAT( dicom_out2in ) ;
         break ;
       }

       nopt++ ; continue ;
     }

     /*-----*/

     fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
     exit(1) ;

   } /* end of loop over options */

   /*-- check to see if we have a warp specified --*/

   if( !use_matvec ){
     fprintf(stderr,"** Don't you want to use -matvec?\n") ;
     exit(1) ;
   }

   /*-- 1 remaining argument should be a dataset --*/

   if( nopt != argc-1 ){
     fprintf(stderr,"** Command line should have exactly 1 dataset!\n"
                    "** Whereas there seems to be %d of them!\n",
             argc-nopt ) ;
     exit(1) ;
   }

   /*-- input dataset header --*/

   inset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(inset) ){
     fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]); exit(1);
   }

   /*-- do the heavy lifting --*/

   if( use_newgrid ){
     newggg = &ddd_newgrid ; gflag = WARP3D_NEWGRID ;
   } else if( newgset != NULL ){
     newggg = newgset      ; gflag = WARP3D_NEWDSET ;
   }

   if( !use_matvec ){
     outset = THD_warp3D( inset ,
                          warp_dicom_in2out ,
                          warp_dicom_out2in ,
                          newggg , prefix , zpad , gflag ) ;
   } else {
     outset = THD_warp3D_affine( inset , dicom_out2in ,
                                 newggg , prefix , zpad , gflag ) ;
   }

   if( outset == NULL ){
     fprintf(stderr,"** ERROR: THD_warp3D() fails for some reason!\n") ;
     exit(1) ;
   }

   /*-- polish up the new dataset info --*/

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   if( verb ) fprintf(stderr,"\n++ Writing dataset\n") ;
   DSET_delete( inset ) ;
   DSET_write( outset ) ;
   exit(0) ;
}
