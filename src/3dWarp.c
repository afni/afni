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
   THD_3dim_dataset *inset , *outset=NULL , *newgset=NULL ;
   int nxin,nyin,nzin,nxyzin,nvals , ival ;
   int nxout,nyout,nzout,nxyzout ;
   char * prefix = "warped" ;
   int nopt=1 , verb=0 , zpad=0 , fsl=0 ;
   int use_matvec=0 ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;
   int use_newgrid=0 ;
   float ddd_newgrid=0.0 , fac ;
   MRI_IMAGE *inim , *outim , *wim ;
   void *newggg=NULL ; int gflag=0 ;

   int tta2mni=0 , mni2tta=0 ;   /* 11 Mar 2004 */

   static THD_coorder cord ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
            "Usage: 3dWarp [options] dataset\n"
            "Warp (spatially transform) a 3D dataset.\n"
            "--------------------------\n"
            "Transform Defining Options: [exactly one of these must be used]\n"
            "--------------------------\n"
            "  -matvec_in2out mmm = Read a 3x4 affine transform matrix+vector\n"
            "                        from file 'mmm':\n"
            "                         x_out = Matrix x_in + Vector\n"
            "\n"
            "  -matvec_out2in mmm = Read a 3x4 affine transform matrix+vector\n"
            "                         from file 'mmm':\n"
            "                         x_in = Matrix x_out + Vector\n"
            "\n"
            "     ** N.B.: The coordinate vectors described above are\n"
            "               defined in DICOM ('RAI') coordinate order.\n"
            "               (Also see the '-fsl_matvec option, below.)\n"
            "     ** N.B.: Using the special name 'IDENTITY' for 'mmm'\n"
            "               means to use the identity matrix.\n"
            "     ** N.B.: You can put the matrix on the command line\n"
            "               directly by using an argument of the form\n"
            "       'MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)'\n"
            "               in place of 'mmm', where the aij values are the\n"
            "               matrix entries (aij = i-th row, j-th column),\n"
            "               separated by commas.\n"
            "             * You will need the 'forward single quotes' around\n"
            "               the argument.\n"
            "\n"
            "  -tta2mni = Transform a dataset in Talairach-Tournoux Atlas\n"
            "              coordinates to MNI-152 coordinates.\n"
            "  -mni2tta = Transform a dataset in MNI-152 coordinates to\n"
            "              Talairach-Tournoux Atlas coordinates.\n"
            "\n"
            "-----------------------\n"
            "Other Transform Options:\n"
            "-----------------------\n"
            "  -linear     }\n"
            "  -cubic      } = Chooses spatial interpolation method.\n"
            "  -NN         } =   [default = linear]\n"
            "  -quintic    }\n"
            "\n"
            "  -fsl_matvec   = Indicates that the matrix file 'mmm' uses FSL\n"
            "                    ordered coordinates ('LPI').  For use with\n"
            "                    matrix files from FSL and SPM.\n"
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

   THD_coorder_fill( my_getenv("AFNI_ORIENT") , &cord ) ;  /* 12 Mar 2004 */

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*----- 11 Mar 2004 -----*/

     if( strcmp(argv[nopt],"-tta2mni") == 0 ){
       if( mni2tta ){
         fprintf(stderr,"** Can't use -tta2mni with -mni2tta!\n"); exit(1);
       }
       if( use_matvec ){
         fprintf(stderr,"** Can't use -tta2mni with -matvec!\n");  exit(1);
       }
       tta2mni = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mni2tta") == 0 ){
       if( tta2mni ){
         fprintf(stderr,"** Can't use -mni2tta with -mni2tta!\n"); exit(1);
       }
       if( use_matvec ){
         fprintf(stderr,"** Can't use -mni2tta with -matvec!\n");  exit(1);
       }
       mni2tta = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-fsl_matvec") == 0 ){
       fsl = 1 ; nopt++ ; continue ;
     }

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
       if( !THD_filename_ok(argv[nopt]) ){
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

       if( mni2tta ){
         fprintf(stderr,"** Can't use -matvec with -mni2tta!\n"); exit(1);
       }
       if( tta2mni ){
         fprintf(stderr,"** Can't use -matvec with -tta2mni!\n"); exit(1);
       }
       if( use_matvec ){
         fprintf(stderr,"** Can't have two -matvec options!\n") ; exit(1);
       }
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -matvec!\n"); exit(1);
       }
       if( strcmp(argv[nopt],"IDENTITY") == 0 ){ /* load identity matrix */
         matim = mri_new( 4,3 , MRI_float ) ;    /* will be all zero */
         matar = MRI_FLOAT_PTR(matim) ;
         matar[0] = matar[5] = matar[10] = 1.0 ;
       } else if( strncmp(argv[nopt],"MATRIX(",7) == 0 ){ /* matrix from arg */
         int nn ;
         matim = mri_new( 4,3 , MRI_float ) ;    /* will be all zero */
         matar = MRI_FLOAT_PTR(matim) ;
         nn = sscanf(argv[nopt],"MATRIX(%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f",
                     matar+0 , matar+1 , matar+2 , matar+3 ,
                     matar+4 , matar+5 , matar+6 , matar+7 ,
                     matar+8 , matar+9 , matar+10, matar+11 ) ;
         if( nn < 12 ){
           fprintf(stderr,"** Could only decode %d numbers from %s\n",
                   nn,argv[nopt]) ;
           exit(1) ;
         }
       } else {
         matim = mri_read_ascii( argv[nopt] ) ;
         if( matim == NULL ){
           fprintf(stderr,"** Can't read -matvec file %s\n",argv[nopt]); exit(1);
         }
         if( matim->nx != 4 || matim->ny < 3 ){
           fprintf(stderr,"** -matvec file not 3x4!\n"); exit(1);
         }
         matar = MRI_FLOAT_PTR(matim) ;
       }

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

   if( !tta2mni && !mni2tta && !use_matvec ){
     fprintf(stderr,"** No transformation on command line!?\n"); exit(1);
   }

   if( tta2mni || mni2tta ) fsl = 0 ;

   /*-- 06 Aug 2003: if FSL matrix, flip stuff around --*/

   if( use_matvec && fsl ){
     dicom_in2out.mm.mat[0][2] = -dicom_in2out.mm.mat[0][2] ;
     dicom_in2out.mm.mat[1][2] = -dicom_in2out.mm.mat[1][2] ;
     dicom_in2out.mm.mat[2][0] = -dicom_in2out.mm.mat[2][0] ;
     dicom_in2out.mm.mat[2][1] = -dicom_in2out.mm.mat[2][1] ;
     dicom_in2out.vv.xyz[0]    = -dicom_in2out.vv.xyz[0]    ;
     dicom_in2out.vv.xyz[1]    = -dicom_in2out.vv.xyz[1]    ;

     dicom_out2in.mm.mat[0][2] = -dicom_out2in.mm.mat[0][2] ;
     dicom_out2in.mm.mat[1][2] = -dicom_out2in.mm.mat[1][2] ;
     dicom_out2in.mm.mat[2][0] = -dicom_out2in.mm.mat[2][0] ;
     dicom_out2in.mm.mat[2][1] = -dicom_out2in.mm.mat[2][1] ;
     dicom_out2in.vv.xyz[0]    = -dicom_out2in.vv.xyz[0]    ;
     dicom_out2in.vv.xyz[1]    = -dicom_out2in.vv.xyz[1]    ;
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

   /*- 11 Mar 2004: check for coherency -*/

   if( tta2mni || mni2tta ){
     if( inset->view_type != VIEW_TALAIRACH_TYPE ){
       fprintf(stderr,"++ WARNING: input dataset not in +tlrc view!\n") ;
     } else {
       ATR_string *ast ;
       ast = THD_find_string_atr( inset->dblk , "TLRC_SPACE" ) ;
       if( ast != NULL ){
         if( strcmp(ast->ch,"MNI-152") == 0 && tta2mni ){
           fprintf(stderr,"++ WARNING: input dataset appears to be MNI-152 already!\n");
         }
       }
     }
   }

   /*-- do the heavy lifting --*/

   if( use_newgrid ){
     newggg = &ddd_newgrid ; gflag = WARP3D_NEWGRID ;
   } else if( newgset != NULL ){
     newggg = newgset      ; gflag = WARP3D_NEWDSET ;
   }

   if( use_matvec ){
     outset = THD_warp3D_affine( inset , dicom_out2in ,
                                 newggg , prefix , zpad , gflag ) ;
   } else if( tta2mni ){
     outset = THD_warp3D_tta2mni( inset , newggg , prefix,zpad,gflag ) ;
   } else if( mni2tta ){
     outset = THD_warp3D_mni2tta( inset , newggg , prefix,zpad,gflag ) ;
   }

   if( outset == NULL ){
     fprintf(stderr,"** ERROR: THD_warp3D() fails for some reason!\n") ;
     exit(1) ;
   }

   /*-- polish up the new dataset info --*/

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   /*- 11 Mar 2004: mark output dataset as being in MNI space -*/

   if( tta2mni ){
     THD_set_string_atr( outset->dblk , "TLRC_SPACE" , "MNI-152" ) ;
   }

   if( verb ) fprintf(stderr,"\n++ Writing dataset\n") ;
   DSET_delete( inset ) ;
   DSET_write( outset ) ;
   exit(0) ;
}
