/*------------------------------------------------------------------------
     ***  This program does something, but nobody is sure what.  ***
     ***  14 Apr 2003 - RWCox.                                   ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/
#undef DEBUG_ON

#define MATVEC_FOR 1
#define MATVEC_BAC 2

static THD_vecmat dicom_in2out , dicom_out2in ;  /* coordinate warps */
/*float compute_oblique_angle(mat44 ijk_to_dicom44);*/
static void Compute_Deoblique_Transformation(THD_3dim_dataset *dset,mat44 *Tw);

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
   char * prefix = "warped" ;
   int nopt=1 , verb=0 , zpad=0 , fsl=0 , gsetopt=0;
   int use_matvec=0 ;
   int use_newgrid=0 ;
   float ddd_newgrid=0.0 ;
   void *newggg=NULL ; int gflag=0 ;

   int tta2mni=0 , mni2tta=0 ;   /* 11 Mar 2004 */
   int matdefined=0 ;

   THD_coorder cord ;
   ATR_float *atr_matfor=NULL , *atr_matinv=NULL ;
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;
   mat44 Tw, Tc, Tw_inv, Tr, Tw2;
   int oblique_flag = 0;
   THD_3dim_dataset *oblparset=NULL ;
   float angle; 
   float matar[12] ; char anam[64] ;
#if 0
   MRI_IMAGE *matflim=NULL ;
   float     *matflar=NULL ;
#endif

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
            "  -matparent mset = Read in the matrix from WARPDRIVE_MATVEC_*\n"
            "                     attributes in the header of dataset 'mset',\n"
            "                     which must have been created by program\n"
            "                     3dWarpDrive.  In this way, you can apply\n"
            "                     a transformation matrix computed from\n"
            "                     in 3dWarpDrive to another dataset.\n\n"
            "     ** N.B.: The above option is analogous to the -rotparent\n"
            "                option in program 3drotate.  Use of -matparent\n"
            "                should be limited to datasets whose spatial\n"
            "                coordinate system corresponds to that which\n"
            "                was used for input to 3dWarpDrive (i.e., the\n"
            "                input to 3dWarp should overlay properly with\n"
            "                the input to 3dWarpDrive that generated the\n"
            "                -matparent dataset).\n\n"
  "  -card2oblique obl_dset or \n"
  "  -oblique_parent obl_dset = Read in the oblique transformation matrix\n"
  "     from an oblique dataset and make cardinal dataset oblique to match.\n"
  "  -deoblique or\n"
  "  -oblique2card = Transform an oblique dataset to a cardinal dataset\n"
  "     Both these oblique transformation options require a new grid for the\n"
  "     output as specified with the -newgrid or -gridset options\n"
  "     or a new grid will be assigned based on the minimum voxel spacing\n"
  "    ** N.B.: EPI time series data should be time shifted with 3dTshift before"
  "                rotating the volumes to a cardinal direction\n\n"
  "Sample usages:\n"
  " 3dWarpDrive -affine_general -base d1+orig -prefix d2WW -twopass"
    " -input d2+orig\n"
  " 3dWarp -matparent d2WW+orig -prefix epi2WW epi2+orig\n\n"
  " 3dWarp -card2oblique oblique_epi+orig -prefix oblique_anat card_anat+orig\n"
  " 3dWarp -oblique2card -prefix card_epi_tshift -newgrid 3.5 epi_tshift+orig\n"
  "\n"
  "Example of warping +tlrc results back to +orig space of some subject\n"
  "(get xform matrix, apply it, tell dataset it is not in orig space):\n"
  "\n"
  "    cat_matvec subj1_anat+tlrc::WARP_DATA > tlrc_xform.1D\n"
  "    3dWarp -matvec_out2in tlrc_xform.1D -prefix group_warped+tlrc \\\n"
  "           -gridset subj1_epi+orig -cubic group_data+tlrc\n"
  "    3drefit -view orig group_warped+tlrc\n"
#if 0
            "\n"
            "  -matfile mname  = Read in the file 'mname', which consists\n"
            "                     of 12 ASCII-formatted numbers per line.\n"
            "                     The i-th input line defines a 3x4 matrix\n"
            "                     which is used to transform the i-th\n"
            "                     sub-brick of the input dataset.\n"
#endif
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
            "  -verb         = Print out some information along the way.\n"
            "  -prefix ppp   = Sets the prefix of the output dataset.\n"
            "\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- startup mechanics --*/

   mainENTRY("3dWarp main"); machdep(); AFNI_logger("3dWarp",argc,argv);
   PRINT_VERSION("3dWarp") ; AUTHOR("RW Cox") ;

   THD_coorder_fill( my_getenv("AFNI_ORIENT") , &cord ) ;  /* 12 Mar 2004 */

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*----- 06 Jul 2005 -----*/

#if 0
     if( strncmp(argv[nopt],"-matfile",8) == 0 ){
       if( matdefined )
         ERROR_exit("-matfile: Matrix already defined!\n");
       if( ++nopt >= argc )
         ERROR_exit("Need argument after -matfile!\n");
       matflim = mri_read_ascii( argv[nopt] ) ;
       if( matflim == NULL )
         ERROR_exit("Can't read from -matfile %s\n",argv[nopt]);
       if( matflim->nx < 12 )
         ERROR_exit("Need 12 numbers per line in -matfile %s\n",argv[nopt]) ;

       matflar = MRI_FLOAT_PTR(matflim) ;
       matdefined = 1 ; nopt++ ; continue ;
     }
#endif

     /*-----*/

     if( strncmp(argv[nopt],"-matparent",10) == 0 ){
       ATR_float *atr ; float *matar ; THD_3dim_dataset *matparset=NULL ;

       if( matdefined )
         ERROR_exit("-matparent: Matrix already defined!\n");
       if( ++nopt >= argc )
         ERROR_exit("Need argument after -matparent!\n");
       matparset = THD_open_dataset( argv[nopt] ) ;
       if( matparset == NULL )
         ERROR_exit("Can't open -matparent %s\n",argv[nopt]);
       atr = THD_find_float_atr( matparset->dblk, 
                                 "WARPDRIVE_MATVEC_INV_000000" );
       if( atr != NULL ) 
         atr_matinv = (ATR_float *)THD_copy_atr( (ATR_any *)atr ) ;
       atr = THD_find_float_atr( matparset->dblk, 
                                 "WARPDRIVE_MATVEC_FOR_000000" );
       if( atr != NULL ) 
         atr_matfor = (ATR_float *)THD_copy_atr( (ATR_any *)atr ) ;
       if( atr_matinv == NULL ||  atr_matinv->nfl < 12 )
         ERROR_exit( "-matparent %s doesn't have WARPDRIVE attributes!?\n",
                     argv[nopt]) ;

       matar = atr_matinv->fl ;
       if( strstr(argv[nopt-1],"INV") == NULL ){
         LOAD_MAT  ( dicom_in2out.mm, matar[0],matar[1],matar[2],
                                      matar[4],matar[5],matar[6],
                                      matar[8],matar[9],matar[10] ) ;
         LOAD_FVEC3( dicom_in2out.vv, matar[3],matar[7],matar[11] ) ;
         dicom_out2in = INV_VECMAT( dicom_in2out ) ;
       } else {
         LOAD_MAT  ( dicom_out2in.mm, matar[0],matar[1],matar[2],
                                      matar[4],matar[5],matar[6],
                                      matar[8],matar[9],matar[10] ) ;
         LOAD_FVEC3( dicom_out2in.vv, matar[3],matar[7],matar[11] ) ;
         dicom_in2out = INV_VECMAT( dicom_out2in ) ;
       }

       DSET_delete(matparset) ;

       use_matvec = MATVEC_FOR ; matdefined = 1 ; nopt++ ; continue ;
     }

     /*----- 11 Mar 2004 -----*/

     if( strcmp(argv[nopt],"-tta2mni") == 0 ){
       if( matdefined )
         ERROR_exit("-tta2mni: Matrix already defined!\n");
       matdefined = 1 ; tta2mni = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mni2tta") == 0 ){
       if( matdefined )
         ERROR_exit("-mni2tta: Matrix already defined!\n");
       matdefined = 1 ; mni2tta = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-fsl_matvec") == 0 ){
       fsl = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-gridset") == 0 ){
       if( use_newgrid )
         ERROR_exit("Can't use -gridset with -newgrid!\n");
       if( newgset != NULL )
         ERROR_exit("Can't use -gridset twice!\n");
       if( ++nopt >= argc )
         ERROR_exit("Need argument after -gridset!\n");
       newgset = (THD_3dim_dataset *) -1; /* open the dataset later instead */
       gsetopt = nopt;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-zpad") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after -zpad!\n");
       zpad = (int) strtod( argv[nopt] , NULL ) ;
       if( zpad < 0 )
         ERROR_exit("Illegal negative argument after -zpad!\n");
       else if( zpad == 0 )
         INFO_message("NOTA BENE: -zpad is 0 ==> ignored\n") ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[nopt],"-newgrid") == 0 ){
       if( newgset != NULL )
         ERROR_exit("Can't use -newgrid with -gridset!\n");
       if( use_newgrid )
         ERROR_exit("Can't use -newgrid twice!\n");
       if( ++nopt >= argc )
         ERROR_exit("Need argument after -newgrid!\n");

       ddd_newgrid = strtod( argv[nopt] , NULL ) ;
       if( ddd_newgrid <= 0.0 )
         ERROR_exit("Illegal argument after -newgrid!\n");

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
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -prefix!\n");
       if( !THD_filename_ok(argv[nopt]) )
         ERROR_exit("-prefix argument is invalid!\n");

       prefix = argv[nopt] ; nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-verbose",5) == 0 ){
       verb++ ; nopt++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[nopt],"-matvec_",8) == 0 ){
       MRI_IMAGE *matim ; float *matar , dm ;

       if( matdefined )
         ERROR_exit("%s: matrix already defined!\n",argv[nopt]);
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after -matvec!\n");
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
         if( nn < 12 )
           ERROR_exit("Could only decode %d numbers from %s\n",nn,argv[nopt]) ;
       } else {
         matim = mri_read_ascii( argv[nopt] ) ;
         if( matim == NULL )
           ERROR_exit("Can't read -matvec file %s\n",argv[nopt]);
         if( matim->nx != 4 || matim->ny < 3 )
           ERROR_exit("-matvec file not a 3x4 matrix!\n");
         matar = MRI_FLOAT_PTR(matim) ;
       }

       use_matvec = (strstr(argv[nopt-1],"_in2out") != NULL) ? 
                        MATVEC_FOR : MATVEC_BAC ;

       switch( use_matvec ){

         case MATVEC_FOR:
           LOAD_MAT  ( dicom_in2out.mm, matar[0],matar[1],matar[2],
                                        matar[4],matar[5],matar[6],
                                        matar[8],matar[9],matar[10] ) ;
           LOAD_FVEC3( dicom_in2out.vv, matar[3],matar[7],matar[11] ) ;

           dm = MAT_DET( dicom_in2out.mm ) ;
           if( dm == 0.0 )
             ERROR_exit("Determinant of matrix is 0\n");

           dicom_out2in = INV_VECMAT( dicom_in2out ) ;
         break ;

         case MATVEC_BAC:
           LOAD_MAT  ( dicom_out2in.mm, matar[0],matar[1],matar[2],
                                        matar[4],matar[5],matar[6],
                                        matar[8],matar[9],matar[10] ) ;
           LOAD_FVEC3( dicom_out2in.vv, matar[3],matar[7],matar[11] ) ;

           dm = MAT_DET( dicom_out2in.mm ) ;
           if( dm == 0.0 )
             ERROR_exit("Determinant of matrix is 0\n");

           dicom_in2out = INV_VECMAT( dicom_out2in ) ;
         break ;
       }

       matdefined = 1 ; nopt++ ; continue ;
     }

     if((strncmp(argv[nopt],"-oblique_parent",15) == 0 ) || \
        (strncmp(argv[nopt],"-card2oblique",13) == 0 )) {
       if( matdefined )
         ERROR_exit("-oblique_parent: Matrix already defined!\n");
      THD_set_oblique_report(0,0); /* turn off obliquity warning */
      if( ++nopt >= argc )
         ERROR_exit("Need argument after -oblique_parent!\n");
       oblparset = THD_open_dataset( argv[nopt] ) ;
       if( oblparset == NULL )
         ERROR_exit("Can't open -oblique_parent %s\n",argv[nopt]);
       oblique_flag = 2;  matdefined = 1; nopt++ ; continue ;
     }

     if((strncmp(argv[nopt],"-deoblique",10) == 0 ) ||
       ( strncmp(argv[nopt],"-oblique2card",13) == 0 )){
        THD_set_oblique_report(0,0);
        oblique_flag = 1; matdefined = 1; nopt++ ; continue ;
     }

     /*-----*/

     ERROR_exit("Unknown option %s\n",argv[nopt]) ;

   } /* end of loop over options */

   /*-- check to see if we have a warp specified --*/

   if( !matdefined )
     ERROR_exit("No transformation on command line!?\n");

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
     ERROR_message("Command line should have exactly 1 dataset!") ;
     ERROR_exit   ("Whereas there seems to be %d of them!\n", argc-nopt ) ;
   }

   /*-- input dataset header --*/

   inset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(inset) )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]);

   #if 0 
   {
      /*- 25 Mar 2008: Deal with xforms that apply to i_in rather 
                       than x_in 
          WARNING: This code has not been tested for lack of data.
          Say the xform specified on command line is M_i such that
               x_out = M_i i_in +  Vector,
               where i_in is a voxel's IJK index, rather than a voxel's
               dicomm coordinate. 
               or 
               X_out = M_I I_in (using 4x4 xform matrix convention) 
          
          We want to calculate the transform M_X such that:
               X_out = M_X X_in , 
               where X_in is the voxel's dicomm coordinate. 
          
               So M_X X_in = M_I I_in
          
          In AFNI's dset structure,
               X_in = M_ijk I_in , 
               where M_ijk is daxes->ijk_to_dicom_real
          
          Now we have:
               M_X M_ijk I_in = M_I I_in
               or
               M_X = M_I inv(M_ijk)
          
          So if needed, the block below will replace
          dicom_in2out with M_X . But first we have
          to wait for some data.         
          */
      fprintf(stderr,"WARNING\nWARNING!\nWARNING!!\n");
      if(ISVALID_MAT44(inset->daxes->ijk_to_dicom_real)) {
         /* load dicom_in2out into Tw */
         LOAD_MAT44(Tw, 
          dicom_in2out.mm.mat[0][0], 
          dicom_in2out.mm.mat[0][1], 
          dicom_in2out.mm.mat[0][2], dicom_in2out.vv.xyz[0],
          dicom_in2out.mm.mat[1][0], 
          dicom_in2out.mm.mat[1][1], 
          dicom_in2out.mm.mat[1][2], dicom_in2out.vv.xyz[1],
          dicom_in2out.mm.mat[2][0], 
          dicom_in2out.mm.mat[2][1], 
          dicom_in2out.mm.mat[2][2], dicom_in2out.vv.xyz[2]);
         DUMP_MAT44("MI:\n", Tw);
         /* equivalent dicom transform is Tw*inv(ijk_to_dicom_real) */
         Tc = MAT44_INV(inset->daxes->ijk_to_dicom_real);
         DUMP_MAT44("inv(Mijk):\n", Tc);
         Tw2 = MAT44_MUL(Tw,Tc);
         DUMP_MAT44("MI*inv(Mijk):\n", Tw2);
         /* Now reload Tw2 into dicom_in2out */
         LOAD_MAT  ( dicom_in2out.mm, 
                     Tw2.m[0][0],Tw2.m[0][1],Tw2.m[0][2],
                     Tw2.m[1][0],Tw2.m[1][1],Tw2.m[1][2],
                     Tw2.m[2][0],Tw2.m[2][1],Tw2.m[2][2] ) ;
         LOAD_FVEC3( dicom_in2out.vv, 
                     Tw2.m[0][3],Tw2.m[1][3],Tw2.m[2][3] ) ;
         /* And recalculate the inverse of dicom_in2out */
         dicom_out2in = INV_VECMAT( dicom_in2out ) ;
      }
   }
   #endif
   /*- 11 Mar 2004: check for coherency -*/

   if( tta2mni || mni2tta ){
     if( inset->view_type != VIEW_TALAIRACH_TYPE ){
       WARNING_message("Input dataset not in +tlrc view!\n") ;
     } else {
       ATR_string *ast ;
       ast = THD_find_string_atr( inset->dblk , "TLRC_SPACE" ) ;
       if( ast != NULL ){
         if( strcmp(ast->ch,"MNI-152") == 0 && tta2mni ){
           WARNING_message("Input dataset appears to be MNI-152 already!\n");
         }
       }
     }
   }

   /*-- do the heavy lifting --*/

   if( use_newgrid ){
     newggg = &ddd_newgrid ; gflag = WARP3D_NEWGRID ;
   } else if( newgset != NULL ){
     newggg = newgset = THD_open_dataset( argv[gsetopt] ) ;
       if( newgset == NULL )
         ERROR_exit("Can't open -gridset %s\n",argv[nopt]);
     gflag = WARP3D_NEWDSET ;
   }

    /* handling oblique and deoblique cases */
   if(oblique_flag) {
      float *matar, dm;
      if(oblique_flag==1)   /* if deobliquing, everything is in the same dataset */
         oblparset = inset;
      Compute_Deoblique_Transformation(oblparset, &Tw);

      if(oblique_flag==1)   /* deoblique case*/
         matar = &Tw.m[0][0];
      else {               /* obliquing case */
         /* don't need oblique parent dataset anymore */
         DSET_delete(oblparset) ;
         Tw_inv = MAT44_INV(Tw);
         matar = &Tw_inv.m[0][0];
         Tw = Tw_inv;      /* just for the DUMP */
#ifdef DEBUG_ON
DUMP_MAT44("Tw_inv",Tw_inv);
#endif
	 if(ISVALID_MAT44(inset->daxes->ijk_to_dicom_real)) {
	   angle = THD_compute_oblique_angle(inset->daxes->ijk_to_dicom_real, 0);
	   if(angle>0.0) {
              INFO_message("Deobliquing original dataset before obliquing\n");
              INFO_message("  Combining oblique transformations");
              Compute_Deoblique_Transformation(inset, &Tw2);
              Tw = MAT44_MUL(Tw_inv, Tw2);
#ifdef DEBUG_ON
DUMP_MAT44("Twcombined", Tw);
#endif
              matar = &Tw.m[0][0];
           }
         }
      }

      /* show overall oblique/deoblique transformation */
      if(verb && oblique_flag)
         DUMP_MAT44("Obliquity Transformation :", Tw);

      LOAD_MAT  ( dicom_in2out.mm, matar[0],matar[1],matar[2],
                                   matar[4],matar[5],matar[6],
                                   matar[8],matar[9],matar[10] ) ;
      LOAD_FVEC3( dicom_in2out.vv, matar[3],matar[7],matar[11] ) ;

      dm = MAT_DET( dicom_in2out.mm ) ;
      if( dm == 0.0 )
        ERROR_exit("Determinant of matrix is 0\n");

      dicom_out2in = INV_VECMAT( dicom_in2out ) ;

      use_matvec = MATVEC_FOR ; matdefined = 1 ;
      if(newggg==NULL) {
         ddd_newgrid = DSET_MIN_DEL(inset);
         newggg = &ddd_newgrid ; gflag = WARP3D_NEWGRID ;
         INFO_message("Using minimum spacing of %f mm for new grid spacing",ddd_newgrid);
      }
    }
   /* if data is not being deobliqued or obliquified */
   if(!oblique_flag) {
     if(ISVALID_MAT44(inset->daxes->ijk_to_dicom_real)) {
       angle = THD_compute_oblique_angle(inset->daxes->ijk_to_dicom_real,1);
       if(angle>0.0) {  
         THD_dicom_card_xform(inset, &tmat, &tvec); 
         LOAD_MAT44(Tc, 
          tmat.mat[0][0], tmat.mat[0][1], tmat.mat[0][2], tvec.xyz[0],
          tmat.mat[1][0], tmat.mat[1][1], tmat.mat[1][2], tvec.xyz[1],
          tmat.mat[2][0], tmat.mat[2][1], tmat.mat[2][2], tvec.xyz[2]);
         Tr = MAT44_SUB(Tc,inset->daxes->ijk_to_dicom_real);
         if(MAT44_NORM(Tr)>0.001) {
            WARNING_message("Deoblique datasets with 3dWarp before proceeding"
               " with other transformations");
         }
       }
      }
   }

   if( use_matvec ){
     outset = THD_warp3D_affine( inset , dicom_out2in ,
                                 newggg , prefix , zpad , gflag ) ;
   } else if( tta2mni ){
     outset = THD_warp3D_tta2mni( inset , newggg , prefix,zpad,gflag ) ;
   } else if( mni2tta ){
     outset = THD_warp3D_mni2tta( inset , newggg , prefix,zpad,gflag ) ;
   }

   if( outset == NULL )
     ERROR_exit("THD_warp3D() fails for some reason!\n") ;

   /*-- polish up the new dataset info --*/
   /* if deobliquing, clear the oblique transformation matrix */
   /* really should update with new info, but clear for now */
   /* make invalid by setting lower right element to 0 */
#if 0
   if(oblique_flag==1) { 
     ZERO_MAT44(outset->daxes->ijk_to_dicom_real);
     outset->daxes->ijk_to_dicom_real.m[0][0] = 0.0;
   }
#endif

   if(oblique_flag) {
      /* recompute Tc (Cardinal transformation matrix for new grid output */
      THD_dicom_card_xform(outset, &tmat, &tvec); 
      LOAD_MAT44(Tc, 
          tmat.mat[0][0], tmat.mat[0][1], tmat.mat[0][2], tvec.xyz[0],
          tmat.mat[1][0], tmat.mat[1][1], tmat.mat[1][2], tvec.xyz[1],
          tmat.mat[2][0], tmat.mat[2][1], tmat.mat[2][2], tvec.xyz[2]);
      outset->daxes->ijk_to_dicom_real = Tc;
   }

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   /*- 11 Mar 2004: mark output dataset as being in MNI space -*/

   if( tta2mni ){
     THD_set_string_atr( outset->dblk , "TLRC_SPACE" , "MNI-152" ) ;
   }

   /*- 03 Aug 2005: save WARPDRIVE attributes, if present -*/

   if( atr_matfor != NULL ) {
      THD_insert_atr( outset->dblk, (ATR_any *)atr_matfor );
   } else if( use_matvec ){ /* put in the new matrix ZSS July 19 07*/
      UNLOAD_MAT(dicom_out2in.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(dicom_out2in.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_MATVEC_FOR_%06d",0) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;

   }
   if( atr_matinv != NULL ) {
      THD_insert_atr( outset->dblk, (ATR_any *)atr_matinv );
   } else if( use_matvec ){ /* put in the new matrix ZSS July 19 07*/
       UNLOAD_MAT(dicom_in2out.mm,matar[0],matar[1],matar[2],
                            matar[4],matar[5],matar[6],
                            matar[8],matar[9],matar[10] ) ;
       UNLOAD_FVEC3(dicom_in2out.vv,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"WARPDRIVE_MATVEC_INV_%06d",0) ;
       THD_set_float_atr( outset->dblk , anam , 12 , matar ) ;
   }
   /*- actually do the writositing -*/

   DSET_delete( inset ) ;
   DSET_write( outset ) ; if( verb ) WROTE_DSET(outset) ;
   exit(0) ;
}


#if 0
#define MAXNUM(a,b) ( (a) > (b) ? (a):(b))
#define MAX3(a,b,c) ( (MAXNUM(a,b)) > (MAXNUM(a,c)) ? (MAXNUM(a,b)):(MAXNUM(a,c)))
#define MINNUM(a,b) ( (a) < (b) ? (a):(b))
#define MIN3(a,b,c) ( (MINNUM(a,b)) < (MINNUM(a,c)) ? (MINNUM(a,b)):(MINNUM(a,c)))
/* compute angle of greatest obliquity given transformation matrix */
float compute_oblique_angle(mat44 ijk_to_dicom44)
{
   float dxtmp, dytmp, dztmp ;
   float xmax, ymax, zmax ;
   float fig_merit, ang_merit ;


   dxtmp = sqrt ( ijk_to_dicom44.m[0][0] * ijk_to_dicom44.m[0][0] +
                  ijk_to_dicom44.m[1][0] * ijk_to_dicom44.m[1][0] +
                  ijk_to_dicom44.m[2][0] * ijk_to_dicom44.m[2][0] ) ;

   xmax = MAX3(fabs(ijk_to_dicom44.m[0][0]),fabs(ijk_to_dicom44.m[1][0]),fabs(ijk_to_dicom44.m[2][0])) / dxtmp ;

   dytmp = sqrt ( ijk_to_dicom44.m[0][1] * ijk_to_dicom44.m[0][1] +
                  ijk_to_dicom44.m[1][1] * ijk_to_dicom44.m[1][1] +
                  ijk_to_dicom44.m[2][1] * ijk_to_dicom44.m[2][1] ) ;

   ymax = MAX3(fabs(ijk_to_dicom44.m[0][1]),
               fabs(ijk_to_dicom44.m[1][1]),
               fabs(ijk_to_dicom44.m[2][1])) / dytmp ;

   dztmp = sqrt ( ijk_to_dicom44.m[0][2] * ijk_to_dicom44.m[0][2] +
                  ijk_to_dicom44.m[1][2] * ijk_to_dicom44.m[1][2] +
                  ijk_to_dicom44.m[2][2] * ijk_to_dicom44.m[2][2] ) ;

   zmax = MAX3(fabs(ijk_to_dicom44.m[0][2]),
               fabs(ijk_to_dicom44.m[1][2]),
               fabs(ijk_to_dicom44.m[2][2])) / dztmp ;

   fig_merit = MIN3(xmax,ymax,zmax) ;
   ang_merit = acos (fig_merit) * 180.0 / 3.141592653 ;

   if (fabs(ang_merit) > .01) {
     INFO_message("%f degrees from plumb.\n",ang_merit ) ;
   }
   else 
      ang_merit = 0.0;
   return(ang_merit);
}
#endif


/* compute the transformation for deobliquing a dataset */
/* the IJK_TO_DICOM_REAL matrix must be stored in the dataset structure */
static void
Compute_Deoblique_Transformation(THD_3dim_dataset *dset, mat44 *Tw)
{
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;
   mat44 Tw_temp, Tc, inv_Tc, Tr;

   ENTRY("Compute_Deoblique_Transformation");
   if(!ISVALID_MAT44(dset->daxes->ijk_to_dicom_real)) {
      ERROR_exit("Oblique parent dataset doesn't have oblique "
                 "transformation attributes!?\n");
   }
   /* load oblique transformation matrix */
   Tr = dset->daxes->ijk_to_dicom_real;

   /* load DICOM (RAI) cardinal transformation matrix */
   THD_dicom_card_xform(dset, &tmat, &tvec); 
   LOAD_MAT44(Tc, tmat.mat[0][0], tmat.mat[0][1], tmat.mat[0][2], tvec.xyz[0],
                  tmat.mat[1][0], tmat.mat[1][1], tmat.mat[1][2], tvec.xyz[1],
		  tmat.mat[2][0], tmat.mat[2][1], tmat.mat[2][2], tvec.xyz[2]);
   inv_Tc = MAT44_INV(Tc);
   Tw_temp = MAT44_MUL(Tr, inv_Tc);
   *Tw = Tw_temp;

#ifdef DEBUG_ON
   DUMP_MAT44("Tr",Tr);
   DUMP_MAT44("Tc",Tc);
   DUMP_MAT44("Tw",Tw_temp);
#endif

   EXRETURN;
}
