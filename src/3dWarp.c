/*------------------------------------------------------------------------
     ***  This program does something, but nobody is sure what.  ***
     ***  14 Apr 2003 - RWCox.                                   ***
--------------------------------------------------------------------------*/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/

#define MATVEC_FOR 1
#define MATVEC_BAC 2

static THD_vecmat matvec_warp ;
static THD_vecmat dicom_in2out , dicom_out2in ;
static THD_vecmat ijk_to_dicom_in , ijk_to_dicom_out ;

static void warp_func( float xi,float yi,float zi, float *xo,float *yo,float *zo )
{
   *xo = matvec_warp.mm.mat[0][0] * xi + matvec_warp.mm.mat[0][1] * yi +
         matvec_warp.mm.mat[0][2] * zi + matvec_warp.vv.xyz[0]          ;

   *yo = matvec_warp.mm.mat[1][0] * xi + matvec_warp.mm.mat[1][1] * yi +
         matvec_warp.mm.mat[1][2] * zi + matvec_warp.vv.xyz[1]          ;

   *zo = matvec_warp.mm.mat[2][0] * xi + matvec_warp.mm.mat[2][1] * yi +
         matvec_warp.mm.mat[2][2] * zi + matvec_warp.vv.xyz[2]          ;
}

/*--------------------------------------------------------------------------*/
/* Find the 8 corners of the input dataset (voxel edges, not centers).
   Warp each one.
   Return the min and max (x,y,z) coordinates of these warped points.
----------------------------------------------------------------------------*/

static void warp_corners( THD_3dim_dataset *inset,
                          float *xb , float *xt ,
                          float *yb , float *yt , float *zb , float *zt )
{
   THD_dataxes *daxes = inset->daxes ;
   THD_fvec3 corner , wcorn ;
   float nx0 = -0.5          , ny0 = -0.5          , nz0 = -0.5           ;
   float nx1 = daxes->nxx-0.5, ny1 = daxes->nyy-0.5, nz1 = daxes->nzz-0.5 ;
   float xx,yy,zz , xbot,ybot,zbot , xtop,ytop,ztop ;

   LOAD_FVEC3( corner , nx0,ny0,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = xtop = xx ;
   ybot = ytop = yy ;
   zbot = ztop = zz ;

   LOAD_FVEC3( corner , nx1,ny0,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny0,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny0,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   warp_func( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   *xb = xbot; *xt = xtop;
   *yb = ybot; *yt = ytop; *zb = zbot; *zt = ztop; return;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset , *outset ;
   int nxin,nyin,nzin,nxyzin,nvals , kk ;
   int nxout,nyout,nzout,nxyzout ;
   char * prefix = "warped" ;
   int nopt=1 , verb=0 ;
   int use_matvec=0 ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;
   int use_newgrid=0 ;
   float ddd_newgrid=0.0 ;

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
            "---------------------\n"
            "Miscellaneous Options:\n"
            "---------------------\n"
            "  -verb         = Print out some information along the way.\n"
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

     if( strcmp(argv[nopt],"-newgrid") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** Need argument after -newgrid!\n"); exit(1);
       }
       if( use_newgrid ){
         fprintf(stderr,"** Can't use two -newgrid arguments!\n"); exit(1);
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

     /*-----*/

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ){
         fprintf(stderr,"** ERROR: need an argument after -prefix!\n"); exit(1);
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

#if 0
     if( strncmp(argv[nopt],"-scale",6) == 0 ){
       if( use_scale ){
         fprintf(stderr,"** Can't have two -scale options!\n"); exit(0);
       }
       if( use_matvec ){
         fprintf(stderr,"** Can't have -scale AND -matvec options!\n"); exit(0);
       }
       if( nopt+3 >= argc ){
         fprintf(stderr,"** Need 3 arguments after -scale!\n"); exit(0);
       }
       scale_a = strtod( argv[++nopt] , NULL ) ;
       scale_b = strtod( argv[++nopt] , NULL ) ;
       scale_c = strtod( argv[++nopt] , NULL ) ;
       if( scale_a <= 0.0 ){
         fprintf(stderr,"** First argument after -scale is illegal!\n");
       }
       if( scale_b <= 0.0 ){
         fprintf(stderr,"** Second argument after -scale is illegal!\n");
       }
       if( scale_c <= 0.0 ){
         fprintf(stderr,"** Third argument after -scale is illegal!\n");
       }
       if( scale_a <= 0.0 || scale_b <= 0.0 || scale_c <= 0.0 ) exit(1) ;
       if( scale_a == 1.0 && scale_b == 1.0 && scale_c == 1.0 ){
         fprintf(stderr,"** Can't have all scale factors = 1!\n"); exit(1);
       }
       use_scale = 1 ; nopt++ ; continue ;
     }
#endif

     fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
     exit(1) ;

   } /* end of loop over options */

   /* check to see if we have a warp specified */

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
      fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]) ;
      exit(1) ;
   }

   /*-- compute mapping from input dataset (i,j,k) to DICOM coords --*/

   { THD_vecmat ijk_to_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_xyz.mm , inset->daxes->xxdel,
                                    inset->daxes->yydel, inset->daxes->zzdel );
     LOAD_FVEC3   ( ijk_to_xyz.vv , inset->daxes->xxorg,
                                    inset->daxes->yyorg, inset->daxes->zzorg );

     xyz_to_dicom.mm = inset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_dicom_in = MUL_VECMAT( xyz_to_dicom , ijk_to_xyz ) ;
   }

   /*-- make empty output dataset --*/

   nxin  = DSET_NX(inset) ;
   nyin  = DSET_NY(inset) ;
   nzin  = DSET_NZ(inset) ; nxyzin = nxin*nyin*nzin;
   nvals = DSET_NVALS(inset) ;

   if( !use_newgrid ){                   /* output is on same grid as input */

     outset = EDIT_empty_copy( inset ) ;
     nxout = nxin ; nyout = nyin ; nzout = nzin ; nxyzout = nxyzin ;

   } else {                              /* output is on new grid */

     float xmid,ymid,zmid ;
     THD_ivec3 nxyz , orixyz ;
     THD_fvec3 dxyz , orgxyz ;
     int *datum ;

     /* compute DICOM coordinates of warped corners */

     matvec_warp = dicom_in2out ;
     warp_corners( inset , &xbot,&xtop , &ybot,&ytop , &zbot,&ztop ) ;

     nxout = (int)( (xtop-xbot)/ddd_newgrid+0.999 ); if( nxout < 1 ) nxout = 1;
     nyout = (int)( (ytop-ybot)/ddd_newgrid+0.999 ); if( nyout < 1 ) nyout = 1;
     nzout = (int)( (ztop-zbot)/ddd_newgrid+0.999 ); if( nzout < 1 ) nzout = 1;
     nxyzout = nxout*nyout*nzout ;

     xmid = 0.5*(xbot+xtop) ; ymid = 0.5*(ybot+ytop) ; zmid = 0.5*(zbot+ztop) ;
     xbot = xmid - 0.5*nxout*ddd_newgrid ; xtop = xbot + (nxout-1)*ddd_newgrid ;
     ybot = ymid - 0.5*nyout*ddd_newgrid ; ytop = ybot + (nyout-1)*ddd_newgrid ;
     zbot = zmid - 0.5*nzout*ddd_newgrid ; ztop = zbot + (nzout-1)*ddd_newgrid ;

     fprintf(stderr,"++ Transformed grid:\n"
                    "++   xbot = %10.4g  xtop = %10.4g  nx = %d\n"
                    "++   ybot = %10.4g  ytop = %10.4g  ny = %d\n"
                    "++   zbot = %10.4g  ztop = %10.4g  nz = %d\n" ,
             xbot,xtop,nxout , ybot,ytop,nyout , zbot,ztop,nzout    ) ;

     if( nxyzout == 1 ){
       fprintf(stderr,"** Transformed dataset is too small!\n"); exit(1);
     }

     nxyz.ijk[0] = nxout ; dxyz.xyz[0] = ddd_newgrid ;  /* setup axes */
     nxyz.ijk[1] = nyout ; dxyz.xyz[1] = ddd_newgrid ;
     nxyz.ijk[2] = nzout ; dxyz.xyz[2] = ddd_newgrid ;

     orixyz.ijk[0] = ORI_R2L_TYPE ; orgxyz.xyz[0] = xbot ;
     orixyz.ijk[1] = ORI_A2P_TYPE ; orgxyz.xyz[1] = ybot ;
     orixyz.ijk[2] = ORI_I2S_TYPE ; orgxyz.xyz[2] = zbot ;

     /** create dataset and mangle it into the desired shape **/

     outset = EDIT_empty_copy( NULL ) ;

     datum = malloc( sizeof(int) * nvals ) ;
     for( kk=0 ; kk < nvals ; kk++ ) datum[kk] = DSET_BRICK_TYPE(inset,kk) ;

     EDIT_dset_items( outset ,
                        ADN_nxyz        , nxyz ,
                        ADN_xyzdel      , dxyz ,
                        ADN_xyzorg      , orgxyz ,
                        ADN_xyzorient   , orixyz ,
                        ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                        ADN_nvals       , nvals ,
                        ADN_type        , inset->type ,
                        ADN_view_type   , inset->view_type ,
                        ADN_func_type   , inset->func_type ,
                        ADN_datum_array , datum ,
                      ADN_none ) ;
     free(datum) ;
     if( DSET_NUM_TIMES(inset) > 1 )
       EDIT_dset_items( outset ,
                          ADN_ntt       , nvals ,
                          ADN_tunits    , DSET_TIMEUNITS(inset) ,
                          ADN_ttorg     , DSET_TIMEORIGIN(inset) ,
                          ADN_ttdel     , DSET_TR(inset) ,
                          ADN_ttdur     , DSET_TIMEDURATION(inset) ,
                        ADN_none ) ;

   } /*-- end of warping to new grid --*/

   /*-- polish up the new dataset info --*/

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   if( THD_is_file(outset->dblk->diskptr->header_name) ){
     fprintf(stderr,
             "*** Output file %s already exists -- cannot continue!\n",
             outset->dblk->diskptr->header_name ) ;
     exit(1) ;
   }

   /*-- read data from disk --*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"** ERROR: can't read data from dataset %s\n",argv[nopt]) ;
      exit(1) ;
   }

   if( verb ) fprintf(stderr,"  ++ read in dataset %s\n",argv[nopt]) ;

   exit(0) ;
}
