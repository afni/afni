#include "mrilib.h"

static THD_vecmat ijk_to_dicom_in , ijk_to_dicom_out , dicom_to_ijk_in ;

static void (*warp_out_to_in)(float  ,float  ,float ,
                              float *,float *,float * ) ;

static void warp_func( float xout, float yout, float zout,
                       float *xin, float *yin, float *zin )
{
   THD_fvec3 xxx ; float xi,yi,zi ;

   LOAD_FVEC3(xxx,xout,yout,zout) ;
   VECMAT_VEC( ijk_to_dicom_out , xxx ) ;
   warp_out_to_in( xxx.xyz[0],xxx.xyz[1],xxx.xyz[2] , &xi,&yi,&zi ) ;
   LOAD_FVEC3(xxx,xi,yi,zi) ;
   VECMAT_VEC( dicom_to_ijk_in , xi,yi,zi ) ;
   *xin = xxx.xyz[0] ; *yin = xxx.xyz[1] ; *zin = xxx.xyz[2] ;
}

/*--------------------------------------------------------------------------*/
/* Find the 8 corners of the input dataset (voxel edges, not centers).
   Warp each one using the provided wfunc().
   Return the min and max (x,y,z) coordinates of these warped points.
----------------------------------------------------------------------------*/

static void warp_corners( THD_3dim_dataset *inset,
                          void wfunc(float,float,float,float *,float *,float *),
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
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = xtop = xx ;
   ybot = ytop = yy ;
   zbot = ztop = zz ;

   LOAD_FVEC3( corner , nx1,ny0,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz0) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny0,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny0,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz1) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   *xb = xbot; *xt = xtop;
   *yb = ybot; *yt = ytop; *zb = zbot; *zt = ztop; return;
}

/*--------------------------------------------------------------------------*/
/*! Geometrically transform a 3D dataset.
     - w_in2out transforms DICOM coords from input grid to output grid
     - w_out2in is the inverse of w_in2out
     - newdel = new grid size (if 0.0, use old grid size)
     - prefix = new dataset prefix
     - zpad   = number of planes to zeropad on each face of inset
     - flag   = reserved for future expansion
----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_warp3D(
                     THD_3dim_dataset *inset ,
                     void w_in2out(float,float,float,float *,float *,float *),
                     void w_out2in(float,float,float,float *,float *,float *),
                     float newdel , char *prefix , int zpad , int flag        )
{
   THD_3dim_dataset *outset ;
   int nxin,nyin,nzin,nxyzin,nvals , ival ;
   int nxout,nyout,nzout,nxyzout ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;
   int use_newgrid=(newdel > 0.0) ;
   float ddd_newgrid=newdel , fac ;
   MRI_IMAGE *inim , *outim , *wim ;

ENTRY("THD_warp3D") ;

   if( !ISVALID_DSET(inset)             ||
       w_out2in == NULL                 ||
       (w_in2out == NULL && use_newgrid)  ) RETURN(NULL);

   /*-- zeropad and replace input, if desired --*/

   if( zpad > 0 ){
     THD_3dim_dataset *qset ;
     qset = THD_zeropad( inset , zpad,zpad,zpad,zpad,zpad,zpad ,
                         "Quetzal" , ZPAD_PURGE ) ;
     if( qset == NULL ){
       fprintf(stderr,"** ERROR: THD_warp3D can't zeropad!\n"); RETURN(NULL);
     }
     DSET_unload(inset) ; inset = qset ;
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
     dicom_to_ijk_in = INV_VECMAT( ijk_to_dicom_in ) ;
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

     /* compute DICOM coordinates of warped corners */

     warp_corners( inset, w_in2out, &xbot,&xtop, &ybot,&ytop, &zbot,&ztop ) ;

     nxout = (int)( (xtop-xbot)/ddd_newgrid+0.999 ); if( nxout < 1 ) nxout = 1;
     nyout = (int)( (ytop-ybot)/ddd_newgrid+0.999 ); if( nyout < 1 ) nyout = 1;
     nzout = (int)( (ztop-zbot)/ddd_newgrid+0.999 ); if( nzout < 1 ) nzout = 1;
     nxyzout = nxout*nyout*nzout ;

     xmid = 0.5*(xbot+xtop); ymid = 0.5*(ybot+ytop); zmid = 0.5*(zbot+ztop);
     xbot = xmid-0.5*(nxout-1)*ddd_newgrid; xtop = xbot+(nxout-1)*ddd_newgrid;
     ybot = ymid-0.5*(nyout-1)*ddd_newgrid; ytop = ybot+(nyout-1)*ddd_newgrid;
     zbot = zmid-0.5*(nzout-1)*ddd_newgrid; ztop = zbot+(nzout-1)*ddd_newgrid;

     if( verb )
       fprintf(stderr,"++ Transformed grid:\n"
                      "++   xbot = %10.4g  xtop = %10.4g  nx = %d\n"
                      "++   ybot = %10.4g  ytop = %10.4g  ny = %d\n"
                      "++   zbot = %10.4g  ztop = %10.4g  nz = %d\n" ,
               xbot,xtop,nxout , ybot,ytop,nyout , zbot,ztop,nzout    ) ;

     if( nxyzout == 1 ) RETURN(NULL) ;

     nxyz.ijk[0] = nxout ; dxyz.xyz[0] = ddd_newgrid ;  /* setup axes */
     nxyz.ijk[1] = nyout ; dxyz.xyz[1] = ddd_newgrid ;
     nxyz.ijk[2] = nzout ; dxyz.xyz[2] = ddd_newgrid ;

     orixyz.ijk[0] = ORI_R2L_TYPE ; orgxyz.xyz[0] = xbot ;
     orixyz.ijk[1] = ORI_A2P_TYPE ; orgxyz.xyz[1] = ybot ;
     orixyz.ijk[2] = ORI_I2S_TYPE ; orgxyz.xyz[2] = zbot ;

     /** create dataset and mangle it into the desired shape **/

     outset = EDIT_empty_copy( NULL ) ;

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
                      ADN_none ) ;

     if( DSET_NUM_TIMES(inset) > 1 )
       EDIT_dset_items( outset ,
                          ADN_ntt       , nvals ,
                          ADN_tunits    , DSET_TIMEUNITS(inset) ,
                          ADN_ttorg     , DSET_TIMEORIGIN(inset) ,
                          ADN_ttdel     , DSET_TR(inset) ,
                          ADN_ttdur     , DSET_TIMEDURATION(inset) ,
                        ADN_none ) ;

   } /*-- end of warping to new grid --*/

   /*-- compute mapping from output dataset (i,j,k) to DICOM coords --*/

   { THD_vecmat ijk_to_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_xyz.mm, outset->daxes->xxdel,
                                   outset->daxes->yydel, outset->daxes->zzdel );
     LOAD_FVEC3   ( ijk_to_xyz.vv, outset->daxes->xxorg,
                                   outset->daxes->yyorg, outset->daxes->zzorg );

     xyz_to_dicom.mm = outset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_dicom_out = MUL_VECMAT( xyz_to_dicom , ijk_to_xyz ) ;
   }

   /*-- polish up the new dataset info --*/

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   if( !THD_filename_ok(prefix) ) prefix = "warped" ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   /*-- read input data from disk --*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){ DSET_delete(outset); RETURN(NULL); }

   if( verb ) fprintf(stderr,"++ read in dataset %s\n",argv[nopt]) ;

   /*-- compute mapping between input and output (i,j,k) --*/

   { THD_vecmat dicom_to_ijk_in , tw ;
     dicom_to_ijk_in = INV_VECMAT( ijk_to_dicom_in ) ;
     tw              = MUL_VECMAT( dicom_to_ijk_in , dicom_out2in ) ;
     matvec_warp     = MUL_VECMAT( tw , ijk_to_dicom_out ) ;
   }

   /*-- loop over bricks and warp them --*/

   if( verb ) fprintf(stderr,"++ Starting warp") ;
   for( ival=0 ; ival < nvals ; ival++ ){
     if( verb ) fprintf(stderr,".") ;
     inim  = DSET_BRICK(inset,ival) ;
     fac   = DSET_BRICK_FACTOR(inset,ival) ;
     if( fac > 0.0 && fac != 0.0 ) wim = mri_scale_to_float( fac , inim ) ;
     else                          wim = inim ;
     outim = mri_warp3D( wim , nxout,nyout,nzout , warp_func ) ;
     if( outim == NULL ){
       fprintf(stderr,"** mri_warp3D fails at ival=%d\n",ival); exit(1);
     }
     if( wim != inim ) mri_free(wim) ;
     EDIT_substitute_brick( outset , ival , outim->kind , mri_data_pointer(outim) ) ;
     DSET_unload_one( inset , ival ) ;
   }

   /*-- done!!! --*/

   if( verb ) fprintf(stderr,"\n++ Writing dataset\n") ;
   DSET_delete( inset ) ;
   DSET_write( outset ) ;
   exit(0) ;
}
