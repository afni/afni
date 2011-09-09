#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*---------------------------------------------------------------------------*/

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL) free((void *)(x)); } while(0)

#undef  HVPRINT
#define HVPRINT(lab,ZZ)                                                         \
 do{ float_pair bt = IW3D_load_hexvol(ZZ) ;                                      \
     if( bt.b > bt.a ) ININFO_message("%s hexvol range %f .. %f",lab,bt.a,bt.b) ; \
 } while(0)

static int verb_nww=0 ;
void NwarpCalcRPN_verb(int i){ verb_nww = i; }

/*---------------------------------------------------------------------------*/
/* Creation ex nihilo! */

IndexWarp3D * IW3D_create( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

   if( nx < 9 || ny < 9 || nz < 9 ) return NULL ;

   AA = (IndexWarp3D *)calloc(1,sizeof(IndexWarp3D)) ;
   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;
   AA->xd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->yd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->zd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->hv = NULL ;  /* to be filled in later, maybe */
   LOAD_DIAG_MAT44(AA->cmat,1.0f,1.0f,1.0f) ;
   LOAD_DIAG_MAT44(AA->imat,1.0f,1.0f,1.0f) ;
   AA->geomstring = NULL ;
   AA->view = VIEW_ORIGINAL_TYPE ;

   return AA ;
}

/*---------------------------------------------------------------------------*/
/* Into the valley of death! */

void IW3D_destroy( IndexWarp3D *AA )
{
   if( AA != NULL ){
     FREEIFNN(AA->xd); FREEIFNN(AA->yd); FREEIFNN(AA->zd); FREEIFNN(AA->hv);
     FREEIFNN(AA->geomstring) ;
     free(AA);
   }
   return ;
}

/*---------------------------------------------------------------------------*/
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normL1( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq])+fabsf(yda[qq])+fabsf(zda[qq]) ;
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq]-xdb[qq])+fabsf(yda[qq]-ydb[qq])+fabsf(zda[qq]-zdb[qq]) ;
   }

   return (sum/nxyz) ;
}

/*---------------------------------------------------------------------------*/
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normL2( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq])+SQR(yda[qq])+SQR(zda[qq]) ;
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq]-xdb[qq])+SQR(yda[qq]-ydb[qq])+SQR(zda[qq]-zdb[qq]) ;
   }

   return sqrtf(sum/nxyz) ;
}

/*---------------------------------------------------------------------------*/
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normLinf( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float vmax,val , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   vmax = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ ){
       val = SQR(xda[qq])+SQR(yda[qq])+SQR(zda[qq]) ;
       if( val > vmax ) vmax = val ;
     }
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ ){
       val = SQR(xda[qq]-xdb[qq])+SQR(yda[qq]-ydb[qq])+SQR(zda[qq]-zdb[qq]) ;
       if( val > vmax ) vmax = val ;
     }
   }

   return sqrtf(vmax) ;
}

/*---------------------------------------------------------------------------*/
/* Same setup, but contents are zero. */

IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

   BB = IW3D_create( AA->nx , AA->ny , AA->nz ) ;

   BB->cmat = AA->cmat ; BB->imat = AA->imat ;

   if( AA->geomstring != NULL )
     BB->geomstring = strdup(AA->geomstring) ;

   BB->view = AA->view ;

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Make a copy, scaling displacements by fac. */

IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

   BB = IW3D_empty_copy(AA) ;  /* all zero displacements */

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( fac == 1.0f ){
     memcpy( BB->xd , AA->xd , sizeof(float)*nxyz ) ;
     memcpy( BB->yd , AA->yd , sizeof(float)*nxyz ) ;
     memcpy( BB->zd , AA->zd , sizeof(float)*nxyz ) ;
   } else if( fac != 0.0f ){
     int qq ;
     for( qq=0 ; qq < nxyz ; qq++ ){
       BB->xd[qq] = fac * AA->xd[qq] ;
       BB->yd[qq] = fac * AA->yd[qq] ;
       BB->zd[qq] = fac * AA->zd[qq] ;
     }
   }

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Cut out a box-shaped piece of pie. */

IndexWarp3D * IW3D_cutoutbox( IndexWarp3D *AA , int ibot , int itop ,
                              int jbot , int jtop , int kbot , int ktop )
{
   IndexWarp3D *BB ;
   int ii,jj,kk , nx,ny,nz,nxy , aoff,boff , qx,qy,qz,qxy ; size_t qrow ;

   if( AA == NULL || ibot < 0 || itop >= AA->nx || ibot > itop ||
                     jbot < 0 || jtop >= AA->ny || jbot > jtop ||
                     kbot < 0 || ktop >= AA->nz || kbot > ktop   ) return NULL ;

   qx  = itop - ibot + 1 ; nx  = AA->nx ;
   qy  = jtop - jbot + 1 ; ny  = AA->ny ;
   qz  = ktop - kbot + 1 ; nz  = AA->nz ;
   qxy = qx*qy           ; nxy = nx*ny  ; qrow = qx*sizeof(float) ;

   BB = IW3D_create(qx,qy,qz) ;

   for( kk=kbot ; kk <= ktop ; kk++ ){
    for( jj=jbot ; jj <= jtop ; jj++ ){
      aoff =  kk      *nxy +  jj      *nx + ibot ;
      boff = (kk-kbot)*qxy + (jj-jbot)*qx ;
      memcpy( BB->xd + boff , AA->xd + aoff , qrow ) ;
      memcpy( BB->yd + boff , AA->yd + aoff , qrow ) ;
      memcpy( BB->zd + boff , AA->zd + aoff , qrow ) ;
   }}

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Paste in a box-shaped piece of pie. */

void IW3D_pasteinbox( IndexWarp3D *AA , IndexWarp3D *BB ,
                      int ibot , int jbot , int kbot     )
{
   int ii,jj,kk , nx,ny,nz,nxy , aoff,boff , qx,qy,qz,qxy , jtop,ktop ;
   size_t qrow ;

   if( AA == NULL || ibot < 0 || ibot >= AA->nx ||
       BB == NULL || jbot < 0 || jbot >= AA->ny ||
                     kbot < 0 || kbot >= AA->nz   ) return ;

   qx = BB->nx ; nx = AA->nx ; qrow = qx * sizeof(float) ;
   qy = BB->ny ; ny = AA->ny ; qxy = qx*qy ; nxy = nx*ny ;
   qz = BB->nz ; nz = AA->nz ;

   if( ibot+qx >= nx || jbot+qy >= ny || kbot+qz >= nz ) return ;

   jtop = jbot + qy - 1 ; ktop = kbot + qz - 1 ;

   for( kk=kbot ; kk <= ktop ; kk++ ){
    for( jj=jbot ; jj <= jtop ; jj++ ){
      aoff =  kk      *nxy +  jj      *nx + ibot ;
      boff = (kk-kbot)*qxy + (jj-jbot)*qx ;
      memcpy( AA->xd + aoff , BB->xd + boff , qrow ) ;
      memcpy( AA->yd + aoff , BB->yd + boff , qrow ) ;
      memcpy( AA->zd + aoff , BB->zd + boff , qrow ) ;
   }}

   return ;
}

/*---------------------------------------------------------------------------*/
/* Scale displacements by fac (in-place). */

void IW3D_scale( IndexWarp3D *AA , float fac )
{
   int nxyz , qq ;

   if( AA == NULL || fac == 1.0f ) return ;

   nxyz = AA->nx * AA->ny * AA->nz ;

   for( qq=0 ; qq < nxyz ; qq++ ){
     AA->xd[qq] *= fac ;
     AA->yd[qq] *= fac ;
     AA->zd[qq] *= fac ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/
/* Sum displacements, with factors. */

IndexWarp3D * IW3D_sum( IndexWarp3D *AA, float Afac, IndexWarp3D *BB, float Bfac )
{
   IndexWarp3D *CC ; int nxyz , qq ;

   if( AA == NULL && BB == NULL ) return NULL ;

   if( AA == NULL || Afac == 0.0f ){
     CC = IW3D_copy( BB , Bfac ) ; return CC ;
   } else if( BB == NULL || Bfac == 0.0f ){
     CC = IW3D_copy( AA , Afac ) ; return CC ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( BB->nx * BB->ny * BB->nz != nxyz ) return NULL ;

   CC = IW3D_empty_copy(AA) ;  /* all zero displacements */

   for( qq=0 ; qq < nxyz ; qq++ ){
     CC->xd[qq] = Afac * AA->xd[qq] + Bfac * BB->xd[qq] ;
     CC->yd[qq] = Afac * AA->yd[qq] + Bfac * BB->yd[qq] ;
     CC->zd[qq] = Afac * AA->zd[qq] + Bfac * BB->zd[qq] ;
   }

   return CC ;
}

/*----------------------------------------------------------------------------*/
/* Convert a 3D dataset of displacments to an index warp. */

IndexWarp3D * IW3D_from_dataset( THD_3dim_dataset *dset , int empty )
{
   IndexWarp3D *AA ;
   MRI_IMAGE *xim , *yim , *zim ;
   mat44 cmat , imat ;
   int nx,ny,nz , nxyz , ii ;
   float *xar,*yar,*zar , *xda,*yda,*zda ;
   char *gstr ;

ENTRY("IW3D_from_dataset") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   if( !empty ){
     if( DSET_NVALS(dset) < 3 ) RETURN(NULL) ;
     DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset->daxes) ;

   cmat = dset->daxes->ijk_to_dicom ;  /* takes ijk to xyz */
   imat = MAT44_INV(cmat) ;            /* takes xyz to ijk */

   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   AA = IW3D_create(nx,ny,nz) ;

   AA->cmat = cmat ; AA->imat = imat ;
   gstr = EDIT_get_geometry_string(dset) ;
   if( gstr != NULL ) AA->geomstring = strdup(gstr) ;

   AA->view = dset->view_type ;

   if( !empty ){
     xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
     xim = THD_extract_float_brick(0,dset) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = THD_extract_float_brick(1,dset) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = THD_extract_float_brick(2,dset) ; zar = MRI_FLOAT_PTR(zim) ;
     DSET_unload(dset) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 88888 )
 { int ii ;
#pragma omp for
     for( ii=0 ; ii < nxyz ; ii++ ){  /* convert mm to index displacements */
       MAT33_VEC( imat , xar[ii],yar[ii],zar[ii] , xda[ii],yda[ii],zda[ii] ) ;
     }
 }
 AFNI_OMP_END ;

     mri_free(zim) ; mri_free(yim) ; mri_free(xim) ;
   }

   RETURN(AA) ;
}

/*----------------------------------------------------------------------------*/
/* Convert an index warp to a 3D dataset of spatial displacmements */

THD_3dim_dataset * IW3D_to_dataset( IndexWarp3D *AA , char *prefix )
{
   THD_3dim_dataset *dset ;
   float *xar,*yar,*zar,*har , *xda,*yda,*zda,*hva , hfac ;
   mat44 cmat ;
   int ii , nxyz ;

ENTRY("IW3D_to_dataset") ;

   if( AA == NULL ) RETURN(NULL) ;

   if( AA->geomstring == NULL ){
     char *gstr = EDIT_imat_to_geometry_string(AA->imat,AA->nx,AA->ny,AA->nz) ;
     if( gstr == NULL ) RETURN(NULL) ;  /* should not transpire */
     AA->geomstring = strdup(gstr) ;
   }

   dset = EDIT_geometry_constructor( AA->geomstring , prefix ) ;

   EDIT_dset_items( dset ,
                      ADN_nvals     , 4         ,
                      ADN_datum_all , MRI_float ,
                      ADN_view_type , AA->view  ,
                    NULL ) ;
   EDIT_BRICK_LABEL( dset , 0 , "x_delta" ) ;
   EDIT_BRICK_LABEL( dset , 1 , "y_delta" ) ;
   EDIT_BRICK_LABEL( dset , 2 , "z_delta" ) ;
   EDIT_BRICK_LABEL( dset , 3 , "hexvol"  ) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   nxyz = AA->nx * AA->ny * AA->nz ;
   cmat = AA->cmat ;

   xar = (float *)malloc(sizeof(float)*nxyz) ;
   yar = (float *)malloc(sizeof(float)*nxyz) ;
   zar = (float *)malloc(sizeof(float)*nxyz) ;
   har = (float *)malloc(sizeof(float)*nxyz) ;

   if( AA->hv == NULL ) (void)IW3D_load_hexvol(AA) ;
   hva = AA->hv ; hfac = MAT44_DET(cmat) ; hfac = fabsf(hfac) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 88888 )
 { int ii ;
#pragma omp for
   for( ii=0 ; ii < nxyz ; ii++ ){
     MAT33_VEC( cmat , xda[ii],yda[ii],zda[ii] , xar[ii],yar[ii],zar[ii] ) ;
     har[ii] = hfac * hva[ii] ;
   }
 }
 AFNI_OMP_END ;

   EDIT_substitute_brick( dset , 0 , MRI_float , xar ) ;
   EDIT_substitute_brick( dset , 1 , MRI_float , yar ) ;
   EDIT_substitute_brick( dset , 2 , MRI_float , zar ) ;
   EDIT_substitute_brick( dset , 3 , MRI_float , har ) ;

   RETURN(dset) ;
}

#ifndef HAVE_HEXVOL
#define HAVE_HEXVOL
/*----------------------------------------------------------------------------*/
/* Volume of a hexahedron (distorted cube) given by 8 corners.
   Looking down from the top, the bottom plane points are numbered so:
       2 -- 3
       |    |  and the top plane is similar (add 4 to each index),
       0 -- 1  with point #(i+4) 'above' point #i.
*//*--------------------------------------------------------------------------*/

#undef  TRIPROD
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )
#undef  DA
#undef  DB
#undef  DC
#define DA(p,q) (p.a-q.a)
#define DB(p,q) (p.b-q.b)
#define DC(p,q) (p.c-q.c)

static INLINE float hexahedron_volume( float_triple x0 , float_triple x1 ,
                                       float_triple x2 , float_triple x3 ,
                                       float_triple x4 , float_triple x5 ,
                                       float_triple x6 , float_triple x7  )
{
   float xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

   xa = DA(x7,x1)+DA(x6,x0); ya = DB(x7,x1)+DB(x6,x0); za = DC(x7,x1)+DC(x6,x0);
   xb = DA(x7,x2)          ; yb = DB(x7,x2)          ; zb = DC(x7,x2) ;
   xc = DA(x3,x0)          ; yc = DB(x3,x0)          ; zc = DC(x3,x0) ;
   vol = TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x6,x0)          ; ya = DB(x6,x0)          ; za = DC(x6,x0) ;
   xb = DA(x7,x2)+DA(x5,x0); yb = DB(x7,x2)+DB(x5,x0); zb = DC(x7,x2)+DC(x5,x0);
   xc = DA(x7,x4)          ; yc = DB(x7,x4)          ; zc = DC(x7,x4) ;
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x7,x1)          ; ya = DB(x7,x1)          ; za = DC(x7,x1) ;
   xb = DA(x5,x0)          ; yb = DB(x5,x0)          ; zb = DC(x5,x0) ;
   xc = DA(x7,x4)+DA(x3,x0); yc = DB(x7,x4)+DB(x3,x0); zc = DC(x7,x4)+DC(x3,x0);
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   return (0.08333333f*vol) ;
}
#undef TRIPROD
#undef DA
#undef DB
#undef DC
#endif /* HAVE_HEXVOL */

/*---------------------------------------------------------------------------*/
/* Load the volumes of each hexahedral element in the displaced grid. */

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

#undef  C2F
#define C2F(p,q,r,xx) ( (xx).a = (p) , (xx).b = (q) , (xx).c = (r) )

#undef  D2F
#define D2F(pqr,xx)   ( (xx).a+=xda[pqr], (xx).b+=yda[pqr], (xx).c+=zda[pqr] )

float_pair IW3D_load_hexvol( IndexWarp3D *AA )
{
   float *xda, *yda , *zda , *hva , top,bot ;
   int nx,ny,nz , nxy,nxyz , ii ;
   float_pair hvm = {0.0f,0.0f} ;

   if( AA == NULL ) return hvm ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   hva = AA->hv ;
   if( hva == NULL ) hva = AA->hv = (float *)calloc(nxyz,sizeof(float)) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 33333 )
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     C2F(ii,jj,kk,x0); C2F(ip,jj,kk,x1); C2F(ii,jp,kk,x2); C2F(ip,jp,kk,x3);
     C2F(ii,jj,kp,x4); C2F(ip,jj,kp,x5); C2F(ii,jp,kp,x6); C2F(ip,jp,kp,x7);
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; D2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; D2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; D2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; D2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; D2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; D2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; D2F(ijk,x7) ;
     ijk = qq            ; D2F(ijk,x0) ;
     hva[qq] = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) ;
   }
 }
 AFNI_OMP_END ;

  top = bot = hva[0] ;
  for( ii=1 ; ii < nxyz ; ii++ ){
         if( hva[ii] > top ) top = hva[ii] ;
    else if( hva[ii] < bot ) bot = hva[ii] ;
  }
  hvm.a = bot ; hvm.b = top ; return hvm ;
}

/*---------------------------------------------------------------------------*/
/* The following functions are for interpolating all 3 components of an index
   warp at one time, and are shamelessly ripped off from mri_genalign_util.c */

#undef  CLIP
#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

#undef  AJK
#define AJK(aaa,j,k) ((aaa)+(j)*nx+(k)*nxy)

/*---------------------------------------------------------------------------*/
/*! Interpolate using linear method */

void IW3D_interp_linear( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
 AFNI_OMP_START ;
#pragma omp parallel if(npp > 9999)
 {
   int nx=nxx, ny=nyy, nz=nzz, nxy=nx*ny, pp, nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz , ix,jy,kz ;
   int   ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
   float wt_00,wt_p1 ;
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
   float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
#if 0
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
#else
     xx = ip[pp] ; if( xx < -0.499f ) xx = -0.499f ; else if( xx > nxh ) xx = nxh ;
     yy = jp[pp] ; if( yy < -0.499f ) yy = -0.499f ; else if( yy > nyh ) yy = nyh ;
     zz = kp[pp] ; if( zz < -0.499f ) zz = -0.499f ; else if( zz > nzh ) zz = nzh ;
#endif

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ;

     wt_00 = 1.0f-fx ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points */

#undef  XINT
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_j00_k00 = XINT(aar,jy_00,kz_00) ; f_jp1_k00 = XINT(aar,jy_p1,kz_00) ;
     f_j00_kp1 = XINT(aar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(aar,jy_p1,kz_p1) ;
     g_j00_k00 = XINT(bar,jy_00,kz_00) ; g_jp1_k00 = XINT(bar,jy_p1,kz_00) ;
     g_j00_kp1 = XINT(bar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(bar,jy_p1,kz_p1) ;
     h_j00_k00 = XINT(car,jy_00,kz_00) ; h_jp1_k00 = XINT(car,jy_p1,kz_00) ;
     h_j00_kp1 = XINT(car,jy_00,kz_p1) ; h_jp1_kp1 = XINT(car,jy_p1,kz_p1) ;

     /* interpolate to jy+fy at each kz level */

     wt_00 = 1.0f-fy ; wt_p1 = fy ;
     f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
     f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;
     g_k00 =  wt_00 * g_j00_k00 + wt_p1 * g_jp1_k00 ;
     g_kp1 =  wt_00 * g_j00_kp1 + wt_p1 * g_jp1_kp1 ;
     h_k00 =  wt_00 * h_j00_k00 + wt_p1 * h_jp1_k00 ;
     h_kp1 =  wt_00 * h_j00_kp1 + wt_p1 * h_jp1_kp1 ;

     /* interpolate to kz+fz to get output */

     uar[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 ;
     var[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 ;
     war[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

   return ;
}

/*---------------------------------------------------------------------------*/
/* Interpolation with weighted (tapered) sinc in 3D.
   ++ Taper function wtap(r) is defined to be 1 for 0 <= r <= WCUT
       and for WCUT < r < 1 is a raised c sine dropping down to wtap(r=1) = 0.
       This choice was made to keep the variance smoothing artifact low.
   ++ Radius of sinc window is WRAD, so the actual taper used is wtap(x/WRAD)
*//*-------------------------------------------------------------------------*/

#undef  WCUT
#define WCUT 0.5f    /* cutoff point for taper */

#undef  WRAD
#define WRAD 5.0001f /* width of sinc interpolation (float) */

#undef  IRAD
#define IRAD 5       /* width of sinc interpolation (int) */

#undef  PIF
#define PIF 3.1415927f /* PI in float */

/* sinc function = sin(PI*x)/(PI*x) [N.B.: x will always be >= 0] */

#undef  sinc
#define sinc(x) ( ((x)>0.01f) ? sinf(PIF*(x))/(PIF*(x))     \
                              : 1.0f - 1.6449341f*(x)*(x) )

/* Weight (taper) function, declining from wtap(WCUT)=1 to wtap(1)=0 */
/* Note that the input to wtap will always be between WCUT and 1.   */

#undef  wtap
#define wtap(x) ( 0.5f+0.5f*cosf(PIF*((x)-WCUT)/(1.0f-WCUT)) )

#undef AW
#undef BW
#undef CW
#define AW(i) aarjk[iqq[i]]*wtt[i]
#define BW(i) barjk[iqq[i]]*wtt[i]
#define CW(i) carjk[iqq[i]]*wtt[i]

/*---------------------------------------------------------------------------*/

void IW3D_interp_wsinc5( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
 AFNI_OMP_START ;
#pragma omp parallel if(npp > 6666)
 {
   int nx=nxx , ny=nyy , nz=nzz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *aarjk , *barjk , *carjk ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;

   float xw,yw,zw,rr , asum,bsum,csum,wsum,wfac,wt ;
   int   iq,jq,kq,iqp , qq,jj,kk , ddi,ddj,ddk ;
   float xsin[2*IRAD] , ysin[2*IRAD]        , zsin[2*IRAD] ;
   float wtt[2*IRAD]  , ajk[2*IRAD][2*IRAD] , ak[2*IRAD]   ;
   float                bjk[2*IRAD][2*IRAD] , bk[2*IRAD]   ;
   float                cjk[2*IRAD][2*IRAD] , ck[2*IRAD]   ;
   int   iqq[2*IRAD]  ;
   /*----- loop over points -----*/

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
#if 0
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
#else
     xx = ip[pp] ; if( xx < -0.499f ) xx = -0.499f ; else if( xx > nxh ) xx = nxh ;
     yy = jp[pp] ; if( yy < -0.499f ) yy = -0.499f ; else if( yy > nyh ) yy = nyh ;
     zz = kp[pp] ; if( zz < -0.499f ) zz = -0.499f ; else if( zz > nzh ) zz = nzh ;
#endif

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     /*- x interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       xw  = fabsf(fx - qq) ; wt = sinc(xw) ;
       xw /= WRAD ; if( xw > WCUT ) wt *= wtap(xw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
       iq = ix+qq ; CLIP(iq,nx1) ; iqq[qq+(IRAD-1)] = iq ;
     }
     wfac = wsum ;

     for( jj=-IRAD+1 ; jj <= IRAD ; jj++ ){  /* interps */
       jq = jy+jj ; CLIP(jq,ny1) ;
       for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
         kq = kz+kk ; CLIP(kq,nz1) ;
         aarjk = AJK(aar,jq,kq) ;
         barjk = AJK(bar,jq,kq) ;
         carjk = AJK(car,jq,kq) ;
         ajk[jj+(IRAD-1)][kk+(IRAD-1)] =
           AW(0)+AW(1)+AW(2)+AW(3)+AW(4)+AW(5)+AW(6)+AW(7)+AW(8)+AW(9) ;
         bjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           BW(0)+BW(1)+BW(2)+BW(3)+BW(4)+BW(5)+BW(6)+BW(7)+BW(8)+BW(9) ;
         cjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           CW(0)+CW(1)+CW(2)+CW(3)+CW(4)+CW(5)+CW(6)+CW(7)+CW(8)+CW(9) ;
       }
     }

     /*- y interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       yw  = fabsf(fy - qq) ; wt = sinc(yw) ;
       yw /= WRAD ; if( yw > WCUT ) wt *= wtap(yw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){  /* interps */
       for( asum=bsum=csum=0.0f,jj=-IRAD+1 ; jj <  IRAD ; jj+=2 ){  /* unrolled by 2 */
         asum += wtt[jj+(IRAD-1)]*ajk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*ajk[jj+ IRAD   ][kk+(IRAD-1)] ;
         bsum += wtt[jj+(IRAD-1)]*bjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*bjk[jj+ IRAD   ][kk+(IRAD-1)] ;
         csum += wtt[jj+(IRAD-1)]*cjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*cjk[jj+ IRAD   ][kk+(IRAD-1)] ;
       }
       ak[kk+(IRAD-1)] = asum ;
       bk[kk+(IRAD-1)] = bsum ;
       ck[kk+(IRAD-1)] = csum ;
     }

     /*- z interpolation -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       zw  = fabsf(fz - qq) ; wt = sinc(zw) ;
       zw /= WRAD ; if( zw > WCUT ) wt *= wtap(zw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     /* interps */

     for( asum=bsum=csum=0.0f,kk=-IRAD+1 ; kk <  IRAD ; kk+=2 ){  /* unrolled by 2 */
       asum += wtt[kk+(IRAD-1)] * ak[kk+(IRAD-1)] + wtt[kk+IRAD] * ak[kk+IRAD] ;
       bsum += wtt[kk+(IRAD-1)] * bk[kk+(IRAD-1)] + wtt[kk+IRAD] * bk[kk+IRAD] ;
       csum += wtt[kk+(IRAD-1)] * ck[kk+(IRAD-1)] + wtt[kk+IRAD] * ck[kk+IRAD] ;
     }

     uar[pp] = asum / wfac ;
     var[pp] = bsum / wfac ;
     war[pp] = csum / wfac ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

   return ;
}

/*---------------------------------------------------------------------------*/
/* define quintic interpolation polynomials (Lagrange) */

#undef  Q_M2
#undef  Q_M1
#undef  Q_00
#undef  Q_P1
#undef  Q_P2
#undef  Q_P3
#define Q_M2(x)  (x*(x*x-1.0f)*(2.0f-x)*(x-3.0f)*0.008333333f)
#define Q_M1(x)  (x*(x*x-4.0f)*(x-1.0f)*(x-3.0f)*0.041666667f)
#define Q_00(x)  ((x*x-4.0f)*(x*x-1.0f)*(3.0f-x)*0.083333333f)
#define Q_P1(x)  (x*(x*x-4.0f)*(x+1.0f)*(x-3.0f)*0.083333333f)
#define Q_P2(x)  (x*(x*x-1.0f)*(x+2.0f)*(3.0f-x)*0.041666667f)
#define Q_P3(x)  (x*(x*x-1.0f)*(x*x-4.0f)*0.008333333f)

/*---------------------------------------------------------------------------*/

void IW3D_interp_quintic( int nxx , int nyy , int nzz ,
                          float *aar , float *bar , float *car ,
                          int npp, float *ip, float *jp, float *kp,
                          float *uar , float *var , float *war     )
{
 AFNI_OMP_START ;
#pragma omp parallel if(npp > 9999)
 {
   int nx=nxx , ny=nyy , nz=nzz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;
   int ix_m2,ix_m1,ix_00,ix_p1,ix_p2,ix_p3 ; /* interpolation indices */
   int jy_m2,jy_m1,jy_00,jy_p1,jy_p2,jy_p3 ; /* (input image) */
   int kz_m2,kz_m1,kz_00,kz_p1,kz_p2,kz_p3 ;

   float wt_m2,wt_m1,wt_00,wt_p1,wt_p2,wt_p3 ; /* interpolation weights */

   float f_jm2_km2, f_jm1_km2, f_j00_km2, f_jp1_km2, f_jp2_km2, f_jp3_km2,
         f_jm2_km1, f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, f_jp3_km1,
         f_jm2_k00, f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00, f_jp3_k00,
         f_jm2_kp1, f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1, f_jp3_kp1,
         f_jm2_kp2, f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2, f_jp3_kp2,
         f_jm2_kp3, f_jm1_kp3, f_j00_kp3, f_jp1_kp3, f_jp2_kp3, f_jp3_kp3,
         f_km2    , f_km1    , f_k00    , f_kp1    , f_kp2    , f_kp3     ;
   float g_jm2_km2, g_jm1_km2, g_j00_km2, g_jp1_km2, g_jp2_km2, g_jp3_km2,
         g_jm2_km1, g_jm1_km1, g_j00_km1, g_jp1_km1, g_jp2_km1, g_jp3_km1,
         g_jm2_k00, g_jm1_k00, g_j00_k00, g_jp1_k00, g_jp2_k00, g_jp3_k00,
         g_jm2_kp1, g_jm1_kp1, g_j00_kp1, g_jp1_kp1, g_jp2_kp1, g_jp3_kp1,
         g_jm2_kp2, g_jm1_kp2, g_j00_kp2, g_jp1_kp2, g_jp2_kp2, g_jp3_kp2,
         g_jm2_kp3, g_jm1_kp3, g_j00_kp3, g_jp1_kp3, g_jp2_kp3, g_jp3_kp3,
         g_km2    , g_km1    , g_k00    , g_kp1    , g_kp2    , g_kp3     ;
   float h_jm2_km2, h_jm1_km2, h_j00_km2, h_jp1_km2, h_jp2_km2, h_jp3_km2,
         h_jm2_km1, h_jm1_km1, h_j00_km1, h_jp1_km1, h_jp2_km1, h_jp3_km1,
         h_jm2_k00, h_jm1_k00, h_j00_k00, h_jp1_k00, h_jp2_k00, h_jp3_k00,
         h_jm2_kp1, h_jm1_kp1, h_j00_kp1, h_jp1_kp1, h_jp2_kp1, h_jp3_kp1,
         h_jm2_kp2, h_jm1_kp2, h_j00_kp2, h_jp1_kp2, h_jp2_kp2, h_jp3_kp2,
         h_jm2_kp3, h_jm1_kp3, h_j00_kp3, h_jp1_kp3, h_jp2_kp3, h_jp3_kp3,
         h_km2    , h_km1    , h_k00    , h_kp1    , h_kp2    , h_kp3     ;
#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
#if 0
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
#else
     xx = ip[pp] ; if( xx < -0.499f ) xx = -0.499f ; else if( xx > nxh ) xx = nxh ;
     yy = jp[pp] ; if( yy < -0.499f ) yy = -0.499f ; else if( yy > nyh ) yy = nyh ;
     zz = kp[pp] ; if( zz < -0.499f ) zz = -0.499f ; else if( zz > nzh ) zz = nzh ;
#endif

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     /* compute indexes from which to interpolate (-2,-1,0,+1,+2,+3),
        but clipped to lie inside input image volume                 */

     ix_m1 = ix-1    ; ix_00 = ix      ; ix_p1 = ix+1    ; ix_p2 = ix+2    ;
     CLIP(ix_m1,nx1) ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ; CLIP(ix_p2,nx1) ;
     ix_m2 = ix-2    ; ix_p3 = ix+3 ;
     CLIP(ix_m2,nx1) ; CLIP(ix_p3,nx1) ;

     jy_m1 = jy-1    ; jy_00 = jy      ; jy_p1 = jy+1    ; jy_p2 = jy+2    ;
     CLIP(jy_m1,ny1) ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ; CLIP(jy_p2,ny1) ;
     jy_m2 = jy-2    ; jy_p3 = jy+3 ;
     CLIP(jy_m2,ny1) ; CLIP(jy_p3,ny1) ;

     kz_m1 = kz-1    ; kz_00 = kz      ; kz_p1 = kz+1    ; kz_p2 = kz+2    ;
     CLIP(kz_m1,nz1) ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ; CLIP(kz_p2,nz1) ;
     kz_m2 = kz-2    ; kz_p3 = kz+3 ;
     CLIP(kz_m2,nz1) ; CLIP(kz_p3,nz1) ;

     wt_m1 = Q_M1(fx) ; wt_00 = Q_00(fx) ;  /* interpolation weights */
     wt_p1 = Q_P1(fx) ; wt_p2 = Q_P2(fx) ;  /* in x-direction        */
     wt_m2 = Q_M2(fx) ; wt_p3 = Q_P3(fx) ;

#undef  XINT
#define XINT(aaa,j,k) wt_m2*aaa[IJK(ix_m2,j,k)]+wt_m1*aaa[IJK(ix_m1,j,k)] \
                     +wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)] \
                     +wt_p2*aaa[IJK(ix_p2,j,k)]+wt_p3*aaa[IJK(ix_p3,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_jm2_km2 = XINT(aar,jy_m2,kz_m2) ; f_jm1_km2 = XINT(aar,jy_m1,kz_m2) ;
     f_j00_km2 = XINT(aar,jy_00,kz_m2) ; f_jp1_km2 = XINT(aar,jy_p1,kz_m2) ;
     f_jp2_km2 = XINT(aar,jy_p2,kz_m2) ; f_jp3_km2 = XINT(aar,jy_p3,kz_m2) ;
     f_jm2_km1 = XINT(aar,jy_m2,kz_m1) ; f_jm1_km1 = XINT(aar,jy_m1,kz_m1) ;
     f_j00_km1 = XINT(aar,jy_00,kz_m1) ; f_jp1_km1 = XINT(aar,jy_p1,kz_m1) ;
     f_jp2_km1 = XINT(aar,jy_p2,kz_m1) ; f_jp3_km1 = XINT(aar,jy_p3,kz_m1) ;
     f_jm2_k00 = XINT(aar,jy_m2,kz_00) ; f_jm1_k00 = XINT(aar,jy_m1,kz_00) ;
     f_j00_k00 = XINT(aar,jy_00,kz_00) ; f_jp1_k00 = XINT(aar,jy_p1,kz_00) ;
     f_jp2_k00 = XINT(aar,jy_p2,kz_00) ; f_jp3_k00 = XINT(aar,jy_p3,kz_00) ;
     f_jm2_kp1 = XINT(aar,jy_m2,kz_p1) ; f_jm1_kp1 = XINT(aar,jy_m1,kz_p1) ;
     f_j00_kp1 = XINT(aar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(aar,jy_p1,kz_p1) ;
     f_jp2_kp1 = XINT(aar,jy_p2,kz_p1) ; f_jp3_kp1 = XINT(aar,jy_p3,kz_p1) ;
     f_jm2_kp2 = XINT(aar,jy_m2,kz_p2) ; f_jm1_kp2 = XINT(aar,jy_m1,kz_p2) ;
     f_j00_kp2 = XINT(aar,jy_00,kz_p2) ; f_jp1_kp2 = XINT(aar,jy_p1,kz_p2) ;
     f_jp2_kp2 = XINT(aar,jy_p2,kz_p2) ; f_jp3_kp2 = XINT(aar,jy_p3,kz_p2) ;
     f_jm2_kp3 = XINT(aar,jy_m2,kz_p3) ; f_jm1_kp3 = XINT(aar,jy_m1,kz_p3) ;
     f_j00_kp3 = XINT(aar,jy_00,kz_p3) ; f_jp1_kp3 = XINT(aar,jy_p1,kz_p3) ;
     f_jp2_kp3 = XINT(aar,jy_p2,kz_p3) ; f_jp3_kp3 = XINT(aar,jy_p3,kz_p3) ;

     g_jm2_km2 = XINT(bar,jy_m2,kz_m2) ; g_jm1_km2 = XINT(bar,jy_m1,kz_m2) ;
     g_j00_km2 = XINT(bar,jy_00,kz_m2) ; g_jp1_km2 = XINT(bar,jy_p1,kz_m2) ;
     g_jp2_km2 = XINT(bar,jy_p2,kz_m2) ; g_jp3_km2 = XINT(bar,jy_p3,kz_m2) ;
     g_jm2_km1 = XINT(bar,jy_m2,kz_m1) ; g_jm1_km1 = XINT(bar,jy_m1,kz_m1) ;
     g_j00_km1 = XINT(bar,jy_00,kz_m1) ; g_jp1_km1 = XINT(bar,jy_p1,kz_m1) ;
     g_jp2_km1 = XINT(bar,jy_p2,kz_m1) ; g_jp3_km1 = XINT(bar,jy_p3,kz_m1) ;
     g_jm2_k00 = XINT(bar,jy_m2,kz_00) ; g_jm1_k00 = XINT(bar,jy_m1,kz_00) ;
     g_j00_k00 = XINT(bar,jy_00,kz_00) ; g_jp1_k00 = XINT(bar,jy_p1,kz_00) ;
     g_jp2_k00 = XINT(bar,jy_p2,kz_00) ; g_jp3_k00 = XINT(bar,jy_p3,kz_00) ;
     g_jm2_kp1 = XINT(bar,jy_m2,kz_p1) ; g_jm1_kp1 = XINT(bar,jy_m1,kz_p1) ;
     g_j00_kp1 = XINT(bar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(bar,jy_p1,kz_p1) ;
     g_jp2_kp1 = XINT(bar,jy_p2,kz_p1) ; g_jp3_kp1 = XINT(bar,jy_p3,kz_p1) ;
     g_jm2_kp2 = XINT(bar,jy_m2,kz_p2) ; g_jm1_kp2 = XINT(bar,jy_m1,kz_p2) ;
     g_j00_kp2 = XINT(bar,jy_00,kz_p2) ; g_jp1_kp2 = XINT(bar,jy_p1,kz_p2) ;
     g_jp2_kp2 = XINT(bar,jy_p2,kz_p2) ; g_jp3_kp2 = XINT(bar,jy_p3,kz_p2) ;
     g_jm2_kp3 = XINT(bar,jy_m2,kz_p3) ; g_jm1_kp3 = XINT(bar,jy_m1,kz_p3) ;
     g_j00_kp3 = XINT(bar,jy_00,kz_p3) ; g_jp1_kp3 = XINT(bar,jy_p1,kz_p3) ;
     g_jp2_kp3 = XINT(bar,jy_p2,kz_p3) ; g_jp3_kp3 = XINT(bar,jy_p3,kz_p3) ;

     h_jm2_km2 = XINT(car,jy_m2,kz_m2) ; h_jm1_km2 = XINT(car,jy_m1,kz_m2) ;
     h_j00_km2 = XINT(car,jy_00,kz_m2) ; h_jp1_km2 = XINT(car,jy_p1,kz_m2) ;
     h_jp2_km2 = XINT(car,jy_p2,kz_m2) ; h_jp3_km2 = XINT(car,jy_p3,kz_m2) ;
     h_jm2_km1 = XINT(car,jy_m2,kz_m1) ; h_jm1_km1 = XINT(car,jy_m1,kz_m1) ;
     h_j00_km1 = XINT(car,jy_00,kz_m1) ; h_jp1_km1 = XINT(car,jy_p1,kz_m1) ;
     h_jp2_km1 = XINT(car,jy_p2,kz_m1) ; h_jp3_km1 = XINT(car,jy_p3,kz_m1) ;
     h_jm2_k00 = XINT(car,jy_m2,kz_00) ; h_jm1_k00 = XINT(car,jy_m1,kz_00) ;
     h_j00_k00 = XINT(car,jy_00,kz_00) ; h_jp1_k00 = XINT(car,jy_p1,kz_00) ;
     h_jp2_k00 = XINT(car,jy_p2,kz_00) ; h_jp3_k00 = XINT(car,jy_p3,kz_00) ;
     h_jm2_kp1 = XINT(car,jy_m2,kz_p1) ; h_jm1_kp1 = XINT(car,jy_m1,kz_p1) ;
     h_j00_kp1 = XINT(car,jy_00,kz_p1) ; h_jp1_kp1 = XINT(car,jy_p1,kz_p1) ;
     h_jp2_kp1 = XINT(car,jy_p2,kz_p1) ; h_jp3_kp1 = XINT(car,jy_p3,kz_p1) ;
     h_jm2_kp2 = XINT(car,jy_m2,kz_p2) ; h_jm1_kp2 = XINT(car,jy_m1,kz_p2) ;
     h_j00_kp2 = XINT(car,jy_00,kz_p2) ; h_jp1_kp2 = XINT(car,jy_p1,kz_p2) ;
     h_jp2_kp2 = XINT(car,jy_p2,kz_p2) ; h_jp3_kp2 = XINT(car,jy_p3,kz_p2) ;
     h_jm2_kp3 = XINT(car,jy_m2,kz_p3) ; h_jm1_kp3 = XINT(car,jy_m1,kz_p3) ;
     h_j00_kp3 = XINT(car,jy_00,kz_p3) ; h_jp1_kp3 = XINT(car,jy_p1,kz_p3) ;
     h_jp2_kp3 = XINT(car,jy_p2,kz_p3) ; h_jp3_kp3 = XINT(car,jy_p3,kz_p3) ;

     /* interpolate to jy+fy at each kz level */

     wt_m1 = Q_M1(fy) ; wt_00 = Q_00(fy) ; wt_p1 = Q_P1(fy) ;
     wt_p2 = Q_P2(fy) ; wt_m2 = Q_M2(fy) ; wt_p3 = Q_P3(fy) ;

     f_km2 =  wt_m2 * f_jm2_km2 + wt_m1 * f_jm1_km2 + wt_00 * f_j00_km2
            + wt_p1 * f_jp1_km2 + wt_p2 * f_jp2_km2 + wt_p3 * f_jp3_km2 ;
     f_km1 =  wt_m2 * f_jm2_km1 + wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
            + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 + wt_p3 * f_jp3_km1 ;
     f_k00 =  wt_m2 * f_jm2_k00 + wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
            + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 + wt_p3 * f_jp3_k00 ;
     f_kp1 =  wt_m2 * f_jm2_kp1 + wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
            + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 + wt_p3 * f_jp3_kp1 ;
     f_kp2 =  wt_m2 * f_jm2_kp2 + wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
            + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 + wt_p3 * f_jp3_kp2 ;
     f_kp3 =  wt_m2 * f_jm2_kp3 + wt_m1 * f_jm1_kp3 + wt_00 * f_j00_kp3
            + wt_p1 * f_jp1_kp3 + wt_p2 * f_jp2_kp3 + wt_p3 * f_jp3_kp3 ;

     g_km2 =  wt_m2 * g_jm2_km2 + wt_m1 * g_jm1_km2 + wt_00 * g_j00_km2
            + wt_p1 * g_jp1_km2 + wt_p2 * g_jp2_km2 + wt_p3 * g_jp3_km2 ;
     g_km1 =  wt_m2 * g_jm2_km1 + wt_m1 * g_jm1_km1 + wt_00 * g_j00_km1
            + wt_p1 * g_jp1_km1 + wt_p2 * g_jp2_km1 + wt_p3 * g_jp3_km1 ;
     g_k00 =  wt_m2 * g_jm2_k00 + wt_m1 * g_jm1_k00 + wt_00 * g_j00_k00
            + wt_p1 * g_jp1_k00 + wt_p2 * g_jp2_k00 + wt_p3 * g_jp3_k00 ;
     g_kp1 =  wt_m2 * g_jm2_kp1 + wt_m1 * g_jm1_kp1 + wt_00 * g_j00_kp1
            + wt_p1 * g_jp1_kp1 + wt_p2 * g_jp2_kp1 + wt_p3 * g_jp3_kp1 ;
     g_kp2 =  wt_m2 * g_jm2_kp2 + wt_m1 * g_jm1_kp2 + wt_00 * g_j00_kp2
            + wt_p1 * g_jp1_kp2 + wt_p2 * g_jp2_kp2 + wt_p3 * g_jp3_kp2 ;
     g_kp3 =  wt_m2 * g_jm2_kp3 + wt_m1 * g_jm1_kp3 + wt_00 * g_j00_kp3
            + wt_p1 * g_jp1_kp3 + wt_p2 * g_jp2_kp3 + wt_p3 * g_jp3_kp3 ;

     h_km2 =  wt_m2 * h_jm2_km2 + wt_m1 * h_jm1_km2 + wt_00 * h_j00_km2
            + wt_p1 * h_jp1_km2 + wt_p2 * h_jp2_km2 + wt_p3 * h_jp3_km2 ;
     h_km1 =  wt_m2 * h_jm2_km1 + wt_m1 * h_jm1_km1 + wt_00 * h_j00_km1
            + wt_p1 * h_jp1_km1 + wt_p2 * h_jp2_km1 + wt_p3 * h_jp3_km1 ;
     h_k00 =  wt_m2 * h_jm2_k00 + wt_m1 * h_jm1_k00 + wt_00 * h_j00_k00
            + wt_p1 * h_jp1_k00 + wt_p2 * h_jp2_k00 + wt_p3 * h_jp3_k00 ;
     h_kp1 =  wt_m2 * h_jm2_kp1 + wt_m1 * h_jm1_kp1 + wt_00 * h_j00_kp1
            + wt_p1 * h_jp1_kp1 + wt_p2 * h_jp2_kp1 + wt_p3 * h_jp3_kp1 ;
     h_kp2 =  wt_m2 * h_jm2_kp2 + wt_m1 * h_jm1_kp2 + wt_00 * h_j00_kp2
            + wt_p1 * h_jp1_kp2 + wt_p2 * h_jp2_kp2 + wt_p3 * h_jp3_kp2 ;
     h_kp3 =  wt_m2 * h_jm2_kp3 + wt_m1 * h_jm1_kp3 + wt_00 * h_j00_kp3
            + wt_p1 * h_jp1_kp3 + wt_p2 * h_jp2_kp3 + wt_p3 * h_jp3_kp3 ;

     /* interpolate to kz+fz to get output */

     wt_m1 = Q_M1(fz) ; wt_00 = Q_00(fz) ; wt_p1 = Q_P1(fz) ;
     wt_p2 = Q_P2(fz) ; wt_m2 = Q_M2(fz) ; wt_p3 = Q_P3(fz) ;

     uar[pp] =  wt_m2 * f_km2 + wt_m1 * f_km1 + wt_00 * f_k00
              + wt_p1 * f_kp1 + wt_p2 * f_kp2 + wt_p3 * f_kp3 ;

     var[pp] =  wt_m2 * g_km2 + wt_m1 * g_km1 + wt_00 * g_k00
              + wt_p1 * g_kp1 + wt_p2 * g_kp2 + wt_p3 * g_kp3 ;

     war[pp] =  wt_m2 * h_km2 + wt_m1 * h_km1 + wt_00 * h_k00
              + wt_p1 * h_kp1 + wt_p2 * h_kp2 + wt_p3 * h_kp3 ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Generic interpolation of warp components, given icode specifying method. */

void IW3D_interp( int icode ,
                  int nxx , int nyy , int nzz ,
                  float *aar , float *bar , float *car ,
                  int npp, float *ip, float *jp, float *kp,
                  float *uar , float *var , float *war     )
{
   switch( icode ){
     case MRI_NN:
     case MRI_LINEAR:
       IW3D_interp_linear( nxx , nyy , nzz , aar , bar , car ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     case MRI_CUBIC:
     case MRI_QUINTIC:
       IW3D_interp_quintic( nxx , nyy , nzz , aar , bar , car ,
                            npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     default:
     case MRI_WSINC5:
       IW3D_interp_wsinc5( nxx , nyy , nzz , aar , bar , car ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#undef  NPER
#define NPER 262144  /* 1 Mbyte per float array */

/*---------------------------------------------------------------------------*/
/* Compute B(A(x)) */

IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop;
   float *xda,*yda,*zda , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *CC ;

ENTRY("IW3D_compose") ;

        if( AA == NULL ){ CC = IW3D_copy(BB,1.0f) ; RETURN(CC) ; }
   else if( BB == NULL ){ CC = IW3D_copy(AA,1.0f) ; RETURN(CC) ; }

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xda[qq] ;  /* x+A(x) warped indexes */
       yq[qq-pp] = jj + yda[qq] ;
       zq[qq-pp] = kk + zda[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Interpolate B() warp index displacments at the A() locations */

     IW3D_interp( icode, nx,ny,nz , BB->xd, BB->yd, BB->zd ,
                         qtop-pp  , xq    , yq    , zq     ,
                                    xdc+pp, ydc+pp, zdc+pp  ) ;

     /* Add in the A() displacments to get the total
        index displacment from each original position: A(x) + B(x+A(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 99999 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xdc[qq] += xda[qq] ; ydc[qq] += yda[qq] ; zdc[qq] += zda[qq] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments of length NPER (or less) */

   free(zq) ; free(yq) ; free(xq) ; RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* Compute A^(2^lev) , using linear interpolation only */

IndexWarp3D * IW3D_2pow( IndexWarp3D *AA , int lev )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop , ll ;
   float *xdb,*ydb,*zdb , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *BB , *CC , *TT ;

ENTRY("IW3D_2pow") ;

   if( AA == NULL ) RETURN(NULL) ;  /* duh */

   /* simple case of squaring */

   if( lev == 1 ){ BB = IW3D_compose(AA,AA,MRI_LINEAR) ; RETURN(BB) ; }

   BB = IW3D_copy(AA,1.0f) ;  /* BB = AA (lev=0 result) */

   if( lev <= 0 ) RETURN(BB) ;

   nx = BB->nx ; ny = BB->ny ; nz = BB->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(BB) ;

   /* input = BB ; compute CC = BB(BB(x)) ;
      then swap so output = BB ; wash, rinse, repeat */

   for( ll=1 ; ll <= lev ; ll++ ){

     xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
     xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

     for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

       qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 44444 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xq[qq-pp] = ii + xdb[qq] ;  /* x+B(x) warped indexes */
         yq[qq-pp] = jj + ydb[qq] ;
         zq[qq-pp] = kk + zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

       /* Interpolate B() warp index displacments,
          at the B() locations, into the C() warp */

       IW3D_interp_linear( nx,ny,nz , xdb   , ydb   , zdb   ,
                           qtop-pp  , xq    , yq    , zq    ,
                                      xdc+pp, ydc+pp, zdc+pp ) ;

        /* Add in the B() displacments to get the total
           index displacment from each original position: B(x) + B(x+B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 66666 )
 { int qq ;
#pragma omp for
        for( qq=pp ; qq < qtop ; qq++ ){
          xdc[qq] += xdb[qq] ; ydc[qq] += ydb[qq] ; zdc[qq] += zdb[qq] ;
        }
 }
 AFNI_OMP_END ;

      } /* end of loop over segments of length NPER (or less) */

      /* at this point, CC = BB(BB(x)) ;
         now swap them, to square BB again on next time thru loop */

      TT = CC ; CC = BB ; BB = TT ;
   }

   /* at the end, BB is the result, and CC is trash */

   IW3D_destroy(CC) ; free(zq) ; free(yq) ; free(xq) ; RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Compute B( 2x - A(B(x)) ) = Newton step for computing Ainv(x) */

static float inewtfac = 0.5f ;

static IndexWarp3D * IW3D_invert_newt( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_invert_newt") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as displacements, so we have
      A(x)              = x + a(x)
      B(x)              = x + b(x),
      A(B(x))           = x + b(x) + a(x+b(x))
      2x - A(B(x))      = x - b(x) - a(x+b(x))
      B( 2x - A(B(x)) ) = x - b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = x+b(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xdb[qq] ;
       yq[qq-pp] = jj + ydb[qq] ;
       zq[qq-pp] = kk + zdb[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = a(x+b(x)) */

     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = x - b(x) - a(x+b(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xr[qq-pp] = ii - xdb[qq] - xr[qq-pp] ;
       yr[qq-pp] = jj - ydb[qq] - yr[qq-pp] ;
       zr[qq-pp] = kk - zdb[qq] - zr[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xq,yq,zq] = b(x-b(x)-a(x+b(x))) */

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute result = -b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

     if( inewtfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xr[qq-pp] - ii + xq[qq-pp] ;
         ydc[qq] = yr[qq-pp] - jj + yq[qq-pp] ;
         zdc[qq] = zr[qq-pp] - kk + zq[qq-pp] ;
       }
 }
 AFNI_OMP_END ;

     } else {

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
   register float nf , nf1 ;
   nf = inewtfac ; nf1 = 1.0f - nf ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = nf * (xr[qq-pp]-ii+xq[qq-pp]) + nf1*xdb[qq] ;
         ydc[qq] = nf * (yr[qq-pp]-jj+yq[qq-pp]) + nf1*ydb[qq] ;
         zdc[qq] = nf * (zr[qq-pp]-kk+zq[qq-pp]) + nf1*zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/* Find the inverse warp BB(x) to AA(x).  If not NULL, BBinit is the
   initial estimate for BB(x).  icode tells how to interpolate warps. */

IndexWarp3D * IW3D_invert( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC , nrat , orat ;
   int ii , nnewt=0 , nss , jcode=MRI_LINEAR ;

ENTRY("IW3D_invert") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normLinf( AA , NULL ) ;
   if( normAA == 0.0f ){
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }

   /* BB = initial guess at inverse */

   if( verb_nww     ) ININFO_message(" -- invert max|AA|=%f",normAA) ;
   if( verb_nww > 1 ) HVPRINT("  -",AA) ;

   if( BBinit == NULL ){
     int pp = (int)ceil(log2(normAA)) ; float qq ;
     if( pp < 2 ) pp = 2 ;
     qq = pow(0.5,pp) ;
     if( verb_nww ) ININFO_message("  - init nstep=%d qq=1/2^%d=%f",pp,pp,qq) ;
     BB = IW3D_copy( AA,-qq ) ;
     for( ii=0 ; ii < pp ; ii++ ){
       if( verb_nww > 1 ) ININFO_message("  - init step %d",ii+1) ;
       CC = IW3D_compose(BB,BB,jcode) ; IW3D_destroy(BB) ; BB = CC ;
       if( verb_nww > 1 ) HVPRINT("    -",BB) ;
     }
   } else {
     BB = IW3D_copy( BBinit , 1.0f ) ;
   }

   normAA  = IW3D_normL2( AA , NULL ) ;
   inewtfac = 2.0f / (2.0f+sqrtf(normAA)) ;  /* Newton damping factor */
   if( inewtfac > 0.333f ) inewtfac = 0.333f ;

   if( verb_nww )
     ININFO_message("  - start iterations: normAA=%f inewtfac=%f",normAA,inewtfac) ;

   /* iterate some, until convergence or exhaustion */

   nrat = 666.666f ;

   for( nss=ii=0 ; ii < 69 ; ii++ ){

     /* take a Newton step */

     CC = BB ; BB = IW3D_invert_newt(AA,CC,jcode) ;

     /* how close are they now? */

     normBC = IW3D_normL2( BB , CC ) ; IW3D_destroy(CC) ;

     orat = nrat ; nrat = normBC / normAA ;

     if( verb_nww     ) ININFO_message("  - iterate %d nrat=%f",++nnewt,nrat) ;
     if( verb_nww > 1 ) HVPRINT("    -",BB) ;

     /* check for convergence of B and C */

     if( jcode != icode && nrat < 0.002f ){
       jcode = icode ; nss = 0 ;
       if( verb_nww ) ININFO_message("  - switching from linear interp") ;
       continue ;
     }

     if( nrat < 0.0001f ){
       if( verb_nww ) ININFO_message(" -- iteration converged") ;
       RETURN(BB) ;   /* converged */
     }

     if( nss > 0 && nrat < 0.199f && nrat < orat && inewtfac < 0.888888f ){
       nss = 0 ; inewtfac *= 1.444f ; if( inewtfac > 0.888888f ) inewtfac = 0.888888f ;
       if( verb_nww > 1 ) ININFO_message("  - switch to inewtfac=%f",inewtfac) ;
     } else if( nss > 0 && nrat > orat ){
       nss = -66 ; inewtfac *= 0.444f ;
       if( verb_nww > 1 ) ININFO_message("  - switch to inewtfac=%f",inewtfac) ;
     } else {
       nss++ ;
     }

   }

   /* failed to converge, return latest result anyhoo */

   WARNING_message("invert: iterations failed to converge") ;
   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Iteration step for sqrt:  Bnew(x) = B( 1.5*x - 0.5*A(B(B(x))) )
   This is actually a step to produce the square root of inverse(A). */

static float sstepfac = 0.5f ;

static IndexWarp3D * IW3D_sqrtinv_step( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_sqrtinv_step") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as displacments:
       A(x) = x + a(x)
       B(x) = x + b(x)  et cetera */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = B(x) = x+b(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xdb[qq] ;
       yq[qq-pp] = jj + ydb[qq] ;
       zq[qq-pp] = kk + zdb[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(B(x)) */

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = B(B(x)) = B(x) + b(B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 99999 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xr[qq-pp] += xq[qq-pp] ;
       yr[qq-pp] += yq[qq-pp] ;
       zr[qq-pp] += zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xq,yq,zq] = a(B(B(x))) */

     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute [xq,yq,zq] = 1.5*x - 0.5*A(B(B(x)))
                           = 1.5*x - 0.5*( B(B(x)) + a(B(B(x))) ) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = 1.5f*ii - 0.5f*( xr[qq-pp] + xq[qq-pp] ) ;
       yq[qq-pp] = 1.5f*jj - 0.5f*( yr[qq-pp] + yq[qq-pp] ) ;
       zq[qq-pp] = 1.5f*kk - 0.5f*( zr[qq-pp] + zq[qq-pp] ) ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(1.5*x - 0.5*A(B(B(x)))) */

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute the answer: B(1.5*x - 0.5*A(B(B(x))))
                          = 1.5*x - 0.5*A(B(B(x))) + b(1.5*x - 0.5*A(B(B(x)))) */

     if( sstepfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xq[qq-pp] + xr[qq-pp] - ii ; /* must subtract off x [ii,jj,kk] */
         ydc[qq] = yq[qq-pp] + yr[qq-pp] - jj ; /* to make result be displacments */
         zdc[qq] = zq[qq-pp] + zr[qq-pp] - kk ;
       }
 }
 AFNI_OMP_END ;

     } else {

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
   register float sf , sf1 ;
   sf = sstepfac ; sf1 = 1.0f - sf ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = sf * (xq[qq-pp] + xr[qq-pp] - ii) + sf1 * xdb[qq] ;
         ydc[qq] = sf * (yq[qq-pp] + yr[qq-pp] - jj) + sf1 * ydb[qq] ;
         zdc[qq] = sf * (zq[qq-pp] + zr[qq-pp] - kk) + sf1 * zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/* Find the inverse square root of warp AA(x):
      the warp BB(x) such that AA(BB(BB(x))) = identity.
   If you want the square root of AA(x), then either invert AA
   before calling this function, or invert the result afterwards. */

IndexWarp3D * IW3D_sqrtinv( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC , nrat , orat ;
   int ii , nstep=0 , nss , jcode=MRI_LINEAR ;

ENTRY("IW3D_sqrtinv") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normLinf( AA , NULL ) ;
   if( normAA == 0.0f ){
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }

   /* BB = initial guess at inverse square root */

   if( verb_nww     ) ININFO_message(" -- sqrtinv max|AA|=%f",normAA) ;
   if( verb_nww > 1 ) HVPRINT("  -",AA) ;

   if( BBinit == NULL ){
     int pp = (int)ceil(log2(normAA)) ; float qq ;
     if( pp < 2 ) pp = 2 ;
     qq = pow(0.5,pp+1.0) ;
     if( verb_nww ) ININFO_message("  - init nstep=%d qq=1/2^%d=%f",pp,pp+1,qq) ;
     BB = IW3D_copy( AA,-qq ) ;
     for( ii=0 ; ii < pp ; ii++ ){
       if( verb_nww > 1 ) ININFO_message("  - init step %d",ii+1) ;
       CC = IW3D_compose(BB,BB,jcode) ; IW3D_destroy(BB) ; BB = CC ;
       if( verb_nww > 1 ) HVPRINT("    -",BB) ;
     }
   } else {
     BB = IW3D_copy( BBinit , 1.0f ) ;
   }

   normAA   = IW3D_normL2( AA , NULL ) ;
   sstepfac = 1.0f / (1.0f+sqrtf(normAA)) ;  /* Newton damping factor */
   if( sstepfac > 0.3f ) sstepfac = 0.3f ;

   if( verb_nww )
     ININFO_message("  - start iterations: normAA=%f sstepfac=%f",normAA,sstepfac) ;

   /* iterate some, until convergence or exhaustion */

   nrat = 666.666f ;

   for( nss=ii=0 ; ii < 49 ; ii++ ){

     /* take a step */

     CC = BB ; BB = IW3D_sqrtinv_step(AA,CC,jcode) ;

     /* how close are they now? */

     normBC = IW3D_normL2( BB , CC ) ; IW3D_destroy(CC) ;

     orat = nrat ; nrat = normBC / normAA ;

     if( verb_nww     ) ININFO_message("  - iterate %d nrat=%f",++nstep,nrat) ;
     if( verb_nww > 1 ) HVPRINT("    -",BB) ;

     /* check for convergence of B and C */

     if( jcode != icode && nrat < 0.002f ){
       jcode = icode ; nss = 0 ;
       if( verb_nww ) ININFO_message("  - switching from linear interp") ;
       continue ;
     }

     if( nrat < 0.0001f ){
       if( verb_nww ) ININFO_message(" -- iteration converged") ;
       RETURN(BB) ;   /* converged */
     }

     if( nss > 0 && nrat < 0.199f && nrat < orat && sstepfac < 0.555555f ){
       nss = 0 ; sstepfac *= 1.333f ; if( sstepfac > 0.555555f ) sstepfac = 0.555555f ;
       if( verb_nww > 1 ) ININFO_message("  - switch to sstepfac=%f",sstepfac) ;
     } else if( nss > 0 && nrat > orat ){
       nss = -66 ; sstepfac *= 0.444f ;
       if( verb_nww > 1 ) ININFO_message("  - switch to sstepfac=%f",sstepfac) ;
     } else {
       nss++ ;
     }

   }

   /* failed to converge, return latest result anyhoo */

   WARNING_message("sqrt: iterations failed to converge") ;
   RETURN(BB) ;
}

/****************************************************************************/
/****************************************************************************/

#undef AFF_PARAM
#undef AFF_MATRIX

#define AFF_PARAM  1
#define AFF_MATRIX 2

static int affmode = AFF_PARAM ;

/*---------------------------------------------------------------------------*/
/* Create a warp from a set of parameters, matching a template warp. */

IndexWarp3D * IW3D_from_poly( int npar, float *par, IndexWarp3D *WW )
{
   GA_warpfunc *wfunc ; char *wname ;
   int nall , ii,jj,kk , nx,ny,nz,nxy,nxyz , pp,qq,qtop ;
   IndexWarp3D *AA ;
   float *xda,*yda,*zda , *xq,*yq,*zq , afpar[12] ;

ENTRY("IW3D_from_poly") ;

   if( par == NULL || WW == NULL ) RETURN(NULL) ;  /* should not happen */

   /* cmat takes ijk -> xyz  ;  imat is cmat's inverse */

   mri_genalign_affine_set_befafter( &(WW->cmat) , &(WW->imat) ) ;

   /* choose the warping function, based on number of parameters */

   switch( npar ){
     default: RETURN(NULL) ;
     case  64: wfunc = mri_genalign_cubic   ; wname = "poly3"  ; break ;
     case 172: wfunc = mri_genalign_quintic ; wname = "poly5"  ; break ;
     case 364: wfunc = mri_genalign_heptic  ; wname = "poly7"  ; break ;
     case 664: wfunc = mri_genalign_nonic   ; wname = "poly9"  ; break ;

     case  12:
       if( affmode == AFF_PARAM ){
         wfunc = mri_genalign_affine ; wname = "affine_param" ;
       } else {
         mat44 wmat , qmat , amat ;                  /* create index warp  */
         LOAD_MAT44_AR(amat,par) ;                   /* matrix from coord  */
         wmat = MAT44_MUL(amat,WW->cmat) ;           /* warp matrix, and   */
         qmat = MAT44_MUL(WW->imat,wmat) ;           /* substitute for the */
         UNLOAD_MAT44_AR(qmat,afpar) ; par = afpar ; /* input parameters   */
         wfunc = mri_genalign_mat44  ; wname = "affine_matrix" ;
       }
     break ;
   }

   /* setup the output warp */

   nx = WW->nx ; ny = WW->ny ; nz = WW->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   nxyz = nx*ny*nz ; nall = MIN(nxyz,NPER) ;

   AA = IW3D_empty_copy( WW ) ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   /* send parameters to warping function, for setup */

   if( verb_nww > 1 ) ININFO_message("  - warp name = '%s' has %d parameters",wname,npar) ;

   wfunc( npar , par , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* do the work, Jake */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* input coords are indexes */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 88888 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii ; yq[qq-pp] = jj ; zq[qq-pp] = kk ;
     }
 }
 AFNI_OMP_END ;

     /* compute index-to-index warp */

     wfunc( npar , NULL , qtop-pp , xq,yq,zq , xda+pp,yda+pp,zda+pp ) ;

     /* subtract off base indexes to make the result just be displacments */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 99999 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xda[qq] -= xq[qq-pp] ; yda[qq] -= yq[qq-pp] ; zda[qq] -= zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments */

   /* time to trot, Bwana */

   free(zq) ; free(yq) ; free(xq) ; RETURN(AA) ;
}

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/* interpolate from a float image to a set of indexes */

void THD_interp_floatim( MRI_IMAGE *fim ,
                         int np , float *ip , float *jp , float *kp ,
                         int code, float *outar )
{
ENTRY("THD_interp_floatim") ;

   switch( code ){
     case MRI_NN:      GA_interp_NN     ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_LINEAR:  GA_interp_linear ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_CUBIC:   GA_interp_cubic  ( fim, np,ip,jp,kp, outar ) ; break ;
     default:
     case MRI_QUINTIC: GA_interp_quintic( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_WSINC5:  GA_interp_wsinc5 ( fim, np,ip,jp,kp, outar ) ; break ;
   }

   if( MRI_HIGHORDER(code) ){
     int ii,nn=fim->nvox ; float bot,top , *far=MRI_FLOAT_PTR(fim) ;
     bot = top = far[0] ;
     for( ii=1 ; ii < nn ; ii++ ) if( bot > far[ii] ) bot = far[ii] ;
                             else if( top < far[ii] ) top = far[ii] ;
     for( ii=0 ; ii < np ; ii++ ) if( outar[ii] < bot ) outar[ii] = bot ;
                             else if( outar[ii] > top ) outar[ii] = top ;
   }

   EXRETURN ;
}

#if 0
/*----------------------------------------------------------------------------*/
/* interpolate from 1 image to another, preserving type */

void THD_interp( MRI_IMAGE *inim ,
                 int np , float *ip , float *jp , float *kp ,
                 int code, void *outar )
{
   MRI_IMAGE *fim=inim ; float *far ; register int ii ;

ENTRY("THD_interp") ;

   switch( fim->kind ){

     default:
       ERROR_message("Illegal input type %d in THD_interp()",(int)fim->kind) ;
     break ;

     /*--------------------*/

     case MRI_float:
       THD_interp_floatim( inim , np,ip,jp,kp , code,(float *)outar ) ;
     break ;

     /*--------------------*/

     case MRI_fvect:{
       int kk , vd=inim->vdim ; float *oar=(float *)outar ;
       far = (float *)malloc(sizeof(float)*np) ;
       for( kk=0 ; kk < vd ; kk++ ){
         fim = mri_fvect_subimage(inim,kk) ;
         THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
         for( ii=0 ; ii < np ; ii++ ) oar[ii*vd+kk] = far[ii] ;
         mri_free(fim) ;
       }
       free(far) ;
     }
     break ;

     /*--------------------*/

     case MRI_short:{
       short *sar=(short *)outar ;
       fim = mri_to_float(inim) ; far = (float *)malloc(sizeof(float)*np) ;
       THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
       for( ii=0 ; ii < np ;  ii++ ) sar[ii] = SHORTIZE(far[ii]) ;
       free(far) ; mri_free(fim) ;
     }
     break ;

     /*--------------------*/

     case MRI_byte:{
       byte *bar=(byte *)outar ;
       fim = mri_to_float(inim) ; far = (float *)malloc(sizeof(float)*np) ;
       THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
       for( ii=0 ; ii < np ;  ii++ ) bar[ii] = BYTEIZE(far[ii]) ;
       free(far) ; mri_free(fim) ;
     }
     break ;

     /*--------------------*/

     case MRI_complex:{
       complex *car=(complex *)outar ; MRI_IMARR *imar ; float *gar ;
       far = (float *)malloc(sizeof(float)*np) ;
       gar = (float *)malloc(sizeof(float)*np) ;
       imar = mri_complex_to_pair(inim) ;
       THD_interp_floatim( IMARR_SUBIM(imar,0) , np,ip,jp,kp , code,far ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,1) , np,ip,jp,kp , code,gar ) ;
       for( ii=0 ; ii < np ; ii++ ){ car[ii].r = far[ii]; car[ii].i = gar[ii]; }
       DESTROY_IMARR(imar) ; free(gar) ; free(far) ;
     }
     break ;

     /*--------------------*/

     case MRI_rgb:{
       MRI_IMARR *imar ; float *gar , *har ; byte *bar=(byte *)outar ;
       far = (float *)malloc(sizeof(float)*np) ;
       gar = (float *)malloc(sizeof(float)*np) ;
       har = (float *)malloc(sizeof(float)*np) ;
       imar = mri_rgb_to_3float(inim) ;
       THD_interp_floatim( IMARR_SUBIM(imar,0) , np,ip,jp,kp , code,far ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,1) , np,ip,jp,kp , code,gar ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,2) , np,ip,jp,kp , code,har ) ;
       for( ii=0 ; ii < np ; ii++ ){
         bar[3*ii  ] = BYTEIZE(far[ii]) ;
         bar[3*ii+1] = BYTEIZE(gar[ii]) ;
         bar[3*ii+2] = BYTEIZE(har[ii]) ;
       }
       DESTROY_IMARR(imar) ; free(har) ; free(gar) ; free(far) ;
     }
     break ;

   }

   EXRETURN ;
}
#endif

/*----------------------------------------------------------------------------*/
/* Setup to warp images given
     bimar    = array of DICOM (x,y,z) deltas == 3D warp displacment function
     cmat_bim = matrix to transform indexes (ib,jb,kb) to DICOM (xb,yb,zb)
     cmat_src = similar matrix for source dataset to be warped from
     cmat_out = similar matrix for output dataset to be warped to

   foreach (io,jo,ko) in output dataset do {
     (xo,yo,zo) =    [cmat_out](io,jo,ko)
     (ib,jb,kb) = inv[cmat_bim](xo,yo,zo)
     (xs,ys,zs) = (xo,yo,zo) + bimar interpolated at (ib,jb,kb)
     (is,js,ks) = inv[cmat_src](xs,ys,zs)
   }

   The output is the array of images of (is,js,ks) = indexes in the source
   dataset, for each point to interpolated to in the output dataset (io,jo,ko).
   This set of images can be used, in turn, to interpolate a src grid image
   to an out grid warped image via THD_interp_floatim().
*//*--------------------------------------------------------------------------*/

MRI_IMARR * THD_setup_nwarp( MRI_IMARR *bimar, mat44 cmat_bim ,
                             int incode      , float wfac     ,
                             mat44 cmat_src  ,
                             mat44 cmat_out  ,
                             int nx_out      , int ny_out     , int nz_out  )
{
   int nx,ny,nz,nxy,nxyz ;
   float *xp, *yp, *zp ;
   MRI_IMAGE *wxim, *wyim, *wzim ; MRI_IMARR *wimar ; mat44 tmat ;

ENTRY("THD_setup_nwarp") ;

   if( bimar == NULL ) RETURN(NULL) ;

   nx = nx_out ; ny = ny_out ; nz = nz_out ; nxy = nx*ny ; nxyz = nxy*nz ;

   /* space for indexes/coordinates */

   xp = (float *)malloc(sizeof(float)*nxyz) ;
   yp = (float *)malloc(sizeof(float)*nxyz) ;
   zp = (float *)malloc(sizeof(float)*nxyz) ;

   if( !MAT44_FLEQ(cmat_bim,cmat_out) ){
     int qq,ii,jj,kk ; mat44 imat_out_to_bim ;

     tmat = MAT44_INV(cmat_bim) ; imat_out_to_bim = MAT44_MUL(tmat,cmat_out) ;

     /* compute indexes of each point in output image
        (the _out grid) in the warp space (the _bim grid),
        using the imat_out_to_bim matrix computed just above */

     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       MAT44_VEC( imat_out_to_bim , ii,jj,kk , xp[qq],yp[qq],zp[qq] ) ;
     }

   } else {   /* case where cmat_bim and cmat_out are equal */
     int qq,ii,jj,kk ;

     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xp[qq] = ii ; yp[qq] = jj ; zp[qq] = kk ;
     }

   }

   /* now interpolate the warp delta volumes from the bim
      grid to the out grid, using the indexes computed just above */

   wxim = mri_new_vol(nx,ny,nz,MRI_float) ;
   THD_interp_floatim( IMARR_SUBIM(bimar,0), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wxim) ) ;
   wyim = mri_new_vol(nx,ny,nz,MRI_float) ;
   THD_interp_floatim( IMARR_SUBIM(bimar,1), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wyim) ) ;
   wzim = mri_new_vol(nx,ny,nz,MRI_float) ;
   THD_interp_floatim( IMARR_SUBIM(bimar,2), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wzim) ) ;

   free(zp) ; zp = MRI_FLOAT_PTR(wzim) ;
   free(yp) ; yp = MRI_FLOAT_PTR(wyim) ;
   free(xp) ; xp = MRI_FLOAT_PTR(wxim) ;

   /* now convert to index warp from src to out space */

   tmat = MAT44_INV(cmat_src) ;  /* takes (x,y,z) to (i,j,k) in src space */

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 33333 )
 { int qq,ii,jj,kk ; float xx,yy,zz , fac ;
   fac = (wfac == 0.0f) ? 1.0f : wfac ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     MAT44_VEC( cmat_out , ii,jj,kk , xx,yy,zz ) ;         /* compute (xo,yo,zo) */
     xx += fac*xp[qq]; yy += fac*yp[qq]; zz += fac*zp[qq]; /* add in the deltas */
     MAT44_VEC( tmat, xx,yy,zz, xp[qq],yp[qq],zp[qq] ) ;   /* ==> to (is,js,ks) */
   }
 }
 AFNI_OMP_END ;

   /* package results for delivery to the (ab)user */

   INIT_IMARR(wimar) ;
   ADDTO_IMARR(wimar,wxim) ; ADDTO_IMARR(wimar,wyim) ; ADDTO_IMARR(wimar,wzim) ;

   RETURN(wimar) ;
}

/*----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_dataset( THD_3dim_dataset *dset_nwarp ,
                                      THD_3dim_dataset *dset_src   ,
                                      THD_3dim_dataset *dset_mast  ,
                                      char *prefix , int interp_code ,
                                      float dxyz_mast , float wfac )
{
   MRI_IMARR *imar_nwarp=NULL , *im_src ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *fim , *wim ; float *ip,*jp,*kp ;
   int nx,ny,nz,nxyz , nvals , kk,iv ;

ENTRY("THD_nwarp_dataset") ;

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL || dset_src == NULL ) RETURN(NULL) ;

   DSET_load(dset_nwarp) ; if( !DSET_LOADED(dset_nwarp) ) RETURN(NULL) ;
   DSET_load(dset_src)   ; if( !DSET_LOADED(dset_src)   ) RETURN(NULL) ;

   if( dset_mast == NULL ) dset_mast = dset_src ;  /* default master */

   if( prefix == NULL || *prefix == '\0' ){ /* fake up a prefix */
     char *cpt ;
     prefix = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
     strcpy( prefix , DSET_PREFIX(dset_src) ) ;
     cpt = strstr(prefix,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
     strcat( prefix , "_nwarp" ) ; if( cpt != NULL ) strcat(prefix,".nii") ;
   }

   /*---------- manufacture the empty shell of the output dataset ----------*/

   if( !ISVALID_MAT44(dset_src->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_src->daxes) ;
   src_cmat = dset_src->daxes->ijk_to_dicom ;

   if( !ISVALID_MAT44(dset_nwarp->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_nwarp->daxes) ;
   nwarp_cmat = dset_nwarp->daxes->ijk_to_dicom ;

   if( dxyz_mast > 0.0f ){
     THD_3dim_dataset *qset ; double dxyz = (double)dxyz_mast ;
     qset = r_new_resam_dset( dset_mast , NULL ,
                              dxyz,dxyz,dxyz ,
                              NULL , RESAM_NN_TYPE , NULL , 0 , 0) ;
     if( qset != NULL ){
       dset_mast = qset ;
       THD_daxes_to_mat44(dset_mast->daxes) ;
     }
   }

   if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) ) /* make sure have */
     THD_daxes_to_mat44(dset_mast->daxes) ;      /* index-to-DICOM matrix */

   mast_cmat = dset_mast->daxes->ijk_to_dicom ;

   nvals    = DSET_NVALS(dset_src) ;
   dset_out = EDIT_empty_copy( dset_mast ) ;  /* create the output dataset! */
   EDIT_dset_items( dset_out ,                /* and patch it up */
                      ADN_prefix    , prefix ,
                      ADN_nvals     , nvals ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   if( DSET_NUM_TIMES(dset_src) > 1 )
     EDIT_dset_items( dset_out ,
                        ADN_ntt   , nvals ,
                        ADN_ttdel , DSET_TR(dset_src) ,
                        ADN_tunits, UNITS_SEC_TYPE ,
                        ADN_nsl   , 0 ,
                      ADN_none ) ;
   else
     EDIT_dset_items( dset_out ,
                        ADN_func_type , ISANAT(dset_out) ? ANAT_BUCK_TYPE
                                                         : FUNC_BUCK_TYPE ,
                      ADN_none ) ;

   /* copy brick info into output */

   THD_copy_datablock_auxdata( dset_src->dblk , dset_out->dblk ) ;
   for( kk=0 ; kk < nvals ; kk++ )
     EDIT_BRICK_FACTOR(dset_out,kk,0.0) ;

   THD_daxes_to_mat44(dset_out->daxes) ;           /* save coord transforms */

   /*----- create warping indexes from warp dataset -----*/

   INIT_IMARR(imar_nwarp) ;
   fim = THD_extract_float_brick(0,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(1,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(2,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   DSET_unload(dset_nwarp) ;

   nx = DSET_NX(dset_out) ;
   ny = DSET_NY(dset_out) ;
   nz = DSET_NZ(dset_out) ; nxyz = nx*ny*nz ;

   /* the actual work of setting up the warp */

   im_src = THD_setup_nwarp( imar_nwarp, nwarp_cmat, interp_code, wfac ,
                             src_cmat , mast_cmat , nx , ny , nz        ) ;

   ip = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,0) ) ;
   jp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,1) ) ;
   kp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,2) ) ;

   DESTROY_IMARR(imar_nwarp) ;

   /*----- warp each sub-brick of the input -----*/

   for( iv=0 ; iv < nvals ; iv++ ){
     fim = THD_extract_float_brick(iv,dset_src) ; DSET_unload_one(dset_src,iv) ;
     wim = mri_new_conforming( fim , MRI_float ) ;
     THD_interp_floatim( fim, nxyz,ip,jp,kp, interp_code, MRI_FLOAT_PTR(wim) ) ;
     EDIT_substitute_brick( dset_out , iv , MRI_float , MRI_FLOAT_PTR(wim) ) ;
     mri_clear_and_free(wim) ;
   }

   DSET_unload(dset_src) ; DESTROY_IMARR(im_src) ;
   RETURN(dset_out) ;
}

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/

#undef  KILL_iwstk
#define KILL_iwstk                                                   \
 do{ if( iwstk != NULL ){                                            \
       int qq; for( qq=0; qq < nstk; qq++ ) IW3D_destroy(iwstk[qq]); \
       free(iwstk) ;                                                 \
 } } while(0)

#undef  ERREX
#define ERREX(sss)                                                     \
  do{ ERROR_message("NwarpCalcRPN('%s')\n"                             \
         "           at '%s': %s" , expr,cmd,sss );                    \
      KILL_iwstk; NI_delete_str_array(sar); FREEIFNN(geomstring);      \
      RETURN(NULL);                                                    \
  } while(0)

#undef  ADDTO_iwstk
#define ADDTO_iwstk(W)                                                       \
 do{ iwstk = (IndexWarp3D **)realloc(iwstk,sizeof(IndexWarp3D *)*(nstk+1)) ; \
     iwstk[nstk] = W ; nstk++ ;                                              \
 } while(0)


/*---------------------------------------------------------------------------*/
/* nwarp RPN calculator function */

THD_3dim_dataset * NwarpCalcRPN( char *expr, char *prefix, int icode, int acode )
{
   NI_str_array *sar ;
   char *cmd , acmd[4096] , mess[4096] ;
   IndexWarp3D **iwstk=NULL ;
   int            nstk=0 , ii , ss ;
   IndexWarp3D *AA , *BB ;
   THD_3dim_dataset *oset=NULL ;
   int nx=0,ny=0,nz=0 ;
   mat44 cmat , imat ;      /* cmat: i->x ; imat: x->i */
   char *geomstring=NULL ;

ENTRY("NwarpCalcRPN") ;

   /**----- break string into sub-strings, delimited by whitespace -----**/

   sar = NI_decode_string_list( expr , "`" ) ;
   if( sar == NULL ) RETURN(NULL) ;

   if( acode < 0 ) acode = icode ;

   /**----- loop thru and process commands -----**/

   if(verb_nww)INFO_message("NwarpCalcRPN('%s')",expr) ;

   for( ss=0 ; ss < sar->num ; ss++ ){

     cmd = sar->str[ss] ;

     if(verb_nww)ININFO_message(" + stack size=%d  next operation='%s'",nstk,cmd) ;

     if( *cmd == '\0' ) continue ;  /* WTF?! */

     if( *cmd != '&' ){
       acmd[0] = '&' ; strcpy(acmd+1,cmd) ; cmd = acmd ;  /* a cheap trick */
     }

     /*--- read warp from a dataset ---*/

     else if( strncasecmp(cmd,"&readnwarp(",11) == 0 ){
       char *buf=strdup(cmd+11) , *bp ; THD_3dim_dataset *dset ;
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       dset = THD_open_dataset(buf) ;
       if( dset == NULL ){
         sprintf(mess,"can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       AA = IW3D_from_dataset(dset,0) ; DSET_delete(dset) ;
       if( AA == NULL ){
         sprintf(mess,"can't make warp from '%s'",buf); free(buf); ERREX(mess);
       }
       if( geomstring == NULL ){
         geomstring = strdup(AA->geomstring) ;
         nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
       } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){
         sprintf(mess,"non-conforming warp from '%s'",buf); free(buf); ERREX(mess);
       }
       ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- make identity warp from a dataset ---*/

     else if( strncasecmp(cmd,"&identwarp(",11) == 0 ){
       char *buf=strdup(cmd+11) , *bp ; THD_3dim_dataset *dset ;
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       dset = THD_open_dataset(buf) ;
       if( dset == NULL ){
         sprintf(mess,"can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       AA = IW3D_from_dataset(dset,1) ; DSET_delete(dset) ;
       if( AA == NULL ){
         sprintf(mess,"can't make identwarp from '%s'",buf); free(buf); ERREX(mess);
       }
       if( geomstring == NULL ){
         geomstring = strdup(AA->geomstring) ;
         nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
       } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){
         sprintf(mess,"non-conforming warp from '%s'",buf); free(buf); ERREX(mess);
       }
       ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- create a warp from a set of polynomial parameters ---*/

     else if( strncasecmp(cmd,"&readpoly(",10) == 0 ){
       char *buf=strdup(cmd+10) , *bp ; MRI_IMAGE *qim,*fim ; float *far ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack -- needed for template"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       qim = mri_read_1D(buf) ;
       if( qim == NULL ){
         sprintf(mess,"can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       fim = mri_transpose(qim) ; mri_free(qim) ; far = MRI_FLOAT_PTR(fim) ;
       affmode = AFF_PARAM ;
       AA = IW3D_from_poly( fim->nx , far , iwstk[nstk-1] ) ;
       if( AA == NULL ){
         sprintf(mess,"can't use file '%s' -- num param=%d",buf,fim->nx);
         mri_free(fim); free(buf); ERREX(mess);
       }
       mri_free(fim) ; ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- create a warp from a matrix ---*/

     else if( strncasecmp(cmd,"&read4x4(",9) == 0 ){
       char *buf=strdup(cmd+9) , *bp ; MRI_IMAGE *qim,*fim ; float *far ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack -- needed for template"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       qim = mri_read_1D(buf) ;
       if( qim == NULL ){
         sprintf(mess,"can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       fim = mri_transpose(qim) ; mri_free(qim) ; far = MRI_FLOAT_PTR(fim) ;
       if( fim->nvox < 12 ){
         sprintf(mess,"file '%s' has fewer than 12 numbers",buf);
         free(buf) ; mri_free(fim) ; ERREX(mess) ;
       }
       affmode = AFF_MATRIX ;
       AA = IW3D_from_poly( 12 , far , iwstk[nstk-1] ) ;
       if( AA == NULL ){
         sprintf(mess,"can't make matrix from file '%s'",buf);
         mri_free(fim); free(buf); ERREX(mess);
       }
       mri_free(fim) ; ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- write it out, babee ---*/

     else if( strncasecmp(cmd,"&write(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp ; THD_3dim_dataset *dset ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       AA = iwstk[nstk-1] ; FREEIFNN(AA->geomstring) ;
       AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
       dset = IW3D_to_dataset( AA , buf ) ;
       DSET_write(dset) ;
       if( verb_nww ) ININFO_message(" -- wrote dataset %s",DSET_BRIKNAME(dset)) ;
       DSET_delete(dset) ; free(buf) ;
     }

     /*--- duplication ---*/

     else if( strcasecmp(cmd,"&dup") == 0 ){
       if( nstk < 1 ) ERREX("nothing on stack") ;
       AA = IW3D_copy( iwstk[nstk-1] , 1.0f ) ;
       ADDTO_iwstk(AA) ;
     }

     /*--- pop tart time! ---*/

     else if( strcasecmp(cmd,"&pop") == 0 ){
        if( nstk < 1 ) ERREX("nothing on stack") ;
        IW3D_destroy( iwstk[nstk-1] ) ;
        nstk-- ;
     }

     /*--- swap-eroni ---*/

     else if( strcasecmp(cmd,"&swap") == 0 ){
        if( nstk < 2 ) ERREX("stack too short") ;
        AA = iwstk[nstk-2] ; BB = iwstk[nstk-1] ;
        iwstk[nstk-2] = BB ; iwstk[nstk-1] = AA ;
     }

     /*--- go to Australia (viz., invert) ---*/

     else if( strcasecmp(cmd,"&invert") == 0 || strcasecmp(cmd,"&inverse") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_invert( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inversion failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- invert CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- inverse square root ---*/

     else if( strcasecmp(cmd,"&sqrtinv") == 0 || strcasecmp(cmd,"&invsqrt") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_sqrtinv( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inverse square root failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- inverse square root CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- square root ---*/

     else if( strcasecmp(cmd,"&sqrt") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_sqrtinv( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inverse square root failed :-(") ;
        BB = IW3D_invert( AA , NULL , icode ) ; IW3D_destroy(AA) ;
        if( BB == NULL ) ERREX("inversion after sqrtinv failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = BB ;
        if( verb_nww )
          ININFO_message(" -- square root CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- compose ---*/

     else if( strcasecmp(cmd,"&compose") == 0 || strcasecmp(cmd,"&*") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 2 ) ERREX("stack too short") ;
        AA = IW3D_compose( iwstk[nstk-1] , iwstk[nstk-2] , icode ) ;
        if( AA == NULL ) ERREX("composition failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; IW3D_destroy( iwstk[nstk-2] ) ;
        iwstk[nstk-2] = AA ; nstk-- ;
        if( verb_nww )
          ININFO_message(" -- compose CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- totally square, man ---*/

     else if( strcasecmp(cmd,"&sqr") == 0 || strcasecmp(cmd,"&square") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_compose( iwstk[nstk-1] , iwstk[nstk-1] , icode ) ;
        if( AA == NULL ) ERREX("composition failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ;
        iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- sqr CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- scale ---*/

     else if( strncasecmp(cmd,"&scale(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp ; float val ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       val = (float)strtod(buf,NULL) ; free(buf) ;
       IW3D_scale( iwstk[nstk-1] , val ) ;
     }

     /*--- apply ---*/

     else if( strncasecmp(cmd,"&apply(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp , *pref ;
       THD_3dim_dataset *wset , *iset , *oset ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       for( bp=buf ; *bp != '\0' && *bp != ',' ; bp++ ) ; /*nada*/
       if( *bp != ',' ){ free(buf); ERREX("no comma for prefix"); }
       *bp = '\0' ; pref = bp+1 ;     /* delete comma */
       if( !THD_filename_ok(pref) ){ free(buf); ERREX("illegal prefix"); }
       AA = iwstk[nstk-1] ; FREEIFNN(AA->geomstring) ;
       AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
       iset = THD_open_dataset(buf) ;
       if( iset == NULL ){
         sprintf(mess,"can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       wset = IW3D_to_dataset( AA , buf ) ;
       oset = THD_nwarp_dataset( wset, iset, NULL, pref, acode, 0.0f, 1.0f ) ;
                                               tross_Copy_History  (iset,oset) ;
       sprintf(mess,"NwarpCalcRPN '%s'",cmd) ; tross_Append_History(oset,mess) ;
       DSET_delete(iset) ; DSET_delete(wset) ; DSET_write(oset) ;
       if( verb_nww ) ININFO_message(" -- wrote dataset %s",DSET_BRIKNAME(oset)) ;
       DSET_delete(oset) ; free(buf) ;
     }

     /*--- No worst, there is none ---*/

     else {
       ERREX("unknown operation :-((") ;
     }

   } /*----- end of loop over operations -----*/

   if(verb_nww)INFO_message("end of evaluation loop") ;

   if( nstk > 0 ){
     AA = iwstk[nstk-1] ;
     FREEIFNN(AA->geomstring) ;
     AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
     oset = IW3D_to_dataset( AA , prefix ) ;
   }

   KILL_iwstk ; NI_delete_str_array(sar) ; FREEIFNN(geomstring) ;

   RETURN(oset) ;
}

/*----------------------------------------------------------------------------*/
/***--- Each function goes to 0 at abs(x)=1 and has peak magnitude of 1. ---***/

#undef B0
#undef B1
#undef B2
#undef B3

#define B0(x) (1.0f-(x)*(x))
#define B1(x) (B0(x)*(x)*2.598076f)
#define B2(x) (B0(x)*(1.0f-(x)*(x)*3.0f))
#define B3(x) (B0(x)*((x)*(x)*5.0f-3.0f)*(x)*1.3499593f)

/*----------------------------------------------------------------------------*/

IndexWarp3DBasis * IW3D_polybasis( int lev, float *junk , int nx,int ny,int nz )
{
   IndexWarp3DBasis *iwar ;
   int nbase=4 , ww , ii,jj,kk,pp , nxyz ;
   float xx,yy,zz , fx,fy,fz , *xd,*yd,*zd ;
   float *b0x , *b0y , *b0z ;
   float *b1x , *b1y , *b1z ;
   float *b2x , *b2y , *b2z ;
   float *b3x , *b3y , *b3z ;

   if( nx < 5 || ny < 5 || nz < 5 ) return NULL ;
   nxyz = nx*ny*nz ;

   if( lev < 1 ) lev = 1 ; else if( lev > 3 ) lev = 3 ;

   switch( lev ){
     case 1: nbase =  4 ; break ;
     case 2: nbase = 10 ; break ;
     case 3: nbase = 20 ; break ;
   }

   iwar = (IndexWarp3DBasis *)malloc(sizeof(IndexWarp3DBasis)) ;
   iwar->nbase = nbase ; iwar->nx = nx ; iwar->ny = ny ; iwar->nz = nz ;
   iwar->db = (float **)calloc(sizeof(float),nbase) ;

   b0x = (float *)calloc(sizeof(float),nx) ;
   b0y = (float *)calloc(sizeof(float),ny) ;
   b0z = (float *)calloc(sizeof(float),nz) ;
   b1x = (float *)calloc(sizeof(float),nx) ;
   b1y = (float *)calloc(sizeof(float),ny) ;
   b1z = (float *)calloc(sizeof(float),nz) ;
   b2x = (float *)calloc(sizeof(float),nx) ;
   b2y = (float *)calloc(sizeof(float),ny) ;
   b2z = (float *)calloc(sizeof(float),nz) ;
   b3x = (float *)calloc(sizeof(float),nx) ;
   b3y = (float *)calloc(sizeof(float),ny) ;
   b3z = (float *)calloc(sizeof(float),nz) ;
   fx = 2.0f/(nx-1.0f) ; fy = 2.0f/(ny-1.0f) ; fz = 2.0f/(nz-1.0f) ;
   for( ii=1 ; ii < nx-1 ; ii++ ){
     xx = fx * ii - 1.0f ;
     b0x[ii] = B0(xx) ; b1x[ii] = B1(xx) ; b2x[ii] = B2(xx) ; b3x[ii] = B3(xx) ;
   }
   for( jj=1 ; jj < ny-1 ; jj++ ){
     yy = fy * jj - 1.0f ;
     b0y[jj] = B0(yy) ; b1y[jj] = B1(yy) ; b2y[jj] = B2(yy) ; b3y[jj] = B3(yy) ;
   }
   for( kk=1 ; kk < nz-1 ; kk++ ){
     zz = fz * kk - 1.0f ;
     b0z[kk] = B0(zz) ; b1z[kk] = B1(zz) ; b2z[kk] = B2(zz) ; b3z[kk] = B3(zz) ;
   }

#undef  BLOAD
#define BLOAD(qq,bx,by,bz)                                       \
 do{ float byz , *del ;                                          \
     del = iwar->db[qq] = (float *)malloc(sizeof(float)*nxyz) ;  \
     for( pp=kk=0 ; kk < nz ; kk++ ){                            \
      for( jj=0 ; jj < ny ; jj++ ){                              \
       byz = by[jj]*bz[kk] ;                                     \
       for( ii=0 ; ii < nx ; ii++,pp++ ) del[pp] = bx[ii]*byz ;  \
     }}                                                          \
 } while(0)

   BLOAD(0,b0x,b0y,b0z) ;
   BLOAD(1,b1x,b0y,b0z) ; BLOAD(2,b0x,b1y,b0z) ; BLOAD(3,b0x,b0y,b1z) ;

   if( lev > 1 ){
     BLOAD(4,b2x,b0y,b0z) ; BLOAD(5,b0x,b2y,b0z) ; BLOAD(6,b0x,b0y,b2z) ;
     BLOAD(7,b1x,b1y,b0z) ; BLOAD(8,b1x,b0y,b1z) ; BLOAD(9,b0x,b1y,b1z) ;

     if( lev > 2 ){
       BLOAD(10,b3x,b0y,b0z) ; BLOAD(11,b0x,b3y,b0z) ; BLOAD(12,b0x,b0y,b3z) ;
       BLOAD(13,b2x,b1y,b0z) ; BLOAD(14,b2x,b0y,b1z) ;
       BLOAD(15,b1x,b2y,b0z) ; BLOAD(16,b1x,b0y,b2z) ;
       BLOAD(17,b0x,b1y,b2z) ; BLOAD(18,b0x,b2y,b1z) ;
       BLOAD(19,b1x,b1y,b1z) ;
     }
   }

   free(b0x) ; free(b0y) ; free(b0z) ; free(b1x) ; free(b1y) ; free(b1z) ;
   free(b2x) ; free(b2y) ; free(b2z) ; free(b3x) ; free(b3y) ; free(b3z) ;

   return iwar ;
}

#undef B0
#undef B1
#undef B2
#undef B3

/*----------------------------------------------------------------------------*/

IndexWarp3D * IW3D_warpgen( IndexWarp3DBasis *iwar , float *wt , int nsq )
{
   IndexWarp3D *AA , *BB ;
   int nx,ny,nz , nxyz , ii , pp ;
   float *xda, *yda , *zda , *del , wx,wy,wz ;

   if( iwar == NULL || wt == NULL ) return NULL ;
   nx = iwar->nx ; ny = iwar->ny ; nz = iwar->nz ; nxyz = nx*ny*nz ;

   AA = IW3D_create(nx,ny,nz) ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   for( pp=0 ; pp < iwar->nbase ; pp++ ){
     wx = wt[3*pp+0] ; wy = wt[3*pp+1] ; wz = wt[3*pp+2] ; del = iwar->db[pp] ;
     for( ii=0 ; ii < nxyz ; ii++ ){
       xda[ii] += wx * del[ii] ; yda[ii] += wy * del[ii] ; zda[ii] += wz * del[ii] ;
     }
   }

   if( nsq > 0 ){
     BB = IW3D_2pow( AA , nsq ) ; IW3D_destroy(AA) ; AA = BB ;
   }

   return AA ;
}

/*----------------------------------------------------------------------------*/

void IW3D_improve_warp( MRI_IMAGE *basim , MRI_IMAGE *srcim , MRI_IMAGE *wsrcim ,
                        IndexWarp3D *AA , int match_code , int basis_code ,
                        int ibot, int itop, int jbot, int jtop, int kbot, int ktop )
{
   MRI_IMAGE *warpim ;
   IW3D_basisfunc basisfunc ;

   if( basim == NULL || srcim == NULL || AA == NULL ) return ;

   warpim = wsrcim ;
}
