#include "mrilib.h"

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

static int dontcheckplus = 0 ;
void FHWM_1dif_dontcheckplus( int i ){ dontcheckplus = i; }

/*---------------------------------------------------------------------------*/

THD_fvec3 mriarr_estimate_FWHM_1dif( MRI_IMARR *imar, byte *mask, int unif )
{
   int nar=IMARR_COUNT(imar) , ii ;
   THD_fvec3 sv ;
   float cx,cy,cz , fx,fy,fz ;
   int   nx,ny,nz , nvox , kk ;
   MRI_IMAGE *medim=NULL , *madim=NULL ;
   float     *medar=NULL , *madar=NULL , *sar=NULL ;

   unif = unif && (nar > 2) ;
   nvox = IMARR_SUBIM(imar,0)->nvox ;
   if( unif ){
     MRI_IMARR *qar = IMARR_medmad_bricks( imar ) ;
     medim = IMARR_SUBIM(qar,0); medar = MRI_FLOAT_PTR(medim);
     madim = IMARR_SUBIM(qar,1); madar = MRI_FLOAT_PTR(madim); FREE_IMARR(qar);
     for( kk=0 ; kk < nvox ; kk++ )
       if( madar[kk] != 0.0f ) madar[kk] = 1.0f / madar[kk] ;
   }
   cx = cy = cz = 0.0f ; nx = ny = nz = 0 ;
   for( ii=0 ; ii < nar ; ii++ ){
     if( unif ){
       sar = MRI_FLOAT_PTR( IMARR_SUBIM(imar,ii) ) ;
       for( kk=0 ; kk < nvox ; kk++ ) sar[kk] = (sar[kk]-medar[kk])*madar[kk] ;
     }
     sv = mri_estimate_FWHM_1dif( IMARR_SUBIM(imar,ii) , mask ) ;
     UNLOAD_FVEC3(sv,fx,fy,fz) ;
     /*** INFO_message("  sub-brick[%d]: fx=%g fy=%g fz=%g",ii,fx,fy,fz) ; ***/
     if( fx > 0.0f ){ cx += fx ; nx++ ; }
     if( fy > 0.0f ){ cy += fy ; ny++ ; }
     if( fz > 0.0f ){ cz += fz ; nz++ ; }
   }
   cx = (nx==0) ? -1.0f : cx / nx ;
   cy = (ny==0) ? -1.0f : cy / ny ;
   cz = (nz==0) ? -1.0f : cz / nz ;
   LOAD_FVEC3(sv,cx,cy,cz) ;
   if( unif ){ mri_free(medim); mri_free(madim); }
   return sv ;
}

/*---------------------------------------------------------------------------*/
/*! Routine to estimate Gaussian FWHM of data brick, using first differences.
     - A negative return value indicates an error condition in that direction
       (e.g., FWHM(z) == -1.0 when nz == 1).
     - Adapted from original 3dFWHM.c by BD Ward.
     - 31 Oct 2006 -- BOO!
     - A sister function is in SUMA_GeomComp.c if changes are made to this 
       one, please notify the half-gloved man.
-----------------------------------------------------------------------------*/

THD_fvec3 mri_estimate_FWHM_1dif( MRI_IMAGE *im , byte *mask )
{
  int nx;                       /* number of voxels along x-axis */
  int ny;                       /* number of voxels along y-axis */
  int nz;                       /* number of voxels along z-axis */
  int nxy, nxyz;                /* total number of voxels */
  int ixyz;                     /* voxel index */
  double dx;                    /* voxel size along x-axis */
  double dy;                    /* voxel size along y-axis */
  double dz;                    /* voxel size along z-axis */
  int ix, jy, kz, ixyz2;
  double fsum, fsq, var , arg ;
  double dfdx, dfdxsum, dfdxsq, varxx;
  double dfdy, dfdysum, dfdysq, varyy;
  double dfdz, dfdzsum, dfdzsq, varzz;
  int count, countx, county, countz;

  float sx=-1.0f,sy=-1.0f,sz=-1.0f ;
  THD_fvec3 fw_xyz ;
  MRI_IMAGE *lim ; float *fim ;

  /*----- initialize local variables -----*/

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;  /* load with error flags */

  if( im == NULL || mri_allzero(im) ) return fw_xyz ;
  lim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
  fim = MRI_FLOAT_PTR(lim) ;
  nx = lim->nx; ny = lim->ny; nz = lim->nz; nxy = nx*ny; nxyz = nx*ny*nz;

  /*----- estimate the variance of the data -----*/

  fsum = 0.0; fsq = 0.0; count = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++){
    if( GOOD(ixyz) ){ count++; arg = fim[ixyz]; fsum += arg; fsq  += arg*arg; }
  }
  if( count < 9 || fsq <= 0.0 ){     /* no data? */
    if( lim != im ) mri_free(lim) ;
    return fw_xyz ;
  }
  var = (fsq - (fsum * fsum)/count) / (count-1.0);
  if( var <= 0.0 ){                  /* crappy data? */
    if( lim != im ) mri_free(lim) ;
    return fw_xyz ;
  }

  /*----- estimate the partial derivatives -----*/

  dfdxsum = 0.0;   dfdysum = 0.0;   dfdzsum = 0.0;
  dfdxsq  = 0.0;   dfdysq  = 0.0;   dfdzsq  = 0.0;
  countx  = 0;     county  = 0;     countz  = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++){
    if( GOOD(ixyz) ){

      arg = fim[ixyz] ;
      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);

      if( ix+1 < nx ){
        ixyz2 = ixyz+1 ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdx = (fim[ixyz2] - arg) ;
          dfdxsum += dfdx; dfdxsq  += dfdx * dfdx; countx++;
        }
     }

      if( jy+1 < ny ){
        ixyz2 = ixyz+nx ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdy = (fim[ixyz2] - arg) ;
          dfdysum += dfdy; dfdysq  += dfdy * dfdy; county++;
        }
      }

      if( kz+1 < nz ){
        ixyz2 = ixyz+nxy ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdz = (fim[ixyz2] - arg) ;
          dfdzsum += dfdz; dfdzsq  += dfdz * dfdz; countz++;
        }
      }
    }
  }

  /*----- estimate the variance of the partial derivatives -----*/

  varxx = (countx < 6) ? 0.0
                       : (dfdxsq - (dfdxsum * dfdxsum)/countx) / (countx-1.0);

  varyy = (county < 6) ? 0.0
                       : (dfdysq - (dfdysum * dfdysum)/county) / (county-1.0);

  varzz = (countz < 6) ? 0.0
                       : (dfdzsq - (dfdzsum * dfdzsum)/countz) / (countz-1.0);

  /*----- now estimate the FWHMs -----*/
  /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ----*/

  dx = lim->dx; dy = lim->dy; dz = lim->dz;

  arg = 1.0 - 0.5*(varxx/var);
  if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dx;

  arg = 1.0 - 0.5*(varyy/var);
  if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dy;

  arg = 1.0 - 0.5*(varzz/var);
  if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dz;

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  if( lim != im ) mri_free(lim) ;
  return fw_xyz ;
}

/*---------------------------------------------------------------------------*/
/*! Routine to estimate Gaussian FWHM of data brick, using differences
    between 1st and 2cd nearest neighbors.
     - A negative return value indicates an error condition in that direction
       (e.g., FWHM(z) == -1.0 when nz == 1).
-----------------------------------------------------------------------------*/

THD_fvec3 mri_estimate_FWHM_12dif( MRI_IMAGE *im , byte *mask )
{
  int nx;                       /* number of voxels along x-axis */
  int ny;                       /* number of voxels along y-axis */
  int nz;                       /* number of voxels along z-axis */
  int nxy, nxyz;                /* total number of voxels */
  int ixyz;                     /* voxel index */
  double dx;                    /* voxel size along x-axis */
  double dy;                    /* voxel size along y-axis */
  double dz;                    /* voxel size along z-axis */
  int ix, jy, kz, qm,qp ;
  double dx1 , dx1sum , dx1sqq , vx1,vy1,vz1 , arg ;
  double dx2 , dx2sum , dx2sqq , vx2,vy2,vz2 ;
  double dy1 , dy1sum , dy1sqq ;
  double dy2 , dy2sum , dy2sqq ;
  double dz1 , dz1sum , dz1sqq ;
  double dz2 , dz2sum , dz2sqq ;
  int countx, county, countz;

  float sx=-1.0f,sy=-1.0f,sz=-1.0f ;
  THD_fvec3 fw_xyz ;
  MRI_IMAGE *lim ; float *fim ;

  /*----- initialize local variables -----*/

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;  /* load with error flags */

  if( im == NULL ) return fw_xyz ;
  lim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
  fim = MRI_FLOAT_PTR(lim) ;
  nx  = lim->nx; ny = lim->ny; nz = lim->nz; nxy = nx*ny; nxyz = nx*ny*nz;

  /*----- loop over voxels, compute differences, sum and sum squares -----*/

  countx = county = countz = 0 ;
  dx1sum = dx2sum = dy1sum = dy2sum = dz1sum = dz2sum = 0.0 ;
  dx1sqq = dx2sqq = dy1sqq = dy2sqq = dz1sqq = dz2sqq = 0.0 ;
  for( ixyz=0 ; ixyz < nxyz ; ixyz++ ){
    if( GOOD(ixyz) ){
      arg = fim[ixyz] ; IJK_TO_THREE (ixyz, ix,jy,kz, nx,nxy);

      if( ix-1 >= 0 && ix+1 < nx ){
        qp = ixyz+1 ; qm = ixyz-1 ;
        if( GOOD(qp) && GOOD(qm) ){
          dx1     = fim[qp]-arg ; dx2     = fim[qp]-fim[qm] ;
          dx1sum += dx1         ; dx2sum += dx2             ;
          dx1sqq += dx1*dx1     ; dx2sqq += dx2*dx2         ;
          countx++ ;
        }
      }

      if( jy-1 >= 0 && jy+1 < ny ){
        qp = ixyz+nx ; qm = ixyz-nx ;
        if( GOOD(qp) && GOOD(qm) ){
          dy1     = fim[qp]-arg ; dy2     = fim[qp]-fim[qm] ;
          dy1sum += dy1         ; dy2sum += dy2             ;
          dy1sqq += dy1*dy1     ; dy2sqq += dy2*dy2         ;
          county++ ;
        }
      }

      if( kz-1 >= 0 && kz+1 < nz ){
        qp = ixyz+nxy ; qm = ixyz-nxy ;
        if( GOOD(qp) && GOOD(qm) ){
          dz1     = fim[qp]-arg ; dz2     = fim[qp]-fim[qm] ;
          dz1sum += dz1         ; dz2sum += dz2             ;
          dz1sqq += dz1*dz1     ; dz2sqq += dz2*dz2         ;
          countz++ ;
        }
      }
    }
  }

  /*----- estimate variances of differences -----*/

  vx1 = (countx < 6) ? 0.0 : (dx1sqq - (dx1sum*dx1sum)/countx) / (countx-1.0) ;
  vy1 = (county < 6) ? 0.0 : (dy1sqq - (dy1sum*dy1sum)/county) / (county-1.0) ;
  vz1 = (countz < 6) ? 0.0 : (dz1sqq - (dz1sum*dz1sum)/countz) / (countz-1.0) ;

  vx2 = (countx < 6) ? 0.0 : (dx2sqq - (dx2sum*dx2sum)/countx) / (countx-1.0) ;
  vy2 = (county < 6) ? 0.0 : (dy2sqq - (dy2sum*dy2sum)/county) / (county-1.0) ;
  vz2 = (countz < 6) ? 0.0 : (dz2sqq - (dz2sum*dz2sum)/countz) / (countz-1.0) ;

#if 0
fprintf(stderr,"countx=%d dx1sum=%g dx1sqq=%g vx1=%g\n",countx,dx1sum,dx1sqq,vx1) ;
fprintf(stderr,"countx=%d dx2sum=%g dx2sqq=%g vx2=%g\n",countx,dx2sum,dx2sqq,vx2) ;
#endif

  /*----- now estimate the FWHMs -----*/

  dx = lim->dx; dy = lim->dy; dz = lim->dz;

  if( lim != im ) mri_free(lim) ;

  /*--- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ---*/
  /*--- y = cbrt(12*sqrt(48-120*r+81*r*r)+108*r-80), and then
        x = y/6 - 4/(3*y) - 1/3
        is the real solution to the equation (1-x^4)/(1-x) = r > 1 ;
        here, r = vx2/vx1 = ratio of variances at Delta=2*dx vs. Delta=1*dx;
        x = exp[-dx**2/(4*sigma**2)] = correlation coefficient of neighbors;
        we solve for x, then use that to solve for sigma, and scale to FWHM --*/

  if( vx1 > 0.0 && vx2 > vx1 ){
    arg = vx2 / vx1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dx ;
  }

  if( vy1 > 0.0 && vy2 > vy1 ){
    arg = vy2 / vy1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dy ;
  }

  if( vz1 > 0.0 && vz2 > vz1 ){
    arg = vz2 / vz1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dz ;
  }

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  return fw_xyz ;
}

/*---------------------------------------------------------------------------*/
/*! Routine to estimate Gaussian FWHM of data brick, using differences
    between 1st and 2cd nearest neighbors.
     - A negative return value indicates an error condition in that direction
       (e.g., FWHM(z) == -1.0 when nz == 1).
-----------------------------------------------------------------------------*/

THD_fvec3 mri_estimate_FWHM_12dif_MAD( MRI_IMAGE *im , byte *mask )
{
  int nx;                       /* number of voxels along x-axis */
  int ny;                       /* number of voxels along y-axis */
  int nz;                       /* number of voxels along z-axis */
  int nxy, nxyz;                /* total number of voxels */
  int ixyz;                     /* voxel index */
  float dx;                    /* voxel size along x-axis */
  float dy;                    /* voxel size along y-axis */
  float dz;                    /* voxel size along z-axis */
  int ix, jy, kz, qm,qp ;
  float vx1,vy1,vz1 , arg ;
  float vx2,vy2,vz2 ;
  int countx, county, countz;

  int ngood ; float *dx1ar,*dy1ar,*dz1ar , *dx2ar,*dy2ar,*dz2ar ;

  float sx=-1.0f,sy=-1.0f,sz=-1.0f ;
  THD_fvec3 fw_xyz ;
  MRI_IMAGE *lim ; float *fim ;

  /*----- initialize local variables -----*/

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;  /* load with error flags */

  if( im == NULL ) return fw_xyz ;
  lim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
  fim = MRI_FLOAT_PTR(lim) ;
  nx  = lim->nx; ny = lim->ny; nz = lim->nz; nxy = nx*ny; nxyz = nx*ny*nz;

  for( ngood=ixyz=0 ; ixyz < nxyz ; ixyz++ ) if( GOOD(ixyz) ) ngood++ ;
  if( ngood < 9 ) return fw_xyz ;

  dx1ar = (float *)malloc(sizeof(float)*ngood) ;
  dy1ar = (float *)malloc(sizeof(float)*ngood) ;
  dz1ar = (float *)malloc(sizeof(float)*ngood) ;
  dx2ar = (float *)malloc(sizeof(float)*ngood) ;
  dy2ar = (float *)malloc(sizeof(float)*ngood) ;
  dz2ar = (float *)malloc(sizeof(float)*ngood) ;

  /*----- loop over voxels, compute differences, sum and sum squares -----*/

  countx = county = countz = 0 ;
  for( ixyz=0 ; ixyz < nxyz ; ixyz++ ){
    if( GOOD(ixyz) ){
      arg = fim[ixyz] ; IJK_TO_THREE (ixyz, ix,jy,kz, nx,nxy);

      if( ix-1 >= 0 && ix+1 < nx ){
        qp = ixyz+1 ; qm = ixyz-1 ;
        if( GOOD(qp) && GOOD(qm) ){
          dx1ar[countx] = fim[qp]-arg ; dx2ar[countx] = fim[qp]-fim[qm] ; countx++ ;
        }
      }

      if( jy-1 >= 0 && jy+1 < ny ){
        qp = ixyz+nx ; qm = ixyz-nx ;
        if( GOOD(qp) && GOOD(qm) ){
          dy1ar[county] = fim[qp]-arg ; dy2ar[county] = fim[qp]-fim[qm] ; county++ ;
        }
      }

      if( kz-1 >= 0 && kz+1 < nz ){
        qp = ixyz+nxy ; qm = ixyz-nxy ;
        if( GOOD(qp) && GOOD(qm) ){
          dz1ar[countz] = fim[qp]-arg ; dz2ar[countz] = fim[qp]-fim[qm] ; countz++ ;
        }
      }
    }
  }

  /*----- estimate variances of differences -----*/

  qmedmad_float( countx , dx1ar , NULL , &vx1 ) ; vx1 = vx1*vx1 ;
  qmedmad_float( county , dy1ar , NULL , &vy1 ) ; vy1 = vy1*vy1 ;
  qmedmad_float( countz , dz1ar , NULL , &vz1 ) ; vz1 = vz1*vz1 ;
  qmedmad_float( countx , dx2ar , NULL , &vx2 ) ; vx2 = vx2*vx2 ;
  qmedmad_float( county , dy2ar , NULL , &vy2 ) ; vy2 = vy2*vy2 ;
  qmedmad_float( countz , dz2ar , NULL , &vz2 ) ; vz2 = vz2*vz2 ;

  /*----- now estimate the FWHMs -----*/

  dx = lim->dx; dy = lim->dy; dz = lim->dz;

  if( lim != im ) mri_free(lim) ;

  /*--- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ---*/
  /*--- y = cbrt(12*sqrt(48-120*r+81*r*r)+108*r-80), and then
        x = y/6 - 4/(3*y) - 1/3
        is the real solution to the equation (1-x^4)/(1-x) = r > 1 ;
        here, r = vx2/vx1 = ratio of variances at Delta=2*dx vs. Delta=1*dx;
        x = exp[-dx**2/(4*sigma**2)] = correlation coefficient of neighbors;
        we solve for x, then use that to solve for sigma, and scale to FWHM --*/

  if( vx1 > 0.0 && vx2 > vx1 ){
    arg = vx2 / vx1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dx ;
  }

  if( vy1 > 0.0 && vy2 > vy1 ){
    arg = vy2 / vy1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dy ;
  }

  if( vz1 > 0.0 && vz2 > vz1 ){
    arg = vz2 / vz1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dz ;
  }

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  return fw_xyz ;
}

/*---------------------------------------------------------------------------*/

static THD_fvec3 (*fester)(MRI_IMAGE *, byte *) = mri_estimate_FWHM_1dif ;

void mri_fwhm_setfester( THD_fvec3 (*func)(MRI_IMAGE *, byte *) )
{
  if( func == NULL ) func = mri_estimate_FWHM_1dif ;
  fester = func ;
}

/*---------------------------------------------------------------------------*/
/*! Get FWHM estimates for each sub-brick in a dataset.
    Output image is 3xN where N=# of sub-bricks.
-----------------------------------------------------------------------------*/

MRI_IMAGE * THD_estimate_FWHM_all( THD_3dim_dataset *dset,
                                   byte *mask, int demed , int unif )
{
   int iv , nvals , ii,nvox ;
   MRI_IMAGE *bim=NULL , *outim=NULL , *medim=NULL , *madim=NULL ;
   float *outar , fac ,  *medar=NULL , *madar=NULL , *bar=NULL ;
   THD_fvec3 fw ;

ENTRY("THD_estimate_FWHM_all") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
   outim = mri_new( 3 , nvals , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   nvox  = DSET_NVOX(dset) ;

   if( unif ){
     MRI_IMARR *imar ;
     demed = 1 ;
     imar  = THD_medmad_bricks(dset) ;
     medim = IMARR_SUBIM(imar,0) ; medar = MRI_FLOAT_PTR(medim) ;
     madim = IMARR_SUBIM(imar,1) ; madar = MRI_FLOAT_PTR(madim) ;
     FREE_IMARR(imar) ;
     for( ii=0 ; ii < nvox ; ii++ )
       if( madar[ii] > 0.0f ) madar[ii] = 1.0f / madar[ii] ;
   } else if( demed ){
     medim = THD_median_brick(dset) ;
     medar = MRI_FLOAT_PTR(medim) ;
   }

   for( iv=0 ; iv < nvals ; iv++ ){
     if( mri_allzero(DSET_BRICK(dset,iv)) ){
       outar[0+3*iv] = outar[1+3*iv] = outar[2+3*iv] = 0.0f ; continue ;
     }
     bim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,iv), DSET_BRICK(dset,iv) );
     if( demed ){
       bar = MRI_FLOAT_PTR(bim) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] -= medar[ii] ;
       if( unif )
        for( ii=0 ; ii < nvox ; ii++ ) bar[ii] *= madar[ii] ;
     }
     fw = fester( bim , mask ) ; mri_free(bim) ;
     UNLOAD_FVEC3( fw , outar[0+3*iv] , outar[1+3*iv] , outar[2+3*iv] ) ;
   }

   if( demed ) mri_free(medim) ;
   if( unif  ) mri_free(madim) ;

   RETURN(outim) ;
}
