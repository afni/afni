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

  /*----- now estimate the FWHMs                                    -----*/
  /*---- the model is the correlation of the neighbors in x is
            exp[ -dx^2 / (4 * sx^2) ]
         which is the result of passing a Gaussian exp[-x^2/(2*sx^2)]
         convolution over a white noise field.  Then the covariance of
         the neigbhors is
            2*V * { 1 - exp[ -dx^2 / (4*sx^2) ] }
         where V = variance of the noise (var).                     -----*/
  /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor -----*/

  dx = lim->dx; dy = lim->dy; dz = lim->dz;

  arg = 1.0 - 0.5*(varxx/var);
/* ININFO_message("dx: varxx/var=%g arg=%g",varxx/var,arg) ; */
  if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dx;

  arg = 1.0 - 0.5*(varyy/var);
  if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dy;
/* ININFO_message("dy: varyy/var=%g arg=%g",varyy/var,arg) ; */

  arg = 1.0 - 0.5*(varzz/var);
  if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(arg)) ) * dz;
/* ININFO_message("dz: varzz/var=%g arg=%g",varzz/var,arg) ; */

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
  float wx1,wy1,wz1 , brg ;
  float wx2,wy2,wz2 ;
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

  qmedmadmeanad_float( countx, dx1ar, NULL, &vx1, &wx1 ); vx1 = vx1*vx1; wx1 = wx1*wx1 ;
  qmedmadmeanad_float( county, dy1ar, NULL, &vy1, &wy1 ); vy1 = vy1*vy1; wy1 = wy1*wy1 ;
  qmedmadmeanad_float( countz, dz1ar, NULL, &vz1, &wz1 ); vz1 = vz1*vz1; wz1 = wz1*wz1 ;
  qmedmadmeanad_float( countx, dx2ar, NULL, &vx2, &wx2 ); vx2 = vx2*vx2; wx2 = wx2*wx2 ;
  qmedmadmeanad_float( county, dy2ar, NULL, &vy2, &wy2 ); vy2 = vy2*vy2; wy2 = wy2*wy2 ;
  qmedmadmeanad_float( countz, dz2ar, NULL, &vz2, &wz2 ); vz2 = vz2*vz2; wz2 = wz2*wz2 ;

  free(dx1ar) ; free(dy1ar) ; free(dz1ar) ;
  free(dx2ar) ; free(dy2ar) ; free(dz2ar) ;

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

  /*--- Modified 11 Aug 2015, to use the mean absolute deviations (wx1 & wx2)
        instead of the median absolute deviations (vx1 & vx2) if the
        computation fails for the median absolute deviations (the backup) ---*/

  if( vx1 > 0.0 && vx2 > vx1 ){
    arg = vx2 / vx1 ; brg = wx2 / wx1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dx ;
    else if( brg > 1.0f ){
      brg = cbrt(12.0*sqrt(48.0 - 120.0*brg + 81.0*brg*brg) + 108.0*brg - 80.0) ;
      brg = brg/6.0 - 4.0/(3.0*brg) - 1.0/3.0 ;
      if( brg > 0.0 && brg < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(brg)) )*dx ;
    }
/* INFO_message("FWHM x: vx1=%f vx2=%f arg=%f sx=%f countx=%d",vx1,vx2,arg,sx,countx) ; */
  } else {
/* INFO_message("FWHM x: vx1=%f vx2=%f NOT GOOD",vx1,vx2) ; */
  }

  if( vy1 > 0.0 && vy2 > vy1 ){
    arg = vy2 / vy1 ; brg = wy2 / wy1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dy ;
    else if( brg > 1.0f ){
      brg = cbrt(12.0*sqrt(48.0 - 120.0*brg + 81.0*brg*brg) + 108.0*brg - 80.0) ;
      brg = brg/6.0 - 4.0/(3.0*brg) - 1.0/3.0 ;
      if( brg > 0.0 && brg < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(brg)) )*dy ;
    }
/* INFO_message("FWHM y: vy1=%f vy2=%f arg=%f sy=%f county=%d",vy1,vy2,arg,sy,county) ; */
  } else {
/* INFO_message("FWHM y: vy1=%f vy2=%f NOT GOOD",vy1,vy2) ; */
  }

  if( vz1 > 0.0 && vz2 > vz1 ){
    arg = vz2 / vz1 ; brg = wz2 / wz1 ;
    arg = cbrt(12.0*sqrt(48.0 - 120.0*arg + 81.0*arg*arg) + 108.0*arg - 80.0) ;
    arg = arg/6.0 - 4.0/(3.0*arg) - 1.0/3.0 ;
    if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dz ;
    else if( brg > 1.0f ){
      brg = cbrt(12.0*sqrt(48.0 - 120.0*brg + 81.0*brg*brg) + 108.0*brg - 80.0) ;
      brg = brg/6.0 - 4.0/(3.0*brg) - 1.0/3.0 ;
      if( brg > 0.0 && brg < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(brg)) )*dz ;
    }
/* INFO_message("FWHM z: vz1=%f vz2=%f arg=%f sz=%f countz=%d",vz1,vz2,arg,sz,countz) ; */
  } else {
/* INFO_message("FWHM z: vz1=%f vz2=%f NOT GOOD",vz1,vz2) ; */
  }

/* INFO_message("12dif_MAD: sxyz = %g %g %g",sx,sy,sz) ; */
  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
/* INFO_message("12dif_MAD: fw = %g %g %g",fw_xyz.xyz[0],fw_xyz.xyz[1],fw_xyz.xyz[2]) ; */
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
   int nvals , ii,nvox ;
   MRI_IMAGE *outim=NULL , *medim=NULL , *madim=NULL ;
   float     *outar,fac  , *medar=NULL , *madar=NULL ;

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

AFNI_OMP_START;
#pragma omp parallel if( nvals > 4 )
 { int iv,ii ; MRI_IMAGE *bim ; float *bar ; THD_fvec3 fw ;
#pragma omp for
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
 }
AFNI_OMP_END;

   if( demed ) mri_free(medim) ;
   if( unif  ) mri_free(madim) ;

   RETURN(outim) ;
}

/*========================================================================*/
static double mom12_stdev_fac = 1.0 ;

void mri_fwhm_mom12_set_stdev_fac( double fff ){ mom12_stdev_fac = fff ; }

/*------------------------------------------------------------------------*/
/*! New method using first and second moments of absolute values of
    differences, instead of only second moments.  [05 Aug 2015]
*//*----------------------------------------------------------------------*/

THD_fvec3 mri_FWHM_1dif_mom12( MRI_IMAGE *im , byte *mask )
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
  double fsum, fsq, var , vfac , arg ;
  double dfdx, dfdy, dfdz ;
  double dfdxs1, dfdxs2, dfdxs3, dfdxs4, dfdxna ;
  double dfdys1, dfdys2, dfdys3, dfdys4, dfdyna ;
  double dfdzs1, dfdzs2, dfdzs3, dfdzs4, dfdzna ;
  int count=0, countx=0, county=0, countz=0 ;

  float sx=-1.0f,sy=-1.0f,sz=-1.0f ;
  MRI_IMAGE *lim ; float *fim ;
  THD_fvec3 fw_xyz ;

  /*----- initialize local variables -----*/

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;  /* load with error flags */

  if( im == NULL || mri_allzero(im) ) return fw_xyz ;
  lim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
  fim = MRI_FLOAT_PTR(lim) ;
  nx = lim->nx; ny = lim->ny; nz = lim->nz; nxy = nx*ny; nxyz = nx*ny*nz;

  /*----- estimate the variance of the data itself -----*/

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
  vfac = 1.0 / sqrt(2.0*var) ;

  /*----- estimate moments of the partial derivatives -----*/
  /*((((( 3rd and 4th moments are not used at present )))))*/

  dfdxs1 = dfdxs2 = dfdxs3 = dfdxs4 = 0.0 ; dfdxna = 0.0 ;
  dfdys1 = dfdys2 = dfdys3 = dfdys4 = 0.0 ; dfdyna = 0.0 ;
  dfdzs1 = dfdzs2 = dfdzs3 = dfdzs4 = 0.0 ; dfdzna = 0.0 ;
  countx = county = countz = 0 ;

  for (ixyz = 0;  ixyz < nxyz;  ixyz++){
    if( GOOD(ixyz) ){

      arg = fim[ixyz] ;
      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);

      if( ix+1 < nx ){
        ixyz2 = ixyz+1 ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdx = vfac*(fim[ixyz2] - arg) ; dfdxna += dfdx ; dfdx = fabs(dfdx) ;
          dfdxs1 += dfdx; dfdxs2 += dfdx*dfdx;
          dfdxs3 += dfdx*dfdx*dfdx; dfdxs4 += dfdx*dfdx*dfdx*dfdx;
          countx++;
        }
     }

      if( jy+1 < ny ){
        ixyz2 = ixyz+nx ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdy = vfac*(fim[ixyz2] - arg) ; dfdyna += dfdy ; dfdy = fabs(dfdy) ;
          dfdys1 += dfdy; dfdys2 += dfdy*dfdy;
          dfdys3 += dfdy*dfdy*dfdy; dfdys4 += dfdy*dfdy*dfdy*dfdy;
          county++;
        }
      }

      if( kz+1 < nz ){
        ixyz2 = ixyz+nxy ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdz = vfac*(fim[ixyz2] - arg) ; dfdzna += dfdz ; dfdz = fabs(dfdz) ;
          dfdzs1 += dfdz; dfdzs2 += dfdz*dfdz;
          dfdzs3 += dfdz*dfdz*dfdz; dfdzs4 += dfdz*dfdz*dfdz*dfdz;
          countz++;
        }
      }
    }
  }
  if( countx == 0 ) countx = 1 ;  /* patch for possible stupidity in data */
  if( county == 0 ) county = 1 ;
  if( county == 0 ) county = 1 ;

  dfdxs1 /= countx; dfdxs2 /= countx; dfdxs3 /= countx; dfdxs4 /= countx;
  dfdys1 /= county; dfdys2 /= county; dfdys3 /= county; dfdys4 /= county;
  dfdzs1 /= countz; dfdzs2 /= countz; dfdzs3 /= countz; dfdzs4 /= countz;

  dfdxna /= countx ; dfdyna /= county ; dfdzna /= countz ;

  dx = lim->dx; dy = lim->dy; dz = lim->dz;
  if( lim != im ) mri_free(lim) ;

#if 0
  ININFO_message(" var= %13.6g",var) ;
  ININFO_message(" dx:  %13.6g  %13.6g  %13.6g  %13.6g  na = %13.6g  #x = %d",
                 dfdxs1 , dfdxs2 , dfdxs3 , dfdxs4 , dfdxna , countx ) ;
  ININFO_message(" dy:  %13.6g  %13.6g  %13.6g  %13.6g  na = %13.6g  #y = %d",
                 dfdys1 , dfdys2 , dfdys3 , dfdys4 , dfdyna , county ) ;
  ININFO_message(" dz:  %13.6g  %13.6g  %13.6g  %13.6g  na = %13.6g  #z = %d",
                 dfdzs1 , dfdzs2 , dfdzs3 , dfdzs4 , dfdzna , countz ) ;
#endif

#undef  SQHPI
#define SQHPI 1.253314  /* sqrt(PI/2) */
#undef  SQEPI
#define SQEPI 0.626657  /* sqrt(PI/8) -- needed for 3rd moment */

  /*--------------------------------------------------------------------------*/
  /*   Model: first differences are distributed like
         ef = Normal( 0 , 2*V*tau^2 )
       where tau = sqrt{ 1 - exp[-del^2/(4*s^2)] }
             V    = variance (var) of data
             del  = spacing of data in mm
             s    = sigma of smoothing kernel
                  = del * sqrt( -1/(4*log(1-tau^2))
                    [s(tau) is a monotonic decreasing function of tau]
             FWHM = sqrt(8*log(2))*s = 2.35482*s
       and we assume that tau in turn is (independently) random, distributed
       with density w(tau) over the interval [0,1].  That is, the density
       function of df = ef/sqrt(2*V) is
             |1
          int|  w(tau) 1/[tau*sqrt(2*pi)] * exp[-df^2/(2*tau^2)] d(tau)
             |0
       Unless w(tau) is a delta function, df will be distributed non-normally.
       Then the moments of the absolute value of df are related to the moments
       of tau by        |1
         E[|df|^p] = int|  w(tau) E[ |Normal(0,tau^2)|^p ] d(tau)
                        |0
                   = 2^(p/2) * Gamma((p+1)/2) / sqrt(pi) * E[tau^p]
         E[|df|]   = sqrt(2/pi) * E[tau]
         E[|df|^2] =              E[tau^2]
         E[|df|^3] = sqrt(8/pi) * E[tau^3]
         E[|df|^4] =          3 * E[tau^4] etc.
       (At this time, we don't use the 3rd or 4th moments, but someday ...?)
       The "standard" (Forman) way to estimate FWHN (mri_estimate_FWHM_1dif)
       assumes s is constant; e.g., that w(tau) = delta(tau-q), in which
       case E[|df|^2] = q^2, and so only the second moment is needed to
       estimate tau=q (and thus FWHM).  In this routine, we use E[|df|] to
       estimate E[tau], and then subtract off one standard deviation
       of tau sqrt(E[tau^2]-E[tau]^2) to weight the estimate towards
       smaller tau and thus larger FWHM -- since larger FWHM will produce
       more "false positive" clusters. Completely ad hoc, but it's something. */
  /*--------------------------------------------------------------------------*/

  { double mn, sg , ff ;
    mn = SQHPI*dfdxs1 ;
    sg = dfdxs2 - mn*mn ;
    if( mn > 0.0 && sg > 0.0 ){
      sg = sqrt(sg) ;
      for( ff=mom12_stdev_fac ; sg*ff >= mn ; ff *= 0.888 ) ; /*nada*/
      mn -= ff*sg ;
    }
    mn *= mn ;
    if( mn > 0.0 && mn < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dx;

    mn = SQHPI*dfdys1 ;
    sg = dfdys2 - mn*mn ;
    if( mn > 0.0 && sg > 0.0 ){
      sg = sqrt(sg) ;
      for( ff=mom12_stdev_fac ; sg*ff >= mn ; ff *= 0.888 ) ; /*nada*/
      mn -= ff*sg ;
    }
    mn *= mn ;
    if( mn > 0.0 && mn < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dy;

    mn = SQHPI*dfdzs1 ;
    sg = dfdzs2 - mn*mn ;
    if( mn > 0.0 && sg > 0.0 ){
      sg = sqrt(sg) ;
      for( ff=mom12_stdev_fac ; sg*ff >= mn ; ff *= 0.888 ) ; /*nada*/
      mn -= ff*sg ;
    }
    mn *= mn ;
    if( mn > 0.0 && mn < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dz;
  }

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  return fw_xyz ;
}

/*========================================================================*/
/*========================================================================*/
/* Return the cluster of points over which we estimage the ACF.
   Use the central NCLU_BASE set of points, and then fill out
   up to NCLU_GOAL from a selection of the outer points,
   so that we don't end up with a vast cluster and slow estimation.
*//*----------------------------------------------------------------------*/

#undef  NCLU_GOAL
#define NCLU_GOAL 666
#undef  NCLU_BASE
#define NCLU_BASE 111

static int do_2D_ACF = 0 ;

void set_ACF_2D( int nn ){ do_2D_ACF = nn ; }

static MCW_cluster * get_ACF_cluster( float dx, float dy, float dz, float radius )
{
   MCW_cluster *clout=NULL , *cltemp=NULL ;
   int pp , dp ;

   /* a ball of desired radius */

   cltemp = MCW_spheremask( dx,dy,dz , radius ) ;

   /* if not too big, just use this */

   if( cltemp->num_pt < NCLU_GOAL ) return cltemp ;

   /* keep the central NCLU_BASE points */

   INIT_CLUSTER(clout) ;
   for( pp=0 ; clout->num_pt < NCLU_BASE && pp < cltemp->num_pt ; pp++ ){
     if( do_2D_ACF && cltemp->k[pp] != 0 ) continue ;
     ADDTO_CLUSTER(clout,cltemp->i[pp],cltemp->j[pp],cltemp->k[pp],0.0f) ;
   }

   /* keep a selection of the outer points, so we don't get too many */

   dp = (int)rintf( (cltemp->num_pt-NCLU_BASE) / (float)(NCLU_GOAL-NCLU_BASE) ) ;
   for( ; pp < cltemp->num_pt ; ){
     if( do_2D_ACF && cltemp->k[pp] != 0 ){
       pp++ ;
     } else {
       ADDTO_CLUSTER(clout,cltemp->i[pp],cltemp->j[pp],cltemp->k[pp],0.0f) ;
       pp += dp ;
     }
   }

   KILL_CLUSTER(cltemp) ;
   MCW_radsort_cluster(clout,dx,dy,dz) ;
   return clout ;
}
/*----------------------------------------------------------------------------*/
/* Estimate the shape of the spatial autocorrelation function   [09 Nov 2015] */

int mri_estimate_ACF( MRI_IMAGE *im , byte *mask , MCW_cluster *clout )
{
   float dx,dy,dz ;
   MRI_IMAGE *lim ; float *fim ;
   int nx,ny,nz,nxy,nxyz , ixyz,ip,jp,kp,ppp,qqq , ix,jy,kz ;
   double fsum, fsq, fbar, fvar , arg ;
   int count , nclu ;
   double *acfsqq ;
   int    *nacf ;

ENTRY("mri_estimate_ACF") ;

   if( im    == NULL || mri_allzero(im)   ) RETURN(-1) ;
   if( clout == NULL || clout->num_pt < 5 ) RETURN(-1) ;

   for( ppp=0 ; ppp < clout->num_pt ; ppp++ ) clout->mag[ppp] = 0.0f ;

   /*-- setup to process the input image --*/

   lim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
   fim = MRI_FLOAT_PTR(lim) ;
   nx = lim->nx; ny = lim->ny; nz = lim->nz; nxy = nx*ny; nxyz = nx*ny*nz;

   /*----- estimate the variance of the data -----*/

   fsum = 0.0; fsq = 0.0; count = 0;
   for (ixyz = 0;  ixyz < nxyz;  ixyz++){
     if( GOOD(ixyz) ){ count++; arg = fim[ixyz]; fsum += arg; fsq += arg*arg; }
   }
   if( count < 9 || fsq <= 0.0 ){     /* no data? */
     if( lim != im ) mri_free(lim) ;
     RETURN(-1) ;
   }
   fbar = fsum / count ;
   fvar = (fsq - (fsum * fsum)/count) / (count-1.0);
   if( fvar <= 0.0 ){
     if( lim != im ) mri_free(lim) ;
     RETURN(-1) ;
   }

   /*--- space to accumulate results ---*/

   nclu   = clout->num_pt ;
   acfsqq = (double *)calloc(sizeof(double),nclu) ;
   nacf   = (int    *)calloc(sizeof(int)   ,nclu) ;

   /*--- loop over voxels, sum up cross products ---*/

   for( ixyz=0 ; ixyz < nxyz ; ixyz++ ){

     if( !GOOD(ixyz) ) continue ;  /* not in mask */

     arg = fim[ixyz] - fbar ;
     IJK_TO_THREE(ixyz,ix,jy,kz,nx,nxy) ;

     /* loop over offsets */

     { int ppp , ip,jp,kp,qqq ;
       for( ppp=1 ; ppp < nclu ; ppp++ ){
         ip = ix + clout->i[ppp] ; if( ip >= nx || ip < 0 ) continue ;
         jp = jy + clout->j[ppp] ; if( jp >= ny || jp < 0 ) continue ;
         kp = kz + clout->k[ppp] ; if( kp >= nz || kp < 0 ) continue ;
         qqq = THREE_TO_IJK(ip,jp,kp,nx,nxy) ; if( !GOOD(qqq) ) continue ;
         acfsqq[ppp] += (fim[qqq]-fbar)*arg ; nacf[ppp]++ ;
       }
     }

   }

   /*--- load results into clout ---*/

   for( ppp=1 ; ppp < nclu ; ppp++ ){
     if( nacf[ppp] > 5 )
       clout->mag[ppp] = (float)( acfsqq[ppp] / ( fvar * (nacf[ppp]-1.0) ) ) ;
   }
   clout->mag[0] = 1.0f ;

   free(acfsqq) ; free(nacf) ;
   if( lim != im ) mri_free(lim) ;
   RETURN(0) ;
}

/*----------------------------------------------------------------------------*/
/*! Get average ACF estimate for all sub-bricks                  [09 Nov 2015]
*//*--------------------------------------------------------------------------*/

MCW_cluster * THD_estimate_ACF( THD_3dim_dataset *dset,
                                byte *mask, int demed, int unif, float radius )
{
   int iv , nvals , ii,nvox , nout ;
   MRI_IMAGE *medim=NULL , *madim=NULL ;
   float     *medar=NULL , *madar=NULL ;
   MCW_cluster *clout=NULL , **cltemp ;

ENTRY("THD_estimate_ACF") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
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

   set_ACF_2D( DSET_NZ(dset)==1 ) ;

   clout = get_ACF_cluster( fabsf(DSET_DX(dset)) , fabsf(DSET_DY(dset)) ,
                            fabsf(DSET_DZ(dset)) , radius ) ;
   if( clout == NULL ){
     if( medim != NULL ) mri_free(medim) ;
     if( madim != NULL ) mri_free(madim) ;
     RETURN(NULL) ;
   }

   for( ii=0 ; ii < clout->num_pt ; ii++ ) clout->mag[ii] = 0.0f ;
   cltemp = (MCW_cluster **)malloc(sizeof(MCW_cluster *)*nvals) ;

AFNI_OMP_START;
#pragma omp parallel if( nvals > 4 )
 { int iv,gg ; MRI_IMAGE *bim ;
#pragma omp for
   for( iv=0 ; iv < nvals ; iv++ ){
     bim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,iv), DSET_BRICK(dset,iv) );
     bim->dx = DSET_DX(dset) ; bim->dy = DSET_DY(dset) ; bim->dz = DSET_DZ(dset) ;
     if( demed ){
       float *bar = MRI_FLOAT_PTR(bim) ; int ii ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] -= medar[ii] ;
       if( unif )
        for( ii=0 ; ii < nvox ; ii++ ) bar[ii] *= madar[ii] ;
     }
     COPY_CLUSTER(cltemp[iv],clout) ;
     gg = mri_estimate_ACF( bim , mask , cltemp[iv] ) ;
     if( gg < 0 ){
       KILL_CLUSTER(cltemp[iv]) ; cltemp[iv] = NULL ;
     }
   }
 }
AFNI_OMP_END;

   for( nout=iv=0 ; iv < nvals ; iv++ ){
     if( cltemp[iv] != NULL ){
       for( ii=0 ; ii < clout->num_pt ; ii++ ) clout->mag[ii] += cltemp[iv]->mag[ii] ;
       KILL_CLUSTER(cltemp[iv]) ; nout++ ;
     }
   }
   free(cltemp) ;

   if( medim != NULL ) mri_free(medim) ;
   if( madim != NULL ) mri_free(madim) ;

   if( nout > 1 ){
     for( ii=0 ; ii < clout->num_pt ; ii++ )
       clout->mag[ii] /= nout ;
   } else if( nout == 0 ){
     ERROR_message("ACF estimation fails :-(") ;
     KILL_CLUSTER(clout) ; clout = NULL ;
   }

   RETURN(clout) ;
}

/*----------------------------------------------------------------------------*/
/*! Get average ACF FWHM estimate for all images in the array    [30 Dec 2015]
*//*--------------------------------------------------------------------------*/

float mriarr_estimate_FWHM_acf( MRI_IMARR *imar, byte *mask, int unif, float radius )
{
   int iv , nvals , ii,nvox , nout , demed=0 ;
   MRI_IMAGE *medim=NULL , *madim=NULL ;
   float     *medar=NULL , *madar=NULL , dx,dy,dz , fwhm_out=0.0f ;
   MCW_cluster *clout=NULL , **cltemp ;

ENTRY("mriarr_estimate_FWHM_acf") ;

   if( imar == NULL ) RETURN(0.0f) ;

   nvals = IMARR_COUNT(imar) ; if( nvals < 2 ) unif = 0 ;
   nvox  = IMARR_SUBIM(imar,0)->nvox ;
   dx    = IMARR_SUBIM(imar,0)->dx ;
   dy    = IMARR_SUBIM(imar,0)->dy ;
   dz    = IMARR_SUBIM(imar,0)->dz ;

   if( unif ){
     MRI_IMARR *qmar ;
     demed = 1 ;
     qmar  = IMARR_medmad_bricks(imar) ;
     medim = IMARR_SUBIM(qmar,0) ; medar = MRI_FLOAT_PTR(medim) ;
     madim = IMARR_SUBIM(qmar,1) ; madar = MRI_FLOAT_PTR(madim) ;
     FREE_IMARR(qmar) ;
     for( ii=0 ; ii < nvox ; ii++ )
       if( madar[ii] > 0.0f ) madar[ii] = 1.0f / madar[ii] ;
   }

   set_ACF_2D( IMARR_SUBIM(imar,0)->nz == 1 ) ;

   clout = get_ACF_cluster( dx,dy,dz, radius ) ;
   if( clout == NULL ){
     if( medim != NULL ) mri_free(medim) ;
     if( madim != NULL ) mri_free(madim) ;
     RETURN(0.0f) ;
   }

   for( ii=0 ; ii < clout->num_pt ; ii++ ) clout->mag[ii] = 0.0f ;
   cltemp = (MCW_cluster **)malloc(sizeof(MCW_cluster *)*nvals) ;

AFNI_OMP_START;
#pragma omp parallel if( nvals > 4 )
 { int iv,gg ; MRI_IMAGE *bim ;
#pragma omp for
   for( iv=0 ; iv < nvals ; iv++ ){
     bim = mri_to_float( IMARR_SUBIM(imar,iv) ) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ;
     if( demed ){
       float *bar = MRI_FLOAT_PTR(bim) ; int ii ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] -= medar[ii] ;
       if( unif )
        for( ii=0 ; ii < nvox ; ii++ ) bar[ii] *= madar[ii] ;
     }
     COPY_CLUSTER(cltemp[iv],clout) ;
     gg = mri_estimate_ACF( bim , mask , cltemp[iv] ) ;
     if( gg < 0 ){
       KILL_CLUSTER(cltemp[iv]) ; cltemp[iv] = NULL ;
     }
   }
 }
AFNI_OMP_END;

   for( nout=iv=0 ; iv < nvals ; iv++ ){
     if( cltemp[iv] != NULL ){
       for( ii=0 ; ii < clout->num_pt ; ii++ ) clout->mag[ii] += cltemp[iv]->mag[ii] ;
       KILL_CLUSTER(cltemp[iv]) ; nout++ ;
     }
   }
   free(cltemp) ;

   if( medim != NULL ) mri_free(medim) ;
   if( madim != NULL ) mri_free(madim) ;

   if( nout > 1 ){
     for( ii=0 ; ii < clout->num_pt ; ii++ )
       clout->mag[ii] /= nout ;
   } else if( nout == 0 ){
     ERROR_message("ACF estimation fails :-(") ;
     KILL_CLUSTER(clout) ; clout = NULL ;
   }

   if( clout != NULL ){
     float_quad acf_Epar ;
     acf_Epar = ACF_cluster_to_modelE( clout , dx,dy,dz ) ;
     fwhm_out = acf_Epar.d ;
     KILL_CLUSTER(clout) ; clout = NULL ;
   }

   RETURN(fwhm_out) ;
}

/*----------------------------------------------------------------------------*/
/* Function for Powell minimization for ACF modelE fit. */

static int    nar=0 ;
static float *aar=NULL , *rar=NULL ;

double ACF_modelE_costfunc( int npar , double *par )
{
   double aa=par[0] , bb=par[1]      , cc=par[2] ;
   double a1=1.0-aa , bq=0.5/(bb*bb) , ci=1.0/cc ;
   double sum=0.0 , fval ;
   int ii ;

   for( ii=0 ; ii < nar ; ii++ ){
     fval = aa * exp(-bq*rar[ii]*rar[ii]) + a1 * exp(-ci*rar[ii]) - aar[ii] ;
     sum += fval * fval ;
   }

   return sum ;
}

/*----------------------------------------------------------------------------*/

static MRI_IMAGE *ACF_im=NULL ;
MRI_IMAGE * ACF_get_1D(void){ return ACF_im ; }

/*----------------------------------------------------------------------------*/
/*! Convert ACF cluster output to function vs radius             [09 Nov 2015]
    Exponential model function vs. radius r is
      ACF(r) = a*exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)
    where a,b,c are the estimated parameters returned in the float_quad.
    With restrictions that   0 <= a <= 1 ; b > 0 ; c > 0
    The last parameter return (d) is the FWHM of the curve fit.
*//*--------------------------------------------------------------------------*/

float_quad ACF_cluster_to_modelE( MCW_cluster *acf, float dx, float dy, float dz )
{
   int nclu, nr, pp, qq, ss, dq ;
   float xx , yy , zz , rmax , dr , vp,rp , vs ;
   float apar , bpar , cpar , dpar , sig , *acar ;
   float_quad fqout = { -1.0f , -1.0f , -1.0f , -1.0f } ;
   double xpar[3] , xbot[3] , xtop[3] ;

ENTRY("ACF_cluster_to_modelE") ;

   if( ACF_im != NULL ){ mri_free(ACF_im) ; ACF_im = NULL ; }

                        if( acf == NULL ) RETURN(fqout) ;
   nclu = acf->num_pt ; if( nclu < 5    ) RETURN(fqout) ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   /* load rar and aar arrays with radius and ACF values */

#undef  CRAD
#define CRAD(a) ( xx = dx * abs(acf->i[a]) ,   \
                  yy = dy * abs(acf->j[a]) ,   \
                  zz = dz * abs(acf->k[a]) , sqrt(xx*xx+yy*yy+zz*zz) )

   aar = (float *)malloc(sizeof(float)*nclu) ;
   rar = (float *)malloc(sizeof(float)*nclu) ;

   for( pp=0 ; pp < nclu ; pp++ ){
     rar[pp] = CRAD(pp) ;
     aar[pp] = acf->mag[pp] ;
   }

   qsort_floatfloat( nclu , rar , aar ) ;  /* to ensure increasing rar */

   /* collapse data with very similar rar values */

   for( pp=0 ; pp < nclu-1 ; pp++ ){
     vp = aar[pp] ; rp = rar[pp]*1.01f ;
     for( qq=pp+1 ; qq < nclu && rar[qq] <= rp ; qq++ ) ; /*nada*/
     if( qq > pp+1 ){
       for( vs=0.0f,ss=pp ; ss < qq ; ss++ ) vs += aar[ss] ;
       aar[pp] = vs/(qq-pp) ;
       dq = qq-(pp+1) ;
       for( ss=qq ; ss < nclu ; ss++ ){
         aar[ss-dq] = aar[ss]; rar[ss-dq] = rar[ss];
       }
       nclu -= dq ;
     }
   }

   /* crude estimates for model parameters */

   nar = nclu ;

   for( pp=0 ; pp < nclu && aar[pp] > 0.5f ; pp++ ) ; /*nada*/
   if( pp == nclu ){
     ERROR_message("largest ACF found is %g -- too big for model fit!",aar[nclu-1]) ;
     free(aar) ; free(rar) ; aar = rar = NULL ; nar = 0 ;
     RETURN(fqout) ;
   }
   apar = 1.0 / logf(aar[pp]) ;
   bpar = sqrtf(-0.5*apar) * rar[pp] ;
   cpar = -rar[pp] * apar;
   apar = 0.5f ;

   /* nonlinear optimization of parameters */

   xpar[0] = apar  ; xpar[1] = bpar      ; xpar[2] = cpar      ;
   xbot[0] = 0.006 ; xbot[1] = 0.05*bpar ; xbot[2] = 0.05*cpar ;
   xtop[0] = 0.994 ; xtop[1] = 5.55*bpar ; xtop[2] = 5.55*cpar ;

#if 0
   pp = powell_newuoa_con( 3 , xpar , xbot , xtop ,
                           99 , 0.05 , 0.0005 , 999 , ACF_modelE_costfunc ) ;
#else
   pp = powell_newuoa_constrained( 3 , xpar , NULL , xbot , xtop ,
                                   666 , 44 , 9 ,
                                   0.05 , 0.0005 , 999 , ACF_modelE_costfunc ) ;
#endif

   if( pp < 0 ){
     ERROR_message("optimization of ACF model fit fails :-(") ;
     free(aar) ; free(rar) ; aar = rar = NULL ; nar = 0 ;
     RETURN(fqout) ;
   }

   ACF_im = mri_new( nar , 4 , MRI_float ) ;
   acar = MRI_FLOAT_PTR(ACF_im) ;
   apar = xpar[0] ; bpar = xpar[1] ; cpar = xpar[2] ;
   for( pp=0 ; pp < nar ; pp++ ){
     vs =         apar * expf( -0.5f*rar[pp]*rar[pp]/(bpar*bpar) )
         + (1.0f-apar) * expf( -rar[pp]/cpar ) ;
     acar[pp]       = rar[pp] ;  /* radius */
     acar[pp+nar]   = aar[pp] ;  /* empirical ACF */
     acar[pp+2*nar] = vs ;       /* model ACF */
   }

   { floatvec *fv ;
     MAKE_floatvec(fv,nar) ;
     for( pp=0 ; pp < nar ; pp++ ) fv->ar[pp] = acar[pp+2*nar] ;
     vs = interp_inverse_floatvec( fv , 0.5f ) ;
     for( pp=0 ; pp < nar ; pp++ ) fv->ar[pp] = rar[pp] ;
     dpar = 2.0*interp_floatvec( fv , vs ) ; sig = FWHM_TO_SIGMA(dpar) ;
     KILL_floatvec(fv) ;
   }

   for( pp=0 ; pp < nar ; pp++ ){  /* add in Gaussian ACF for fun */
     acar[pp+3*nar] = expf(-0.5f*rar[pp]*rar[pp]/(sig*sig)) ;
   }

   free(aar) ; free(rar) ; aar = rar = NULL ; nar = 0 ;

   fqout.a = xpar[0] ; fqout.b = xpar[1] ; fqout.c = xpar[2] ; fqout.d = dpar ;
   RETURN(fqout) ;
}
