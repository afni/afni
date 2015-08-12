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
/* INFO_message("fester: fw = %g %g %g",fw.xyz[0],fw.xyz[1],fw.xyz[2]) ; */
   }

   if( demed ) mri_free(medim) ;
   if( unif  ) mri_free(madim) ;

   RETURN(outim) ;
}

#if 1
/*------------------------------------------------------------------------*/
static double mom12_stdev_fac = 0.5 ;

mri_fwhm_mom12_set_stdev_fac( double fff ){ mom12_stdev_fac = fff ; }

/*------------------------------------------------------------------------*/
/*! New method using first and second moments of differences, instead
    of only second moments.  [05 Aug 2015]
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
#define SQEPI 0.626657  /* sqrt(PI/8) */

  { double mn, sg ;
    mn = SQHPI*dfdxs1 ;
    sg = dfdxs2 - mn*mn ;
    if( sg > 0.0 ) mn -= mom12_stdev_fac*sqrt(sg) ;
    mn *= mn ;
/* ININFO_message("dx: arg=%g",mn) ; */
    if( mn > 0.0 && mn < 1.0 ) sx = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dx;

    mn = SQHPI*dfdys1 ;
    sg = dfdys2 - mn*mn ;
    if( sg > 0.0 ) mn -= mom12_stdev_fac*sqrt(sg) ;
    mn *= mn ;
/* ININFO_message("dy: arg=%g",mn) ; */
    if( mn > 0.0 && mn < 1.0 ) sy = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dy;

    mn = SQHPI*dfdzs1 ;
    sg = dfdzs2 - mn*mn ;
    if( sg > 0.0 ) mn -= mom12_stdev_fac*sqrt(sg) ;
    mn *= mn ;
/* ININFO_message("dz: arg=%g",mn) ; */
    if( mn > 0.0 && mn < 1.0 ) sz = 2.35482*sqrt( -1.0 / (4.0*log(1.0-mn)) ) * dz;
  }

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  return fw_xyz ;
}
#endif
