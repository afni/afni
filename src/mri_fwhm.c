#include "mrilib.h"

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

static int dontcheckplus = 0 ;
void FHWM_1dif_dontcheckplus( int i ){ dontcheckplus = i; }

/*---------------------------------------------------------------------------*/
/*! Routine to estimate Gaussian FWHM of data brick, using first differences.
     - A negative return value indicates an error condition in that direction
       (e.g., FWHM(z) == -1.0 when nz == 1).
     - Adapted from original 3dFWHM.c by BD Ward.
     - 31 Oct 2006 -- BOO!
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

  if( im == NULL ) return fw_xyz ;
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
          dfdx     = (fim[ixyz2] - arg) ;
          dfdxsum += dfdx;
          dfdxsq  += dfdx * dfdx;
          countx++ ;
        }
     }

      if( jy+1 < ny ){
        ixyz2 = ixyz+nx ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdy     = (fim[ixyz2] - arg) ;
          dfdysum += dfdy;
          dfdysq  += dfdy * dfdy;
          county++ ;
        }
      }

      if( kz+1 < nz ){
        ixyz2 = ixyz+nxy ;
        if( dontcheckplus || GOOD(ixyz2) ){
          dfdz     = (fim[ixyz2] - arg) ;
          dfdzsum += dfdz;
          dfdzsq  += dfdz * dfdz;
          countz++ ;
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

  dx = lim->dx; dy = lim->dy; dz = lim->dz;

  /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ----*/

  arg = 1.0 - 0.5*(varxx/var);
  sx  = ( arg <= 0.0 || arg >= 1.0 ) ? -1.0f
                                     : 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dx;

  arg = 1.0 - 0.5*(varyy/var);
  sy  = ( arg <= 0.0 || arg >= 1.0 ) ? -1.0f
                                     : 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dy;

  arg = 1.0 - 0.5*(varzz/var);
  sz  = ( arg <= 0.0 || arg >= 1.0 ) ? -1.0f
                                     : 2.35482*sqrt( -1.0 / (4.0*log(arg)) )*dz;

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
  if( lim != im ) mri_free(lim) ;
  return fw_xyz ;
}

/*---------------------------------------------------------------------------*/

static THD_fvec3 (*fester)(MRI_IMAGE *, byte *) = mri_estimate_FWHM_1dif ;

/*---------------------------------------------------------------------------*/
/*! Get FWHM estimates for each sub-brick in a dataset.
    Output image is 3xN where N=# of sub-bricks.
-----------------------------------------------------------------------------*/

MRI_IMAGE * THD_estimate_FWHM_all( THD_3dim_dataset *dset , byte *mask )
{
   int iv , nvals ;
   MRI_IMAGE *bim , *outim ;
   float *outar , fac ;
   THD_fvec3 fw ;

ENTRY("THD_estimate_FWHM_all") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
   outim = mri_new( 3 , nvals , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   for( iv=0 ; iv < nvals ; iv++ ){
     bim = DSET_BRICK(dset,iv) ;
     fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0f ) fac = 1.0f ;
     if( bim->kind != MRI_float || fac != 1.0f )
       bim = mri_scale_to_float( fac , bim ) ;
     fw = fester( bim , mask ) ;
     if( bim != DSET_BRICK(dset,iv) ) mri_free(bim) ;
     UNLOAD_FVEC3( fw , outar[0+3*iv] , outar[1+3*iv] , outar[2+3*iv] ) ;
   }

   RETURN(outim) ;
}
