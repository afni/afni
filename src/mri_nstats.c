#include "mrilib.h"

/** if using OpenMP, this file should be #include-d into the main program! **/

#ifdef USE_OMP
#include <omp.h>
#include "cs_qmed.c"
#endif

/*--------------------------------------------------------------------------*/
/*! Input = 1D float array, and an NSTAT_ code to compute some statistic.
    Output = statistic's value.
*//*------------------------------------------------------------------------*/

float mri_nstat( int code , int npt , float *far , float voxval )
{
   register float outval ; float val ;

   outval = 0.0f ;

   if( npt <= 0 || far == NULL ) return outval ;

   switch( code ){

     case NSTAT_NUM: outval = (float)npt ; break ;  /* quite easy */

     default:
     case NSTAT_SUM:
     case NSTAT_MEAN:{
       register int ii ;
       for( ii=0 ; ii < npt ; ii++ ) outval += far[ii] ;
       if( code != NSTAT_SUM ) outval /= npt ;
     }
     break ;

     case NSTAT_FNZNUM:
     case NSTAT_NZNUM:{
       register int ii ;
       for( ii=0 ; ii < npt ; ii++ ) if (far[ii] != 0.0f) outval += 1 ;
       if( code != NSTAT_NZNUM) outval /= npt ;
     }
     break ;
     
     case NSTAT_SIGMA:   /* these 3 need the mean and variance sums */
     case NSTAT_CVAR:
     case NSTAT_VAR:{
       register float mm,vv ; register int ii ;
       if( npt == 1 ) break ;                     /* will return 0.0 */
       for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
       mm /= npt ;
       for( vv=0.0,ii=0 ; ii < npt ; ii++ ) vv += (far[ii]-mm)*(far[ii]-mm) ;
       vv /= (npt-1) ;
            if( code == NSTAT_SIGMA ) outval = sqrt(vv) ;
       else if( code == NSTAT_VAR   ) outval = vv ;
       else if( mm   !=  0.0f       ) outval = sqrt(vv) / fabsf(mm) ;
     }
     break ;

     case NSTAT_MEDIAN:
       qmedmad_float( npt , far , &val , NULL ) ; outval = val ;
     break ;

     case NSTAT_MAD:
       qmedmad_float( npt , far , NULL , &val ) ; outval = val ;
     break ;

     case NSTAT_MODE:
       outval = qmode_float( npt , far);
     break ;

     case NSTAT_NZMODE:
       outval = qnzmode_float( npt , far);
     break ;

     case NSTAT_P2SKEW:
       /* Pearson's second skewness coefficient */
       {
          register float mm,vv, sig, mean; register int ii ;
          if( npt == 1 ) break ;                     /* will return 0.0 */
          for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
          mm /= npt ; mean = mm;
          for( vv=0.0,ii=0 ; ii < npt ; ii++ )
                                 vv += (far[ii]-mm)*(far[ii]-mm) ;
          vv /= (npt-1) ;
          sig = sqrt(vv) ;
          if (sig) {
            qmedmad_float( npt , far , &val , NULL ) ;
            outval = 3.0 * (mean - val) / sig;
          } else outval = 0.0;
       }
     break ;

     case NSTAT_KURT:
       /* Kurtosis estimate, unbiased under normality condition */
       {
          register double mm,vv,vv2,pp, sig; register int ii ;
          if( npt < 4  ) break ;                     /* will return 0.0 */
          for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
          mm /= npt ;
          for( vv=0.0,vv2=0.0,ii=0 ; ii < npt ; ii++ ) {
                                 pp = (far[ii]-mm)*(far[ii]-mm) ;
                                 vv += pp; vv2 += pp*pp;
          }
          if (vv != 0.0f) {
            ii = npt-1;
            vv2 = (vv2/(vv*vv))*(npt+1.0)*npt*ii - 3.0*ii*ii;
            outval = (float)(vv2/((npt-2)*(npt-3)));
          } else outval = 0.0;
       }
     break ;

     case NSTAT_MAX:{
       register int ii ;
       outval = far[0] ;
       for( ii=1 ; ii < npt ; ii++ ) if( far[ii] > outval ) outval = far[ii] ;
     }
     break ;

     case NSTAT_MIN:{
       register int ii ;
       outval = far[0] ;
       for( ii=1 ; ii < npt ; ii++ ) if( far[ii] < outval ) outval = far[ii] ;
     }
     break ;

     case NSTAT_ABSMAX:{
       register int ii ; register float vv ;
       outval = fabsf(far[0]) ;
       for( ii=1 ; ii < npt ; ii++ ){
         vv = fabsf(far[ii]) ; if( vv > outval ) outval = vv ;
       }
     }
     break ;

     case NSTAT_RANK:{
       register int ii ;
       qsort_float(npt, far);
       outval = 1.0 ;
       for( ii=1 ; ii < npt ; ii++ ){
         if (voxval > far[ii]) outval = ii;
         else break ;
       }
     }
     break ;

     case NSTAT_FRANK:{
       register int ii ;
       outval = 1.0 ;
       if (npt) {
          qsort_float(npt, far);
          for( ii=1 ; ii < npt ; ii++ ){
            if (voxval > far[ii]) outval = ii;
            else break ;
          }
          outval /= npt;
       }
     }
     break ;

   }

   return outval ;
}

/*--------------------------------------------------------------------------*/
/*!
   A specialized function for speeding up computations for segmentation
    computes  5 statistic values: mean, median, sigma, mad, and skew and
    stores them in fv5
*/
/*------------------------------------------------------------------------*/

int mri_nstat_mMP2S( int npt , float *far , float voxval, float *fv5)
{
   /*             fv5[5]={mean, median, sigma, MAD, skew} */
   register float mm,vv;
   register int ii ;

   fv5[0] = fv5[1] = fv5[2] = fv5[3] = fv5[4] = 0.0;
   if( npt <= 0 || far == NULL ) return 0 ;
   if ( npt == 1 ) {
      fv5[0] = fv5[1] = voxval ;
      return 1;
   }

   for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
   mm /= npt ; fv5[0] = mm;
   for( vv=0.0,ii=0 ; ii < npt ; ii++ )
                           vv += (far[ii]-mm)*(far[ii]-mm) ;
   vv /= (npt-1) ;
   fv5[2] = sqrt(vv) ;
   if (fv5[2]) {
     qmedmad_float( npt , far , fv5+1 , fv5+3 ) ;
     fv5[4] = 3.0 * (fv5[0] - fv5[1]) / fv5[2];
   } else fv5[4] = 0.0;


   return 1 ;
}

/* Compute differences fom the central value far[0] */
int mri_nstat_diffs( int npt , float *far , float *fv6, int doabs)
{
   /*             fv6[6]={average_diff, min_diff, max_diff,
                          average_adiff, min_adiff, max_adiff} */
   register float mm,vv, vvmin, vvmax;
   register int ii ;

   fv6[0] = fv6[1] = fv6[2] = fv6[3] = fv6[4] = fv6[5] = 0.0;
   if( npt <= 0 || far == NULL ) return 0 ;
   if ( npt == 1 ) { /* Nothing to do, return quietly though */
      return 1;
   }
   
   if (doabs==0) {
      vv = (far[1]-far[0]);
      mm = vvmin = vvmax = vv;
      for( ii=2 ; ii < npt ; ii++ ) {
         vv = (far[ii]-far[0]);
         if (vv < vvmin) {
            vvmin = vv;
         } else if (vv > vvmax) {
            vvmax = vv; 
         }
         mm += vv ;
      }
      mm /= (npt-1) ; 
      fv6[0] = mm;
      fv6[1] = vvmin;
      fv6[2] = vvmax;
   } else if (doabs==1) {
      vv = ABS(far[1]-far[0]);
      mm = vvmin = vvmax = vv;
      for( ii=2 ; ii < npt ; ii++ ) {
         vv = ABS(far[ii]-far[0]);
         if (vv < vvmin) {
            vvmin = vv;
         } else if (vv > vvmax) {
            vvmax = vv; 
         }
         mm += vv ;
      }
      mm /= (npt-1) ; 
      fv6[0] = mm;
      fv6[1] = vvmin;
      fv6[2] = vvmax;
   } else {
      register float mma,vva, vvmina, vvmaxa;
      vv = (far[1]-far[0]); vva = ABS(vv);
      mm = vvmin = vvmax = vv;
      mma = vvmina = vvmaxa = vva;
      for( ii=2 ; ii < npt ; ii++ ) {
         vv = (far[ii]-far[0]); vva = ABS(vv);
         if (vv < vvmin) {
            vvmin = vv;
         } else if (vv > vvmax) {
            vvmax = vv; 
         }
         mm += vv ;
         if (vva < vvmina) {
            vvmina = vva;
         } else if (vva > vvmaxa) {
            vvmaxa = vva; 
         }
         mma += vva ;
      }
      mm  /= (npt-1) ; 
      mma /= (npt-1) ; 
      fv6[0] = mm;
      fv6[1] = vvmin;
      fv6[2] = vvmax;
      fv6[3] = mma;
      fv6[4] = vvmina;
      fv6[5] = vvmaxa;
   } 
   
   return 1 ;
}

/*--------------------------------------------------------------------------*/

#if 0
static int fwhm_use_variance = 1 ;
void mri_nstat_fwhmxyz_usevar( int i ){ fwhm_use_variance = i; }
#endif

#undef  INMASK
#define INMASK(i) (mask==NULL || mask[i])

/*--------------------------------------------------------------------------*/
/*! FWHM parameters in a neigbhorhood of a point. */

THD_fvec3 mri_nstat_fwhmxyz( int xx, int yy, int zz,
                             MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd )
{
   float     *far ;
   int npt , nx,ny,nz,nxy , aa,bb,cc, kk,ii,pp ;
   THD_fvec3 fw_xyz ;
   double fsum, fsq, var , arg ;
   double dfdx, dfdxsum, dfdxsq, varxx;
   double dfdy, dfdysum, dfdysq, varyy;
   double dfdz, dfdzsum, dfdzsq, varzz;
   double dx,dy,dz ;
   float  sx=-1.0f,sy=-1.0f,sz=-1.0f ;
   int count, countx, county, countz;

   LOAD_FVEC3(fw_xyz,-1,-1,-1) ;  /* load with bad values */
   
   if( im == NULL || im->kind != MRI_float || nbhd == NULL 
                  || nbhd->num_pt < 19) return fw_xyz;

   far = MRI_FLOAT_PTR(im) ;
   nx  = im->nx; ny = im->ny; nz = im->nz; nxy = nx*ny; npt = nbhd->num_pt;
   kk  = xx + yy*nx + zz*nxy ;
   if( npt < 6 || kk < 0 || kk >= nxy*nz || !INMASK(kk) ) return fw_xyz ;

   /*----- estimate the variance of the local data -----*/

   fsum = 0.0; fsq = 0.0; count = 0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( INMASK(kk) ){
       count++; arg = far[kk]; fsum += arg; fsq += arg*arg;
     }
   }
   if( count < 6 || fsq <= 0.0 ) return fw_xyz ;
   var = (fsq - (fsum * fsum)/count) / (count-1.0);
   if( var <= 0.0 )              return fw_xyz ;

  /*----- estimate the partial derivatives -----*/

  dfdxsum = 0.0;   dfdysum = 0.0;   dfdzsum = 0.0;
  dfdxsq  = 0.0;   dfdysq  = 0.0;   dfdzsq  = 0.0;
  countx  = 0;     county  = 0;     countz  = 0;
  for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;     if( !INMASK(kk) ) continue ;
     arg = far[kk] ;
     if( aa+1 < nx ){
       pp = kk+1 ;
       if( INMASK(pp) ){
         dfdx     = (far[pp] - arg) ;
         dfdxsum += dfdx; dfdxsq += dfdx * dfdx; countx++ ;
       }
     }
     if( bb+1 < ny ){
       pp = kk+nx ;
       if( INMASK(pp) ){
         dfdy     = (far[pp] - arg) ;
         dfdysum += dfdy; dfdysq += dfdy * dfdy; county++ ;
       }
     }
     if( cc+1 < nz ){
       pp = kk+nxy ;
       if( INMASK(pp) ){
         dfdz     = (far[pp] - arg) ;
         dfdzsum += dfdz; dfdzsq += dfdz * dfdz; countz++ ;
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

   /*---- now estimate the FWHMs                                     ----*/
   /*---- 2.35482 = sqrt(8*log(2)) = sigma-to-FWHM conversion factor ----*/

   dx = im->dx; dy = im->dy; dz = im->dz;

   arg = 1.0 - 0.5*(varxx/var);
   if( arg > 0.0 && arg < 1.0 ) sx = 2.35482*sqrt( -1.0/(4.0*log(arg)) )*dx;

   arg = 1.0 - 0.5*(varyy/var);
   if( arg > 0.0 && arg < 1.0 ) sy = 2.35482*sqrt( -1.0/(4.0*log(arg)) )*dy;

   arg = 1.0 - 0.5*(varzz/var);
   if( arg > 0.0 && arg < 1.0 ) sz = 2.35482*sqrt( -1.0/(4.0*log(arg)) )*dz;

   LOAD_FVEC3(fw_xyz,sx,sy,sz) ;
   return fw_xyz ;
}

/*--------------------------------------------------------------------------*/

float mri_nstat_fwhmbar( int xx, int yy, int zz,
                         MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd )
{
   THD_fvec3 fw ;
   float fx,fy,fz , sum ; int nsum ;

   fw = mri_nstat_fwhmxyz( xx,yy,zz , im,mask,nbhd ) ;
   UNLOAD_FVEC3(fw,fx,fy,fz) ;

   sum = 0.0f ; nsum = 0 ;
   if( fx > 0.0f ){ sum += fx ; nsum++ ; }
   if( fy > 0.0f ){ sum += fy ; nsum++ ; }
   if( fz > 0.0f ){ sum += fz ; nsum++ ; }
   if( nsum > 0 ) sum /= nsum ;
   return sum ;
}

/*--------------------------------------------------------------------------*/
/*! FWHM parameters in a neigbhorhood of a point -- another way. */

THD_fvec3 mri_nstat_fwhmxyz_12dif( int xx, int yy, int zz,
                                   MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd,
                                   float *ws )
{
  int nx;                      /* number of voxels along x-axis */
  int ny;                      /* number of voxels along y-axis */
  int nz;                      /* number of voxels along z-axis */
  int nxy, nxyz;               /* total number of voxels */
  float dx;                    /* voxel size along x-axis */
  float dy;                    /* voxel size along y-axis */
  float dz;                    /* voxel size along z-axis */
  int aa,bb,cc, ii,kk, qm,qp ;
  float vx1,vy1,vz1 , arg ;
  float vx2,vy2,vz2 ;
  int countx, county, countz , npt ;
  float *dx1ar,*dy1ar,*dz1ar , *dx2ar,*dy2ar,*dz2ar , *fim ;
  float sx=-1.0f,sy=-1.0f,sz=-1.0f ;
  THD_fvec3 fw_xyz ;

  /*----- initialize local variables -----*/

  LOAD_FVEC3(fw_xyz,sx,sy,sz) ;  /* load with error flags */

  if( im == NULL || im->kind != MRI_float || nbhd == NULL ) return fw_xyz ;
  nx  = im->nx; ny = im->ny; nz = im->nz; nxy = nx*ny; nxyz = nxy*nz;
  kk  = xx + yy*nx + zz*nxy ; npt = nbhd->num_pt ;
  if( npt < 9 || kk < 0 || kk >= nxyz || !INMASK(kk) ) return fw_xyz ;
  fim = MRI_FLOAT_PTR(im) ;

  if( ws == NULL ){
#pragma omp critical (MALLOC)
    { dx1ar = (float *)malloc(sizeof(float)*npt) ;
      dy1ar = (float *)malloc(sizeof(float)*npt) ;
      dz1ar = (float *)malloc(sizeof(float)*npt) ;
      dx2ar = (float *)malloc(sizeof(float)*npt) ;
      dy2ar = (float *)malloc(sizeof(float)*npt) ;
      dz2ar = (float *)malloc(sizeof(float)*npt) ;
    }
  } else {
    dx1ar = ws + 0*npt ; dy1ar = ws + 1*npt ;
    dz1ar = ws + 2*npt ; dx2ar = ws + 3*npt ;
    dy2ar = ws + 4*npt ; dz2ar = ws + 5*npt ;
  }

  /*----- loop over voxels, compute differences, sum and sum squares -----*/

  countx = county = countz = 0 ;
  for( ii=0 ; ii < npt ; ii++ ){
    aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
    bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
    cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
    kk = aa + bb*nx + cc*nxy ;     if( !INMASK(kk) ) continue ;

    arg = fim[kk] ;

    if( aa-1 >= 0 && aa+1 < nx ){
      qp = kk+1 ; qm = kk-1 ;
      if( INMASK(qp) && INMASK(qm) ){
        dx1ar[countx] = fim[qp]-arg ; dx2ar[countx] = fim[qp]-fim[qm] ; countx++ ;
      }
    }

    if( bb-1 >= 0 && bb+1 < ny ){
      qp = kk+nx ; qm = kk-nx ;
      if( INMASK(qp) && INMASK(qm) ){
        dy1ar[county] = fim[qp]-arg ; dy2ar[county] = fim[qp]-fim[qm] ; county++ ;
      }
    }

    if( cc-1 >= 0 && cc+1 < nz ){
      qp = kk+nxy ; qm = kk-nxy ;
      if( INMASK(qp) && INMASK(qm) ){
        dz1ar[countz] = fim[qp]-arg ; dz2ar[countz] = fim[qp]-fim[qm] ; countz++ ;
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

  if( ws == NULL ){
#pragma omp critical (MALLOC)
    { free(dx1ar); free(dy1ar); free(dz1ar); free(dx2ar); free(dy2ar); free(dz2ar); }
  }

  /*----- now estimate the FWHMs -----*/

  dx = im->dx; dy = im->dy; dz = im->dz;

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

/*--------------------------------------------------------------------------*/

float mri_nstat_fwhmbar12( int xx, int yy, int zz,
                           MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd,float *ws )
{
   THD_fvec3 fw ;
   float fx,fy,fz , sum ; int nsum ;

   fw = mri_nstat_fwhmxyz_12dif( xx,yy,zz , im,mask,nbhd,ws ) ;
   UNLOAD_FVEC3(fw,fx,fy,fz) ;

   sum = 0.0f ; nsum = 0 ;
   if( fx > 0.0f ){ sum += fx ; nsum++ ; }
   if( fy > 0.0f ){ sum += fy ; nsum++ ; }
   if( fz > 0.0f ){ sum += fz ; nsum++ ; }
   if( nsum > 0 ) sum /= nsum ;
   return sum ;
}

/*--------------------------------------------------------------------------*/
#if 0
/*--------------------------------------------------------------------------*/
/*! Compute a local statistic at each voxel of an image, possibly with
    a mask; 'local' is defined with a neighborhood; 'statistic' is defined
    by an NSTAT_ code.
----------------------------------------------------------------------------*/

MRI_IMAGE * mri_localstat( MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd, int code )
{
   MRI_IMAGE *outim , *nbim ;
   float     *outar ;
   int ii,jj,kk , nx,ny,nz , ijk ;

ENTRY("mri_localstat") ;

   if( im == NULL || nbhd == NULL ) RETURN(NULL) ;

   outim = mri_new_conforming( im , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   nx = outim->nx ; ny = outim->ny ; nz = outim->nz ;

   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       nbim = mri_get_nbhd( im , mask , ii,jj,kk , nbhd ) ;
       outar[ijk++] = mri_nstat( code , nbim ) ;
       mri_free(nbim) ;
   }}}

   RETURN(outim) ;
}
#endif

/*--------------------------------------------------------------------------*/

static int verb=0 , vn=0 ;
static int localstat_datum = MRI_float;

void THD_localstat_datum(int i) {
   localstat_datum=i;
   if (  localstat_datum != MRI_byte &&
         localstat_datum != MRI_short &&
         localstat_datum != MRI_float) {
      fprintf(stderr ,  "Warning: Datum can only be one of "
                        "MRI_byte, MRI_short or MRI_float\n"
                        "Setting datum to float default.\n");
      localstat_datum = MRI_float;
   }
}

void THD_localstat_verb(int i){ verb=i; vn=0; }

#ifndef USE_OMP
static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}
#endif

/*--------------------------------------------------------------------------*/
/*
   Function to turn ijk 1D index on iset into its equivalent i,j,k on a
   different gridset gset

   ZSS Gov. Shutdown Imminent 2011
*/
int DSET_1Dindex_to_regrid_ijk( THD_3dim_dataset *iset, int ijk,
                                 THD_3dim_dataset *gset,
                                 int *ii, int *jj, int *kk)
{
   THD_fvec3 m_ncoord, m_ndicom;
   THD_ivec3 m_nind3;

   /* turn ijk on oset to RAI */
   m_nind3.ijk[0] = DSET_index_to_ix(iset,ijk) ;
   m_nind3.ijk[1] = DSET_index_to_jy(iset,ijk) ;
   m_nind3.ijk[2] = DSET_index_to_kz(iset,ijk) ;
   m_ncoord = THD_3dind_to_3dmm(iset,m_nind3);
   /* setenv OMP_NUM_THREADS 1 when you uncomment debugging lines */
   /*
   if (ijk < 10) fprintf(stderr,"LR ijk %d [%d %d %d] [%f %f %f]\n",
            ijk, m_nind3.ijk[0], m_nind3.ijk[1], m_nind3.ijk[2],
            m_ncoord.xyz[0], m_ncoord.xyz[1], m_ncoord.xyz[2]);
   */
   m_ndicom = THD_3dmm_to_dicomm(iset,m_ncoord);
   /* now go back to new grid ijk */
   m_ncoord = THD_dicomm_to_3dmm(gset, m_ndicom);
   m_nind3 = THD_3dmm_to_3dind(gset,m_ncoord);
   *ii = m_nind3.ijk[0];
   *jj = m_nind3.ijk[1];
   *kk = m_nind3.ijk[2];
   /*
   if (ijk < 10) fprintf(stderr,"HR    [%d %d %d] [%f %f %f]\n",
             m_nind3.ijk[0], m_nind3.ijk[1], m_nind3.ijk[2],
             m_ncoord.xyz[0], m_ncoord.xyz[1], m_ncoord.xyz[2]);
   */
   return(1);
}

/*
   Return a downsampled grid, or just an empty copy
*/
THD_3dim_dataset * THD_reduced_grid_copy(THD_3dim_dataset *dset, float *redx)
{
   float dx_o,dy_o,dz_o ;
   THD_3dim_dataset *oset=NULL;

   if( dset == NULL) return(NULL);

   if (!redx) {
      oset  = EDIT_empty_copy( dset ) ;
   } else { /* create a new grid */
      if (verb) {
         INFO_message("Reducing output grid by %f %f %f",
                      redx[0], redx[1], redx[2]);
      }
      dx_o = fabs(DSET_DX(dset)*redx[0]);
      dy_o = fabs(DSET_DY(dset)*redx[1]);
      dz_o = fabs(DSET_DZ(dset)*redx[2]);
      if (!(oset = r_new_resam_dset( dset, dset, dx_o, dy_o, dz_o,
                               NULL, 0, NULL, 0, 1))) {
         ERROR_message("Failed to reduce output grid");
         return(NULL);
      }
   }
   return(oset);
}

/*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_localstat( THD_3dim_dataset *dset , byte *mask ,
                                  MCW_cluster *nbhd , int ncode, int *code,
                                  float codeparam[][MAX_CODE_PARAMS+1],
                                  float *redx, int resam_mode)
{
   THD_3dim_dataset *oset=NULL;
   int iv,cc , nvin,nvout , nxyz_o , need_nbar=0,need_ws12=0 , npt ;
   float **aar ;
   MRI_IMAGE *dsim ;
   float dx,dy,dz , fac ;
   float *brick=NULL, voxval=0.0;
#ifndef USE_OMP
   int vstep ;
#endif

ENTRY("THD_localstat") ;
/*fprintf(stderr,"%p, %p, %d, %p", dset, nbhd, ncode, code);*/
   if( dset == NULL || nbhd == NULL || ncode < 1 || code == NULL ) RETURN(NULL);
/*fprintf(stderr,"%d\n", nbhd->num_pt);*/
   npt = nbhd->num_pt ; if( npt == 0 )                             RETURN(NULL);

   /* check for stupid reduction parameters allowing = 1.0 for testing purposes*/
   if (redx && redx[0] < 1.0 && redx[1]<1.0 && redx[2] <1.0) redx = NULL;

   if (!(oset  = THD_reduced_grid_copy(dset, redx))) {
      ERROR_message("Failed to create output dset");
      return(NULL);
   }

   nvin  = DSET_NVALS( dset ) ;
   nvout = nvin * ncode ;
   EDIT_dset_items( oset ,
                      ADN_nvals     , nvout       ,
                      ADN_datum_all , MRI_float   ,
                      ADN_nsl       , 0           ,
                      ADN_brick_fac , NULL        ,
                      ADN_prefix    , "localstat" ,
                    ADN_none ) ;

   nxyz_o = DSET_NX(oset)*DSET_NY(oset)*DSET_NZ(oset) ;

   dx = fabsf(DSET_DX(dset)) ; if( dx <= 0.0f ) dx = 1.0f ;
   dy = fabsf(DSET_DY(dset)) ; if( dy <= 0.0f ) dy = 1.0f ;
   dz = fabsf(DSET_DZ(dset)) ; if( dz <= 0.0f ) dz = 1.0f ;

#ifndef USE_OMP
   vstep = (verb && nxyz_o > 9999) ? nxyz_o/50 : 0 ;
#endif

   aar = (float **)malloc(sizeof(float *)*ncode) ;  /* output array of arrays */

   for( cc=0 ; cc < ncode ; cc++ ){
     if( code[cc] <  NSTAT_FWHMx     ){ need_nbar = 1; }  /* need nbhd image */
     if( code[cc] == NSTAT_FWHMbar12 ){ need_ws12 = 1; }
   }

   /** loop over input sub-bricks **/

   for( iv=0 ; iv < nvin ; iv++ ){

#ifdef USE_OMP
     if( verb && nxyz_o > 999 ) INFO_message("Start sub-brick [%d]",iv) ;
#endif

     for( cc=0 ; cc < ncode ; cc++ ){     /* create output sub-bricks */
       aar[cc] = (float *)malloc(sizeof(float)*nxyz_o) ;
       if( aar[cc] == NULL )
         ERROR_exit("THD_localstat: out of memory at iv=%d cc=%d",iv,cc);
     }

     /* extract copy of float-ized brick */

     dsim = THD_extract_float_brick( iv , dset ) ;
     dsim->dx = dx ; dsim->dy = dy ; dsim->dz = dz ;
     brick = MRI_FLOAT_PTR(dsim);

     /** loop over voxels **/

#ifndef USE_OMP
     if( vstep ) fprintf(stderr,"++ voxel loop [%d]:",iv) ;
#endif

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz_o > 1111 )    /* parallelization: 13 Jul 2009 */
 {
   int ijk,kk,jj,ii,cc ;
   THD_fvec3 fwv ;
   double perc[MAX_CODE_PARAMS], mpv[MAX_CODE_PARAMS] ;  /* no longer static */
   float *nbar , fv6[6]={0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
   int nbar_num=0, Hinit = 0;
   float *ws12 ;

   /* 17 Jul 2009: create workspace for neighborhood data */

   nbar = (need_nbar) ? (float *)malloc(sizeof(float)*npt  ) : NULL ;
   ws12 = (need_ws12) ? (float *)malloc(sizeof(float)*npt*6) : NULL ;
#pragma omp for
     for( ijk=0 ; ijk < nxyz_o ; ijk++ ){   /* parallelized loop */
       if (!redx) { /* no grid change */
          ii = DSET_index_to_ix(dset,ijk) ;  /* convert ijk to voxel indexes */
          jj = DSET_index_to_jy(dset,ijk) ;
          kk = DSET_index_to_kz(dset,ijk) ;
       } else {
         /* get ii, jj, kk on original resolution */
         DSET_1Dindex_to_regrid_ijk(oset, ijk, dset, &ii, &jj, &kk);
       }

#ifndef USE_OMP
       if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
#endif

       if( need_nbar )  /* extract vector of data from voxel neighborhood */
         nbar_num = mri_get_nbhd_array( dsim , mask,ii,jj,kk , nbhd , nbar ) ;

       for( cc=0 ; cc < ncode ; cc++ ){ /* loop over desired statistics */

         if( code[cc] == NSTAT_FWHMbar ){       /* 1 FWHM measurement */

           aar[cc][ijk] = mri_nstat_fwhmbar( ii,jj,kk , dsim,mask,nbhd ) ;

         } else if( code[cc] == NSTAT_FWHMbar12 ){

           aar[cc][ijk] = mri_nstat_fwhmbar12( ii,jj,kk , dsim,mask,nbhd,ws12 ) ;

         } else if( code[cc] == NSTAT_FWHMx ){  /* 3 FWHM measurements */

           fwv = mri_nstat_fwhmxyz( ii,jj,kk , dsim,mask,nbhd ) ;
           UNLOAD_FVEC3( fwv, aar[cc][ijk],aar[cc+1][ijk],aar[cc+2][ijk] ) ;
           cc += 2 ;  /* skip FWHMy and FWHMz codes that follow */

         } else if( code[cc] == NSTAT_PERCENTILE ){  /* percentiles */

           int N_mp, pp;
           float *sfar=NULL;

           if( codeparam[cc][0] < 1 )
             ERROR_exit("THD_localstat: No percentile parameters set!");
           N_mp = (int)codeparam[cc][0];
           if( N_mp > MAX_CODE_PARAMS )
             ERROR_exit( "THD_localstat: Cannot exceed %d params but have %d!" ,
                         MAX_CODE_PARAMS, N_mp);

           for( pp=0 ; pp < N_mp ; ++pp )
             mpv[pp] = (double)codeparam[cc][1+pp]/100.0;

           if( nbar != NULL && nbar_num > 0 ){

             if( !(sfar = (float *)Percentate( nbar ,
                                               NULL , nbar_num ,
                                               MRI_float , mpv , N_mp ,
                                               0 , perc , 1, 1, 1 ) ) ) {

               ERROR_exit("Failed to compute percentiles.");
             }

             for (pp=0; pp<N_mp; ++pp) aar[cc+pp][ijk] = (float)perc[pp];
          } else {
             for( pp=0 ; pp < N_mp; ++pp ) aar[cc+pp][ijk] = 0.0;
          }

          cc += (N_mp-1) ; /* number of sub-bricks added, minus 1 */

         } else if( code[cc] == NSTAT_HIST ){  /* histograms */
            static int N, iout;
            static double W, min, max;
            int pp, ib, cnt;

            if (!Hinit) { /* init */
              if( codeparam[cc][0] < 1 )
                ERROR_exit("THD_localstat: No histogram parameters set!");
              if( (int)codeparam[cc][0] != 4 )
                ERROR_exit("THD_localstat: Expecting only 4 params, have %d!" ,
                            (int)codeparam[cc][0]);

              min =      codeparam[cc][1];
              max =      codeparam[cc][2]; 
              N   = (int)codeparam[cc][3];
              iout= (int)codeparam[cc][4];
              W = (max - min)/(double)N;
              Hinit = 1;
            }
            for (ib=0; ib<N;++ib) aar[cc+ib][ijk] = 0;
            if (iout) { /* ignore outliers */
             for (pp=0, cnt=0; pp<nbar_num;++pp) {
               if (nbar[pp]>=min && nbar[pp]<=max) {
                  ib = (int)((nbar[pp]-min)/W); if (ib==N) ib = N-1;
                  ++aar[cc+ib][ijk];
                  ++cnt;
               }
             }
            } else {
            cnt = nbar_num;
            for (pp=0; pp<nbar_num;++pp) {
               ib = (int)((nbar[pp]-min)/W);
               if (ib>=N) ib = N-1;
               else if (ib < 0) ib = 0;
               ++aar[cc+ib][ijk];
            }
            }
            for (ib=0; ib<N;++ib) aar[cc+ib][ijk] /= (float)cnt;

            cc += (N-1) ; /* number of sub-bricks added, minus 1 */

         } else if( code[cc] == NSTAT_LIST ){ /* Just all the neighbors mam*/
           int pp;

           if (nbar) {
            for (pp=0; pp<nbar_num; ++pp) aar[cc+pp][ijk] = (float)nbar[pp];
           } 
           cc += (nbhd->num_pt-1) ; /* number of sub-bricks added, minus 1 
                                   Do not use nbar_num-1 because at 
                                   volume edges you will not have
                                   as many neighbors as elsewhere*/
         
         } else if( code[cc] == NSTAT_diffs0 ){ /*3 values */
           mri_nstat_diffs( nbar_num , nbar, fv6, 0 ) ;
           aar[cc  ][ijk] = fv6[0]; /* average difference */
           aar[cc+1][ijk] = fv6[1]; /* minimum difference */
           aar[cc+2][ijk] = fv6[2]; /* maximum difference */
           cc += 2 ;  /* skip redundant codes that follow */
         } else if( code[cc] == NSTAT_adiffs0 ){ /*3 values */
           mri_nstat_diffs( nbar_num , nbar, fv6, 1) ;
           aar[cc  ][ijk] = fv6[0]; /* average absolute difference */
           aar[cc+1][ijk] = fv6[1]; /* minimum absolute difference */
           aar[cc+2][ijk] = fv6[2]; /* maximum absolute difference */
           cc += 2 ;  /* skip redundant codes that follow */
         } else if( code[cc] == NSTAT_mMP2s0 ){ /*3 values, median, MAD, P2Skew*/
           mri_nstat_mMP2S( nbar_num , nbar, brick[ijk], fv6 ) ;
           aar[cc  ][ijk] = fv6[1]; /* median */
           aar[cc+1][ijk] = fv6[3]; /* MAD */
           aar[cc+2][ijk] = fv6[4]; /* Skew */
           cc += 2 ;  /* skip redundant codes that follow */
         } else if( code[cc] == NSTAT_mmMP2s0 ){
               /*4 values, mean, median, MAD, P2Skew*/
           mri_nstat_mMP2S( nbar_num , nbar, brick[ijk], fv6 ) ;
           aar[cc  ][ijk] = fv6[0]; /* mean */
           aar[cc+1][ijk] = fv6[1]; /* median */
           aar[cc+2][ijk] = fv6[3]; /* MAD */
           aar[cc+3][ijk] = fv6[4]; /* Skew */
           cc += 3 ;  /* skip redundant codes that follow */
         } else {   /* the "usual" (catchall) case */

           aar[cc][ijk] = mri_nstat( code[cc] , nbar_num , nbar, brick[ijk] ) ;

         }

       } /* end of loop over cc */

     } /** end of voxel loop **/

     if( nbar != NULL ) free(nbar) ;
     if( ws12 != NULL ) free(ws12) ;

 } /* end OpenMP */
 AFNI_OMP_END ;

#ifndef USE_OMP
     if( vstep ) fprintf(stderr,"\n") ;
#endif

     if( dsim != NULL ){ mri_free(dsim); dsim = NULL; }

     /* put data arrays from aar into the dataset */

     for( cc=0 ; cc < ncode ; cc++ ) {
       /* EDIT_substitute_brick( oset , iv*ncode+cc , MRI_float , aar[cc] ) ; */
       EDIT_substscale_brick( oset , iv*ncode+cc , MRI_float , aar[cc],
                              localstat_datum, -1.0);
       if( localstat_datum != MRI_float ) free(aar[cc]) ;  /* 13 Jul 2009 */
     }

   } /** end of sub-brick loop **/

   free((void *)aar) ;

   if ( resam_mode >= FIRST_RESAM_TYPE) {
      THD_3dim_dataset *tout=NULL;
      INFO_message("Restoring grid with %d resampling mode", resam_mode);
      /* resample back to original grid */
      if (!(tout = r_new_resam_dset( oset, dset, 0.0, 0.0, 0.0,
                               NULL, resam_mode, NULL, 1, 1))) {
         ERROR_exit("Failed to reduce output grid");
      }
      DSET_delete(oset) ; oset = tout; tout = NULL;
      /* apply mask to resampled output */
      if (mask) THD_applydsetmask(oset,mask);
   }
   RETURN(oset) ;
}
