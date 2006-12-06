#include "mrilib.h"

/*--------------------------------------------------------------------------*/
/*! Input = 1D image, and an NSTAT_ code to compute some statistic.
   Output = statistic's value.
----------------------------------------------------------------------------*/

float mri_nstat( int code , MRI_IMAGE *im )
{
   MRI_IMAGE *fim ;
   float     *far , outval=0.0f ;
   int npt ;

   if( im == NULL || im->nvox == 0 ) return outval ;

   /* convert input to float format, if not already there */

   if( im->kind != MRI_float ) fim = mri_to_float(im) ;
   else                        fim = im ;
   far = MRI_FLOAT_PTR(fim) ;  /* array of values to statisticate */
   npt = fim->nvox ;           /* number of values */

   switch( code ){

     case NSTAT_NUM: outval = (float)npt ; break ;  /* quite easy */

     default:
     case NSTAT_MEAN:{
       register int ii ;
       for( ii=0 ; ii < npt ; ii++ ) outval += far[ii] ;
       outval /= npt ;
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
       qmedmad_float( npt , far , &outval , NULL ) ;
     break ;

     case NSTAT_MAD:
       qmedmad_float( npt , far , NULL , &outval ) ;
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
   }

   /* cleanup and exit */

   if( fim != im  ) mri_free(fim) ;
   return outval ;
}

/*--------------------------------------------------------------------------*/

#if 0
static int fwhm_use_variance = 1 ;
void mri_nstat_fwhmxyz_usevar( int i ){ fwhm_use_variance = i; }
#endif

#undef  INMASK
#define INMASK(i) (mask == NULL || mask[i] != 0)

/*--------------------------------------------------------------------------*/
/*! FWHM parameters in a neigbhorhood of a point. */

THD_fvec3 mri_nstat_fwhmxyz( int xx, int yy, int zz,
                             MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd )
{
   MRI_IMAGE *fim ;
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

   if( im == NULL || im->kind != MRI_float || nbhd == NULL ) return fw_xyz;

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

/*--------------------------------------------------------------------------*/

static int verb=0 , vn=0 ;
void THD_localstat_verb(int i){ verb=i; vn=0; }

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_localstat( THD_3dim_dataset *dset , byte *mask ,
                                  MCW_cluster *nbhd , int ncode, int *code )
{
   THD_3dim_dataset *oset ;
   MRI_IMAGE *nbim=NULL ;
   int iv,cc , nvin,nvout , nx,ny,nz,nxyz , ii,jj,kk,ijk ;
   float **aar ;
   int vstep ;
   THD_fvec3 fwv ;
   MRI_IMAGE *dsim=NULL; int need_dsim, need_nbim; float dx,dy,dz ;

ENTRY("THD_localstat") ;

   if( dset == NULL || nbhd == NULL || ncode < 1 || code == NULL ) RETURN(NULL);

   oset  = EDIT_empty_copy( dset ) ;
   nvin  = DSET_NVALS( dset ) ;
   nvout = nvin * ncode ;
   EDIT_dset_items( oset ,
                      ADN_nvals     , nvout       ,
                      ADN_datum_all , MRI_float   ,
                      ADN_nsl       , 0           ,
                      ADN_brick_fac , NULL        ,
                      ADN_prefix    , "localstat" ,
                    ADN_none ) ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxyz = nx*ny*nz ;
   dx = fabs(DSET_DX(dset)) ; if( dx <= 0.0f ) dx = 1.0f ;
   dy = fabs(DSET_DY(dset)) ; if( dy <= 0.0f ) dy = 1.0f ;
   dz = fabs(DSET_DZ(dset)) ; if( dz <= 0.0f ) dz = 1.0f ;

   vstep = (verb && nxyz > 99999) ? nxyz/50 : 0 ;

   aar = (float **)malloc(sizeof(float *)*ncode) ;

   need_dsim = need_nbim = 0 ;
   for( cc=0 ; cc < ncode ; cc++ )
          if( code[cc] >= NSTAT_FWHMx ) need_dsim = 1;
     else if( code[cc] <  NSTAT_FWHMx ) need_nbim = 1;

   for( iv=0 ; iv < nvin ; iv++ ){
     for( cc=0 ; cc < ncode ; cc++ ){
       aar[cc] = (float *)malloc(sizeof(float)*nxyz) ;
       if( aar[cc] == NULL )
         ERROR_exit("THD_localstat: out of memory at iv=%d cc=%d",iv,cc);
     }
     if( need_dsim ){
       float fac = DSET_BRICK_FACTOR(dset,iv) ;
       if( fac <= 0.0f ) fac = 1.0f ;
       dsim = mri_scale_to_float( fac , DSET_BRICK(dset,iv) ) ;
       dsim->dx = dx ; dsim->dy = dy ; dsim->dz = dz ;
     }

     if( vstep ) fprintf(stderr,"++ voxel loop [%d]:",iv) ;
     for( ijk=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,ijk++ ){
         if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
         if( need_nbim )
           nbim = THD_get_dset_nbhd( dset,iv , mask,ii,jj,kk , nbhd ) ;
         for( cc=0 ; cc < ncode ; cc++ ){
           if( code[cc] != NSTAT_FWHMx ){
             aar[cc][ijk] = mri_nstat( code[cc] , nbim ) ;
           } else {
             fwv = mri_nstat_fwhmxyz( ii,jj,kk , dsim,mask,nbhd ) ;
             UNLOAD_FVEC3( fwv, aar[cc][ijk],aar[cc+1][ijk],aar[cc+2][ijk] ) ;
             cc += 2 ;  /* skip FWHMy and FWHMz codes */
           }
         }
         if( nbim != NULL ){ mri_free(nbim); nbim = NULL; }
     }}}

     if( vstep ) fprintf(stderr,"\n") ;

     if( dsim != NULL ){ mri_free(dsim); dsim = NULL; }
     for( cc=0 ; cc < ncode ; cc++ )
       EDIT_substitute_brick( oset , iv*ncode+cc , MRI_float , aar[cc] ) ;
   }

   free((void *)aar) ;
   RETURN(oset) ;
}
