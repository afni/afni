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

float mri_nstat( int code , int npt , float *far )
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

THD_3dim_dataset * THD_localstat( THD_3dim_dataset *dset , byte *mask ,
                                  MCW_cluster *nbhd , int ncode, int *code,
                                  float codeparam[][MAX_CODE_PARAMS+1] )
{
   THD_3dim_dataset *oset ;
   int iv,cc , nvin,nvout , nx,ny,nz,nxyz , need_nbar=0 , npt ;
   float **aar ;
   MRI_IMAGE *dsim ;
   float dx,dy,dz , fac ;
#ifndef USE_OMP
   int vstep ;
#endif

ENTRY("THD_localstat") ;

   if( dset == NULL || nbhd == NULL || ncode < 1 || code == NULL ) RETURN(NULL);
   npt = nbhd->num_pt ; if( npt == 0 )                             RETURN(NULL);

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
   dx = fabsf(DSET_DX(dset)) ; if( dx <= 0.0f ) dx = 1.0f ;
   dy = fabsf(DSET_DY(dset)) ; if( dy <= 0.0f ) dy = 1.0f ;
   dz = fabsf(DSET_DZ(dset)) ; if( dz <= 0.0f ) dz = 1.0f ;

#ifndef USE_OMP
   vstep = (verb && nxyz > 9999) ? nxyz/50 : 0 ;
#endif

   aar = (float **)malloc(sizeof(float *)*ncode) ;  /* output array of arrays */

   for( cc=0 ; cc < ncode ; cc++ ){
     if( code[cc] <  NSTAT_FWHMx ){ need_nbar = 1; break;}  /* need nbhd image */
   }

   /** loop over input sub-bricks **/

   for( iv=0 ; iv < nvin ; iv++ ){

#ifdef USE_OMP
     if( verb && nxyz > 999 ) INFO_message("Start sub-brick [%d]",iv) ;
#endif

     for( cc=0 ; cc < ncode ; cc++ ){     /* create output sub-bricks */
       aar[cc] = (float *)malloc(sizeof(float)*nxyz) ;
       if( aar[cc] == NULL )
         ERROR_exit("THD_localstat: out of memory at iv=%d cc=%d",iv,cc);
     }

     /* extract copy of float-ized brick */

     dsim = THD_extract_float_brick( iv , dset ) ;
     dsim->dx = dx ; dsim->dy = dy ; dsim->dz = dz ;

     /** loop over voxels **/

#ifndef USE_OMP
     if( vstep ) fprintf(stderr,"++ voxel loop [%d]:",iv) ;
#endif

#pragma omp parallel if( nxyz > 1111 )    /* parallelization: 13 Jul 2009 */
 {
   int ijk,kk,jj,ii,cc ;
   MRI_IMAGE *nbim=NULL ;
   THD_fvec3 fwv ;
   double perc[MAX_CODE_PARAMS], mpv[MAX_CODE_PARAMS] ;  /* no longer static */
   float *nbar ; int nbar_num ;

 AFNI_OMP_START ;

   /* 17 Jul 2009: create workspace for neighborhood data */

   nbar = (need_nbar) ? (float *)malloc(sizeof(float)*npt) : NULL ;

#pragma omp for
     for( ijk=0 ; ijk < nxyz ; ijk++ ){   /* parallelized loop */
       ii = DSET_index_to_ix(dset,ijk) ;  /* convert ijk to voxel indexes */
       jj = DSET_index_to_jy(dset,ijk) ;
       kk = DSET_index_to_kz(dset,ijk) ;

#ifndef USE_OMP
       if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
#endif

       if( need_nbar )  /* extract vector of data from voxel neighborhood */
         nbar_num = mri_get_nbhd_array( dsim , mask,ii,jj,kk , nbhd , nbar ) ;

       for( cc=0 ; cc < ncode ; cc++ ){ /* loop over desired statistics */

         if( code[cc] == NSTAT_FWHMbar ){       /* 1 FWHM measurement */

           aar[cc][ijk] = mri_nstat_fwhmbar( ii,jj,kk , dsim,mask,nbhd ) ;

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
                /***
                  fprintf(stderr,"sar=[");
                  for (pp=0; pp<nbim->nvox; ++pp) fprintf(stderr,"%f,", sfar[pp]);
                  fprintf(stderr,"];\nperc=[");
                  for (pp=0; pp<N_mp; ++pp) fprintf(stderr,"%f,", perc[pp]);
                  fprintf(stderr,"];\n");
                ***/

             for (pp=0; pp<N_mp; ++pp) aar[cc+pp][ijk] = (float)perc[pp];
          } else {
             for( pp=0 ; pp < N_mp; ++pp ) aar[cc+pp][ijk] = 0.0;
          }

          cc += (N_mp-1) ; /* number of sub-bricks added, minus 1 */

         } else {   /* the "usual" (catchall) case */

           aar[cc][ijk] = mri_nstat( code[cc] , nbar_num , nbar ) ;

         }

       } /* end of loop over cc */

     } /** end of voxel loop **/

     if( nbar != NULL ) free(nbar) ;

 AFNI_OMP_END ;
 } /* end OpenMP */

#ifndef USE_OMP
     if( vstep ) fprintf(stderr,"\n") ;
#endif

     if( dsim != NULL ){ mri_free(dsim); dsim = NULL; }

     /* put data arrays from aar into the dataset */

     for( cc=0 ; cc < ncode ; cc++ ) {
       /* EDIT_substitute_brick( oset , iv*ncode+cc , MRI_float , aar[cc] ) ; */
       EDIT_substscale_brick( oset , iv*ncode+cc , MRI_float , aar[cc], localstat_datum, -1.0);
       if( localstat_datum != MRI_float ) free(aar[cc]) ;  /* 13 Jul 2009 */
     }

   } /** end of sub-brick loop **/

   free((void *)aar) ;
   RETURN(oset) ;
}
