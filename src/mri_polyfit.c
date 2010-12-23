#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/* define some polynomials over the domain [-1..1] */

#undef LP1
#undef LP2
#undef LP3
#undef LP4
#undef LP5
#undef LP6
#undef LP7
#undef LP8
#undef LP9

#define LP1(x) (x)
#define LP2(x) ((x)*(x)-0.3333333f)
#define LP3(x) (((x)*(x)-0.6f)*(x))*1.5f
#define LP4(x) ((x)*(x)*((x)*(x)-0.857143f)+0.0857143f)*2.0f
#define LP5(x) (((x)*(x)*((x)*(x)-1.11111f)+0.238095f)*(x))*3.0f
#define LP6(x) ((x)*(x)*((x)*(x)*((x)*(x)-1.36364f)+0.454545f)-0.021645f)*6.0f
#define LP7(x) (((x)*(x)*((x)*(x)*((x)*(x)-1.61538f)+0.734266f)-0.081585f)*(x))*10.0f

#define LP8(x) ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x) * \
               ( (x)*(x)-1.86667f )+1.07692f )-0.195804f )+0.0054390f ) * 18.0f

#define LP9(x) ( ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x) * \
                 ( (x)*(x)-2.11765f )+1.48235f )-0.380090f )+0.0259153f ) \
               * (x) )*32.0f

/*----------------------------------------------------------------------------*/
/* Evaluate product polynomial px(x)*py(y)*pz(z) at a bunch of points. */

static void poly3D( int px, int py, int pz,
                    int nxyz, float *x, float *y, float *z, float *val )
{
   register int ii ;

   switch( px ){
     default: for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = 1.0f       ; break ;
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP1(x[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP2(x[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP3(x[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP4(x[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP5(x[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP6(x[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP7(x[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP8(x[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = LP9(x[ii]) ; break ;
   }

   switch( py ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP1(y[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP2(y[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP3(y[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP4(y[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP5(y[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP6(y[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP7(y[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP8(y[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP9(y[ii]) ; break ;
   }

   switch( pz ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP1(z[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP2(z[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP3(z[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP4(z[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP5(z[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP6(z[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP7(z[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP8(z[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= LP9(z[ii]) ; break ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/
/* Fit a polynomial to a 3D image and return the fitted image:
     nord = maximum order of polynomial (0..9)
     mask = optional mask of voxels to actually use
     mrad = optional preliminary median filter radius (voxels)
     meth = 2 for least squares fit, 1 for L1 fit
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_polyfit( MRI_IMAGE *imin, int nord, byte *mask, float mrad, int meth )
{
   MRI_IMAGE *fim ;
   float *far,*fdat , **ref , *xx,*yy,*zz,*vv ; floatvec *fvit ;
   int *pir , *pjr , *pkr ;
   int nmask , ii,jj,kk,pp,qq , nx,ny,nz,nxyz , ibot,itop,jbot,jtop,kbot,ktop ;
   int pi,pj,pk , pitop=0,pjtop=0,pktop=0 , nref ;
   float imid,jmid,kmid , ifac=0.0f,jfac=0.0f,kfac=0.0f , rfac ;

ENTRY("mri_polyfit") ;

   if( imin == NULL || nord < 0 || nord > 9 ) RETURN(NULL) ;

   ii = mri_dimensionality(imin) ; if( ii < 2 || ii > 3 ) RETURN(NULL) ;

   if( mask != NULL ) nmask = THD_countmask( imin->nvox , mask ) ;
   else               nmask = imin->nvox ;
   jj = 8*(int)rint( pow( (double)(nord+1) , (double)ii ) ) ;
   if( nmask < jj ) RETURN(NULL) ;

#undef  GOOD
#define GOOD(i) (mask == NULL || mask[i])

   nx = imin->nx ; ny = imin->ny ; nz = imin->nz ; nxyz = nx*ny*nz ;

   /* find min and max values of indexes in the mask */

   if( mask == NULL ){
     ibot = jbot = kbot = 0 ; itop = nx-1 ; jtop = ny-1 ; ktop = nz-1 ;
   } else {
     ibot = jbot = kbot = 9999999 ; itop = jtop = ktop = -9999999 ;
     for( qq=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
        for( ii=0 ; ii < nx ; ii++,qq++ ){
          if( mask[qq] ){
            if( ii < ibot ) ibot = ii ;
            if( jj < jbot ) jbot = jj ;
            if( kk < kbot ) kbot = kk ;
            if( ii > itop ) itop = ii ;
            if( jj > jtop ) jtop = jj ;
            if( kk > ktop ) ktop = kk ;
          }
      }}}
      if( ((ibot>=itop) + (jbot>=jtop) + (kbot>=ktop)) > 1 ) RETURN(NULL) ;
   }

   imid = 0.5f*(ibot+itop) ; jmid = 0.5f*(jbot+jtop) ; kmid = 0.5f*(kbot+ktop) ;

   if( ibot < itop ){ pitop = nord ; ifac = 2.0f/(itop-ibot) ; }
   if( jbot < jtop ){ pjtop = nord ; jfac = 2.0f/(jtop-jbot) ; }
   if( kbot < ktop ){ pktop = nord ; kfac = 2.0f/(ktop-kbot) ; }

   xx = (float *)malloc(sizeof(float)*nmask) ;
   yy = (float *)malloc(sizeof(float)*nmask) ;
   zz = (float *)malloc(sizeof(float)*nmask) ;
   for( pp=qq=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx ; ii++,qq++ ){
        if( GOOD(qq) ){
          xx[pp] = (ii-imid)*ifac ;
          yy[pp] = (jj-jmid)*jfac ;
          zz[pp] = (kk-kmid)*kfac ; pp++ ;
        }
   }}}

   pir = (int *)malloc(sizeof(int)*(pktop+1)*(pjtop+1)*(pitop+1)) ;
   pjr = (int *)malloc(sizeof(int)*(pktop+1)*(pjtop+1)*(pitop+1)) ;
   pkr = (int *)malloc(sizeof(int)*(pktop+1)*(pjtop+1)*(pitop+1)) ;
   for( nref=pk=0 ; pk <= pktop ; pk++ ){
    for( pj=0 ; pj <= pjtop ; pj++ ){
     for( pi=0 ; pi <= pitop ; pi++ ){
       if( pi+pj+pk <= nord ){
         pir[nref] = pi ; pjr[nref] = pj ; pkr[nref] = pk ; nref++ ;
       }
   }}}

   ref = (float **)malloc(sizeof(float *)*nref) ;
   for( kk=0 ; kk < nref ; kk++ ){
     ref[kk] = (float *)malloc(sizeof(float)*nmask) ;
     poly3D( pir[kk],pjr[kk],pkr[kk] , nmask , xx,yy,zz , ref[kk] ) ;
   }
   free(zz) ; free(yy) ; free(xx) ;

   fim = mri_to_float(imin) ;  /* convert input to float */
   if( mrad > 0.0f ){
     MRI_IMAGE *qim = mri_medianfilter(fim,mrad,mask,0) ;
     if( qim != NULL ){ mri_free(fim) ; fim = qim ; }
   }
   far = MRI_FLOAT_PTR(fim) ;  /* will later be output image */

   if( nmask < nxyz ){
     fdat = (float *)malloc(sizeof(float)*nmask) ;
     for( qq=ii=0 ; ii < nxyz ; ii++ ){ if( GOOD(ii) ) fdat[qq++] = far[ii]; }
   } else {
     fdat = far ;
   }

        if( meth < 1 ) meth = 1 ;
   else if( meth > 2 ) meth = 2 ;
   fvit = THD_fitter( nmask , fdat , nref , ref , meth , NULL ) ;

   if( fdat != far ) free(fdat) ;
   for( kk=0 ; kk < nref ; kk++ ) free(ref[kk]) ;
   free(ref) ;

   if( fvit == NULL ){
     free(pir) ; free(pjr) ; free(pkr) ;
     ERROR_message("Can't calculate fit in mri_polyfit?! :-(") ;
     RETURN(NULL) ;
   }

   xx = (float *)malloc(sizeof(float)*nxyz) ;
   yy = (float *)malloc(sizeof(float)*nxyz) ;
   zz = (float *)malloc(sizeof(float)*nxyz) ;
   vv = (float *)malloc(sizeof(float)*nxyz) ;
   for( qq=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx ; ii++,qq++ ){
        xx[qq] = (ii-imid)*ifac ;
        yy[qq] = (jj-jmid)*jfac ;
        zz[qq] = (kk-kmid)*kfac ;
   }}}

   memset( far , 0 , sizeof(float)*nxyz ) ;
   for( kk=0 ; kk < nref ; kk++ ){
     poly3D( pir[kk],pjr[kk],pkr[kk] , nxyz , xx,yy,zz , vv ) ;
     rfac = fvit->ar[kk] ;
     for( ii=0 ; ii < nxyz ; ii++ ) far[ii] += rfac * vv[ii] ;
   }

   KILL_floatvec(fvit); free(pir); free(pjr); free(pkr);
   free(vv); free(zz); free(yy); free(xx);

   RETURN(fim) ;
}
