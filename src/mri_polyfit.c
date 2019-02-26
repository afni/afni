#include "mrilib.h"

static int verb = 0 ;
void mri_polyfit_verb( int ii ){ verb = ii ; }

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

ENTRY("poly3D") ;

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

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Evaluate product Hermite functions. */

#define HP1(x) (x)
#define HP2(x) ((x)*(x)-2.0f)
#define HP3(x) ((x)*(2.0f*(x)*(x)-3.0f))
#define HP4(x) ((4.0f*(x)*(x)-12.0f)*(x)*(x)+3.0f)
#define HP5(x) ((x)*((4.0f*(x)*(x)-20.0f)*(x)*(x)+15.0f))
#define HP6(x) (((2.0f*(x)*(x)-15.0f)*(x)*(x)+22.5f)*(x)*(x)-3.75f)
#define HP7(x) (0.5f*(x)*(((8.0f*(x)*(x)-84.0f)*(x)*(x)+210.0f)*(x)*(x)-105.0f))
#define HP8(x) (((((x)*(x)-14.0f)*(x)*(x)+52.5f)*(x)*(x)-52.5f)*(x)*(x)+6.5625f)
#define HP9(x) (0.02f*(x)*((((16.0f*(x)*(x)-288.0f)*(x)*(x)+1512.0f)*(x)*(x)-2520.0f)*(x)*(x)+945.0f))

#define HH1(x) HP1(3.0f*(x))*exp(-(x)*(x))
#define HH2(x) HP2(3.0f*(x))*exp(-2.0f*(x)*(x))
#define HH3(x) HP3(3.0f*(x))*exp(-3.0f*(x)*(x))
#define HH4(x) HP4(3.0f*(x))*exp(-5.0f*(x)*(x))
#define HH5(x) HP5(3.0f*(x))*exp(-6.0f*(x)*(x))
#define HH6(x) HP6(3.0f*(x))*exp(-6.0f*(x)*(x))
#define HH7(x) HP7(3.0f*(x))*exp(-7.0f*(x)*(x))
#define HH8(x) HP8(3.0f*(x))*exp(-7.0f*(x)*(x))
#define HH9(x) HP9(3.0f*(x))*exp(-7.0f*(x)*(x))

static void herm3D( int px, int py, int pz,
                    int nxyz, float *x, float *y, float *z, float *val )
{
   register int ii ;

ENTRY("herm3D") ;

   switch( px ){
     default: for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = 1.0f       ; break ;
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH1(x[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH2(x[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH3(x[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH4(x[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH5(x[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH6(x[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH7(x[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH8(x[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] = HH9(x[ii]) ; break ;
   }

   switch( py ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH1(y[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH2(y[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH3(y[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH4(y[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH5(y[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH6(y[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH7(y[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH8(y[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH9(y[ii]) ; break ;
   }

   switch( pz ){
     case 1:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH1(z[ii]) ; break ;
     case 2:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH2(z[ii]) ; break ;
     case 3:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH3(z[ii]) ; break ;
     case 4:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH4(z[ii]) ; break ;
     case 5:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH5(z[ii]) ; break ;
     case 6:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH6(z[ii]) ; break ;
     case 7:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH7(z[ii]) ; break ;
     case 8:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH8(z[ii]) ; break ;
     case 9:  for( ii=0 ; ii < nxyz ; ii++ ) val[ii] *= HH9(z[ii]) ; break ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static void (*BFUN3D)(int,int,int,int,float *,float *,float*,float *) = poly3D ;

void mri_polyfit_set_basis( char *str )
{
   if( str != NULL && strcasecmp(str,"Hermite") == 0 )
      BFUN3D = herm3D ;
   else
      BFUN3D = poly3D ;  /* default */
   return ;
}

/*----------------------------------------------------------------------------*/
/* for getting the fit coefficients [26 Feb 2019] */

static floatvec *pfit_vec = NULL ;
floatvec * mri_polyfit_get_fitvec(void){ return pfit_vec ; }

/*----------------------------------------------------------------------------*/
/* Fit a polynomial to a 3D image and return the fitted image:
     nord = maximum order of polynomial (0..9)
     mask = optional mask of voxels to actually use
     mrad = optional preliminary median filter radius (voxels)
     meth = 2 for least squares fit, 1 for L1 fit
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_polyfit( MRI_IMAGE *imin, int nord, MRI_IMARR *exar, byte *mask, float mrad, int meth )
{
   MRI_IMAGE *fim ;
   float *far,*fdat , **ref=NULL , *xx,*yy,*zz,*vv ; floatvec *fvit ;
   int *pir=NULL , *pjr=NULL , *pkr=NULL ;
   int nmask , ii,jj,kk,pp,qq , nx,ny,nz,nxyz , ibot,itop,jbot,jtop,kbot,ktop ;
   int pi,pj,pk , pitop=0,pjtop=0,pktop=0 , nref=0 , npref=0 ;
   float imid=0.0,jmid=0.0,kmid=0.0 , ifac=0.0f,jfac=0.0f,kfac=0.0f , rfac ;
   int nex=0 ;

ENTRY("mri_polyfit") ;

   KILL_floatvec(pfit_vec) ;

   if( imin == NULL || (nord < 0 && exar == NULL) || nord > 9 ) RETURN(NULL) ;

   if( nord < -1 ) nord = -1 ;

   ii = mri_dimensionality(imin) ;
   if( ii < 2 || ii > 3 ){
     if( verb ) ERROR_message("mri_polyfit: dimensionality = %d",ii) ;
     RETURN(NULL) ;
   }

   if( exar != NULL ) nex = IMARR_COUNT(exar) ;

   if( mask != NULL ) nmask = THD_countmask( imin->nvox , mask ) ;
   else               nmask = imin->nvox ;

   jj = 8*( nex + (int)rint( pow( (double)(nord+1) , (double)ii ) ) ) ;
   if( nmask < jj ){
     if( verb ) ERROR_message("mri_polyfit: #points=%d < %d",nmask,jj) ;
     RETURN(NULL) ;
   }

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

   if( nord >= 0 ){
     imid = 0.5f*(ibot+itop) ; jmid = 0.5f*(jbot+jtop) ; kmid = 0.5f*(kbot+ktop) ;

     if( ibot < itop ){ kk = (itop-ibot)/3; pitop = MIN(nord,kk); ifac = 2.0f/(itop-ibot); }
     if( jbot < jtop ){ kk = (jtop-jbot)/3; pjtop = MIN(nord,kk); jfac = 2.0f/(jtop-jbot); }
     if( kbot < ktop ){ kk = (ktop-kbot)/3; pktop = MIN(nord,kk); kfac = 2.0f/(ktop-kbot); }

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
     npref = nref ;

     if( verb )
       ININFO_message("create %d polynomial regressors with %d points each",nref,nmask) ;

     ref = (float **)malloc(sizeof(float *)*nref) ;
     for( kk=0 ; kk < nref ; kk++ ){
       ref[kk] = (float *)malloc(sizeof(float)*nmask) ;
       BFUN3D( pir[kk],pjr[kk],pkr[kk] , nmask , xx,yy,zz , ref[kk] ) ;
     }
     free(zz) ; free(yy) ; free(xx) ;
   }

   if( nex > 0 ){            /* 27 Dec 2012 */
     float *ex, *rr ;
     if( verb ) ININFO_message("add %d image regressors",nex) ;
     ref = (float **)realloc(ref,sizeof(float *)*(npref+nex)) ;
     for( kk=0 ; kk < nex ; kk++ ){
       ex = MRI_FLOAT_PTR(IMARR_SUBIM(exar,kk)) ;
       rr = ref[npref+kk] = (float *)malloc(sizeof(float)*nmask) ;
       for( qq=ii=0 ; ii < nxyz ; ii++ ){ if( GOOD(ii) ) rr[qq++] = ex[ii]; }
     }
     nref = npref + nex ;
   }

   fim = mri_to_float(imin) ;  /* convert input to float */
   if( mrad > 0.0f ){
     MRI_IMAGE *qim ;
     if( verb ) ININFO_message("median filter input image") ;
     qim =  mri_medianfilter(fim,mrad,mask,verb) ;
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
   if( verb ) ININFO_message("L%d fit polynomial field to data",meth) ;

   fvit = THD_fitter( nmask , fdat , nref , ref , meth , NULL ) ;

   if( fdat != far ) free(fdat) ;
   for( kk=0 ; kk < nref ; kk++ ) free(ref[kk]) ;
   free(ref) ;

   if( fvit == NULL ){
     if( pir != NULL ){ free(pir); free(pjr); free(pkr); }
     ERROR_message("Can't calculate fit in mri_polyfit?! :-(") ;
     RETURN(NULL) ;
   }

   COPY_floatvec(pfit_vec,fvit) ; /* save coefficients */

   memset( far , 0 , sizeof(float)*nxyz ) ;

   if( nord >= 0 ){
     if( verb ) ININFO_message("compute final fit polynomial field") ;
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

     for( kk=0 ; kk < npref ; kk++ ){
       BFUN3D( pir[kk],pjr[kk],pkr[kk] , nxyz , xx,yy,zz , vv ) ;
       rfac = fvit->ar[kk] ;
       for( ii=0 ; ii < nxyz ; ii++ ) far[ii] += rfac * vv[ii] ;
     }
     free(pir); free(pjr); free(pkr); free(vv); free(zz); free(yy); free(xx);
   }

   if( nex > 0 ){
     float *ex ;
     for( kk=0 ; kk < nex ; kk++ ){
       ex = MRI_FLOAT_PTR(IMARR_SUBIM(exar,kk)) ;
       rfac = fvit->ar[kk+npref] ;
       for( ii=0 ; ii < nxyz ; ii++ ) far[ii] += rfac * ex[ii] ;
     }
   }

   KILL_floatvec(fvit) ;

   RETURN(fim) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_polyfit_byslice( MRI_IMAGE *imin, int nord, MRI_IMARR *exar,
                                 byte *mask, float mrad, int meth )
{
   MRI_IMAGE *sl_imin , *sl_exim , *sl_out , *out_im ;
   MRI_IMARR *sl_exar=NULL , *out_imar=NULL ;
   byte *sl_mask=NULL ;
   int nx=imin->nx , ny=imin->ny , nz=imin->nz , nex=0 ;
   int kk ;

ENTRY("mri_polyfit_byslice") ;

   if( nz == 1 ){
     out_im = mri_polyfit( imin,nord,exar , mask,mrad,meth ) ;
     RETURN(out_im) ;
   }

   INIT_IMARR(out_imar) ;

   for( kk=0 ; kk < nz ; kk++ ){
     sl_imin = mri_cut_3D( imin , 0,nx-1 , 0,ny-1 , kk,kk ) ;
     if( mask != NULL ) sl_mask = mask + kk*nx*ny ;
     if( exar != NULL ){
       int nex=IMARR_COUNT(exar) , ee ; MRI_IMAGE *eim ;
       INIT_IMARR(sl_exar) ;
       for( ee=0 ; ee < nex ; ee++ ){
         eim = mri_cut_3D( IMARR_SUBIM(exar,ee) , 0,nx-1 , 0,ny-1 , kk,kk ) ;
         ADDTO_IMARR(sl_exar,eim) ;
       }
     }
     sl_out = mri_polyfit( sl_imin,nord,sl_exar , sl_mask,mrad,meth ) ;
     ADDTO_IMARR(out_imar,sl_out) ;
     if( sl_exar != NULL ) DESTROY_IMARR(sl_exar) ;
     mri_free(sl_imin) ;
   }

   out_im = mri_catvol_1D( out_imar , 3 ) ;
   DESTROY_IMARR(out_imar) ;
   RETURN(out_im) ;
}
