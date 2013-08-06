#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static INLINE float mytanh( float x )
{
  register float ex , exi ;
       if( x >  7.0f ) return  1.0f ;
  else if( x < -7.0f ) return -1.0f ;
  ex = exp(x) ; exi = 1.0f/ex ;
  return (ex-exi)/(ex+exi) ;
}

/*----------------------------------------------------------------------------*/
/* Despike the hard way, in place (as in 3dDespike). */
/*----------------------------------------------------------------------------*/

void THD_vectim_despike_L1( MRI_vectim *mrv , int localedit )
{
   int iv , nxyz , ii,jj,kk ;
   float cut1=2.5,cut2=4.0 , sq2p,sfac , fq , tm,fac ;
   int corder, nref , nuse ;
   float **ref ;
   float  c21,ic21 ;

   /*-- initialization --*/

   sq2p  = sqrt(0.5*PI) ;
   sfac  = sq2p / 1.4826f ;
   c21   = cut2 - cut1 ;
   ic21  = 1.0f / c21 ;

   nuse   = mrv->nvals ;
   nxyz   = mrv->nvec ;
   corder = (int)rintf(nuse/30.0f) ;
   if( corder < 2 ) corder = 2 ; else if( corder > 50 ) corder = 50 ;

   /* ref functions */

   nref = 2*corder+3 ;  /* always odd */
   ref  = (float **)malloc( sizeof(float *) * nref ) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *)malloc( sizeof(float) * nuse ) ;

   tm = 0.5f * (nuse-1.0f) ; fac = 2.0f / nuse ;
   for( iv=0 ; iv < nuse ; iv++ ){  /* polynomials */
     ref[0][iv] = 1.0f ;
     ref[1][iv] = (iv-tm)*fac ;
     ref[2][iv] = ref[1][iv] * ref[1][iv] - 0.3333333f ;
   }

   for( jj=2,kk=1 ; kk <= corder ; kk++,jj+=2 ){  /* sines and cosines */
     fq = (2.0*PI*kk)/nuse ;
     for( iv=0 ; iv < nuse ; iv++ ){
       ref[jj  ][iv] = sinf(fq*iv) ;
       ref[jj+1][iv] = cosf(fq*iv) ;
     }
   }

   /*--- loop over voxels and do some work ---*/

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 99 )
 { int ii , iv , jj , didit ;
   float *far , *dar , *var , *fitar , *ssp , *fit , *zar ;
   float fsig , fq , cls , snew , val ;

#pragma omp critical (DESPIKE_malloc)
  { far   = (float *) malloc( sizeof(float) * nuse ) ;
    dar   = (float *) malloc( sizeof(float) * nuse ) ;
    var   = (float *) malloc( sizeof(float) * nuse ) ;
    fitar = (float *) malloc( sizeof(float) * nuse ) ;
    ssp   = (float *) malloc( sizeof(float) * nuse ) ;
    fit   = (float *) malloc( sizeof(float) * nref ) ;
  }

#pragma omp for
   for( ii=0 ; ii < nxyz ; ii++ ){   /* ii = voxel index */

      /*** extract ii-th time series into far[] and dar[] ***/

      zar = VECTIM_PTR(mrv,ii) ;
      for( iv=0 ; iv < nuse ; iv++ ) far[iv] = dar[iv] = zar[iv] ;

      /*** solve for L1 fit ***/

      cls = cl1_solve( nuse , nref , far , ref , fit,0 ) ; /* the slow part */

      if( cls < 0.0f ) continue ;            /* fit failed! */

      for( iv=0 ; iv < nuse ; iv++ ){        /* detrend */
        val = fit[0] ;
        for( jj=1 ; jj < nref ; jj+=2 )      /* unrolled (nref is odd) */
          val += fit[jj] * ref[jj][iv] + fit[jj+1] * ref[jj+1][iv] ;

        fitar[iv] = val ;                    /* save curve fit value */
        var[iv]   = dar[iv]-val ;            /* remove fitted value = resid */
        far[iv]   = fabsf(var[iv]) ;         /* abs value of resid */
      }

      /*** estimate standard deviation of detrended data ***/

      fsig = sq2p * qmed_float(nuse,far) ;   /* also mangles far array */

      /*** process time series for spikes, editing data in dar[] ***/

      if( fsig > 0.0f ){                     /* data wasn't fit perfectly */

        /* find spikiness for each point in time */

        fq = 1.0f / fsig ;
        for( iv=0 ; iv < nuse ; iv++ ){
          ssp[iv] = fq * var[iv] ;           /* spikiness s = how many sigma out */
        }
        didit = 0 ;

        /* process values of |s| > cut1, editing dar[] */

        for( iv=0 ; iv < nuse ; iv++ ){ /* loop over time points */
          if( !localedit ){             /** classic 'smash' edit **/
            if( ssp[iv] > cut1 ){
              snew = cut1 + c21*mytanh((ssp[iv]-cut1)*ic21) ;   /* edit s down */
              dar[iv] = fitar[iv] + snew*fsig ; didit++ ;
            } else if( ssp[iv] < -cut1 ){
              snew = -cut1 + c21*mytanh((ssp[iv]+cut1)*ic21) ;  /* edit s up */
              dar[iv] = fitar[iv] + snew*fsig ; didit++ ;
            }
          } else {                      /** local edit **/
            if( ssp[iv] >= cut2 || ssp[iv] <= -cut2 ){
              int iu , id ;
              for( iu=iv+1 ; iu < nuse ; iu++ )  /* find non-spike above */
                if( ssp[iu] < cut2 && ssp[iu] > -cut2 ) break ;
              for( id=iv-1 ; id >= 0   ; id-- )  /* find non-spike below */
                if( ssp[id] < cut2 && ssp[id] > -cut2 ) break ;
              switch( (id>=0) + 2*(iu<nuse) ){   /* compute replacement val */
                case 3: val = 0.5*(dar[iu]+dar[id]); break; /* iu and id OK */
                case 2: val =      dar[iu]         ; break; /* only iu OK   */
                case 1: val =              dar[id] ; break; /* only id OK   */
               default: val = fitar[iv]            ; break; /* shouldn't be */
              }
              dar[iv] = val ; didit++ ;
            }
          }
        } /* end of loop over time points */

        /* put edited result back into source */

        if( didit){
          for( iv=0 ; iv < nuse ; iv++ ) zar[iv] = dar[iv] ;
        }

      } /* end of processing time series when fsig is positive */

   } /* end of loop over voxels #ii */

#pragma omp critical (DESPIKE_malloc)
   { free(fit); free(ssp); free(fitar); free(var); free(dar); free(far); }

 } /* end OpenMP */
 AFNI_OMP_END ;

   for( jj=0 ; jj < nref ; jj++ ) free(ref[jj]) ;
   free(ref) ;
   return ;
}
