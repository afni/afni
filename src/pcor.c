/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "pcor.h"

/************************************************************************/
/************************************************************************/

/***
     recursive calculation of partial correlation coefficients for
     a lot of voxels at once. -- RWCox, Feb 1994
***/

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/***
   create a new references data structure:
      input:  numref = number of reference vectors to allow for
      output: pointer to the data structure
***/

references * new_references(numref)
     int numref;
{
   references *ref ;
   int ii,jj ;

   /*** check input for reasonableness ***/

   if( numref < 1 ){
      fprintf( stderr , "new_references called with numref=%d\n" , numref ) ;
      exit(-1) ;
   }

   /*** allocate storage for top level data ***/

   /* malloc->calloc   13 Feb 2009 [lesstif patrol] */
   ref = (references *) calloc( 1, sizeof(references) ) ;
   if( ref == NULL ){
      fprintf( stderr , "new_references:  malloc error for base\n" ) ;
      exit(-1) ;
   }
   ref->nref    = numref ;
   ref->nupdate = 0 ;      /* June 1995: not updated at all yet */

   /*** allocate storage for Cholesky factor
        (an array of rows, row #ii is length ii+1, for ii=0..numref-1) ***/

   ref->chol = (ref_float **) malloc( sizeof(ref_float *) * numref ) ;
   if( ref->chol == NULL ){
      fprintf( stderr , "new_references: malloc error for chol\n" ) ;
      exit(-1) ;
   }

   for( ii=0 ; ii < numref ; ii++ ){
      ref->chol[ii] = (ref_float *) malloc( sizeof(ref_float) * (ii+1) ) ;
      if( ref->chol[ii] == NULL ){
         fprintf( stderr , "new_references: malloc error for chol[ii]\n" ) ;
         exit(-1) ;
      }
   }

   /*** allocate storage for vectors of alpha, f, g ***/

   ref->alp = (ref_float *) malloc( sizeof(ref_float) * numref ) ;
   ref->ff  = (ref_float *) malloc( sizeof(ref_float) * numref ) ;
   ref->gg  = (ref_float *) malloc( sizeof(ref_float) * numref ) ;

   if( ref->alp == NULL || ref->ff == NULL || ref->gg == NULL ){
      fprintf( stderr , "new_references: malloc error for data\n" ) ;
      exit(-1) ;
   }

   /*** initialize Cholesky factor ***/

   for( ii=0 ; ii < numref ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ) RCH(ref,ii,jj) = 0.0 ;
      RCH(ref,ii,ii) = REF_EPS ;
#ifdef OV_DEBUG2
      ref->alp[ii] = ref->ff[ii] = ref->gg[ii] = 0.0 ;
#endif
   }

#ifdef OV_DEBUG2
   ref->betasq = 0.0 ;
#endif

   return ref ;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/***
   update a references structure:
      input:  vec = pointer to nref values of the reference vectors
                    at the new time point
              ref = references data structure
      output: ref is updated;
                the Cholesky factor is modified via the Carlson algorithm,
                the alpha, f, and g factors are saved for later use
***/

void update_references(vec, ref)
     float *vec;
     references *ref;
{
   int nr = ref->nref , jj,kk ;
   ref_float bold , bnew , aaa,fff,ggg ;
   static ref_float * zz = NULL ;
   static int         zz_size = -1 ;

#ifdef OV_DEBUG2
   static ref_float qinput[50] ;  /* to hold the sums of squares of inputs */
#endif

   /*** copy vector data into local storage ***/

   if( zz_size < nr ){   /* get new space, if not enough is present */

      if( zz != NULL ) free( zz ) ;
      zz      = (ref_float *) malloc( sizeof(ref_float) * nr ) ;
      zz_size = nr ;
      if( zz == NULL ){
         fprintf( stderr , "update_references: cannot malloc!\n" ) ;
         exit(-1) ;
      }
   }

   for( jj=0 ; jj < nr ; jj++) zz[jj] = (ref_float) vec[jj] ;

#ifdef OV_DEBUG2
   for( jj=0 ; jj < nr ; jj++) qinput[jj] += SQR(zz[jj]) ;  /* for later */

   REF_DUMP(ref,"before update") ;
   fprintf(stderr,"  input vec= ") ;
   for( jj=0 ; jj < nr ; jj++ ) fprintf(stderr,"%11.4e ",zz[jj]) ;
   fprintf(stderr,"\n") ;
#endif

   /*** Carlson algorithm ***/

   bold = 1.0 ;

   for( jj=0 ; jj < nr ; jj++ ){

      aaa  = zz[jj] / RCH(ref,jj,jj) ;        /* alpha */
      bnew = sqrt( bold*bold + aaa*aaa ) ;    /* new beta */
      fff  = bnew / bold ;                    /* f factor */
      ggg  = aaa  / (bnew*bold) ;             /* g factor */
      bold = bnew ;                           /* new beta becomes old beta */

      ref->alp[jj] = aaa ;   /* save these for later use */
      ref->ff[jj]  = fff ;
      ref->gg[jj]  = ggg ;

      for( kk=jj ; kk < nr ; kk++ ){
         zz[kk]        -= aaa * RCH(ref,kk,jj) ;
         RCH(ref,kk,jj) = fff * RCH(ref,kk,jj) + ggg * zz[kk] ;
      }
   }

   ref->betasq = 1.0 / ( bold * bold ) ;  /* and save this too! */

#ifdef OV_DEBUG2
   REF_DUMP(ref,"after update") ;
   fprintf(stderr,"  qsum of input vecs= ") ;
   for( jj=0 ; jj < nr ; jj++ ) fprintf(stderr,"%11.4e ",qinput[jj]) ;
   fprintf(stderr,"\n") ;
#endif

   (ref->nupdate)++ ;  /* June 1995: another update! */
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/***
   create a new voxel partial correlation data structure
      inputs:  numvox = number of voxels in image
               numref = number of reference vectors to allow for
      output:  pointer to voxel_corr data structure
***/

voxel_corr * new_voxel_corr(numvox, numref)
     int numvox;
     int numref;
{
   int vox , jj ;
   voxel_corr *vc ;

   /*** check input for OK-osity ***/

   if( numvox < 1 ){
      fprintf( stderr , "new_voxel_corr: numvox=%d\n" , numvox ) ;
      exit(-1) ;
   }

   /*** get the base storage ***/

   vc = (voxel_corr *) malloc( sizeof(voxel_corr) ) ;
   if( vc == NULL ){
      fprintf( stderr , "new_voxel_corr:  cannot malloc base\n" ) ;
      exit(-1) ;
   }

   /*** setup the references common to all voxels ***/

   vc->nvox    = numvox ;
   vc->nref    = numref ;
   vc->nupdate = 0 ;      /* June 1995: not updated at all yet */

   /*** setup the storage of the last row for each voxel ***/

   vc->chrow = (ref_float *)malloc( sizeof(ref_float) * numvox*(numref+1) );
   if( vc->chrow == NULL ){
      fprintf( stderr , "new_voxel_corr:  cannot malloc last rows\n" ) ;
      exit(-1) ;
   }

   /*** initialize each voxel ***/

   for( vox=0 ; vox < numvox ; vox++ ){
      for( jj=0 ; jj < numref ; jj++ ) VCH(vc,vox,jj) = 0 ;
      VCH(vc,vox,numref) = REF_EPS ;
   }

   return vc ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** de-allocate a references data structure ***/

void free_references(ref)
     references *ref;
{
   int ii , nr ;

   if( ref == NULL ) return ;

   nr = ref->nref ; if( nr <= 0 ) return ;

   free(ref->alp) ; free(ref->ff)  ; free(ref->gg)  ;

   for( ii=0 ; ii < nr ; ii++ ) free( ref->chol[ii] ) ;

   free(ref->chol) ; free(ref) ;

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void free_voxel_corr(vc)
     voxel_corr *vc;
{
   if( vc != NULL ){
      free( vc->chrow ) ;
      free( vc ) ;
   }
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** update all voxels with a new array of data
      inputs:  vdata = array[nvox] of new data for each voxel
               ref   = pointer to references structure to use
               vc    = pointer to correlation data structure
      output:  updated vc
***/

void update_voxel_corr(vdata, ref, vc)
     vox_data *vdata;
     references *ref;
     voxel_corr *vc;
{
   int vox , jj ,       /* loop indices */
       nv = vc->nvox ,  /* number of voxels */
       nr = vc->nref ;  /* number of references */

   ref_float *aaa = ref->alp ,
             *fff = ref->ff  ,
             *ggg = ref->gg  ;

   ref_float zz ,
             bq = ref->betasq ;

#ifdef OV_DEBUG2
   static ref_float qvox = 0.0 ;
#endif

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "update_voxel_corr: reference size mismatch!\n" ) ;
      exit(-1) ;
   }

#ifdef OV_DEBUG2
   VOX_DUMP(vc,VD,"before update") ;
#ifdef VOX_SHORT
   fprintf(stderr,"  integer input data = %d\n" , vdata[VD] ) ;
#else
   fprintf(stderr,"  float input data = %11.4e\n" , vdata[VD] ) ;
#endif
   qvox += SQR(vdata[VD]) ;
#endif

/** innermost loop expansion is for speedup if nref is small, if enabled **/

#ifdef EXPAND_UPDATE
#define UPZZ(j)  zz -= aaa[j] * VCH(vc,vox,j)
#define UPCH(j)  VCH(vc,vox,j) = fff[j] * VCH(vc,vox,j) + ggg[j] * zz
#define UPLL(j)  VCH(vc,vox,j) += bq * zz * zz

   switch( nr ){
   default:
#endif

   /*** for each voxel ***/

   for( vox=0 ; vox < nv ; vox++ ){

      /*** update last row of each Cholesky factor ***/

      zz = (ref_float) vdata[vox] ;
      for( jj=0 ; jj < nr ; jj++ ){
         zz            -= aaa[jj] * VCH(vc,vox,jj) ;
         VCH(vc,vox,jj) = fff[jj] * VCH(vc,vox,jj) + ggg[jj] * zz ;
      }
      VCH(vc,vox,nr) += bq * zz * zz ; /* square of true Cholesky diagonal */
   }

#ifdef EXPAND_UPDATE
   break ;

   case 1:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ; UPLL(1) ;
      }
   break ;

   case 2:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ; UPLL(2) ;
      }
   break ;

   case 3:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ; UPLL(3) ;
   }
   break ;

   case 4:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ; UPLL(4) ;
   }
   break ;

   case 5:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ; UPLL(5) ;
   }
   break ;

   case 6:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ; UPLL(6) ;
   }
   break ;

   case 7:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ; UPLL(7) ;
   }
   break ;

   case 8:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ;
         UPZZ(7) ; UPCH(7) ; UPLL(8) ;
   }
   break ;

   case 9:
      for( vox=0 ; vox < nv ; vox++ ){
         zz = (ref_float) vdata[vox] ;
         UPZZ(0) ; UPCH(0) ;
         UPZZ(1) ; UPCH(1) ;
         UPZZ(2) ; UPCH(2) ;
         UPZZ(3) ; UPCH(3) ;
         UPZZ(4) ; UPCH(4) ;
         UPZZ(5) ; UPCH(5) ;
         UPZZ(6) ; UPCH(6) ;
         UPZZ(7) ; UPCH(7) ;
         UPZZ(8) ; UPCH(8) ; UPLL(9) ;
   }
   break ;

   }
#endif /* EXPAND_UPDATE */

#ifdef OV_DEBUG2
   VOX_DUMP(vc,VD,"after update") ;
   fprintf(stderr,"  qsum of vox[VD]=%11.4e\n",qvox) ;
#endif

   (vc->nupdate)++ ;  /* June 1995: another update */
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** compute the partial correlation coefficients ***/

void get_pcor(ref, vc, pcor)
     references *ref;
     voxel_corr *vc;
     float *pcor;
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   ref_float den ;
#define DENEPS 1.e-5

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "get_pcor: reference size mismatch!\n" ) ;
      exit(-1) ;
   }

   /*** Work ***/

   for( vox=0 ; vox < nv ; vox++ ){

      den = VCH(vc,vox,nr) ;
      if( den > DENEPS ){
         pcor[vox] = VCH(vc,vox,nr-1)
                      / sqrt( den + SQR(VCH(vc,vox,nr-1)) ) ;
      } else {
         pcor[vox] = 0.0 ;
      }

   }

#ifdef OV_DEBUG2
   fprintf(stderr,"get_pcor: pcor[VD]=%11.4e\n",pcor[VD]) ;
#endif

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get activation coefficient ***/

void get_coef(ref, vc, coef)
     references *ref;
     voxel_corr *vc;
     float *coef;
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   ref_float scale ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "get_coef: reference size mismatch!\n" ) ;
      exit(-1) ;
   }

   /*** Work ***/

   scale = 1.0 / RCH(ref,nr-1,nr-1) ;

   for( vox=0 ; vox < nv ; vox++ ){
      coef[vox] = scale * VCH(vc,vox,nr-1) ;
   }

#ifdef OV_DEBUG2
   fprintf(stderr,"get_coef: coef[VD]=%11.4e\n",coef[VD]) ;
#endif

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get variance estimate (June 1995) ***/

void get_variance(vc, var)
     voxel_corr *vc;
     float *var;
{
   int vox , nv = vc->nvox , nr = vc->nref , nup = vc->nupdate ;
   ref_float scale ;

   /*** check inputs for OK-ness ***/

   if( nup <= nr ){
      fprintf(stderr,"get_variance: not enough data to compute!\n") ;
      for( vox=0 ; vox < nv ; vox++ ) var[vox] = 0.0 ;
      return ;
   }

   /*** Work ***/

   scale = 1.0 / ( nup - nr ) ;

   for( vox=0 ; vox < nv ; vox++ ){
      var[vox] = scale * VCH(vc,vox,nr) ;
   }

#ifdef OV_DEBUG2
   fprintf(stderr,"get_variance: var[VD]=%11.4e\n",var[VD]) ;
#endif

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** Get least squares fit coefficients (all of them, Frank).
     "fit" is an array of pointers to floats, of length nref.
     If fit[j] != NULL, then it points to an array of size nvox that
     will get the coefficient for reference #j (j=0..nref-1).  [June 1995] ***/

void get_lsqfit(ref, vc, fit)
     references *ref;
     voxel_corr *vc;
     float *fit[] ;
{
   int vox,jj,kk , nv = vc->nvox , nr = vc->nref ;
   ref_float sum ;
   ref_float * ff ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "get_lsqfit: reference size mismatch!\n" ) ;
      exit(-1) ;
   }

   kk = 0 ;
   for( jj=0 ; jj < nr ; jj++ ) kk += (fit[jj] != NULL) ;
   if( kk == 0 ){
      fprintf(stderr,"get_lsqfit: NO OUTPUT REQUESTED!\n") ;
      return ;
   }

   ff = (ref_float *) malloc( sizeof(ref_float) * nr ) ;
   if( ff == NULL ){
      fprintf( stderr, "get_lsqfit: cannot malloc workspace!\n") ;
      exit(-1) ;
   }

   /*** for each voxel, compute the nr fit coefficients (backwards) ***/

   for( vox=0 ; vox < nv ; vox++ ){

      for( jj=nr-1 ; jj >=0 ; jj-- ){
         sum = VCH(vc,vox,jj) ;
         for( kk=jj+1 ; kk < nr ; kk++ ) sum -= ff[kk] * RCH(ref,kk,jj) ;
         ff[jj] = sum / RCH(ref,jj,jj) ;
      }

      for( jj=0 ; jj < nr ; jj++ )
         if( fit[jj] != NULL ) fit[jj][vox] = ff[jj] ;
   }

   free( ff ) ;
   return ;
}


/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get correlation and thresholded alpha:

     only |pcor| >= pcthresh will be computed;
     only those voxels will have coef computed;
     if cothresh > 0, then voxels whose |coef| is less than
       cothresh * max|coef| will be also be set to zero
***/

void get_pcor_thresh_coef(ref, vc, pcthresh, cothresh, pcor, coef, thr)
     references *ref;
     voxel_corr *vc;
     float pcthresh;
     float cothresh;
     float *pcor;
     float *coef;
     thresh_result *thr;
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   ref_float den , num , scale ;
#define DENEPS 1.e-5

   float pc , co , thfac ;
   int   npc_pos=0 , npc_neg=0 , nco_pos=0 , nco_neg=0 ;
   float mpc_pos=0., mpc_neg=0., mco_pos=0., mco_neg=0.;

   int do_pcth , do_coth ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "get_pcor: reference size mismatch!\n" ) ;
      exit(-1) ;
   }

   scale   = 1.0 / RCH(ref,nr-1,nr-1) ;      /* for coef calculation */
   thfac   = SQR(pcthresh)/(1.0-SQR(pcthresh)) ;
   do_pcth = pcthresh <= 0.0 ;               /* whether to do these tests */
   do_coth = cothresh  > 0.0 ;

   /*** Compute pcor and coef, thresholded on pcthresh ***/

   for( vox=0 ; vox < nv ; vox++ ){

      den = VCH(vc,vox,nr) ;
      num = VCH(vc,vox,nr-1) ;
      if( do_pcth || SQR(num) > thfac*den ){   /* fancy threshold test */

         pc = pcor[vox] = num / sqrt(den+SQR(num)) ;
         co = coef[vox] = scale * num ;

         if( pc > 0 ){
            npc_pos++ ;
            if( mpc_pos < pc ) mpc_pos = pc ;
            if( mco_pos < co ) mco_pos = co ;
         } else {
            npc_neg++ ;
            if( mpc_neg > pc ) mpc_neg = pc ;
            if( mco_neg > co ) mco_neg = co ;
         }

      } else {                                    /* fails pcor thresh */
         pcor[vox] = coef[vox] = 0.0 ;
      }

   }

   nco_pos = npc_pos ;
   nco_neg = npc_neg ;

/*** threshold coef on cothresh as well ***/

   if( do_coth && nco_pos+nco_neg > 0 ){

      thfac = cothresh * MAX(mco_pos,-mco_neg) ;

      for( vox=0 ; vox < nv ; vox++ ){
         if( coef[vox] > 0.0 && coef[vox] < thfac ){
            coef[vox] = 0.0 ;
            nco_pos-- ;
         } else if( coef[vox] < 0.0 && coef[vox] > -thfac ){
            coef[vox] = 0.0 ;
            nco_neg-- ;
         }
      }
   }

/*** load threshold output report ***/

   thr->num_pcor_pos = npc_pos ;
   thr->num_pcor_neg = npc_neg ;
   thr->max_pcor_pos = mpc_pos ;
   thr->max_pcor_neg = mpc_neg ;

   thr->num_coef_pos = nco_pos ;
   thr->num_coef_neg = nco_neg ;
   thr->max_coef_pos = mco_pos ;
   thr->max_coef_neg = mco_neg ;

   return ;
}
