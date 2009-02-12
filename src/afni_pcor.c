/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#undef MAIN
#include "afni_pcor.h"

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/***
   create a new references data structure:
      input:  numref = number of reference vectors to allow for
      output: pointer to the data structure
***/

PCOR_references * new_PCOR_references(int numref)
{
   PCOR_references *ref ;
   int ii,jj , nbad ;

   /*** check input for reasonableness ***/

   if( numref < 1 ){
      fprintf( stderr , "new_PCOR_references called with numref=%d\n" , numref ) ;
      return NULL ;
   }

   /*** allocate storage for top level data ***/

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   ref = (PCOR_references *) calloc( 1, sizeof(PCOR_references) ) ;
   if( ref == NULL ){
      fprintf( stderr , "new_PCOR_references:  malloc error for base\n" ) ;
      return NULL ;
   }
   ref->nref    = numref ;
   ref->nupdate = 0 ;      /* June 1995: not updated at all yet */

   /*** allocate storage for Cholesky factor
        (an array of rows, row #ii is length ii+1, for ii=0..numref-1) ***/

   ref->chol = (float **) malloc( sizeof(float *) * numref ) ;
   if( ref->chol == NULL ){
      fprintf( stderr , "new_PCOR_references: malloc error for chol\n" ) ;
      free(ref) ; return NULL ;
   }

   for( ii=0,nbad=0 ; ii < numref ; ii++ ){
      ref->chol[ii] = (float *) malloc( sizeof(float) * (ii+1) ) ;
      if( ref->chol[ii] == NULL ) nbad++ ;
   }

   if( nbad > 0 ){
      fprintf( stderr , "new_PCOR_references: malloc error for chol[ii]\n" ) ;
      free_PCOR_references( ref ) ; return NULL ;
   }

   /*** allocate storage for vectors of alpha, f, g ***/

   ref->alp = (float *) malloc( sizeof(float) * numref ) ;
   ref->ff  = (float *) malloc( sizeof(float) * numref ) ;
   ref->gg  = (float *) malloc( sizeof(float) * numref ) ;

   if( ref->alp == NULL || ref->ff == NULL || ref->gg == NULL ){
      fprintf( stderr , "new_PCOR_references: malloc error for data\n" ) ;
      free_PCOR_references( ref ) ; return NULL ;
   }

   /*** 14 Jan 1998: space for ref vector statistics ***/

   ref->rmin = (float *) malloc( sizeof(float) * numref ) ;
   ref->rmax = (float *) malloc( sizeof(float) * numref ) ;
   ref->rsum = (float *) malloc( sizeof(float) * numref ) ;

   if( ref->rmin == NULL || ref->rmax == NULL || ref->rsum == NULL ){
      fprintf( stderr , "new_PCOR_references: malloc error for data\n" ) ;
      free_PCOR_references( ref ) ; return NULL ;
   }

   /*** initialize Cholesky factor ***/

   for( ii=0 ; ii < numref ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ) RCH(ref,ii,jj) = 0.0 ;
      RCH(ref,ii,ii) = REF_EPS ;
   }

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

void update_PCOR_references(float * vec, PCOR_references * ref)
{
   int nr = ref->nref , jj,kk ;
   float bold , bnew , aaa,fff,ggg ;

   static float * zz = NULL ;      /* local storage for a copy of vec */
   static int     zz_size = -1 ;

   /*** copy vector data into local storage (will be altered below) ***/

   if( zz_size < nr ){   /* get new space, if not enough is present */

      if( zz != NULL ) free( zz ) ;
      zz      = (float *) malloc( sizeof(float) * nr ) ;
      zz_size = nr ;
      if( zz == NULL ){
         fprintf( stderr , "\nupdate_PCOR_references: can't malloc!\n" ) ;
         EXIT(1) ;
      }
   }

   for( jj=0 ; jj < nr ; jj++) zz[jj] = vec[jj] ;

   /*** 14 Jan 1998: collect ref vector stats ***/

   if( ref->nupdate == 0 ){  /* initialize */
      for( jj=0 ; jj < nr ; jj++ ){
         ref->rmin[jj] = ref->rmax[jj] = ref->rsum[jj] = zz[jj] ;
      }
   } else {
      register float val ;
      for( jj=0 ; jj < nr ; jj++ ){
         val = zz[jj] ;                 ref->rsum[jj] += val ;
              if( val < ref->rmin[jj] ) ref->rmin[jj]  = val ;
         else if( val > ref->rmax[jj] ) ref->rmax[jj]  = val ;
      }
   }

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

PCOR_voxel_corr * new_PCOR_voxel_corr(int numvox, int numref)
{
   int vox , jj ;
   PCOR_voxel_corr *vc ;

   /*** check input for OK-osity ***/

   if( numvox < 1 ){
      fprintf( stderr , "new_PCOR_voxel_corr: numvox=%d\n" , numvox ) ;
      return NULL ;
   }

   /*** get the base storage ***/

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   vc = (PCOR_voxel_corr *) calloc( 1, sizeof(PCOR_voxel_corr) ) ;
   if( vc == NULL ){
      fprintf( stderr , "new_PCOR_voxel_corr:  can't malloc base\n" ) ;
      return NULL ;
   }

   /*** setup the references common to all voxels ***/

   vc->nvox    = numvox ;
   vc->nref    = numref ;
   vc->nupdate = 0 ;      /* June 1995: not updated at all yet */

   /*** setup the storage of the last row for each voxel ***/

   vc->chrow = (float *)malloc( sizeof(float) * numvox*(numref+1) );
   if( vc->chrow == NULL ){
      fprintf( stderr , "new_PCOR_voxel_corr:  can't malloc last rows\n" ) ;
      free( vc ) ; return NULL ;
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

void free_PCOR_references(PCOR_references * ref)
{
   int ii , nr ;

   if( ref == NULL ) return ;

   nr = ref->nref ; if( nr <= 0 ) return ;

   if( ref->alp  != NULL ) free(ref->alp) ;
   if( ref->ff   != NULL ) free(ref->ff)  ;
   if( ref->gg   != NULL ) free(ref->gg)  ;
   if( ref->rmin != NULL ) free(ref->rmin);  /* 14 Jan 1998 */
   if( ref->rmax != NULL ) free(ref->rmax);
   if( ref->rsum != NULL ) free(ref->rsum);

   if( ref->chol != NULL ){
      for( ii=0 ; ii < nr ; ii++ )
         if( ref->chol[ii] != NULL ) free( ref->chol[ii] ) ;
      free(ref->chol) ;
   }

   free(ref) ;
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void free_PCOR_voxel_corr(PCOR_voxel_corr * vc)
{
   if( vc != NULL ){
      if( vc->chrow != NULL ) free( vc->chrow ) ;
      free( vc ) ;
   }
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** compute the partial correlation coefficients ***/
/*** array pcor must be large enough to hold the results ***/

#define DENEPS 1.e-5

void PCOR_get_pcor(PCOR_references * ref, PCOR_voxel_corr * vc, float * pcor)
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   float den ;
   static float deneps=-666.0 ; /* 28 Sep 1999 */

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_pcor: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }

   /* 28 Sep 1999: load user option for denominator epsilon */

   if( deneps < 0.0 ){
      char * ccc = my_getenv("AFNI_PCOR_DENEPS") ;
      if( ccc != NULL ) deneps = strtod( ccc , NULL ) ;
      if( deneps < 0.0 ) deneps = DENEPS ;
   }

   /*** Work ***/

   for( vox=0 ; vox < nv ; vox++ ){

   /* change below made 15 July 1998 */
#if 0
      den = VCH(vc,vox,nr) ;
      if( den > DENEPS ){
         pcor[vox] = VCH(vc,vox,nr-1)
                      / sqrt( den + SQR(VCH(vc,vox,nr-1)) ) ;
      } else {
         pcor[vox] = 0.0 ;
      }
#else
      /*----- Allow for numerical roundoff error,  26 August 1998  BDW -----*/
      /*----- Replace DENEPS with deneps:          28 September 1999 RWC ---*/

      den = VCH(vc,vox,nr) + SQR(VCH(vc,vox,nr-1)) ;
      if( den > deneps )
         pcor[vox] = VCH(vc,vox,nr-1) / sqrt(den) ;
      else
         pcor[vox] = 0.0 ;
#endif

   }

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void PCOR_get_mcor(PCOR_references * ref, PCOR_voxel_corr * vc, int m , float * pcor)
{
   int vox , nv = vc->nvox , nr = vc->nref , ii ;
   double den ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_mcor: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }
   if( m >= nr ){
      fprintf( stderr , "\nPCOR_get_mcor: m=%d but nref=%d\n",m,nr) ;
      EXIT(1) ;
   }

   /*** Work ***/

   for( vox=0 ; vox < nv ; vox++ ){
      den = VCH(vc,vox,nr) ;
      switch(m){
         default:
            for( ii=1 ; ii <= m ; ii++ ) den += SQR(VCH(vc,vox,nr-ii)) ;
         break ;

         case 1: den +=  SQR(VCH(vc,vox,nr-1)) ; break ;

         case 2: den +=  SQR(VCH(vc,vox,nr-1))
                       + SQR(VCH(vc,vox,nr-2)) ; break ;

         case 3: den +=  SQR(VCH(vc,vox,nr-1))
                       + SQR(VCH(vc,vox,nr-2))
                       + SQR(VCH(vc,vox,nr-3)) ; break ;

         case 4: den +=  SQR(VCH(vc,vox,nr-1))
                       + SQR(VCH(vc,vox,nr-2))
                       + SQR(VCH(vc,vox,nr-3))
                       + SQR(VCH(vc,vox,nr-4)) ; break ;
      }

      den = 1.0 - VCH(vc,vox,nr) / den ;
      pcor[vox] = (den > 0.0) ? sqrt(den) : 0.0 ;
   }

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get activation coefficient ***/
/*** array coef must be big enough to hold the results ***/

void PCOR_get_coef(PCOR_references * ref, PCOR_voxel_corr * vc, float * coef)
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   float scale ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_coef: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }

   /*** Work ***/

   scale = 1.0 / RCH(ref,nr-1,nr-1) ;

   for( vox=0 ; vox < nv ; vox++ ){
      coef[vox] = scale * VCH(vc,vox,nr-1) ;
   }

   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get variance estimate (June 1995) ***/

void PCOR_get_variance(PCOR_voxel_corr * vc, float * var)
{
   int vox , nv = vc->nvox , nr = vc->nref , nup = vc->nupdate ;
   float scale ;

   /*** check inputs for OK-ness ***/

   if( nup <= nr ){
      fprintf(stderr,"PCOR_get_variance: not enough data to compute!\n") ;
      for( vox=0 ; vox < nv ; vox++ ) var[vox] = 0.0 ;
      return ;
   }

   /*** Work ***/

   scale = 1.0 / ( nup - nr ) ;

   for( vox=0 ; vox < nv ; vox++ ){
      var[vox] = scale * VCH(vc,vox,nr) ;
   }

   return ;
}

void PCOR_get_stdev(PCOR_voxel_corr * vc, float * sig)  /* 03 Jan 2000 */
{
   int vox , nv = vc->nvox ;
   PCOR_get_variance( vc , sig ) ;
   for( vox=0 ; vox < nv ; vox++ ) sig[vox] = sqrt(fabs(sig[vox])) ;
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** Get least squares fit coefficients (all of them, Frank).
     "fit" is an array of pointers to floats, of length nref.
     If fit[j] != NULL, then it points to an array of size nvox that
     will get the coefficient for reference #j (j=0..nref-1).  [June 1995] ***/

void PCOR_get_lsqfit(PCOR_references * ref, PCOR_voxel_corr * vc, float *fit[] )
{
   int vox,jj,kk , nv = vc->nvox , nr = vc->nref ;
   float sum ;
   float * ff ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_lsqfit: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }

   kk = 0 ;
   for( jj=0 ; jj < nr ; jj++ ) kk += (fit[jj] != NULL) ;
   if( kk == 0 ){
      fprintf(stderr,"PCOR_get_lsqfit: NO OUTPUT REQUESTED!\n") ;
      return ;
   }

   ff = (float *) malloc( sizeof(float) * nr ) ;
   if( ff == NULL ){
      fprintf( stderr, "\nPCOR_get_lsqfit: can't malloc workspace!\n") ;
      EXIT(1) ;
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

/*** get percent change (due to last reference).
     array coef must be big enough to hold the results.
     if bline != NULL, it gets the baseline estimate.
     08 Sep 1999:
       basaver = 0 ==> baseline for percent change calculation
                       is the bottom of the curve [old method]
               = 1 ==> baseline is average of the curve [for AJ]
               = 2 ==> baseline is the top of the curve [03 Jan 2000] ***/

void PCOR_get_perc(PCOR_references * ref, PCOR_voxel_corr * vc,
                   float * coef, float * bline, int basaver )
{
   int vox,jj,kk , nv=vc->nvox , nr=vc->nref , nup=ref->nupdate ;
   float sum , base , rdif , rmin , rmax ;
   float * ff , * bb , * dd ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_perc: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }

   if( coef == NULL && bline == NULL ) return ;  /* nothing to do */

   /*** Setup ***/

   ff = (float *) malloc( sizeof(float) * nr ) ;
   bb = (float *) malloc( sizeof(float) * nr ) ;
   dd = (float *) malloc( sizeof(float) * nr ) ;
   if( ff == NULL || bb == NULL || dd == NULL ){
      fprintf( stderr, "\nPCOR_get_perc: can't malloc workspace!\n") ;
      EXIT(1) ;
   }

   /* range of last reference */

   rmin = ref->rmin[nr-1] ;
   rmax = ref->rmax[nr-1] ; rdif = 100.0 * (rmax-rmin) ;
   if( rdif == 0.0 ){
      /** 06 Dec 2000: zero out both coef and bline in this case **/
      if( coef  != NULL ) for( vox=0; vox < nv; vox++ ) coef[vox]  = 0.0;
      if( bline != NULL ) for( vox=0; vox < nv; vox++ ) bline[vox] = 0.0;
      fprintf(stderr,"\nPCOR_get_perc: ref vector has no range!\n") ;
      return ;
   }

   for( jj=0 ; jj < nr ; jj++ ){
      bb[jj] = ref->rsum[jj] / nup ;    /* average of each ref */
      dd[jj] = 1.0 / RCH(ref,jj,jj) ;   /* factor for loop below */
   }

   /*** Work:                               jj=nr-2
        x(t) = fit[nr-1] * ref(t,nr-1) + SUM       fit[jj] * ref(t,jj)
                                            jj=0

        The percent change due to ref(t,nr-1) is computed by scaling
        this ref to run from 0 to 1 (that's why rmin and rmax are used),
        and by computing the baseline as fit[nr-1]*rmin + the sum
        of the averages of the other refs times their fit coefficients. ***/

   for( vox=0 ; vox < nv ; vox++ ){
      for( jj=nr-1 ; jj >=0 ; jj-- ){  /* compute fit coefficients */
         sum = VCH(vc,vox,jj) ;
         for( kk=jj+1 ; kk < nr ; kk++ ) sum -= ff[kk] * RCH(ref,kk,jj) ;
         ff[jj] = sum * dd[jj] ;
      }

      switch( basaver ){
          default:
          case 0: base = ff[nr-1] * rmin    ; break; /* baseline = bottom  */
          case 1: base = ff[nr-1] * bb[nr-1]; break; /* baseline = average */
          case 2: base = ff[nr-1] * rmax    ; break; /* baseline = top     */
      }

      for( jj=0 ; jj < nr-1 ; jj++ )   /* 30 May 1999: used to have nr-2 */
         base += ff[jj] * bb[jj] ;     /*              which was wrong!  */

      if( coef != NULL ) coef[vox] = (base > 0.0) ? ff[nr-1] * rdif / base
                                                  : 0.0 ;
      if( bline != NULL ) bline[vox] = base ;
   }

   free(ff) ; free(bb) ; free(dd) ;
   return ;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** get correlation and alpha:

     only |pcor| >= pcthresh will be computed;
     only those voxels will have coef computed;
     arrays pcor and coef must be big enough to hold the results.
***/

void PCOR_get_pcor_and_coef(PCOR_references * ref , PCOR_voxel_corr * vc ,
                            float pcthresh , float * pcor , float * coef )
{
   int vox , nv = vc->nvox , nr = vc->nref ;
   float den , num , scale ;
   float pc , co , thfac ;

   /*** check inputs for OK-ness ***/

   if( vc->nref != ref->nref ){
      fprintf( stderr , "\nPCOR_get_pcor_and_coef: reference size mismatch!\n" ) ;
      EXIT(1) ;
   }

   scale   = 1.0 / RCH(ref,nr-1,nr-1) ;      /* for coef calculation */
   thfac   = SQR(pcthresh)/(1.0-SQR(pcthresh)) ;

   /*** Compute pcor and coef, thresholded on pcthresh ***/

   if( pcthresh <= 0.0 ){
      for( vox=0 ; vox < nv ; vox++ ){
         den       = VCH(vc,vox,nr) ;
         num       = VCH(vc,vox,nr-1) ;
         pcor[vox] = num / sqrt(den+SQR(num)) ;
         coef[vox] = scale * num ;
      }
   } else {
      thfac   = SQR(pcthresh)/(1.0-SQR(pcthresh)) ;
      for( vox=0 ; vox < nv ; vox++ ){
         den = VCH(vc,vox,nr) ;
         num = VCH(vc,vox,nr-1) ;
         if( SQR(num) > thfac*den ){                 /* fancy threshold test */
            pcor[vox] = num / sqrt(den+SQR(num)) ;
            coef[vox] = scale * num ;
         } else {                                    /* fails pcor thresh */
            pcor[vox] = coef[vox] = 0.0 ;
         }
      }
   }

   return ;
}
