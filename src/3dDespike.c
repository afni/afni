#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/************************************************************************/
/******* Hack to remove large spikes from time series data (oog). *******/
/******* 30 Aug 2002 - RWCox                                      *******/
/************************************************************************/

#define TFAC  0.1    /* scale factor for -ssave dataset */
#define ITFAC 10.0   /* inverse of above */

/*----------------------------------------------------------------------*/

static INLINE float mytanh( float x )
{
  register float ex , exi ;
       if( x >  7.0f ) return  1.0f ;  /* 03 Sep: check for stupid inputs */
  else if( x < -7.0f ) return -1.0f ;
  ex = exp(x) ; exi = 1.0f/ex ;
  return (ex-exi)/(ex+exi) ;
}

/*----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset , *oset=NULL , *tset=NULL ;
   int nvals , iv , nxyz , ii,jj,kk , iarg , kz,kzold ;
   float cut1=2.5,cut2=4.0 , sq2p , fq ;
   MRI_IMAGE *flim ;
   char *prefix="despike" , *tprefix=NULL ;

   int corder=-1 , nref , ignore=0 , polort=2 , nuse , nomask=0 ;
   int nspike, nbig, nproc ;
   float **ref ;
   float  c21,ic21 , pspike,pbig ;
   short  *sar , *qar ;
   byte   *tar , *mask=NULL ;
   float  *zar , *yar ;
   int     datum ;
   int     localedit=0 ;  /* 04 Apr 2007 */
   int     verb=1 ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dDespike [options] dataset\n"
             "Removes 'spikes' from the 3D+time input dataset and writes\n"
             "a new dataset with the spike values replaced by something\n"
             "more pleasing to the eye.\n"
             "\n"
             "Method:\n"
             " * L1 fit a smooth-ish curve to each voxel time series\n"
             "    [see -corder option for description of the curve].\n"
             " * Compute the MAD of the difference between the curve and\n"
             "    the data time series (the residuals).\n"
             " * Estimate the standard deviation 'sigma' of the residuals\n"
             "    as sqrt(PI/2)*MAD.\n"
             " * For each voxel value, define s = (value-curve)/sigma.\n"
             " * Values with s > c1 are replaced with a value that yields\n"
             "    a modified s' = c1+(c2-c1)*tanh((s-c1)/(c2-c1)).\n"
             " * c1 is the threshold value of s for a 'spike' [default c1=2.5].\n"
             " * c2 is the upper range of the allowed deviation from the curve:\n"
             "    s=[c1..infinity) is mapped to s'=[c1..c2)   [default c2=4].\n"
             "\n"
             "Options:\n"
             " -ignore I  = Ignore the first I points in the time series:\n"
             "               these values will just be copied to the\n"
             "               output dataset [default I=0].\n"
             " -corder L  = Set the curve fit order to L:\n"
             "               the curve that is fit to voxel data v(t) is\n"
             "\n"
             "                       k=L [        (2*PI*k*t)          (2*PI*k*t) ]\n"
             " f(t) = a+b*t+c*t*t + SUM  [ d * sin(--------) + e * cos(--------) ]\n"
             "                       k=1 [  k     (    T   )    k     (    T   ) ]\n"
             "\n"
             "               where T = duration of time series;\n"
             "               the a,b,c,d,e parameters are chosen to minimize\n"
             "               the sum over t of |v(t)-f(t)| (L1 regression);\n"
             "               this type of fitting is is insensitive to large\n"
             "               spikes in the data.  The default value of L is\n"
             "               NT/30, where NT = number of time points.\n"
             "\n"
             " -cut c1 c2 = Alter default values for the spike cut values\n"
             "               [default c1=2.5, c2=4.0].\n"
             " -prefix pp = Save de-spiked dataset with prefix 'pp'\n"
             "               [default pp='despike']\n"
             " -ssave ttt = Save 'spikiness' measure s for each voxel into a\n"
             "               3D+time dataset with prefix 'ttt' [default=no save]\n"
             " -nomask    = Process all voxels\n"
             "               [default=use a mask of high-intensity voxels, ]\n"
             "               [as created via '3dAutomask -dilate 4 dataset'].\n"
             " -q[uiet]   = Don't print '++' informational messages.\n"
             "\n"
             " -localedit = Change the editing process to the following:\n"
             "                If a voxel |s| value is >= c2, then replace\n"
             "                the voxel value with the average of the two\n"
             "                nearest non-spike (|s| < c2) values; the first\n"
             "                one previous and the first one after.\n"
             "                Note that the c1 cut value is not used here.\n"
             "\n"
             "Caveats:\n"
             "* Despiking may interfere with image registration, since head\n"
             "   movement may produce 'spikes' at the edge of the brain, and\n"
             "   this information would be used in the registration process.\n"
             "   This possibility has not been explored or calibrated.\n"
             "* Check your data visually before and after despiking and\n"
             "   registration!\n"
             "   [Hint: open 2 AFNI controllers, and turn Time Lock on.]\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dDespike main"); machdep(); AFNI_logger("3dDespike",argc,argv);
   PRINT_VERSION("3dDespike") ; AUTHOR("RW Cox") ;

   /** parse options **/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-q",2) == 0 ){       /* 04 Apr 2007 */
        verb = 0 ; iarg++ ; continue ;
      }
      if( strncmp(argv[iarg],"-v",2) == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      /** -localedit **/

      if( strcmp(argv[iarg],"-localedit") == 0 ){  /* 04 Apr 2007 */
        localedit = 1 ; iarg++ ; continue ;
      }

      /** don't use masking **/

      if( strcmp(argv[iarg],"-nomask") == 0 ){
        nomask = 1 ; iarg++ ; continue ;
      }

      /** output dataset prefix **/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) ) ERROR_exit("-prefix is not good");
        iarg++ ; continue ;
      }

      /** ratio dataset prefix **/

      if( strcmp(argv[iarg],"-ssave") == 0 ){
        tprefix = argv[++iarg] ;
        if( !THD_filename_ok(tprefix) ) ERROR_exit("-ssave prefix is not good");
        iarg++ ; continue ;
      }

      /** trigonometric polynomial order **/

      if( strcmp(argv[iarg],"-corder") == 0 ){
        corder = strtol( argv[++iarg] , NULL , 10 ) ;
        if( corder < 0 ) ERROR_exit("Illegal value of -corder");
        iarg++ ; continue ;
      }

      /** how much to ignore at start **/

      if( strcmp(argv[iarg],"-ignore") == 0 ){
        ignore = strtol( argv[++iarg] , NULL , 10 ) ;
        if( ignore < 0 ) ERROR_exit("Illegal value of -ignore");
        iarg++ ; continue ;
      }

      /** thresholds for s ratio **/

      if( strcmp(argv[iarg],"-cut") == 0 ){
        cut1 = strtod( argv[++iarg] , NULL ) ;
        cut2 = strtod( argv[++iarg] , NULL ) ;
        if( cut1 < 1.0 || cut2 < cut1+0.5 )
          ERROR_exit("Illegal values after -cut");
        iarg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   c21 = cut2-cut1 ; ic21 = 1.0/c21 ;

   /*----- read input dataset -----*/

   if( iarg >= argc ) ERROR_exit("No input dataset!!??");

   dset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   datum = DSET_BRICK_TYPE(dset,0) ;
   if( (datum != MRI_short && datum != MRI_float) || !DSET_datum_constant(dset) )
     ERROR_exit("Can't process non-short, non-float dataset!") ;

   if( verb ) INFO_message("Input data type = %s\n",MRI_TYPE_name[datum]) ;
   nvals = DSET_NUM_TIMES(dset) ; nuse = nvals - ignore ;
   if( nuse < 15 )
     ERROR_exit("Can't use dataset with < 15 time points per voxel!") ;

   if( verb ) INFO_message("ignoring first %d time points, using last %d",ignore,nuse);
   if( corder > 0 && 4*corder+2 > nuse ){
     ERROR_exit("-corder %d is too big for NT=%d",corder,nvals) ;
   } else if( corder < 0 ){
     corder = rint(nuse/30.0) ;
     if( verb ) INFO_message("using %d time points => -corder %d",nuse,corder) ;
   } else {
     if( verb ) INFO_message("-corder %d set from command line",corder) ;
   }
   nxyz = DSET_NVOX(dset) ;
   if( verb ) INFO_message("Loading dataset %s",argv[iarg]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /*-- create automask --*/

   if( !nomask ){
     mask = THD_automask( dset ) ;
     for( ii=0 ; ii < 4 ; ii++ )
       THD_mask_dilate( DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset), mask, 3 ) ;
     ii = THD_countmask( DSET_NVOX(dset) , mask ) ;
     if( verb ) INFO_message("%d voxels in the mask [out of %d in dataset]",ii,DSET_NVOX(dset)) ;
   } else {
     if( verb ) INFO_message("processing all %d voxels in dataset",DSET_NVOX(dset)) ;
   }

   /*-- create empty despiked dataset --*/

   oset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , datum ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) )
     ERROR_exit("output dataset already exists: %s",DSET_HEADNAME(oset));

   tross_Copy_History( oset , dset ) ;
   tross_Make_History( "3dDespike" , argc , argv , oset ) ;

   /* create bricks (will be filled with zeros) */

   for( iv=0 ; iv < nvals ; iv++ )
     EDIT_substitute_brick( oset , iv , datum , NULL ) ;

   /* copy the ignored bricks */

   switch( datum ){
     case MRI_short:
       for( iv=0 ; iv < ignore ; iv++ ){
         sar = DSET_ARRAY(oset,iv) ;
         qar = DSET_ARRAY(dset,iv) ;
         memcpy( sar , qar , DSET_BRICK_BYTES(dset,iv) ) ;
         DSET_unload_one(dset,iv) ;
       }
     break ;
     case MRI_float:
       for( iv=0 ; iv < ignore ; iv++ ){
         zar = DSET_ARRAY(oset,iv) ;
         yar = DSET_ARRAY(dset,iv) ;
         memcpy( zar , yar , DSET_BRICK_BYTES(dset,iv) ) ;
         DSET_unload_one(dset,iv) ;
       }
     break ;
   }

   /*-- setup to save a threshold statistic dataset, if desired --*/

   if( tprefix != NULL ){
     float *fac ;
     tset = EDIT_empty_copy( dset ) ;
     fac  = (float *) malloc( sizeof(float) * nvals ) ;
     for( ii=0 ; ii < nvals ; ii++ ) fac[ii] = TFAC ;
     EDIT_dset_items( tset ,
                        ADN_prefix    , tprefix ,
                        ADN_brick_fac , fac ,
                        ADN_datum_all , MRI_byte ,
                        ADN_func_type , FUNC_FIM_TYPE ,
                      ADN_none ) ;
     free(fac) ;

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

#if 0
     if( THD_is_file(DSET_HEADNAME(tset)) )
       ERROR_exit("-ssave dataset already exists");
#endif

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

     for( iv=0 ; iv < nvals ; iv++ )
       EDIT_substitute_brick( tset , iv , MRI_byte , NULL ) ;
   }

   /*-- setup to find spikes --*/

   sq2p  = sqrt(0.5*PI) ;

   /* make ref functions */

   nref = 2*corder+3 ;
   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *) malloc( sizeof(float) * nuse ) ;


   /* r(t) = 1 */

   for( iv=0 ; iv < nuse ; iv++ ) ref[0][iv] = 1.0 ;
   jj = 1 ;

   /* r(t) = t - tmid */

   { float tm = 0.5 * (nuse-1.0) ; float fac = 2.0 / nuse ;
     for( iv=0 ; iv < nuse ; iv++ ) ref[1][iv] = (iv-tm)*fac ;
     jj = 2 ;

     /* r(t) = (t-tmid)**jj */

     for( ; jj <= polort ; jj++ )
       for( iv=0 ; iv < nuse ; iv++ )
         ref[jj][iv] = pow( (iv-tm)*fac , (double)jj ) ;
   }

   for( kk=1 ; kk <= corder ; kk++ ){
     fq = (2.0*PI*kk)/nuse ;

     /* r(t) = sin(2*PI*k*t/N) */

     for( iv=0 ; iv < nuse ; iv++ )
       ref[jj][iv] = sin(fq*iv) ;
     jj++ ;

     /* r(t) = cos(2*PI*k*t/N) */

     for( iv=0 ; iv < nuse ; iv++ )
       ref[jj][iv] = cos(fq*iv) ;
     jj++ ;
   }

   /*--- loop over voxels and do work ---*/

   if( verb ){
    if( !localedit ){
      INFO_message("smash edit thresholds: %.1f .. %.1f standard deviations",cut1,cut2) ;
      INFO_message("                      [ %.4f%% .. %.4f%% of normal distribution]",
                     200.0*qg(cut1) , 200.0*qg(cut2) ) ;
    } else {
      INFO_message("local edit threshold:  %.1f standard deviations",cut2) ;
      INFO_message("                      [ %.4f%% of normal distribution]",
                    200.0*qg(cut2) ) ;
    }
    INFO_message("%d slices to process",DSET_NZ(dset)) ;
   }
   kzold  = -1 ;
   nspike =  0 ; nbig = 0 ; nproc = 0 ;
#pragma omp parallel if( nxyz > 11111 )
 { int ii , iv , iu , id , jj ;
   float *far , *dar , *var , *fitar , *ssp , *fit , *zar ;
   short *sar , *qar ; byte *tar ;
   float fsig , fq , cls , snew , val ;

#pragma omp critical (DESPIKE_malloc)
  { far   = (float *) malloc( sizeof(float) * nvals ) ;
    dar   = (float *) malloc( sizeof(float) * nvals ) ;
    var   = (float *) malloc( sizeof(float) * nvals ) ;
    fitar = (float *) malloc( sizeof(float) * nvals ) ;
    ssp   = (float *) malloc( sizeof(float) * nvals ) ;
    fit   = (float *) malloc( sizeof(float) * nref  ) ;
  }

#ifdef USE_OMP
   INFO_message("start OpenMP thread #%d",omp_get_thread_num()) ;
#endif

#pragma omp for
   for( ii=0 ; ii < nxyz ; ii++ ){   /* ii = voxel index */

      if( mask != NULL && mask[ii] == 0 ) continue ;   /* skip this voxel */

#ifndef USE_OMP
      kz = DSET_index_to_kz(dset,ii) ;       /* starting a new slice */
      if( kz != kzold ){
        if( verb ){
          fprintf(stderr, "++ start slice %2d",kz ) ;
          if( nproc > 0 ){
            pspike = (100.0*nspike)/nproc ;
            pbig   = (100.0*nbig  )/nproc ;
            fprintf(stderr,
                    "; so far %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]",
                    nproc,nspike,pspike,nbig,pbig ) ;
          }
          fprintf(stderr,"\n") ;
        }
        kzold = kz ;
      }
#endif

      /*** extract ii-th time series into far[] ***/

      switch( datum ){
        case MRI_short:
          for( iv=0 ; iv < nuse ; iv++ ){
            qar = DSET_ARRAY(dset,iv+ignore) ;   /* skip ignored data */
            far[iv] = (float)qar[ii] ;
          }
        break ;
        case MRI_float:
          for( iv=0 ; iv < nuse ; iv++ ){
            zar = DSET_ARRAY(dset,iv+ignore) ;
            far[iv] = zar[ii] ;
          }
        break ;
      }
      memcpy(dar,far,sizeof(float)*nuse) ;   /* copy time series into dar[] */

      /*** solve for L1 fit ***/

      cls = cl1_solve( nuse , nref , far , ref , fit,0 ) ; /* the slow part */

      if( cls < 0.0f ){                      /* fit failed! */
#if 0
        fprintf(stderr,"curve fit fails at voxel %d %d %d\n",
                DSET_index_to_ix(dset,ii) ,
                DSET_index_to_jy(dset,ii) ,
                DSET_index_to_kz(dset,ii)  ) ;
#endif
        continue ;                           /* skip this voxel */
      }

      for( iv=0 ; iv < nuse ; iv++ ){        /* detrend */
        val =  fit[0]
             + fit[1]*ref[1][iv]             /* quadratic part of curve fit */
             + fit[2]*ref[2][iv] ;
        for( jj=3 ; jj < nref ; jj++ )       /* rest of curve fit */
          val += fit[jj] * ref[jj][iv] ;

        fitar[iv] = val ;                    /* save curve fit value */
        var[iv]   = dar[iv]-val ;            /* remove fitted value = resid */
        far[iv]   = fabsf(var[iv]) ;         /* abs value of resid */
      }

      /*** compute estimate standard deviation of detrended data ***/

      fsig = sq2p * qmed_float(nuse,far) ;   /* also mangles far array */

      /*** process time series for spikes, editing data in dar[] ***/

      if( fsig > 0.0f ){                     /* data wasn't fit perfectly */

        /* find spikiness for each point in time */

        fq = 1.0f / fsig ;
        for( iv=0 ; iv < nuse ; iv++ ){
          ssp[iv] = fq * var[iv] ;           /* spikiness s = how many sigma out */
        }

        /* save spikiness in -ssave datset */

        if( tset != NULL ){
          for( iv=0 ; iv < nuse ; iv++ ){
            tar     = DSET_ARRAY(tset,iv+ignore) ;
            snew    = ITFAC*fabsf(ssp[iv]) ;  /* scale for byte storage */
            tar[ii] = BYTEIZE(snew) ;         /* cf. mrilib.h */
          }
        }

        /* process values of |s| > cut1, editing dar[] */

        for( iv=0 ; iv < nuse ; iv++ ){ /* loop over time points */
          if( !localedit ){             /** classic 'smash' edit **/
            if( ssp[iv] > cut1 ){
              snew = cut1 + c21*mytanh((ssp[iv]-cut1)*ic21) ;   /* edit s down */
              dar[iv] = fitar[iv] + snew*fsig ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; if( ssp[iv] > cut2 ) nbig++ ; }
            } else if( ssp[iv] < -cut1 ){
              snew = -cut1 + c21*mytanh((ssp[iv]+cut1)*ic21) ;  /* edit s up */
              dar[iv] = fitar[iv] + snew*fsig ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; if( ssp[iv] < -cut2 ) nbig++ ; }
            }
          } else {                      /** local edit: 04 Apr 2007 **/
            if( ssp[iv] >= cut2 || ssp[iv] <= -cut2 ){
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
              dar[iv] = val ;
#pragma omp critical (DESPIKE_counter)
              { nspike++ ; nbig++ ; }
            }
          }
        } /* end of loop over time points */
#pragma omp atomic
        nproc += nuse ;  /* number data points processed */

      } /* end of processing time series when fsig is positive */

      /* put dar[] time series (possibly edited above) into output bricks */

      switch( datum ){
        case MRI_short:
          for( iv=0 ; iv < nuse ; iv++ ){
            sar = DSET_ARRAY(oset,iv+ignore) ; /* output brick */
            sar[ii] = (short)dar[iv] ;         /* original or mutated data */
          }
        break ;
        case MRI_float:
          for( iv=0 ; iv < nuse ; iv++ ){
            zar = DSET_ARRAY(oset,iv+ignore) ; /* output brick */
            zar[ii] = dar[iv] ;                /* original or mutated data */
          }
        break ;
      }

   } /* end of loop over voxels #ii */

#pragma omp critical (DESPIKE_malloc)
   { free(fit); free(ssp); free(fitar); free(var); free(dar); free(far); }

 } /* end OpenMP */

   /*--- finish up ---*/

   DSET_delete(dset) ; /* delete input dataset */

   if( verb ){
     if( nproc > 0 ){
       pspike = (100.0*nspike)/nproc ;
       pbig   = (100.0*nbig  )/nproc ;
       INFO_message("FINAL: %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]",
               nproc,nspike,pspike,nbig,pbig ) ;
     } else {
       INFO_message("FINAL: no good voxels found to process!!??") ;
     }
   }

   /* write results */

   DSET_write(oset) ;
   if( verb ) WROTE_DSET(oset) ;
   DSET_delete(oset) ;

   if( tset != NULL ){
     DSET_write(tset) ;
     if( verb ) WROTE_DSET(tset) ;
     DSET_delete(tset) ;
   }

   exit(0) ;
}
