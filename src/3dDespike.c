#include "mrilib.h"

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
       if( x >  7.0 ) return  1.0 ;  /* 03 Sep: check for stupid inputs */
  else if( x < -7.0 ) return -1.0 ;
  ex = exp(x) ; exi = 1.0/ex ;
  return (ex-exi)/(ex+exi) ;
}

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *oset=NULL , *tset=NULL ;
   int nvals , iv , nxyz , ii,jj,kk , iarg , kz,kzold ;
   float cut1=2.5,cut2=4.0 , sq2p , fq , val , fsig ;
   MRI_IMAGE *flim ;
   float *far, *dar , *var , *fitar ;
   char *prefix="despike" , *tprefix=NULL ;

   int corder=-1 , nref , ignore=0 , polort=2 , nuse , nomask=0 ;
   int nspike, nbig, nproc ;
   float **ref ;
   float  *fit , *ssp , snew , c21,ic21 , pspike,pbig , cls ;
   short  *sar , *qar ;
   byte   *tar , *mask=NULL ;
   float  *zar , *yar ;
   int     datum ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dDespike [options] dataset\n"
             "Removes 'spikes' from the 3D+time input dataset and writes\n"
             "a new dataset with the spike values replaced by something\n"
             "more pleasing.\n"
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
             "\n"
             "Caveats:\n"
             "* Despiking may interfere with image registration, since head\n"
             "   movement may produce 'spikes' at the edge of the brain, and\n"
             "   this information would be used in the registration process.\n"
             "   This possibility has not been explored.\n"
             "* Check your data visually before and after despiking and\n"
             "   registration!\n"
             "   [Hint: open 2 AFNI controllers, and turn Time Lock on.]\n"
            ) ;
      exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dDespike main"); machdep(); AFNI_logger("3dDespike",argc,argv);

   /** parse options **/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /** don't use masking **/

      if( strcmp(argv[iarg],"-nomask") == 0 ){
        nomask = 1 ;
        iarg++ ; continue ;
      }

      /** output dataset prefix **/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) ){
          fprintf(stderr,"** ERROR: -prefix is not good!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      /** ratio dataset prefix **/

      if( strcmp(argv[iarg],"-ssave") == 0 ){
        tprefix = argv[++iarg] ;
        if( !THD_filename_ok(tprefix) ){
          fprintf(stderr,"** ERROR: -ssave prefix is not good!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      /** trigonometric polynomial order **/

      if( strcmp(argv[iarg],"-corder") == 0 ){
        corder = strtol( argv[++iarg] , NULL , 10 ) ;
        if( corder < 0 ){
          fprintf(stderr,"** Illegal value of -corder!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      /** how much to ignore at start **/

      if( strcmp(argv[iarg],"-ignore") == 0 ){
        ignore = strtol( argv[++iarg] , NULL , 10 ) ;
        if( ignore < 0 ){
          fprintf(stderr,"** Illegal value of -ignore!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      /** thresholds for s ratio **/

      if( strcmp(argv[iarg],"-cut") == 0 ){
        cut1 = strtod( argv[++iarg] , NULL ) ;
        cut2 = strtod( argv[++iarg] , NULL ) ;
        if( cut1 < 1.0 || cut2 < cut1+0.5 ){
          fprintf(stderr,"** Illegal values after -cut!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   c21 = cut2-cut1 ; ic21 = 1.0/c21 ;

   /*----- read input dataset -----*/

   if( iarg >= argc ){
     fprintf(stderr,"** No input dataset!?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[iarg] ) ;
   if( !ISVALID_DSET(dset) ){
     fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]); exit(1);
   }
   datum = DSET_BRICK_TYPE(dset,0) ;
   if( (datum != MRI_short && datum != MRI_float) || !DSET_datum_constant(dset) ){
     fprintf(stderr,"** Can't process non-short, non-float dataset!\n") ; exit(1) ;
   }
   fprintf(stderr,"Input data type = %s\n",MRI_TYPE_name[datum]) ;
   nvals = DSET_NUM_TIMES(dset) ; nuse = nvals - ignore ;
   if( nuse < 15 ){
     fprintf(stderr,"** Can't use dataset with < 15 time points per voxel!\n") ;
     exit(1) ;
   }
   fprintf(stderr,"++ ignoring first %d time points, using last %d\n",ignore,nuse);
   if( corder > 0 && 4*corder+2 > nuse ){
     fprintf(stderr,"** -corder %d is too big for NT=%d!\n",corder,nvals) ;
     exit(1) ;
   } else if( corder < 0 ){
     corder = rint(nuse/30.0) ;
     fprintf(stderr,"++ using %d time points => -corder %d\n",nuse,corder) ;
   } else {
     fprintf(stderr,"++ -corder %d set from command line\n",corder) ;
   }
   nxyz = DSET_NVOX(dset) ;
   fprintf(stderr,"++ Loading dataset %s\n",argv[iarg]) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
     fprintf(stderr,"** Can't read dataset from disk!\n") ; exit(1) ;
   }

   /*-- create automask --*/

   if( !nomask ){
     mask = THD_automask( dset ) ;
     for( ii=0 ; ii < 4 ; ii++ )
       THD_mask_dilate( DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset), mask, 3 ) ;
     ii = THD_countmask( DSET_NVOX(dset) , mask ) ;
     fprintf(stderr,"++ %d voxels in the mask [out of %d in dataset]\n",ii,DSET_NVOX(dset)) ;
   } else {
     fprintf(stderr,"++ processing all %d voxels in dataset\n",DSET_NVOX(dset)) ;
   }

   /*-- create empty despiked dataset --*/

   oset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , datum ,
                    ADN_none ) ;

   if( THD_is_file(DSET_HEADNAME(oset)) ){
     fprintf(stderr,"** ERROR: output dataset already exists: %s\n",DSET_HEADNAME(oset));
     exit(1);
   }

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

     if( THD_is_file(DSET_HEADNAME(tset)) ){
       fprintf(stderr,"** ERROR: -ssave dataset already exists!\n"); exit(1);
     }

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

     for( iv=0 ; iv < nvals ; iv++ )
       EDIT_substitute_brick( tset , iv , MRI_byte , NULL ) ;
   }

   /*-- setup to find spikes --*/

   sq2p  = sqrt(0.5*PI) ;
   var   = (float *) malloc( sizeof(float) * nvals ) ;
   far   = (float *) malloc( sizeof(float) * nvals ) ;
   dar   = (float *) malloc( sizeof(float) * nvals ) ;
   fitar = (float *) malloc( sizeof(float) * nvals ) ;
   ssp   = (float *) malloc( sizeof(float) * nvals ) ;

   /* make ref functions */

   nref = 2*corder+3 ;
   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *) malloc( sizeof(float) * nuse ) ;

   fit = (float *) malloc( sizeof(float) * nref ) ;

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

   fprintf(stderr,"++ edit thresholds: %.1f .. %.1f standard deviations\n",cut1,cut2) ;
   fprintf(stderr,"++                [ %.4f%% .. %.4f%% of normal distribution]\n",
                  200.0*qg(cut1) , 200.0*qg(cut2) ) ;
   fprintf(stderr,"++ %d slices to process\n",DSET_NZ(dset)) ;
   kzold  = -1 ;
   nspike =  0 ; nbig = 0 ; nproc = 0 ;
   for( ii=0 ; ii < nxyz ; ii++ ){   /* ii = voxel index */

      if( mask != NULL && mask[ii] == 0 ) continue ;   /* skip this voxel */

      kz = DSET_index_to_kz(dset,ii) ;       /* starting a new slice */
      if( kz != kzold ){
        fprintf(stderr, "++ start slice %2d",kz ) ;
        if( nproc > 0 ){
          pspike = (100.0*nspike)/nproc ;
          pbig   = (100.0*nbig  )/nproc ;
          fprintf(stderr,"; done %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]",
                  nproc,nspike,pspike,nbig,pbig ) ;
        }
        fprintf(stderr,"\n") ;
        kzold = kz ;
      }

      /*** extract ii-th time series into far[] ***/

      switch( datum ){
        case MRI_short:
          for( iv=0 ; iv < nuse ; iv++ ){
            qar = DSET_ARRAY(dset,iv+ignore) ;   /* skip ignored data */
            far[iv] = (float) qar[ii] ;
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

      cls = cl1_solve( nuse , nref , far , ref , fit,0 );

      if( cls < 0.0 ){                       /* fit failed! */
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

        fitar[iv] = val ;                    /* save curve value */
        var[iv]   = dar[iv]-val ;            /* remove fitted value = resid */
        far[iv]   = fabs(var[iv]) ;          /* abs value of resid */
      }

      /*** compute estimate standard deviation of detrended data ***/

      fsig = sq2p * qmed_float(nuse,far) ;   /* also mangles far array */

      /*** process time series for spikes, editing data in dar[] ***/

      if( fsig > 0.0 ){                      /* data wasn't fit perfectly */

        /* find spikiness for each point in time */

        fq = 1.0 / fsig ;
        for( iv=0 ; iv < nuse ; iv++ ){
          ssp[iv] = fq * var[iv] ;           /* spikiness s = how many sigma out */
        }

        /* save spikiness in -ssave datset */

        if( tset != NULL ){
          for( iv=0 ; iv < nuse ; iv++ ){
            tar   = DSET_ARRAY(tset,iv+ignore) ;
            snew   = ITFAC*fabs(ssp[iv]) ;   /* scale for byte storage */
            tar[ii] = BYTEIZE(snew) ;        /* cf. mrilib.h */
          }
        }

        /* process values of |s| > cut1, editing dar[] */

        for( iv=0 ; iv < nuse ; iv++ ){
          if( ssp[iv] > cut1 ){
            snew = cut1 + c21*mytanh((ssp[iv]-cut1)*ic21) ;   /* edit s down */
            dar[iv] = fitar[iv] + snew*fsig ;
            nspike++ ; if( ssp[iv] > cut2 ) nbig++ ;          /* count edits */
          } else if( ssp[iv] < -cut1 ){
            snew = -cut1 + c21*mytanh((ssp[iv]+cut1)*ic21) ;  /* edit s up */
            dar[iv] = fitar[iv] + snew*fsig ;
            nspike++ ; if( ssp[iv] < -cut2 ) nbig++ ;
          }
        }
        nproc += nuse ;  /* number data points processed */

      } /* end of processing time series when fsig is positive */

      /* put dar[] time series (possibly edited above) into output bricks */

      switch( datum ){
        case MRI_short:
          for( iv=0 ; iv < nuse ; iv++ ){
            sar = DSET_ARRAY(oset,iv+ignore) ; /* output brick */
            sar[ii] = (short) dar[iv] ;        /* original or mutated data */
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

   /*--- finish up ---*/

   DSET_delete(dset) ; /* delete input dataset */

   if( nproc > 0 ){
     pspike = (100.0*nspike)/nproc ;
     pbig   = (100.0*nbig  )/nproc ;
     fprintf(stderr,"++ FINAL: %d data points, %d edits [%.3f%%], %d big edits [%.3f%%]\n",
             nproc,nspike,pspike,nbig,pbig ) ;
   } else {
     fprintf(stderr,"++ FINAL: no good voxels found to process!!!!\n") ;
   }

   /* write results */

   fprintf(stderr,"++ Writing output dataset %s\n",DSET_HEADNAME(oset)) ;
   DSET_write(oset) ; DSET_delete(oset) ;

   if( tset != NULL ){
     fprintf(stderr,"++ Writing -ssave dataset %s\n",DSET_HEADNAME(tset)) ;
     DSET_write(tset) ;
   }

   exit(0) ;
}
