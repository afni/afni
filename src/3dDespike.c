#include "mrilib.h"

/************************************************************************/
/******* Hack to remove large spikes from time series data (oog). *******/
/******* 30 Aug 2002 - RWCox                                      *******/
/************************************************************************/

#define TFAC  0.1
#define ITFAC 10.0

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *oset=NULL , *tset=NULL ;
   int nvals , iv , nxyz , ii,jj,kk , iarg , ic,cc ;
   float thresh=5.0 , sq2p , fq , val , fsig , tval ;
   MRI_IMAGE *flim ;
   float *far, *dar , *var , *fitar ;
   char *prefix="despike" , *tprefix=NULL ;

   int corder=-1 , nref , ignore=0 , polort=2 , nuse , verb , nomask=0 ;
   float **ref ;
   float  *fit ;
   short  *sar , *qar ;
   byte   *tar , tcut , *tbar , *mask=NULL ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dDespike [options] dataset\n"
             "Removes 'spikes' from the 3D+time input dataset and writes\n"
             "a new dataset with the spike values replaced by something\n"
             "more pleasing.\n"
             "\n"
             "Method:\n"
             " * L1 fit a smooth-ish curve to each voxel time series\n"
             " * Compute the MAD of the difference between the curve and\n"
             "    the data time series (the residuals)\n"
             " * Estimate the standard deviation 'sigma' of the residuals\n"
             "    as sqrt(PI/2)*MAD\n"
             " * A voxel value is called a spike if its residual is more\n"
             "    than t*sigma from the fitted curve (t defaults to 5)\n"
             " * Spike values are replaced by an average of nearby non-\n"
             "    spike values from the same time series\n"
             "\n"
             "Options:\n"
             " -ignore N  = Ignore the 1st N points in the time series;\n"
             "               these values will just be copied to the\n"
             "               output dataset [default N=0].\n"
             " -corder L  = Set the curve fit order to L; the curve\n"
             "               that is fit to data v(t) is\n"
             "\n"
             "                      k=L [        (2*PI*k*t)          (2*PI*k*t) ]\n"
             "  a + b*t + c*t*t + SUM   [ d * sin(--------) + e * cos(--------) ]\n"
             "                      k=1 [  k     (    T   )    k     (    T   ) ]\n"
             "\n"
             "               where T = duration of time series.  The fit is\n"
             "               done using L1 regression, which is insensitive\n"
             "               to large spikes in the data.  The default value\n"
             "               of L is NT/30, where NT = number of time points.\n"
             "\n"
             " -thresh t  = Threshold for declaring a spike [default t=5].\n"
             " -tsave ttt = Save 'spikiness' measure (value that is thresholded)\n"
             "               for each voxel into a 3D+time dataset with prefix\n"
             "               'ttt' [default=don't save]\n"
             " -prefix pp = Save de-spiked dataset with prefix 'pp'\n"
             "               [default pp='despike']\n"
             " -nomask    = Process all voxels\n"
             "               [default=use a mask of high-intensity voxels].\n"
             "\n"
             "Caveat:\n"
             " Despiking may interfere with registration.  This possibility has\n"
             " not been explored.  Check your data visually!\n"
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

      if( strcmp(argv[iarg],"-tsave") == 0 ){
        tprefix = argv[++iarg] ;
        if( !THD_filename_ok(tprefix) ){
           fprintf(stderr,"** ERROR: -tsave prefix is not good!\n"); exit(1);
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

      /** threshold for ratio **/

      if( strcmp(argv[iarg],"-thresh") == 0 ){
        thresh = strtod( argv[++iarg] , NULL ) ;
        if( thresh <= 3.0 ){
          fprintf(stderr,"** Illegal value of -thresh!\n"); exit(1);
        }
        iarg++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*----- read input dataset -----*/

   if( iarg >= argc ){
     fprintf(stderr,"** No input dataset!?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[iarg] ) ;
   if( !ISVALID_DSET(dset) ){
     fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]); exit(1);
   }
   nvals = DSET_NUM_TIMES(dset) ; nuse = nvals - ignore ;
   if( nuse < 15 ){
     fprintf(stderr,"** Can't use dataset with < 15 time points per voxel!\n") ;
     exit(1) ;
   }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short || !DSET_datum_constant(dset) ){
     fprintf(stderr,"** Can't process non-short dataset!\n") ; exit(1) ;
   }
   if( corder > 0 && 4*corder+2 > nuse ){
     fprintf(stderr,"** -corder %d is too big for NT=%d!\n",corder,nvals) ;
     exit(1) ;
   } else if( corder < 0 ){
     corder = rint(nuse/30.0) ;
     fprintf(stderr,"++ using %d time points => -corder %d\n",nuse,corder) ;
   }
   nxyz = DSET_NVOX(dset) ;
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
     fprintf(stderr,"++ %d voxels in the mask [out of %d]\n",ii,DSET_NVOX(dset)) ;
   }

   /*-- create empty despiked dataset --*/

fprintf(stderr,"++ creating oset\n") ;

   oset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , MRI_short ,
                    ADN_none ) ;

   if( THD_is_file(DSET_HEADNAME(oset)) ){
     fprintf(stderr,"** ERROR: output dataset already exists: %s\n",DSET_HEADNAME(oset));
     exit(1);
   }

   tross_Copy_History( oset , dset ) ;
   tross_Make_History( "3dDespike" , argc , argv , oset ) ;

   /* create bricks */

fprintf(stderr,"++ creating oset bricks\n") ;

   for( iv=0 ; iv < nvals ; iv++ )
     EDIT_substitute_brick( oset , iv , MRI_short , NULL ) ;

   /* copy the ignored bricks */

if( ignore ) fprintf(stderr,"++ copying ignored bricks\n") ;

   for( iv=0 ; iv < ignore ; iv++ ){
     sar = DSET_ARRAY(oset,iv) ;
     qar = DSET_ARRAY(dset,iv) ;
     memcpy( sar , qar , DSET_BRICK_BYTES(dset,iv) ) ;
     DSET_unload_one(dset,iv) ;
   }

   /*-- setup to save a threshold statistic dataset, if desired --*/

   if( tprefix != NULL ){
     float *fac ;
fprintf(stderr,"++ creating -tsave dataset\n") ;
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

     if( THD_is_file(DSET_HEADNAME(tset)) ){
       fprintf(stderr,"** ERROR: -tsave dataset already exists!\n"); exit(1);
     }

     tross_Copy_History( tset , dset ) ;
     tross_Make_History( "3dDespike" , argc , argv , tset ) ;

fprintf(stderr,"++ creating -tsave dataset bricks \n") ;
     for( iv=0 ; iv < nvals ; iv++ )
       EDIT_substitute_brick( tset , iv , MRI_byte , NULL ) ;
   }

   /*-- setup to find spikes --*/

   sq2p  = sqrt(0.5*PI) ;
   var   = (float *) malloc( sizeof(float) * nvals ) ;
   far   = (float *) malloc( sizeof(float) * nvals ) ;
   dar   = (float *) malloc( sizeof(float) * nvals ) ;
   fitar = (float *) malloc( sizeof(float) * nvals ) ;
   tbar  = (byte *)  malloc( sizeof(byte)  * nvals ) ;

   tval  = thresh * ITFAC ; tcut = BYTEIZE(tval) ;

   /* make ref functions */

fprintf(stderr,"++ making ref functions\n") ;

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

fprintf(stderr,"++ starting main loop\n") ;

   for( ii=0 ; ii < nxyz ; ii++ ){   /* ii = voxel index */

      if( mask != NULL && mask[ii] == 0 ) continue ;   /* skip this voxel */

verb = (ii % 2000 == 999) ;
if( verb )
 fprintf(stderr,"starting voxel %d %d %d\n",
         DSET_index_to_ix(dset,ii) ,
         DSET_index_to_jy(dset,ii) ,
         DSET_index_to_kz(dset,ii)  ) ;

      /* extract ii-th time series */

      for( iv=0 ; iv < nuse ; iv++ ){
        sar = DSET_ARRAY(dset,iv+ignore) ;         /* skip ignored data */
        far[iv] = (float) sar[ii] ;
      }
      memcpy(dar,far,sizeof(float)*nuse) ;         /* copy data */

      jj = cl1_solve( nuse , nref , far , ref , fit,0 );  /* get fit */

      if( jj ){                                    /* bad! should not happen */
        fprintf(stderr,"regression fails at voxel %d %d %d\n",
                DSET_index_to_ix(dset,ii) ,
                DSET_index_to_jy(dset,ii) ,
                DSET_index_to_kz(dset,ii)  ) ;
        continue ;
      }

      for( iv=0 ; iv < nuse ; iv++ ){                     /* detrend */
        val = 0.0 ;
        for( jj=0 ; jj < nref ; jj++ )                    /* fitted value */
          val += fit[jj] * ref[jj][iv] ;

        fitar[iv] = val ;                    /* save fitted value */
        var[iv]   = dar[iv]-val ;            /* remove fitted value = resid */
        far[iv]   = fabs(var[iv]) ;          /* abs value of resid */
      }

      /* find standard deviation of detrended data */

      fsig = sq2p * qmed_float( nuse , far ) ;  /* also mangles far array */

if(verb) fprintf(stderr,"fsig = %f\n",fsig) ;

      /* process time series for spikes */

      if( fsig > 0.0 ){

        /* find spikiness for each point in time */

        fq = ITFAC / fsig ;
        for( iv=0 ; iv < nuse ; iv++ ){
          tval = fq * fabs(var[iv]) ;
          tbar[iv] = BYTEIZE(tval) ;

if(verb)
 fprintf(stderr,"%3d: %7.1f %7.1f %7.1f %7.1f\n",
         iv,dar[iv],fitar[iv],var[iv],tval ) ;

        }
        if( tset != NULL ){               /* save spikiness in -tsave datset */
          for( iv=0 ; iv < nuse ; iv++ ){
            tar = DSET_ARRAY(tset,iv+ignore) ;
            tar[ii] = tbar[iv] ;
          }
        }
      }

   }

   if( tset != NULL ) DSET_write(tset) ;

   exit(0) ;
}
