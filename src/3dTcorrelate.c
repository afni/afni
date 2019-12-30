#include "mrilib.h"

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define KTAUB    4
#define COVAR    5
#define YCOEF    6
#define PARTIAL  7

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

/*----------------------------------------------------------------------------*/

void usage_3dTcorrelate(int detail)
{
         printf(
"\n"
"Usage: 3dTcorrelate [options] xset yset   ~1~\n"
"\n"
"Computes the correlation coefficient between corresponding voxel\n"
"time series in two input 3D+time datasets 'xset' and 'yset', and\n"
"stores the output in a new 1 sub-brick dataset.\n"
"\n"
"--------\n"
"Options:   ~1~\n"
"--------\n"
"  -pearson    = Correlation is the normal Pearson (product moment)\n"
"                correlation coefficient [this is the default method].\n"
"  -spearman   = Correlation is the Spearman (rank) correlation\n"
"                coefficient.\n"
"  -quadrant   = Correlation is the quadrant correlation coefficient.\n"
"  -ktaub      = Correlation is Kendall's tau_b coefficient.\n"
"                ++ For 'continuous' or finely-discretized data, tau_b\n"
"                   and rank correlation are nearly equivalent.\n"
"  -covariance = Covariance instead of correlation. That would be \n"
"                the Pearson correlation without scaling by the product\n"
"                of the standard deviations.\n"
"  -partial z   = Partial Pearson's Correlation of X & Y, adjusting for Z \n"
"                Supply dataset z to be taken into account. *EXPERIMENTAL* \n"
"  -ycoef      = Least squares coefficient that best fits y(t) to x(t),\n"
"                after detrending.  That is, if yd(t) is the detrended\n"
"                y(t) and xd(t) is the detrended x(t), then the ycoef\n"
"                value is from the OLSQ fit to xd(t) = ycoef * y(t) + error.\n"
"\n"
"  -Fisher     = Apply the 'Fisher' (inverse hyperbolic tangent) transformation\n"
"                to (correlation) results.\n"
"                ++ It does not make sense to use this with '-ktaub', but if\n"
"                    you want to do it, the program will not stop you.\n"
"                ++ This option does not apply to '-covariance' or '-ycoef'.\n"
"\n"
"  -polort m = Remove polynomical trend of order 'm', for m=-1..9.\n"
"                [default is m=1; removal is by least squares].\n"
"                Using m=-1 means no detrending; this is only useful\n"
"                for data/information that has been pre-processed.\n"
"\n"
"  -ort r.1D = Also detrend using the columns of the 1D file 'r.1D'.\n"
"                Only one -ort option can be given.  If you want to use\n"
"                more than one, create a temporary file using 1dcat.\n"
"\n"
"  -autoclip = Clip off low-intensity regions in the two datasets,\n"
"  -automask =  so that the correlation is only computed between\n"
"               high-intensity (presumably brain) voxels.  The\n"
"               intensity level is determined the same way that\n"
"               3dClipLevel works.\n"
"           ** At present, this program does not have a '-mask'\n"
"              option. Maybe someday?\n"
"\n"
"  -zcensor  = Omit (censor out) any time points where the xset\n"
"               volume is all zero OR where the yset volume is all\n"
"               zero (in mask). Please note that using -zcensor\n"
"               with any detrending is unlikely to be useful.\n"
"           ** That is, you should use '-polort -1' with this\n"
"               option, and NOT use '-ort'.\n"
"            *  In fact, using '-zcensor' will set polort = -1,\n"
"               and if you insist on using detrending, you will\n"
"               have to put the '-polort' option AFTER '-zcensor.\n"
"           ** Since correlation is calculated from the sum\n"
"               of the point-by-point products xset(t)*yset(t),\n"
"               why censor out points where xset or yset is 0?\n"
"               Because the denominator of correlation is from\n"
"               the sum of xset(t)*xset(t) and yset(t)*yset(t)\n"
"               and unless the t-points where the datasets are\n"
"               censored are BOTH zero at the same time, the\n"
"               denominator will be incorrect.\n"
"           ** [RWCox - Dec 2019, day of Our Lady of Guadalupe]\n"
"              [for P Molfese and E Finn]\n"
"\n"
"  -prefix p = Save output into dataset with prefix 'p'\n"
"               [default prefix is 'Tcorr'].\n"
"\n"
"------\n"
"Notes:   ~1~\n"
"------\n"
"* The output dataset is functional bucket type, with just one\n"
"   sub-brick, stored in floating point format.\n"
"\n"
"* Because both time series are detrended prior to correlation,\n"
"   the results will not be identical to using FIM or FIM+ to\n"
"   calculate correlations (whose ideal vector is not detrended).\n"
"\n"
"* Also see 3dTcorr1D if you want to correlate each voxel time series\n"
"   in a dataset xset with a single 1D time series file, instead of\n"
"   separately with time series from another 3D+time dataset.\n"
"\n"
"* https://en.wikipedia.org/wiki/Correlation\n"
"* https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient\n"
"* https://en.wikipedia.org/wiki/Spearman%%27s_rank_correlation_coefficient\n"
"* https://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient\n"
"* https://en.wikipedia.org/wiki/Partial_correlation\n"
"\n"
"-- RWCox - Aug 2001++\n"
            ) ;
      PRINT_COMPILE_DATE ;
   return;
}

/*----------------------------------------------------------------------------*/

float THD_ycoef( int npt , float *xx , float *yy )  /* 20 Feb 2014 */
{
   float coef[1] = {0.0f} ;
   if( THD_is_constant(npt,xx) || THD_is_constant(npt,yy) ) return 0.0f ;
   THD_generic_detrend_LSQ(npt,xx,-1,1,&yy,coef) ;
   return coef[0] ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *yset , *cset, *pset=NULL ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii , polort=1 ;
   MRI_IMAGE *xsim , *ysim , *psim;
   float     *xsar , *ysar , *car , *psar=NULL;
   char *prefix = "Tcorr" ;
   char *psetFile = NULL ;
   byte *mmm=NULL ;
   MRI_IMAGE *im_ort=NULL ;            /* 13 Mar 2003 */
   int nort=0 ; float **fort=NULL ;
   int do_atanh = 0 ;                  /* 12 Jan 2018 */

   int do_zcens=0 , nzcens=0 , ngood , *zcens=NULL ;  /* 12 Dec 2019 */
   float *xcens=NULL , *ycens=NULL , *pcens=NULL ;

   /*----*/

   if (argc == 1) { usage_3dTcorrelate(1); exit(0); } /* Bob's help shortcut */

   mainENTRY("3dTCorrelate main"); machdep(); AFNI_logger("3dTcorrelate",argc,argv);
   PRINT_VERSION("3dTcorrelate") ; THD_check_AFNI_version("3dTcorrelate") ;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if (strcmp(argv[nopt], "-h") == 0 || strcmp(argv[nopt], "-help") == 0) {
        usage_3dTcorrelate(strlen(argv[nopt]) > 3 ? 2:1);
        exit(0);
      }

      if( strcmp(argv[nopt],"-zcensor") == 0 ){       /* 12 Dec 2019 */
        do_zcens = 1 ;
        if( polort >= 0 ){
          INFO_message("-zcensor sets polort = -1") ; polort = -1 ;
        }
        nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-ort") == 0 ){           /* 13 Mar 2003 */
        if( im_ort != NULL ){
          ERROR_exit("Can't have multiple -ort options!") ;
        }
        im_ort = mri_read_1D( argv[++nopt] ) ;
        if( im_ort == NULL ){
          ERROR_exit("Can't read 1D file %s",argv[nopt]) ;
        }
        nort = im_ort->ny ;
        fort = (float **) malloc( sizeof(float *)*nort ) ;
        for( ii=0 ; ii < nort ; ii++ )
          fort[ii] = MRI_FLOAT_PTR(im_ort) + (ii*im_ort->nx) ;

        nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         do_autoclip = 1 ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-pearson") == 0 ){
         method = PEARSON ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-partial") == 0 )
      {
         method = PARTIAL ; nopt++ ;
         //name the covariate dataset
         psetFile = argv[nopt];
         INFO_message("Performing Partial Correlation with: %s\n", psetFile);
         pset = THD_open_dataset( argv[nopt++] ) ;
         if( pset == NULL )
         {
           ERROR_exit("-partial Can't open dataset %s",argv[nopt]) ;
         }
         continue ;
      }

      if( strcasecmp(argv[nopt],"-covariance") == 0 ){
         method = COVAR ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-ycoef") == 0 ){  /* 20 Feb 2014 */
         method = YCOEF ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-spearman") == 0 ){
         method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-quadrant") == 0 ){
         method = QUADRANT ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-ktaub") == 0 ){
         method = KTAUB ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-fisher") == 0 ){ /* 12 Jan 2018 */
         do_atanh = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = argv[++nopt] ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("Illegal value after -prefix") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         char *cpt ;
         int val = strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 9 ){
           ERROR_exit("Illegal value '%s' after -polort",argv[nopt]) ;
         }
         polort = val ;
         if( polort >=0 && do_zcens )
           WARNING_message("-polort %d is combined with -zcensor",polort) ;
         nopt++ ; continue ;
      }

      ERROR_message("Illegal option %s\n", argv[nopt]);
      suggest_best_prog_option(argv[0], argv[nopt]);
      exit(1);

   }

   if (argc < 2) {
      ERROR_message("Too few options, use -help for details");
      exit(1);
   }

   if( do_atanh && (method == COVAR || method == YCOEF) ){ /* 12 Jan 2018 */
     WARNING_message("-Fisher doesn't apply to your chosen method") ;
     do_atanh = 0 ;
   }

   /*-- open datasets, check for legality --*/

   if( nopt+1 >= argc ){
      ERROR_exit("Need 2 input datasets on command line") ;
   }

   xset = THD_open_dataset( argv[nopt] ) ;
   if( xset == NULL ){
      ERROR_exit("Can't open input dataset %s",argv[nopt]) ;
   }
   if( DSET_NVALS(xset) < 2 ){
      ERROR_exit("Input dataset %s does not have multiple time points",argv[nopt]) ;
   }
   yset = THD_open_dataset( argv[++nopt] ) ;
   if( yset == NULL ){
      ERROR_exit("Can't open input dataset %s",argv[nopt]) ;
   }
   if( DSET_NVALS(yset) != DSET_NVALS(xset) ){
      ERROR_exit("Input dataset %s is different length than %s\n",argv[nopt],argv[nopt-1]) ;
   }
   if( DSET_NVOX(yset) != DSET_NVOX(xset) ){
      ERROR_exit("Input dataset %s is different size than %s\n",
                 argv[nopt],argv[nopt-1]) ;
   }
   if( im_ort != NULL && im_ort->nx < DSET_NVALS(xset) ){
      ERROR_exit("Input datsets are longer than -ort file") ;
   }
   if( !EQUIV_GRIDS(xset,yset) )
     WARNING_message("Grid mismatch between input datasets!") ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;
   DSET_load(yset) ; CHECK_LOAD_ERROR(yset) ;

    if ( method == PARTIAL ) {
        //pset
        if( DSET_NVALS(pset) != DSET_NVALS(xset) )
        {
            //assume that if it doesn't match xset, then it doesn't match yset
            ERROR_exit("-parital input dataset %s is different length than %s\n",
                       psetFile,argv[nopt-1]) ;
        }
        if( !EQUIV_GRIDS(xset,pset) )
            WARNING_message("Grid mismatch between input datasets & partial dataset!") ;
        DSET_load(pset) ; CHECK_LOAD_ERROR(pset) ;
    }

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ; ngood = nvals = DSET_NVALS(xset) ;

   if( do_autoclip ){
      byte *xmm , *ymm ;
      xmm = THD_automask( xset ) ;
      ymm = THD_automask( yset ) ;
      mmm = (byte *) malloc(sizeof(byte)*nvox) ;
      for( ii=0 ; ii < nvox ; ii++ )
         mmm[ii] = ( xmm[ii] && ymm[ii] ) ;
      free(xmm) ; free(ymm) ;
      ii = THD_countmask( nvox , mmm ) ;
      INFO_message("%d voxels survive -autoclip",ii) ;
      if( ii == 0 ) exit(1) ;
   }

   /*-- compute censoring, if desired [12 Dec 2019] --*/

   if( do_zcens ){  /* make list of time points to keep */

     zcens = (int *)calloc(sizeof(int),nvals) ;
     for( ii=0 ; ii < nvals ; ii++ ){
       if( mri_nonzero_count_inmask(DSET_BRICK(xset,ii),mmm) > 0 &&    /* keep only */
           mri_nonzero_count_inmask(DSET_BRICK(yset,ii),mmm) > 0   ){    /* if BOTH */
              zcens[ii] = 1; nzcens++;                    /* have some nonzero data */
       }
     }
     if( nzcens == 0 )
       ERROR_exit("-zcensor keeps 0 data volumes to process, out of %d input :(",nvals) ;
     else if( nzcens == 1 )
       ERROR_exit("-zcensor keeps only 1 data volume to process, out of %d input :(",nvals) ;
     else if( nzcens == nvals )
       INFO_message("-zcensor keeps ALL %d data volumes to process",nvals) ;
     else
       INFO_message("-zcensor omits %d data volumes, and keeps %d",nvals-nzcens,nzcens) ;

     ngood = nzcens ;  /* number of good time points */

     if( ngood < nvals ){
       xcens = (float *)malloc(sizeof(float)*nzcens) ;
       ycens = (float *)malloc(sizeof(float)*nzcens) ;
       pcens = (float *)malloc(sizeof(float)*nzcens) ;

       if( polort >= 0 || nort > 0 )
         WARNING_message("Using -zcensor with detrending is dangerous!") ;
     } else {
       do_zcens = 0 ;
     }
   }

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;
   EDIT_dset_items( cset ,
                      ADN_prefix    , prefix         ,
                      ADN_nvals     , 1              ,
                      ADN_ntt       , 0              , /* no time axis */
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) ){
      ERROR_exit("Output dataset %s already exists\n",DSET_HEADNAME(cset)) ;
   }

   EDIT_BRICK_FACTOR(cset,0,0.0) ;                     /* to be safe  */

   switch( method ){                                   /* looks nice  */
      default:
      case PEARSON:  EDIT_BRICK_LABEL(cset,0,"Pear.Corr.") ;
          EDIT_BRICK_TO_FICO(cset,0,ngood,1,polort+1+nort) ;  /* stat params */
                                                               break ;
      case SPEARMAN: EDIT_BRICK_LABEL(cset,0,"Spmn.Corr."  ) ; break ;
      case QUADRANT: EDIT_BRICK_LABEL(cset,0,"Quad.Corr."  ) ; break ;
      case KTAUB:    EDIT_BRICK_LABEL(cset,0,"Taub.Corr."  ) ; break ;
      case COVAR:    EDIT_BRICK_LABEL(cset,0,"Covariance"  ) ; break ;
      case YCOEF:    EDIT_BRICK_LABEL(cset,0,"Ycoef"       ) ; break ;
      case PARTIAL:  EDIT_BRICK_LABEL(cset,0,"Partial.Corr") ; break ;
   }

   EDIT_substitute_brick( cset , 0 , MRI_float , NULL ) ; /* make array  */
   car = DSET_ARRAY(cset,0) ;                             /* get array   */

   /* include prior history  [9 May 2019 rickr] */
   tross_Copy_History( xset , cset );
   tross_Make_History( "3dTcorrelate" , argc,argv , cset ) ;

   /* loop over voxels, correlate */
   /* INFO_message("have %d voxels to work with, %d values/time series\n",nvox,ngood);*/

   for( ii=0 ; ii < nvox ; ii++ ){

      if( mmm != NULL && mmm[ii] == 0 ){  /* the simple case */
         car[ii] = 0.0 ; continue ;
      }

      /* get time series */

      xsim = THD_extract_series(ii,xset,0) ; xsar = MRI_FLOAT_PTR(xsim) ;
      ysim = THD_extract_series(ii,yset,0) ; ysar = MRI_FLOAT_PTR(ysim) ;

      if ( method == PARTIAL ) {
        psim = THD_extract_series(ii,pset,0); psar = MRI_FLOAT_PTR(psim);
      }

      (void)THD_generic_detrend_LSQ( nvals,xsar, polort, nort,fort,NULL ) ;  /* 13 Mar 2003 */
      (void)THD_generic_detrend_LSQ( nvals,ysar, polort, nort,fort,NULL ) ;

      if( do_zcens ){  /* 12 Dec 2019 */
        int jj , kk ;
        for( kk=jj=0 ; jj < nvals ; jj++ ){
          if( zcens[jj] ){
            xcens[kk] = xsar[jj] ;
            ycens[kk] = ysar[jj] ;
            if( psar != NULL ) pcens[kk] = psar[jj] ;
            kk++;
          }
        }
      } else {
        xcens = xsar ; ycens = ysar ; pcens = psar ;
      }

#undef  DAT
#define DAT if(do_atanh)car[ii]=MYatanh(car[ii])

      switch( method ){                    /* correlate */
         default:
         case PEARSON:  car[ii] = THD_pearson_corr ( ngood,xcens,ycens ); DAT; break;
         case SPEARMAN: car[ii] = THD_spearman_corr( ngood,xcens,ycens ); DAT; break;
         case QUADRANT: car[ii] = THD_quadrant_corr( ngood,xcens,ycens ); DAT; break;
         case KTAUB:    car[ii] = THD_ktaub_corr   ( ngood,xcens,ycens ); DAT; break;
         case COVAR:    car[ii] = THD_covariance   ( ngood,xcens,ycens );      break;
         case YCOEF:    car[ii] = THD_ycoef        ( ngood,xcens,ycens );      break;

         case PARTIAL:  car[ii] = THD_pearson_partial_corr( ngood,xcens,ycens,pcens ); DAT; break;
      }

      mri_free(xsim) ; mri_free(ysim) ;    /* toss time series */

   } /* end of loop over voxels */

   /* toss the other trash */

   DSET_unload(xset); DSET_unload(yset); if( mmm != NULL ) free(mmm);
   if( do_zcens ){
     if( xcens != NULL ) free(xcens) ; /* 12 Dec 2019 */
     if( ycens != NULL ) free(ycens) ;
     if( pcens != NULL ) free(pcens) ;
   }

   /* e finito */

   DSET_write(cset) ;
   INFO_message("Wrote dataset: %s",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
