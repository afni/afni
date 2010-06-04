#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset , *oset=NULL ;
   int nvals , iv , nxyz , ii,jj , iarg , saveit=0 , oot , ic,cc ;
   int *count ;
   float qthr=0.001 , alph,fmed,fmad , fbot,ftop,fsig=0 , sq2p,cls ;
   MRI_IMAGE *flim ;
   float *far , *var ;
   byte *mmm=NULL ;
   int    mmvox=0 ;
   char *prefix=NULL ;
   int do_autoclip=0 , npass=0 , do_range=0 ;   /* 12 Aug 2001 */

   int polort=0 , nref , nbad=0 , nfsc=0 ;      /* 07 Aug 2002 */
   int do_frac=0;                               /* 04 Jun 2010 [rickr] */
   float **ref ;
   float  *fit ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dToutcount [options] dataset\n"
             "Calculates number of 'outliers' a 3D+time dataset, at each\n"
             "time point, and writes the results to stdout.\n"
             "\n"
             "Options:\n"
             " -mask mset = Only count voxels in the mask dataset.\n"
             " -qthr q    = Use 'q' instead of 0.001 in the calculation\n"
             "                of alpha (below): 0 < q < 1.\n"
             "\n"
             " -autoclip }= Clip off 'small' voxels (as in 3dClipLevel);\n"
             " -automask }=   you can't use this with -mask!\n"
             "\n"
             " -fraction  = Output the fraction of (masked) voxels which are\n"
             "              outliers at each time point, instead of the count.\n"
             "\n"
             " -range     = Print out median+3.5*MAD of outlier count with\n"
             "                each time point; use with 1dplot as in\n"
             "                3dToutcount -range fred+orig | 1dplot -stdin -one\n"
             " -save ppp  = Make a new dataset, and save the outlier Q in each\n"
             "                voxel, where Q is calculated from voxel value v by\n"
             "                Q = -log10(qg(abs((v-median)/(sqrt(PI/2)*MAD))))\n"
             "             or Q = 0 if v is 'close' to the median (not an outlier).\n"
             "                That is, 10**(-Q) is roughly the p-value of value v\n"
             "                under the hypothesis that the v's are iid normal.\n"
             "              The prefix of the new dataset (float format) is 'ppp'.\n"
             "\n"
             " -polort nn = Detrend each voxel time series with polynomials of\n"
             "                order 'nn' prior to outlier estimation.  Default\n"
             "                value of nn=0, which means just remove the median.\n"
             "                Detrending is done with L1 regression, not L2.\n"
             "\n"
             "OUTLIERS are defined as follows:\n"
             " * The trend and MAD of each time series are calculated.\n"
             "   - MAD = median absolute deviation\n"
             "         = median absolute value of time series minus trend.\n"
             " * In each time series, points that are 'far away' from the\n"
             "    trend are called outliers, where 'far' is defined by\n"
             "      alpha * sqrt(PI/2) * MAD\n"
             "      alpha = qginv(0.001/N) (inverse of reversed Gaussian CDF)\n"
             "      N     = length of time series\n"
             " * Some outliers are to be expected, but if a large fraction of the\n"
             "    voxels in a volume are called outliers, you should investigate\n"
             "    the dataset more fully.\n"
             "\n"
             "Since the results are written to stdout, you probably want to redirect\n"
             "them to a file or another program, as in this example:\n"
             "  3dToutcount -automask v1+orig | 1dplot -stdin\n"
             "\n"
             "NOTE: also see program 3dTqual for a similar quality check.\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dToutcount main"); machdep(); AFNI_logger("3dToutcount",argc,argv);
   PRINT_VERSION("3dToutcount");

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-autoclip") == 0 ||
          strcmp(argv[iarg],"-automask") == 0   ){

         if( mmm != NULL ){
           fprintf(stderr,"** ERROR: can't use -autoclip/mask with -mask!\n");
           exit(1) ;
         }
         do_autoclip = 1 ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg], "-fraction", 5) == 0 ){  /* 6 Jun 2010 */
         do_frac = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-range") == 0 ){  /* 12 Aug 2001 */
         do_range = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-save") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** ERROR: -save prefix is not good!\n"); exit(1);
         }
         saveit = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-qthr") == 0 ){
         qthr = strtod( argv[++iarg] , NULL ) ;
         if( qthr <= 0.0 || qthr >= 0.999 ){
            fprintf(stderr,"** ERROR: -qthr value is illegal!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         int mcount ;
         THD_3dim_dataset *mask_dset ;
         if( do_autoclip ){
           fprintf(stderr,"** ERROR: can't use -mask with -autoclip/mask!\n");
           exit(1) ;
         }
         mask_dset = THD_open_dataset(argv[++iarg]) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"** ERROR: can't open -mask dataset!\n"); exit(1);
         }
         if( mmm != NULL ){
            fprintf(stderr,"** ERROR: can't have 2 -mask options!\n"); exit(1);
         }
         mmm = THD_makemask( mask_dset , 0 , 1.0,-1.0 ) ;
         mmvox = DSET_NVOX( mask_dset ) ;
         mcount = THD_countmask( mmvox , mmm ) ;
         fprintf(stderr,"++ %d voxels in the mask\n",mcount) ;
         if( mcount <= 5 ){
            fprintf(stderr,"** Mask is too small!\n");exit(1);
         }
         DSET_delete(mask_dset) ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-polort") == 0 ){
        polort = strtol( argv[++iarg] , NULL , 10 ) ;
        if( polort < 0 || polort > 3 ){
          fprintf(stderr,"** Illegal value of polort!\n"); exit(1);
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
   nvals = DSET_NVALS(dset) ;
   if( nvals < 5 ){
      fprintf(stderr,"** Can't use dataset with < 5 time points per voxel!\n") ;
      exit(1) ;
   }
   nxyz = DSET_NVOX(dset) ;
   if( mmm != NULL && mmvox != nxyz ){
      fprintf(stderr,"** Mask and input datasets not the same size!\n") ;
      exit(1) ;
   }
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /*-- 12 Aug 2001: compute clip, if desired --*/

   if( do_autoclip && mmm == NULL ){
      mmm = THD_automask( dset ) ;
   }

   /*-- setup to save a new dataset, if desired --*/

   if( saveit ){
      float *bar ;
      oset = EDIT_empty_copy( dset ) ;
      EDIT_dset_items( oset ,
                         ADN_prefix    , prefix ,
                         ADN_brick_fac , NULL ,
                         ADN_datum_all , MRI_float ,
                         ADN_func_type , FUNC_FIM_TYPE ,
                       ADN_none ) ;

      if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) ){
        fprintf(stderr,"** ERROR: -save dataset already exists!\n"); exit(1);
      }

      tross_Copy_History( oset , dset ) ;
      tross_Make_History( "3dToutcount" , argc , argv , oset ) ;

      for( iv=0 ; iv < nvals ; iv++ ){
        EDIT_substitute_brick( oset , iv , MRI_float , NULL ) ;
        bar = DSET_ARRAY(oset,iv) ;
        for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = 0.0 ;
      }
   }

   /*-- setup to count --*/

   sq2p  = sqrt(0.5*PI) ;
   alph  = qginv(qthr/nvals) * sq2p ;
   count = (int *) calloc( sizeof(int) , nvals ) ;
   var   = (float *) malloc( sizeof(float) * nvals ) ;

   /* 07 Aug 2002: make polort refs */

   nref = polort+1 ;
   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *) malloc( sizeof(float) * nvals ) ;

   fit = (float *) malloc( sizeof(float) * nref ) ;

   /* r(t) = 1 */

   for( iv=0 ; iv < nvals ; iv++ ) ref[0][iv] = 1.0 ;

   jj = 1 ;
   if( polort > 0 ){

     /* r(t) = t - tmid */

     float tm = 0.5 * (nvals-1.0) ; float fac = 2.0 / nvals ;
     for( iv=0 ; iv < nvals ; iv++ ) ref[1][iv] = (iv-tm)*fac ;
     jj = 2 ;

     /* r(t) = (t-tmid)**jj */

     for( ; jj <= polort ; jj++ )
      for( iv=0 ; iv < nvals ; iv++ )
       ref[jj][iv] = pow( (iv-tm)*fac , (double)jj ) ;
   }

   /*--- loop over voxels and count ---*/

   for( cc=ii=0 ; ii < nxyz ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ;  /* masked out */

      npass++ ;

      flim = THD_extract_series( ii , dset , 0 ) ;  /* get data */
      far  = MRI_FLOAT_PTR(flim) ;
      memcpy(var,far,sizeof(float)*nvals ) ;        /* copy data */

      if( polort == 0 ){                     /* the old way */

        fmed = qmed_float( nvals , far ) ;
        for( iv=0 ; iv < nvals ; iv++ ){
          var[iv] = var[iv] - fmed ;         /* remove median = resid */
          far[iv] = fabs(var[iv]) ;          /* abs value of resid */
        }

      } else {                               /* 07 Aug 2002: detrend */

        float val ;
        cls = cl1_solve( nvals, nref , far , ref , fit,0 ); /* get fit */
        if( cls < 0.0 ){ nbad++ ; continue; }               /* bad! should not happen */
        for( iv=0 ; iv < nvals ; iv++ ){                    /* detrend */
          val = 0.0 ;
          for( jj=0 ; jj < nref ; jj++ )                    /* fitted value */
            val += fit[jj] * ref[jj][iv] ;

          var[iv] = var[iv]-val ;            /* remove fitted value = resid */
          far[iv] = fabs(var[iv]) ;          /* abs value of resid */
        }
      }

      /* find median of abs(detrended data) */

      fmad = qmed_float( nvals , far ) ;
      ftop = alph*fmad ; fbot = -ftop ;

      if( fmad > 0.0 ){
        if( saveit ) fsig = 1.0/(sq2p*fmad) ;
        for( ic=iv=0 ; iv < nvals ; iv++ ){
          oot = (var[iv] < fbot || var[iv] > ftop ) ;
          if( oot ){ count[iv]++ ; cc++ ; }
          if( saveit ){
            if( oot ){ far[iv] = -log10qg(fabs(var[iv]*fsig)); ic++; }
            else     { far[iv] = 0.0 ; }
          }
        }

        if( ic > 0 ){
          nfsc += thd_floatscan( nvals , far ) ;
          THD_insert_series( ii,oset, nvals,MRI_float,far , 0 ) ;
        }
      }

      mri_free(flim) ;

   } /* end of loop over voxels */

   if( nbad > 0 ) WARNING_message("%d failures of cl1_solve",nbad) ;
   if( nfsc > 0 ) WARNING_message("%d float errors in -save output",nfsc) ;

   if( saveit && cc > 0 ){
     DSET_write( oset ) ;
     INFO_message("Output dataset = %s\n",DSET_BRIKNAME(oset)) ;
   }

   if( do_range ){
      float *ff = (float *)malloc(sizeof(float)*nvals) , cmed,cmad ;
      int ctop ;

      for( iv=0 ; iv < nvals ; iv++ ) ff[iv] = count[iv] ;
      qmedmad_float( nvals,ff , &cmed,&cmad ) ; free(ff) ;
      ctop = (int)(cmed+3.5*cmad+0.499) ;
      if( do_frac )     /* 04 Jun 2010 [rickr] */
         for( iv=0 ; iv < nvals ; iv++ )
            printf("%0.5f %d\n",(float)count[iv]/npass,ctop) ;
      else
         for( iv=0 ; iv < nvals ; iv++ ) printf("%6d %d\n",count[iv],ctop) ;
   } else {
      if( do_frac )     /* 04 Jun 2010 [rickr] */
         for( iv=0 ; iv < nvals ; iv++ )
            printf("%0.5f\n",(float)count[iv]/npass) ;
      else
         for( iv=0 ; iv < nvals ; iv++ ) printf("%6d\n",count[iv]) ;
   }

#if 0
   DSET_unload(dset) ; free(count) ; free(var) ; if(mmm!=NULL)free(mmm) ;
#endif

   if( npass < nxyz )
      fprintf(stderr,"++ %d voxels passed mask/clip\n",npass) ;

   exit(0) ;
}
