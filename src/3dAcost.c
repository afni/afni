#include "mrilib.h"

/** Adapted from 3ddot.c -- 10 Oct 2006 -- RWCox **/

#define DISABLE_PROGRAM

#ifdef  DISABLE_PROGRAM
int main( int argc , char *argv[] )
{
  printf("\n"
    "  *** Program 3dAcost is no longer available.\n"
    "  *** Use '3dAllineate -allcostX' instead;\n"
    "  *** See the output of '3dAllineate -HELP' for more information.\n\n"
  ) ;
  exit(0) ;
}

#else

int main( int argc , char *argv[] )
{
   MRI_IMAGE *xim , *yim ;
   float     *xar , *yar , *war ;
   int iarg , ndset , nvox , ii , jj, xyall , clip=0 ;
   THD_3dim_dataset *xset , *yset , * mask_dset=NULL ;
   float xbot=1.0f,xtop=0.0f , ybot=1.0f,ytop=0.0f , val ;
   float xsum,ysum , xsig,ysig ;
   byte *mmm=NULL ;
   char *save_hist=NULL, *save_hist_1D=NULL ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
     printf("Usage: 3dAcost [options] xset yset\n"
            "Output = 3dAllineate cost functions between 2 dataset bricks\n"
            "         (for debugging purposes, mostly).\n"
            "\n"
            "Options:\n"
            "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
            "                 Only voxels with nonzero values in 'mset'\n"
            "                 will be averaged from 'dataset'.  Note\n"
            "                 that the mask dataset and the input dataset\n"
            "                 must have the same number of voxels.\n"
            " -histpow p    Set histogram power to 'p'; number of bins in\n"
            "                 histogram (each axis) = n^p (n=# of points).\n"
            "                 (Default is p=0.33333.)\n"
            " -histbin m    Set histogram to use 'm' bins (each axis).\n"
            "                 (Max number of bins is 255.)\n"
            " -savehist ss  Save 2D histogram to image file 'ss'\n"
            "                 (floating point image; read with 'afni -im')\n"
            " -savehist_1D dd  Save original form of 2D histogram to 1D file 'dd'\n"
            "                 (-savehist produces the rootogram)\n"
            " -xrange a b   Use only xset values in range a..b.\n"
            " -yrange c d   Use only yset values in range c..d.\n"
            "                 (Default is to use all values.)\n"
            " -clip         Use values from min to mri_topclip()\n"
           ) ;
     exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-xrange") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("no arguments after '%s'!",argv[iarg-1]) ;
       xbot = (float)strtod(argv[iarg++],NULL) ;
       xtop = (float)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     if( strcmp(argv[iarg],"-yrange") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("no arguments after '%s'!",argv[iarg-1]) ;
       ybot = (float)strtod(argv[iarg++],NULL) ;
       ytop = (float)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     if( strcmp(argv[iarg],"-clip") == 0 ){
       clip = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-savehist") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]);
       save_hist = argv[iarg] ; iarg++ ; continue ;
     }
     
     if( strcmp(argv[iarg],"-savehist_1D") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       save_hist_1D= argv[iarg] ; iarg++ ; continue ;
     }
     
     if( strcmp(argv[iarg],"-histpow") == 0 ){
       double hist_pow ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_pow = strtod(argv[iarg],NULL) ;
       set_2Dhist_hpower(hist_pow) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-histbin") == 0 ){
       int hist_nbin ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_nbin = (int)strtod(argv[iarg],NULL) ;
       set_2Dhist_hbin(hist_nbin) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mask",5) == 0 ){
       if( mask_dset != NULL ) ERROR_exit("Can't use -mask twice!") ;
       if( iarg+1 >= argc ) ERROR_exit("-mask needs a filename!") ;
       mask_dset = THD_open_dataset( argv[++iarg] ) ;
       CHECK_OPEN_ERROR(mask_dset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /* should have at least 2 more arguments */

   ndset = argc - iarg ;
   if( ndset <= 1 ){
      fprintf(stderr,"*** No input datasets!?\n") ; exit(1) ;
   }

   xset = THD_open_dataset(argv[iarg++]) ; CHECK_OPEN_ERROR(xset,argv[iarg-1]) ;
   yset = THD_open_dataset(argv[iarg++]) ; CHECK_OPEN_ERROR(yset,argv[iarg-1]) ;
   DSET_load(xset) ; DSET_load(yset) ;
   CHECK_LOAD_ERROR(xset) ; CHECK_LOAD_ERROR(yset) ;
   if( DSET_NVALS(xset) > 1 || DSET_NVALS(yset) > 1 )
     WARNING_message("Using only sub-brick [0] of input datasets") ;

   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) )
     ERROR_exit("Input datasets dimensions don't match!") ;

   xim = mri_scale_to_float( DSET_BRICK_FACTOR(xset,0) , DSET_BRICK(xset,0) );
   yim = mri_scale_to_float( DSET_BRICK_FACTOR(yset,0) , DSET_BRICK(yset,0) );
   xar = MRI_FLOAT_PTR(xim) ; yar = MRI_FLOAT_PTR(yim) ;

   DSET_unload(xset); DSET_unload(yset);

   /* make a byte mask from mask dataset */

   war = (float *)malloc(sizeof(float)*nvox) ;
   if( mask_dset != NULL ){
     int mcount ;
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!");

     mmm = THD_makemask( mask_dset , 0 , 666.0,-666.0 ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     if( mcount <= 5 ) ERROR_exit("Mask is too small: %d voxels",mcount) ;
     DSET_delete(mask_dset) ;
     for( ii=0 ; ii < nvox ; ii++ ) war[ii] = (float)mmm[ii] ;
     free((void *)mmm) ;
   } else {
     for( ii=0 ; ii < nvox ; ii++ ) war[ii] = 1.0f ;
   }

   if( clip ){
     xbot = mri_min(xim) ; xtop = mri_topclip(xim) ;
     INFO_message("xbot=%g  xclip=%g",xbot,xtop) ;
     ybot = mri_min(yim) ; ytop = mri_topclip(yim) ;
     INFO_message("ybot=%g  yclip=%g",ybot,ytop) ;
   }

   xyall = 0 ;
   if( xbot >= xtop ){
     xbot = 1.e+38 ; xtop = -1.e+38 ;
     for( ii=0 ; ii < nvox ; ii++ )
       if( war[ii] > 0.0f ){
              if( xar[ii] < xbot ) xbot = xar[ii] ;
         else if( xar[ii] > xtop ) xtop = xar[ii] ;
       }
     INFO_message("xbot=%g  xtop=%g",xbot,xtop) ;
     if( xbot >= xtop ) ERROR_exit("no x range?!") ;
     xyall++ ;
   }
   if( ybot >= ytop ){
     ybot = 1.e+38 ; ytop = -1.e+38 ;
     for( ii=0 ; ii < nvox ; ii++ )
       if( war[ii] > 0.0f ){
              if( yar[ii] < ybot ) ybot = yar[ii] ;
         else if( yar[ii] > ytop ) ytop = yar[ii] ;
       }
     INFO_message("ybot=%g  ytop=%g",ybot,ytop) ;
     if( ybot >= ytop ) ERROR_exit("no y range?!") ;
     xyall++ ;
   }
   if( xyall < 2 ){
     for( ii=0 ; ii < nvox ; ii++ )
       if( xar[ii] < xbot || xar[ii] > xtop ||
           yar[ii] < ybot || yar[ii] > ytop   ) war[ii] = 0.0f ;
   }
   xyall = 0 ;
   for( ii=0 ; ii < nvox ; ii++ ) xyall += (war[ii] > 0.0f) ;
   INFO_message("Processing %d voxels",xyall) ;
   if( xyall <= 5 ) ERROR_exit("Too few voxels to continue!") ;

   xsum = ysum = 0.0f ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( war[ii] > 0.0f ){ xsum += xar[ii]; ysum += yar[ii]; }
   }
   xsum /= xyall ; ysum /= xyall ;
   INFO_message("xmean=%g  ymean=%g",xsum,ysum) ;
   xsig = ysig = 0.0f ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( war[ii] > 0.0f ){
       val = (xar[ii]-xsum) ; xsig += val*val ;
       val = (yar[ii]-ysum) ; ysig += val*val ;
     }
   }
   xsig = sqrt( xsig/(xyall-1.0) ); ysig = sqrt( ysig/(xyall-1.0) );
   INFO_message("xsig =%g  ysig =%g",xsig,ysig) ;

   val = THD_pearson_corr_wt( nvox , xar , yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Correlation     = %+.5f\n",val) ;

   val = -THD_mutual_info_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   printf("-Mutual Info      = %+.5f\n",val ) ;

   val = THD_norm_mutinf_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   printf("Norm Mutual Info  = %+.5f\n",val ) ;

#if 0
   INFO_message("THD_corr_ratio_scl(%d,%g,%g,xar,%g,%g,yar,war)",
                 nvox , xbot,xtop , ybot,ytop ) ;
#endif

   THD_corr_ratio_sym_mul ;
   val = THD_corr_ratio_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Corr ratio sym* = %+.5f\n",val) ;

   THD_corr_ratio_sym_add ;
   val = THD_corr_ratio_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Corr ratio sym+ = %+.5f\n",val) ;

   THD_corr_ratio_sym_not ;
   val = THD_corr_ratio_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Corr ratio unsym= %+.5f\n",val) ;

   val = -THD_hellinger_scl( nvox , xbot,xtop,xar , ybot,ytop,yar , war ) ;
   printf("-Hellinger metric = %+.5f\n",val) ;

   if( save_hist != NULL ){  /* Save 2D histogram */
     int nbin ; float *xyc ;
     nbin = retrieve_2Dhist( &xyc ) ;
     if( nbin > 0 && xyc != NULL ){
       MRI_IMAGE *fim,*qim ; double ftop ;
       fim = mri_new(nbin,nbin,MRI_float); mri_fix_data_pointer(xyc,fim);
#if 0
       for( ii=0 ; ii < nbin*nbin ; ii++ ) xyc[ii] = sqrtf(xyc[ii]) ;
       ftop = mri_max(fim); if( ftop == 0.0 ) ftop = 1.0;
       qim = mri_to_byte_scl(255.4/ftop,0.0,fim) ;
       mri_clear_data_pointer(fim); mri_free(fim);
       fim = mri_flippo(MRI_ROT_180,1,qim); mri_free(qim);
       mri_write_pnm(save_hist,fim); mri_free(fim);
#else
       qim = mri_flippo(MRI_ROT_180,1,fim);
       mri_clear_data_pointer(fim); mri_free(fim);
       mri_write(save_hist,qim); mri_free(qim);
#endif
       INFO_message("- Saved %dx%d histogram to %s",nbin,nbin,save_hist) ;
       
     }
   }
   
   if( save_hist_1D != NULL ){  /* Save 2D raw histogram */
     int nbin ; float *xyc ;
     nbin = retrieve_2Dhist( &xyc ) ;
     if( nbin > 0 && xyc != NULL ){
         FILE *fid=fopen(save_hist_1D,"w");
         for( ii=0 ; ii < nbin; ii++ ) {
            for( jj=0 ; jj < nbin; jj++ ) {
               fprintf(fid,"%.4f\t", xyc[ii*nbin+jj]); 
            }
            fprintf(fid,"\n");
         }
         fclose(fid);
      }
      INFO_message("- Saved %dx%d 1D histogram to %s",nbin,nbin,save_hist_1D) ;
       
  }
  
   exit(0) ;
}
#endif  /* DISABLE_PROGRAM */
