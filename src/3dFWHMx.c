#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   int iarg=1 , ii , nvals,nvox , ncon ;
   MRI_IMAGE *outim ; float *outar ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *outfile = NULL ;
   double fx,fy,fz , cx,cy,cz , ccomb ; int nx,ny,nz , ncomb ;
   int geom=1 , demed=0 , unif=0 , corder=0 , combine=0 ;
   char *newprefix=NULL ;

   /*---- for the clueless who wish to become clueful ----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dFWHMx [options] dataset\n"
      "\n"
      "Unlike the older 3dFWHM, this program computes FWHMs for all sub-bricks\n"
      "in the input dataset, each one separately.  The output for each one is\n"
      "written to the file specified by '-out'.  The mean (arithmetic or geometric)\n"
      "of all the FWHMs along each axis is written to stdout.  (A non-positive\n"
      "output value indicates something happened; e.g., FWHM in z is meaningless\n"
      "for a 2D dataset.)\n"
      "\n"
      "METHODS:\n"
      " - Calculate ratio of variance of first differences to data variance.\n"
      " - Should be the same as 3dFWHM for a 1-brick dataset.\n"
      "   (But the output format is simpler to use in a script.)\n"
      "\n"
      "OPTIONS:\n"
      "  -mask mmm   = Use only voxels that are nonzero in dataset 'mmm'.\n"
      "  -automask   = Compute a mask from THIS dataset, a la 3dAutomask.\n"
      "                [Default = use all voxels]\n"
      "\n"
      "  -input ddd }=\n"
      "    *OR*     }= Use dataset 'ddd' as the input.\n"
      "  -dset  ddd }=\n"
      "\n"
      "  -demed      = If the input dataset has more than one sub-brick\n"
      "                (e.g., has a time axis), then subtract the median\n"
      "                of each voxel's time series before processing FWHM.\n"
      "                This will tend to remove intrinsic spatial structure\n"
      "                and leave behind the noise.\n"
      "                [Default = don't do this]\n"
      "  -unif       = If the input dataset has more than one sub-brick,\n"
      "                then normalize each voxel's time series to have\n"
      "                the same MAD before processing FWHM.  Implies -demed.\n"
      "                [Default = don't do this]\n"
      "  -detrend [q]= Instead of demed (0th order detrending), detrend to\n"
      "                order 'q'.  If q is not given, the program picks q=NT/30.\n"
      "                -detrend disables -demed, and includes -unif.\n"
      "        **N.B.: I recommend this option, and it is not the default\n"
      "                only for historical compatibility reasons.  It may\n"
      "                become the default someday. Depending on my mood.\n"
      "                It is already the default in program 3dBlurToFWHM.\n"
      "        **N.B.: This is the same detrending as done in 3dDespike;\n"
      "                using 2*q+3 basis functions for q > 0.\n"
      "  -detprefix d= Save the detrended file into a dataset with prefix 'd'.\n"
      "                Used mostly to figure out what the hell is going on,\n"
      "                when funky results transpire.\n"
      "\n"
      "  -geom      }= If the input dataset has more than one sub-brick,\n"
      "    *OR*     }= compute the final estimate as the geometric mean\n"
      "  -arith     }= or the arithmetic mean of the individual sub-brick\n"
      "                FWHM estimates. [Default = -geom, for no good reason]\n"
      "\n"
      "  -combine    = combine the final measurements along each axis into\n"
      "                one result\n"
      "\n"
      "  -out ttt    = Write output to file 'ttt' (3 columns of numbers).\n"
      "                If not given, the sub-brick outputs are not written.\n"
      "                Use '-out -' to write to stdout, if desired.\n"
      "\n"
      "  -compat     = Be compatible with the older 3dFWHM, where if a\n"
      "                voxel is in the mask, then its neighbors are used\n"
      "                for differencing, even if they are not themselves in\n"
      "                the mask.  This was an error; now, neighbors must also\n"
      "                be in the mask to be used in the differencing.\n"
      "                Use '-compat' to use the older method.\n"
      "              **NOT RECOMMENDED except for comparison purposes!\n"
      "\n"
      "SAMPLE USAGE: (tcsh)\n"
      "  set zork = ( `3dFWHMx -automask -input junque+orig` )\n"
      "Captures the FWHM-x, FWHM-y, FWHM-z values into shell variable 'zork'.\n"
      "\n"
      "INPUT FILE RECOMMENDATIONS:\n"
      "For FMRI statistical purposes, you DO NOT want the FWHM to reflect\n"
      "the spatial structure of the underlying anatomy.  Rather, you want\n"
      "the FWHM to reflect the spatial structure of the noise.  This means\n"
      "that the input dataset should not have anatomical structure.  One\n"
      "good form of input is the output of '3dDeconvolve -errts', which is\n"
      "the residuals left over after the GLM fitted signal model is subtracted\n"
      "out from each voxel's time series.  If you don't want to go to that\n"
      "trouble, use '-unif' to at least partially subtract out the anatomical\n"
      "spatial structure, or use the output of 3dDetrend for the same purpose.\n"
      "\n"
      "IF YOUR DATA HAS SMOOTH-ISH SPATIAL STRUCTURE YOU CAN'T GET RID OF:\n"
      "For example, you only have 1 volume, say from PET imaging.  In this case,\n"
      "the standard estimate of the noise smoothness will be mixed in with the\n"
      "structure of the background.  An approximate way to avoid this problem\n"
      "is provided with the semi-secret '-2difMAD' option, which uses a combination of\n"
      "first-neighbor and second-neighbor differences to estimate the smoothness,\n"
      "rather than just first-neighbor differences, and uses the MAD of the differences\n"
      "rather than the standard deviation.  (If you must know the details, read the\n"
      "source code in mri_fwhm.c!)                    [For Jatin Vaidya, March 2010]\n"
      "\n"
      "ALSO SEE:\n"
      " - The older program 3dFWHM is superseded by 3dFWHMx.\n"
      " - 3dLocalstat -stat FWHM will estimate the FWHM values at each\n"
      "   voxel, using the same first-difference algorithm as this program but applied\n"
      "   only to a local neighborhood of each voxel in turn.\n"
      " - 3dBlurToFWHM will blur a dataset to have a given global FWHM.\n"
      " - 3dBlurInMask will blur a dataset inside a mask, but doesn't measure FWHM.\n"
      "\n"
      "-- Emperor Zhark - Halloween 2006 --- BOO!\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dFWHMx"); mainENTRY("3dFWHMx main"); machdep();

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-detrend") == 0 ){          /* 10 May 2007 */
       corder = -1 ;
       if( iarg < argc-1 && isdigit(argv[iarg+1][0]) ){
         corder = (int)strtod(argv[++iarg],NULL) ;
         if( corder == 0 ){
           demed = 1 ; INFO_message("-detrend 0 replaced by -demed") ;
         }
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-2dif") == 0 ){             /* 20 Nov 2006 */
       mri_fwhm_setfester( mri_estimate_FWHM_12dif ) ;  /* secret option */
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-2difMAD") == 0 ){              /* 24 Mar 2010 */
       mri_fwhm_setfester( mri_estimate_FWHM_12dif_MAD ) ;  /* secret option */
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-geom",4) == 0 ){          /* 15 Nov 2006 */
       geom = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-arith",5) == 0 ){         /* 15 Nov 2006 */
       geom = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-demed",5) == 0 ){         /* 15 Nov 2006 */
       demed = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-unif",5) == 0 ){          /* 07 Dec 2006 */
       unif = demed = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-comb",4) == 0 ){          /* 24 Mar 2010 */
       combine = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-compat",6) == 0 ){        /* 09 Nov 2006 */
       FHWM_1dif_dontcheckplus(1) ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-out",4) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-out'") ;
       outfile = argv[iarg] ;
            if( strcasecmp(outfile,"NULL") == 0 ) outfile = NULL ;
       else if( !THD_filename_ok(outfile) ) ERROR_exit("Illegal filename after '-out'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-dset") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 16 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-detprefix") == 0 ){
       newprefix = argv[++iarg] ; iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   if( (demed || unif || corder ) && DSET_NVALS(inset) < 4 ){
     WARNING_message(
       "-demed and/or -corder and/or -unif ignored: only %d input sub-bricks",
       DSET_NVALS(inset) ) ;
     demed = corder = unif = 0 ;
   }

   if( demed && corder ){
     demed = 0 ; WARNING_message("-demed is overriden by -corder") ;
   }

   if( corder < 0 ) corder = DSET_NVALS(inset) / 30 ;
   if( corder > 0 && 2*corder+3 >= DSET_NVALS(inset) )
     ERROR_exit("-corder %d is too big for this dataset",corder) ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid dimensions don't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 16 ) ERROR_exit("Automask is too small to process") ;
   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ;
     memset(mask,1,sizeof(byte)*nvox) ;
   }
   if( nvals > 3 ){
     for( ncon=ii=0 ; ii < nvox ; ii++ ){
       if( mask[ii] && THD_voxel_is_constant(ii,inset) ){ mask[ii] = 0; ncon++; }
     }
     if( ncon > 0 )
       WARNING_message("removed %d voxels from mask because they are constant in time",ncon) ;
   }

   /*-- if detrending, do that now --*/

   if( corder > 0 ){
     int nref=2*corder+3 , jj,iv,kk ;
     float **ref , tm,fac,fq ;
     THD_3dim_dataset *newset ;

     INFO_message("detrending start: %d baseline funcs, %d time points",nref,nvals) ;

     ref = THD_build_trigref( corder , nvals ) ;
     if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

     newset = THD_detrend_dataset( inset , nref , ref , 2 , 1 , mask , NULL ) ;
     if( newset == NULL ) ERROR_exit("detrending failed!") ;

     for(jj=0;jj<nref;jj++) free(ref[jj]) ;
     free(ref); DSET_delete(inset); inset=newset;
     demed = unif = 0 ;
     INFO_message("detrending done") ;

     if( newprefix != NULL ){    /** for debugging **/
       EDIT_dset_items(newset,ADN_prefix,newprefix,NULL) ;
       (void)THD_deconflict_prefix(newset) ;
       DSET_write(newset) ; WROTE_DSET(newset) ;
     }
   }

   /*-- do the FWHM-izing work --*/

   outim = THD_estimate_FWHM_all( inset , mask , demed,unif ) ;

   DSET_unload(inset) ;

   if( outim == NULL ) ERROR_exit("Function THD_estimate_FWHM_all() fails?!") ;

   if( outfile != NULL ) mri_write_ascii( outfile , outim ) ;

   outar = MRI_FLOAT_PTR(outim) ;

   nx = thd_floatscan( 3*nvals, outar ) ;  /* 07 Dec 2006 */
   if( nx > 0 ) WARNING_message("found %d non-finite FWHM array values!",nx);

   nx = ny = nz = 0 ;
   if( geom ){
     cx = cy = cz = 0.0 ;
     for( ii=0 ; ii < nvals ; ii++ ){
       fx = outar[0+3*ii]; fy = outar[1+3*ii]; fz = outar[2+3*ii];
       if( fx > 0.0 ){ cx += log(fx) ; nx++ ; }
       if( fy > 0.0 ){ cy += log(fy) ; ny++ ; }
       if( fz > 0.0 ){ cz += log(fz) ; nz++ ; }
     }
     cx = (nx == 0) ? 0.0 : exp(cx/nx) ;
     cy = (ny == 0) ? 0.0 : exp(cy/ny) ;
     cz = (nz == 0) ? 0.0 : exp(cz/nz) ;
     ccomb = 1.0 ; ncomb = 0 ;
     if( cx > 0.0 ){ ccomb *= cx ; ncomb++ ; }
     if( cy > 0.0 ){ ccomb *= cy ; ncomb++ ; }
     if( cz > 0.0 ){ ccomb *= cz ; ncomb++ ; }
          if( ncomb == 2 ) ccomb = sqrt(ccomb) ;
     else if( ncomb == 3 ) ccomb = cbrt(ccomb) ;
   } else {
     cx = cy = cz = 0.0 ;
     for( ii=0 ; ii < nvals ; ii++ ){
       fx = outar[0+3*ii]; fy = outar[1+3*ii]; fz = outar[2+3*ii];
       if( fx > 0.0 ){ cx += fx ; nx++ ; }
       if( fy > 0.0 ){ cy += fy ; ny++ ; }
       if( fz > 0.0 ){ cz += fz ; nz++ ; }
     }
     cx = (nx == 0) ? 0.0 : cx/nx ;
     cy = (ny == 0) ? 0.0 : cy/ny ;
     cz = (nz == 0) ? 0.0 : cz/nz ;
     ccomb = 1.0 ; ncomb = 0 ;
     if( cx > 0.0 ){ ccomb *= cx ; ncomb++ ; }
     if( cy > 0.0 ){ ccomb *= cy ; ncomb++ ; }
     if( cz > 0.0 ){ ccomb *= cz ; ncomb++ ; }
     if( ncomb > 1 ) ccomb /= ncomb ;
   }
   printf(" %g  %g  %g",cx,cy,cz) ;
   if( combine ) printf("     %g",ccomb) ;
   printf("\n") ;
   exit(0) ;
}
