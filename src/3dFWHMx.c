#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   int iarg=1 , ii , nvals ;
   MRI_IMAGE *outim ; float *outar ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *outfile = "-" ;
   double fx,fy,fz , cx,cy,cz ; int nx,ny,nz ;

   /*---- for the clueless who wish to become clued-in ----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dFWHMx [options] dataset\n"
      "\n"
      "Unlike the older 3dFWHM, this program computes FWHMs for all sub-bricks\n"
      "in the input dataset, each one separately.  The output for each one\n"
      "is written to the file specified by '-out'.  The geometric mean\n"
      "of all the FWHMs along each axis is written to stdout.  (A non-positive\n"
      "output value indicates something happened; e.g., FWHM in z is meaningless\n"
      "for a 2D dataset.)\n"
      "\n"
      "OPTIONS:\n"
      "  -mask mmm  = Use only voxels that are nonzero in dataset 'mmm'.\n"
      "  -automask  = Compute a mask from THIS dataset, a la 3dAutomask.\n"
      "\n"
      "  -input ddd}=\n"
      "    *OR*    }= Use dataset 'ddd' as the input.\n"
      "  -dset  ddd}=\n"
      "\n"
      "  -out ttt   = Write output to file 'ttt' (3 columns of numbers).\n"
      "               [If not given, 'ttt' defaults to stdout.]\n"
      "               [Use '-out NULL' to suppress this output file.]\n"
      "\n"
      "SAMPLE USAGE: (tcsh)\n"
      "  set zork = `3dFWHMx -automask -input junque+orig -out NULL`\n"
      "\n"
      "-- Emperor Zhark - Halloween 2006 --- BOO!\n"
     ) ;
     exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dFWHM"); mainENTRY("3dFWHM main"); machdep();

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

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
       if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ;
       if( !DSET_LOADED(mset) ) ERROR_exit("Can't load dataset '%s'",argv[iarg]) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
   }

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) )
     ERROR_exit("Can't load input dataset '%s' from disk") ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 9 ) ERROR_exit("Automask is too small to process") ;
   }

   /*-- do the work --*/

   outim = THD_estimate_FWHM_all( inset , mask ) ;

   DSET_unload(inset) ; nvals = DSET_NVALS(inset) ;

   if( outim == NULL ) ERROR_exit("Function THD_estimate_FWHM_all() fails?!") ;

   if( outfile != NULL ) mri_write_ascii( outfile , outim ) ;

   outar = MRI_FLOAT_PTR(outim) ;
   cx = cy = cz = 1.0 ; nx = ny = nz = 0 ;
   for( ii=0 ; ii < nvals ; ii++ ){
     fx = outar[0+3*ii]; fy = outar[1+3*ii]; fz = outar[2+3*ii];
     if( fx > 0.0 ){ cx *= fx ; nx++ ; }
     if( fy > 0.0 ){ cy *= fy ; ny++ ; }
     if( fz > 0.0 ){ cz *= fz ; nz++ ; }
   }
   if( nx == 0 ) cx = 0.0 ; else if( nx > 1 ) cx = pow(cx,1.0/nx) ;
   if( ny == 0 ) cy = 0.0 ; else if( ny > 1 ) cy = pow(cy,1.0/ny) ;
   if( nz == 0 ) cz = 0.0 ; else if( nz > 1 ) cz = pow(cz,1.0/nz) ;
   printf(" %g  %g  %g\n",cx,cy,cz) ;
   exit(0) ;
}
