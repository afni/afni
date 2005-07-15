#include "mrilib.h"
#include "thd_niftiwrite.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL , *fname ;
   int narg=1 , flags=0 , ii , verb=0 , newid=1 , denote=0 , floatize=0 ;
   niftiwr_opts_t options ;

   PRINT_VERSION("3dAFNItoNIFTI");

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoNIFTI [options] dataset\n"
             "Reads an AFNI dataset, writes it out as a NIfTI-1.1 (.nii) file.\n"
             "\n"
             "NOTES:\n"
             "* The nifti_tool program can be used to manipulate\n"
             "   the contents of a NIfTI-1.1 file.\n"
             "* The input dataset can actually be in any input format\n"
             "   that AFNI can read directly (e.g., MINC-1).\n"
             "* There is no 3dNIFTItoAFNI program, since AFNI programs\n"
             "   can directly read .nii files.  If you wish to make such\n"
             "   a conversion anyway, one way to do so is like so:\n"
             "     3dcalc -a ppp.nii -prefix ppp -expr 'a'\n"
             "\n"
             "OPTIONS:\n"
             "  -prefix ppp = Write the NIfTI-1.1 file as 'ppp.nii'.\n"
             "                  Default: the dataset's prefix is used.\n"
#ifdef HAVE_ZLIB
             "                  If you want a compressed file, try\n"
             "                  using a prefix like 'ppp.nii.gz'.\n"
#endif
             "  -verb       = Be verbose = print progress messages.\n"
             "                  Repeating this increases the verbosity\n"
             "                  (maximum setting is 3 '-verb' options).\n"
             "  -float      = Force the output dataset to be 32-bit\n"
             "                  floats.  This option should be used when\n"
             "                  the input AFNI dataset has different\n"
             "                  float scale factors for different sub-bricks,\n"
             "                  an option that NIfTI-1.1 does not support.\n"
             "\n"
             "The following options affect the contents of the AFNI extension\n"
             "field that is written by default into the NIfTI-1.1 header:\n"
             "\n"
             "  -pure       = Do NOT write an AFNI extension field into\n"
             "                  the output file.  Only use this option if\n"
             "                  needed.  You can also use the 'nifti_tool'\n"
             "                  program to strip extensions from a file.\n"
             "  -denote     = When writing the AFNI extension field, remove\n"
             "                  text notes that might contain subject\n"
             "                  identifying information.\n"
             "  -oldid      = Give the new dataset the input dataset's\n"
             "                  AFNI ID code.\n"
             "  -newid      = Give the new dataset a new AFNI ID code, to\n"
             "                  distinguish it from the input dataset.\n"
             "     **** N.B.:  -newid is now the default action.\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAFNItoNIFTI main"); machdep();

   /*--- check options ---*/

   while( narg < argc && argv[narg][0] == '-' ){

     if( strcmp(argv[narg],"-newid") == 0 ){  /* 11 May 2005 - RWCox */
       newid = 1 ; narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-oldid") == 0 ){  /* 15 Jul 2005 - RWCox */
       newid = 0 ; narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-denote") == 0 ){ /* 11 Jul 2005 - RWCox */
       denote = 1 ; narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-float") == 0 ){  /* 14 Jul 2005 - RWCox */
       floatize = 1 ; narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-pure") == 0 ){   /* 11 May 2005 - RWCox */
       putenv("AFNI_NIFTI_NOEXT=YES") ;
       narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-prefix") == 0 ){
       prefix = argv[++narg] ;
       if( !THD_filename_ok(prefix) || prefix[0] == '-' )
         ERROR_exit("-prefix is illegal: %s\n",prefix) ;
       narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-verb") == 0 ){
       verb++ ; narg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[narg]);
   }

   /*--- get the dataset ---*/

   if( narg >= argc )
     ERROR_exit("No dataset on command line?\n");

   dset = THD_open_dataset( argv[narg] ) ;
   if( !ISVALID_DSET(dset) )
     ERROR_exit("Can't open dataset %s\n",argv[narg]);

   /*--- deal with the filename ---*/

   if( prefix == NULL ) prefix = DSET_PREFIX(dset) ;

   if( newid ) dset->idcode = MCW_new_idcode() ;  /* 11 May 2005 */

   fname = malloc( strlen(prefix)+16 ) ;
   strcpy(fname,prefix) ;
   if( strstr(fname,".nii") == NULL ) strcat(fname,".nii") ;

   options.infile_name = nifti_strdup(fname) ;
   options.debug_level = verb ;

   /*--- 14 Jul 2005: floatization ---*/

   if( floatize ){
     int nvals=DSET_NVALS(dset) , nxyz=DSET_NVOX(dset) , tt ;
     float fac , *far ;

     DSET_mallocize(dset); DSET_load(dset);
     if( !DSET_LOADED(dset) ) ERROR_exit("Can't load input dataset from disk") ;
     if( verb ) INFO_message("Converting dataset to floats (in memory)") ;

     /* loop over sub-bricks */

     for( ii=0 ; ii < nvals ; ii++ ){
       fac = DSET_BRICK_FACTOR(dset,ii) ; if( fac == 0.0f ) fac = 1.0f ;
       tt  = DSET_BRICK_TYPE(dset,ii) ;

       /* if already floats and scale fac is 1.0, don't need to do anything */

       if( fac == 1.0f && (tt == MRI_float || tt == MRI_complex) ) continue ;

       /* create output space */

       far = (float *)calloc( nxyz , sizeof(float) ) ;

       /* scale input to output */

       EDIT_coerce_scale_type( nxyz , fac ,
                               DSET_BRICK_TYPE(dset,ii),DSET_BRICK_ARRAY(dset,ii) ,
                               MRI_float , far ) ;

       /* replace input with output, and fix scale factor */

       EDIT_substitute_brick( dset , ii , MRI_float , far ) ;
       EDIT_BRICK_FACTOR( dset , ii , 0.0 ) ;
     } /* end of loop over sub-bricks */
   } /* end of floatizationing */

   /*--- Go Baby, Go! ---*/

   if( denote ) THD_anonymize_write(1) ; /* sets a flag for attribute output */
   ii = THD_write_nifti( dset , options ) ; /* actually write the damn thing */
   exit(0) ;
}
