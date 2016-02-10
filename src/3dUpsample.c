/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See https://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

static void vstep_print(void) ; /* prototype */

int main( int argc , char *argv[] )
{
   int vstep=0 , ii,nvox , ntin , ntout , do_one=0 , nup=-1 ;
   THD_3dim_dataset *inset=NULL , *outset ;
   char *prefix="Upsam", *dsetname=NULL ;
   int verb=0 , iarg=1, datum = MRI_float;
   float *ivec , *ovec , trin , trout, *fac=NULL, *ofac=NULL, 
         top=0.0, maxtop=0.0;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dUpsample [options] n dataset\n"
      "\n"
      "* Upsamples a 3D+time dataset, in the time direction,\n"
      "   by a factor of 'n'.\n"
      "* The value of 'n' must be between 2 and 320 (inclusive).\n"
      "* The output dataset is in float format by default.\n"
      "\n"
      "Options:\n"
      "--------\n"
      " -1 or -one = Use linear interpolation. Otherwise,\n"
      " or -linear   7th order polynomial interpolation is used.\n"
      "\n"
      " -prefix pp = Define the prefix name of the output dataset.\n"
      "              [default prefix is 'Upsam']\n"
      "\n"
      " -verb      = Be eloquently and mellifluosly verbose.\n"
      "\n"
      " -n n       = An alternate way to specify n\n"
      " -input dataset = An alternate way to specify dataset\n"
      "\n"
      " -datum ddd = Use datatype ddd at output. Choose from\n"
      "              float (default), short, byte.\n"
      "Example:\n"
      "--------\n"
      " 3dUpsample -prefix LongFred 5 Fred+orig\n"
      "\n"
      "Nota Bene:\n"
      "----------\n"
      "* You should not use this for files that were 3dTcat-ed across\n"
      "   imaging run boundaries, since that will result in interpolating\n"
      "   between non-contiguous time samples!\n"
      "* If the input has M time points, the output will have n*M time\n"
      "   points.  The last n-1 of them will be past the end of the original\n"
      "   time series.\n"
      "* This program gobbles up memory and diskspace as a function of n.\n"
      "  You can reduce output file size with -datum option.\n"
      "\n"
      "--- RW Cox - April 2008\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dUpsample"); machdep();
   PRINT_VERSION("3dUpsample"); AUTHOR("RWCox") ;
   AFNI_logger("3dUpsample",argc,argv);

   /*------- read command line args -------*/

   datum = MRI_float;
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal string after -prefix: '%s'",prefix) ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-one",4) == 0 ||
         strcmp     (argv[iarg],"-1"    ) == 0 ||
         strncasecmp(argv[iarg],"-lin",4) == 0   ){
       do_one = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-verb",3) == 0 ){
       verb = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-n") == 0 ){
      if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
      nup = (int)strtod(argv[iarg],NULL) ;
      if( nup < 2 || nup > 320 )
        ERROR_exit("3dUpsample rate '%d' is outside range 2..320",nup) ;
      iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-input") == 0 ){
      if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
      dsetname = argv[iarg];
      iarg++ ; continue ;
     }
     
     if( strcasecmp(argv[iarg],"-datum") == 0 ){
      if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
      
         if( strcmp(argv[iarg],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[iarg],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[iarg],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_message("-datum of type '%s' not supported in 3dUpsample!\n",
                    argv[iarg] ) ;
            exit(1) ;
         }
         
      iarg++ ; continue ;
     }
     
     ERROR_message("Unknown argument on command line: '%s'",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit (1);
   }

   /*------- check options for completeness and consistency -----*/
   
   if (nup == -1) {
      if( iarg+1 >= argc )
        ERROR_exit("need 'n' and 'dataset' on command line!") ;

      nup = (int)strtod(argv[iarg++],NULL) ;
      if( nup < 2 || nup > 320 )
        ERROR_exit("3dUpsample rate '%d' is outside range 2..320",nup) ;
   } 
   if (!dsetname) {
      if( iarg >= argc )
        ERROR_exit("need 'dataset' on command line!") ;
      dsetname = argv[iarg];
   }
   
   inset = THD_open_dataset(dsetname) ;
   if( !ISVALID_DSET(inset) )
     ERROR_exit("3dUpsample can't open dataset '%s'", dsetname) ;
   ntin = DSET_NVALS(inset) ; trin = DSET_TR(inset) ;
   if( ntin < 2 )
     ERROR_exit("dataset '%s' has only 1 value per voxel?!",dsetname) ;

   nvox = DSET_NVOX(inset) ;

   if( verb ) INFO_message("loading input dataset into memory") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;


   /*------ create output dataset ------*/

   ntout = ntin * nup ; trout = trin / nup ;

   /* scaling factor for output */
   fac = NULL; maxtop = 0.0;
   if (MRI_IS_INT_TYPE(datum)) {
      fac = (float *)calloc(DSET_NVALS(inset), sizeof(float));
      ofac = (float *)calloc(ntout, sizeof(float));
      for (ii=0; ii<DSET_NVALS(inset); ++ii) {
         top = MCW_vol_amax( DSET_NVOX(inset),1,1 , 
                             DSET_BRICK_TYPE(inset,ii), 
                             DSET_BRICK_ARRAY(inset,ii) ) ;
         if (DSET_BRICK_FACTOR(inset, ii)) 
            top = top * DSET_BRICK_FACTOR(inset,ii);
         fac[ii] = (top > MRI_TYPE_maxval[datum]) ? 
                        top/MRI_TYPE_maxval[datum] : 0.0 ;
         if (top > maxtop) maxtop = top;
      }
      if (storage_mode_from_filename(prefix) != STORAGE_BY_BRICK) {
         fac[0] = (maxtop > MRI_TYPE_maxval[datum]) ? 
                        maxtop/MRI_TYPE_maxval[datum] : 0.0 ;
         for (ii=0; ii<ntout; ++ii) 
            ofac[ii] = fac[0];
         if (verb) INFO_message("Forcing global scaling, Max = %f, fac = %f\n", 
                        maxtop, fac[0]);
      } else {
         if (verb) INFO_message("Reusing scaling factors of input dset\n");
         upsample_1( nup, DSET_NVALS(inset), fac, ofac);
      }
   }
   free(fac); fac = NULL;
   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                        ADN_nvals     , ntout          ,
                        ADN_ntt       , DSET_NUM_TIMES(inset) > 1 ? ntout : 0 ,
                        ADN_datum_all , datum      ,
                        ADN_brick_fac , ofac           ,
                        ADN_prefix    , prefix         ,
                      ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dUpsample" , argc,argv , outset ) ;
   free(ofac); ofac = NULL;
   
   if( outset->taxis != NULL ){
     outset->taxis->ttdel /= nup ;
     outset->taxis->ttdur /= nup ;
     if( outset->taxis->toff_sl != NULL ){
       for( ii=0 ; ii < outset->taxis->nsl ; ii++ )
         outset->taxis->toff_sl[ii] /= nup ;
     }
   }

   for( ii=0 ; ii < ntout ; ii++ ){ /* create empty bricks to be filled below */
     EDIT_substitute_brick( outset , ii , datum , NULL ) ;
   }

   /*------- loop over voxels and process them one at a time ---------*/

   if( verb )
     INFO_message("Upsampling time series from %d to %d: %s interpolation",
                  ntin , ntout , (do_one) ? "linear" : "heptic" ) ;

   if( verb && nvox > 499 ) vstep = nvox / 50 ;
   if( vstep > 0 ) fprintf(stderr,"++ voxel loop: ") ;

   ivec = (float *)malloc(sizeof(float)*ntin) ;
   ovec = (float *)malloc(sizeof(float)*ntout) ;

   for( ii=0 ; ii < nvox ; ii++ ){

     if( vstep > 0 && ii%vstep==vstep-1 ) vstep_print() ;

     THD_extract_array( ii , inset , 0 , ivec ) ;

     if( do_one ) upsample_1( nup , ntin , ivec , ovec ) ;
     else         upsample_7( nup , ntin , ivec , ovec ) ;

     THD_insert_series( ii , outset , ntout , MRI_float , ovec , 
                        datum==MRI_float ? 1:0 ) ;
   } /* end of loop over voxels */

   if( vstep > 0 ) fprintf(stderr," Done!\n") ;

   /*----- clean up and go away -----*/

   DSET_write(outset) ;
   if( verb ) WROTE_DSET(outset) ;
   if( verb ) INFO_message("Total CPU time = %.1f s",COX_cpu_time()) ;
   exit(0);
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}
