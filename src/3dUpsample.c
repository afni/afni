/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See http://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

static void vstep_print(void) ; /* prototype */

int main( int argc , char *argv[] )
{
   int vstep=0 , ii,nvox , ntin , ntout , do_one=0 , nup=0 ;
   THD_3dim_dataset *inset=NULL , *outset ;
   char *prefix="Upsam" ;
   int verb=0 , iarg=1 ;
   float *ivec , *ovec ;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dUpsam [options] n dataset\n"
      "\n"
      "* Upsamples a 3D+time dataset, in the time direction,\n"
      "   by a factor of 'n'.\n"
      "* The value of 'n' must be between 2 and 32 (inclusive).\n"
      "* The output dataset is always in float format.\n"
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
      "Example:\n"
      "--------\n"
      " 3dUpsample -prefix LongFred 5 Fred+orig\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dUpsample"); machdep();
   PRINT_VERSION("3dUpsample"); AUTHOR("RWCox") ;
   AFNI_logger("3dUpsample",argc,argv);

   /*------- read command line args -------*/

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

     ERROR_exit("Unknown argument on command line: '%s'",argv[iarg]) ;
   }

   /*------- check options for completeness and consistency -----*/

   if( iarg+1 >= argc )
     ERROR_exit("need 'n' and 'dataset' on command line!") ;

   nup = (int)strtod(argv[iarg++],NULL) ;
   if( nup < 2 || nup > 32 )
     ERROR_exit("3dUpsample rate '%d' is outside range 2..32",nup) ;

   inset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(inset) )
     ERROR_exit("3dUpsample can't open dataset '%s'",argv[iarg]) ;
   ntin = DSET_NVALS(inset) ;
   if( ntin < 2 )
     ERROR_exit("dataset '%s' has only 1 value per voxel?!",argv[iarg]) ;

   nvox = DSET_NVOX(inset) ;

   if( verb ) INFO_message("loading input dataset into memory") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   /*------ create output dataset ------*/

   ntout = ntin * nup ;

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                        ADN_nvals     , ntout          ,
                        ADN_ntt       , DSET_NUM_TIMES(inset) > 1 ? ntout : 0 ,
                        ADN_datum_all , MRI_float      ,
                        ADN_brick_fac , NULL           ,
                        ADN_prefix    , prefix         ,
                      ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dUpsample" , argc,argv , outset ) ;

   for( ii=0 ; ii < ntout ; ii++ ){ /* create empty bricks to be filled below */
     EDIT_substitute_brick( outset , ii , MRI_float , NULL ) ;
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

     THD_insert_series( ii , outset , ntout , MRI_float , ovec , 1 ) ;

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
