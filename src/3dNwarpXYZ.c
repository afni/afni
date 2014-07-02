#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void print_usage(void)
{
   printf(
    "Usage: 3dNwarpXYZ -nwarp 'warp specification' XYZfile.1D > Output.1D\n"
    "\n"
    "Transforms the DICOM xyz coordinates in the input XYZfile.1D (3 columns)\n"
    "based on the '-nwarp' specification -- which is as in 3dNwarpApply.\n"
    "\n"
    "If this warp is the _WARP output from 3dQwarp, then it takes XYZ values\n"
    "from the base dataset and transforms them to the corresponding source\n"
    "dataset location.  To do the reverse operation, either use the _WARPINV\n"
    "output from 3dQwarp, or using the 'INV(dataset)' form for '-nwarp'\n"
    "(however, full warp inversion is somewhat slow).\n"
    "\n"
    "July 2014 - Zhark the Coordinated\n"
   ) ;
   PRINT_AFNI_OMP_USAGE("3dNwarpXYZ",NULL) ; PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_nwarp=NULL ;
   MRI_IMAGE *xyz_in , *xyz_out ;
   int npt , code , iarg ;
   float *xin,*yin,*zin , *xut,*yut,*zut ;

   AFNI_SETUP_OMP(0) ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) print_usage() ;


   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dNwarpXYZ"); machdep();
   AFNI_logger("3dNwarpXYZ",argc,argv);
   PRINT_VERSION("3dNwarpXYZ"); AUTHOR("Zhark the Coordinated");
   (void)COX_clock_time() ;
   putenv("AFNI_WSINC5_SILENT=YES") ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-nwarp") == 0 || strcasecmp(argv[iarg],"-warp") == 0 ){
       if( dset_nwarp != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       dset_nwarp = IW3D_read_catenated_warp( argv[iarg] ) ;  /* the complicated way */
       if( dset_nwarp == NULL ) ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg]);
       if( DSET_NVALS(dset_nwarp) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg]);
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown, Illegal, and Fattening option '%s' :-( :-( :-(",argv[iarg]) ;
   }

   if( dset_nwarp == NULL )
     ERROR_exit("No -nwarp option?  How do you want to warp? :-(") ;

   if( iarg >= argc )
     ERROR_exit("No 1D file on command line? :-(") ;

   xyz_in = mri_read_1D( argv[iarg] ) ;
   if( xyz_in == NULL )
     ERROR_exit("Can't read 1D file '%s'",argv[iarg]) ;

   if( xyz_in->ny != 3 )
     ERROR_exit("1D file '%s' has %d columns, not the 3 required",argv[iarg],xyz_in->ny) ;

   npt = xyz_in->nx ;
   xin = MRI_FLOAT_PTR(xyz_in) ;
   yin = xin + npt ;
   zin = yin + npt ;

   xyz_out = mri_new( npt , 3 , MRI_float ) ;
   xut = MRI_FLOAT_PTR(xyz_out) ;
   yut = xut + npt ;
   zut = yut + npt ;

   code = THD_nwarp_forward_xyz( dset_nwarp ,
                                 npt , xin,yin,zin , xut,yut,zut ) ;

   if( code < 0 )
     ERROR_exit("THD_nwarp_forward_xyz returns error code = %d",code) ;

   mri_write_1D( "stdout:" , xyz_out ) ;
   exit(0) ;
}
