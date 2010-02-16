#include "mrilib.h"

void help_autobox()
{
   printf(
"Usage: 3dAutobox [options] DATASET\n"
 "Computes size of a box that fits around the volume.\n"
 "Also can be used to crop the volume to that box.\n"
 "Optional parameters are:\n"
 "-prefix PREFIX: Crop the input dataset to the size of the box.\n"
 "                If left empty no new volume is written out.\n"
 "-input DATASET: An alternate way to specify the input dataset.\n"
 "                The default method is to pass DATASET as\n"
 "                the last parameter on the command line.\n"
 "-noclust      : Don't do any clustering to find box. Any non-zero\n"
 "                voxel will be preserved in cropped volume.\n"
 "                The default uses some clustering to find cropping\n"
 "\n"
            ) ;
   PRINT_COMPILE_DATE ; return;
}
int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset, *outset = NULL;
   int iarg=1 ;
   char *prefix=NULL, *iname=NULL;
   
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      help_autobox();
      exit(0) ;
   }

   mainENTRY("3dAutobox main"); machdep(); AFNI_logger("3dAutobox",argc,argv);
   PRINT_VERSION("3dAutobox") ;

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
 
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** 3dAutobox: Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         help_autobox();
         exit(0) ;
      }
      
      if( strcmp(argv[iarg],"-input") == 0 ){
         iname = argv[++iarg] ;
         if( !THD_filename_ok(iname) ){
            fprintf(stderr,"** 3dAutobox: Illegal string after -input!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-noclust") == 0 ){
         MRI_autobbox_clust(0) ;  /* turn of clustering and clipping */
         THD_autobbox_clip(0) ;
         iarg++ ; continue ;
      }

      
      /*- wsshappenin? -*/
      
      fprintf(stderr,"** 3dAutobox: option %s is not known.\n",
                     argv[iarg]) ; 
      exit(1) ;
   }
   
   /* got input ? */
   if (iarg == argc-1) iname = argv[iarg];
   else if (iarg != argc) {
      fprintf(stderr,"** 3dAutobox: %s is nonsense on the line \n",
                     argv[iarg]) ; 
      exit(1) ;
   }
   
   if (!iname) {
      fprintf(stderr,"** 3dAutobox: Where is my input?\n"); exit(1);
   }
   
   /*-- read data --*/

   dset = THD_open_dataset(iname); CHECK_OPEN_ERROR(dset,iname);
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   ){
      fprintf(stderr,"** ILLEGAL dataset type\n"); exit(1);
   }
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   { 
      int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
      int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1 ;
      THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp ) ;
      fprintf(stderr,"++ Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
               xm,xp,ym,yp,zm,zp ) ;

      if (prefix){
         outset = THD_zeropad( dset ,
                            -xm, xp-nx+1, 
                            -ym, yp-ny+1, 
                            -zm, zp-nz+1,
                            prefix , ZPAD_IJK ) ;
         if( THD_deathcon() && THD_is_file(DSET_HEADNAME(outset)) ){
            fprintf(stderr,
                    "** 3dAutobox: output file %s already exists - FATAL ERROR!\n",
                    DSET_HEADNAME(outset) ) ;
            exit(1) ;
         }

         if( outset == NULL ){
            fprintf(stderr,"** 3dAutobox: Some error occurred in processing!\n") ;
            exit(1) ;
         }

         tross_Copy_History( dset , outset ) ;             /* 31 Jan 2001 - RWCox */
         tross_Make_History( "3dAutobox" , argc,argv , outset ) ;

         DSET_write(outset) ;
         fprintf(stderr,"++ output dataset: %s\n",DSET_BRIKNAME(outset)) ;
      }

   }

   exit(0) ;
}
