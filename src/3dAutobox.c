#include "mrilib.h"

void help_autobox()
{
   printf(
    "Usage: 3dAutobox [options] DATASET\n"
     "Computes size of a box that fits around the volume.\n"
     "Also can be used to crop the volume to that box.\n"
     "\n"
     "OPTIONS:\n"
     "--------\n"
     "-prefix PREFIX = Crop the input dataset to the size of the box, and\n"
     "                 write an output dataset with PREFIX for the name.\n"
     "               * If -prefix is not used, no new volume is written out,\n"
     "                 just the (x,y,z) extents of the voxels to be kept.\n"
     "\n"
     "-input DATASET = An alternate way to specify the input dataset.\n"
     "                 The default method is to pass DATASET as\n"
     "                 the last parameter on the command line.\n"
     "\n"
     "-noclust       = Don't do any clustering to find box. Any non-zero\n"
     "                 voxel will be preserved in the cropped volume.\n"
     "                 The default method uses some clustering to find the\n"
     "                 cropping box, and will clip off small isolated blobs.\n"
     "\n"
     "-extent: Write to standard out the spatial extent of the box\n"
     "-npad NNN      = Number of extra voxels to pad on each side of box,\n"
     "                 since some troublesome people (that's you, LRF) want\n"
     "                 this feature for no apparent reason.\n"
     "               * With this option, it is possible to get a dataset that\n"
     "                 is actually bigger than the input.\n"
     "               * You can input a negative value for NNN, which will\n"
     "                 crop the dataset even more than the automatic method.\n"
     "\n"
   ) ;
   PRINT_COMPILE_DATE ; return ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset, *outset=NULL;
   int iarg=1, npad = 0, extent=0;
   char *prefix=NULL, *iname=NULL;

   /*-- startup bureaucracy --*/


   mainENTRY("3dAutobox main"); machdep(); AFNI_logger("3dAutobox",argc,argv);
   PRINT_VERSION("3dAutobox") ;

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         help_autobox();
         exit(0) ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** 3dAutobox: Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
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

      if( strcmp(argv[iarg],"-npad") == 0 ){
        npad = (int)strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-extent") == 0 ){
        extent = 1 ;
        iarg++ ; continue ;
      }

     /*- washappenin, dood? -*/

      ERROR_message("** 3dAutobox: %s makes no sense here.\n",
                 argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   if( argc < 2){ help_autobox(); exit(0); }

   /* got input ? */

   if( iarg == argc-1 )
     iname = argv[iarg] ;
   else if (iarg != argc)
     ERROR_exit("** 3dAutobox: %s is nonsense on the line \n"
                "   I know you're John; stop pretending you have an accent!",
                argv[iarg]) ;

   if( !iname )
     ERROR_exit("** 3dAutobox: Where is my input?") ;

   /*-- read data --*/

   dset = THD_open_dataset(iname); CHECK_OPEN_ERROR(dset,iname);
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   )
       ERROR_exit("** ILLEGAL dataset type: %s :-(",MRI_type_name[DSET_BRICK_TYPE(dset,0)]) ;

   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   {
      int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
      int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1;
      THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp, NULL ) ;

      xm -= npad; ym -= npad; zm -= npad;  /* for LRF */
      xp += npad; yp += npad; zp += npad;

      INFO_message("Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
                   xm,xp,ym,yp,zm,zp ) ;

      if (extent && !prefix) prefix = "EXTENT_ONLY";
      if( prefix ){
         outset = THD_zeropad( dset ,
                            -xm, xp-nx+1,
                            -ym, yp-ny+1,
                            -zm, zp-nz+1,
                            prefix , ZPAD_IJK ) ;
         if( THD_deathcon() && THD_is_file(DSET_HEADNAME(outset)) )
            ERROR_exit("3dAutobox: output file %s already exists :-(",
                       DSET_HEADNAME(outset) ) ;

         if( outset == NULL )
            ERROR_exit("3dAutobox: Some error occurred in processing :-(") ;

         tross_Copy_History( dset , outset ) ;             /* 31 Jan 2001 - RWCox */
         tross_Make_History( "3dAutobox" , argc,argv , outset ) ;

         if (!strstr(prefix,"EXTENT_ONLY")) {
            DSET_write(outset) ;
            INFO_message("3dAutobox: output dataset = %s",
                         DSET_BRIKNAME(outset)) ;
         }
         if (extent) {
          float RL_AP_IS[6];
          THD_dset_extent(outset, '-', RL_AP_IS);
          INFO_message("Extent auto bbox: RL=%f..%f  AP=%f..%f  IS=%f..%f\n",
                    RL_AP_IS[0],RL_AP_IS[1],
                    RL_AP_IS[2],RL_AP_IS[3],
                    RL_AP_IS[4],RL_AP_IS[5] ) ;
         }
      }

   }

   exit(0) ;
}
