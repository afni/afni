#include "mrilib.h"

#include "thd_dset_to_grayplot.c"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL ;
   MRI_IMAGE *imout ;
   char *prefix = "Grayplot.png" ;
   byte *mask=NULL ; int mask_nvox=0 ;
   int iarg = 1 ;
   int polort=2 , nxout=0,nyout=0 ; float fwhm=6.0f ;

   /*----------------------------------------------------------------*/

   if( argc < 2 ){
     printf("\n"
      "Make a grayplot from a 3D+time dataset, sort of like Jonathan Power:\n"
      "  https://www.ncbi.nlm.nih.gov/pubmed/27510328\n"
      "  https://www.jonathanpower.net/2017-ni-the-plot.html\n"
      "Result is saved to a PNG image for your viewing pleasure.\n"
      "\n"
      "  3dGrayplot [options] dataset\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      "\n"
      " -mask mset      = Name of mask dataset [REQUIRED]\n"
      "                   * Voxels that are 0 in mset will not be processed.\n"
      "                   * Dataset must be byte-valued (8 bits: 0..255);\n"
      "                     shorts (16 bits) are also acceptable, but only\n"
      "                     values from 1.255 will be processed.\n"
      "                   * Each distinct value from 1..255 will be processed\n"
      "                     separately, and the output image will be ordered\n"
      "                     with the mask=1 voxels on top, mask=2 voxels next,\n"
      "                     and so on down the image.\n"
      "                   * A partition (e.g., mask=3) with fewer than 9 voxels\n"
      "                     will not be processed at all.\n"
      "                   * If there is more than one partition, horizontal dashed\n"
      "                     lines will drawn between them.\n"
      "\n"
      " -input dataset  = Alternative way to input dataset to process.\n"
      "\n"
      " -prefix ppp.png = Name for output file.\n"
      "                   * Default is Grayplot.png\n"
      "                   * If the filename ends in '.jpg', a JPEG file is output.\n"
      "                   * If the filename ends in '.pgm', a PGM file is output.\n"
      "                   * If the filename does not end in '.jpg' OR in '.png'\n"
      "                     OR in '.pgm', then '.png' will be added at the end.\n"
      "\n"
      " -dimen X Y      = Output size of image in pixels.\n"
      "                   * X = width  = time axis direction\n"
      "                   * Y = height = space dimensions\n"
      "                   * Defaults are X=1024 Y=512.\n"
      "\n"
      " -polort p       = Order of polynomials for detrending.\n"
      "                   * Default value is 2 (mean, slope, quadratic curve).\n"
      "                   * Use '-1' if data is already detrended and de-meaned.\n"
      "                     (e.g., is an AFNI errts.* file)\n"
      "\n"
      " -fwhm f         = FWHM of blurring radius to use in the dataset before\n"
      "                   making the image.\n"
      "                   * Each partition (i.e., mask=1, mask=2, ...) is blurred\n"
      "                     independently, as in program 3dBlurInMask.\n"
      "                   * Default value is 6 mm.\n"
      "                   * '-fwhm 0' will prevent blurring\n"
      "                     (e.g., if the input dataset is already blurred).\n"
      "                   * If the dataset was NOT previously blurred, a little\n"
      "                     blurring here will help bring out larger scale\n"
      "                     features in the times series.\n"
      "\n"
      " -pvorder        = Within each mask partition, order the voxels (top to\n"
      "                   bottom) by how well they match the two leading principal\n"
      "                   components of that partition. The result is to make the\n"
      "                   top part of each partition be made up of voxels with\n"
      "                   similar time series, and the bottom part will be more\n"
      "                   'random looking'.\n"
      "                   * The default order of voxels is just the index\n"
      "                     order in which they appear in the dataset.\n"
      "\n"
      " -peelorder      = Within each mask partition, order the voxels by how\n"
      "                   many 'peel' steps are needed to get from the partition\n"
      "                   boundary to a given voxel.\n"
      "\n"
      " -ijkorder       = Set the intra-partition ordering to the default, by\n"
      "                   dataset 3D index ('ijk').\n"
      "                   In AFNI's +tlrc ordering, this ordering primarily will be\n"
      "                   from Inferior to Superior in the brain (from top to bottom\n"
      "                   in the grayplot image).\n"
      "\n"
      "** Quick hack for Cesar Caballero-Gaudes, April 2019, by @AFNIman.\n"
      "   As such, this program may be modified in the future to be more useful,\n"
      "   or at least more beautifully gorgeous.\n"
      "\n"
      "** Applied to 'raw' EPI data, the results may not be very informative.\n"
      "   It seems to be more useful to look at the grayplot calculated from\n"
      "   pre-processed data (e.g., time series registered, filtered, etc.).\n"
      "\n"
      "** See also the script @grayplot, which can process the results from\n"
      "   afni_proc.py and produce an image with the grayplot combined with\n"
      "   a graph of the motion magnitude, and with the GM, WM, and CSF in\n"
      "   different partitions.\n"
      "\n"
     ) ;
     exit(0) ;
   }

   /*----------------------------------------------------------------*/

   mainENTRY("3dGrayplot"); machdep(); (void)COX_clock_time();

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-dimen") == 0 ){
       if( ++iarg >= argc-1 )
         ERROR_exit("'-dimen' needs 2 arguments") ;
       nxout = (int)strtod(argv[iarg++],NULL) ;
       nyout = (int)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     if( strcasecmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-polort' needs an argument") ;
       polort = (int)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     if( strcasecmp(argv[iarg],"-pvorder") == 0 ){   /* 04 May 2018 */
       grayplot_order_by_pvmap(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-peelorder") == 0 ){ /* 08 May 2018 */
       grayplot_order_by_peels(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-ijkorder") == 0 ){  /* 09 May 2018 */
       grayplot_order_by_ijk(1) ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-fwhm") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-fwhm' needs an argument") ;
       fwhm = (float)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-prefix' needs an argument") ;
       if( STRING_HAS_SUFFIX(argv[iarg],".png") ||
           STRING_HAS_SUFFIX(argv[iarg],".pgm") ||
           STRING_HAS_SUFFIX(argv[iarg],".jpg")   ){
         prefix = strdup(argv[iarg]) ;
       } else {
         prefix = malloc(sizeof(char)*(strlen(argv[iarg])+16)) ;
         sprintf(prefix,"%s.png",argv[iarg]) ;
       }
       if( !THD_filename_ok(prefix) )
         ERROR_exit("prefix '%s' is illegal :(",prefix) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nvox = DSET_NVOX(mset) ;
       if( DSET_BRICK_TYPE(mset,0) != MRI_byte  &&
           DSET_BRICK_TYPE(mset,0) != MRI_short   )
         ERROR_exit("-mask dataset is NOT byte- or short-valued :(") ;

       if( DSET_BRICK_TYPE(mset,0) == MRI_byte ){
         mask = DSET_BRICK_ARRAY(mset,0) ;
       } else {
         short *smm = DSET_BRICK_ARRAY(mset,0) ;
         mask = (byte *)calloc(sizeof(byte),mask_nvox) ;
         EDIT_coerce_type( mask_nvox , MRI_short,smm , MRI_byte,mask ) ;
         DSET_unload(mset) ;
       }
 
       mmm = THD_countmask( mask_nvox , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 19 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       if( dset != NULL ) ERROR_exit("Can't have two -input options") ;
       dset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(dset,argv[iarg]) ;
       INFO_message("Loading dataset") ;
       DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( mask == NULL ) ERROR_exit("Need '-mask'") ;

   /*----------------------------------------------------------------*/

   if( dset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset?") ;
     dset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(dset,argv[iarg]) ;
     INFO_message("Loading dataset") ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   }

   if( mask_nvox != DSET_NVOX(dset) )
     ERROR_exit("mask and dataset voxel counts don't match :(") ;

   INFO_message("Grayplot-ing dataset") ;
   imout = THD_dset_to_grayplot( dset,mask , nxout,nyout , polort,fwhm ) ;

   mri_write_png( prefix , imout ) ;

   INFO_message("3dGrayplot: Elapsed = %.1f s\n", COX_clock_time() ) ;

   exit(0) ;
}
