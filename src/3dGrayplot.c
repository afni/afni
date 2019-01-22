#include "mrilib.h"

#include "thd_dset_to_grayplot.c"
void show_help(void) ;

#define IS_NUMERIC(sss)                                           \
 ( (                                       isdigit((sss)[0]) ) || \
   ( (sss)[0] == '.'                    && isdigit((sss)[1]) ) || \
   ( (sss)[0] == '-'                    && isdigit((sss)[1]) ) || \
   ( (sss)[0] == '-' && (sss)[1] == '.' && isdigit((sss)[2]) )   )

/*===========================================================================*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL ;
   MRI_IMAGE *imout ;
   char *prefix = "Grayplot.png" ;
   byte *mask=NULL ; int mask_nvox=0 ;
   int iarg = 1 ;
   int polort=2 , nxout=0,nyout=0 ; float fwhm=0.0f ;

   /*----------------------------------------------------------------*/

   if( argc < 2 ){
     show_help() ;
     exit(0) ;
   }

   /*----------------------------------------------------------------*/

   mainENTRY("3dGrayplot"); machdep(); (void)COX_clock_time();
   if( strcasecmp(argv[1],"-help") != 0 ){
     PRINT_VERSION("3dGrayplot") ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcasecmp(argv[iarg],"-help") == 0 ){
       show_help() ;
       exit(0) ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-dimen") == 0 ){
       if( ++iarg >= argc-1 )
         ERROR_exit("'-dimen' needs 2 arguments") ;
       nxout = (int)strtod(argv[iarg++],NULL) ;
       nyout = (int)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-polort' needs an argument") ;
       polort = (int)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-pvorder") == 0 ){   /* 04 May 2018 */
       grayplot_order_by_pvmap(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-peelorder") == 0 ){ /* 08 May 2018 */
       grayplot_order_by_peels(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-ijkorder") == 0 ){  /* 09 May 2018 */
       grayplot_order_by_ijk(1) ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncasecmp(argv[iarg],"-oldresam",7) == 0 ||
         strncasecmp(argv[iarg],"-resamold",7) == 0    ){  /* 14 Aug 2018 */
       ININFO_message("Using old (deprecated) resampling method") ;
       grayplot_set_use_old_resam() ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-fwhm") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-fwhm' needs an argument") ;
       fwhm = (float)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     /*-----*/

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

     /*-----*/

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
       ININFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 19 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       if( dset != NULL ) ERROR_exit("Can't have two -input options") ;
       dset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(dset,argv[iarg]) ;
       if( DSET_NVALS(dset) < 9 )
         ERROR_exit("Dataset %s has too few time points",DSET_HEADNAME(dset)) ;
       ININFO_message("Loading dataset %s",DSET_HEADNAME(dset)) ;
       DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-percent") == 0 ){  /* 30 Jul 2018 */
       grayplot_set_percent() ;
       grayplot_norming_none() ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[iarg],"-range") == 0 ){
       if( ++iarg < argc && IS_NUMERIC(argv[iarg]) ){
         float val = (float)strtod(argv[iarg],NULL) ;
         if( val > 0.0f ) grayplot_set_range(val) ;
         iarg++ ;
       }
       continue ;
     }

     /*-----*/

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   /*----------------------------------------------------------------*/

   /*----- no dataset given yet? -----*/

   if( dset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset?") ;
     dset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(dset,argv[iarg]) ;
     if( DSET_NVALS(dset) < 9 )
       ERROR_exit("Dataset %s has too few time points",DSET_HEADNAME(dset)) ;
     ININFO_message("Loading dataset %s",DSET_HEADNAME(dset)) ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   }

   /*----- create default mask -----*/

   if( mask == NULL ){
     int ii,jj,kk , nx,ny,nz,nxy , nx1,ny1,nz1 , vv ;
     ININFO_message("No mask input ==> using all interior voxels") ;
     mask_nvox = DSET_NVOX(dset) ;
     mask = (byte *)malloc(sizeof(byte)*mask_nvox) ;
     nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxy = nx*ny;
     nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ;
     if( nx1 == 0 || ny1 == 0 || nz1 == 0 )
       ERROR_exit("Cannot use automask on dataset with only one slice :(") ;
     for( vv=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,vv++){
         mask[vv] = (ii==0 || jj==0 || kk==0 || ii==nx1 || jj==ny1 || kk==nz1)
                    ? 1 : 0 ;
     }}}
     vv = THD_countmask( mask_nvox , mask ) ;
     ININFO_message("Number of voxels in mask = %d",vv) ;
   } else if( mask_nvox != DSET_NVOX(dset) ){
     ERROR_exit("mask and dataset voxel counts don't match :(") ;
   }

   /*------- do all the work -------*/

   imout = THD_dset_to_grayplot( dset,mask , nxout,nyout , polort,fwhm ) ;

   mri_write_png( prefix , imout ) ;

   ININFO_message("3dGrayplot: Elapsed = %.1f s\n", COX_clock_time() ) ;

   exit(0) ;
}

/*===========================================================================*/

void show_help(void)
{
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
      " -mask mset      = Name of mask dataset\n"
      "                    * Voxels that are 0 in mset will not be processed.\n"
      "                    * Dataset must be byte-valued (8 bits: 0..255);\n"
      "                      shorts (16 bits) are also acceptable, but only\n"
      "                      values from 1.255 will be processed.\n"
      "                    * Each distinct value from 1..255 will be processed\n"
      "                      separately, and the output image will be ordered\n"
      "                      with the mask=1 voxels on top, mask=2 voxels next,\n"
      "                      and so on down the image.\n"
      "                    * A partition (e.g., mask=3) with fewer than 9 voxels\n"
      "                      will not be processed at all.\n"
      "                    * If there is more than one partition, horizontal dashed\n"
      "                      lines will drawn between them.\n"
      "                    * If '-mask' is not given, then all voxels will be used,\n"
      "                      except those at the very edge of a volume.\n"
      "\n"
      " -input dataset  = Alternative way to input dataset to process.\n"
      "\n"
      " -prefix ppp.png = Name for output file.\n"
      "                    * Default is Grayplot.png\n"
      "                    * If the filename ends in '.jpg', a JPEG file is output.\n"
      "                    * If the filename ends in '.pgm', a PGM file is output.\n"
      "                        [PGM files can be manipulated with the NETPBM package.]\n"
      "                    * If the filename does not end in '.jpg' OR in '.png'\n"
      "                      OR in '.pgm', then '.png' will be added at the end.\n"
      "\n"
      " -dimen X Y      = Output size of image in pixels.\n"
      "                    * X = width  = time axis direction\n"
      "                    * Y = height = voxel/space dimensions\n"
      "                    * Defaults are X=1024 Y=512 -- suitable for screen display.\n"
      "                    * For publication, you might want more pixels, as in\n"
      "                        -dimen 1800 1200\n"
      "                      which would be 6 inches wide by 4 inches high, at the usual\n"
      "                      300 dots-per-inch (dpi) of high resolution image printing.\n"
      "                   ** Note that there are usually many more voxels in the Y direction\n"
      "                      than there are pixels in the output image. This fact requires\n"
      "                      coarsening the output grid and resampling the data to match.\n"
      "                      See the next option for a little more information.\n"
      "\n"
      " -oldresam       = The method for resampling the processed dataset to the final\n"
      "                   grayscale image size was changed in a major way.\n"
      "                   If you want to use the original method, then give this option.\n"
      "                    * The only reason for using this option is for\n"
      "                      comparison with the new method.\n"
      "                    * The new resampling method was added 15 Aug 2018, and it\n"
      "                      uses minimum-sidelobe local averaging when coarsening\n"
      "                      the grid (usually vertical direction = voxels/space)\n"
      "                      -- whose purpose is to reduce aliasing artifacts --\n"
      "                      and uses cubic interpolation when refining the grid\n"
      "                      (usually horizontal direction = time) -- whose purpose\n"
      "                      is purely beautification.\n"
      "                    * Note that the collapsing of multiple voxels into one\n"
      "                      pixel will tend to cancel out signals that change sign\n"
      "                      within neighbors in the voxel ordering method you choose.\n"
      "                      (See the 'order' options below.)\n"
      "\n"
      " -polort p       = Order of polynomials for detrending.\n"
      "                    * Default value is 2 (mean, slope, quadratic curve).\n"
      "                    * Use '-1' if data is already detrended and de-meaned.\n"
      "                      (e.g., is an AFNI errts.* file)\n"
      "\n"
      " -fwhm f         = FWHM of blurring radius to use in the dataset before\n"
      "                   making the image.\n"
      "                    * Each partition (i.e., mask=1, mask=2, ...) is blurred\n"
      "                      independently, as in program 3dBlurInMask.\n"
      "                    * Default value is 0 mm = no blurring.\n"
      "                        [In the past, the default value was 6.]\n"
      "                    * If the dataset was NOT previously blurred, a little\n"
      "                      spatial blurring here will help bring out larger scale\n"
      "                      features in the times series, which might otherwise\n"
      "                      look very noisy.\n"
      "\n"
      " -pvorder        = Within each mask partition, order the voxels (top to\n"
      "                   bottom) by how well they match the two leading principal\n"
      "                   components of that partition. The result is to make the\n"
      "                   top part of each partition be made up of voxels with\n"
      "                   similar time series, and the bottom part will be more\n"
      "                   'random looking'.\n"
      "\n"
      " -peelorder      = Within each mask partition, order the voxels by how\n"
      "                   many 'peel' steps are needed to get from the partition\n"
      "                   boundary to a given voxel.\n"
      "                    * This ordering puts voxels in 'similar' geometrical\n"
      "                      positions sort-of close together in the image.\n"
      "\n"
      " -ijkorder       = Set the intra-partition ordering to the default, by\n"
      "                   dataset 3D index ('ijk').\n"
      "                    * In AFNI's +tlrc ordering, this ordering primarily will\n"
      "                      be from Inferior to Superior in the brain (from top to\n"
      "                      bottom in the grayplot image).\n"
      "                    * This is the default ordering method, but perhaps it is\n"
      "                      not the most useful.\n"
      "\n"
      " -range X        = Set the range of the data to be plotted to be 'X'.\n"
      "                   When this option is used, then:\n"
      "                    * a value of 0 will be plotted as middle-gray\n"
      "                    * a value of +X (or above) will be plotted as white\n"
      "                    * a value of -X (or below) will be plotted as black\n"
      "                   Thus, this option should be used with data that is centered\n"
      "                   around zero -- or will be so after '-polort' detrending.\n"
      "                    * For example, if you are applying this option to an\n"
      "                      afni_proc.py 'errts' (residuals) dataset, a good value\n"
      "                      of X to use is 3 or 4, since those values are in percents.\n"
      "                    * The @grayplot script uses '-range 3.89' since that is the\n"
      "                      value at which a standard normal N(0,1) deviate has a 1e-4\n"
      "                      two-sided tail probability. (If nothing else, this sounds cool.)\n"
      "                   If you do NOT use '-range', then the data will be automatically\n"
      "                   normalized so each voxel time series has RMS value 1, and then\n"
      "                   the grayscale plot will be black-to-white being the min-to-max,\n"
      "                   where the min and max computed over the entire detrended\n"
      "                   and normalized dataset.\n"
      "                    * This default automatic normalizing and scaling makes it\n"
      "                      almost impossible to directly compare grayplots from\n"
      "                      different datasets. This difficulty is why the '-range'\n"
      "                      and '-percent' options were added [30 Jul 2018].\n"
      "\n"
      " -percent        = Use this option on 'raw' time series datasets, to compute\n"
      "                   the mean of each voxel timeseries and then use that value\n"
      "                   to scale the values to percent differences from the mean.\n"
      "                    * NOT suitable for use with a residual 'errts' dataset!\n"
      "                    * Should be combined with '-range'.\n"
      "                    * Detrending will be applied while calculating the mean.\n"
      "                      By default, that will be quadratic detrending of each\n"
      "                      voxel time series, but that can be changed with the\n"
      "                      '-polort' option.\n"
      "\n"
      "** Quick hack for Cesar Caballero-Gaudes, April 2018, by @AFNIman.\n"
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
      "--------\n"
      "EXAMPLE:\n"
      "--------\n"
      "The following commands first generate a time series dataset,\n"
      "then create grayplots using each of the ordering methods\n"
      "(so you can compare them). No mask is given.\n"
      "\n"
      " 3dcalc -a jRandomDataset:64:64:30:256 -datum float \\\n"
      "        -prefix Qsc.nii -expr 'abs(.3+cos(0.1*i))*sin(0.1*t+0.1*i)+gran(0,3)'\n"
      " 3dGrayplot -pvorder   -prefix QscPV.png   -input Qsc.nii -fwhm 8\n"
      " 3dGrayplot -ijkorder  -prefix QscIJK.png  -input Qsc.nii -fwhm 8\n"
      " 3dGrayplot -peelorder -prefix QscPEEL.png -input Qsc.nii -fwhm 8\n"
      "\n"
      "\n"
     ) ;
}
