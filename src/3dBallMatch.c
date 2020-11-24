#include "mrilib.h"

/*---------------------------------------------------------------------*/
/* This program is in the publick domaine and so you can use/abuse it  */
/* for anything you can imagine. Get to it - think of something weird! */
/*---------------------------------------------------------------------*/

#include "thd_ballcorr.c" /* in which all the work is done */

/*----------------------------------------------------------------------------*/

void Bmatch_help(void) ;  /* prototype for the .... help */

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL ;
   float arad=72.0f , brad=0.0f ;
   int_triple ijkout ;
   THD_ivec3 ijk_vec ; THD_fvec3 xyzshift_vec , xyzdicom_vec ;

   float_triple avec , bvec ;
   float dtheta=(PI/180.0)*15.0 ; int ntheta=2 , iarg ;

   /* help help help */

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     Bmatch_help() ; exit(0) ;
   }

   /* check inputs */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( dset   != NULL ) ERROR_exit("Can't use %s more than once!" ,argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("Option '%s' needs an argument",argv[iarg-1]) ;
       dset = THD_open_dataset( argv[iarg] ) ;
       if( !ISVALID_DSET(dset) ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-ball") == 0 || strcasecmp(argv[iarg],"-sphere") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Option '%s' needs an argument",argv[iarg-1]) ;
       arad = (float)strtod(argv[iarg],NULL) ;
       if( arad <= 7.0f )   ERROR_exit("Option '%s %s' should be a positive radius",
                                       argv[iarg-1] , argv[iarg] ) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-spheroid") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("Option '%s' needs 2 arguments",argv[iarg-1]) ;
       arad = (float)strtod(argv[iarg],NULL) ; iarg++ ;
       brad = (float)strtod(argv[iarg],NULL) ; iarg++ ;

       if( arad <= 7.0f )   ERROR_exit("Option '%s %s' should be a positive radius",
                                       argv[iarg-2] , argv[iarg-1] ) ;
       continue ;
     }

     /* why do I have to put up with this crapola any more? */

     ERROR_message("Unknown option %s",argv[iarg]) ; iarg++ ; continue ;
   }

   /* see if the user actually did something right for a change */

   if( dset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset?") ;
     dset = THD_open_dataset( argv[iarg] ) ;
     if( !ISVALID_DSET(dset) ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ; iarg++ ;
   }

   /* do we have a really sophisticated user? */

   if( iarg < argc && isdigit(argv[iarg][0]) ){
     arad = brad = (float)strtod(argv[iarg],NULL) ;
     if( arad < 7.0f ) ERROR_exit("ball radius %g is too small",arad) ;
   }

   /* do all the REAL MANLY work */

   if( brad <= 0.0f || brad == arad ){

     ijkout = THD_ball_mask_overlapation( dset , arad ) ;

   } else {
     THD_fvec3 av0,av1,tvv ;

     LOAD_FVEC3(tvv,0.0f,0.0f,0.0f) ;
     av0 = THD_dicomm_to_3dmm( dset , tvv) ;
     LOAD_FVEC3(tvv,0.0f,1.0f,0.0f) ;
     av1 = THD_dicomm_to_3dmm( dset , tvv) ;
     avec.a = av1.xyz[0] - av0.xyz[0] ;
     avec.b = av1.xyz[1] - av0.xyz[1] ;
     avec.c = av1.xyz[2] - av0.xyz[2] ;

     LOAD_FVEC3(tvv,1.0f,0.0f,0.0f) ;
     av1 = THD_dicomm_to_3dmm( dset , tvv) ;
     bvec.a = av1.xyz[0] - av0.xyz[0] ;
     bvec.b = av1.xyz[1] - av0.xyz[1] ;
     bvec.c = av1.xyz[2] - av0.xyz[2] ;

     ijkout = THD_spheroid_overlapation( dset ,
                                         arad , avec ,
                                         brad , bvec ,
                                         dtheta , -ntheta , ntheta ) ;
   }

   DSET_unload(dset) ; /* don't need the data no more */

   /* compute and print the output triples */

   LOAD_IVEC3( ijk_vec , ijkout.i , ijkout.j , ijkout.k ) ; /* index coords */

   xyzshift_vec = THD_3dind_to_3dmm( dset , ijk_vec ) ;  /* personal coords */

   xyzdicom_vec = THD_3dmm_to_dicomm( dset , xyzshift_vec ) ;      /* DICOM */

   printf(" %d %d %d",ijkout.i,ijkout.j,ijkout.k) ;
   printf(" %.4f %.4f %.4f" ,
          -xyzshift_vec.xyz[0] , -xyzshift_vec.xyz[1] , -xyzshift_vec.xyz[2] ) ;
   printf(" %.4f %.4f %.4f" ,
           xyzdicom_vec.xyz[0] ,  xyzdicom_vec.xyz[1] ,  xyzdicom_vec.xyz[2] ) ;
   printf("\n") ;

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

void Bmatch_help(void)
{
   printf("\n"
    "--------------------------------------\n"
    "Usage #1:  3dBallMatch dataset [radius]\n"
    "--------------------------------------\n"
    "\n"
    "-----------------------------------------------------------------------\n"
    "Usage #2:  3dBallMatch [options]\n"
    "\n"
    "where the pitifully few options are:\n"
    "\n"
    " -input dataset  =  read this dataset\n"
    "\n"
    " -ball  radius   =  set the radius of the 3D ball to match (mm)\n"
    "\n"
    " -spheroid a b   =  match with a spheroid of revolution, with principal\n"
    "                    axis radius of 'a' and secondary axes radii 'b'\n"
    "                    ++ this option is considerably slower\n"
    "-----------------------------------------------------------------------\n"
    "\n"
    "-------------------\n"
    "WHAT IT IS GOOD FOR\n"
    "-------------------\n"
    "* This program tries to find a good match between a ball (filled sphere)\n"
    "  of the given radius (in mm) and a dataset. The goal is to find a crude\n"
    "  approximate center of the brain quickly.\n"
    "\n"
    "* The output can be used to re-center a dataset so that its coordinate\n"
    "  origin is inside the brain and/or as a starting point for more refined\n"
    "  3D alignment. Sample scripts are given below.\n"
    "\n"
    "* The reason for this program is that not all brain images are even\n"
    "  crudely centered by using the center-of-mass ('3dAllineate -cmass')\n"
    "  as a starting point -- if the volume covered by the image includes\n"
    "  a lot of neck or even shoulders, then the center-of-mass may be\n"
    "  far from the brain.\n"
    "\n"
    "* If you don't give a radius, the default is 72 mm, which is about the\n"
    "  radius of an adult human brain/cranium. A larger value would be needed\n"
    "  for elephant brain images. A smaller value for marmosets.\n"
    "\n"
    "* For advanced use, you could try a prolate spheroid, using something like\n"
    "     3dBallMatch -input Fred.nii -spheroid 90 70\n"
    "  for a human head image (that was not skull stripped). This option is\n"
    "  several times slower than the 'ball' option, as multiple spheroids have\n"
    "  to be correlated with the input dataset.\n"
    "\n"
    "* This program does NOT work well with datasets containing large amounts\n"
    "  of negative values or background junk -- such as I've seen with animal\n"
    "  MRI scans and CT scans. Such datasets will likely require some repair\n"
    "  first, such as cropping (cf. 3dZeropad), to make this program useful.\n"
    "\n"
    "* Frankly, this program may not be that useful for any purpose :(\n"
    "\n"
    "* The output is text to stdout containing 3 triples of numbers, all on\n"
    "  one line:\n"
    "    i j k xs ys zs xd yd zd\n"
    "  where\n"
    "    i j k    = index triple of the central voxel\n"
    "    xs ys zs = values to use in '3drefit -dxxorigin' (etc.)\n"
    "               to make (i,j,k) be at coordinates (x,y,z)=(0,0,0)\n"
    "    xd yd zd = DICOM-order (x,y,z) coordinates of (i,j,k) in the\n"
    "               input dataset\n"
    "\n"
    "* The intention is that this output line be captured and then the\n"
    "  appropriate pieces be used for some higher purpose.\n"

    "\n"
    "--------------------------------------------------------------\n"
    "SAMPLE SCRIPT - VISUALIZING THE MATCHED LOCATION  (csh syntax)\n"
    "--------------------------------------------------------------\n"
    "Below is a script to process all the entries in a directory.\n"
    "\n"

    "#!/bin/tcsh\n"
    "\n"
    "# optional: start a virtual X11 server\n"
    "  set xdisplay = `count -dig 1 3 999 R1`\n"
    "  echo \" -- trying to start Xvfb :${xdisplay}\"\n"
    "  Xvfb :${xdisplay} -screen 0 1024x768x24 >& /dev/null &\n"
    "  sleep 1\n"
    "  set display_old = $DISPLAY\n"
    "  setenv DISPLAY :${xdisplay}\n"
    "\n"
    "# loop over all subjects\n"
    "  foreach sss ( sub-?????_T1w.nii.gz )\n"
    "\n"
    "# extract subject ID code\n"
    "    set sub = `echo $sss | sed -e 's/sub-//' -e 's/_T1w.nii.gz//'`\n"
    "\n"
    "# skip if already finished\n"
    "    if ( -f $sub.match   ) continue\n"
    "    if ( -f $sub.sag.jpg ) continue\n"
    "    if ( -f $sub.cor.jpg ) continue\n"
    "\n"
    "# run the program, save output to a file\n"
    "    3dBallMatch $sss > $sub.match\n"
    "\n"
    "# capture the output for use below\n"
    "    set ijk = ( `cat $sub.match` )\n"
    "    echo $sub $ijk\n"
    "\n"
    "# run afni to make some QC images\n"
    "    afni -DAFNI_NOSPLASH=YES                            \\\n"
    "         -DAFNI_NOPLUGINS=YES                           \\\n"
    "         -com \"OPEN_WINDOW A.sagittalimage\"             \\\n"
    "         -com \"OPEN_WINDOW A.coronalimage\"              \\\n"
    "         -com \"SET_IJK $ijk[1-3]\"                       \\\n"
    "         -com \"SAVE_JPEG A.sagittalimage $sub.sag.jpg\"  \\\n"
    "         -com \"SAVE_JPEG A.coronalimage $sub.cor.jpg\"   \\\n"
    "         -com \"QUITT\"                                   \\\n"
    "         $sss\n"
    "\n"
    "# end of loop over subject\n"
    "  end\n"
    "\n"
    "# kill the virtual X11 server (if it was started above)\n"
    "  sleep 1\n"
    "  killall Xvfb\n"
    "\n"
    "# make a movie of the sagittal slices\n"
    "  im_to_mov -resize -prefix Bsag -npure 4 -nfade 0 *.sag.jpg\n"
    "# make a movie of the coronal slices\n"
    "  im_to_mov -resize -prefix Bcor -npure 4 -nfade 0 *.cor.jpg\n"
    "exit 0\n"

    "\n"
    "------------------------------------------------------------\n"
    "SAMPLE SCRIPT - IMPROVING THE MATCHED LOCATION  (csh syntax)\n"
    "------------------------------------------------------------\n"
    "This script is an extension of the one above, where it uses\n"
    "3dAllineate to align the human brain image to the MNI template,\n"
    "guided by the initial point computed by 3dBallMatch. The output\n"
    "of 3dAllineate is the coordinate of the center of the original\n"
    "volume, in the first 3 values stored in '*Aparam.1D' file.\n"
    " * Note that the 3dAllineate step presumes that the input\n"
    "   dataset is a T1-weighted volume. A different set of options would\n"
    "   have to be used for an EPI (T2*-weighted) or T2-weighted volume.\n"
    " * This script worked pretty well for putting the crosshairs at\n"
    "   the 'origin' of the brain -- near the anterior commissure.\n"
    "   Of course, you will need to evaluate its performance yourself.\n"
    "\n"

    "#!/bin/tcsh\n"
    "\n"
    "# optional: start Xvfb to avoid the AFNI GUI starting visibly\n"
    "  set xdisplay = `count -dig 1 3 999 R1`\n"
    "  echo \" -- trying to start Xvfb :${xdisplay}\"\n"
    "  Xvfb :${xdisplay} -screen 0 1024x768x24 >& /dev/null &\n"
    "  sleep 1\n"
    "  set display_old = $DISPLAY\n"
    "  setenv DISPLAY :${xdisplay}\n"
    "\n"
    "# loop over datasets in the current directory\n"
    "  foreach sss ( anat_sub?????.nii.gz )\n"
    "\n"
    "# extract the subject identfier code (the '?????')\n"
    "    set sub = `echo $sss | sed -e 's/anat_sub//' -e 's/.nii.gz//'`\n"
    "\n"
    "# if 3dAllineate was already run on this, skip to next dataset\n"
    "    if ( -f $sub.Aparam.1D ) continue\n"
    "\n"
    "# find the 'center' voxel location with 3dBallMatch\n"
    "    if ( ! -f $sub.match ) then\n"
    "      echo \"Running 3dBallMatch $sss\"\n"
    "      3dBallMatch $sss | tee $sub.match\n"
    "    endif\n"
    "\n"
    "# extract results from 3dBallMatch output\n"
    "# in this case, we want the final triplet of coordinates\n"
    "    set ijk = ( `cat $sub.match` )\n"
    "# set shift range to be 55 mm about 3dBallMatch coordinates\n"
    "    set  xd = $ijk[7] ; set xbot = `ccalc \"${xd}-55\"` ; set xtop = `ccalc \"${xd}+55\"`\n"
    "    set  yd = $ijk[8] ; set ybot = `ccalc \"${yd}-55\"` ; set ytop = `ccalc \"${yd}+55\"`\n"
    "    set  zd = $ijk[9] ; set zbot = `ccalc \"${zd}-55\"` ; set ztop = `ccalc \"${zd}+55\"`\n"
    "\n"
    "# Align the brain image volume with 3dAllineate:\n"
    "#  match to 'skull on' part of MNI template = sub-brick [1]\n"
    "#  only save the parameters, not the final aligned dataset\n"
    "    3dAllineate                                          \\\n"
    "      -base ~/abin/MNI152_2009_template_SSW.nii.gz'[1]'  \\\n"
    "      -source $sss                                       \\\n"
    "      -parang 1 $xbot $xtop                              \\\n"
    "      -parang 2 $ybot $ytop                              \\\n"
    "      -parang 3 $zbot $ztop                              \\\n"
    "      -prefix NULL -lpa                                  \\\n"
    "      -1Dparam_save $sub.Aparam.1D                       \\\n"
    "      -conv 3.666 -fineblur 3 -num_rtb 0 -norefinal -verb\n"
    "\n"
    "# 1dcat (instead of cat) to strip off the comments at the top of the file\n"
    "# the first 3 values in 'param' are the (x,y,z) shifts\n"
    "# Those values could be used in 3drefit to re-center the dataset\n"
    "    set param = ( `1dcat $sub.Aparam.1D` )\n"
    "\n"
    "# run AFNI to produce the snapshots with crosshairs at\n"
    "# the 3dBallMatch center and the 3dAllineate center\n"
    "# - B.*.jpg = 3dBallMatch result in crosshairs\n"
    "# - A.*.jpg = 3dAllineate result in crosshairs\n"
    "    afni -DAFNI_NOSPLASH=YES                             \\\n"
    "         -DAFNI_NOPLUGINS=YES                            \\\n"
    "         -com \"OPEN_WINDOW A.sagittalimage\"              \\\n"
    "         -com \"SET_IJK $ijk[1-3]\"                        \\\n"
    "         -com \"SAVE_JPEG A.sagittalimage B.$sub.sag.jpg\"  \\\n"
    "         -com \"SET_DICOM_XYZ $param[1-3]\"                \\\n"
    "         -com \"SAVE_JPEG A.sagittalimage A.$sub.sag.jpg\" \\\n"
    "         -com \"QUITT\"                                    \\\n"
    "         $sss\n"
    "\n"
    "# End of loop over datasets\n"
    "  end\n"
    "\n"
    "# stop Xvfb (only needed if it was started above)\n"
    "  sleep 1\n"
    "  killall Xvfb\n"
    "\n"
    "# make movies from the resulting images\n"
    "  im_to_mov -resize -prefix Bsag -npure 4 -nfade 0 B.[1-9]*.sag.jpg\n"
    "  im_to_mov -resize -prefix Asag -npure 4 -nfade 0 A.[1-9]*.sag.jpg\n"
    "exit 0\n"

    "\n"
    "----------------------------\n"
    "HOW IT WORKS (approximately)\n"
    "----------------------------\n"
    "1] Create the automask of the input dataset (as in 3dAutomask).\n"
    "   + This is a 0/1 binary marking of outside/inside voxels.\n"
    "   + Then convert it to a -1/+1 mask instead.\n"
    "\n"
    "2] Create a -1/+1 mask for the ball [-1=outside, +1=inside],\n"
    "   inside a rectangular box.\n"
    "\n"
    "3] Convolve these 2 masks (using FFTs for speed).\n"
    "   + Basically, this is moving the ball around, then adding up\n"
    "     the voxel counts where the masks match sign (both positive\n"
    "     means ball and dataset are both 'inside'; both negative\n"
    "     means ball and dataset are both 'outside'), and subtracting\n"
    "     off the voxel counts where the mask differ in sign\n"
    "     (one is 'inside' and one is 'outside' == not matched).\n"
    "   + That is, the convolution value is the sum of matched voxels\n"
    "     minus the sum of mismatched voxels, at every location of\n"
    "     offset (i,j,k) of the corner of the ball mask.\n"
    "   + The ball mask is in a cube of side 2*radius, which has volume\n"
    "     8*radius^3. The volume of the ball is 4*pi/3*radius^3, so the\n"
    "     inside of the ball is about 4*pi/(3*8) = 52%% of the volume of the cube\n"
    "     -- that is, inside and outside voxels are (roughly) matched, so they\n"
    "     have (approximately) equal weight.\n"
    "   + Most of the CPU time is in the 3D FFTs required.\n"
    "\n"
    "4] Find the centroid of the locations where the convolution\n"
    "   is positive (matches win over non-matches) and at least 5%%\n"
    "   of the maximum convolution. This centroid gives (i,j,k).\n"
    "\n"
    "Why the centroid? I found that the peak convolution location\n"
    "is not very stable, as a lot of locations have results barely less\n"
    "than the peak value -- it was more stable to average them together.\n"
    "\n"
    "------------------------\n"
    "WHY 'ball' NOT 'sphere'?\n"
    "------------------------\n"
    " * Because a 'sphere' is a 2D object, the surface of the 3D object 'ball'.\n"
    " * Because my training was in mathematics, where precise terminology has\n"
    "   been developed and honed for centuries.\n"
    " * Because I'm yanking your chain. Any other questions? No? Good.\n"
    "\n"
    "-------\n"
    "CREDITS\n"
    "-------\n"
    "By RWCox, September 2020 (the year it all fell apart).\n"
    "Delenda est. Never forget.\n"
    "\n"
   ) ;

   return ;
}
