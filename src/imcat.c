/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

#define XYNUM    0
#define YXNUM    1
#define XDOTYNUM 2
#define YDOTXNUM 3
void imcat_usage(int detail) 
{
         printf(
 "Usage: imcat [options] fname1 fname2 etc.\n"
 "Puts a set images into an image matrix (IM) \n"
 " montage of NX by NY images.\n"
 " The minimum set of input is N images (N >= 1).\n"
 " If need be, the default is to reuse images until the desired\n"
 " NX by NY size is achieved. \n"
 " See options -zero_wrap and -image_wrap for more detail.\n"
 " \n"
 "OPTIONS:\n"
 " ++ Options for editing, coloring input images:\n"
 "  -scale_image SCALE_IMG: Multiply each image IM(i,j) in output\n"
 "                          image matrix IM by the color or intensity\n"
 "                          of the pixel (i,j) in SCALE_IMG.\n"
 "  -scale_pixels SCALE_PIX: Multiply each pixel (i,j) in output image\n"
 "                          by the color or intensity\n"
 "                          of the pixel (i,j) in SCALE_IMG.\n"
 "                          SCALE_IMG is automatically resized to the\n"
 "                          resolution of the output image.\n"
 "  -scale_intensity: Instead of multiplying by the color of \n"
 "                          pixel (i,j), use its intensity \n"
 "                          (average color)\n"
 "  -gscale FAC: Apply FAC in addition to scaling of -scale_* options\n"
 "  -rgb_out: Force output to be in rgb, even if input is bytes.\n"
 "            This option is turned on automatically in certain cases.\n"
 "  -res_in RX RY: Set resolution of all input images to RX by RY pixels.\n"
 "                 Default is to make all input have the same\n"
 "                 resolution as the first image.\n"
 "  -respad_in RPX RPY: Like -res_in, but resample to the max while respecting\n"
 "                      the aspect ratio, and then pad to achieve desired \n"
 "                      pixel count.\n"
 "  -pad_val VAL: Set the padding value, should it be needed by -respad_in\n"
 "                to VAL. VAL is typecast to byte, default is 0, max is 255.\n"
 "  -crop L R T B: Crop images by L (Left), R (Right), T (Top), B (Bottom)\n"
 "                 pixels. Cutting is performed after any resolution change, \n"
 "                 if any, is to be done.\n"
 "  -autocrop_ctol CTOL: A line is eliminated if none of its R G B values \n"
 "                       differ by more than CTOL%% from those of the corner\n"
 "                       pixel.\n"
 "  -autocrop_atol ATOL: A line is eliminated if none of its R G B values \n"
 "                       differ by more than ATOL%% from those of line\n"
 "                       average.\n"
 "  -autocrop: This option is the same as using both of -autocrop_atol 20 \n"
 "             and -autocrop_ctol 20\n"
 "  NOTE: Do not mix -autocrop* options with -crop\n"
 "        Cropping is determined from the 1st input image and applied to \n"
 "        to all remaining ones.\n"
 " ++ Options for output:\n"
 "  -zero_wrap: If number of images is not enough to fill matrix\n"
 "              solid black images are used.\n"
 "  -white_wrap: If number of images is not enough to fill matrix\n"
 "              solid white images are used.\n"
 "  -gray_wrap GRAY: If number of images is not enough to fill matrix\n"
 "              solid gray images are used. GRAY must be between 0 and 1.0\n"
 "  -image_wrap: If number of images is not enough to fill matrix\n"
 "               images on command line are reused (default)\n"
 "  -rand_wrap: When reusing images to fill matrix, randomize the order\n"
 "              in refill section only.\n"
 "  -prefix ppp = Prefix the output files with string 'ppp'\n"
 "          Note: If the prefix ends with .1D, then a 1D file containing\n"
 "                the average of RGB values. You can view the output with\n"
 "                1dgrayplot.\n"
 "  -matrix NX NY: Specify number of images in each row and column \n"
 "                 of IM at the same time. \n"
 "  -nx NX: Number of images in each row (3 for example below)\n"
 "  -ny NY: Number of images in each column (4 for example below)\n"
 "      Example: If 12 images appearing on the command line\n"
 "               are to be assembled into a 3x4 IM matrix they\n"
 "               would appear in this order:\n"
 "                 0  1  2\n"
 "                 3  4  5\n"
 "                 6  7  8\n"
 "                 9  10 11\n"
 "    NOTE: The program will try to guess if neither NX nor NY \n"
 "          are specified.\n"
 "  -matrix_from_scale: Set NX and NY to be the same as the \n"
 "                      SCALE_IMG's dimensions. (needs -scale_image)\n"
 "  -gap G: Put a line G pixels wide between images.\n"
 "  -gap_col R G B: Set color of line to R G B values.\n"
 "                  Values range between 0 and 255.\n"
 "\n"
 "Example 0 (assuming afni is in ~/abin directory):\n"
 "   Resizing an image:\n"
 "   imcat -prefix big -res_in 1024 1024 \\\n"
 "         ~/abin/funstuff/face_zzzsunbrain.jpg \n"
 "   imcat -prefix small -res_in 64 64 \\\n"
 "         ~/abin/funstuff/face_zzzsunbrain.jpg \n"
 "   aiv small.ppm big.ppm \n"
 "\n"
 "Example 1:\n"
 "   Stitching together images:\n"
 "    (Can be used to make very high resolution SUMA images.\n"
 "     Read about 'Ctrl+r' in SUMA's GUI help.)\n"
 "   imcat -prefix cat -matrix 14 12 \\\n"
 "         ~/abin/funstuff/face_*.jpg\n"
 "   aiv cat.ppm\n"
 "\n"
 "Example 2:\n"
 "   Stitching together 3 images getting rid of annoying white boundary:\n"
 "\n"
 "   imcat -prefix surfview_pry3b.jpg -ny 1 -autocrop surfview.000[789].jpg\n"
 "\n"   
 "Example 20 (assuming afni is in ~/abin directory):\n"
 "   imcat -prefix bigcat.jpg -scale_image ~/abin/afnigui_logo.jpg \\\n"
 "         -matrix_from_scale -rand_wrap -rgb_out -respad_in 128 128 \\\n"
 "         -pad_val 128 ~/abin/funstuff/face_*.jpg \n"
 "   aiv   bigcat.jpg bigcat.jpg \n"
 "   Crop/Zoom in to see what was done. In practice, you want to use\n"
 "   a faster image viewer to examine the result. Zooming on such\n"
 "   a large image is not fast in aiv.\n"
 "   Be careful with this toy. Images get real big, real quick.\n"
 "\n"
 "You can look at the output image file with\n"
 "  afni -im ppp.ppm  [then open the Sagittal image window]\n"
 "\n"
            ) ;
   return;
}

#define ABS(a) ( (a) < 0 ? (-(a)):(a) )
int main( int argc , char * argv[] )
{
   MRI_IMARR * imar, *inimar;
   MRI_IMAGE * im ;
   char prefix[240] = "cat" , fnam[256] ;
   char suffix[50] = "\0";
   char *scale_image = NULL, *scale_pixels = NULL;
   int iarg , ii,jj , nx,ny , nxim,nyim ;
   int nmode = XYNUM, nxin, nyin, nbad = 0;
   int cutL=0, cutR=0, cutT=0, cutB=0, cutlev=0, cutlevu=0;
   int gap = 0, ScaleInt=0, force_rgb_out = 0, matrix_size_from_scale = 0;
   byte  gap_col[3] = {255, 20, 128}, pval = 0 ;
   MRI_IMAGE *imscl=NULL;
   int kkk, nscl=-1, resix=-1, resiy=-1, force_rgb_at_input=0, 
       N_byte = 0, N_rgb = 0, ks = 1, wrapmode = 1;
   float *scl=NULL, fac=1.0, ff=0;
   byte *scl3=NULL, *rgb=NULL, *b1, *b2, *b3;
   char name[100];
   void *ggg=NULL;
   
   
   mainENTRY("imcat main"); machdep() ;
    
    wrapmode = 1;
    ScaleInt = 0;
    resix=-1;
    resiy=-1;
    ks = 1;
    cutL=0;
    cutB=0;
    cutT=0;
    cutR=0;
    cutlev =-2;
    cutlevu=-2;
    fac = 1.0;
    force_rgb_out = 0;
    iarg = 1 ;
    pval = 0 ;
    nx = -1; ny = -1;
    while( iarg < argc && argv[iarg][0] == '-' ){
       if( strcmp(argv[iarg],"-help") == 0 ||
           strcmp(argv[iarg],"-h") == 0 ) {
         imcat_usage(strlen(argv[iarg])>2 ? 2:1);
         exit(0);  /* do not continue   18 Sep 2018 [rickr] */
       }

       if( strcmp(argv[iarg],"-rand_wrap") == 0  ) {
         wrapmode = 2;
         iarg++ ; continue ;
       }
              
       if( strcmp(argv[iarg],"-matrix") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -matrix\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
         ny = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-crop") == 0 ){
         if (iarg+4 >= argc) {
            fprintf(stderr,
                     "*** ERROR: Need four positive integers after -crop\n");
            exit(1);
         }
         cutL = (int) strtod( argv[++iarg] , NULL ) ;
         cutR = (int) strtod( argv[++iarg] , NULL ) ;
         cutT = (int) strtod( argv[++iarg] , NULL ) ;
         cutB = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-autocrop") == 0 ){
         cutlev = -1; cutlevu = -1;
         iarg++ ; continue ;
       }
       
       if ( strcmp(argv[iarg],"-autocrop_ctol") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,
             "*** ERROR: Need one integer between 0 and 100 after"
             " -autocrop_ctol\n");
            exit(1);
         }
         cutlev  = ((int)strtod( argv[++iarg] , NULL ) );
         if (cutlev < 0 || cutlev > 100) {
            fprintf(stderr,
         "*** ERROR: Need one integer between 0 and 100 after -autocrop_ctol\n"
               "Got %d\n", cutlevu);
            exit(1);
         }
         
         iarg++ ; continue ;
       }
       
       if ( strcmp(argv[iarg],"-autocrop_atol") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,
             "*** ERROR: Need one integer between 0 and 100 after"
             " -autocrop_atol\n");
            exit(1);
         }
         cutlevu  = ((int)strtod( argv[++iarg] , NULL ) );
         if (cutlevu < 0 || cutlevu > 100) {
            fprintf(stderr,
         "*** ERROR: Need one integer between 0 and 100 after -autocrop_atol\n"
               "Got %d\n", cutlevu);
            exit(1);
         }
         
         iarg++ ; continue ;
       }
       
       
       if( strcmp(argv[iarg],"-res_in") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -res_in\n");
            exit(1);
         }
         resix = (int) strtod( argv[++iarg] , NULL ) ;
         resiy = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-gscale") == 0 ){
         if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need one float after -gscale\n");
            exit(1);
         }
         fac = (float) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       

       if( strcmp(argv[iarg],"-respad_in") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -respad_in\n");
            exit(1);
         }
         resix = (int) strtod( argv[++iarg] , NULL ) ;
         resiy = (int) strtod( argv[++iarg] , NULL ) ;
         ks = -1;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-pad_val") == 0 ){
         if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need one byte after -pad_val\n");
            exit(1);
         }
         pval = (byte) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-nx") == 0 ){
         if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -nx\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-ny") == 0 ){
         if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -ny\n");
            exit(1);
         }
         ny = (int) strtod( argv[++iarg] , NULL ) ; 
          iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-gap") == 0 ){
         if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -gap\n");
            exit(1);
         }
         gap = (int) strtod( argv[++iarg] , NULL ) ; 
          iarg++ ; continue ;
       }
       if( strcmp(argv[iarg],"-gap_col") == 0 ){
         if (iarg+2 >= argc) {
            fprintf(stderr,
         "*** ERROR: Need three integers between 0 and 255 after -gap_col\n");
            exit(1);
         }
         gap_col[0] = (byte) strtod( argv[++iarg] , NULL ) ;
         gap_col[1] = (byte) strtod( argv[++iarg] , NULL ) ;
         gap_col[2] = (byte) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-scale_image") == 0 ){
          if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need an image after -scale_image\n");
            exit(1);
         }
          scale_image = argv[++iarg];
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-scale_pixels") == 0 ){
          if (iarg+1 >= argc) {
            fprintf(stderr,"*** ERROR: Need an image after -scale_pixels\n");
            exit(1);
          }
          scale_pixels = argv[++iarg];
          if (!THD_is_ondisk(scale_pixels)) {
            fprintf(stderr,"*** ERROR: Image %s not found\n", scale_pixels);
            exit(1);
          }
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-scale_intensity") == 0) {
         ScaleInt = 1;
         iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-rgb_out") == 0) {
         force_rgb_out = 1;
         iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-zero_wrap") == 0) {
         mri_Set_OK_WrapZero(1);
         iarg++ ; continue ;
       }
       if( strcmp(argv[iarg],"-white_wrap") == 0) {
         mri_Set_OK_WrapZero(255);
         iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-gray_wrap") == 0) {
         byte bb;
         if (iarg+1 >= argc) {
            fprintf(stderr,
                    "*** ERROR: Need a number between 0 and 1 for -gray_wrap\n");
            exit(1);
         }
         ++iarg;
         bb = (byte)(atof(argv[iarg])*255);
         if (bb < 1) bb = 1;
         mri_Set_OK_WrapZero(bb);
         iarg++ ; continue ;
       }
       if( strcmp(argv[iarg],"-image_wrap") == 0) {
         mri_Set_KO_WrapZero();
         iarg++ ; continue ;
       }
       if( strcmp(argv[iarg],"-matrix_from_scale") == 0) {
         matrix_size_from_scale = 1;
         iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-prefix") == 0 ){
          MCW_strncpy( prefix , argv[++iarg] , 240 ) ;
          iarg++ ; continue ;
       }

       fprintf(stderr,"*** ERROR: illegal option %s\n",argv[iarg]) ;
       suggest_best_prog_option(argv[0], argv[iarg]);
       exit(1) ;
    }
    
    if( argc < 4 ){
      ERROR_message("Too few options");
      imcat_usage(0);
      exit(1) ;
    }
    
    if (cutlevu == -1 && cutlev == -1) {
      /* No params set, but autocrop desired*/
      cutlev = 20;
      cutlevu = 20;
    }
    
    if (scale_image) {
      if (!(imscl = mri_read_just_one(scale_image))) {
         fprintf(stderr,"*** Failed to read scale image.\n");
         exit(1);
      }else {
         fprintf(stderr,"++Scale image prep\n");
         rgb= MRI_BYTE_PTR(imscl);
         nscl = imscl->nx*imscl->ny;
         scl = (float *)malloc(sizeof(float)*nscl);
         scl3 = (byte *)malloc(sizeof(byte)*nscl*3);
         if (imscl->kind == MRI_rgb) {
            for (kkk=0; kkk<nscl; ++kkk) {
               scl[kkk] = ( (float)rgb[3*kkk  ] +
                            (float)rgb[3*kkk+1] +
                            (float)rgb[3*kkk+2] ) / 3.0 * fac;
               if ((ff = rgb[3*kkk  ]* fac) > 255) ff = 255;
               scl3[3*kkk  ] = ff;
               if ((ff = rgb[3*kkk+1]* fac) > 255) ff = 255;
               scl3[3*kkk+1] = ff;
               if ((ff = rgb[3*kkk+2]* fac) > 255) ff = 255;
               scl3[3*kkk+2] = ff;
            }
        } else if (imscl->kind == MRI_byte) {
            for (kkk=0; kkk<nscl; ++kkk) {
               scl[kkk] = ((float)rgb[kkk  ]* fac);
               if ((ff=scl[kkk])>255) ff = 255;
               scl3[3*kkk  ] = scl3[3*kkk+1] = scl3[3*kkk+2] = ff;
            } 
        } else {
         fprintf(stderr,"*** Scale image must be RGB or byte type.\n");
            exit(1);
        }
      }
   }
   
   if (matrix_size_from_scale) {
      if (imscl) {
         fprintf(stderr,"+++ Matrix size of %dx%d, based on scale image\n", 
                         imscl->nx, imscl->ny);
         nx = imscl->nx; ny = imscl->ny;
      } else {
         fprintf(stderr,
            "*** Want matrix size from scale image but no scale image found.\n");
         exit(1);
      }
   }
   /* allow from catwrapping */
   if (wrapmode == 2) mri_Set_OK_catrandwrap();
   else mri_Set_OK_catwrap();
  
   /* read all images */
   inimar = mri_read_resamp_many_files( argc-iarg , argv+iarg, 
                                        resix, ks*resiy, pval) ;
   if( inimar == NULL ){
      fprintf(stderr,"*** no input images read!\a\n") ;
      exit(1) ;
   } else if( inimar->num < 1 ){
      fprintf(stderr,"*** less than 1 input image read!\a\n") ;
      exit(1) ;
   }

   /* Figure out cropping based on one image, written as loop in 
   case I change my mind */
   if (cutlev >= 0 || cutlevu >= 0) {
      MRI_IMAGE *imin;
      byte *colav=NULL, *rowav=NULL;
      int mis=0, kx=0, ky=0, e0, e1, e2;
      double d0, d1, d2;
      if (cutlev  > 0) cutlev   = (int)(cutlev *2.55);
      if (cutlevu > 0) cutlevu  = (int)(cutlevu*2.55);
      cutL = 0;
      for (kkk=0; kkk<1; ++kkk) {
         imin = IMAGE_IN_IMARR(inimar,kkk);
         rgb= MRI_BYTE_PTR(imin);
         if (imin->kind == MRI_rgb) {
            e0 = (imin->nx*imin->ny-1)*3;
            e1 = e0+1;
            e2 = e0+2;
            kkk = 0;
            /* fprintf(stderr,"RGB image nx=%d, ny=%d, cut lev %d, avg lev %d\n",
                    imin->nx, imin->ny, cutlev, cutlevu); */
            
            /* Compute column and row averages */
            if (cutlevu >= 0) {
               colav = (byte *)calloc(3*imin->nx, sizeof(byte));
               for (kx=0; kx<imin->nx; ++kx) {
                  d0 = d1 = d2 = 0.0;
                  for (ky=0; ky<imin->ny; ++ky) {
                     kkk = kx+imin->nx*ky;
                     d0 += rgb[3*kkk  ];
                     d1 += rgb[3*kkk+1];
                     d2 += rgb[3*kkk+2];
                  }
                  colav[3*kx  ] = (byte)(d0/imin->ny);
                  colav[3*kx+1] = (byte)(d1/imin->ny);
                  colav[3*kx+2] = (byte)(d2/imin->ny);
               }
               rowav = (byte *)calloc(3*imin->ny, sizeof(byte));   
               for (ky=0; ky<imin->ny; ++ky) {
                  d0 = d1 = d2 = 0.0;
                  for (kx=0; kx<imin->nx; ++kx) {
                     kkk = kx+imin->nx*ky;
                     d0 += rgb[3*kkk  ];
                     d1 += rgb[3*kkk+1];
                     d2 += rgb[3*kkk+2];
                  }
                  rowav[3*ky  ] = (byte)(d0/imin->nx);
                  rowav[3*ky+1] = (byte)(d1/imin->nx);
                  rowav[3*ky+2] = (byte)(d2/imin->nx);
               }
            }
            
            /* Left */
            cutL = 0;
            mis = 0;
               for (kx=0; kx<imin->nx && !mis; ++kx) {
            for (ky=0; ky<imin->ny && !mis; ++ky) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     /* break if color changes from corner */
                     if (ABS(rgb[3*kkk  ] - rgb[0]) > cutlev ||
                         ABS(rgb[3*kkk+1] - rgb[1]) > cutlev ||
                         ABS(rgb[3*kkk+2] - rgb[2]) > cutlev ) {
                        /* fprintf(stderr,
                             "Break from [%d %d %d] at %d %d with [%d %d %d]\n",
                                rgb[0], rgb[1], rgb[2], kx, ky, 
                                rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */ 
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[3*kkk  ] - colav[3*kx  ]) > cutlevu ||
                         ABS(rgb[3*kkk+1] - colav[3*kx+1]) > cutlevu ||
                         ABS(rgb[3*kkk+2] - colav[3*kx+2]) > cutlevu ) {
                        /* fprintf(stderr,
                     "Break from colav [%d %d %d] at %d %d with [%d %d %d]\n",
                          colav[3*kx  ], colav[3*kx+1], colav[3*kx+2], kx, ky,
                          rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutL;
                  /* fprintf(stderr,"Line at x=%d is cst\n", kx); */
               }
            }
            /* Right */
            cutR = 0;
            mis = 0;
               for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
            for (ky=0; ky<imin->ny && !mis; ++ky) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[3*kkk  ] - rgb[e0]) > cutlev ||
                         ABS(rgb[3*kkk+1] - rgb[e1]) > cutlev ||
                         ABS(rgb[3*kkk+2] - rgb[e2]) > cutlev ) {
                        /* fprintf(stderr,
                             "Break from [%d %d %d] at %d %d with [%d %d %d]\n",
                                rgb[0], rgb[1], rgb[2], kx, ky, 
                                rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[3*kkk  ] - colav[3*kx  ]) > cutlevu ||
                         ABS(rgb[3*kkk+1] - colav[3*kx+1]) > cutlevu ||
                         ABS(rgb[3*kkk+2] - colav[3*kx+2]) > cutlevu ) {
                        /* fprintf(stderr,
                     "Break from colav [%d %d %d] at %d %d with [%d %d %d]\n",
                          colav[3*kx  ], colav[3*kx+1], colav[3*kx+2], kx, ky,
                          rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutR;
                  /* fprintf(stderr,"Line at x=%d is cst\n", kx); */
               }
            }
            /* Bottom */
            cutB = 0;
            mis = 0;
            for (ky=imin->ny-1; ky>=0 && !mis; --ky) {
               for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[3*kkk  ] - rgb[e0]) > cutlev ||
                         ABS(rgb[3*kkk+1] - rgb[e1]) > cutlev ||
                         ABS(rgb[3*kkk+2] - rgb[e2]) > cutlev ) {
                        /* fprintf(stderr,
                             "Break from [%d %d %d] at %d %d with [%d %d %d]\n",
                                rgb[0], rgb[1], rgb[2], kx, ky, 
                                rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[3*kkk  ] - rowav[3*ky  ]) > cutlevu ||
                         ABS(rgb[3*kkk+1] - rowav[3*ky+1]) > cutlevu ||
                         ABS(rgb[3*kkk+2] - rowav[3*ky+2]) > cutlevu ) {
                        /* fprintf(stderr,
                     "Break from colav [%d %d %d] at %d %d with [%d %d %d]\n",
                          rowav[3*ky  ], rowav[3*ky+1], rowav[3*ky+2], kx, ky,
                          rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutB;
                  /* fprintf(stderr,"Line at y=%d is cst\n", ky); */
               }
            }
            /* Top */
            cutT = 0;
            mis = 0;
            for (ky=0; ky<imin->ny && !mis; ++ky) {
               for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[3*kkk  ] - rgb[e0]) > cutlev ||
                         ABS(rgb[3*kkk+1] - rgb[e1]) > cutlev ||
                         ABS(rgb[3*kkk+2] - rgb[e2]) > cutlev ) {
                        /* fprintf(stderr,
                             "Break from [%d %d %d] at %d %d with [%d %d %d]\n",
                                rgb[0], rgb[1], rgb[2], kx, ky, 
                                rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[3*kkk  ] - rowav[3*ky  ]) > cutlevu ||
                         ABS(rgb[3*kkk+1] - rowav[3*ky+1]) > cutlevu ||
                         ABS(rgb[3*kkk+2] - rowav[3*ky+2]) > cutlevu ) {
                        /* fprintf(stderr,
                     "Break from colav [%d %d %d] at %d %d with [%d %d %d]\n",
                          rowav[3*ky  ], rowav[3*ky+1], rowav[3*ky+2], kx, ky,
                          rgb[3*kkk  ], rgb[3*kkk+1], rgb[3*kkk+2]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutT;
                  /* fprintf(stderr,"Line at y=%d is cst\n", ky); */
               }
            }
            if (colav) free(colav); colav=NULL;
            if (rowav) free(rowav); rowav=NULL;
        } else if (imin->kind == MRI_byte) {
            e0 = (imin->nx*imin->ny-1);
            kkk = 0;
            /* fprintf(stderr,"Byte image nx=%d, ny=%d, cut level %d, %d\n", 
                    imin->nx, imin->ny, cutlev, cutlevu); */
            /* Compute column and row averages */
            if (cutlevu >= 0) {
               colav = (byte *)calloc(imin->nx, sizeof(byte));
               for (kx=0; kx<imin->nx; ++kx) {
                  d0 = 0.0;
                  for (ky=0; ky<imin->ny; ++ky) {
                     kkk = kx+imin->nx*ky;
                     d0 += rgb[kkk  ];
                  }
                  colav[kx  ] = (byte)(d0/imin->ny);
               }
               rowav = (byte *)calloc(imin->ny, sizeof(byte));   
               for (ky=0; ky<imin->ny; ++ky) {
                  d0 = 0.0;
                  for (kx=0; kx<imin->nx; ++kx) {
                     kkk = kx+imin->nx*ky;
                     d0 += rgb[kkk  ];
                  }
                  rowav[ky  ] = (byte)(d0/imin->nx);
               }
            }
            /* Left */
            cutL = 0;
            mis = 0;
            for (kx=0; kx<imin->nx && !mis; ++kx) {
               for (ky=0; ky<imin->ny && !mis; ++ky) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[kkk] - rgb[0]) > cutlev) {
                        /* fprintf(stderr,
                                "Break from [%d] at %d %d with [%d]\n",
                                rgb[0], kx, ky, rgb[kkk]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[kkk  ] - colav[kx  ]) > cutlevu) {
                        /* fprintf(stderr,
                     "Break from colav [%d] at %d %d with [%d]\n",
                          colav[kx  ], kx, ky,
                          rgb[kkk  ]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutL;
                  /* fprintf(stderr,"Line at x=%d is cst\n", kx); */
               }
            }
            /* Right */
            cutR = 0;
            mis = 0;
            for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
               for (ky=0; ky<imin->ny && !mis; ++ky) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[kkk] - rgb[e0]) > cutlev ) {
                        /* fprintf(stderr,
                                "Break from [%d] at %d %d with [%d]\n",
                                rgb[0], kx, ky, rgb[kkk]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[kkk  ] - colav[kx  ]) > cutlevu) {
                        /* fprintf(stderr,
                     "Break from colav [%d] at %d %d with [%d]\n",
                          colav[kx  ], kx, ky,
                          rgb[kkk  ]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutR;
                  /* fprintf(stderr,"Line at x=%d is cst\n", kx); */
               }
            }
            /* Bottom */
            cutB = 0;
            mis = 0;
            for (ky=imin->ny-1; ky>=0 && !mis; --ky) {
               for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[kkk] - rgb[e0]) > cutlev ) {
                        /* fprintf(stderr,
                                "Break from [%d] at %d %d with [%d]\n",
                                rgb[0], kx, ky, rgb[kkk]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[kkk  ] - rowav[ky  ]) > cutlevu) {
                        /* fprintf(stderr,
                     "Break from rowav [%d] at %d %d with [%d]\n",
                          rowav[ky  ], kx, ky,
                          rgb[kkk  ]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutB;
                  /* fprintf(stderr,"Line at y=%d is cst\n", ky); */
               }
            }
            /* Top */
            cutT = 0;
            mis = 0;
            for (ky=0; ky<imin->ny && !mis; ++ky) {
               for (kx=imin->nx-1; kx>=0 && !mis; --kx) {
                  kkk = kx+imin->nx*ky;
                  if (cutlev >= 0) {
                     if (ABS(rgb[kkk  ] - rgb[e0]) > cutlev) {
                        /* fprintf(stderr,
                                "Break from [%d] at %d %d with [%d]\n",
                                rgb[0], kx, ky, rgb[kkk]); */
                        mis = 1;
                     }
                  }
                  /* break if color changes from average of column */
                  if (cutlevu >= 0) {
                     if (ABS(rgb[kkk  ] - rowav[ky  ]) > cutlevu) {
                        /* fprintf(stderr,
                     "Break from rowav [%d] at %d %d with [%d]\n",
                          rowav[ky  ], kx, ky,
                          rgb[kkk  ]); */
                        mis = 1;
                     }
                  }
               }
               if (!mis) {
                  ++cutT;
                  /* fprintf(stderr,"Line at y=%d is cst\n", ky); */
               }
            } 
            if (colav) free(colav); colav=NULL;
            if (rowav) free(rowav); rowav=NULL;
        } else {
            fprintf(stderr,"*** For autocrop image must be RGB or byte type.\n");
            exit(1);
        }
        
        fprintf(stdout,"+++ Autocrop set to L%d R%d T%d B%d\n", 
                       cutL, cutR, cutT, cutB); 
      }
   }
   
   /* crop all images if needed */
   if (cutL || cutR || cutT || cutB) {
      /* original image size */
      nxin = IMAGE_IN_IMARR(inimar,0)->nx;
      nyin = IMAGE_IN_IMARR(inimar,0)->ny;
      
      nbad = mri_cut_many_2D(inimar, cutL, nxin-1-cutR, cutT, nyin-1-cutB);
      if (nbad) {
         fprintf(stderr,"*** failed (%d) in cropping operation!\a\n", nbad) ;
         exit(1) ;
      }
   }
   
   /* figure out the count */
   if (nx < 0 && ny < 0) {
      nx = ny = (int)sqrt((double)inimar->num+0.00001);
      if (nx*ny != inimar->num) {
         fprintf(stderr,"*** Can't guess at matrix layout for %d images.\n"
                        "    Use -nx, -ny or -matrix\n", inimar->num);
         exit(1);
      }
   } else if (nx < 0) {
      if (inimar->num % ny) {
         fprintf(stderr,"--- Have %d images, not divisible by %d\n"
                        "    will pad to fill matrix.\n"
                        , inimar->num, ny);      
         nx = inimar->num / ny + 1;
      } else {
         nx = inimar->num / ny;
      }
   } else if (ny < 0) {
      if (inimar->num % nx) {
         fprintf(stderr,"--- Have %d images, not divisible by %d!\n"
                        "    will pad to fill matrix.\n"
                        , inimar->num, nx);
         ny = inimar->num / nx + 1;
      } else {
         ny = inimar->num / nx;
      }
   }
   
   if( nx < 1 || ny < 1 ){
    fprintf(stderr,"*** ERROR: illegal values nx=%d ny=%d\n",nx,ny);
    exit(1) ;
   }
   if (nx*ny > inimar->num) {
      fprintf(stderr,"+++ Have %d images. Will wrap to fill %dx%d matrix\n", 
               inimar->num, nx, ny);
   } else if (nx*ny < inimar->num) {
      fprintf(stderr,"+++ Have more images than needed to fill matrix.\n"
                     "    Will ignore the last %d images.\n", 
                     inimar->num- nx*ny);
   }
   
   nxin = IMAGE_IN_IMARR(inimar,0)->nx;
   nyin = IMAGE_IN_IMARR(inimar,0)->ny;
   
   fprintf(stdout, "+++ Arranging %d images (each %dx%d) into a %dx%d matrix.\n",                    inimar->num, nxin, nyin, nx, ny);

   force_rgb_at_input = 0;
   N_byte = 0; N_rgb = 0;
   if (gap) force_rgb_at_input = 1; /* must go rgb */
   else {
      /* if have multiple types, must also go rgb */
      for (kkk=0; kkk<inimar->num; ++kkk) {
         if (IMAGE_IN_IMARR(inimar,kkk)->kind != MRI_byte && 
             IMAGE_IN_IMARR(inimar,kkk)->kind != MRI_rgb) {
            fprintf(stderr,"*** Unexpected image kind on input.\n"
                           "    Only byte and rgb (3byte) images allowed.\n");
            exit(1);
         } else if (IMAGE_IN_IMARR(inimar,kkk)->kind == MRI_byte) {
            ++N_byte;
         } else if (IMAGE_IN_IMARR(inimar,kkk)->kind == MRI_rgb) {
            ++N_rgb;
         } 
         if (N_byte && N_rgb) { force_rgb_at_input = 1; continue; }
      }
   }
   
   if (force_rgb_at_input) { /* make sure all images are rgb type */
      MRI_IMAGE *imin, *newim;
      int kkk, nmin;
      if (nx*ny < inimar->num) nmin = nx*ny;
      else nmin = inimar->num;
      /* must transform input to rgb here to allow for gap option */
      fprintf(stderr,"+++ Transforming all input to rgb for a good reason \n");
      for (kkk=0; kkk<nmin; ++kkk) {
         imin = IMAGE_IN_IMARR(inimar,kkk%inimar->num);
         if(imin->kind == MRI_byte) {
            newim = mri_3to_rgb( imin, imin, imin ) ;
            MRI_COPY_AUX(newim,imin) ; mri_free(imin);
            IMAGE_IN_IMARR(inimar,kkk%inimar->num) = newim;
         } else if (imin->kind != MRI_rgb) {
            fprintf(stderr,"*** Unexpected image kind.\n");
            exit(1);
         }
      }
   } 

   
   ggg = (void *)gap_col;
   
   if (!(im = mri_cat2D( nx , ny , gap , ggg , inimar ))) {
      fprintf(stderr,"*** ERROR: Failed to create image!\n");
      exit(1) ;
   }
   FREE_IMARR(inimar) ; inimar = NULL;

   if (force_rgb_out && im->kind == MRI_byte) {
      MRI_IMAGE *newim=NULL;
      fprintf(stderr,"+++ Forcing output to RGB\n");
      newim = mri_3to_rgb( im, im, im ) ;
      MRI_COPY_AUX(newim,im) ; mri_free(im);
      im = newim;
   }
   
   /* image scaling needed, not very efficient in implementation but 
      mostly for fun */
   if (scale_image && ( im->kind == MRI_rgb || im->kind == MRI_byte) ) {
      MRI_IMARR *imtriple ; 
      MRI_IMAGE *rim, *gim, *bim, *tim, *imin, *newim;
      fprintf(stderr,"+++ Scaling by image\n");
      if (!imscl) {
         fprintf(stderr,"*** No scale image!!!.\n");
         exit(1);
      }else {
         rgb= MRI_BYTE_PTR(imscl);
         nscl = imscl->nx*imscl->ny;
         scl = (float *)malloc(sizeof(float)*nscl);
         scl3 = (byte *)malloc(sizeof(byte)*nscl*3);
         if (imscl->kind == MRI_rgb) {
            for (kkk=0; kkk<nscl; ++kkk) {
               scl[kkk] = ((float)rgb[3*kkk  ] +
                          (float)rgb[3*kkk+1] +
                          (float)rgb[3*kkk+2] ) / 3.0 * fac ;
               
               if ((ff = rgb[3*kkk  ]* fac) > 255)  ff=255 ;
               scl3[3*kkk  ] = ff;
               if ((ff = rgb[3*kkk+1]* fac) > 255)  ff=255;
               scl3[3*kkk+1] = ff;
               if ((ff = rgb[3*kkk+2]* fac) > 255)  ff=255;
               scl3[3*kkk+2] = ff;
            }
        } else if (imscl->kind == MRI_byte) {
            for (kkk=0; kkk<nscl; ++kkk) {
               scl[kkk] = ((float)rgb[kkk  ]* fac) ;
               if ((ff = rgb[kkk]* fac) > 255) ff = 255;
               scl3[3*kkk  ] = scl3[3*kkk+1] = scl3[3*kkk+2] = ff;
            } 
        } else {
         fprintf(stderr,"*** Scale image must be RGB or byte type.\n");
            exit(1);
        }
      }
      
      /* Now break the image again */
      if (!(inimar = mri_uncat2D(  nxin ,  nyin ,im ))) {
         fprintf(stderr,"*** Failed to cut up image\n");
         exit(1);
      }
      mri_free(im); im = NULL;
      
      for (kkk=0; kkk<inimar->num; ++kkk) {  
         imin = IMAGE_IN_IMARR(inimar,kkk);
         /*  sprintf(name,"prescaled.%d.ppm", kkk);
           mri_write(name,imin); */
         if(imin->kind == MRI_rgb) {
            imtriple = mri_rgb_to_3byte( imin ) ;
            if( imtriple == NULL ){
             fprintf(stderr,"*** mri_rgb_to_3byte fails!\n"); break;
            }
           rim = IMAGE_IN_IMARR(imtriple,0) ;
           gim = IMAGE_IN_IMARR(imtriple,1) ;
           bim = IMAGE_IN_IMARR(imtriple,2) ; FREE_IMARR(imtriple) ;
           /* scale image */
           if (ScaleInt) {
              /* fprintf(stderr,"Scaling image %d by %f\n", 
                                 kkk, scl[kkk%nscl]); */
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], rim ); 
                                                   mri_free(rim); rim = tim;
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], gim ); 
                                                   mri_free(gim); gim = tim;
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], bim ); 
                                                   mri_free(bim); bim = tim;
           } else {
              /* fprintf(stderr,"Scaling image %d by [%.2f %.2f %.2f]\n", 
                              kkk,  (float)scl3[3*(kkk%nscl)  ],
                                    (float)scl3[3*(kkk%nscl)+1],
                                    (float)scl3[3*(kkk%nscl)+2]); */
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)  ], rim ); 
                                                    mri_free(rim); rim = tim;
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)+1], gim ); 
                                                    mri_free(gim); gim = tim;
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)+2], bim ); 
                                                    mri_free(bim); bim = tim;
           }
           newim = mri_3to_rgb( rim, gim, bim ) ;
           mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
           MRI_COPY_AUX(newim,imin) ; mri_free(imin);
           IMAGE_IN_IMARR(inimar,kkk) = newim;
         } else if(imin->kind == MRI_byte) {
           /* scale image */
           /* fprintf(stderr,"Scaling byte image %d by %f\n", 
                              kkk, scl[kkk%nscl]); */ 
           newim = mri_to_byte_scl(0.0, (float)scl[kkk%nscl], imin ); 
           MRI_COPY_AUX(newim,imin) ; mri_free(imin);
           /* sprintf(name,"postscaled.%d.ppm", kkk);
           mri_write(name,newim); */
           IMAGE_IN_IMARR(inimar,kkk) = newim;
         } else {
            fprintf(stderr,
               "*** image %d is not rgb or byte. No scaling for you!\n", 
               kkk%inimar->num);
         }
      }
      if (scl) free(scl); scl = NULL;
      if (scl3) free(scl3); scl3 = NULL;
      /* Now put it back together */
      /* fprintf(stderr,"Going to cat2D, again\n"); */
      if (!(im = mri_cat2D( nx , ny , gap , ggg , inimar ))) {
         fprintf(stderr,"*** ERROR: Failed to create image!\n");
         exit(1) ;
      }
      FREE_IMARR(inimar) ; inimar = NULL;

   }

   if (scale_pixels) {
      MRI_IMARR *imtriple ; 
      MRI_IMAGE *rim, *gim, *bim, *tim, *newim;
      if (im->kind != MRI_rgb && im->kind != MRI_byte) {
         fprintf(stderr,"*** Output not MRI_rgb or MRI_byte. Cannot scale.\n"); 
         exit(1);
      }
      fprintf(stderr,"*** Pixel scaling ...\n");
      if (!(imar = mri_read_resamp_many_files( 1 , &scale_pixels, 
                                                 im->nx, ks*im->ny, pval )) ||
           imar->num != 1) {
         fprintf(stderr,"*** Failed to read 1 image from %s\n", scale_pixels);
         exit(1) ;
      }
      
      if (imscl) mri_free(imscl); imscl=NULL;
      if (scl) free(scl); scl=NULL;
      if (scl3) free(scl3); scl3 = NULL;
      
      imscl = IMAGE_IN_IMARR(imar,0);      
      rgb= MRI_BYTE_PTR(imscl);
      nscl = imscl->nx*imscl->ny;
      scl = (float *)malloc(sizeof(float)*nscl);
      scl3 = (byte *)malloc(sizeof(byte)*nscl*3);
      if (imscl->kind == MRI_rgb) {
         for (kkk=0; kkk<nscl; ++kkk) {
            scl[kkk] = (   (float)rgb[3*kkk  ] +
                           (float)rgb[3*kkk+1] +
                           (float)rgb[3*kkk+2] ) / 3.0;
            scl3[3*kkk  ] = rgb[3*kkk  ];
            scl3[3*kkk+1] = rgb[3*kkk+1];
            scl3[3*kkk+2] = rgb[3*kkk+2];
         }
      } else if (imscl->kind == MRI_byte) {
         for (kkk=0; kkk<nscl; ++kkk) {
            scl[kkk] = ((float)rgb[kkk  ]);
            scl3[3*kkk  ] = rgb[kkk];  /* inefficient, but makes life easy */
            scl3[3*kkk+1] = rgb[kkk];
            scl3[3*kkk+2] = rgb[kkk];
         } 
      } else {
         fprintf(stderr,"*** Scale image must be RGB or byte type.\n");
         exit(1);
      }
      /* Now scale */
      if(im->kind == MRI_rgb) {
         imtriple = mri_rgb_to_3byte( im ) ;
         if( imtriple == NULL ){
            fprintf(stderr,"*** mri_rgb_to_3byte fails!\n"); exit(1);
         }
         rim = IMAGE_IN_IMARR(imtriple,0) ; b1 = MRI_BYTE_PTR(rim) ;
         gim = IMAGE_IN_IMARR(imtriple,1) ; b2 = MRI_BYTE_PTR(gim) ;
         bim = IMAGE_IN_IMARR(imtriple,2) ; b3 = MRI_BYTE_PTR(bim) ;
         FREE_IMARR(imtriple) ;
         /* scale image */
         if (ScaleInt) {
            for (kkk=0; kkk<im->nvox; ++kkk) {
               b1[kkk] = BYTEIZE((b1[kkk]*(float)scl[kkk])/255.0);
               b2[kkk] = BYTEIZE((b2[kkk]*(float)scl[kkk])/255.0);
               b3[kkk] = BYTEIZE((b3[kkk]*(float)scl[kkk])/255.0);
            }      
         } else {
            /* fprintf(stderr,"Scaling image %d by [%.2f %.2f %.2f]\n", 
                           kkk,  (float)scl3[3*(kkk%nscl)  ],
                                 (float)scl3[3*(kkk%nscl)+1],
                                 (float)scl3[3*(kkk%nscl)+2]); */
            for (kkk=0; kkk<im->nvox; ++kkk) {
               b1[kkk] = BYTEIZE((b1[kkk]*(float)scl3[3*kkk  ])/255.0);
               b2[kkk] = BYTEIZE((b2[kkk]*(float)scl3[3*kkk+1])/255.0);
               b3[kkk] = BYTEIZE((b3[kkk]*(float)scl3[3*kkk+2])/255.0);
            }   
         }
         newim = mri_3to_rgb( rim, gim, bim ) ;
         mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
         MRI_COPY_AUX(newim,im) ; mri_free(im);
         im = newim; newim = NULL;
      } else if(im->kind == MRI_byte) {
         /* scale image */
         b1 = MRI_BYTE_PTR(im) ;
         for (kkk=0; kkk<im->nvox; ++kkk) {
            b1[kkk] = BYTEIZE((b1[kkk]*(float)scl[kkk])/255.0);
         }
      } else {
            fprintf(stderr,
               "*** image is not rgb or byte. No scaling for you!\n");
      }
      if (scl) free(scl); scl = NULL;
      if (scl3) free(scl3); scl3 = NULL;
   }
   
   if (!(STRING_HAS_SUFFIX_CASE(prefix,".1D"))) {
      if (!(STRING_HAS_SUFFIX_CASE(prefix,".jpg")) &&
          !(STRING_HAS_SUFFIX_CASE(prefix,".ppm")) ) {
         sprintf(fnam,"%s.ppm",prefix);
      } else {
         sprintf(fnam,"%s",prefix);
      }
      fprintf(stderr,"+++ Writing image to %s\n", fnam);
      mri_write(fnam,im) ;
      fprintf(stdout, "You can view image %s with:\n aiv %s \n", fnam, fnam);
   } else { /* write a 1D out */
      FILE *fout=fopen(prefix,"w");
      float *fff=NULL;
      MRI_IMAGE *tim;
      if (!fout) {
         fprintf(stderr,"*** ERROR: Failed to open %s for output!\n", prefix);
         exit(1) ;
      }
      if (!(tim = mri_to_float(im))) {
         fprintf(stderr,"*** ERROR: Failed to convert output image.\n");
         exit(1) ;
      }
      fff= MRI_FLOAT_PTR(tim);  
      for (jj=0; jj<tim->ny; ++jj)  {
         for (ii=0; ii<tim->nx; ++ii) {
            fprintf(fout,"%.4f ", fff[jj*tim->nx+ii]);
         }
         fprintf(fout,"\n");
      }
      fclose(fout);
      mri_free(tim); tim = NULL;  
      fprintf(stdout, "You can view image %s with:\n 1dgrayplot %s \n", 
                        fnam, prefix);
   }
   
   mri_free(im); im = NULL;
    exit(0) ;
}
