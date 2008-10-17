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

int main( int argc , char * argv[] )
{
   MRI_IMARR * imar, *inimar;
   MRI_IMAGE * im ;
   char prefix[240] = "cat" , fnam[256] ;
   char suffix[50] = "\0";
   char *scale_image = NULL;
   int iarg , ii,jj , nx,ny , nxim,nyim ;
   int nmode = XYNUM, nxin, nyin, nbad = 0;
   int cutL=0, cutR=0, cutT=0, cutB=0;
   int gap = 0, ScaleInt=0, force_rgb_out = 0, matrix_size_from_scale = 0;
   byte  gap_col[3] = {255, 20, 128} ;
   MRI_IMAGE *imscl=NULL;
   int kkk, nscl=-1, resix=-1, resiy=-1, force_rgb_at_input=0, 
       N_byte = 0, N_rgb = 0;
   float *scl=NULL;
   byte *scl3=NULL, *rgb=NULL;
   char name[100];
   void *ggg=NULL;
   
   
   mainENTRY("imcat main"); machdep() ;
    
   if( argc < 4 ){
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
 "  -scale_intensity: Instead of multiplying by the color of \n"
 "                          pixel (i,j), use its intensity \n"
 "                          (average color)\n"
 "  -rgb_out: Force output to be in rgb, even if input is bytes.\n"
 "            This option is turned on automatically in certain cases.\n"
 "  -res_in RX RY: Set resolution of all input images to RX by RY pixels.\n"
 "                 Default is to make all input have the same\n"
 "                 resolution as the first image.\n"
 "  -crop L R T B: Crop images by L (Left), R (Right), T (Top), B (Bottom)\n"
 "                 pixels. Cutting is performed after any resolution change, \n"
 "                 if any, is to be done.\n"
 " ++ Options for output:\n"
 "  -zero_wrap: If number of images is not enough to fill matrix\n"
 "              blank images are used.\n"
 "  -image_wrap: If number of images is not enough to fill matrix\n"
 "               images on command line are reused (default)\n"
 "  -prefix ppp = Prefix the output files with string 'ppp'\n"
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
 "         ~/abin/face_zzzsunbrain.jpg \n"
 "   imcat -prefix small -res_in 64 64 \\\n"
 "         ~/abin/face_zzzsunbrain.jpg \n"
 "   aiv small.ppm big.ppm \n"
 "\n"
 "Example 1:\n"
 "   Stitching together images:\n"
 "    (Can be used to make very high resolution SUMA images.\n"
 "     Read about 'Ctrl+r' in SUMA's GUI help.)\n"
 "   imcat -prefix cat -matrix 14 12 \\\n"
 "         ~/abin/face_*.jpg\n"
 "   aiv cat.ppm\n"
 "\n"
 "Example 2 (assuming afni is in ~/abin directory):\n"
 "   imcat -prefix bigcat -scale_image ~/abin/face_rwcox.jpg \\\n"
 "         -matrix_from_scale -rgb_out -res_in 32 32 ~/abin/face_*.jpg \n"
 "   aiv   bigcat.ppm bigcat.ppm \n"
 "   Crop/Zoom in to see what was done. In practice, you want to use\n"
 "   a faster image viewer to examine the result. Zooming on such\n"
 "   a large image is not fast in aiv.\n"
 "   Be careful with this toy. Images get real big, real quick.\n"
 "\n"
 "You can look at the output image file with\n"
 "  afni -im ppp.ppm  [then open the Sagittal image window]\n"
 "\n"
            ) ;
      exit(0) ;
    }

    ScaleInt = 0;
    resix=-1;
    resiy=-1;
    cutL=0;
    cutB=0;
    cutT=0;
    cutR=0;
    force_rgb_out = 0;
    iarg = 1 ;
      nx = -1; ny = -1;
    while( iarg < argc && argv[iarg][0] == '-' ){

       if( strcmp(argv[iarg],"-matrix") == 0 ){
         if (iarg+2 > argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -matrix\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
         ny = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-crop") == 0 ){
         if (iarg+4 > argc) {
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
       
       if( strcmp(argv[iarg],"-res_in") == 0 ){
         if (iarg+2 > argc) {
            fprintf(stderr,"*** ERROR: Need two integers after -res_in\n");
            exit(1);
         }
         resix = (int) strtod( argv[++iarg] , NULL ) ;
         resiy = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-nx") == 0 ){
         if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -nx\n");
            exit(1);
         }
         nx = (int) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-ny") == 0 ){
         if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -ny\n");
            exit(1);
         }
         ny = (int) strtod( argv[++iarg] , NULL ) ; 
          iarg++ ; continue ;
       }

       if( strcmp(argv[iarg],"-gap") == 0 ){
         if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an integer after -gap\n");
            exit(1);
         }
         gap = (int) strtod( argv[++iarg] , NULL ) ; 
          iarg++ ; continue ;
       }
       if( strcmp(argv[iarg],"-gap_col") == 0 ){
         if (iarg+2 > argc) {
            fprintf(stderr,"*** ERROR: Need three integers between 0 and 255 after -gap_col\n");
            exit(1);
         }
         gap_col[0] = (byte) strtod( argv[++iarg] , NULL ) ;
         gap_col[1] = (byte) strtod( argv[++iarg] , NULL ) ;
         gap_col[2] = (byte) strtod( argv[++iarg] , NULL ) ;
          iarg++ ; continue ;
       }
       
       if( strcmp(argv[iarg],"-scale_image") == 0 ){
          if (iarg+1 > argc) {
            fprintf(stderr,"*** ERROR: Need an image after -scale_image\n");
            exit(1);
         }
          scale_image = argv[++iarg];
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
         mri_Set_OK_WrapZero();
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
       exit(1) ;
    }
      
    if (scale_image) {
      if (!(imscl = mri_read_just_one(scale_image))) {
         fprintf(stderr,"*** Failed to read scale image.\n");
         exit(1);
      }else {
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
      }
   }
   
   if (matrix_size_from_scale) {
      if (imscl) {
         fprintf(stderr,"+++ Matrix size of %dx%d, based on scale image\n", imscl->nx, imscl->ny);
         nx = imscl->nx; ny = imscl->ny;
      } else {
         fprintf(stderr,"*** Want matrix size from scale image but no scale image found.\n");
         exit(1);
      }
   }
   /* allow from catwrapping */
   mri_Set_OK_catwrap();
  
   /* read all images */
   inimar = mri_read_resamp_many_files( argc-iarg , argv+iarg, resix, resiy ) ;
   if( inimar == NULL ){
      fprintf(stderr,"*** no input images read!\a\n") ;
      exit(1) ;
   } else if( inimar->num < 1 ){
      fprintf(stderr,"*** less than 1 input image read!\a\n") ;
      exit(1) ;
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
      fprintf(stderr,"+++ Have %d images. Will wrap to fill %dx%d matrix\n", inimar->num, nx, ny);
   } else if (nx*ny < inimar->num) {
      fprintf(stderr,"+++ Have more images than needed to fill matrix.\n"
                     "    Will ignore the last %d images.\n", inimar->num- nx*ny);
   }
   
   nxin = IMAGE_IN_IMARR(inimar,0)->nx;
   nyin = IMAGE_IN_IMARR(inimar,0)->ny;
   
   fprintf(stdout, "+++ Arranging %d images (each %dx%d) into a %dx%d matrix.\n", inimar->num, nxin, nyin, nx, ny);

   force_rgb_at_input = 0;
   N_byte = 0; N_rgb = 0;
   if (gap) force_rgb_at_input = 1; /* must go rgb */
   else {
      /* if have multiple types, must also go rgb */
      for (kkk=0; kkk<inimar->num; ++kkk) {
         if (IMAGE_IN_IMARR(inimar,kkk)->kind != MRI_byte && IMAGE_IN_IMARR(inimar,kkk)->kind != MRI_rgb) {
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
   
   /* image scaling needed, not very efficient in implementation but mostly for fun */
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
              /* fprintf(stderr,"Scaling image %d by %f\n", kkk, scl[kkk%nscl]); */
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], rim ); mri_free(rim); rim = tim;
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], gim ); mri_free(gim); gim = tim;
              tim = mri_to_byte_scl(0.0, scl[kkk%nscl], bim ); mri_free(bim); bim = tim;
           } else {
              /* fprintf(stderr,"Scaling image %d by [%.2f %.2f %.2f]\n", 
                              kkk,  (float)scl3[3*(kkk%nscl)  ],
                                    (float)scl3[3*(kkk%nscl)+1],
                                    (float)scl3[3*(kkk%nscl)+2]); */
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)  ], rim ); mri_free(rim); rim = tim;
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)+1], gim ); mri_free(gim); gim = tim;
              tim = mri_to_byte_scl(0.0, (float)scl3[3*(kkk%nscl)+1], bim ); mri_free(bim); bim = tim;
           }
           newim = mri_3to_rgb( rim, gim, bim ) ;
           mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
           MRI_COPY_AUX(newim,imin) ; mri_free(imin);
           IMAGE_IN_IMARR(inimar,kkk) = newim;
         } else if(imin->kind == MRI_byte) {
           /* scale image */
           /* fprintf(stderr,"Scaling byte image %d by %f\n", kkk, scl[kkk%nscl]); */ 
           newim = mri_to_byte_scl(0.0, (float)scl[kkk%nscl], imin ); 
           MRI_COPY_AUX(newim,imin) ; mri_free(imin);
           /* sprintf(name,"postscaled.%d.ppm", kkk);
           mri_write(name,newim); */
           IMAGE_IN_IMARR(inimar,kkk) = newim;
         } else {
            fprintf(stderr,"*** image %d is not rgb or byte. No scaling for you!\n", kkk%inimar->num);
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

   if (!(STRING_HAS_SUFFIX_CASE(prefix,".jpg"))) {
      sprintf(fnam,"%s.ppm",prefix);
   } else {
      sprintf(fnam,"%s",prefix);
   }
   fprintf(stderr,"+++ Writing image to %s\n", fnam);
   mri_write(fnam,im) ;
   
   mri_free(im); im = NULL;
   fprintf(stdout, "You can view image %s with:\n aiv %s \n", fnam, fnam);
    exit(0) ;
}
