.. _ahelp_imcat:

*****
imcat
*****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    [7m** ERROR:[0m Too few options
    Usage: imcat [options] fname1 fname2 etc.
    Puts a set images into an image matrix (IM) 
     montage of NX by NY images.
     The minimum set of input is N images (N >= 1).
     If need be, the default is to reuse images until the desired
     NX by NY size is achieved. 
     See options -zero_wrap and -image_wrap for more detail.
     
    OPTIONS:
     ++ Options for editing, coloring input images:
      -scale_image SCALE_IMG: Multiply each image IM(i,j) in output
                              image matrix IM by the color or intensity
                              of the pixel (i,j) in SCALE_IMG.
      -scale_pixels SCALE_PIX: Multiply each pixel (i,j) in output image
                              by the color or intensity
                              of the pixel (i,j) in SCALE_IMG.
                              SCALE_IMG is automatically resized to the
                              resolution of the output image.
      -scale_intensity: Instead of multiplying by the color of 
                              pixel (i,j), use its intensity 
                              (average color)
      -gscale FAC: Apply FAC in addition to scaling of -scale_* options
      -rgb_out: Force output to be in rgb, even if input is bytes.
                This option is turned on automatically in certain cases.
      -res_in RX RY: Set resolution of all input images to RX by RY pixels.
                     Default is to make all input have the same
                     resolution as the first image.
      -respad_in RPX RPY: Like -res_in, but resample to the max while respecting
                          the aspect ratio, and then pad to achieve desired 
                          pixel count.
      -pad_val VAL: Set the padding value, should it be needed by -respad_in
                    to VAL. VAL is typecast to byte, default is 0, max is 255.
      -crop L R T B: Crop images by L (Left), R (Right), T (Top), B (Bottom)
                     pixels. Cutting is performed after any resolution change, 
                     if any, is to be done.
      -autocrop_ctol CTOL: A line is eliminated if none of its R G B values 
                           differ by more than CTOL% from those of the corner
                           pixel.
      -autocrop_atol ATOL: A line is eliminated if none of its R G B values 
                           differ by more than ATOL% from those of line
                           average.
      -autocrop: This option is the same as using both of -autocrop_atol 20 
                 and -autocrop_ctol 20
      NOTE: Do not mix -autocrop* options with -crop
            Cropping is determined from the 1st input image and applied to 
            to all remaining ones.
     ++ Options for output:
      -zero_wrap: If number of images is not enough to fill matrix
                  solid black images are used.
      -white_wrap: If number of images is not enough to fill matrix
                  solid white images are used.
      -gray_wrap GRAY: If number of images is not enough to fill matrix
                  solid gray images are used. GRAY must be between 0 and 1.0
      -image_wrap: If number of images is not enough to fill matrix
                   images on command line are reused (default)
      -rand_wrap: When reusing images to fill matrix, randomize the order
                  in refill section only.
      -prefix ppp = Prefix the output files with string 'ppp'
              Note: If the prefix ends with .1D, then a 1D file containing
                    the average of RGB values. You can view the output with
                    1dgrayplot.
      -matrix NX NY: Specify number of images in each row and column 
                     of IM at the same time. 
      -nx NX: Number of images in each row (3 for example below)
      -ny NY: Number of images in each column (4 for example below)
          Example: If 12 images appearing on the command line
                   are to be assembled into a 3x4 IM matrix they
                   would appear in this order:
                     0  1  2
                     3  4  5
                     6  7  8
                     9  10 11
        NOTE: The program will try to guess if neither NX nor NY 
              are specified.
      -matrix_from_scale: Set NX and NY to be the same as the 
                          SCALE_IMG's dimensions. (needs -scale_image)
      -gap G: Put a line G pixels wide between images.
      -gap_col R G B: Set color of line to R G B values.
                      Values range between 0 and 255.
    
    Example 0 (assuming afni is in ~/abin directory):
       Resizing an image:
       imcat -prefix big -res_in 1024 1024 \
             ~/abin/funstuff/face_zzzsunbrain.jpg 
       imcat -prefix small -res_in 64 64 \
             ~/abin/funstuff/face_zzzsunbrain.jpg 
       aiv small.ppm big.ppm 
    
    Example 1:
       Stitching together images:
        (Can be used to make very high resolution SUMA images.
         Read about 'Ctrl+r' in SUMA's GUI help.)
       imcat -prefix cat -matrix 14 12 \
             ~/abin/funstuff/face_*.jpg
       aiv cat.ppm
    
    Example 2:
       Stitching together 3 images getting rid of annoying white boundary:
    
       imcat -prefix surfview_pry3b.jpg -ny 1 -autocrop surfview.000[789].jpg
    
    Example 20 (assuming afni is in ~/abin directory):
       imcat -prefix bigcat.jpg -scale_image ~/abin/afnigui_logo.jpg \
             -matrix_from_scale -rand_wrap -rgb_out -respad_in 128 128 \
             -pad_val 128 ~/abin/funstuff/face_*.jpg 
       aiv   bigcat.jpg bigcat.jpg 
       Crop/Zoom in to see what was done. In practice, you want to use
       a faster image viewer to examine the result. Zooming on such
       a large image is not fast in aiv.
       Be careful with this toy. Images get real big, real quick.
    
    You can look at the output image file with
      afni -im ppp.ppm  [then open the Sagittal image window]
    
    Usage: imcat [options] fname1 fname2 etc.
    Puts a set images into an image matrix (IM) 
     montage of NX by NY images.
     The minimum set of input is N images (N >= 1).
     If need be, the default is to reuse images until the desired
     NX by NY size is achieved. 
     See options -zero_wrap and -image_wrap for more detail.
     
    OPTIONS:
     ++ Options for editing, coloring input images:
      -scale_image SCALE_IMG: Multiply each image IM(i,j) in output
                              image matrix IM by the color or intensity
                              of the pixel (i,j) in SCALE_IMG.
      -scale_pixels SCALE_PIX: Multiply each pixel (i,j) in output image
                              by the color or intensity
                              of the pixel (i,j) in SCALE_IMG.
                              SCALE_IMG is automatically resized to the
                              resolution of the output image.
      -scale_intensity: Instead of multiplying by the color of 
                              pixel (i,j), use its intensity 
                              (average color)
      -gscale FAC: Apply FAC in addition to scaling of -scale_* options
      -rgb_out: Force output to be in rgb, even if input is bytes.
                This option is turned on automatically in certain cases.
      -res_in RX RY: Set resolution of all input images to RX by RY pixels.
                     Default is to make all input have the same
                     resolution as the first image.
      -respad_in RPX RPY: Like -res_in, but resample to the max while respecting
                          the aspect ratio, and then pad to achieve desired 
                          pixel count.
      -pad_val VAL: Set the padding value, should it be needed by -respad_in
                    to VAL. VAL is typecast to byte, default is 0, max is 255.
      -crop L R T B: Crop images by L (Left), R (Right), T (Top), B (Bottom)
                     pixels. Cutting is performed after any resolution change, 
                     if any, is to be done.
      -autocrop_ctol CTOL: A line is eliminated if none of its R G B values 
                           differ by more than CTOL% from those of the corner
                           pixel.
      -autocrop_atol ATOL: A line is eliminated if none of its R G B values 
                           differ by more than ATOL% from those of line
                           average.
      -autocrop: This option is the same as using both of -autocrop_atol 20 
                 and -autocrop_ctol 20
      NOTE: Do not mix -autocrop* options with -crop
            Cropping is determined from the 1st input image and applied to 
            to all remaining ones.
     ++ Options for output:
      -zero_wrap: If number of images is not enough to fill matrix
                  solid black images are used.
      -white_wrap: If number of images is not enough to fill matrix
                  solid white images are used.
      -gray_wrap GRAY: If number of images is not enough to fill matrix
                  solid gray images are used. GRAY must be between 0 and 1.0
      -image_wrap: If number of images is not enough to fill matrix
                   images on command line are reused (default)
      -rand_wrap: When reusing images to fill matrix, randomize the order
                  in refill section only.
      -prefix ppp = Prefix the output files with string 'ppp'
              Note: If the prefix ends with .1D, then a 1D file containing
                    the average of RGB values. You can view the output with
                    1dgrayplot.
      -matrix NX NY: Specify number of images in each row and column 
                     of IM at the same time. 
      -nx NX: Number of images in each row (3 for example below)
      -ny NY: Number of images in each column (4 for example below)
          Example: If 12 images appearing on the command line
                   are to be assembled into a 3x4 IM matrix they
                   would appear in this order:
                     0  1  2
                     3  4  5
                     6  7  8
                     9  10 11
        NOTE: The program will try to guess if neither NX nor NY 
              are specified.
      -matrix_from_scale: Set NX and NY to be the same as the 
                          SCALE_IMG's dimensions. (needs -scale_image)
      -gap G: Put a line G pixels wide between images.
      -gap_col R G B: Set color of line to R G B values.
                      Values range between 0 and 255.
    
    Example 0 (assuming afni is in ~/abin directory):
       Resizing an image:
       imcat -prefix big -res_in 1024 1024 \
             ~/abin/funstuff/face_zzzsunbrain.jpg 
       imcat -prefix small -res_in 64 64 \
             ~/abin/funstuff/face_zzzsunbrain.jpg 
       aiv small.ppm big.ppm 
    
    Example 1:
       Stitching together images:
        (Can be used to make very high resolution SUMA images.
         Read about 'Ctrl+r' in SUMA's GUI help.)
       imcat -prefix cat -matrix 14 12 \
             ~/abin/funstuff/face_*.jpg
       aiv cat.ppm
    
    Example 2:
       Stitching together 3 images getting rid of annoying white boundary:
    
       imcat -prefix surfview_pry3b.jpg -ny 1 -autocrop surfview.000[789].jpg
    
    Example 20 (assuming afni is in ~/abin directory):
       imcat -prefix bigcat.jpg -scale_image ~/abin/afnigui_logo.jpg \
             -matrix_from_scale -rand_wrap -rgb_out -respad_in 128 128 \
             -pad_val 128 ~/abin/funstuff/face_*.jpg 
       aiv   bigcat.jpg bigcat.jpg 
       Crop/Zoom in to see what was done. In practice, you want to use
       a faster image viewer to examine the result. Zooming on such
       a large image is not fast in aiv.
       Be careful with this toy. Images get real big, real quick.
    
    You can look at the output image file with
      afni -im ppp.ppm  [then open the Sagittal image window]
