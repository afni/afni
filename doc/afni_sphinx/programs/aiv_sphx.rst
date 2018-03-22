***
aiv
***

.. _ahelp_aiv:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: aiv [-v] [-q] [-title] [-p xxxx ] image ...
    
    AFNI Image Viewer program.
    
    Shows the 2D images on the command line in an AFNI-like image viewer.
    
    Can also read images in NIML '<MRI_IMAGE...>' format from a TCP/IP socket.
    
    Image file formats are those supported by to3d:
     * various MRI formats (e.g., DICOM, GEMS I.xxx)
     * raw PPM or PGM
     * JPEG (if djpeg is in the path)
     * GIF, TIFF, BMP, and PNG (if netpbm is in the path)
    
    The '-v' option will make aiv print out the image filenames
      as it reads them - this can be a useful progress meter if
      the program starts up slowly.
    
    The '-q' option tells the program to be very quiet.
    
    The '-pad' option tells the program to pad all input images
     (from the command line) to be the same size.
     Images that are much smaller than the largest image will
     also be inflated somewhat so as not to look tiny.
    In the form '-pad X Y', where 'X' and 'Y' are integers >= 64,
     then all images will be resized to fit inside those dimensions.
    
    The '-title WORD' option titles the window WORD. 
    The default is the name of the image file if only one is 
    specified on the command line. If many images are read in
    the default window title is 'Images'.
    
    The '-p xxxx' option will make aiv listen to TCP/IP port 'xxxx'
    for incoming images in the NIML '<MRI_IMAGE...>' format.  The
    port number must be between 1024 and 65535, inclusive.  For
    conversion to NIML '<MRI_IMAGE...>' format, see program im2niml.
    
    Normally, at least one image must be given on the command line.
    If the '-p xxxx' option is used, then you don't have to input
    any images this way; however, since the program requires at least
    one image to start up, a crude 'X' will be displayed.  When the
    first image arrives via the socket, the 'X' image will be replaced.
    Subsequent images arriving by socket will be added to the sequence.
    
    -----------------------------------------------------------------
    Sample program fragment, for sending images from one program
    into a copy of aiv (which that program also starts up):
    
    #include "mrilib.h"
    NI_stream ns; MRI_IMAGE *im; float *far; int nx,ny;
    system("aiv -p 4444 &");                               /* start aiv */
    ns = NI_stream_open( "tcp:localhost:4444" , "w" ); /* connect to it */
    while(1){
      /** ......... create 2D nx X ny data into the far array .........**/
      im = mri_new_vol_empty( nx , ny , 1 , MRI_float );  /* fake image */
      mri_fix_data_pointer( far , im );                  /* attach data */
      NI_element nel = mri_to_niml(im);      /* convert to NIML element */
      NI_write_element( ns , nel , NI_BINARY_MODE );     /* send to aiv */
      NI_free_element(nel); mri_clear_data_pointer(im); mri_free(im);
    }
    NI_stream_writestring( ns , "<ni_do ni_verb='QUIT'>" ) ;
    NI_stream_close( ns ) ;  /* do this, or the above, if done with aiv */
    
    -- Authors: RW Cox and DR Glen
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
