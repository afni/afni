*****
djpeg
*****

.. _djpeg:

.. contents:: 
    :depth: 4 

.. code-block:: none

    usage: djpeg [switches] [inputfile]
    Switches (names may be abbreviated):
      -colors N      Reduce image to no more than N colors
      -fast          Fast, low-quality processing
      -grayscale     Force grayscale output
      -rgb           Force RGB output
      -scale M/N     Scale output image by fraction M/N, eg, 1/8
      -bmp           Select BMP output format (Windows style)
      -gif           Select GIF output format
      -os2           Select BMP output format (OS/2 style)
      -pnm           Select PBMPLUS (PPM/PGM) output format (default)
      -targa         Select Targa output format
    Switches for advanced users:
      -dct int       Use integer DCT method (default)
      -dct fast      Use fast integer DCT (less accurate)
      -dct float     Use floating-point DCT method
      -dither fs     Use F-S dithering (default)
      -dither none   Don't use dithering in quantization
      -dither ordered  Use ordered dither (medium speed, quality)
      -map FILE      Map to colors used in named image file
      -nosmooth      Don't use high-quality upsampling
      -onepass       Use 1-pass quantization (fast, low quality)
      -maxmemory N   Maximum memory to use (in kbytes)
      -outfile name  Specify name for output file
