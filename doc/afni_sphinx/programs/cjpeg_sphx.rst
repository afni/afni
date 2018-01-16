*****
cjpeg
*****

.. _cjpeg:

.. contents:: 
    :depth: 4 

.. code-block:: none

    usage: cjpeg [switches] [inputfile]
    Switches (names may be abbreviated):
      -quality N[,...]   Compression quality (0..100; 5-95 is useful range)
      -grayscale     Create monochrome JPEG file
      -rgb           Create RGB JPEG file
      -optimize      Optimize Huffman table (smaller file, but slow compression)
      -progressive   Create progressive JPEG file
      -scale M/N     Scale image by fraction M/N, eg, 1/2
      -targa         Input file is Targa format (usually not needed)
    Switches for advanced users:
      -arithmetic    Use arithmetic coding
      -block N       DCT block size (1..16; default is 8)
      -rgb1          Create RGB JPEG file with reversible color transform
      -bgycc         Create big gamut YCC JPEG file
      -dct int       Use integer DCT method (default)
      -dct fast      Use fast integer DCT (less accurate)
      -dct float     Use floating-point DCT method
      -nosmooth      Don't use high-quality downsampling
      -restart N     Set restart interval in rows, or in blocks with B
      -smooth N      Smooth dithered input (N=1..100 is strength)
      -maxmemory N   Maximum memory to use (in kbytes)
      -outfile name  Specify name for output file
      -verbose  or  -debug   Emit debug output
    Switches for wizards:
      -baseline      Force baseline quantization tables
      -qtables file  Use quantization tables given in file
      -qslots N[,...]    Set component quantization tables
      -sample HxV[,...]  Set component sampling factors
