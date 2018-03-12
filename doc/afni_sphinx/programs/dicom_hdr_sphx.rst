*********
dicom_hdr
*********

.. _dicom_hdr:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: dicom_hdr [options] fname [...]
    Prints information from the DICOM file 'fname' to stdout.
    Multiple files can be given on the command line.
    
    OPTIONS:
     -hex     = Include hexadecimal printout for integer values.
     -noname  = Don't include element names in the printout.
     -sexinfo = Dump Siemens EXtra INFO text (0029 1020), if present
                 (can be VERY lengthy).
     -mulfram = Dump multi-frame information, if present
                 (1 line per frame, plus an XML-style header/footer)
                 [-mulfram also implies -noname]
     -v n     = Dump n words of binary data also.
    
     -no_length        = Skip lengths and offsets (helps diffs).
     -slice_times      = Show slice times from Siemens mosaic images.
     -slice_times_verb = Same, but be more verbose about it.
    
    Based on program dcm_dump_file from the RSNA, developed at
    the Mallinckrodt Institute of Radiology.  See the source
    code file mri_dicom_hdr.c for their Copyright and license.
    
    SOME SAMPLE OUTPUT LINES:
    
    0028 0010      2 [1234   ] //              IMG Rows// 512
    0028 0011      2 [1244   ] //           IMG Columns// 512
    0028 0030     18 [1254   ] //     IMG Pixel Spacing//0.488281\0.488281
    0028 0100      2 [1280   ] //    IMG Bits Allocated// 16
    0028 0101      2 [1290   ] //       IMG Bits Stored// 12
    0028 0102      2 [1300   ] //          IMG High Bit// 11
    
    * The first 2 numbers on each line are the DICOM group and element tags,
       in hexadecimal.
    * The next number is the number of data bytes, in decimal.
    * The next number [in brackets] is the offset in the file of the data,
       in decimal.  This is where the data bytes start, and does not include
       the tag, Value Representation, etc.
    * If -noname is NOT given, then the string in the '// ... //' region is
       the standard DICOM dictionary name for this data element.  If this string
       is blank, then this element isn't in the dictionary (e.g., is a private
       tag, or an addition to DICOM that the program doesn't know about, etc.).
    * The value after the last '//' is the value of the data in the element.
    * In the example above, we have a 512x512 image with 0.488281 mm pixels,
       with 12 bits (stored in 16 bits) per pixel.
    * For vastly more detail on DICOM standard, you can start with the
       documents at ftp://afni.nimh.nih.gov/dicom/ (1000+ pages of PDF)!
    * Also see program dicom_hinfo -- which will print out just a few user-chosen
       values for each input file.  It can be used in a script to sort through
