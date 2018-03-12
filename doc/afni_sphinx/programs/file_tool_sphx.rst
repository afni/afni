*********
file_tool
*********

.. _file_tool:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    file_tool - display or modify sections of a file
    
        This program can be used to display or edit data in arbitrary
        files.  If no '-mod_data' option is provided (with DATA), it
        is assumed the user wishes only to display the specified data
        (using both '-offset' and '-length', or using '-ge_XXX').
    
      usage: file_tool [options] -infiles file1 file2 ...
    
      examples:
    
       ----- help examples -----
    
       1. get detailed help:
    
          file_tool -help
    
       2. get descriptions of GE struct elements:
    
          file_tool -help_ge
    
       ----- GEMS 4.x and 5.x display examples -----
    
       1. display GE header and extras info for file I.100:
    
          file_tool -ge_all -infiles I.100
    
       2. display GEMS 4.x series and image headers for file I.100:
    
          file_tool -ge4_all -infiles I.100
    
       3. display run numbers for every 100th I-file in this directory
    
          file_tool -ge_uv17 -infiles I.?42
          file_tool -ge_run  -infiles I.?42
    
       ----- general value display examples -----
    
       1. display the 32 characters located 100 bytes into each file:
    
          file_tool -offset 100 -length 32 -infiles file1 file2
    
       2. display the 8 4-byte reals located 100 bytes into each file:
    
          file_tool -disp_real4 -offset 100 -length 32 -infiles file1 file2
    
       3. display 8 2-byte hex integers, 100 bytes into each file:
    
          file_tool -disp_hex2 -offset 100 -length 16 -infiles file1 file2
    
       ----- ANALYZE file checking examples -----
    
       1. define the field contents of an ANALYZE header
    
          file_tool -def_ana_hdr
    
       2. display the field contents of an ANALYZE file
    
          file_tool -disp_ana_hdr -infiles dset.hdr
    
       3. display field differences between 2 ANALYZE headers
    
          file_tool -diff_ana_hdrs -infiles dset1.hdr dset2.hdr
    
       4. display field differences between 2 ANALYZE headers (in HEX)
    
          file_tool -diff_ana_hdrs -hex -infiles dset1.hdr dset2.hdr
    
       5. modify some fields of an ANALYZE file
    
          file_tool -mod_ana_hdr -prefix new.hdr -mod_field smin 0   \
             -mod_field descrip 'test ANALYZE file'           \
             -mod_field pixdim '0 2.1 3.1 4 0 0 0 0 0'        \
             -infiles old.hdr
    
       ----- script file checking examples -----
    
       0. check for any script issues (Unix, backslashes, chars)
          (-test is the same as -show_bad_all)
    
          file_tool -test -infiles my_scripts_*.txt
    
       1. in each file, check whether it is a UNIX file type
    
          file_tool -show_file_type -infiles my_scripts_*.txt
    
       2. in one file, convert a non-UNIX file type to UNIX
          (basically a dos2unix operation)
    
          file_tool -show_file_type -infile non.unix.txt -prefix is.unix.txt
    
       3. in each file, look for spaces after trailing backslashes '\'
    
          file_tool -show_bad_backslash -infiles my_scripts_*.txt
    
       4. in ONE file, correct spaces after trailing backslashes '\'
    
          file_tool -show_bad_backslash -infile scripts.txt -prefix s.fixed.txt
    
       ----- character modification examples -----
    
       1. in each file, change the 8 characters at 2515 to 'hi there':
    
          file_tool -mod_data "hi there" -offset 2515 -length 8 -infiles I.*
    
       2. in each file, change the 21 characters at 2515 to all 'x's
          (and print out extra debug info)
    
          file_tool -debug 1 -mod_data x -mod_type val -offset 2515 \
                    -length 21 -infiles I.*
    
       ----- raw number modification examples -----
    
      1. in each file, change the 3 short integers starting at position
         2508 to '2 -419 17'
    
          file_tool -mod_data '2 -419 17' -mod_type sint2 -offset 2508 \
                    -length 6 -infiles I.*
    
      2. in each file, change the 3 binary floats starting at position
         2508 to '-83.4 2 17' (and set the next 8 bytes to zero by
         setting the length to 20, instead of just 12).
    
          file_tool -mod_data '-83.4 2 17' -mod_type float4 -offset 2508 \
                    -length 20 -infiles I.*
    
      3. in each file, change the 3 binary floats starting at position
         2508 to '-83.4 2 17', and apply byte swapping
    
          file_tool -mod_data '-83.4 2 17' -mod_type float4 -offset 2508 \
                    -length 12 -swap_bytes -infiles I.*
    
      notes:
    
        o  Use of '-infiles' is required.
        o  Use of '-length' or a GE information option is required.
        o  As of this version, only modification with text is supported.
           Editing binary data is coming soon to a workstation near you.
    
      special options:
    
        -help              : show this help information
                           : e.g. -help
    
        -version           : show version information
                           : e.g. -version
    
        -hist              : show the program's modification history
    
        -debug LEVEL       : print extra info along the way
                           : e.g. -debug 1
                           : default is 0, max is 2
    
      required 'options':
    
        -infiles f1 f2 ... : specify input files to print from or modify
                           : e.g. -infiles file1
                           : e.g. -infiles I.*
    
              Note that '-infiles' should be the final option.  This is
              to allow the user an arbitrary number of input files.
    
      GE info options:
    
          -ge_all          : display GE header and extras info
          -ge_header       : display GE header info
          -ge_extras       : display extra GE image info
          -ge_uv17         : display the value of uv17 (the run #)
          -ge_run          : (same as -ge_uv17)
          -ge_off          : display file offsets for various fields
    
      GEMS 4.x info options:
    
          -ge4_all         : display GEMS 4.x series and image headers
          -ge4_image       : display GEMS 4.x image header
          -ge4_series      : display GEMS 4.x series header
          -ge4_study       : display GEMS 4.x study header
    
      ANALYZE info options:
    
          -def_ana_hdr     : display the definition of an ANALYZE header
          -diff_ana_hdrs   : display field differences between 2 headers
          -disp_ana_hdr    : display ANALYZE headers
          -hex             : display field values in hexidecimal
          -mod_ana_hdr     : modify ANALYZE headers
          -mod_field       : specify a field and value(s) to modify
    
          -prefix          : specify an output filename
          -overwrite       : specify to overwrite the input file(s)
    
      script file options:
    
          -show_bad_all : show lines with whitespace after '\'
    
              This is meant to find problems in script files where the
              script programmer has spaces or tabs after a final '\'
              on the line.  That would break the line continuation.
    
              The -test option is a shorthand version of this one.
    
          -show_bad_backslash : show lines with whitespace after '\'
    
              This is meant to find problems in script files where the
              script programmer has spaces or tabs after a final '\'
              on the line.  That would break the line continuation.
    
              ** If the -prefix option is specified, whitespace after
                 backslashes will be removed in the given output file.
    
                 This can also be used in conjunction with -overwrite.
    
              See also -prefix and -overwrite.
    
          -show_bad_char   : show any non-printable characters'\'
    
              Sometimes non-visible-but-detrimental characters appear
              in scripts due to editors or email programs.  This option
              helps to point out their presence to the user.
    
              See also -show_bad_all or -test.
    
          -show_file_type  : print file type of UNIX, Mac or DOS
    
              Shell scripts need to be UNIX type files.  This option
              will inform the programmer if there are end of line
              characters that define an alternate file type.
    
          -fix_rich_quotes y/n : replace rich-text quotes with ASCII
    
                   e.g. -fix_rich_quotes no
    
              Rich text quote values seem to be:
                   single: 0xe28098   or   0x e28099
                   double: 0xe2809c   or   0x e2809d
    
              In the case of scripts being fixed (e.g. -test -prefix P),
              rich-text quote characters will be replaced by ASCII
              quotes by default.  Use this option to turn off that
              behavior.
    
          -test  : short for -show_bad_all
    
              Check script files for known issues.
    
      raw ascii options:
    
        -length LENGTH     : specify the number of bytes to print/modify
                           : e.g. -length 17
    
              This includes numbers after the conversion to binary.  So
              if -mod_data is '2 -63 186', and -mod_type is 'sint2' (or
              signed shorts), then 6 bytes will be written (2 bytes for
              each of 3 short integers).
    
           ** Note that if the -length argument is MORE than what is
              needed to write the numbers out, the remaining length of
              bytes will be written with zeros.  If '17' is given for
              the length, and 3 short integers are given as data, there 
              will be 11 bytes of 0 written after the 6 bytes of data.
    
        -mod_data DATA     : specify a string to change the data to
                           : e.g. -mod_data hello
                           : e.g. -mod_data '2 -17.4 649'
                           : e.g. -mod_data "change to this string"
    
              This is the data that will be written into the modified
              file.  If the -mod_type is 'str' or 'char', then the
              output data will be those characters.  If the -mod_type
              is any other (i.e. a binary numerical format), then the
              output will be the -mod_data, converted from numerical
              text to binary.
    
           ** Note that a list of numbers must be contained in quotes,
              so that it will be processed as a single parameter.
    
        -mod_type TYPE     : specify the data type to write to the file
                           : e.g. -mod_type string
                           : e.g. -mod_type sint2
                           : e.g. -mod_type float4
                           : default is 'str'
    
            TYPE can be one of:
    
              str       : perform a string substitution
              char, val : perform a (repeated?) character substitution
              uint1     : single byte unsigned int   (binary write)
              sint1     : single byte   signed int   (binary write)
              uint2     : two    byte unsigned int   (binary write)
              sint2     : two    byte   signed int   (binary write)
              uint4     : four   byte unsigned int   (binary write)
              sint4     : four   byte   signed int   (binary write)
              float4    : four   byte floating point (binary write)
              float8    : eight  byte floating point (binary write)
    
              If 'str' is used, which is the default action, the data is
              replaced by the contents of the string DATA (from the
              '-mod_data' option).
    
              If 'char' is used, then LENGTH bytes are replaced by the
              first character of DATA, repeated LENGTH times.
    
              For any of the others, the list of numbers found in the
              -mod_data option will be written in the supplied binary
              format.  LENGTH must be large enough to accommodate this
              list.  And if LENGTH is higher, the output will be padded
              with zeros, to fill to the requested length.
    
        -offset OFFSET     : use this offset into each file
                           : e.g. -offset 100
                           : default is 0
    
              This is the offset into each file for the data to be
              read or modified.
    
        -quiet             : do not output header information
    
      numeric options:
    
        -disp_hex          : display bytes in hex
        -disp_hex1         : display bytes in hex
        -disp_hex2         : display 2-byte integers in hex
        -disp_hex4         : display 4-byte integers in hex
    
        -disp_int2         : display 2-byte integers
        -disp_int4         : display 4-byte integers
    
        -disp_real4        : display 4-byte real numbers
    
        -swap_bytes        : use byte-swapping on numbers
    
              If this option is used, then byte swapping is done on any
              multi-byte numbers read from or written to the file.
    
      - R Reynolds, version: 3.18 (August 23, 2015), compiled: Jan 29 2018
