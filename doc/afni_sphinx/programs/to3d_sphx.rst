.. _ahelp_to3d:

****
to3d
****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: to3d [options] image_files ...
           Creates 3D datasets for use with AFNI from 2D image files
    
    ****** PLEASE NOTE ******************************************************
    ****** If you are convering DICOM files to AFNI or NIfTI datasets, ******
    ****** you will likely be happier using the Dimon program, which   ******
    ****** can properly organize the Dicom files for you (knock wood). ******
    ****** Example:                                                    ******
    ******   Dimon -infile_prefix im. -dicom_org -gert_create_dataset  ******
    ****** See the output of                                           ******
    ******   Dimon -help                                               ******
    ****** for more examples and the complete instructions for use.    ******
    
    The available options are
      -help   show this message
      -'type' declare images to contain data of a given type
              where 'type' is chosen from the following options:
           ANATOMICAL TYPES
             spgr == Spoiled GRASS
              fse == Fast Spin Echo
             epan == Echo Planar
             anat == MRI Anatomy
               ct == CT Scan
             spct == SPECT Anatomy
              pet == PET Anatomy
              mra == MR Angiography
             bmap == B-field Map
             diff == Diffusion Map
             omri == Other MRI
             abuc == Anat Bucket
           FUNCTIONAL TYPES
              fim == Intensity
             fith == Inten+Thr
             fico == Inten+Cor
             fitt == Inten+Ttest
             fift == Inten+Ftest
             fizt == Inten+Ztest
             fict == Inten+ChiSq
             fibt == Inten+Beta
             fibn == Inten+Binom
             figt == Inten+Gamma
             fipt == Inten+Poisson
             fbuc == Func-Bucket
                     [for paired (+) types above, images are fim first,]
                     [then followed by the threshold (etc.) image files]
    
      -statpar value value ... value [* NEW IN 1996 *]
         This option is used to supply the auxiliary statistical parameters
         needed for certain dataset types (e.g., 'fico' and 'fitt').  For
         example, a correlation coefficient computed using program 'fim2'
         from 64 images, with 1 ideal, and with 2 orts could be specified with
           -statpar 64 1 2
    
      -prefix  name      will write 3D dataset using prefix 'name'
      -session name      will write 3D dataset into session directory 'name'
      -geomparent fname  will read geometry data from dataset file 'fname'
                           N.B.: geometry data does NOT include time-dependence
      -anatparent fname  will take anatomy parent from dataset file 'fname'
    
      -nosave  will suppress autosave of 3D dataset, which normally occurs
               when the command line options supply all needed data correctly
    
      -nowritebrik  will suppress saving of the BRIK file. May be useful for
               realtime saving when symbolic links are used instead
    
      -view type [* NEW IN 1996 *]
        Will set the dataset's viewing coordinates to 'type', which
        must be one of these strings:  orig acpc tlrc
    
    TIME DEPENDENT DATASETS [* NEW IN 1996 *]
      -time:zt nz nt TR tpattern  OR  -time:tz nt nz TR tpattern
    
        These options are used to specify a time dependent dataset.
        '-time:zt' is used when the slices are input in the order
                   z-axis first, then t-axis.
        '-time:tz' is used when the slices are input in the order
                   t-axis first, then z-axis.
    
        nz  =  number of points in the z-direction (minimum 1)
        nt  =  number of points in the t-direction
                (thus exactly nt * nz slices must be read in)
        TR  =  repetition interval between acquisitions of the
                same slice, in milliseconds (or other units, as given below)
    
        tpattern = Code word that identifies how the slices (z-direction)
                   were gathered in time.  The values that can be used:
    
           alt+z = altplus    = alternating in the plus direction
           alt+z2             = alternating, starting at slice #1
           alt-z = altminus   = alternating in the minus direction
           alt-z2             = alternating, starting at slice #nz-2
           seq+z = seqplus    = sequential in the plus direction
           seq-z = seqminus   = sequential in the minus direction
           zero  = simult     = simultaneous acquisition
           FROM_IMAGE         = (try to) read offsets from input images
           @filename          = read temporal offsets from 'filename'
    
        For example if nz = 5 and TR = 1000, then the inter-slice
        time is taken to be dt = TR/nz = 200.  In this case, the
        slices are offset in time by the following amounts:
    
                        S L I C E   N U M B E R
          tpattern        0    1    2    3    4  Comment
          ----------   ---- ---- ---- ---- ----  -------------------------------
          altplus         0  600  200  800  400  Alternating in the +z direction
          alt+z2        400    0  600  200  800  Alternating, but starting at #1
          altminus      400  800  200  600    0  Alternating in the -z direction
          alt-z2        800  200  600    0  400  Alternating, starting at #nz-2 
          seqplus         0  200  400  600  800  Sequential  in the +z direction
          seqminus      800  600  400  200    0  Sequential  in the -z direction
          simult          0    0    0    0    0  All slices acquired at once
    
        If @filename is used for tpattern, then nz ASCII-formatted numbers are
        read from the file.  These are used to indicate the time offsets (in ms)
        for each slice. For example, if 'filename' contains
           0 600 200 800 400
        then this is equivalent to 'altplus' in the above example.
    
        Notes:
          * Time-dependent functional datasets are not yet supported by
              to3d or any other AFNI package software.  For many users,
              the proper dataset type for these datasets is '-epan'.
          * Time-dependent datasets with more than one value per time point
              (e.g., 'fith', 'fico', 'fitt') are also not allowed by to3d.
          * If you use 'abut' to fill in gaps in the data and/or to
              subdivide the data slices, you will have to use the @filename
              form for tpattern, unless 'simult' or 'zero' is acceptable.
          * At this time, the value of 'tpattern' is not actually used in
              any AFNI program.  The values are stored in the dataset
              .HEAD files, and will be used in the future.
          * The values set on the command line can't be altered interactively.
          * The units of TR can be specified by the command line options below:
                -t=ms or -t=msec  -->  milliseconds (the default)
                -t=s  or -t=sec   -->  seconds
                -t=Hz or -t=Hertz -->  Hertz (for chemical shift images?)
              Alternatively, the units symbol ('ms', 'msec', 's', 'sec',
                'Hz', or 'Hertz') may be attached to TR in the '-time:' option,
                as in '-time:zt 16 64 4.0sec alt+z'
     ****** 15 Aug 2005 ******
          * Millisecond time units are no longer stored in AFNI dataset
              header files.  For backwards compatibility, the default unit
              of TR (i.e., without a suffix 's') is still milliseconds, but
              this value will be converted to seconds when the dataset is
              written to disk.  Any old AFNI datasets that have millisecond
              units for TR will be read in to all AFNI programs with the TR
              converted to seconds.
    
      -Torg ttt = set time origin of dataset to 'ttt' [default=0.0]
    
    COMMAND LINE GEOMETRY SPECIFICATION [* NEW IN 1996 *]
       -xFOV   [dimen1][direc1]-[dimen2][direc2]
         or       or
       -xSLAB  [dimen1][direc1]-[direc2]
    
       (Similar -yFOV, -ySLAB, -zFOV and -zSLAB option are also present.)
    
     These options specify the size and orientation of the x-axis extent
     of the dataset.  [dimen#] means a dimension (in mm); [direc] is
     an anatomical direction code, chosen from
          A (Anterior)    P (Posterior)    L (Left)
          I (Inferior)    S (Superior)     R (Right)
     Thus, 20A-30P means that the x-axis of the input images runs from
     20 mm Anterior to 30 mm Posterior.  For convenience, 20A-20P can be
     abbreviated as 20A-P.
    
     -xFOV  is used to mean that the distances are from edge-to-edge of
              the outermost voxels in the x-direction.
     -xSLAB is used to mean that the distances are from center-to-center
              of the outermost voxels in the x-direction.
    
     Under most circumstance, -xFOV , -yFOV , and -zSLAB would be the
     correct combination of geometry specifiers to use.  For example,
     a common type of run at MCW would be entered as
        -xFOV 120L-R -yFOV 120A-P -zSLAB 60S-50I
    
     **NOTE WELL: -xFOV 240L-R does not mean a Field-of-View that is 240 mm
                   wide!  It means one that stretches from 240R to 240L, and
                   so is 480 mm wide.
                  The 'FOV' indicates that this direction was acquired with
                   with Fourier encoding, and so the distances are naturally
                   specified from the edge of the volume.
                  The 'SLAB' indicates that this direction was acquired with
                   slice encoding (by the RF excitation), and so distances
                   are naturally specified by the center of the slices.
                  For non-MRI data (e.g., CT), I'm not sure what the correct
                   input format to use here would be -- be careful out there!
    
    Z-AXIS SLICE OFFSET ONLY
     -zorigin distz  Puts the center of the 1st slice off at the
                     given distance ('distz' in mm).  This distance
                     is in the direction given by the corresponding
                     letter in the -orient code.  For example,
                       -orient RAI -zorigin 30
                     would set the center of the first slice at
                     30 mm Inferior.
        N.B.: This option has no effect if the FOV or SLAB options
              described above are used.
    
    INPUT IMAGE FORMATS [* SIGNIFICANTLY CHANGED IN 1996 *]
      Image files may be single images of unsigned bytes or signed shorts
      (64x64, 128x128, 256x256, 512x512, or 1024x1024) or may be grouped
      images (that is, 3- or 4-dimensional blocks of data).
      In the grouped case, the string for the command line file spec is like
    
        3D:hglobal:himage:nx:ny:nz:fname   [16 bit input]
        3Ds:hglobal:himage:nx:ny:nz:fname  [16 bit input, swapped bytes]
                       (consider also -ushort2float for unsigned shorts)
        3Db:hglobal:himage:nx:ny:nz:fname  [ 8 bit input]
        3Di:hglobal:himage:nx:ny:nz:fname  [32 bit input]
        3Df:hglobal:himage:nx:ny:nz:fname  [floating point input]
        3Dc:hglobal:himage:nx:ny:nz:fname  [complex input]
        3Dd:hglobal:himage:nx:ny:nz:fname  [double input]
    
      where '3D:' or '3Ds': signals this is a 3D input file of signed shorts
            '3Db:'          signals this is a 3D input file of unsigned bytes
            '3Di:'          signals this is a 3D input file of signed ints
            '3Df:'          signals this is a 3D input file of floats
            '3Dc:'          signals this is a 3D input file of complex numbers
                             (real and imaginary pairs of floats)
            '3Dd:'          signals this is a 3D input file of double numbers
                             (will be converted to floats)
            hglobal = number of bytes to skip at start of whole file
            himage  = number of bytes to skip at start of each 2D image
            nx      = x dimension of each 2D image in the file
            ny      = y dimension of each 2D image in the file
            nz      = number of 2D images in the file
            fname   = actual filename on disk to read
    
      * The ':' separators are required.  The k-th image starts at
          BYTE offset hglobal+(k+1)*himage+vs*k*nx*ny in file 'fname'
          for k=0,1,...,nz-1.
      * Here, vs=voxel length=1 for bytes, 2 for shorts, 4 for ints and floats,
          and 8 for complex numbers.
      * As a special case, hglobal = -1 means read data starting at
          offset len-nz*(vs*nx*ny+himage), where len=file size in bytes.
          (That is, to read the needed data from the END of the file.)
      * Note that there is no provision for skips between data rows inside
          a 2D slice, only for skips between 2D slice images.
      * The int, float, and complex formats presume that the data in
          the image file are in the 'native' format for this CPU; that is,
          there is no provision for data conversion (unlike the 3Ds: format).
      * Double input will be converted to floats (or whatever -datum is)
          since AFNI doesn't support double precision datasets.
      * Whether the 2D image data is interpreted as a 3D block or a 3D+time
          block depends on the rest of the command line parameters.  The
          various 3D: input formats are just ways of inputting multiple 2D
          slices from a single file.
      * SPECIAL CASE: If fname is ALLZERO, then this means not to read
          data from disk, but instead to create nz nx*ny images filled
          with zeros.  One application of this is to make it easy to create
          a dataset of a specified geometry for use with other programs.
      * ENVIRONMENT VARIABLE: You can set an environment variable
          (e.g., AFNI_IMSIZE_1) to put a '3D:' type of prefix in front
          of any filename whose file has a given size.  For example,
            setenv AFNI_IMSIZE_1 16384=3D:0:0:64:64:1
          means that any input file of size 16384 bytes will be read
          as a 64x64 image of floats.
    
    The 'raw pgm' image format is also supported; it reads data into 'byte' images.
    
    * ANALYZE (TM) .hdr/.img files can now be read - give the .hdr filename on
      the command line.  The program will detect if byte-swapping is needed on
      these images, and can also set the voxel grid sizes from the first .hdr file.
      If the 'funused1' field in the .hdr is positive, it will be used to scale the
      input values.  If the environment variable AFNI_ANALYZE_FLOATIZE is YES, then
      .img files will be converted to floats on input.
    
    * Siemens .ima image files can now be read.  The program will detect if
      byte-swapping is needed on these images, and can also set voxel grid
      sizes and orientations (correctly, I hope).
    * Some Siemens .ima files seems to have their EPI slices stored in
      spatial order, and some in acquisition (interleaved) order.  This
      program doesn't try to figure this out.  You can use the command
      line option '-sinter' to tell the program to assume that the images
      in a single .ima file are interleaved; for example, if there are
      7 images in a file, then without -sinter, the program will assume
      their order is '0 1 2 3 4 5 6'; with -sinter, the program will
      assume their order is '0 2 4 6 1 3 5' (here, the number refers
      to the slice location in space).
    
    * GEMS I.* (IMGF) 16-bit files can now be read. The program will detect
      if byte-swapping is needed on these images, and can also set voxel
      grid sizes and orientations.  It can also detect the TR in the
      image header.  If you wish to rely on this TR, you can set TR=0
      in the -time:zt or -time:tz option.
    * If you use the image header's TR and also use @filename for the
      tpattern, then the values in the tpattern file should be fractions
      of the true TR; they will be multiplied by the true TR once it is
      read from the image header.
    
     NOTES:
      * Not all AFNI programs support all datum types.  Shorts and
          floats are safest. (See the '-datum' option below.)
      * If '-datum short' is used or implied, then int, float, and complex
          data will be scaled to fit into a 16 bit integer.  If the '-gsfac'
          option below is NOT used, then each slice will be SEPARATELY
          scaled according to the following choice:
          (a) If the slice values all fall in the range -32767 .. 32767,
              then no scaling is performed.
          (b) Otherwise, the image values are scaled to lie in the range
              0 .. 10000 (original slice min -> 0, original max -> 10000).
          This latter option is almost surely not what you want!  Therefore,
          if you use the 3Di:, 3Df:, or 3Dc: input methods and store the
          data as shorts, I suggest you supply a global scaling factor.
          Similar remarks apply to '-datum byte' scaling, with even more force.
      * To3d now incoporates POSIX filename 'globbing', which means that
          you can input filenames using 'escaped wildcards', and then to3d
          will internally do the expansion to the list of files.  This is
          only desirable because some systems limit the number of command-line
          arguments to a program.  It is possible that you would wish to input
          more slice files than your computer supports.  For example,
              to3d exp.?.*
          might overflow the system command line limitations.  The way to do
          this using internal globbing would be
              to3d exp.\?.\*
          where the \ characters indicate to pass the wildcards ? and *
          through to the program, rather than expand them in the shell.
          (a) Note that if you choose to use this feature, ALL wildcards in
              a filename must be escaped with \ or NONE must be escaped.
          (b) Using the C shell, it is possible to turn off shell globbing
              by using the command 'set noglob' -- if you do this, then you
              do not need to use the \ character to escape the wildcards.
          (c) Internal globbing of 3D: file specifiers is supported in to3d.
              For example, '3D:0:0:64:64:100:sl.\*' could be used to input
              a series of 64x64x100 files with names 'sl.01', 'sl.02' ....
              This type of expansion is specific to to3d; the shell will not
              properly expand such 3D: file specifications.
          (d) In the C shell (csh or tcsh), you can use forward single 'quotes'
              to prevent shell expansion of the wildcards, as in the command
                  to3d '3D:0:0:64:64:100:sl.*'
        The globbing code is adapted from software developed by the
        University of California, Berkeley, and is copyrighted by the
        Regents of the University of California (see file mcw_glob.c).
    
    RGB datasets [Apr 2002]
    -----------------------
    You can now create RGB-valued datasets.  Each voxel contains 3 byte values
    ranging from 0..255.  RGB values may be input to to3d in one of two ways:
     * Using raw PPM formatted 2D image files.
     * Using JPEG formatted 2D files.
     * Using TIFF, BMP, GIF, PNG formatted 2D files [if netpbm is installed].
     * Using the 3Dr: input format, analogous to 3Df:, etc., described above.
    RGB datasets can be created as functional FIM datasets, or as anatomical
    datasets:
     * RGB fim overlays are transparent in AFNI only where all three
        bytes are zero - that is, you can't overlay solid black.
     * At present, there is limited support for RGB datasets.
        About the only thing you can do is display them in 2D slice
        viewers in AFNI.
    You can also create RGB-valued datasets using program 3dThreetoRGB.
    
    Other Data Options
    ------------------
      -2swap
         This option will force all input 2 byte images to be byte-swapped
         after they are read in.
      -4swap
         This option will force all input 4 byte images to be byte-swapped
         after they are read in.
      -8swap
         This option will force all input 8 byte images to be byte-swapped
         after they are read in.
      BUT PLEASE NOTE:
         Input images that are auto-detected to need byte-swapping
         (GEMS I.*, Siemens *.ima, ANALYZE *.img, and 3Ds: files)
         will NOT be swapped again by one of the above options.
         If you want to swap them again for some bizarre reason,
         you'll have to use the 'Byte Swap' button on the GUI.
         That is, -2swap/-4swap will swap bytes on input files only
         if they haven't already been swapped by the image input
         function.
    
      -zpad N   OR
      -zpad Nmm 
         This option tells to3d to write 'N' slices of all zeros on each side
         in the z-direction.  This will make the dataset 'fatter', but make it
         simpler to align with datasets from other scanning sessions.  This same
         function can be accomplished later using program 3dZeropad.
       N.B.: The zero slices will NOT be visible in the image viewer in to3d, but
              will be visible when you use AFNI to look at the dataset.
       N.B.: If 'mm' follows the integer N, then the padding is measured in mm.
              The actual number of slices of padding will be rounded up.  So if
              the slice thickness is 5 mm, then '-zpad 16mm' would be the equivalent
              of '-zpad 4' -- that is, 4 slices on each z-face of the volume.
       N.B.: If the geometry parent dataset was created with -zpad, the spatial
              location (origin) of the slices is set using the geometry dataset's
              origin BEFORE the padding slices were added.  This is correct, since
              you need to set the origin on the current dataset as if the padding
              slices were not present.
       N.B.: Unlike the '-zpad' option to 3drotate and 3dvolreg, this adds slices
              only in the z-direction.
       N.B.: You can set the environment variable 'AFNI_TO3D_ZPAD' to provide a
              default for this option.
    
      -gsfac value
         will scale each input slice by 'value'.  For example,
         '-gsfac 0.31830989' will scale by 1/Pi (approximately).
         This option only has meaning if one of '-datum short' or
         '-datum byte' is used or implied.  Otherwise, it is ignored.
    
      -datum type
         will set the voxel data to be stored as 'type', which is currently
         allowed to be short, float, byte, or complex.
         If -datum is not used, then the datum type of the first input image
         will determine what is used.  In that case, the first input image will
         determine the type as follows:
            byte       --> byte
            short      --> short
            int, float --> float
            complex    --> complex
         If -datum IS specified, then all input images will be converted
         to the desired type.  Note that the list of allowed types may
         grow in the future, so you should not rely on the automatic
         conversion scheme.  Also note that floating point datasets may
         not be portable between CPU architectures.
    
      -nofloatscan
         tells to3d NOT to scan input float and complex data files for
         illegal values - the default is to scan and replace illegal
         floating point values with zeros (cf. program float_scan).
    
      -in:1
         Input of huge 3D: files (with all the data from a 3D+time run, say)
         can cause to3d to fail from lack of memory.  The reason is that
         the images are from a file are all read into RAM at once, and then
         are scaled, converted, etc., as needed, then put into the final
         dataset brick.  This switch will cause the images from a 3D: file
         to be read and processed one slice at a time, which will lower the
         amount of memory needed.  The penalty is somewhat more I/O overhead.
    
    NEW IN 1997:
      -orient code
         Tells the orientation of the 3D volumes.  The code must be 3 letters,
         one each from the pairs {R,L} {A,P} {I,S}.  The first letter gives
         the orientation of the x-axis, the second the orientation of the
         y-axis, the third the z-axis:
            R = right-to-left         L = left-to-right
            A = anterior-to-posterior P = posterior-to-anterior
            I = inferior-to-superior  S = superior-to-inferior
         Note that the -xFOV, -zSLAB constructions can convey this information.
    
    NEW IN 2001:
      -skip_outliers
         If present, this tells the program to skip the outlier check that is
         automatically performed for 3D+time datasets.  You can also turn this
         feature off by setting the environment variable AFNI_TO3D_OUTLIERS
         to "No".
      -text_outliers
        If present, tells the program to only print out the outlier check
         results in text form, not graph them.  You can make this the default
         by setting the environment variable AFNI_TO3D_OUTLIERS to "Text".
        N.B.: If to3d is run in batch mode, then no graph can be produced.
              Thus, this option only has meaning when to3d is run with the
              interactive graphical user interface.
      -save_outliers fname
        Tells the program to save the outliers count into a 1D file with
        name 'fname'.  You could graph this file later with the command
           1dplot -one fname
        If this option is used, the outlier count will be saved even if
        nothing appears 'suspicious' (whatever that means).
      NOTES on outliers:
        * See '3dToutcount -help' for a description of how outliers are
           defined.
        * The outlier count is not done if the input images are shorts
           and there is a significant (> 1%) number of negative inputs.
        * There must be at least 6 time points for the outlier count to
           be carried out.
    
    OTHER NEW OPTIONS:
      -assume_dicom_mosaic
        If present, this tells the program that any Siemens DICOM file
        is a potential MOSAIC image, even without the indicator string.
      -oblique_origin
        assume origin and orientation from oblique transformation matrix
        rather than traditional cardinal information (ignores FOV/SLAB
        options Sometimes useful for Siemens mosaic flipped datasets
      -reverse_list
        reverse the input file list.
        Convenience for Siemens non-mosaic flipped datasets
    
      -use_last_elem
        If present, search DICOM images for the last occurance of each
        element, not the first.
      -use_old_mosaic_code
        If present, do not use the Dec 2010 updates to siemens mosaic code.
        By default, use the new code if this option is not provided.
      -ushort2float
        Convert input shorts to float, and add 2^16 to any negatives.
      -verb
        show debugging information for reading DICOM files
    
    
    OPTIONS THAT AFFECT THE X11 IMAGE DISPLAY
       -gamma gg    the gamma correction factor for the
                      monitor is 'gg' (default gg is 1.0; greater than
                      1.0 makes the image contrast larger -- this may
                      also be adjusted interactively)
       -ncolors nn  use 'nn' gray levels for the image
                      displays (default is 80)
       -xtwarns     turn on display of Xt warning messages
       -quit_on_err Do not launch interactive to3d mode if input has errors.
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
