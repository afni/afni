*******
Vecwarp
*******

.. _Vecwarp:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: Vecwarp [options]
    Transforms (warps) a list of 3-vectors into another list of 3-vectors
    according to the options.  Error messages, warnings, and informational
    messages are written to stderr.  If a fatal error occurs, the program
    exits with status 1; otherwise, it exits with status 0.
    
    OPTIONS:
     -apar aaa   = Use the AFNI dataset 'aaa' as the source of the
                   transformation; this dataset must be in +acpc
                   or +tlrc coordinates, and must contain the
                   attributes WARP_TYPE and WARP_DATA which describe
                   the forward transformation from +orig coordinates
                   to the 'aaa' coordinate system.
                 N.B.: The +orig version of this dataset must also be
                       readable, since it is also needed when translating
                       vectors between SureFit and AFNI coordinates.
                       Only the .HEAD files are actually used.
    
     -matvec mmm = Read an affine transformation matrix-vector from file
                   'mmm', which must be in the format
                       u11 u12 u13 v1
                       u21 u22 u23 v2
                       u31 u32 u33 v3
                   where each 'uij' and 'vi' is a number.  The forward
                   transformation is defined as
                       [ xout ]   [ u11 u12 u13 ] [ xin ]   [ v1 ]
                       [ yout ] = [ u21 u22 u23 ] [ yin ] + [ v2 ]
                       [ zout ]   [ u31 u32 u33 ] [ zin ]   [ v3 ]
    
     Exactly one of -apar or -matvec must be used to specify the
     transformation.
    
     -forward    = -forward means to apply the forward transformation;
       *OR*        -backward means to apply the backward transformation
     -backward     * For example, if the transformation is specified by
                      '-apar fred+tlrc', then the forward transformation
                      is from +orig to +tlrc coordinates, and the backward
                      transformation is from +tlrc to +orig coordinates.
                   * If the transformation is specified by -matvec, then
                      the matrix-vector read in defines the forward
                      transform as above, and the backward transformation
                      is defined as the inverse.
                   * If neither -forward nor -backward is given, then
                      -forward is the default.
    
     -input iii  = Read input 3-vectors from file 'iii' (from stdin if
                   'iii' is '-' or the -input option is missing).  Input
                   data may be in one of the following ASCII formats:
    
                   * SureFit .coord files:
                       BeginHeader
                       lines of text ...
                       EndHeader
                       count
                       int x y z
                       int x y z
                       et cetera...
                     In this case, everything up to and including the
                     count is simply passed through to the output.  Each
                     (x,y,z) triple is transformed, and output with the
                     int label that precedes it.  Lines that cannot be
                     scanned as 1 int and 3 floats are treated as comments
                     and are passed to through to the output unchanged.
                 N.B.-1: For those using SureFit surfaces created after
                         the SureFit/Caret merger (post. 2005), you need
                         to use the flag -new_surefit. Talk to Donna about
                         this!
                 N.B.-2: SureFit coordinates are
                       x = distance Right    of Left-most      dataset corner
                       y = distance Anterior to Posterior-most dataset corner
                       z = distance Superior to Inferior-most  dataset corner
                     For example, if the transformation is specified by
                       -forward -apar fred+tlrc
                     then the input (x,y,z) are relative to fred+orig and the
                     output (x,y,z) are relative to fred+tlrc.  If instead
                       -backward -apar fred+tlrc
                     is used, then the input (x,y,z) are relative to fred+tlrc
                     and the output (x,y,z) are relative to fred+orig.
                     For this to work properly, not only fred+tlrc must be
                     readable by Vecwarp, but fred+orig must be as well.
                     If the transformation is specified by -matvec, then
                     the matrix-vector transformation is applied to the
                     (x,y,z) vectors directly, with no coordinate shifting.
    
                   * AFNI .1D files with 3 columns
                       x y z
                       x y z
                       et cetera...
                     In this case, each (x,y,z) triple is transformed and
                     written to the output.  Lines that cannot be scanned
                     as 3 floats are treated as comments and are passed
                     through to the output unchanged.
                   N.B.: AFNI (x,y,z) coordinates are in DICOM order:
                       -x = Right     +x = Left
                       -y = Anterior  +y = Posterior
                       -z = Inferior  +z = Superior
    
     -output ooo = Write the output to file 'ooo' (to stdout if 'ooo'
                   is '-', or if the -output option is missing).  If the
                   file already exists, it will not be overwritten unless
                   the -force option is also used.
    
     -force      = If the output file already exists, -force can be
                   used to overwrite it.  If you want to use -force,
                   it must come before -output on the command line.
    
    EXAMPLES:
    
      Vecwarp -apar fred+tlrc -input fred.orig.coord > fred.tlrc.coord
    
    This transforms the vectors defined in original coordinates to
    Talairach coordinates, using the transformation previously defined
    by AFNI markers.
    
      Vecwarp -apar fred+tlrc -input fred.tlrc.coord -backward > fred.test.coord
    
    This does the reverse transformation; fred.test.coord should differ from
    fred.orig.coord only by roundoff error.
    
    Author: RWCox - October 2001
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
