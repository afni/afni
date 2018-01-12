.. contents:: 
    :depth: 4 

**********
IsoSurface
**********

.. code-block:: none

    
    Usage: A program to perform isosurface extraction from a volume.
      Based on code by Thomas Lewiner (see below).
    
      IsoSurface  < -input VOL | -shape S GR >
                  < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >
                  [< -o_TYPE PREFIX>] [-Tsmooth KPB NITER]
                  [< -debug DBG >]
    
      Mandatory parameters:
         You must use one of the following two options:
         -input VOL: Input volume.
         -shape S GR: Built in shape.
                      where S is the shape number, 
                      between 0 and 9 (see below). 
                      and GR is the grid size (like 64).
                      If you use -debug 1 with this option
                      a .1D volume called mc_shape*.1D is
                      written to disk. Watch the debug output
                      for a command suggesting how to turn
                      this 1D file into a BRIK volume for viewing
                      in AFNI.
         You must use one of the following iso* options:
         -isorois: Create isosurface for each unique value in the input volume
                   This outputs multiple surfaces that are automatically named
                   using PREFIX and the label and key corresponding to each of
                   the ROIs to the extent that they are available.
                   Example:
               IsoSurface -isorois -input TTatlas+tlrc'[0]' -o_gii auto.all
               suma -onestate -i auto.all.k*.gii
                   You can also follow -isorois with the only ROI keys you want
                   considered as in:
               IsoSurface -isorois 276 277 54 input TTatlas+tlrc'[0]'-o_gii auto
         -isorois+dsets: Same as -isorois, but also write out a labeled dataset 
                         for each surface.               Example:
               IsoSurface -isorois+dsets -input TTatlas+tlrc'[0]' -o_gii auto.all
               suma -onestate -i auto.all.k*.gii
         -mergerois [LAB_OUT]: Combine all surfaces from isorois into one surface
                     If you specify LAB_OUT then a dataset with ROIs labels is 
                     also written out and named LAB_OUT
                    Example:
               IsoSurface -isorois -mergerois auto.all.niml.dset \
                          -input TTatlas+tlrc'[0]' -o_gii auto.all
               suma -i auto.all.gii
         -mergerois+dset: Same as -mergerois PREFIX, where PREFIX is the
                          prefix of the name you will use for the output surface.
                          The reason for this convenience function is that
                          suma now automatically loads a dataset that has the 
                          same prefix as the loaded surface and this option saves
                          you from having to manually match the names.
    
         -isoval V: Create isosurface where volume = V
         -isorange V0 V1: Create isosurface where V0 <= volume < V1
         -isocmask MASK_COM: Create isosurface where MASK_COM != 0
            For example: -isocmask '-a VOL+orig -expr (1-bool(a-V))' 
            is equivalent to using -isoval V. 
         NOTE: -isorange and -isocmask are only allowed with -xform mask
                See -xform below for details.
         NOTE NOTE: Sometimes you can get the equivalent of the negative space
                    of what you're expecting. If that is the case, try padding
                    your volume with zeros on all sides and try again.
                    You can do the padding with something like:
                    3dZeropad -I 2 -S 2 -R 2 -L 2 -A 2 -P 2 -prefix ... 
    
         -Tsmooth KPB NITER: Smooth resultant surface using the Taubin smoothing
                             approach in SurfSmooth and with parameters KPB and 
                             NITER. If unsure, try -Tsmooth 0.1 100 for a start.
      Optional Parameters:
         -autocrop: Crop input volume (and any internally computed volume) before
                    doing IsoSurface extraction. When you're running the program
                    on largely empty datasets or with -isorois* then using 
                    -autocrop will make the program run faster. Either way 
                    the output should not change however.
         -remesh EDGE_FRACTION: Remesh the surface(s) to result in a surface with
                                N_edges_new = N_edges_old x EDGE_FRACTION
                                EDGE_FRACTION should be between 0.0 and 1.0
         -xform XFORM:  Transform to apply to volume values
                        before searching for sign change
                        boundary. XFORM can be one of:
                mask: values that meet the iso* conditions
                      are set to 1. All other values are set
                      to -1. This is the default XFORM.
                shift: subtract V from the dataset and then 
                       search for 0 isosurface. This has the
                       effect of constructing the V isosurface
                       if your dataset has a continuum of values.
                       This option can only be used with -isoval V.
                none: apply no transforms. This assumes that
                      your volume has a continuum of values 
                      from negative to positive and that you
                      are seeking to 0 isosurface.
                      This option can only be used with -isoval 0.
         -o_TYPE PREFIX: prefix of output surface.
            where TYPE specifies the format of the surface
            and PREFIX is, well, the prefix.
            TYPE is one of: fs, 1d (or vec), sf, ply.
            Default is: -o_ply 
    
     Specifying output surfaces using -o or -o_TYPE options: 
        -o_TYPE outSurf specifies the output surface, 
                TYPE is one of the following:
           fs: FreeSurfer ascii surface. 
           fsp: FeeSurfer ascii patch surface. 
                In addition to outSurf, you need to specify
                the name of the parent surface for the patch.
                using the -ipar_TYPE option.
                This option is only for ConvertSurface 
           sf: SureFit surface. 
               For most programs, you are expected to specify prefix:
               i.e. -o_sf brain. In some programs, you are allowed to 
               specify both .coord and .topo file names: 
               i.e. -o_sf XYZ.coord TRI.topo
               The program will determine your choice by examining 
               the first character of the second parameter following
               -o_sf. If that character is a '-' then you have supplied
               a prefix and the program will generate the coord and topo names.
           vec (or 1D): Simple ascii matrix format. 
                For most programs, you are expected to specify prefix:
                i.e. -o_1D brain. In some programs, you are allowed to 
                specify both coord and topo file names: 
                i.e. -o_1D brain.1D.coord brain.1D.topo
                coord contains 3 floats per line, representing 
                X Y Z vertex coordinates.
                topo contains 3 ints per line, representing 
                v1 v2 v3 triangle vertices.
           ply: PLY format, ascii or binary.
           stl: STL format, ascii or binary (see also STL under option -i_TYPE).
           byu: BYU format, ascii or binary.
           mni: MNI obj format, ascii only.
           gii: GIFTI format, ascii.
                You can also enforce the encoding of data arrays
                by using gii_asc, gii_b64, or gii_b64gz for 
                ASCII, Base64, or Base64 Gzipped. 
                If AFNI_NIML_TEXT_DATA environment variable is set to YES, the
                the default encoding is ASCII, otherwise it is Base64.
           obj: No support for writing OBJ format exists yet.
     Note that if the surface filename has the proper extension, 
     it is enough to use the -o option and let the programs guess
     the type from the extension.
    
    
         -debug DBG: debug levels of 0 (default), 1, 2, 3.
            This is no Rick Reynolds debug, which is oft nicer
            than the results, but it will do.
    
      Built In Shapes:
         0: Cushin
         1: Sphere
         2: Plane
         3: Cassini
         4: Blooby
         5: Chair
         6: Cyclide
         7: 2 Torus
         8: mc case
         9: Drip
    
      NOTE:
      The code for the heart of this program is a translation of:
      Thomas Lewiner's C++ implementation of the algorithm in:
      Efficient Implementation of Marching Cubes' Cases with Topological Guarantees
      by Thomas Lewiner, Helio Lopes, Antonio Wilson Vieira and Geovan Tavares 
      in Journal of Graphics Tools. 
      http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf
    
       [-novolreg]: Ignore any Rotate, Volreg, Tagalign, 
                    or WarpDrive transformations present in 
                    the Surface Volume.
       [-noxform]: Same as -novolreg
       [-setenv "'ENVname=ENVvalue'"]: Set environment variable ENVname
                    to be ENVvalue. Quotes are necessary.
                 Example: suma -setenv "'SUMA_BackgroundColor = 1 0 1'"
                    See also options -update_env, -environment, etc
                    in the output of 'suma -help'
      Common Debugging Options:
       [-trace]: Turns on In/Out debug and Memory tracing.
                 For speeding up the tracing log, I recommend 
                 you redirect stdout to a file when using this option.
                 For example, if you were running suma you would use:
                 suma -spec lh.spec -sv ... > TraceFile
                 This option replaces the old -iodbg and -memdbg.
       [-TRACE]: Turns on extreme tracing.
       [-nomall]: Turn off memory tracing.
       [-yesmall]: Turn on memory tracing (default).
      NOTE: For programs that output results to stdout
        (that is to your shell/screen), the debugging info
        might get mixed up with your results.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
    
