**********
ScaleToMap
**********

.. _ScaleToMap:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage:  ScaleToMap <-input IntFile icol vcol>  
        [-cmap MapType] [-cmapfile Mapfile] [-cmapdb Palfile] [-frf] 
        [-clp/-perc_clp clp0 clp1] [-apr/-anr range]
        [-interp/-nointerp/-direct] [-msk msk0 msk1] [-nomsk_col]
        [-msk_col R G B] [-br BrightFact]
        [-h/-help] [-verb] [-showmap] [-showdb]
    
        -input IntFile icol vcol: input data.
           Infile: 1D formatted ascii file containing node values
           icol: index of node index column 
           (-1 if the node index is implicit)
           vcol: index of node value column.
           Example: -input ValOnly.1D -1 0 
           for a 1D file containing node values
           in the first column and no node indices.
           Example: -input NodeVal.1D 1 3
           for a 1D file containing node indices in
           the SECOND column and node values in the 
           FOURTH column (index counting begins at 0)
        -v and -iv options are now obsolete.
           Use -input option instead.
        -cmap MapName: (optional, default RGYBR20) 
           choose one of the standard colormaps available with SUMA:
           RGYBR20, BGYR19, BW20, GRAY20, MATLAB_DEF_BYR64, 
           ROI64, ROI128
           You can also use AFNI's default paned color maps:
           The maps are labeled according to the number of 
           panes and their sign. Example: afni_p10
           uses the positive 10-pane afni colormap.
           afni_n10 is the negative counterpart.
           These maps are meant to be used with
           the options -apr and -anr listed below.
           You can also load non-default AFNI colormaps
           from .pal files (AFNI's colormap format); see option
           -cmapdb below.
        -cmapdb Palfile: read color maps from AFNI .pal file
           In addition to the default paned AFNI colormaps, you
           can load colormaps from a .pal file.
           To access maps in the Palfile you must use the -cmap option
           with the label formed by the name of the palette, its sign
           and the number of panes. For example, to following palette:
           ***PALETTES deco [13]
           should be accessed with -cmap deco_n13
           ***PALETTES deco [13+]
           should be accessed with -cmap deco_p13
        -cmapfile Mapfile: read color map from Mapfile.
           Mapfile:1D formatted ascii file containing colormap.
                   each row defines a color in one of two ways:
                   R  G  B        or
                   R  G  B  f     
           where R, G, B specify the red, green and blue values, 
           between 0 and 1 and f specifies the fraction of the range
           reached at this color. THINK values of right of AFNI colorbar.
           The use of fractions (it is optional) would allow you to create
           non-linear color maps where colors cover differing fractions of 
           the data range.
           Sample colormap with positive range only (a la AFNI):
                   0  0  1  1.0
                   0  1  0  0.8
                   1  0  0  0.6
                   1  1  0  0.4
                   0  1  1  0.2
           Note the order in which the colors and fractions are specified.
           The bottom color of the +ve colormap should be at the bottom of the
           file and have the lowest +ve fraction. The fractions here define a
           a linear map so they are not necessary but they illustrate the format
           of the colormaps.
           Comparable colormap with negative range included:
                   0  0  1   1.0
                   0  1  0   0.6
                   1  0  0   0.2
                   1  1  0  -0.2
                   0  1  1  -0.6
           The bottom color of the -ve colormap should have the 
           lowest -ve fraction. 
           You can use -1 -1 -1 for a color to indicate a no color
           (like the 'none' color in AFNI). Values mapped to this
           'no color' will be masked as with the -msk option.
           If your 1D color file has more than three or 4 columns,
           you can use the [] convention adopted by AFNI programs
           to select the columns you need.
        -frf: (optional) first row in file is the first color.
           As explained in the -cmapfile option above, the first 
           or bottom (indexed 0 )color of the colormap should be 
           at the bottom of the file. If the opposite is true, use
           the -frf option to signal that.
           This option is only useful with -cmapfile.
        -clp/-perc_clp clp0 clp1: (optional, default no clipping)
           clips values in IntVect. if -clp is used then values in vcol
           < clp0 are clipped to clp0 and > clp1 are clipped to clp1
           if -perc_clp is used them vcol is clipped to the values 
           corresponding to clp0 and clp1 percentile.
           The -clp/-prec_clp options are mutually exclusive with -apr/-anr.
        -apr range: (optional) clips the values in IntVect to [0 range].
           This option allows range of colormap to be set as in AFNI, 
           with Positive colorbar (Pos selected).
           This option is mutually exclusive with -clp/-perc_clp).
           set range = 0 for autoranging.
           If you use -apr and your colormap contains fractions, you
           must use a positive range colormap.
        -anr range: (optional) clips the values in IntVect to [-range range].
           This option allows range of colormap to be set as in AFNI, 
           with Negative colorbar (Pos NOT selected).
           This option is mutually exclusive with -clp/-perc_clp).
           set range = 0 for autoranging.
           If you use -anr and your colormap contains fractions, you
           must use a negative range colormap.
        -interp: (default) use color interpolation between colors in colormap
           If a value is assigned between two colors on the colorbar,
           it receives a color that is an interpolation between those two colors.
           This is the default behaviour in SUMA and AFNI when using the continuous
           colorscale. Mutually exclusive with -nointerp and -direct options.
        -nointerp: (optional) turns off color interpolation within the colormap
           Color assigniment is done a la AFNI when the paned colormaps are used.
           Mutually exclusive with -interp and -direct options.
        -direct: (optional) values (typecast to integers) are mapped directly
           to index of color in color maps. Example: value 4 is assigned
           to the 5th (index 4) color in the color map (same for values
           4.2 and 4.7). This mapping scheme is useful for ROI indexed type
           data. Negative data values are set to 0 and values >= N_col 
           (the number of colors in the colormap) are set to N_col -1
        -msk_zero: (optional) values that are 0 will get masked no matter
           what colormaps or mapping schemes you are using. 
           AFNI masks all zero values by default.
        -msk msk0 msk1: (optinal, default is no masking) 
           Values in vcol (BEFORE clipping is performed) 
           between [msk0 msk1] are masked by the masking color.
        -msk_col R G B: (optional, default is 0.3 0.3 0.3) 
           Sets the color of masked voxels.
        -nomsk_col: do not output nodes that got masked.
           It does not make sense to use this option with
           -msk_col.
        -br BrightFact: (optional, default is 1) 
           Applies a brightness factor to the colors 
           of the colormap and the mask color.
        -h or -help: displays this help message.
    
       The following options are for debugging and sanity checks.
        -verb: (optional) verbose mode.
        -showmap: (optional) print the colormap to the screen and quit.
           This option is for debugging and sanity checks.
           You can use MakeColorMap in Usage3 to write out a colormap
           in its RGB form.
        -showdb: (optional) print the colors and colormaps of AFNI
           along with any loaded from the file Palfile.
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
      
    
    
    Compile Date:
       Nov  9 2017
    
        Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov 
          July 31/02 
