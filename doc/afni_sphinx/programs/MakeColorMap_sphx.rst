.. _ahelp_MakeColorMap:

************
MakeColorMap
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage1: 
    MakeColorMap <-fn Fiducials_Ncol> [-pos] [-ah prefix] [-h/-help]
        Creates a colormap of N colors that contains the fiducial colors.
        -fn Fiducials_Ncol: Fiducial colors and their indices in the color
                            map are listed in file Fiducials_Ncol.
           Each row contains 4 tab delimited values:
           R G B i
           R G B values are between 0 and 1 and represent the 
           i-th color in the colormap. i should be between 0 and
           N-1, N being the total number of colors in the colormap.
    
    Usage2: 
    MakeColorMap <-f Fiducials> <-nc N> [-sl] [-ah prefix] [-h/-help]
        Creates a colormap of N colors that contains the fiducial colors.
        -f Fiducials:  Fiducial colors are listed in an ascii file Fiducials. 
           Each row contains 3 tab delimited R G B values between 0 and 1.
        -nc N: Total number of colors in the color map.
        -sl: (optional, default is NO) if used, the last color in the Fiducial 
           list is omitted. This is useful in creating cyclical color maps.
    
    Usage3: 
    MakeColorMap <-std MapName>
        Returns one of SUMA's standard colormaps. Choose from:
        rgybr20, ngray20, gray20, bw20, bgyr19, 
        matlab_default_byr64, roi128, roi256, roi64
     or if the colormap is in a .pal file:  
    MakeColorMap -cmapdb Palfile -cmap MapName
    
    Usage4:
    MakeColorMap <-fscolut lbl0 lbl1> 
                 [<-fscolutfile FS_COL_LUT>]
       Create AFNI/SUMA colormaps of FreeSurfer colors
       indexed between lbl0 and lbl1. 
       -fscolut lbl0 lbl1: Get colors indexed between
                            lbl0 and lbl1, non existing
                            integer labels are given a 
                            gray color. Use -fscolut -1 -1 to
                            get all the colors and labels.
       -fscolutfile FS_COL_LUT: Use color LUT file FS_COL_LUT
                                Default is to use 
                                $FREESURFER_HOME/FreeSurferColorLUT.txt
       -show_fscolut: Show all of the LUT
    
    Common options to all usages:
        -ah prefix: (optional, Afni Hex format.
                     default is RGB values in decimal form)
           use this option if you want a color map formatted to fit 
           in AFNI's .afnirc file. The colormap is written out as 
          prefix_01 = #xxxxxxx 
          prefix_02 = #xxxxxxx
           etc...
        -ahc prefix: optional, Afni Hex format, ready to go into.
                     pbardefs.h 
        -h or -help: displays this help message.
        -flipud: Flip the map upside down. If the colormap is being 
                 created for interactive loading into SUMA with the 'New'
                 button from the 'Surface Controller' you will need
                 to flip it upside down. 
        -usercolutfile USER_COL_LUT: A user's own color lookup file.
                 The format of the file is similar to FreeSurfer's ColorLUT.txt
                 It is an ascii file whith each line containing the following:
                    Key   R  G  B  A  Label
                 With Key being an integer color/region identifier,
                 Label is the string identifier and R,G,B,A are the colors 
                 and alpha values either between 0 and 1, or 0 and 255.
                 Alpha values are ignored at the moment, but they must be 
                 in the file.
        -suma_cmap: write colormap in SUMA's format
        -sdset DSET: Add colormap to surface-based dataset DSET, making it a
                     Labeled data set, which gets special treatment in SUMA.
                     A labeled data set can only have one value per node.
        -sdset_prefix DSET_PREF: Prefix of dset for writing labeled version
                                 of DSET. Without it, the new name is based on
                                 DSET's name
    
    Example Usage 1: Creating a colormap of 20 colors that goes from 
    Red to Green to Blue to Yellow to Red.
    
       The file FidCol_Nind contains the following:
       1 0 0 0
       0 1 0 5
       0 0 1 10
       1 1 0 15
       1 0 0 19
    
       The following command will generate the RGB colormap in decimal form:
       MakeColorMap -fn FidCol_Nind 
    
       The following command will generate the colormap and write it as 
       an AFNI color palette file:
       MakeColorMap -fn FidCol_Nind -ah TestPalette > TestPalette.pal
    
    Example Usage 2: Creating a cyclical version of the colormap in usage 1:
    
       The file FidCol contains the following:
       1 0 0
       0 1 0
       0 0 1
       1 1 0
       1 0 0
    
       The following command will generate the RGB colormap in decimal form:
       MakeColorMap -f FidCol -sl -nc 20 
    
    Example Usage 3: 
       MakeColorMap -std ngray20 
    
    Example Usage 4: 
       MakeColorMap -fscolut 0 255
    
    Example Usage 5: Make your own colormap and add it to a surface-based dset
       Say you have your own color lookup table formatted much like FreeSurfer's
       color lookup files. The content of this sample colut.txt file is:
        #integer label    String Label      R    G    B    A
         1                Big_House         0.3  0.1  1    1
         2                Small_Face        1    0.2  0.4  1
         3                Electric          1    1    0    1
         4                Atomic            0.1  1    0.3  1
    
       The command to create a SUMA formatted colormap would be:
           MakeColorMap -usercolutfile colut.txt -suma_cmap toylut 
    
       You can attach the colormap to a surface-based datatset with 
       ConvertDset's -labelize option, or you can also do it here in one
       pass with:
           MakeColorMap -usercolutfile colut.txt -suma_cmap toylut \
                        -sdset you_look_marvellous.niml.dset
    
    Adding a new colormap into AFNI:To read in a new colormap into AFNI, either paste the contents of 
    TestPalette.pal in your .afnirc file or read the .pal file using 
    AFNI as follows:
    1- run afni
    2- Define Function --> right click on Inten (over colorbar) 
       --> Read in palette (choose TestPalette.pal)
    3- set the #colors chooser (below colorbar) to 20 (the number of colors in 
       TestPalette.pal).
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
       Mar 22 2018
    
        Ziad S. Saad & Rick R. Reynolds SSCC/NIMH/NIH saadz@mail.nih.gov    Tue Apr 23 14:14:48 EDT 2002
