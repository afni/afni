******
1dplot
******

.. _ahelp_1dplot:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dplot [options] tsfile ...
    Graphs the columns of a *.1D time series file to the X11 screen.
    
    -------
    OPTIONS
    -------
     -install   = Install a new X11 colormap.
     -sep       = Plot each column in a separate sub-graph.
     -one       = Plot all columns together in one big graph.
                    [default = -sep]
     -sepscl    = Plot each column in a separate sub-graph
                  and allow each sub-graph to have a different
                  y-scale.  -sepscl is meaningless with -one!
     -noline    = Don't plot the connecting lines (also implies '-box').
     -NOLINE    = Same as '-noline', but will not try to plot values outside
                  the rectangular box that contains the graph axes.
     -box       = Plot a small 'box' at each data point, in addition
                  to the lines connecting the points.
                 * The box size can be set via the environment variable
                   AFNI_1DPLOT_BOXSIZE; the value is a fraction of the
                   overall plot size.  The standard box size is 0.006.
                   Example with a bigger box:
                     1dplot -DAFNI_1DPLOT_BOXSIZE=0.01 -box A.1D
                 * The box shapes are different for different time
                   series columns.  At present, there is no way to
                   control which shape is used for what column
                   (unless you modify the source code, that is).
                 * You can set environment variable AFNI_1DPLOT_RANBOX
                   to YES to get the '-noline' boxes plotted in a
                   pseudo-random order, so that one particular color
                   doesn't dominate just because it is last in the
                   plotting order; for example:
            1dplot -DAFNI_1DPLOT_RANBOX=YES -one -x X.1D -noline Y1.1D Y2.1D Y3.1D
    
     -hist      = Plot graphs in histogram style (i.e., vertical boxes).
                 * Histograms can be generated from 3D or 1D files using
                   program 3dhistog; for example
                    3dhistog -nbin 50 -notitle -min 0 -max .04 err.1D > eh.1D
                    1dplot -hist -x eh.1D'[0]' -xlabel err -ylabel hist eh.1D'[1]'
                   or, for something a little more fun looking:
                    1dplot -one -hist -dashed 1:2 -x eh.1D'[0]' \
                           -xlabel err -ylabel hist eh.1D'[1]' eh.1D'[1]'
    
               ** The '-norm' options below can be useful for plotting data
                   with different value ranges on top of each other via '-one':
     -norm2     = Independently scale each time series plotted to
                  have L_2 norm = 1 (sum of squares).
     -normx     = Independently scale each time series plotted to
                  have max absolute value = 1 (L_infinity norm).
     -norm1     = Independently scale each time series plotted to
                  have max sum of absolute values = 1 (L_1 norm).
     -demean    = This option will remove the mean from each time series
                  (before normalizing).  The combination '-demean -normx -one'
                  can be useful when plotting disparate data together.
                 * If you use '-demean' twice, you will get linear detrending.
                 * Et cetera (e.g,, 4 times gives you cubic detrending.)
    
     -x  X.1D   = Use for X axis the data in X.1D.
                  Note that X.1D should have one column
                  of the same length as the columns in tsfile. 
                ** Coupled with '-box -noline', you can use '-x' to make
                   a scatter plot, as in graphing file A1.1D along the
                   x-axis and file A2.1D along the y-axis:
                     1dplot -box -noline -x A1.1D -xlabel A1 -ylabel A2 A2.1D
                ** '-x' will override -dx and -xzero; -xaxis still works
    
     -xl10 X.1D = Use log10(X.1D) as the X axis.
    
     -xmulti X1.1D X2.1D ...
                  This new [Oct 2013] option allows you to plot different
                  columns from the data with different values along the
                  x-axis.  You can supply one or more 1D files after the
                  '-xmulti' option.  The columns from these files are
                  catenated, and then the first xmulti column is used as
                  as x-axis values for the first data column plotted, the
                  second xmulti column gives the x-axis values for the
                  second data column plotted, and so on.
               ** The command line arguments after '-xmulti' are taken
                  as 1D filenames to read, until an argument starts with
                  a '-' character -- this would either be another option,
                  or just a single '-' to separate the xmulti 1D files
                  from the data files to be plotted.
               ** If you don't provide enough xmulti columns for all the
                  data files, the last xmulti column will be re-used.
               ** Useless but fun example:
                   1deval -num 100 -expr '(i-i)+z+gran(0,6)' > X1.1D
                   1deval -num 100 -expr '(i-i)+z+gran(0,6)' > X2.1D
                   1dplot -one -box -xmulti X1.1D X2.1D - X2.1D X1.1D
    
     -dx xx     = Spacing between points on the x-axis is 'xx'
                    [default = 1] SYNONYMS: '-dt' and '-del'
     -xzero zz  = Initial x coordinate is 'zz' [default = 0]
                    SYNONYMS: '-tzero' and '-start'
     -nopush    = Don't 'push' axes ranges outwards.
     -ignore nn = Skip first 'nn' rows in the input file
                    [default = 0]
     -use mm    = Plot 'mm' points [default = all of them]
     -xlabel aa = Put string 'aa' below the x-axis
                    [default = no axis label]
     -ylabel aa = Put string 'aa' to the left of the y-axis
                    [default = no axis label]
     -plabel pp = Put string 'pp' atop the plot.
                  Some characters, such as '_' have
                  special formatting effects. You 
                  can escape that with ''. For example:
            echo 2 4.5 -1 | 1dplot -plabel 'test_underscore' -stdin
                  versus
            echo 2 4.5 -1 | 1dplot -plabel 'test\_underscore' -stdin
     -title pp = Same as -plabel, but only works with -ps/-png/-jpg/-pnm options.
     -wintitle pp = Set string 'pp' as the title of the frame 
                    containing the plot. Default is based on input.
    
     -stdin     = Don't read from tsfile; instead, read from
                  stdin and plot it. You cannot combine input
                  from stdin and tsfile(s).  If you want to do so,
                  use program 1dcat first.
    
     -ps        = Don't draw plot in a window; instead, write it
                  to stdout in PostScript format.
                 * If you view the result in 'gv', you should turn
                   'anti-alias' off, and switch to landscape mode.
                 * You can use the 'gs' program to convert PostScript
                   to other formats; for example, a .bmp file:
                1dplot -ps ~/data/verbal/cosall.1D | 
                 gs -r100 -sOutputFile=fred.bmp -sDEVICE=bmp256 -q -dBATCH -
    
     -jpg fname  } = Render plot to an image and save to a file named
     -jpeg fname } = 'fname', in JPEG mode or in PNG mode or in PNM mode.
     -png fname  } = The default image width is 1024 pixels; to change
     -pnm fname  } = this value to 2000 pixels (say), do
                       setenv AFNI_1DPLOT_IMSIZE 2000
                     before running 1dplot.  Widths over 2000 may start
                     to look odd, and will run more slowly.
                   * PNG files will be smaller than JPEG, and are
                     compressed without loss.
                   * PNG output requires that the netpbm program
                     pnmtopng be installed somewhere in your PATH.
                   * PNM output files are not compressed, and are manipulable
                     by the netpbm package: http://netpbm.sourceforge.net/
    
     -pngs SIZE fname } = a convenience function equivalent to
     -jpgs SIZE fname } = setenv AFNI_1DPLOT_IMSIZE SIZE and 
     -jpegs SIZE fname} = -png (or -jpg or -pnm) fname
     -pnms SIZE fname }
    
     -ytran 'expr'   = Transform the data along the y-axis by
                       applying the expression to each input value.
                       For example:
                         -ytran 'log10(z)'
                       will take log10 of each input time series value
                       before plotting it.
                     * The expression should have one variable (any letter
                       from a-z will do), which stands for the time series
                       data to be transformed.
                     * An expression such as 'sqrt(x*x+i)' will use 'x'
                       for the time series value and use 'i' for the time
                       index (starting at 0) -- in this way, you can use
                       time-dependent transformations, if needed.
                     * This transformation applies to all input time series
                       (at present, there is no way to transform different
                       time series in distinct ways inside 1dplot).
                     * '-ytran' is applied BEFORE the various '-norm' options.
    
     -xtran 'expr'    = Similar, but for the x-axis.
                      ** Applies to '-xmulti' , '-x' , or the default x-axis.
    
     -xaxis b:t:n:m  = Set the x-axis to run from value 'b' to
                       value 't', with 'n' major divisions and
                       'm' minor tic marks per major division.
                       For example:
                         -xaxis 0:100:5:20
                       Setting 'n' to 0 means no tic marks or labels.
                     * You can set 'b' to be greater than 't', to
                       have the x-coordinate decrease from left-to-right.
                     * This is the only way to have this effect in 1dplot.
                     * In particular, '-dx' with a negative value will not work!
    
     -yaxis b:t:n:m  = Similar to above, for the y-axis.  These
                       options override the normal autoscaling
                       of their respective axes.
    
     -ynames a b ... = Use the strings 'a', 'b', etc., as
                       labels to the right of the graphs,
                       corresponding to each input column.
                       These strings CANNOT start with the
                       '-' character.
                 N.B.: Each separate string after '-ynames'
                       is taken to be a new label, until the
                       end of the command line or until some
                       string starts with a '-'.  In particular,
                       This means you CANNOT do something like
                         1dplot -ynames a b c file.1D
                       since the input filename 'file.1D' will
                       be used as a label string, not a filename.
                       Instead, you must put another option between
                       the end of the '-ynames' label list, OR you
                       can put a single '-' at the end of the label
                       list to signal its end:
                         1dplot -ynames a b c - file.1D
    
     -volreg         = Makes the 'ynames' be the same as the
                       6 labels used in plug_volreg for
                       Roll, Pitch, Yaw, I-S, R-L, and A-P
                       movements, in that order.
    
     -thick          = Each time you give this, it makes the line
                       thickness used for plotting a little larger.
                       [An alternative to using '-DAFNI_1DPLOT_THIK=...']
     -THICK          = Twice the power of '-thick' at no extra cost!!
    
     -dashed codes   = Plot dashed lines between data points.  The 'codes'
                       are a colon-separated list of dash values, which
                       can be 1 (solid), 2 (longer dashes), or 3 (shorter dashes).
                    ** Example: '-dashed 1:2:3' means to plot the first time
                       series with solid lines, the second with long dashes,
                       and the third with short dashes.
    
     -Dname=val      = Set environment variable 'name' to 'val'
                       for this run of the program only:
     1dplot -DAFNI_1DPLOT_THIK=0.01 -DAFNI_1DPLOT_COLOR_01=blue '1D:3 4 5 3 1 0'
    
    You may also select a subset of columns to display using
    a tsfile specification like 'fred.1D[0,3,5]', indicating
    that columns #0, #3, and #5 will be the only ones plotted.
    For more details on this selection scheme, see the output
    of '3dcalc -help'.
    
    Example: graphing a 'dfile' output by 3dvolreg, when TR=5:
       1dplot -volreg -dx 5 -xlabel Time 'dfile[1..6]'
    
    You can also input more than one tsfile, in which case the files
    will all be plotted.  However, if the files have different column
    lengths, the shortest one will rule.
    
    The colors for the line graphs cycle between black, red, green, and
    blue.  You can alter these colors by setting Unix environment
    variables of the form AFNI_1DPLOT_COLOR_xx -- cf. README.environment.
    You can alter the thickness of the lines by setting the variable
    AFNI_1DPLOT_THIK to a value between 0.00 and 0.05 -- the units are
    fractions of the page size; of course, you can also use the options
    '-thick' or '-THICK' if you prefer.
    
    ----------------
    RENDERING METHOD
    ----------------
    On 30 Apr 2012, a new method of rendering the 1dplot graph into an X11
    window was introduced -- this method uses 'anti-aliasing' to produce
    smoother-looking lines and characters.  If you want the old coarser-looking
    rendering method, set environment variable AFNI_1DPLOT_RENDEROLD to YES.
    
    The program always uses the new rendering method when drawing to a JPEG
    or PNG or PNM file (which is not and never has been just a screen capture).
    There is no way to disable the new rendering method for image-file saves.
    
    ------
    LABELS
    ------
    Besides normal alphabetic text, the various labels can include some
    special characters, using TeX-like escapes starting with '\'.
    Also, the '^' and '_' characters denote super- and sub-scripts,
    respectively.  The following command shows many of the escapes:
     1deval -num 100 -expr 'J0(t/4)' | 1dplot -stdin -thick \
     -xlabel '\alpha\beta\gamma\delta\epsilon\zeta\eta^{\oplus\dagger}\times c' \
     -ylabel 'Bessel Function \green J_0(t/4)'     \
     -plabel '\Upsilon\Phi\Chi\Psi\Omega\red\leftrightarrow\blue\partial^{2}f/\partial x^2'
    
    TIMESERIES (1D) INPUT
    ---------------------
    A timeseries file is in the form of a 1D or 2D table of ASCII numbers;
    for example:   3 5 7
                   2 4 6
                   0 3 3
                   7 2 9
    This example has 4 rows and 3 columns.  Each column is considered as
    a timeseries in AFNI.  The convention is to store this type of data
    in a filename ending in '.1D'.
    
    ** COLUMN SELECTION WITH [] **
    When specifying a timeseries file to an command-line AFNI program, you
    can select a subset of columns using the '[...]' notation:
      'fred.1D[5]'            ==> use only column #5
      'fred.1D[5,9,17]'       ==> use columns #5, #9, and #17
      'fred.1D[5..8]'         ==> use columns #5, #6, #7, and #8
      'fred.1D[5..13(2)]'     ==> use columns #5, #7, #9, #11, and #13
    Column indices start at 0.  You can use the character '$'
    to indicate the last column in a 1D file; for example, you
    can select every third column in a 1D file by using the selection list
      'fred.1D[0..$(3)]'      ==> use columns #0, #3, #6, #9, ....
    
    ** ROW SELECTION WITH {} **
    Similarly, you select a subset of the rows using the '{...}' notation:
      'fred.1D{0..$(2)}'      ==> use rows #0, #2, #4, ....
    You can also use both notations together, as in
      'fred.1D[1,3]{1..$(2)}' ==> columns #1 and #3; rows #1, #3, #5, ....
    
    ** DIRECT INPUT OF DATA ON THE COMMAND LINE WITH 1D: **
    You can also input a 1D time series 'dataset' directly on the command
    line, without an external file. The 'filename' for such input has the
    general format
      '1D:n_1@val_1,n_2@val_2,n_3@val_3,...'
    where each 'n_i' is an integer and each 'val_i' is a float.  For
    example
       -a '1D:5@0,10@1,5@0,10@1,5@0'
    specifies that variable 'a' be assigned to a 1D time series of 35,
    alternating in blocks between values 0 and value 1.
     * Spaces or commas can be used to separate values.
     * A '|' character can be used to start a new input "line":
       Try 1dplot '1D: 3 4 3 5 | 3 5 4 3'
    
    ** TRANSPOSITION WITH \' **
    Finally, you can force most AFNI programs to transpose a 1D file on
    input by appending a single ' character at the end of the filename.
    N.B.: Since the ' character is also special to the shell, you'll
          probably have to put a \ character before it. Examples:
           1dplot '1D: 3 2 3 4 | 2 3 4 3'   and
           1dplot '1D: 3 2 3 4 | 2 3 4 3'\'
    When you have reached this level of understanding, you are ready to
    take the AFNI Jedi Master test.  I won't insult you by telling you
    where to find this examination.
    
    --------------
    MARKING BLOCKS (e.g., censored time points)
    --------------
    The following options let you mark blocks along the x-axis, by drawing
    colored vertical boxes over the standard white background.
     * The intended use is to mark blocks of time points that are censored
       out of an analysis, which is why the options are the same as those
       in 3dDeconvolve -- but you can mark blocks for any reason, of course.
     * These options don't do anything when the '-x' option is used to
       alter the x-axis spacings.
     * To see what the various color markings look like, try this silly example:
    
       1deval -num 100 -expr 'lran(2)' > zz.1D
       1dplot -thick -censor_RGB red    -CENSORTR 3-8   \
                     -censor_RGB green  -CENSORTR 11-16 \
                     -censor_RGB blue   -CENSORTR 22-27 \
                     -censor_RGB yellow -CENSORTR 34-39 \
                     -censor_RGB violet -CENSORTR 45-50 \
                     -censor_RGB pink   -CENSORTR 55-60 \
                     -censor_RGB gray   -CENSORTR 65-70 \
                     -censor_RGB #2cf   -CENSORTR 75-80 \
              -plabel 'red green blue yellow violet pink gray #2cf' zz.1D &
    
     -censor_RGB clr   = set the color used for the marking to 'clr', which
                         can be one of the strings below:
                           red green blue yellow violet pink gray (OR grey)
                       * OR 'clr' can be in the form '#xyz' or '#xxyyzz', where
                         'x', 'y', and 'z' are hexadecimal digits -- for example,
                         '#2cf' is sort of a cyan color.
                       * OR 'clr' can be in the form 'rgbi:rf/gf/bf' where
                         each color intensity (rf, gf, bf) is a number between
                         0.0 and 1.0 -- e.g., white is 'rgbi:1.0/1.0/1.0'.
                         Since the background is white, dark colors don't look
                         good here, and will obscure the graphs; for example,
                         pink is defined here as 'rgbi:1.0/0.5/0.5'.
                       * The default color is (a rather pale) yellow.
                       * You can use '-censor_RGB' more than once.  The color
                         most recently specified previous on the command line
                         is what will be used with the '-censor' and '-CENSORTR'
                         options.  This allows you to mark different blocks
                         with different colors (e.g., if they were censored
                         for different reasons).
                       * The feature of allowing multiple '-censor_RGB' options
                         means that you must put this option BEFORE the
                         relevant '-censor' and/or '-CENSORTR' options.
                         Otherwise, you'll get the default yellow color!
    
     -censor cname     = cname is the filename of censor .1D time series   
                       * This is a file of 1s and 0s, indicating which     
                         time points are to be un-marked (1) and which are 
                         to be marked (0).                                 
                       * Please note that only one '-censor' option can be 
                         used, for compatibility with 3dDeconvolve.        
                       * The option below may be simpler to use!           
                         (And can be used multiple times.)                 
    
     -CENSORTR clist   = clist is a list of strings that specify time indexes
                         to be marked in the graph(s).  Each string is of  
                         one of the following forms:                       
                               37 => mark global time index #37            
                             2:37 => mark time index #37 in run #2         
                           37..47 => mark global time indexes #37-47       
                           37-47  => same as above                         
                         *:0-2    => mark time indexes #0-2 in all runs    
                         2:37..47 => mark time indexes #37-47 in run #2    
                       * Time indexes within each run start at 0.          
                       * Run indexes start at 1 (just be to confusing).    
                       * Multiple -CENSORTR options may be used, or        
                         multiple -CENSORTR strings can be given at        
                         once, separated by spaces or commas.              
                       * Each argument on the command line after           
                         '-CENSORTR' is treated as a censoring string,     
                         until an argument starts with a '-' or an         
                         alphabetic character, or it contains the substring
                         '1D'.  This means that if you want to plot a file 
                         named '9zork.xyz', you may have to do this:       
                           1dplot -CENSORTR 3-7 18-22 - 9zork.xyz          
                         The stand-alone '-' will stop the processing      
                         of censor strings; otherwise, the '9zork.xyz'     
                         string, since it doesn't start with a letter,     
                         would be treated as a censoring string, which     
                         you would find confusing.                         
                      ** N.B.: 2:37,47 means index #37 in run #2 and       
                         global time index 47; it does NOT mean            
                         index #37 in run #2 AND index #47 in run #2.      
    
     -concat rname      = rname is the filename for list of concatenated runs
                          * 'rname' can be in the format                   
                              '1D: 0 100 200 300'                          
                            which indicates 4 runs, the first of which     
                            starts at time index=0, second at index=100,   
                            and so on.                                     
                          * The ONLY function of '-concat' is for use with 
                            '-CENSORTR', to be compatible with 3dDeconvolve
                              [e.g., for plotting motion parameters from]
                              [3dvolreg -1Dfile, where you've cat-enated]
                              [the 1D files from separate runs into one ]
                              [long file for plotting with this program.]
    
     -rbox x1 y1 x2 y2 color1 color2
                        = Draw a rectangular box with corners (x1,y1) to
                          (x2,y2), in color1, with an outline in color2.
                          Colors are names, such as 'green'.
                            [This option lets you make bar]
                            [charts, *if* you care enough.]
    
     -Rbox x1 y1 x2 y2 y3 color1 color2
                        = As above, with an extra horizontal line at y3.
    
     -line x1 y1 x2 y2 color dashcode
                        = Draw one line segment.
    
    Another fun fun example:
    
      1dplot -censor_RGB #ffa -CENSORTR '0-99'           \
             `1deval -1D: -num 61 -dx 0.3 -expr 'J0(x)'`
    
    which illustrates the use of 'censoring' to mark the entire graph
    background in pale yellow '#ffa', and also illustrates the use
    of the '-1D:' option in 1deval to produce output that can be
    used directly on the command line, via the backquote `...` operator.
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
