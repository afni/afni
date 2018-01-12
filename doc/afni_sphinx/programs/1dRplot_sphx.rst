.. contents:: 
    :depth: 4 

*******
1dRplot
*******

.. code-block:: none

    
    Usage:
    ------ 
     1dRplot is a program for plotting a 1D file
    Options in alphabetical order:
    ------------------------------
    
       -addavg:  Add line at average of column
    
       -col.color COL1 [COL2 ...]: Colors for each column in -input.
                                 COL? are integers for now.
    
       -col.grp 1Dfile or Rexp: integer labels defining column grouping
    
       -col.line.type LT1 [LT2 ...]: Line type for each column in -input.
                                 LT? are integers for now.
    
       -col.name NAME1 [NAME2 ...]: Name of each column in -input. 
            Special flags:
                 VOLREG: --> 'Roll Pitch Yaw I-S R-L A-P'
    
       -col.name.show : Show names of column in -input.
    
       -col.nozeros:  Do not plot all zeros columns
    
       -col.plot.char CHAR1 [CHAR2 ...] : Symbols for each column in -input.
                                 CHAR? are integers (usually 0-127), or
                                 characters + - I etc.
          See the following link for what CHAR? values you can use:
     http://stat.ethz.ch/R-manual/R-patched/library/graphics/html/points.html
    
       -col.plot.type PLOT_TYPE: Column plot type. 
                              'l' for line, 'p' for points, 'b' for both
    
       -col.text.lym LYM_TEXT: Text to be placed at left Y margin.
                             You need one string per column.
             Special Flags: You can also use COL.NAME to use column
                             names for the margin text, or you can use
                             COL.IND to use the colum's index in the file
    
       -col.text.rym RYM_TEXT: Text to be placed at right Y margin.
                             You need one string per column.
            See also Special Flags section under -col.text.lym
    
       -col.ystack:  Scale each column and offset it based on its
                    column index. This is useful for stacking
                    a large number of columns on one plot.
                   It is only carried out when graphing more
                   than one series with the -one option.
    
       -grid.show : Show grid.
    
       -grp.label GROUP1 [GROUP2 ...]: Labels assigned to each group.
                              Default is no labeling
    
       -help: this help message
    
       -i 1D_INPUT: file to plot. This field can have multiple
                      formats. See Data Strings section below.
                  1dRplot will automatically detect certain
                  1D files ouput by some programs such as 3dhistog
                  or 3ddot and adjust parameters accordingly.
    
       -input 1D_INPUT: Same as -i
    
       -input_delta 1D_INPUT: file containing value for error bars
    
       -input_type 1D_TYPE: Type of data in 1D file.
                 Choose from 'VOLREG', or 'XMAT'
    
       -leg.fontsize : fontsize for legend text.
    
       -leg.line.color : Color to use for items in legend.
                  Default is taken from column line color.
    
       -leg.line.type : Line type to use for items in legend.
                  Default is taken from column line types.
                  If you want no line, set -leg.line.type = 0
    
       -leg.names : Names to use for items in legend.
                  Default is taken from column names.
    
       -leg.ncol : Number of columns in legend.
    
       -leg.plot.char : plot characters to use for items in legend.
                  Default is taken from column plot character (-col.plot.char).
    
       -leg.position : Legend position. Choose from:
                     bottomright, bottom, bottomleft
                     left, topleft, top, topright, right,
                     and center
    
       -leg.show : Show legend.
    
       -load.Rdat RDAT: load data list from save.Rdat for reproducing plot.
                      Note that you cannot override the settings in RDAT,
                      unless you run in the interactive R mode. For example,
                      say you have dice.Rdat saved from a previous command
                      and you want to change P$nodisp to TRUE:
                   load('dice.Rdat'); P$nodisp <- TRUE; plot.1D.eng(P)
    
       -mat:  Display as matrix
    
       -matplot:  Display as matrix
    
       -msg.trace: Output trace information along with errors and notices
    
       -multi:  Put columns in separate graphs
    
       -multiplot:  Put columns in separate graphs
    
       -nozeros:  Do not plot all zeros time series
    
       -one:  Put all columns on one graph
    
       -oneplot:  Put all columns on one graph
    
       -prefix PREFIX: Output prefix. See also -save. 
    
       -row.name NAME1 [NAME2 ...]: Name of each row in -input. 
            For the moment, this is only used with -matplot
    
       -rowcol.name NAME1 [NAME2 ...]: Names of rows, same as name of columns.
            For the moment, this is only used with -matplot.
    
       -run_examples: Run all examples, one after the other.
    
       -save PREFIX: Save plot and quit
                        No need for -prefix with this option
    
       -save.Rdat : Save data list for reproducing plot in R.
                  You need to specify -prefix or -save
                  along with this option to set the prefix.
                  See also -load.Rdat
    
       -save.size width height: Save figure size in pixels
                        Default is 2000 2000
    
       -show_allowed_options: list of allowed options
    
       -title TITLE: Graph title. File name is used by default.
                   Use NONE to be sure no title is used.
    
       -TR TR: Sampling period, in seconds. 
    
       -verb VERB: VERB is an integer specifying verbosity level.
                 0 for quiet (Default). 1 or more: talkative.
    
       -x 1D_INPUT: x axis. You can also use the string 'ENUM'
                  to indicate that the x axis should go from
                  1 to N, the number of samples in -input
    
       -xax.label XLABEL: Label of X axis 
    
       -xax.lim MIN MAX [STEP]: Range of X axis, STEP is optional
    
       -xax.tic.text XTTEXT: X tics text
    
       -yax.label YLABEL: Label of Y axis
    
       -yax.lim MIN MAX [STEP]: Range of X axis, STEP is optional
    
       -yax.tic.text YTTEXT: Y tics text 
    
       -zeros:  Do  plot all zeros time series
    
    
    Data Strings:
    -------------
    You can specify input matrices and vectors in a variety of
    ways. The simplest is by specifying a .1D file with all 
    the trimmings of column and row selectors. You can also
    specify a string that gets evaluated on the fly. 
    For example: '1D: 1 4 8' evaluates to a vector of values 1 4 and 8.
    Also, you can use R expressions such as: 'R: seq(0,10,3)'   
    
    
    To download demo data from AFNI's website run this command:
    -----------------------------------------------------------
    curl -o demo.X.xmat.1D afni.nimh.nih.gov/pub/dist/edu/data/samples/X.xmat.1D
    curl -o demo.motion.1D afni.nimh.nih.gov/pub/dist/edu/data/samples/motion.1D
    
    
    Example 1 --- :
    -------------------------------- 
    1dRplot -input demo.X.xmat.1D'[5..10]'
    
    
    
    Example 2 --- :
    -------------------------------- 
    1dRplot  -input demo.X.xmat.1D'[5..10]' \
             -input_type XMAT
    
    
    
    Example 3 --- :
    -------------------------------- 
    1dRplot  -input demo.motion.1D \
             -input_type VOLREG
    
    
    
    Example 4 --- :
    -------------------------------- 
    1dRplot -input 'R:plot.1D.testmat(100, 10)'
    
    
    
    Example 5 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 5)' \
             -one 
    
    
    
    Example 6 --- :
    -------------------------------- 
    1dRplot -input 'R:plot.1D.testmat(100, 10)' \
             -one \
             -col.ystack
    
    
    
    Example 7 --- :
    -------------------------------- 
    1dRplot -input 'R:plot.1D.testmat(100, 10)' \
             -one \
             -col.ystack \
             -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \
             -grp.label slow medium fast \
             -prefix ta.jpg \
             -yax.lim 0 18 \
             -leg.show \
             -leg.position top 
    
    
    
    Example 8 --- :
    -------------------------------- 
    1dRplot -input 'R:plot.1D.testmat(100, 10)' \
             -one \
             -col.ystack \
             -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \
             -grp.label slow medium fast \
             -prefix tb.jpg \
             -yax.lim 0 18 \
             -leg.show \
             -leg.position top \
             -nozeros \
             -addavg 
    
    
    
    Example 9 --- :
    -------------------------------- 
    1dRplot -input 'R:plot.1D.testmat(100, 10)' \
             -one \
             -col.ystack \
             -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \
             -grp.label slow medium fast \
             -prefix tb.jpg \
             -yax.lim 0 18 \
             -leg.show \
             -leg.position top \
             -nozeros \
             -addavg \
             -col.text.lym Tutti mi chiedono tutti mi vogliono \
                           Donne ragazzi vecchi fanciulle \
             -col.text.rym "R:paste('Col',seq(1,10), sep='')" 
    
    
    
    Example 10 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 2)' \
             -one \
             -col.plot.char 2 \
             -col.plot.type p  
    
    
    
    Example 11 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 2)' \
             -one \
             -col.line.type 3 \
             -col.plot.type l 
    
    
    
    Example 12 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 2)' \
             -one \
             -col.plot.char 2 \
             -col.line.type 3 \
             -col.plot.type b 
    
    
    
    Example 13 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 2)' \
             -one \
             -col.plot.char 2 5\
             -col.line.type 3 4\
             -col.plot.type b \
             -TR 2 
    
    
    
    Example 14 --- :
    -------------------------------- 
    1dRplot  -input 'R:plot.1D.testmat(100, 2)' \
             -one -col.plot.char 2 -col.line.type 3 \
             -col.plot.type b -TR 2 \
             -yax.tic.text 'numa numa numa numaei' \
             -xax.tic.text 'Alo'  'Salut' 'sunt eu' 'un haiduc'
    
    
