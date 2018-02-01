************
xmat_tool.py
************

.. _xmat_tool.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    =============================================================================
    xmat_tool.py    - a tool for evaluating an AFNI X-matrix
    
       This program gives the user the ability to evaluate a regression matrix
       (often referred to as an X-matrix).  With an AFNI X-matrix specified via
       -load_xmat, optionally along with an MRI time series specified via
       -load_1D, this program can display the:
    
             o  matrix condition numbers
             o  correlation matrix
             o  warnings regarding the correlation matrix
             o  cosine matrix (normalized XtX)
             o  warnings regarding the cosine matrix
             o  beta weights for fit against 1D time series
             o  fit time series
    
       --------------------------------------------------------------------------
       examples:
    
          Note that -no_gui is applied in each example, so that the program
          performs any requested actions and terminates, without opening a GUI
          (graphical user interface).
    
          0. Basic commands:
    
                xmat_tool.py -help
                xmat_tool.py -help_gui
                xmat_tool.py -hist
                xmat_tool.py -show_valid_opts
                xmat_tool.py -test
                xmat_tool.py -test_libs
                xmat_tool.py -ver
    
          1. Load an X-matrix and display the condition numbers.
    
                xmat_tool.py -no_gui -load_xmat X.xmat.1D -show_conds
    
          2. Load an X-matrix and display correlation and cosine warnings.
    
                xmat_tool.py -no_gui -load_xmat X.xmat.1D      \
                    -show_cormat_warnings -show_cosmat_warnings
    
          3. Load an X-matrix and a 1D time series.  Display beta weights for
             the best fit to all regressors (specifed as columns 0 to the last).
    
                xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D \
                    -choose_cols '0..$' -show_fit_betas
    
          4. Similar to 3, but show the actual fit time series.  Also, redirect
             the output to save the results in a 1D file.
    
                xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D \
                    -choose_cols '0..$' -show_fit_ts > fitts.1D
    
          5. Show many things.  Load an X-matrix and time series, and display
             conditions and warnings (but setting own cutoff values), as well as
             fit betas.
    
                xmat_tool.py -no_gui -load_xmat X.xmat.1D -load_1D norm.ts.1D  \
                    -choose_cols '0..$'                                        \
                    -show_conds                                                \
                    -cormat_cutoff 0.3 -cosmat_cutoff 0.25                     \
                    -show_cormat_warnings -show_cosmat_warnings                \
                    -show_fit_betas
    
          6. Script many operations.  Load a sequence of X-matrices, and display
             condition numbers and warnings for each.
    
             Note that with -chrono, options are applied chronologically.
    
                xmat_tool.py -no_gui -chrono                                \
                    -load_xmat X.1.xmat.1D                                  \
                    -show_conds -show_cormat_warnings -show_cosmat_warnings \
                    -load_xmat X.2.xmat.1D                                  \
                    -show_conds -show_cormat_warnings -show_cosmat_warnings \
                    -load_xmat X.3.xmat.1D                                  \
                    -show_conds -show_cormat_warnings -show_cosmat_warnings \
                    -load_1D norm.ts.1D                                     \
                    -show_fit_betas                                         \
                    -choose_cols '0..$'                                     \
                    -show_fit_betas                                         \
                    -choose_cols '0..26,36..$'                              \
                    -show_fit_betas                                         \
                    -load_xmat X.2.xmat.1D                                  \
                    -choose_cols '0..$'                                     \
                    -show_fit_betas
    
       --------------------------------------------------------------------------
       basic informational options:
    
          -help                           : show this help
          -help_gui                       : show the GUI help
          -hist                           : show the module history
          -show_valid_opts                : show all valid options
          -test                           : run a basic test
                                   (requires X.xmat.1D and norm.022_043_012.1D)
          -test_libs                      : test for required python libraries
          -ver                            : show the version number
    
       ------------------------------------------
       general options:
    
          -choose_cols 'COLUMN LIST'      : select columns to fit against
    
              e.g. -choose_cols '0..$'
              e.g. -choose_cols '1..19(3),26,29,40..$'
    
              These columns will be used as the basis for the top condition
              number, as well as the regressor columns for fit computations.
    
              The column selection string should not contain spaces, and should
              be in the format of AFNI sub-brick selection.  Consider these
              examples
    
                  2..13           : 2,3,4,5,6,7,8,9,10,11,12,13
                  2..13(3)        : 2,5,8,11
                  3,7,11          : 3,7,11
                  20..$(4)        : 20,24,28,32 (assuming 33 columns, say)
    
          -chrono                         : apply options chronologically
    
              By default, the general options are applied before the show
              options, with the show options being in order.
    
              When the -chrono option is applied, all options are chronological,
              allowing the options to be applied as in a script.
    
              For example, a matrix could be loaded, and then a series of fit
              betas could be displayed by alternating a sequence of -choose_cols
              and -show_fit_betas options.
    
              Consider example 6.
    
          -cormat_cutoff CUTOFF           : set min cutoff for cormat warnings
    
              e.g. -cormat_cutoff 0.5
    
              By default, any value in the correlation matrix that is greater
              than or equal to 0.4 generates a warning.  This option can be used
              to override that minumum cutoff.
    
          -cosmat_cutoff CUTOFF           : set min cutoff for cosmat warnings
    
              e.g. -cosmat_cutoff 0.5
    
              By default, any value in the cosine matrix that is greater than or
              equal to 0.3827 generates a warning.  This option can be used to
              override that minumum cutoff.
    
              Note a few cosine values, relative to 90 degrees (PI/2):
    
                  cos(.50 *PI/2) = .707
                  cos(.75 *PI/2) = .3827
                  cos(.875*PI/2) = .195
    
          -cosmat_motion                  : include motion in cosmat warnings
    
              In the cosine matrix, motion regressors are often pointing in a
              direction close to that of either baseline or other motion
              regressors.  By default, such warnings are not displayed.
    
              Use this option to include all such warnings.
    
          -load_xmat XMAT.xmat.1D         : load the AFNI X-matrix
    
              e.g. -load_xmat X.xmat.1D
    
              Load the X-matrix, as the basis for most computations.
    
          -load_1D DATA.1D                : load the 1D time series
    
              e.g. -load_1D norm_ts.1D
    
              Load the 1D time series, for which fit betas and a fit time series
              can be generated.
    
          -no_gui                         : do not start the GUI
    
              By default, this program runs a graphical interface.  If the user
              wishes to perform some actions and terminate without starting the
              GUI, this option can be applied.
    
          -verb LEVEL                     : set the verbose level
    
              Specify how much extra text should be displayed regarding the
              internal operations.  Valid levels are currently 0..5, with 0
              meaning 'quiet', 1 being the default, and 5 being the most verbose.
    
     ------------------------------------------
     show options:
    
          -show_col_types                 : display columns by regressor types
    
              Show which columns are considered 'main', 'chosen', 'baseline'
              and 'motion'.  This would correspond to condition numbers.
    
          -show_conds                     : display a list of condition numbers
    
              The condition number is the ratio of the largest eigen value to
              the smallest.  It provides an indication of how sensitive results
              of linear regression are to small changes in the data.  Condition
              numbers will tend to be larger with regressors that are more highly
              correlated.
    
              This option requests to display condition numbers for the X-matrix,
              restricted to the given sets of columns (regressors):
    
                  - all regressors
                  - chosen regressors (if there are any)
                  - main regressors (non-baseline, non-motion)
                  - main + baseline (non-motion)
                  - main + motion   (non-baseline)
    
                  - motion + baseline
                  - baseline
                  - motion
    
          -show_cormat                    : show the correlation matrix
    
              Display the entire correlation matrix as text.
    
              For an N-regressor (N columns) matrix, the NxN correlation matrix
              has as its i,j entry the Pearson correlation between regressors
              i and j.  It is computed as the de-meaned, normalized XtX.
    
              Values near +/-1.0 are highly correlated (go up and down together,
              or in reverse).  A value of 0.0 would mean they are orthogonal.
    
          -show_cormat_warnings           : show correlation matrix warnings
    
              Correlations for regressor pairs that are highly correlated
              (abs(r) >= 0.4, say) are displayed, unless it is for a motion
              regressor with either another motion regressor or a baseline
              regressor.
    
          -show_cosmat                    : show the cosine matrix
    
              Display the entire cosine matrix as text.
    
              This is similar to the correlation matrix, but the values show the
              cosines of the angles between pairs of regressor vectors.  Values
              near 1 mean the regressors are "pointed in the same direction" (in
              M-dimensional space).  A value of 0 means they are at right angles,
              which is to say orthogonal.
             
          -show_cosmat_warnings           : show cosine matrix warnings
    
              Cosines for regressor pairs that are pointed similar directions
              (abs(cos) >= 0.3827, say) are displayed.
    
          -show_fit_betas                 : show fit betas
    
              If a 1D time series is specified, beta weights will be displayed as
              best fit parameters of the model (X-matrix) to the data (1D time
              series).  These values are the scalars by which the corresponding
              regressors are multiplied, in order to fit the data as closely as
              possibly (minimizing the sum of squared errors).
    
              Only chosen columns are fit to the data.
    
                  see -choose_cols
    
          -show_fit_ts                    : show fit time series
    
              Similar to showing beta weights, the actual fit time series can
              be displayed with this option.  The fit time series is the sum of
              each regressor multiplied by its corresponding beta weight.
    
              Only chosen columns are fit to the data.
    
                  see -choose_cols
    
          -show_xmat                      : display general X-matrix information
    
              This will display some general information that is stored in the
              .xmat.1D file.
    
          -show_1D                        : display general 1D information
    
              This will display some general information from the 1D time series
              file.
    
     ------------------------------------------
     GUI (graphical user interface) options:
    
          -gui_plot_xmat_as_one           : plot Xmat columns on single axis
    
    -----------------------------------------------------------------------------
    R Reynolds    October 2008
    =============================================================================
