.. contents:: 
    :depth: 4 

*******
1dNLfit
*******

.. code-block:: none

    
    Program to fit a model to a vector of data.  The model is given by a
    symbolic expression, with parameters to be estimated.
    
    Usage: 1dNLfit OPTIONS
    
    Options: [all but '-meth' are actually mandatory]
    --------
    
     -expr eee   = The expression for the fit.  It must contain one symbol from
                   'a' to 'z' which is marked as the independent variable by
                   option '-indvar', and at least one more symbol which is
                   a parameter to be estimated.
                   ++ Expressions use the same syntax as 3dcalc, ccalc, and 1deval.
                   ++ Note: expressions and symbols are not case sensitive.
    
     -indvar c d = Indicates which variable in '-expr' is the independent
                   variable.  All other symbols are parameters, which are
                   either fixed (constants) or variables to be estimated.
                   ++ Then, read the values of the independent variable from
                      1D file 'd' (only the first column will be used).
                   ++ If the independent variable has a constant step size,
                      you can input it via with 'd' replaced by a string like
                        '1D: 100%0:2.1'
                      which creates an array with 100 value, starting at 0,
                      then adding 2.1 for each step:
                        0 2.1 4.2 6.3 8.4 ...
    
     -param ppp  = Set fixed value or estimating range for a particular
                   symbol.
                   ++ For a fixed value, 'ppp' takes the form 'a=3.14', where the
                      first letter is the symbol name, which must be followed by
                      an '=', then followed by a constant expression.  This
                      expression can be symbolic, as in 'a=cbrt(3)'.
                   ++ For a parameter to be estimated, 'ppp' takes the form of
                      two constant expressions separated by a ':', as in
                      'q=-sqrt(2):sqrt(2)'.
                   ++ All symbols in '-expr' must have a corresponding '-param'
                      option, EXCEPT for the '-indvar' symbol (which will be set
                      by its data file).
    
     -depdata v  = Read the values of the dependent variable (to be fitted to
                   '-expr') from 1D file 'v'.
                   ++ File 'v' must have the same number of rows as file 'd'
                      from the '-indvar' option!
                   ++ File 'v' can have more than one column; each will be fitted
                      separately to the expression.
    
     -meth m     = Set the method for fitting: '1' for L1, '2' for L2.
                   (The default method is L2, which is usually better.)
    
    Example:
    --------
    Create a sin wave corrupted by logistic noise, to file ss.1D.
    Create a cos wave similarly, to file cc.1D.
    Put these files together into a 2 column file sc.1D.
    Fit both columns to a 3 parameter model and write the fits to file ff.1D.
    Plot the data and the fit together, for fun and profit(?).
    
    1deval -expr 'sin(2*x)+lran(0.3)' -del 0.1 -num 100 > ss.1D
    1deval -expr 'cos(2*x)+lran(0.3)' -del 0.1 -num 100 > cc.1D
    1dcat ss.1D cc.1D > sc.1D ; \rm ss.1D cc.1D
    1dNLfit -depdata sc.1D -indvar x '1D: 100%0:0.1' -expr 'a*sin(b*x)+c*cos(b*x)' \
            -param a=-2:2 -param b=1:3 -param c=-2:2  > ff.1D
    1dplot -one -del 0.1 -ynames sin:data cos:data sin:fit cos:fit - sc.1D ff.1D
    
    Notes:
    ------
    * PLOT YOUR RESULTS!  There is no guarantee that you'll get a good fit.
    
    * This program is not particularly efficient, so using it on a large
      scale (e.g., for lots of columns, or in a shell loop) will be slow.
    
    * The results (fitted time series models) are written to stdout,
      and should be saved by '>' redirection (as in the example).
      The first few lines of the output from the example are:
       # 1dNLfit output (meth=L2)
       # expr = a*sin(b*x)+c*cos(b*x)
       # Fitted parameters:
       # A =      1.0828     0.12786
       # B =      1.9681      2.0208
       # C =     0.16905      1.0102
       #     ----------- -----------
                 0.16905      1.0102
                 0.37753      1.0153
                 0.57142     0.97907
    
    * Coded by Zhark the Well-Fitted - during Snowzilla 2016.
