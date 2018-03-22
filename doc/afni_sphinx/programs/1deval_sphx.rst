******
1deval
******

.. _1deval:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1deval [options] -expr 'expression'
    Evaluates an expression that may include columns of data
    from one or more text files and writes the result to stdout.
    
    ** Only a single column can be used for each input 1D file. **
    *  Simple multiple column operations (e.g., addition, scaling)
        can be done with program 1dmatcalc.
    *  Any single letter from a-z can be used as the independent
        variable in the expression.
    *  Unless specified using the '[]' notation (cf. 1dplot -help),
        only the first column of an input 1D file is used, and other
        columns are ignored.
    *  Only one column of output will be produced -- if you want to
        calculate a multi-column output file, you'll have to run 1deval
        separately for each column, and then glue the results together
        using program 1dcat.  [However, see the 1dcat example combined
        with the '-1D:' option, infra.]
    
    Options:
    --------
      -del d     = Use 'd' as the step for a single undetermined variable
                     in the expression [default = 1.0]
                     SYNONYMS: '-dx' and '-dt'
    
      -start s   = Start at value 's' for a single undetermined variable
                     in the expression [default = 0.0]
                     That is, for the indeterminate variable in the expression
                     (if any), the i-th value will be s+i*d for i=0, 1, ....
                     SYNONYMS: '-xzero' and '-tzero'
    
      -num n     = Evaluate the expression 'n' times.
                     If -num is not used, then the length of an
                     input time series is used.  If there are no
                     time series input, then -num is required.
    
      -a q.1D    = Read time series file q.1D and assign it
                     to the symbol 'a' (as in 3dcalc).
                   * Letters 'a' to 'z' may be used as symbols.
                   * You can use the filename 'stdin:' to indicate that
                   the data for 1 symbol comes from standard input:
         1dTsort q.1D stdout: | 1deval -a stdin: -expr 'sqrt(a)' | 1dplot stdin:
    
      -a=NUMBER   = set the symbol 'a' to a fixed numerical value
                    rather than a variable value from a 1D file.
                 * Letters 'a' to 'z' may be used as symbols.
                 * You can't assign the same symbol twice!
    
      -index i.1D = Read index column from file i.1D and
                     write it out as 1st column of output.
                     This option is useful when working with
                     surface data.
    
      -1D:       = Write output in the form of a single '1D:'
                     string suitable for input on the command
                     line of another program.
                     [-1D: is incompatible with the -index option!]
                     [This won't work if the output string is very long,]
                     [since the maximum command line length is limited. ]
    
    Examples:
    ---------
     * 't' is the indeterminate variable in the expression below:
         1deval -expr 'sin(2*PI*t)' -del 0.01 -num 101 > sin.1D
     * Multiply two columns of data (no indeterminate variable):
         1deval -expr 'a*b' -a fred.1D -b ethel.1D > ab.1D
     * Compute and plot the F-statistic corresponding to p=0.001 for
       varying degrees of freedom given by the indeterminate variable 'n':
         1deval -start 10 -num 90 -expr 'fift_p2t(0.001,n,2*n)' | 1dplot -xzero 10 -stdin
     * Compute the square root of some numbers given in '1D:' form
       directly on the command line:
         1deval -x '1D: 1 4 9 16' -expr 'sqrt(x)'
    
    Examples using '-1D:' as the output format:
    -------------------------------------------
    The examples use the shell backquote `xxx` operation, where the
    command inside the backquotes is executed, its stdout is captured
    into a string, and placed back on the command line. When you have
    mastered this idea, you have taken another step towards becoming
    a Jedi AFNI Master!
    
     1dplot `1deval -1D: -num 71 -expr 'cos(t/2)*exp(-t/19)'`
     1dcat `1deval -1D: -num 100 -expr 'cos(t/5)'` \
           `1deval -1D: -num 100 -expr 'sin(t/5)'` > sincos.1D
     3dTfitter -quiet -prefix -                                     \
               -RHS `1deval -1D: -num 30 -expr 'cos(t)*exp(-t/7)'`  \
               -LHS `1deval -1D: -num 30 -expr 'cos(t)'`            \
                    `1deval -1D: -num 30 -expr 'sin(t)'`              
    
    Notes:
    ------
    * Program 3dcalc operates on 3D and 3D+time datasets in a similar way.
    
    * Program ccalc can be used to evaluate a single numeric expression.
    
    * If I had any sense, THIS program would have been called 1dcalc!
    
    * For generic 1D file usage help, see '1dplot -help'
    
    * For help with expression format, see '3dcalc -help', or type
       'help' when using ccalc in interactive mode.
    
    * 1deval only produces a single column of output.  3dcalc can be
       tricked into doing multi-column 1D format output by treating
       a 1D file as a 3D dataset and auto-transposing it with \'
       For example:
         3dcalc -a '1D: 3 4 5 | 1 2 3'\' -expr 'cbrt(a)' -prefix -
       The input has 2 'columns' and so does the output.
       Note that the 1D 'file' is transposed on input to 3dcalc!
       This is essential, or 3dcalc will not treat the 1D file as
       a dataset, and the results will be very different.  Recall that
       when a 1D file is read as an 3D AFNI dataset, the row direction
       corresponds to the sub-brick (e.g., time) direction, and the
       column direction corresponds to the voxel direction.
    
    A Dastardly Trick:
    ------------------
    If you use some other letter than 'z' as the indeterminate variable
    in the calculation, and if 'z' is not assigned to any input 1D file,
    then 'z' in the expression will be the previous value computed.
    This trick can be used to create 1 point recursions, as in the
    following command for creating a AR(1) noise time series:
        1deval -num 500 -expr 'gran(0,1)+(i-i)+0.7*z' > g07.1D
    Note the use of '(i-i)' to intoduce the variable 'i' so that 'z'
    would be used as the previous output value, rather than as the
    indeterminate variable generated by '-del' and '-start'.
    The initial value of 'z' is 0 (for the first evaluation).
    * [02 Apr 2010] You can set the initial value of 'z' to a nonzero
      value by using the environment variable AFNI_1DEVAL_ZZERO, as in
        1deval -DAFNI_1DEVAL_ZZERO=1 -num 10 -expr 'i+z'
    
    -- RW Cox --
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
