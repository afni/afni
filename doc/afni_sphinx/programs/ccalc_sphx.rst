.. _ahelp_ccalc:

*****
ccalc
*****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: ccalc [-form FORM] [-eval 'expr']
    Usage mode 1: Interactive numerical calculator
        Interactive numerical calculator, using the 
        same expression syntax as 3dcalc. 
        No command line parameters are permitted in
        usage 1 mode.
    Usage mode 2: Command line expression calculator
        Evaluate an expression specified on command
        line, return answer and quit.
        Optional parameters: (must come first)
        -form FORM: Format output in a nice form
                    Choose from:
                    double: Macho numbers (default).
                    nice: Metrosexual output.
                    int (or rint): Rounded to nearest integer.
                    cint: Rounded up.
                    fint: Rounded down.
                    %n.mf: custom format string, used as in printf.
                       format string can contain %%, \n and other
                       regular characters.
                       See man fprintf and man printf for details.
                    You can also replace:
                       -form int    with    -i
                       -form nice   with    -n
                       -form double with    -d
                       -form fint   with    -f
                       -form cint   with    -c
        Mandatory parameter: (must come last on command line)
        -eval EXPR: EXPR is the expression to evaluate.
                    Example: ccalc -eval '3 + 5 * sin(22)' 
                         or: ccalc -eval 3 +5 '*' 'sin(22)'
                    You can not use variables in EXPR
                    as you do with 3dcalc.
        Example with formatting:
            ccalc -form '********\n%6.4f%%\n********' -eval '100*328/457'
        gives:
            ********
            0.7177%
            ********
        Try also:
            ccalc -i 3.6
            ccalc -f 3.6
            ccalc -c 3.6
            ccalc -form '%3.5d' 3.3
            ccalc -form '**%5d**' 3.3
            ccalc -form '**%-5d**' 3.3
    
     ** SECRET: You don't need to use -eval if you are 
                not using any other options. I hate typing
                it for quick command line calculations. 
                But that feature might be removed in the
                future, so always use -eval when you are 
