.. contents:: 
    :depth: 4 

************
afni_util.py
************

.. code-block:: none

    
    afni_util.py: not really intended as a main program
    
       However, there is some functionality for devious purposes...
    
       options:
    
          -help             : show this help
    
          -eval STRING      : evaluate STRING in the context of afni_util.py
                              (i.e. STRING can be function calls or other)
    
             This option is used to simply execute the code in STRING.
    
             Examples for eval:
    
                afni_util.py -eval "show_process_stack()"
                afni_util.py -eval "show_process_stack(verb=2)"
                afni_util.py -eval "show_process_stack(pid=1000)"
    
          -exec STRING      : execute STRING in the context of afni_util.py
    
             This option is used to simply execute the code in STRING.
    
             Examples for exec:
    
                afni_util.py -exec "y = 3+4 ; print y"
                afni_util.py -exec "import PyQt4"
                afni_util.py -exec "show_process_stack()"
    
          -funchelp FUNC    : print the help for afni_util.py function FUNC
    
             Pring the FUNC.__doc__ text, if any.
    
             Example:
    
                afni_util.py -funchelp wrap_file_text
    
          -print STRING     : print the result of executing STRING
    
             Akin to -eval, but print the results of evaluating STRING.
    
          -lprint STRING    : line print: print result list, one element per line
    
             The 'l' stands for 'line' (or 'list').  This is akin to -print,
             but prints a list with one element per line.
    
          -listfunc [SUB_OPTS] FUNC LIST ... : execute FUNC(LIST)
    
             With this option, LIST is a list of values to be passed to FUNC().
             Note that LIST can be simply '-' or 'stdin', in which case the
             list values are read from stdin.
    
             This is similar to eval, but instead of requiring:
                -eval "FUNC([v1,v2,v3,...])"
             the list values can be left as trailing arguments:
                -listfunc FUNC v1 v2 v3 ...
             (where LIST = v1 v2 v3 ...).
    
             SUB_OPTS sub-options:
    
                    -float  : convert the list to floats before passing to FUNC()
                    -print  : print the result
                    -join   : print the results join()'d together
    
             Examples for listfunc:
    
                afni_util.py -listfunc        min_mean_max_stdev 1 2 3 4 5
                afni_util.py -listfunc -print min_mean_max_stdev 1 2 3 4 5
                afni_util.py -listfunc -join  min_mean_max_stdev 1 2 3 4 5
    
                afni_util.py -listfunc -join -float demean 1 2 3 4 5
    
                afni_util.py -listfunc -join shuffle `count -digits 4 1 124`
                count -digits 4 1 124 | afni_util.py -listfunc -join shuffle -
                afni_util.py -listfunc glob2stdout 'EPI_run1/8*'
    
                afni_util.py -listfunc -joinc list_minus_glob_form *HEAD
    
                afni_util.py -listfunc -join -float linear_fit 2 3 5 4 8 5 8 9
    
    
             Also, if LIST contains -list2, then 2 lists can be input to do
             something like:
                -eval "FUNC([v1,v2,v3], [v4,v5,v6])"
    
             Examples with -list2:
    
                afni_util.py -listfunc -print -float ttest 1 2 3 4 5 \
                                                    -list2 2 2 4 6 8
    
                afni_util.py -listfunc -print -float ttest_paired   \
                              1 2 3 4 5 -list2 2 4 5 6 8
    
                afni_util.py -listfunc -join -float linear_fit      \
                             `cat y.1D` -list2 `cat x.1D`
    
