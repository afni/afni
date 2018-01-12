.. contents:: 
    :depth: 4 

*****
count
*****

.. code-block:: none

    Usage: count [options] bot top [step]
    
    * Produces many numbered copies of the root and/or suffix,
        counting from 'bot' to 'top' with stride 'step'.
    * If 'bot' > 'top', counts backwards with stride '-step'.
    * If step is of the form 'R#', then '#' random counts are produced
        in the range 'bot..top' (inclusive).
    * If step is of the form 'S', then a random sequence of unique integers
        in the range 'bot..top' (inclusive) is output.
        A number after S ('S#') indicates the number of unique integers
        to output. If # exceeds the number of unique values, the shuffled
        sequence will simply repeat itself. (N.B.: 'S' is for 'Shuffle'.)
    * 'bot' and 'top' must not be negative; step must be +ve (defaults to 1).
    * 'bot' and 'top' can be any character between 'A' and 'Z' or 'a' and 'z'.
                      In these instances, the counting is from character bot 
                      to character top. If you do not specify -form, the program
                      will automatically choose -form '%c'. For example:
                           count a z
                      or to get the ASCII value of the characters:
                           count -form %d a z
    
    Options:
      -seed        seed number for random number generator (for S and R above)
      -sseed       seed string for random number generator (for S and R above)
      -column      writes output, one number per line (with root and suffix, if any)
      -digits n    prints numbers with 'n' digits [default=4]
      -form CFRM   print the numbers with the CFRM formatting string. 
                   e.g.: count -form %c 49 130 
                      or count -form '%03d<:-)' 97 99 
                   You can't use any type of C formatting, only those who
                   take an integer for an input. Using '%f', or '%s' will 
                   cause a crash.
                   -form overrides -digits.
      -root rrr    prints string 'rrr' before the number [default=empty]
      -sep s       prints single character 's' between the numbers [default=blank]
                     [normally you would not use '-sep' with '-column']
      -suffix sss  prints string 'sss' after the number [default=empty]
      -scale fff   multiplies each number by the factor 'fff';
                     if this option is used, -digits is ignored and
                     the floating point format '%g' is used for output.
                     ('fff' can be a floating point number.)
      -comma       put commas between the outputs, instead of spaces
                     (same as '-sep ,')
      -skipnmodm n m   skip over numbers with a modulus of n with m
                      -skipnmodm 15 16 would skip 15, 31, 47, ...
                   not valid with random number sequence options
    
    The main application of this program is for use in C shell programming:
      foreach fred ( `count 1 20` )
         mv wilma.${fred} barney.${fred}
      end
    The backward quote operator in the foreach statement executes the
    count program, captures its output, and puts it on the command line.
    The loop body renames each file wilma.0001 to wilma.0020 to barney.0001
    to barney.0020.  Read the man page for csh to get more information.  In
    particular, the csh built-in command '@' can be useful.
    
    Shuffle Example:
    ----------------
    You can use the 'S' mode to reorder a dataset or 1D file randomly.
    Suppose you have several 1D files with 60 columns and you want to rearrange
    each one in the same random way -- interchanging columns to scramble some
    stimulus amplitude modulation sequences, say:
      count -dig 1 0 59 S > randorder.1D
      1dcat A.1D"[`cat randorder.1D`]" > Areordered.1D
      1dcat B.1D"[`cat randorder.1D`]" > Breordered.1D
      1dcat C.1D"[`cat randorder.1D`]" > Creordered.1D
    Unlike 'R', which can produce duplicates, 'S' will give set of unique numbers.
    
    -- Written by RWCox back in the ancient mists of forgotten time --
