*******
1dTsort
*******

.. _1dTsort:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dTsort [options] file.1D
    Sorts each column of the input 1D file and writes result to stdout.
    
    Options
    -------
     -inc     = sort into increasing order [default]
     -dec     = sort into decreasing order
     -flip    = transpose the file before OUTPUT
                * the INPUT can be transposed using file.1D\'
                * thus, to sort each ROW, do something like
                   1dTsort -flip file.1D\' > sfile.1D
     -col j   = sort only on column #j (counting starts at 0),
                and carry the rest of the columns with it.
     -imode   = typecast all values to integers, return the mode in
                the input then exit. No sorting results are returned.
    
    N.B.: Data will be read from standard input if the filename IS stdin,
          and will also be row/column transposed if the filename is stdin\'
          For example:
            1deval -num 100 -expr 'uran(1)' | 1dTsort stdin | 1dplot stdin
    
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
