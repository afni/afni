*******************
quick.alpha.vals.py
*******************

.. _quick.alpha.vals.py:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    quick.alpha.vals.py   - make an alpha table from slow_surf_clustsim.py results
    
       Run this on each z.max.area file output by slow_surf_clustsim.py.  In some
       cases the z.max.area might not have as many lines as iterations, for which
       the -niter option can be applied.
    
       usage: quick.alpha.vals.py [-niter N] max_file
    
           -niter: number of iterations that should be in the z file
    
                ** Note: -niter should match that from slow_surf_clustsim.py.
    
       This pathetic program will surely be enhanced.  Someday.
    
       R Reynolds
