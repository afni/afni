.. contents:: 
    :depth: 4 

**********
@4Daverage
**********

.. code-block:: none

    
    **********************************
    This script is somewhat outdated.
    I suggest you use 3dMean which is
    faster, meaner and not limited to
    the alphabet.   ZSS, 03/14/03
    **********************************
    
    \012Usage : @4Daverage <average 3D+t brick prefix> <3D+t brik names...>
    \012This script file uses 3Dcalc to compute average 3D+time bricks
    example : @4Daverage NPt1av NPt1r1+orig NPt1r2+orig NPt1r3+orig
    The output NPt1av+orig is the average of the three bricks
     NPt1r1+orig, NPt1r2+orig and NPt1r3+orig
    
    You can use wildcards such as
     @4Daverage test ADzst2*.HEAD AFzst2r*.HEAD 
     Make sure you do not pass both .HEAD and .BRIK names.
     If you do so they will be counted twice.\012
    The bricks to be averaged must be listed individually.
    The total number of bricks that can be averaged at once (26)
    is determined by 3dcalc.
    
    \012Ziad Saad Nov 21 97, Marquette University
    Modified to accept wild cards Jan 24 01, FIM/LBC/NIH
