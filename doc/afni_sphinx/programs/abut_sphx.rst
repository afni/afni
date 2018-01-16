****
abut
****

.. _abut:

.. contents:: 
    :depth: 4 

.. code-block:: none

    ABUT:  put noncontiguous FMRI slices together [for to3d]
    
    method: put zero valued slices in the gaps, then
            replicate images to simulate thinner slices
    
    Usage:
       abut [-dzin thickness] [-dzout thickness] [-root name]
            [-linear | -blocky] [-verbose] [-skip n+gap] ... images ...
    
       -dzin   the thickness value in mm;  if not given,
                 taken to be 1.0 (in which case, the output
                 thickness and gap sizes are simply relative
                 to the slice thickness, rather than absolute)
    
       -dzout  the output slice thickness, usually smaller than
                 the input thickness;  if not given, the program
                 will compute a value (the smaller the ratio
                 dzout/dzin is, the more slices will be output)
    
       -root   'name' is the root (or prefix) for the output
                 slice filename;  for example, '-root fred.'
                 will result in files fred.0001, fred.0002, ...
    
       -linear if present, this flag indicates that subdivided slices
                 will be linearly interpolated rather than simply
                 replicated -- this will make the results smoother
                 in the through-slice direction (if dzout < dzin)
    
       -blocky similar to -linear, but uses AFNI's 'blocky' interpolation
                 when possible to put out intermediate slices.
                 Both interpolation options only apply when dzout < dzin
                 and when an output slice has a non-gappy neighbor.
    
       -skip   'n+gap' indicates that a gap is to be inserted
                 between input slices #n and #n+1, where n=1,2,...;
                 for example, -skip 6+5.5 means put a gap of 5.5 mm
                 between slices 6 and 7.
    
       More than one -skip option is allowed.  They must all occur
