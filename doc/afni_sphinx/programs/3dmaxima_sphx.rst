.. _ahelp_3dmaxima:

********
3dmaxima
********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    3dmaxima - used to locate extrema in a functional dataset.
    
       This program reads a functional dataset and locates any relative extrema
       (maximums or minimums, depending on the user option).  A _relative_
       maximum is a point that is greater than all neighbors (not necessarily
       greater than all other values in the sub-brick).  The output from this
       process can be text based (sent to the terminal window) and it can be a
       mask (integral) dataset, where the locations of the extrema are set.
    
       When writing a dataset, it is often useful to set a sphere around each
       extrema, not to just set individual voxels.  This makes viewing those
       locations much more reasonable.  Also, if the 'Sphere Values' option is
       set to 'N to 1', the sphere around the most extreme voxel will get the
       value N, giving it the 'top' color in afni (and so on, down to 1).
    
       Notes : The only required option is the input dataset.
               Input datasets must be of type short.
               All distances are in voxel units.
    
    ----------------------------------------------------------------------
                            ***  Options  ***
    
    -----  Input Dset:  -----
    
       -input DSET           : specify input dataset
    
             e.g. -input func+orig'[7]'
    
           Only one sub-brick may be specified.  So if a dataset has multiple
           sub-bricks, the [] selector must be used.
    
    -----  Output Dset:  -----
    
       -prefix PREFIX        : prefix for an output mask dataset
    
             e.g. -prefix maskNto1
    
           This dataset may be viewed as a mask.  It will have a value set at
           the location of any selected extrema.  The -out_rad option can be
           used to change those points to 'spheres'.
    
       -spheres_1            : [flag] set all output values to 1
    
           This is the default, which sets all values in the output dataset
           to 1.  This is for the extreme points, and for the spheres centered
           around them.
    
       -spheres_1toN         : [flag] output values will range from 1 to N
    
           In this case, the most extreme voxel will be set with a value of 1.
           The next most extreme voxel will get 2, and so on.
    
       -spheres_Nto1         : [flag] output values will range from N to 1
    
           With this option, the highest extrema will be set to a value of N,
           where N equals the number of reported extrema.  The advantage of
           this is that the most extreme point will get the highest color in
           afni.
    
    -----  Threshold:  -----
    
       -thresh CUTOFF        : provides a cutoff value for extrema
    
             e.g. -thresh 17.4
    
           Extrema not meeting this cutoff will not be considered.
           Note that if the '-neg_ext' option is applied, the user
           will generally want a negative threshold.
    
    -----  Separation:  -----
    
       -min_dist VOXELS      : minimum acceptable distance between extrema
    
             e.g. -min_dist 4
    
           Less significant extrema which are close to more significant extrema
           will be discounted in some way, depending on the 'neighbor style'
           options.
    
           See '-n_style_sort' and '-n_style_weight_ave' for more information.
    
           Note that the distance is in voxels, not mm.
    
    -----  Output Size:  -----
    
       -out_rad SIZE         : set the output radius around extrema voxels
    
             e.g. -out_rad 9
    
           If the user wants the output BRIK to consist of 'spheres' centered
           at extrema points, this option can be used to set the radius for
           those spheres.  Note again that this is in voxel units.
    
    -----  Neighbor:  -----
    
       If extrema are not as far apart as is specified by the '-min_dist'
       option, the neighbor style options specify how to handle the points.
    
       -n_style_sort         : [flag] use 'Sort-n-Remove' style (default)
    
           The extrema are sorted by magnitude.  For each extrema (which has
           not previously removed), all less significant extrema neighbors
           within the separation radius (-min_dist) are removed.
    
           See '-min_dist' for more information.
    
       -n_style_weight_ave   : [flag] use 'Weighted-Average' style
    
           Again, traverse the sorted list of extrema.  Replace the current
           extrema with the center of mass of all extrema within the Separation
           radius of the current point, removing all others within this radius.
    
           This should not change the number of extrema, it should only shift
           the locations.
    
    -----  Params:  -----
    
       -neg_ext              : [flag] search for negative extrema (minima)
    
           This will search for the minima of the dataset.
           Note that a negative threshold may be desired.
    
       -true_max             : [flag] extrema may not have equal neighbors
    
           By default, points may be considered extrema even if they have a
           neighbor with the same value.  This flag option requires extrema
           to be strictly greater than any of their neighbors.
    
           With this option, extrema locations that have neighbors at the same
           value are ignored.
    
    -----  Output Text:  -----
    
       -debug LEVEL          : output extra information to the terminal
    
           e.g. -debug 2
    
       -no_text              : [flag] do not display the extrma points as text
    
       -coords_only          : [flag] only output coordinates (no text or vals)
    
    -----  Output Coords:  -----
    
       -dset_coords          : [flag] display output in the dataset orientation
    
           By default, the xyz-coordinates are displayed in DICOM orientation
           (RAI), i.e. right, anterior and inferior coordinates are negative,
           and they are printed in that order (RL, then AP, then IS).
    
           If this flag is set, the dataset orientation is used, whichever of
           the 48 it happens to be.
    
           Note that in either case, the output orientation is printed above
           the results in the terminal window, to remind the user.
    
    -----  Other :  -----
    
       -help                 : display this help
    
       -hist                 : display module history
    
       -ver                  : display version number
    
    Author: R Reynolds
