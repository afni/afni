************
@ElectroGrid
************

.. _@ElectroGrid:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: 
    @ElectroGrid <[-strip Nx] | [-grid Nx Ny]> 
                  [-prefix PREFIX] [-coords XYZ.1D] 
                  [-echo]
    
    Creates a mesh representation of an electrode grid for use with SUMA
    
    Mandatory parameters:
       One of the two options -strip or -grid
    
       -strip Nx: Make an Nx strip (array) of electrodes.
    
       -grid Nx Ny: Make an Nx by Ny grid of electrodes.
                    A node at (i,j) has a node ID = i+Nx*j with 
                    0<=i<Nx and 0<=j<=Ny
    
    Optional parameters:
       -coords XYZ.1D: Specify the coordinates of the nodes on the grid,
                       or the array. XYZ.1D should have three columns,
                       with each row specifying the coordinates of one node.
                       You can use sub-brick selectors to select from more
                       than three columns.
    
                       The fist row is for node 0, second for node 1, etc.
                       The ordering is trivial for an array. For a grid you
                       need to be a bit more careful. You march along the x 
                       direction first, then move up the y.
                       A flat grid (Z=0) for a 2x3 electrodes system would 
                       have coordinates layed out as such:
                    #  X Y Z   (ID shown here for clarity)
                       0 0 0            0
                       1 0 0            1
                       0 1 0            2
                       1 1 0            3
                       0 2 0            4
                       1 2 0            5
    
                       Usually, you would have coordinates in the subject's
                       anatomical space.
    
       [-prefix PREFIX]: Use PREFIX for the output surface. 
       [-with_markers]: Add markers to the surface at each electrode. See
                        examples below for detail.
       [-echo]    : set echo 
    
    Examples:
       Make a flat 4 electrode array:
       Node X coordinates are regularly spaced from 0 to 3.
       Node Y coordinates are small and random, to allow array 
       representation as a surface
    
          @ElectroGrid -prefix flat4 -strip 4
          suma -i flat4.gii
    
       Make a flat 4 electrode array and assign anatomical coordinates
       in first three columns of file:   HPelectrodes_AF.1D
    
          @ElectroGrid -prefix HP_array -strip 4 \
                       -coords  HPelectrodes_AF.1D'[0,1,2]'
          suma -i HP_array.gii
    
       Make a 2x3 flat grid:
       Node coordinates are on a regular grid.
    
          @ElectroGrid -prefix flat23 \
                       -grid 2 3  
          suma -i flat23.gii
    
       Make an 8x8 grid, and assign to its nodes the coordinates listed
       in the first three columns of HPelectrodes_Grid.1D
    
          @ElectroGrid -prefix HP_grid \
                       -coords HPelectrodes_Grid.1D'[0,1,2]' \
                       -grid 8 8  
          suma -i HP_grid.gii
    
       Say you're too lazy to know the grid (or strip) count 
       and you have a file with the electrode's coordinates.
    
          @ElectroGrid -prefix HP_grid2 \
                       -coords HPelectrodes_Grid.1D'[0,1,2]' 
          suma -i HP_grid2.gii
    
       You can also give the grid a special appearance by adding
       special node markers. For example, put the following text
       in file marker.niml.do
       echo "\
         <nido_head coord_type = 'mobile' /> \
         <S rad = '2' style = 'silhouette' stacks = '20' slices = '20' /> \
       " > marker.niml.do
       Then create a spec file for one of the grid surfaces:
         quickspec -spec grid.spec \
                   -tsnadm gii pial HP_grid2.gii y SAME marker.niml.do
         suma -spec grid.spec
    
       Using option -with_markers will do the trick for you.
    
    Ziad Saad (saadz@mail.nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
