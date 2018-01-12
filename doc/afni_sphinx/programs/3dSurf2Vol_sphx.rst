.. contents:: 
    :depth: 4 

**********
3dSurf2Vol
**********

.. code-block:: none

    
    3dSurf2Vol - map data from a surface domain to an AFNI volume domain
    
      usage: 3dSurf2Vol [options] -spec SPEC_FILE -surf_A SURF_NAME \
                 -grid_parent AFNI_DSET -sv SURF_VOL \
                 -map_func MAP_FUNC -prefix OUTPUT_DSET
    
        This program is meant to take as input a pair of surfaces,
        optionally including surface data, and an AFNI grid parent
        dataset, and to output a new AFNI dataset consisting of the
        surface data mapped to the dataset grid space.  The mapping
        function determines how to map the surface values from many
        nodes to a single voxel.
    
        Surfaces (from the spec file) are specified using '-surf_A'
        (and '-surf_B', if a second surface is input).  If two
        surfaces are input, then the computed segments over node
        pairs will be in the direction from surface A to surface B.
    
        The basic form of the algorithm is:
    
           o for each node pair (or single node)
               o form a segment based on the xyz node coordinates,
                 adjusted by any '-f_pX_XX' options
               o divide the segment up into N steps, according to 
                 the '-f_steps' option
               o for each segment point
                   o if the point is outside the space of the output
                     dataset, skip it
                   o locate the voxel in the output dataset which
                     corresponds to this segment point
                   o if the '-cmask' option was given, and the voxel
                     is outside the implied mask, skip it
                   o if the '-f_index' option is by voxel, and this
                     voxel has already been considered, skip it
                   o insert the surface node value, according to the
                     user-specified '-map_func' option
    
      Surface Coordinates:
    
          Surface coordinates are assumed to be in the Dicom
          orientation.  This information may come from the option
          pair of '-spec' and '-sv', with which the user provides
          the name of the SPEC FILE and the SURFACE VOLUME, along
          with '-surf_A' and optionally '-surf_B', used to specify
          actual surfaces by name.  Alternatively, the surface
          coordinates may come from the '-surf_xyz_1D' option.
          See these option descriptions below.
    
          Note that the user must provide either the three options
          '-spec', '-sv' and '-surf_A', or the single option,
          '-surf_xyz_1D'.
    
      Surface Data:
    
          Surface domain data can be input via the '-sdata_1D'
          or '-sdata' option.  In such a case, the data is with 
          respect to the input surface.  
          Note: With -sdata_1D,  the first column of the file 
          should contain a node's index, and following columns are
          that node's data. See the '-sdata_1D' option for more info.
          Option -sdata takes NIML or GIFTI input which contain
          node index information in their headers.
    
          If the surfaces have V values per node (pair), then the
          resulting AFNI dataset will have V sub-bricks (unless the
          user applies the '-data_expr' option).
    
      Mapping Functions:
    
          Mapping functions exist because a single volume voxel may
          be occupied by multiple surface nodes or segment points.
          Depending on how dense the surface mesh is, the number of
          steps provided by the '-f_steps' option, and the indexing
          type from '-f_index', even a voxel which is only 1 cubic
          mm in volume may have quite a few contributing points.
    
          The mapping function defines how multiple surface values
          are combined to get a single result in each voxel.  For
          example, the 'max' function will take the maximum of all
          surface values contributing to each given voxel.
    
          Current mapping functions are listed under the '-map_func'
          option, below.
    
    ------------------------------------------------------------
    
      examples:
    
        1. Map a single surface to an anatomical volume domain,
           creating a simple mask of the surface.  The output
           dataset will be fred_surf+orig, and the orientation and
           grid spacing will follow that of the grid parent.  The
           output voxels will be 1 where the surface exists, and 0
           elsewhere.
    
        3dSurf2Vol                       \
           -spec         fred.spec                \
           -surf_A       pial                     \
           -sv           fred_anat+orig           \
           -grid_parent  fred_anat+orig           \
           -map_func     mask                     \
           -prefix       fred_surf
    
        2. Map the cortical grey ribbon (between the white matter
           surface and the pial surface) to an AFNI volume, where
           the resulting volume is restricted to the mask implied by
           the -cmask option.
    
           Surface data will come from the file sdata_10.1D, which
           has 10 values per node, and lists only a portion of the
           entire set of surface nodes.  Each node pair will be form
           a segment of 15 equally spaced points, the values from
           which will be applied to the output dataset according to
           the 'ave' filter.  Since the index is over points, each
           of the 15 points will have its value applied to the
           appropriate voxel, even multiple times.  This weights the
           resulting average by the fraction of each segment that
           occupies a given voxel.
    
           The output dataset will have 10 sub-bricks, according to
           the 10 values per node index in sdata_10.1D.
    
        3dSurf2Vol                       \
           -spec         fred.spec                               \
           -surf_A       smoothwm                                \
           -surf_B       pial                                    \
           -sv           fred_anat+orig                          \
           -grid_parent 'fred_func+orig[0]'                      \
           -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \
           -sdata_1D     sdata_10.1D                             \
           -map_func     ave                                     \
           -f_steps      15                                      \
           -f_index      points                                  \
           -prefix       fred_surf_ave
    
        3. The inputs in this example are identical to those in
           example 2, including the surface dataset, sdata_10.1D.
           Again, the output dataset will have 10 sub-bricks.
    
           The surface values will be applied via the 'max_abs'
           filter, with the intention of assigning to each voxel the
           node value with the most significance.  Here, the index
           method does not matter, so it is left as the default,
           'voxel'.
    
           In this example, each node pair segment will be extended
           by 20% into the white matter, and by 10% outside of the
           grey matter, generating a "thicker" result.
    
        3dSurf2Vol                       \
           -spec         fred.spec                               \
           -surf_A       smoothwm                                \
           -surf_B       pial                                    \
           -sv           fred_anat+orig                          \
           -grid_parent 'fred_func+orig[0]'                      \
           -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \
           -sdata_1D     sdata_10.1D                             \
           -map_func     max_abs                                 \
           -f_steps      15                                      \
           -f_p1_fr      -0.2                                    \
           -f_pn_fr       0.1                                    \
           -prefix       fred_surf_max_abs
    
        4. This is similar to example 2.  Here, the surface nodes
           (coordinates) come from 'surf_coords_2.1D'.  But these
           coordinates do not happen to be in Dicom orientation,
           they are in the same orientation as the grid parent, so
           the '-sxyz_orient_as_gpar' option is applied.
    
           Even though the data comes from 'sdata_10.1D', the output
           AFNI dataset will only have 1 sub-brick.  That is because
           of the '-data_expr' option.  Here, each applied surface
           value will be the average of the sines of the first 3
           data values (columns of sdata_10.1D).
    
        3dSurf2Vol                       \
           -surf_xyz_1D  surf_coords_2.1D                        \
           -sxyz_orient_as_gpar                                  \
           -grid_parent 'fred_func+orig[0]'                      \
           -sdata_1D     sdata_10.1D                             \
           -data_expr   '(sin(a)+sin(b)+sin(c))/3'               \
           -map_func     ave                                     \
           -f_steps      15                                      \
           -f_index      points                                  \
           -prefix       fred_surf_ave_sine
    
        5. In this example, voxels will get the maximum value from
           column 3 of sdata_10.1D (as usual, column 0 is used for
           node indices).  The output dataset will have 1 sub-brick.
    
           Here, the output dataset is forced to be of type 'short',
           regardless of what the grid parent is.  Also, there will
           be no scaling factor applied.
    
           To track the numbers for surface node #1234, the '-dnode'
           option has been used, along with '-debug'.  Additionally,
           '-dvoxel' is used to track the results for voxel #6789.
    
        3dSurf2Vol                       \
           -spec         fred.spec                               \
           -surf_A       smoothwm                                \
           -surf_B       pial                                    \
           -sv           fred_anat+orig                          \
           -grid_parent 'fred_func+orig[0]'                      \
           -sdata_1D     sdata_10.1D'[0,3]'                      \
           -map_func     max                                     \
           -f_steps      15                                      \
           -datum        short                                   \
           -noscale                                              \
           -debug        2                                       \
           -dnode        1234                                    \
           -dvoxel       6789                                    \
           -prefix       fred_surf_max
    
        6. Draw some surface ROIs, and map them to the volume.  Some
           voxels may contain nodes from multiple ROIs, so take the
           most common one (the mode), as suggested by R Mruczek.
    
           ROIs are left in 1D format for the -sdata_1D option.
    
    
        setenv AFNI_NIML_TEXT_DATA YES
        ROI2dataset -prefix rois.1D.dset -input rois.niml.roi
    
        3dSurf2Vol                           \
           -spec         fred.spec           \
           -surf_A       smoothwm            \
           -surf_B       pial                \
           -sv           fred_anat+orig      \
           -grid_parent 'fred_func+orig[0]'  \
           -sdata_1D     rois.1D.dset        \
           -map_func     mode                \
           -f_steps      10                  \
           -prefix       rois.from.surf
    
    
    ------------------------------------------------------------
    
      REQUIRED COMMAND ARGUMENTS:
    
        -spec SPEC_FILE        : SUMA spec file
    
            e.g. -spec fred.spec
    
            The surface specification file contains the list of
            mappable surfaces that are used.
    
            See @SUMA_Make_Spec_FS and @SUMA_Make_Spec_SF.
    
            Note: this option, along with '-sv', may be replaced
                  by the '-surf_xyz_1D' option.
    
        -surf_A SURF_NAME      : specify surface A (from spec file)
        -surf_B SURF_NAME      : specify surface B (from spec file)
    
            e.g. -surf_A smoothwm
            e.g. -surf_A lh.smoothwm
            e.g. -surf_B lh.pial
    
            This parameter is used to tell the program with surfaces
            to use.  The '-surf_A' parameter is required, but the
            '-surf_B' parameter is an option.
    
            The surface names must uniquely match those in the spec
            file, though a sub-string match is good enough.  The
            surface names are compared with the names of the surface
            node coordinate files.
    
            For instance, given a spec file that has only the left
            hemisphere in it, 'pial' should produce a unique match
            with lh.pial.asc.  But if both hemispheres are included,
            then 'pial' would not be unique (matching rh.pial.asc,
            also).  In that case, 'lh.pial' would be better.
    
        -sv SURFACE_VOLUME     : AFNI dataset
    
            e.g. -sv fred_anat+orig
    
            This is the AFNI dataset that the surface is mapped to.
            This dataset is used for the initial surface node to xyz
            coordinate mapping, in the Dicom orientation.
    
            Note: this option, along with '-spec', may be replaced
                  by the '-surf_xyz_1D' option.
    
        -surf_xyz_1D SXYZ_NODE_FILE : 1D coordinate file
    
            e.g. -surf_xyz_1D my_surf_coords.1D
    
            This ascii file contains a list of xyz coordinates to be
            considered as a surface, or 2 sets of xyz coordinates to
            considered as a surface pair.  As usual, these points
            are assumed to be in Dicom orientation.  Another option
            for coordinate orientation is to use that of the grid
            parent dataset.  See '-sxyz_orient_as_gpar' for details.
    
            This option is an alternative to the pair of options, 
            '-spec' and '-sv'.
    
            The number of rows of the file should equal the number
            of nodes on each surface.  The number of columns should
            be either 3 for a single surface, or 6 for two surfaces.
            
            sample line of an input file (one surface):
            
            11.970287  2.850751  90.896111
            
            sample line of an input file (two surfaces):
            
            11.97  2.85  90.90    12.97  2.63  91.45
            
    
        -grid_parent AFNI_DSET : AFNI dataset
    
            e.g. -grid_parent fred_function+orig
    
            This dataset is used as a grid and orientation master
            for the output AFNI dataset.
    
        -map_func MAP_FUNC     : surface to dataset function
    
            e.g. -map_func max
            e.g. -map_func mask -f_steps 20
    
            This function applies to the case where multiple data
            points get mapped to a single voxel, which is expected
            since surfaces tend to have a much higher resolution
            than AFNI volumes.  In the general case data points come
            from each point on each partitioned line segment, with
            one segment per node pair.  Note that these segments may
            have length zero, such as when only a single surface is
            input.
    
            See "Mapping Functions" above, for more information.
    
            The current mapping function for one surface is:
    
              mask   : For each xyz location, set the corresponding
                       voxel to 1.
    
            The current mapping functions for two surfaces are as
            follows.  These descriptions are per output voxel, and
            over the values of all points mapped to a given voxel.
    
              mask2  : if any points are mapped to the voxel, set
                       the voxel value to 1
    
              ave    : average all values
    
              count  : count the number of mapped data points
    
              min    : find the minimum value from all mapped points
    
              max    : find the maximum value from all mapped points
    
              max_abs: find the number with maximum absolute value
                       (the resulting value will retain its sign)
    
              mode   : apply the most common value per voxel
                       (appropriate where surf ROIs overlap)
    
        -prefix OUTPUT_PREFIX  : prefix for the output dataset
    
            e.g. -prefix anat_surf_mask
    
            This is used to specify the prefix of the resulting AFNI
            dataset.
    
      ------------------------------
      SUB-SURFACE DATA FILE OPTIONS:
    
        -sdata_1D SURF_DATA.1D : 1D sub-surface file, with data
    
            e.g. -sdata_1D roi3.1D
    
            This is used to specify a 1D file, which contains
            surface indices and data.  The indices refer to the
            surface(s) read from the spec file.
            
            The format of this data file is a surface index and a
            list of data values on each row.  To be a valid 1D file,
            each row must have the same number of columns.
    
        -sdata SURF_DATA_DSET: NIML, or GIFTI formatted dataset.
    
      ------------------------------
      OPTIONS SPECIFIC TO SEGMENT SELECTION:
    
        (see "The basic form of the algorithm" for more details)
    
        -f_steps NUM_STEPS     : partition segments
    
            e.g. -f_steps 10
            default: -f_steps 2   (or 1, the number of surfaces)
    
            This option specifies the number of points to divide
            each line segment into, before mapping the points to the
            AFNI volume domain.  The default is the number of input
            surfaces (usually, 2).  The default operation is to have
            the segment endpoints be the actual surface nodes,
            unless they are altered with the -f_pX_XX options.
    
        -f_index TYPE          : index by points or voxels
    
            e.g. -f_index points
            e.g. -f_index voxels
            default: -f_index voxels
    
            Along a single segment, the default operation is to
            apply only those points mapping to a new voxel.  The
            effect of the default is that a given voxel will have
            at most one value applied per voxel pair.
    
            If the user applies this option with 'points' or 'nodes'
            as the argument, then every point along the segment will
            be applied.  This may be preferred if, for example, the
            user wishes to have the average weighted by the number
            of points occupying a voxel, not just the number of node
            pair segments.
    
        Note: the following -f_pX_XX options are used to alter the
              locations of the segment endpoints, per node pair.
              The segments are directed, from the node on the first
              surface to the node on the second surface.  To modify
              the first endpoint, use a -f_p1_XX option, and use
              -f_pn_XX to modify the second.
    
        -f_p1_fr FRACTION      : offset p1 by a length fraction
    
            e.g. -f_p1_fr -0.2
            e.g. -f_p1_fr -0.2  -f_pn_fr 0.2
    
            This option moves the first endpoint, p1, by a distance
            of the FRACTION times the original segment length.  If
            the FRACTION is positive, it moves in the direction of
            the second endpoint, pn.
    
            In the example, p1 is moved by 20% away from pn, which
            will increase the length of each segment.
    
        -f_pn_fr FRACTION      : offset pn by a length fraction
    
            e.g. -f_pn_fr  0.2
            e.g. -f_p1_fr -0.2  -f_pn_fr 0.2
    
            This option moves pn by a distance of the FRACTION times
            the original segment length, in the direction from p1 to
            pn.  So a positive fraction extends the segment, and a
            negative fraction reduces it.
    
            In the example above, using 0.2 adds 20% to the segment
            length past the original pn.
    
        -f_p1_mm DISTANCE      : offset p1 by a distance in mm.
    
            e.g. -f_p1_mm -1.0
            e.g. -f_p1_mm -1.0  -f_pn_fr 1.0
    
            This option moves p1 by DISTANCE mm., in the direction
            of pn.  If the DISTANCE is positive, the segment gets
            shorter.  If DISTANCE is negative, the segment will get
            longer.
    
            In the example, p1 is moved away from pn, extending the
            segment by 1 millimeter.
    
        -f_pn_mm DISTANCE      : offset pn by a distance in mm.
    
            e.g. -f_pn_mm  1.0
            e.g. -f_p1_mm -1.0  -f_pn_fr 1.0
    
            This option moves pn by DISTANCE mm., in the direction
            from the first point to the second.  So if DISTANCE is
            positive, the segment will get longer.  If DISTANCE is
            negative, the segment will get shorter.
    
            In the example, pn is moved 1 millimeter farther from
            p1, extending the segment by that distance.
    
      ------------------------------
      GENERAL OPTIONS:
    
        -cmask MASK_COMMAND    : command for dataset mask
    
            e.g. -cmask '-a fred_func+orig[2] -expr step(a-0.8)'
    
            This option will produce a mask to be applied to the
            output dataset.  Note that this mask should form a
            single sub-brick.
    
            This option follows the style of 3dmaskdump (since the
            code for it was, uh, borrowed from there (thanks Bob!)).
    
            See '3dmaskdump -help' for more information.
    
        -data_expr EXPRESSION  : apply expression to surface input
    
            e.g. -data_expr 17
            e.g. -data_expr '(a+b+c+d)/4'
            e.g. -data_expr '(sin(a)+sin(b))/2'
    
            This expression is applied to the list of data values
            from the surface data file input via '-sdata_1D'.  The
            expression is applied for each node or node pair, to the
            list of data values corresponding to that node.
    
            The letters 'a' through 'z' may be used as input, and
            refer to columns 1 through 26 of the data file (where
            column 0 is a surface node index).  The data file must
            have enough columns to support the expression.  It is
            valid to have a constant expression without a data file.
    
        -datum DTYPE           : set data type in output dataset
    
            e.g. -datum short
            default: same as that of grid parent
    
            This option specifies the data type for the output AFNI
            dataset.  Valid choices are byte, short and float, which
            are 1, 2 and 4 bytes for each data point, respectively.
    
        -debug LEVEL           : verbose output
    
            e.g. -debug 2
    
            This option is used to print out status information 
            during the execution of the program.  Current levels are
            from 0 to 5.
    
        -dnode DEBUG_NODE      : extra output for that node
    
            e.g. -dnode 123456
    
            This option requests additional debug output for the
            given surface node.  This index is with respect to the
            input surface (included in the spec file, or through the
            '-surf_xyz_1D' option).
    
            This will have no effect without the '-debug' option.
    
        -dvoxel DEBUG_VOXEL    : extra output for that voxel
    
            e.g. -dvoxel 234567
    
            This option requests additional debug output for the
            given volume voxel.  This 1-D index is with respect to
            the output AFNI dataset.  One good way to find a voxel
            index to supply is from output via the '-dnode' option.
    
            This will have no effect without the '-debug' option.
    
        -hist                  : show revision history
    
            Display module history over time.
    
        -help                  : show this help
    
            If you can't get help here, please get help somewhere.
    
        -noscale               : no scale factor in output dataset
    
            If the output dataset is an integer type (byte, shorts
            or ints), then the output dataset may end up with a
            scale factor attached (see 3dcalc -help).  With this
            option, the output dataset will not be scaled.
    
        -sxyz_orient_as_gpar   : assume gpar orientation for sxyz
    
            This option specifies that the surface coordinate points
            in the '-surf_xyz_1D' option file have the orientation
            of the grid parent dataset.
    
            When the '-surf_xyz_1D' option is applied the surface
            coordinates are assumed to be in Dicom orientation, by
            default.  This '-sxyz_orient_as_gpar' option overrides
            the Dicom default, specifying that the node coordinates
            are in the same orientation as the grid parent dataset.
    
            See the '-surf_xyz_1D' option for more information.
    
        -version               : show version information
    
            Show version and compile date.
    
    ------------------------------------------------------------
    
      Author: R. Reynolds  - version  3.7 (November 4, 2011)
    
                    (many thanks to Z. Saad and R.W. Cox)
