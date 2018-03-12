**********
3dVol2Surf
**********

.. _3dVol2Surf:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    3dVol2Surf - map data from a volume domain to a surface domain
    
      usage: 3dVol2Surf [options] -spec SPEC_FILE -sv SURF_VOL \
                        -grid_parent AFNI_DSET -map_func MAP_FUNC
    
    This program is used to map data values from an AFNI volume
    dataset to a surface dataset.  A filter may be applied to the
    volume data to produce the value(s) for each surface node.
    
    The surface and volume domains are spacially matched via the
    'surface volume' AFNI dataset.  This gives each surface node xyz
    coordinates, which are then matched to the input 'grid parent'
    dataset.  This grid parent is an AFNI dataset containing the
    data values destined for output.
    
    Typically, two corresponding surfaces will be input (via the
    spec file and the '-surf_A' and '-surf_B' options), along with
    a mapping function and relevant options.  The mapping function
    will act as a filter over the values in the AFNI volume.
    
    Note that an alternative to using a second surface with the
    '-surf_B' option is to define the second surface by using the
    normals from the first surface.  By default, the second surface
    would be defined at a distance of 1mm along the normals, but the
    user may modify the applied distance (and direction).  See the
    '-use_norms' and '-norm_len' options for more details.
    
    For each pair of corresponding surface nodes, let NA be the node
    on surface A (such as a white/grey boundary) and NB be the
    corresponding node on surface B (such as a pial surface).  The
    filter is applied to the volume data values along the segment
    from NA to NB (consider the average or maximum as examples of
    filters).
    
    Note: if either endpoint of a segment is outside the grid parent
          volume, that node (pair) will be skipped.
    
    Note: surface A corresponds to the required '-surf_A' argument,
          while surface B corresponds to '-surf_B'.
    
    By default, this segment only consists of the endpoints, NA and
    NB (the actual nodes on the two surfaces).  However the number
    of evenly spaced points along the segment may be specified with
    the -f_steps option, and the actual locations of NA and NB may
    be altered with any of the -f_pX_XX options, covered below.
    
    As an example, for each node pair, one could output the average
    value from some functional dataset along a segment of 10 evenly
    spaced points, where the segment endpoints are defined by the
    xyz coordinates of the nodes.  This is example 3, below.
    
    The mapping function (i.e. filter) is a required parameter to
    the program.
    
    Brief descriptions of the current mapping functions are as
    follows.  These functions are defined over a segment of points.
    
        ave       : output the average of all voxel values along the
                    segment
        mask      : output the voxel value for the trivial case of a
                    segment - defined by a single surface point
        median    : output the median value from the segment
        midpoint  : output the dataset value at the segment midpoint
        mode      : output the mode of the values along the segment
        max       : output the maximum volume value over the segment
        max_abs   : output the dataset value with max abs over seg
        min       : output the minimum volume value over the segment
        seg_vals  : output _all_ volume values over the segment (one
                    sub-brick only)
    
      --------------------------------------------------
    
      examples:
    
        1. Apply a single surface mask to output volume values over
           each surface node.  Output is one value per sub-brick
           (per surface node).
    
        3dVol2Surf                                \
           -spec         fred.spec                \
           -surf_A       smoothwm                 \
           -sv           fred_anat+orig           \
           -grid_parent  fred_anat+orig           \
           -map_func     mask                     \
           -out_1D       fred_anat_vals.1D
    
        2. Apply a single surface mask to output volume values over
           each surface node.  In this case restrict input to the
           mask implied by the -cmask option.  Supply additional
           debug output, and more for surface node 1874
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -sv           fred_anat+orig                           \
           -grid_parent 'fred_epi+orig[0]'                        \
           -cmask       '-a fred_func+orig[2] -expr step(a-0.6)'  \
           -map_func     mask                                     \
           -debug        2                                        \
           -dnode        1874                                     \
           -out_niml     fred_epi_vals.niml.dset
    
        3. Given a pair of related surfaces, for each node pair,
           break the connected line segment into 10 points, and
           compute the average dataset value over those points.
           Since the index is nodes, each of the 10 points will be
           part of the average.  This could be changed so that only
           values from distinct volume nodes are considered (by
           changing the -f_index from nodes to voxels).  Restrict
           input voxels to those implied by the -cmask option
           Output is one average value per sub-brick (per surface
           node).
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -surf_B       pial                                     \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_func+orig                           \
           -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \
           -map_func     ave                                      \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -out_niml     fred_func_ave.niml.dset
    
        4. Similar to example 3, but restrict the output columns to
           only node indices and values (i.e. skip 1dindex, i, j, k
           and vals).
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -surf_B       pial                                     \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_func+orig                           \
           -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \
           -map_func     ave                                      \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -skip_col_1dindex                                      \
           -skip_col_i                                            \
           -skip_col_j                                            \
           -skip_col_k                                            \
           -skip_col_vals                                         \
           -out_niml     fred_func_ave_short.niml.dset
    
        5. Similar to example 3, but each of the node pair segments
           has grown by 10% on the inside of the first surface,
           and 20% on the outside of the second.  This is a 30%
           increase in the length of each segment.  To shorten the
           node pair segment, use a '+' sign for p1 and a '-' sign
           for pn.
           As an interesting side note, '-f_p1_fr 0.5 -f_pn_fr -0.5'
           would give a zero length vector identical to that of the
           'midpoint' filter.
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -surf_B       pial                                     \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_func+orig                           \
           -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \
           -map_func     ave                                      \
           -f_steps      10                                       \
           -f_index      voxels                                   \
           -f_p1_fr      -0.1                                     \
           -f_pn_fr      0.2                                      \
           -out_niml     fred_func_ave2.niml.dset
    
        6. Similar to example 3, instead of computing the average
           across each segment (one average per sub-brick), output
           the volume value at _every_ point across the segment.
           The output here would be 'f_steps' values per node pair,
           though the output could again be restricted to unique
           voxels along each segment with '-f_index voxels'.
           Note that only sub-brick 0 will be considered here.
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -surf_B       pial                                     \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_func+orig                           \
           -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \
           -map_func     seg_vals                                 \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -out_niml     fred_func_segvals_10.niml.dset
    
        7. Similar to example 6, but make sure there is output for
           every node pair in the surfaces.  Since it is expected
           that some nodes are out of bounds (meaning that they lie
           outside the domain defined by the grid parent dataset),
           the '-oob_value' option is added to include a default
           value of 0.0 in such cases.  And since it is expected
           that some node pairs are "out of mask" (meaning that
           their resulting segment lies entirely outside the cmask),
           the '-oom_value' was added to output the same default
           value of 0.0.
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -surf_B       pial                                     \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_func+orig                           \
           -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \
           -map_func     seg_vals                                 \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -oob_value    0.0                                      \
           -oom_value    0.0                                      \
           -out_niml     fred_func_segvals_10_all.niml.dset
    
        8. This is a basic example of calculating the average along
           each segment, but where the segment is produced by only
           one surface, along with its set of surface normals.  The
           segments will be 2.5 mm in length.
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_anat+orig                           \
           -use_norms                                             \
           -norm_len     2.5                                      \
           -map_func     ave                                      \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -out_niml     fred_anat_norm_ave.2.5.niml.dset
    
        9. This is the same as example 8, but where the surface
           nodes are restricted to the range 1000..1999 via the
           options '-first_node' and '-last_node'.
    
        3dVol2Surf                                                \
           -spec         fred.spec                                \
           -surf_A       smoothwm                                 \
           -sv           fred_anat+orig                           \
           -grid_parent  fred_anat+orig                           \
           -first_node   1000                                     \
           -last_node    1999                                     \
           -use_norms                                             \
           -norm_len     2.5                                      \
           -map_func     ave                                      \
           -f_steps      10                                       \
           -f_index      nodes                                    \
           -out_niml     fred_anat_norm_ave.2.5.niml.dset
    
       10. Create an EPI time-series surface dataset, suitable for
           performing single-subject processing on the surface.  So
           map a time-series onto each surface node.
    
           Note that any time shifting (3dTshift) or registration
           of volumes (3dvolreg) should be done before this step.
    
           After this step, the user can finish pre-processing with
           blurring (SurfSmooth) and scaling (3dTstat, 3dcalc),
           before performing the regression (3dDeconvolve).
    
        3dVol2Surf                                                \
           -spec                fred.spec                         \
           -surf_A              smoothwm                          \
           -surf_B              pial                              \
           -sv                  SurfVolAlndExp+orig               \
           -grid_parent         EPI_all_runs+orig                 \
           -map_func            ave                               \
           -f_steps             15                                \
           -f_index             nodes                             \
           -outcols_NSD_format                                    \
           -out_niml            EPI_runs.niml.dset
    
      --------------------------------------------------
    
      REQUIRED COMMAND ARGUMENTS:
    
        -spec SPEC_FILE        : SUMA spec file
    
            e.g. -spec fred.spec
    
            The surface specification file contains the list of
            mappable surfaces that are used.
    
            See @SUMA_Make_Spec_FS and @SUMA_Make_Spec_SF.
    
        -surf_A SURF_NAME      : name of surface A (from spec file)
        -surf_B SURF_NAME      : name of surface B (from spec file)
    
            e.g. -surf_A smoothwm
            e.g. -surf_A lh.smoothwm
            e.g. -surf_B lh.pial
    
            This is used to specify which surface(s) will be used by
            the program.  The '-surf_A' parameter is required, as it
            specifies the first surface, whereas since '-surf_B' is
            used to specify an optional second surface, it is not
            required.
    
            Note that any need for '-surf_B' may be fulfilled using
            the '-use_norms' option.
    
            Note that any name provided must be in the spec file,
            uniquely matching the name of a surface node file (such
            as lh.smoothwm.asc, for example).  Note that if both
            hemispheres are represented in the spec file, then there
            may be both lh.pial.asc and rh.pial.asc, for instance.
            In such a case, 'pial' would not uniquely determine a
            a surface, but the name 'lh.pial' would.
    
        -sv SURFACE_VOLUME     : AFNI volume dataset
    
            e.g. -sv fred_anat+orig
    
            This is the AFNI dataset that the surface is mapped to.
            This dataset is used for the initial surface node to xyz
            coordinate mapping, in the Dicom orientation.
    
        -grid_parent AFNI_DSET : AFNI volume dataset
    
            e.g. -grid_parent fred_function+orig
    
            This dataset is used as a grid and orientation master
            for the output (i.e. it defines the volume domain).
            It is also the source of the output data values.
    
        -map_func MAP_FUNC     : filter for values along the segment
    
            e.g. -map_func ave
            e.g. -map_func ave -f_steps 10
            e.g. -map_func ave -f_steps 10 -f_index nodes
    
            The current mapping function for 1 surface is:
    
              mask     : For each surface xyz location, output the
                         dataset values of each sub-brick.
    
            Most mapping functions are defined for 2 related input
            surfaces (such as white/grey boundary and pial).  For
            each node pair, the function will be performed on the
            values from the 'grid parent dataset', and along the
            segment connecting the nodes.
    
              ave      : Output the average of the dataset values
                         along the segment.
    
              max      : Output the maximum dataset value along the
                         connecting segment.
    
              max_abs  : Output the dataset value with the maximum
                         absolute value along the segment.
    
              median   : Output the median of the dataset values
                         along the connecting segment.
    
              midpoint : Output the dataset value with xyz
                         coordinates at the midpoint of the nodes.
    
              min      : Output the minimum dataset value along the
                         connecting segment.
    
              mode     : Output the mode of the dataset values along
                         the connecting segment.
    
              nzave, nzmin, nzmax : Non-zero equivalents to ave, min, max
                         Does not include the zero values in the
                         computation
    
              seg_vals : Output all of the dataset values along the
                         connecting segment.  Here, only sub-brick
                         number 0 will be considered.
    
      ------------------------------
    
      options specific to functions on 2 surfaces:
    
              -f_steps NUM_STEPS :
    
                         Use this option to specify the number of
                         evenly spaced points along each segment.
                         The default is 2 (i.e. just use the two
                         surface nodes as endpoints).
    
                         e.g.     -f_steps 10
                         default: -f_steps 2
    
              -f_index TYPE :
    
                         This option specifies whether to use all
                         segment point values in the filter (using
                         the 'nodes' TYPE), or to use only those
                         corresponding to unique volume voxels (by
                         using the 'voxel' TYPE).
    
                         For instance, when taking the average along
                         one node pair segment using 10 node steps,
                         perhaps 3 of those nodes may occupy one
                         particular voxel.  In this case, does the
                         user want the voxel counted only once, or 3
                         times?  Each way makes sense.
                         
                         Note that this will only make sense when
                         used along with the '-f_steps' option.
                         
                         Possible values are "nodes", "voxels".
                         The default value is voxels.  So each voxel
                         along a segment will be counted only once.
                         
                         e.g.  -f_index nodes
                         e.g.  -f_index voxels
                         default: -f_index voxels
    
              -f_keep_surf_order :
    
                         Depreciated.
    
                         See required arguments -surf_A and -surf_B,
                         above.
    
              Note: The following -f_pX_XX options are used to alter
                    the lengths and locations of the computational
                    segments.  Recall that by default, segments are
                    defined using the node pair coordinates as
                    endpoints.  And the direction from p1 to pn is
                    from the inner surface to the outer surface.
    
              -f_p1_mm DISTANCE :
    
                         This option is used to specify a distance
                         in millimeters to add to the first point of
                         each line segment (in the direction of the
                         second point).  DISTANCE can be negative
                         (which would set p1 to be farther from pn
                         than before).
    
                         For example, if a computation is over the
                         grey matter (from the white matter surface
                         to the pial), and it is wished to increase
                         the range by 1mm, set this DISTANCE to -1.0
                         and the DISTANCE in -f_pn_mm to 1.0.
    
                         e.g.  -f_p1_mm -1.0
                         e.g.  -f_p1_mm -1.0 -f_pn_mm 1.0
    
              -f_pn_mm DISTANCE :
    
                         Similar to -f_p1_mm, this option is used
                         to specify a distance in millimeters to add
                         to the second point of each line segment.
                         Note that this is in the same direction as
                         above, from point p1 to point pn.
                         
                         So a positive DISTANCE, for this option,
                         would set pn to be farther from p1 than
                         before, and a negative DISTANCE would set
                         it to be closer.
    
                         e.g.  -f_pn_mm 1.0
                         e.g.  -f_p1_mm -1.0 -f_pn_mm 1.0
    
              -f_p1_fr FRACTION :
    
                         Like the -f_pX_mm options above, this
                         is used to specify a change to point p1, in
                         the direction of point pn, but the change
                         is a fraction of the original distance,
                         not a pure change in millimeters.
                         
                         For example, suppose one wishes to do a
                         computation based on the segments spanning
                         the grey matter, but to add 20% to either
                         side.  Then use -0.2 and 0.2:
    
                         e.g.  -f_p1_fr -0.2
                         e.g.  -f_p1_fr -0.2 -f_pn_fr 0.2
    
              -f_pn_fr FRACTION :
    
                         See -f_p1_fr above.  Note again that the
                         FRACTION is in the direction from p1 to pn.
                         So to extend the segment past pn, this
                         FRACTION will be positive (and to reduce
                         the segment back toward p1, this -f_pn_fr
                         FRACTION will be negative).
    
                         e.g.  -f_pn_fr 0.2
                         e.g.  -f_p1_fr -0.2 -f_pn_fr 0.2
    
                         Just for entertainment, one could reverse
                         the order that the segment points are
                         considered by adjusting p1 to be pn, and
                         pn to be p1.  This could be done by adding
                         a fraction of 1.0 to p1 and by subtracting
                         a fraction of 1.0 from pn.
    
                         e.g.  -f_p1_fr 1.0 -f_pn_fr -1.0
    
      ------------------------------
    
      options specific to use of normals:
    
        Notes:
    
          o Using a single surface with its normals for segment
            creation can be done in lieu of using two surfaces.
    
          o Normals at surface nodes are defined by the average of
            the normals of the triangles including the given node.
    
          o The default normals have a consistent direction, but it
            may be opposite of what is should be.  For this reason,
            the direction is verified by default, and may be negated
            internally.  See the '-keep_norm_dir' option for more
            information.
    
        -use_norms             : use normals for second surface
    
            Segments are usually defined by connecting corresponding
            node pairs from two surfaces.  With this options the
            user can use one surface, along with its normals, to
            define the segments.
    
            By default, each segment will be 1.0 millimeter long, in
            the direction of the normal.  The '-norm_len' option
            can be used to alter this default action.
    
        -keep_norm_dir         : keep the direction of the normals
    
            Normal directions are verified by checking that the
            normals of the outermost 6 points point away from the
            center of mass.  If they point inward instead, then
            they are negated.
    
            This option will override the directional check, and
            use the normals as they come.
    
            See also -reverse_norm_dir, below.
    
        -norm_len LENGTH       : use LENGTH for node normals
    
            e.g.     -norm_len  3.0
            e.g.     -norm_len -3.0
            default: -norm_len  1.0
    
            For use with the '-use_norms' option, this allows the
            user to specify a directed distance to use for segments
            based on the normals.  So for each node on a surface,
            the computation segment will be from the node, in the
            direction of the normal, a signed distance of LENGTH.
    
            A negative LENGTH means to use the opposite direction
            from the normal.
    
            The '-surf_B' option is not allowed with the use of
            normals.
    
        -reverse_norm_dir      : reverse the normal directions
    
            Normal directions are verified by checking that the
            normals of the outermost 6 points point away from the
            center of mass.  If they point inward instead, then
            they are negated.
    
            This option will override the directional check, and
            reverse the direction of the normals as they come.
    
            See also -keep_norm_dir, above.
    
      ------------------------------
    
      output options:
    
        -debug LEVEL           :  (optional) verbose output
    
            e.g. -debug 2
    
            This option is used to print out status information 
            during the execution of the program.  Current levels are
            from 0 to 5.
    
        -first_node NODE_NUM   : skip all previous nodes
    
            e.g. -first_node 1000
            e.g. -first_node 1000 -last_node 1999
    
            Restrict surface node output to those with indices as
            large as NODE_NUM.  In the first example, the first 1000
            nodes are ignored (those with indices from 0 through
            999).
    
            See also, '-last_node'.
    
        -dnode NODE_NUM        :  (optional) node for debug
    
            e.g. -dnode 1874
    
            This option is used to print out status information 
            for node NODE_NUM.
    
        -out_1D OUTPUT_FILE    : specify a 1D file for the output
    
            e.g. -out_1D mask_values_over_dataset.1D
    
            This is where the user will specify which file they want
            the output to be written to.  In this case, the output
            will be in readable, column-formatted ASCII text.
    
            Note : the output file should not yet exist.
                 : -out_1D or -out_niml must be used
    
        -out_niml OUTPUT_FILE  : specify a niml file for the output
    
            e.g. -out_niml mask_values_over_dataset.niml.dset
    
            The user may use this option to get output in the form
            of a niml element, with binary data.  The output will
            contain (binary) columns of the form:
    
                node_index  value_0  value_1  value_2  ...
    
            A major difference between 1D output and niml output is
            that the value_0 column number will be 6 in the 1D case,
            but will be 2 in the niml case.  The index columns will
            not be used for niml output.
            It is possible to write niml datasets in both ASCII and 
            BINARY formats. BINARY format is recommended for large
            datasets. The .afnirc environment variable:
            AFNI_NIML_TEXT_DATA controls whether output is
            ASCII (YES) or BINARY (NO).
    
            Note : the output file should not yet exist.
                 : -out_1D or -out_niml must be used
    
        -help                  : show this help
    
            If you can't get help here, please get help somewhere.
    
        -hist                  : show revision history
    
            Display module history over time.
    
            See also, -v2s_hist
    
        -last_node NODE_NUM    : skip all following nodes
    
            e.g. -last_node 1999
            e.g. -first_node 1000 -last_node 1999
    
            Restrict surface node output to those with indices no
            larger than NODE_NUM.  In the first example, nodes above
            1999 are ignored (those with indices from 2000 on up).
    
            See also, '-first_node'.
    
        -no_headers            : do not output column headers
    
            Column header lines all begin with the '#' character.
            With the '-no_headers' option, these lines will not be
            output.
    
        -oob_index INDEX_NUM   : specify default index for oob nodes
    
            e.g.     -oob_index -1
            default: -oob_index  0
    
            By default, nodes which lie outside the box defined by
            the -grid_parent dataset are considered out of bounds,
            and are skipped.  If an out of bounds index is provided,
            or an out of bounds value is provided, such nodes will
            not be skipped, and will have indices and values output,
            according to the -oob_index and -oob_value options.
            
            This INDEX_NUM will be used for the 1dindex field, along
            with the i, j and k indices.
            
    
        -oob_value VALUE       : specify default value for oob nodes
    
            e.g.     -oob_value -999.0
            default: -oob_value    0.0
    
            See -oob_index, above.
            
            VALUE will be output for nodes which are out of bounds.
    
        -oom_value VALUE       : specify default value for oom nodes
    
            e.g. -oom_value -999.0
            e.g. -oom_value    0.0
    
            By default, node pairs defining a segment which gets
            completely obscured by a command-line mask (see -cmask)
            are considered "out of mask", and are skipped.
    
            If an out of mask value is provided, such nodes will not
            be skipped.  The output indices will come from the first
            segment point, mapped to the AFNI volume.  All output vN
            values will be the VALUE provided with this option.
    
            This option is meaningless without a '-cmask' option.
    
        -outcols_afni_NSD      : output nodes and one result column
        -outcols_1_result      : output only one result column
        -outcols_results       : output only all result columns
        -outcols_NSD_format    : output nodes and all results
                                 (NI_SURF_DSET foramt)
    
            These options are used to restrict output.  They are
            similar to the -skip_col_* options, but are used to
            choose columns to output (they are for convenience, so
            the user need not apply many -skip_col options).
    
            see also: -skip_col_*
    
        -save_seg_coords FILE  : save segment coordinates to FILE
    
            e.g. -save_seg_coords seg.coords.1D
    
            Each node that has output values computed along a valid
            segment (i.e. not out-of-bounds or out-of-mask) has its
            index written to this file, along with all applied
            segment coordinates.
    
        -skip_col_nodes        : do not output node column
        -skip_col_1dindex      : do not output 1dindex column
        -skip_col_i            : do not output i column
        -skip_col_j            : do not output j column
        -skip_col_k            : do not output k column
        -skip_col_vals         : do not output vals column
    
            These options are used to restrict output.  Each option
            will prevent the program from writing that column of
            output to the 1D file.
    
            For now, the only effect that these options can have on
            the niml output is by skipping nodes or results (all
            other columns are skipped by default).
    
            see also: -outcols_*
    
        -v2s_hist              : show revision history for library
    
            Display vol2surf library history over time.
    
            See also, -hist
    
        -version               : show version information
    
            Show version and compile date.
    
      ------------------------------
    
      general options:
    
        -cmask MASK_COMMAND    : (optional) command for dataset mask
    
            e.g. -cmask '-a fred_func+orig[2] -expr step(a-0.8)'
    
            This option will produce a mask to be applied to the
            input AFNI dataset.  Note that this mask should form a
            single sub-brick.
    
            This option follows the style of 3dmaskdump (since the
            code for it was, uh, borrowed from there (thanks Bob!)).
    
            See '3dmaskdump -help' for more information.
    
        -gp_index SUB_BRICK    : choose grid_parent sub-brick
    
            e.g. -gp_index 3
    
            This option allows the user to choose only a single
            sub-brick from the grid_parent dataset for computation.
            Note that this option is virtually useless when using
            the command-line, as the user can more directly do this
            via brick selectors, e.g. func+orig'[3]'.
            
            This option was written for the afni interface.
    
      --------------------------------------------------
    
    Output from the program defaults to 1D format, in ascii text.
    For each node (pair) that results in output, there will be one
    line, consisting of:
    
        node    : the index of the current node (or node pair)
    
        1dindex : the global index of the AFNI voxel used for output
    
                  Note that for some filters (min, max, midpoint,
                  median and mode) there is a specific location (and
                  therefore voxel) that the result comes from.  It
                  will be accurate (though median may come from one
                  of two voxels that are averaged).
    
                  For filters without a well-defined source (such as
                  average or seg_vals), the 1dindex will come from
                  the first point on the corresponding segment.
    
                  Note: this will _not_ be output in the niml case.
    
        i j k   : the i j k indices matching 1dindex
    
                  These indices are based on the orientation of the
                  grid parent dataset.
    
                  Note: these will _not_ be output in the niml case.
    
        vals    : the number of segment values applied to the filter
    
                  Note that when -f_index is 'nodes', this will
                  always be the same as -f_steps, except when using
                  the -cmask option.  In that case, along a single 
                  segment, some points may be in the mask, and some
                  may not.
    
                  When -f_index is 'voxels' and -f_steps is used,
                  vals will often be much smaller than -f_steps.
                  This is because many segment points may in a
                  single voxel.
    
                  Note: this will _not_ be output in the niml case.
    
        v0, ... : the requested output values
    
                  These are the filtered values, usually one per
                  AFNI sub-brick.  For example, if the -map_func
                  is 'ave', then there will be one segment-based
                  average output per sub-brick of the grid parent.
    
                  In the case of the 'seg_vals' filter, however,
                  there will be one output value per segment point
                  (possibly further restricted to voxels).  Since
                  output is not designed for a matrix of values,
                  'seg_vals' is restricted to a single sub-brick.
    
    
      Author: R. Reynolds  - version  6.10 (Aug 30, 2010)
    
                    (many thanks to Z. Saad and R.W. Cox)
