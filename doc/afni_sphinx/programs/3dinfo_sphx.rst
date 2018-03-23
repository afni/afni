.. _ahelp_3dinfo:

******
3dinfo
******

.. contents:: 
    :depth: 4 

| 

    
    Prints out sort-of-useful information from a 3D dataset's header

Usage: 3dinfo [-verb OR -short] dataset [dataset ...]
=====================================================

.. code-block:: none

      -verb means to print out lots of stuff
      -VERB means even more stuff [including slice time offsets]
      -short means to print out less stuff [now the default]
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      

Alternative Usage (without either of the above options):
========================================================

.. code-block:: none

      3dinfo -label2index label dataset
      * Prints to stdout the index corresponding to the sub-brick with
        the name label, or a blank line if label not found.
      * If this option is used, then the ONLY output is this sub-brick index.
        This is intended to be used in a script, as in this tcsh fragment:
          set face = `3dinfo -label2index Face#0 AA_Decon+orig`
          set hous = `3dinfo -label2index House#0 AA_Decon+orig`
          3dcalc -a AA_Decon+orig"[$face]" -b AA_Decon+orig"[$hous]" ...
      * Added per the request and efforts of Colm Connolly.
    

Alternate Alternative Usage:
============================

.. code-block:: none

      3dinfo <OPTION> [OPTION ..] dataset [dataset ...]
      Outputs a specific piece of information depending on OPTION.
    

Options producing one value (string)
++++++++++++++++++++++++++++++++++++

.. code-block:: none

       -exists: 1 if dset is loadable, 0 otherwise
                This works on prefix also.
       -id: Idcodestring of dset
       -is_atlas: 1 if dset is an atlas.
       -is_nifti: 1 if dset is NIFTI format, 0 otherwise
       -space: dataset's space
       -gen_space: datasets generic space
       -av_space: AFNI format's view extension for the space
       -is_oblique: 1 if dset is oblique
       -handedness: L if orientation is Left handed, R if it is right handed
       -obliquity: Angle from plumb direction.
                   Angles of 0 (or close) are for cardinal orientations
       -prefix: Return the prefix
       -prefix_noext: Return the prefix without extensions
       -n[i|j|k]: Return the number of voxels in i, j, k dimensions
       -nijk: Return ni*nj*nk
       -nv: Return number of points in time or the number of sub-bricks
       -nt: same as -nv
       -n4: same as -ni -nj -nk -nv
       -nvi: The maximum sub-brick index (= nv -1 )
       -nti: same as -nvi
       -ntimes: Return number of sub-bricks points in time
            This is an option for debugging use, stay away from it.
       -max_node: For a surface-based dset, return the maximum node index
       -di: Signed displacement per voxel along i direction, aka dx
       -dj: Signed displacement per voxel along j direction, aka dy
       -dk: Signed displacement per voxel along k direction, aka dz
       -d3: same as -di -dj -dk
       -adi: Voxel size along i direction (abs(di))
       -adj: Voxel size along j direction (abs(dj))
       -adk: Voxel size along k direction (abs(dk))
       -ad3: same as -adi -adj -adk
       -voxvol: Voxel volume in cubic millimeters
       -oi: Volume origin along the i direction
       -oj: Volume origin along the j direction
       -ok: Volume origin along the k direction
       -o3: same as -oi -oj -ok
       -tr: The TR value in seconds.
       -dmin: The dataset's minimum value, scaled by fac
       -dmax: The dataset's maximum value, scaled by fac
       -dminus: The dataset's minimum value, unscaled.
       -dmaxus: The dataset's maximum value, unscaled.
       -smode: Dset storage mode string.
       -header_name: Value of dset structure (sub)field 'header_name'
       -brick_name: Value of dset structure (sub)field 'brick_name'
       -iname: Name of dset as input on the command line
       -orient: Value of orientation string.
                For example, LPI means:
                   i direction grows from Left(negative) to Right(positive).
                   j direction grows from Posterior (neg.) to Anterior (pos.)
                   k direction grows from Inferior (neg.) to Superior (pos.)
       -extent: The spatial extent of the dataset along R, L, A, P, I and S
       -Rextent: Extent along R
       -Lextent: Extent along L
       -Aextent: Extent along P
       -Pextent: Extent along P
       -Iextent: Extent along I
       -Sextent: Extent along S
       -all_names: Value of various dset structures handling filenames.
    

Options producing one value per sub-brick
+++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

       -fac: Return the float scaling factor
       -label: The label of each sub-brick
       -datum: The data storage type
       -min: The minimum value, scaled by fac
       -max: The maximum value, scaled by fac
       -minus: The minimum value, unscaled.
       -maxus: The maximum value, unscaled.
    

Options producing multiple values (strings of multiple lines)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

       You can specify the delimiter between sub-brick parameters with
           -sb_delim DELIM. Default DELIM is "|"
       -labeltable: Show label table, if any
       -labeltable_as_atlas_points: Show label table in atlas point format.
       -atlas_points: Show atlas points list, if any
       -history: History note. 
       -slice_timing: Show slice timing. 
    

Options affecting output format
+++++++++++++++++++++++++++++++

.. code-block:: none

       -header_line: Output as the first line the names of attributes
                     in each field (column)
       -hdr: Same as -header_line
       -sb_delim SB_DELIM: Delimiter string between sub-brick values
                           Default SB_DELIM is "|"
       -NA_flag NAFLAG: String to use when a field is not found or not
                        applicable. Default is "NA"
       -atr_delim ATR_DELIM: Delimiter string between attributes
                             Default ATR_DELIM is the tab character.
    

Options requiring dataset pairing at input
++++++++++++++++++++++++++++++++++++++++++

.. code-block:: none

        3dinfo allows you to make some comparisons between dataset pairs.
        The comparison is always done in both directions whether or not
        the answer can be different. For example:
              3dinfo -same_grid dset1 dset2 
        will output two values, one comparing dset1 to dset2 and the second
        comparing dset2 to dset1. With -same_grid, the answers will always
        be identical, but this might be different for other queries.
        This behaviour allows you to mix options requiring dataset pairs
        with those that do not. For example:
              3dinfo -header_line -prefix -n4 -same_grid \
                                  DSET1+orig DSET2.nii DSET3.nii DSET4.nii
    
       -same_grid: Output 1 if the grid is identical between two dsets
                          0 otherwise. 
                   For -same_grid to be 1, all of -same_dim, -same_delta,
                   -same_orient, -same_center, and -same_obl must return 1
       -same_dim: 1 if dimensions are the same between dset pairs
       -same_delta: 1 if voxels sizes are the same between dset pairs
       -same_orient: 1 if orientation is the same between dset pairs
       -same_center: 1 if geometric center is the same between dset pairs
       -same_obl: 1 if obliquity is the same between dset pairs
       -same_all_grid: Equivalent to listing all of -same_dim -same_delta
                       -same_orient, -same_center, and -same_obl on the 
                       command line.
       -val_diff: Output the sum of absolute differences of all voxels in the
                  dataset pair.
       -sval_diff: Same as -val_diff, but the sum is divided (scaled) by the 
                   total number of voxels that are not zero in at least one
                   of the two datasets.
    
       -monog_pairs: Instead of pairing each dset with the first, pair each
                    couple separately. This requires you to have an even
                    number of dsets on the command line
    

Examples with csh syntax using datasets in your afni binaries directory
=======================================================================

.. code-block:: none

    
      0- First get some datasets with which we'll play
         set dsets = ( `apsearch -list_all_afni_P_dsets` )
    
      1- The classic
         3dinfo $dsets[1]
    
      2- Produce a table of results using 1-value-options for two datasets
         3dinfo  -echo_edu -prefix_noext -prefix -space -ni -nj -nk -nt  \
                   $dsets[1-2]
    
      3- Use some of the options that operate on pairs, mix with other options
         3dinfo -echo_edu -header_line -prefix -n4 -same_grid $dsets[1-4]
    
    
    
    ++ Compile date = Mar 22 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
