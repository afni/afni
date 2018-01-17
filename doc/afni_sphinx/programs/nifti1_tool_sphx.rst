***********
nifti1_tool
***********

.. _nifti1_tool:

.. contents:: 
    :depth: 4 

.. code-block:: none

    nifti_tool
    
       - display, modify or compare nifti structures in datasets
       - copy a dataset by selecting a list of volumes from the original
       - copy a dataset, collapsing any dimensions, each to a single index
       - display a time series for a voxel, or more generally, the data
           from any collapsed image, in ASCII text
    
      This program can be used to display information from nifti datasets,
      to modify information in nifti datasets, to look for differences
      between two nifti datasets (like the UNIX 'diff' command), and to copy
      a dataset to a new one, either by restricting any dimensions, or by
      copying a list of volumes (the time dimension) from a dataset.
    
      Only one action type is allowed, e.g. one cannot modify a dataset
      and then take a 'diff'.
    
      one can display - any or all fields in the nifti_1_header structure
                      - any or all fields in the nifti_image structure
                      - any or all fields in the nifti_analyze75 structure
                      - the extensions in the nifti_image structure
                      - the time series from a 4-D dataset, given i,j,k
                      - the data from any collapsed image, given dims. list
    
      one can check   - perform internal check on the nifti_1_header struct
                        (by nifti_hdr_looks_good())
                      - perform internal check on the nifti_image struct
                        (by nifti_nim_is_valid())
    
      one can modify  - any or all fields in the nifti_1_header structure
                      - any or all fields in the nifti_image structure
                      - swap all fields in NIFTI or ANALYZE header structure
              add/rm  - any or all extensions in the nifti_image structure
              remove  - all extensions and descriptions from the datasets
    
      one can compare - any or all field pairs of nifti_1_header structures
                      - any or all field pairs of nifti_image structures
    
      one can copy    - an arbitrary list of dataset volumes (time points)
                      - a dataset, collapsing across arbitrary dimensions
                        (restricting those dimensions to the given indices)
    
      one can create  - a new dataset out of nothing
    
      Note: to learn about which fields exist in either of the structures,
            or to learn a field's type, size of each element, or the number
            of elements in the field, use either the '-help_hdr' option, or
            the '-help_nim' option.  No further options are required.
      ------------------------------
    
      usage styles:
    
        nifti_tool -help                 : show this help
        nifti_tool -help_hdr             : show nifti_1_header field info
        nifti_tool -help_nim             : show nifti_image field info
        nifti_tool -help_ana             : show nifti_analyze75 field info
        nifti_tool -help_datatypes       : show datatype table
    
        nifti_tool -ver                  : show the current version
        nifti_tool -hist                 : show the modification history
        nifti_tool -nifti_ver            : show the nifti library version
        nifti_tool -nifti_hist           : show the nifti library history
        nifti_tool -with_zlib            : was library compiled with zlib
    
    
        nifti_tool -check_hdr -infiles f1 ...
        nifti_tool -check_nim -infiles f1 ...
    
        nifti_tool -copy_brick_list -infiles f1'[indices...]'
        nifti_tool -copy_collapsed_image I J K T U V W -infiles f1
        nifti_tool -copy_im -infiles f1
    
        nifti_tool -make_im -prefix new_im.nii
    
        nifti_tool -disp_hdr [-field FIELDNAME] [...] -infiles f1 ...
        nifti_tool -disp_nim [-field FIELDNAME] [...] -infiles f1 ...
        nifti_tool -disp_ana [-field FIELDNAME] [...] -infiles f1 ...
        nifti_tool -disp_exts -infiles f1 ...
        nifti_tool -disp_ts I J K [-dci_lines] -infiles f1 ...
        nifti_tool -disp_ci I J K T U V W [-dci_lines] -infiles f1 ...
    
        nifti_tool -mod_hdr  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1
        nifti_tool -mod_nim  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1
    
        nifti_tool -swap_as_nifti   -overwrite -infiles f1
        nifti_tool -swap_as_analyze -overwrite -infiles f1
        nifti_tool -swap_as_old     -overwrite -infiles f1
    
        nifti_tool -add_afni_ext    'extension in quotes' [...] -infiles f1
        nifti_tool -add_comment_ext 'extension in quotes' [...] -infiles f1
        nifti_tool -add_comment_ext 'file:FILENAME' [...] -infiles f1
        nifti_tool -rm_ext INDEX [...] -infiles f1 ...
        nifti_tool -strip_extras -infiles f1 ...
    
        nifti_tool -diff_hdr [-field FIELDNAME] [...] -infiles f1 f2
        nifti_tool -diff_nim [-field FIELDNAME] [...] -infiles f1 f2
    
      ------------------------------
    
      selected examples:
    
        A. checks header (for problems):
    
          1. nifti_tool -check_hdr -infiles dset0.nii dset1.nii
          2. nifti_tool -check_hdr -infiles *.nii *.hdr
          3. nifti_tool -check_hdr -quiet -infiles *.nii *.hdr
    
        B. show header differences:
    
          1. nifti_tool -diff_hdr -field dim -field intent_code  \
                        -infiles dset0.nii dset1.nii 
          2. nifti_tool -diff_hdr -new_dims 3 10 20 30 0 0 0 0   \
                        -infiles my_dset.nii MAKE_IM 
    
        C. display structures or fields:
    
          1. nifti_tool -disp_hdr -infiles dset0.nii dset1.nii dset2.nii
          2. nifti_tool -disp_hdr -field dim -field descrip -infiles dset.nii
          3. nifti_tool -disp_exts -infiles dset0.nii dset1.nii dset2.nii
          4. nifti_tool -disp_ts 23 0 172 -infiles dset1_time.nii
          5. nifti_tool -disp_ci 23 0 172 -1 0 0 0 -infiles dset1_time.nii
    
          6. nifti_tool -disp_ana -infiles analyze.hdr
          7. nifti_tool -disp_nim -infiles nifti.nii
    
        D. create a new dataset from nothing:
    
          1. nifti_tool -make_im -prefix new_im.nii 
          2. nifti_tool -make_im -prefix float_im.nii \
                        -new_dims 3 10 20 30 0 0 0 0  -new_datatype 16
          3. nifti_tool -mod_hdr -mod_field descrip 'dataset with mods'  \
                        -new_dims 3 10 20 30 0 0 0 0                     \
                        -prefix new_desc.nii -infiles MAKE_IM
    
        E. copy dataset, brick list or collapsed image:
    
          1. nifti_tool -copy_im -prefix new.nii -infiles dset0.nii
          2. nifti_tool -cbl -prefix new_07.nii -infiles dset0.nii'[0,7]'
          3. nifti_tool -cbl -prefix new_partial.nii \
                        -infiles dset0.nii'[3..$(2)]'
    
          4. nifti_tool -cci 5 4 17 -1 -1 -1 -1 -prefix new_5_4_17.nii
          5. nifti_tool -cci 5 0 17 -1 -1 2 -1  -keep_hist \
                        -prefix new_5_0_17_2.nii
    
        F. modify the header (modify fields or swap entire header):
    
          1. nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                        -mod_field dim '4 64 64 20 30 1 1 1 1'
          2. nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                        -mod_field descrip 'beer, brats and cheese, mmmmm...'
          3. cp old_dset.hdr nifti_swap.hdr 
             nifti_tool -swap_as_nifti -overwrite -infiles nifti_swap.hdr
          4. cp old_dset.hdr analyze_swap.hdr 
             nifti_tool -swap_as_analyze -overwrite -infiles analyze_swap.hdr
          5. nifti_tool -swap_as_old -prefix old_swap.hdr -infiles old_dset.hdr
             nifti_tool -diff_hdr -infiles nifti_swap.hdr old_swap.hdr
    
        G. strip, add or remove extensions:
           (in example #3, the extension is copied from a text file)
    
    
          1. nifti_tool -strip -overwrite -infiles *.nii
          2. nifti_tool -add_comment 'converted from MY_AFNI_DSET+orig' \
                        -prefix dnew -infiles dset0.nii
          3. nifti_tool -add_comment 'file:my.extension.txt' \
                        -prefix dnew -infiles dset0.nii
          4. nifti_tool -rm_ext ALL -prefix dset1 -infiles dset0.nii
          5. nifti_tool -rm_ext 2 -rm_ext 3 -rm_ext 5 -overwrite \
                        -infiles dset0.nii
    
      ------------------------------
    
      options for check actions:
    
        -check_hdr         : check for a valid nifti_1_header struct
    
           This action is used to check the nifti_1_header structure for
           problems.  The nifti_hdr_looks_good() function is used for the
           test, and currently checks:
           
             dim[], sizeof_hdr, magic, datatype
           
           More tests can be requested of the author.
    
           e.g. perform checks on the headers of some datasets
           nifti_tool -check_hdr -infiles dset0.nii dset1.nii
           nifti_tool -check_hdr -infiles *.nii *.hdr
           
           e.g. add the -quiet option, so that only errros are reported
           nifti_tool -check_hdr -quiet -infiles *.nii *.hdr
    
        -check_nim         : check for a valid nifti_image struct
    
           This action is used to check the nifti_image structure for
           problems.  This is tested via both nifti_convert_nhdr2nim()
           and nifti_nim_is_valid(), though other functions are called
           below them, of course.  Current checks are:
    
             dim[], sizeof_hdr, datatype, fname, iname, nifti_type
           
           Note that creation of a nifti_image structure depends on good
           header fields.  So errors are terminal, meaning this check would
           probably report at most one error, even if more exist.  The
           -check_hdr action is more complete.
    
           More tests can be requested of the author.
    
                 e.g. nifti_tool -check_nim -infiles dset0.nii dset1.nii
                 e.g. nifti_tool -check_nim -infiles *.nii *.hdr
    
      ------------------------------
    
      options for create action:
    
        -make_im           : create a new dataset from nothing
    
           With this the user can create a new dataset of a basic style,
           which can then be modified with other options.  This will create
           zero-filled data of the appropriate size.
           
           The default is a 1x1x1 image of shorts.  These settings can be
           modified with the -new_dim option, to set the 8 dimension values,
           and the -new_datatype, to provide the integral type for the data.
    
           See -new_dim, -new_datatype and -infiles for more information.
           
           Note that any -infiles dataset of the name MAKE_IM will also be
           created on the fly.
    
        -new_dim D0 .. D7  : specify the dim array for the a new dataset.
    
             e.g. -new_dim 4 64 64 27 120 0 0 0
    
           This dimension list will apply to any dataset created via
           MAKE_IM or -make_im.  All 8 values are required.  Recall that
           D0 is the number of dimensions, and D1 through D7 are the sizes.
           
        -new_datatype TYPE : specify the dim array for the a new dataset.
    
             e.g. -new_datatype 16
             default: -new_datatype 4   (short)
    
           This dimension list will apply to any dataset created via
           MAKE_IM or -make_im.  TYPE should be one of the NIFTI_TYPE_*
           numbers, from nifti1.h.
           
      ------------------------------
    
      options for copy actions:
    
        -copy_brick_list   : copy a list of volumes to a new dataset
        -cbl               : (a shorter, alternative form)
        -copy_im           : (a shorter, alternative form)
    
           This action allows the user to copy a list of volumes (over time)
           from one dataset to another.  The listed volumes can be in any
           order and contain repeats, but are of course restricted to
           the set of values {1, 2, ..., nt-1}, from dimension 4.
    
           This option is a flag.  The index list is specified with the input
           dataset, contained in square brackets.  Note that square brackets
           are special to most UNIX shells, so they should be contained
           within single quotes.  Syntax of an index list:
    
           notes:
    
             - indices start at zero
             - indices end at nt-1, which has the special symbol '$'
             - single indices should be separated with commas, ','
                 e.g. -infiles dset0.nii'[0,3,8,5,2,2,2]'
             - ranges may be specified using '..' or '-' 
                 e.g. -infiles dset0.nii'[2..95]'
                 e.g. -infiles dset0.nii'[2..$]'
             - ranges may have step values, specified in ()
               example: 2 through 95 with a step of 3, i.e. {2,5,8,11,...,95}
                 e.g. -infiles dset0.nii'[2..95(3)]'
    
           This functionality applies only to 3 or 4-dimensional datasets.
    
           e.g. to copy a dataset:
           nifti_tool -copy_im -prefix new.nii -infiles dset0.nii
    
           e.g. to copy sub-bricks 0 and 7:
           nifti_tool -cbl -prefix new_07.nii -infiles dset0.nii'[0,7]'
    
           e.g. to copy an entire dataset:
           nifti_tool -cbl -prefix new_all.nii -infiles dset0.nii'[0..$]'
    
           e.g. to copy every other time point, skipping the first three:
           nifti_tool -cbl -prefix new_partial.nii \
                      -infiles dset0.nii'[3..$(2)]'
    
    
        -copy_collapsed_image ... : copy a list of volumes to a new dataset
        -cci I J K T U V W        : (a shorter, alternative form)
    
           This action allows the user to copy a collapsed dataset, where
           some dimensions are collapsed to a given index.  For instance, the
           X dimension could be collapsed to i=42, and the time dimensions
           could be collapsed to t=17.  To collapse a dimension, set Di to
           the desired index, where i is in {0..ni-1}.  Any dimension that
           should not be collapsed must be listed as -1.
    
           Any number (of valid) dimensions can be collapsed, even down to a
           a single value, by specifying enough valid indices.  The resulting
           dataset will then have a reduced number of non-trivial dimensions.
    
           Assume dset0.nii has nim->dim[8] = { 4, 64, 64, 21, 80, 1, 1, 1 }.
           Note that this is a 4-dimensional dataset.
    
             e.g. copy the time series for voxel i,j,k = 5,4,17
             nifti_tool -cci 5 4 17 -1 -1 -1 -1 -prefix new_5_4_17.nii
    
             e.g. read the single volume at time point 26
             nifti_tool -cci -1 -1 -1 26 -1 -1 -1 -prefix new_t26.nii
    
           Assume dset1.nii has nim->dim[8] = { 6, 64, 64, 21, 80, 4, 3, 1 }.
           Note that this is a 6-dimensional dataset.
    
             e.g. copy all time series for voxel i,j,k = 5,0,17, with v=2
                  (and add the command to the history)
             nifti_tool -cci 5 0 17 -1 -1 2 -1  -keep_hist \
                        -prefix new_5_0_17_2.nii
    
             e.g. copy all data where i=3, j=19 and v=2
                  (I do not claim to know a good reason to do this)
             nifti_tool -cci 3 19 -1 -1 -1 2 -1 -prefix new_mess.nii
    
           See '-disp_ci' for more information (which displays/prints the
           data, instead of copying it to a new dataset).
    
      ------------------------------
    
      options for display actions:
    
        -disp_hdr          : display nifti_1_header fields for datasets
    
           This flag means the user wishes to see some of the nifti_1_header
           fields in one or more nifti datasets. The user may want to specify
           multiple '-field' options along with this.  This option requires
           one or more files input, via '-infiles'.
    
           If no '-field' option is present, all fields will be displayed.
    
           e.g. to display the contents of all fields:
           nifti_tool -disp_hdr -infiles dset0.nii
           nifti_tool -disp_hdr -infiles dset0.nii dset1.nii dset2.nii
    
           e.g. to display the contents of select fields:
           nifti_tool -disp_hdr -field dim -infiles dset0.nii
           nifti_tool -disp_hdr -field dim -field descrip -infiles dset0.nii
    
        -disp_nim          : display nifti_image fields for datasets
    
           This flag option works the same way as the '-disp_hdr' option,
           except that the fields in question are from the nifti_image
           structure.
    
        -disp_ana          : display nifti_analyze75 fields for datasets
    
           This flag option works the same way as the '-disp_hdr' option,
           except that the fields in question are from the nifti_analyze75
           structure.
    
        -disp_exts         : display all AFNI-type extensions
    
           This flag option is used to display all nifti_1_extension data,
           for only those extensions of type AFNI (code = 4).  The only
           other option used will be '-infiles'.
    
           e.g. to display the extensions in datasets:
           nifti_tool -disp_exts -infiles dset0.nii
           nifti_tool -disp_exts -infiles dset0.nii dset1.nii dset2.nii
    
        -disp_ts I J K    : display ASCII time series at i,j,k = I,J,K
    
           This option is used to display the time series data for the voxel
           at i,j,k indices I,J,K.  The data is displayed in text, either all
           on one line (the default), or as one number per line (via the
           '-dci_lines' option).
    
           Notes:
    
             o This function applies only to 4-dimensional datasets.
             o The '-quiet' option can be used to suppress the text header,
               leaving only the data.
             o This option is short for using '-disp_ci' (display collapsed
               image), restricted to 4-dimensional datasets.  i.e. :
                   -disp_ci I J K -1 -1 -1 -1
    
           e.g. to display the time series at voxel 23, 0, 172:
           nifti_tool -disp_ts 23 0 172            -infiles dset1_time.nii
           nifti_tool -disp_ts 23 0 172 -dci_lines -infiles dset1_time.nii
           nifti_tool -disp_ts 23 0 172 -quiet     -infiles dset1_time.nii
    
        -disp_collapsed_image  : display ASCII values for collapsed dataset
        -disp_ci I J K T U V W : (a shorter, alternative form)
    
           This option is used to display all of the data from a collapsed
           image, given the dimension list.  The data is displayed in text,
           either all on one line (the default), or as one number per line
           (by using the '-dci_lines' flag).
    
           The '-quiet' option can be used to suppress the text header.
    
           e.g. to display the time series at voxel 23, 0, 172:
           nifti_tool -disp_ci 23 0 172 -1 0 0 0 -infiles dset1_time.nii
    
           e.g. to display z-slice 14, at time t=68:
           nifti_tool -disp_ci -1 -1 14 68 0 0 0 -infiles dset1_time.nii
    
           See '-ccd' for more information, which copies such data to a new
           dataset, instead of printing it to the terminal window.
    
      ------------------------------
    
      options for modification actions:
    
        -mod_hdr           : modify nifti_1_header fields for datasets
    
           This action is used to modify some of the nifti_1_header fields in
           one or more datasets.  The user must specify a list of fields to
           modify via one or more '-mod_field' options, which include field
           names, along with the new (set of) values.
    
           The user can modify a dataset in place, or use '-prefix' to
           produce a new dataset, to which the changes have been applied.
           It is recommended to normally use the '-prefix' option, so as not
           to ruin a dataset.
    
           Note that some fields have a length greater than 1, meaning that
           the field is an array of numbers, or a string of characters.  In
           order to modify an array of numbers, the user must provide the
           correct number of values, and contain those values in quotes, so
           that they are seen as a single option.
    
           To modify a string field, put the string in quotes.
    
           The '-mod_field' option takes a field_name and a list of values.
    
           e.g. to modify the contents of various fields:
    
           nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                      -mod_field qoffset_x -17.325
           nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                      -mod_field dim '4 64 64 20 30 1 1 1 1'
           nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                      -mod_field descrip 'beer, brats and cheese, mmmmm...'
    
           e.g. to modify the contents of multiple fields:
           nifti_tool -mod_hdr -prefix dnew -infiles dset0.nii  \
                      -mod_field qoffset_x -17.325 -mod_field slice_start 1
    
           e.g. to modify the contents of multiple files (must overwrite):
           nifti_tool -mod_hdr -overwrite -mod_field qoffset_x -17.325   \
                      -infiles dset0.nii dset1.nii
    
        -mod_nim          : modify nifti_image fields for datasets
    
           This action option is used the same way that '-mod_hdr' is used,
           except that the fields in question are from the nifti_image
           structure.
    
        -strip_extras     : remove extensions and descriptions from datasets
    
           This action is used to attempt to 'clean' a dataset of general
           text, in order to make it more anonymous.  Extensions and the
           nifti_image descrip field are cleared by this action.
    
           e.g. to strip all *.nii datasets in this directory:
           nifti_tool -strip -overwrite -infiles *.nii
    
        -swap_as_nifti    : swap the header according to nifti_1_header
    
           Perhaps a NIfTI header is mal-formed, and the user explicitly
           wants to swap it before performing other operations.  This action
           will swap the field bytes under the assumption that the header is
           in the NIfTI format.
    
           ** The recommended course of action is to make a copy of the
              dataset and overwrite the header via -overwrite.  If the header
              needs such an operation, it is likely that the data would not
              otherwise be read in correctly.
    
        -swap_as_analyze  : swap the header according to nifti_analyze75
    
           Perhaps an ANALYZE header is mal-formed, and the user explicitly
           wants to swap it before performing other operations.  This action
           will swap the field bytes under the assumption that the header is
           in the ANALYZE 7.5 format.
    
           ** The recommended course of action is to make a copy of the
              dataset and overwrite the header via -overwrite.  If the header
              needs such an operation, it is likely that the data would not
              otherwise be read in correctly.
    
        -swap_as_old      : swap the header using the old method
    
           As of library version 1.35 (3 Aug, 2008), nifticlib now swaps all
           fields of a NIfTI dataset (including UNUSED ones), and it swaps
           ANALYZE datasets according to the nifti_analyze75 structure.
           This is a significant different in the case of ANALYZE datasets.
    
           The -swap_as_old option was added to compare the results of the
           swapping methods, or to undo one swapping method and replace it
           with another (such as to undo the old method and apply the new).
    
      ------------------------------
    
      options for adding/removing extensions:
    
        -add_afni_ext EXT : add an AFNI extension to the dataset
    
           This option is used to add AFNI-type extensions to one or more
           datasets.  This option may be used more than once to add more than
           one extension.
    
           If EXT is of the form 'file:FILENAME', then the extension will
           be read from the file, FILENAME.
    
           The '-prefix' option is recommended, to create a new dataset.
           In such a case, only a single file may be taken as input.  Using
           '-overwrite' allows the user to overwrite the current file, or
           to add the extension(s) to multiple files, overwriting them.
    
           e.g. to add a generic AFNI extension:
           nifti_tool -add_afni_ext 'wow, my first extension' -prefix dnew \
                      -infiles dset0.nii
    
           e.g. to add multiple AFNI extensions:
           nifti_tool -add_afni_ext 'wow, my first extension :)'      \
                      -add_afni_ext 'look, my second...'              \
                      -prefix dnew -infiles dset0.nii
    
           e.g. to add an extension, and overwrite the dataset:
           nifti_tool -add_afni_ext 'some AFNI extension' -overwrite \
                      -infiles dset0.nii dset1.nii 
    
        -add_comment_ext EXT : add a COMMENT extension to the dataset
    
           This option is used to add COMMENT-type extensions to one or more
           datasets.  This option may be used more than once to add more than
           one extension.  This option may also be used with '-add_afni_ext'.
    
           If EXT is of the form 'file:FILENAME', then the extension will
           be read from the file, FILENAME.
    
           The '-prefix' option is recommended, to create a new dataset.
           In such a case, only a single file may be taken as input.  Using
           '-overwrite' allows the user to overwrite the current file, or
           to add the extension(s) to multiple files, overwriting them.
    
           e.g. to add a comment about the dataset:
           nifti_tool -add_comment 'converted from MY_AFNI_DSET+orig' \
                      -prefix dnew                                    \
                      -infiles dset0.nii
    
           e.g. to add multiple extensions:
           nifti_tool -add_comment  'add a comment extension'         \
                      -add_afni_ext 'and an AFNI XML style extension' \
                      -add_comment  'dataset copied from dset0.nii'   \
                      -prefix dnew -infiles dset0.nii
    
        -rm_ext INDEX     : remove the extension given by INDEX
    
           This option is used to remove any single extension from the
           dataset.  Multiple extensions require multiple options.
    
           notes  - extension indices begin with 0 (zero)
                  - to view the current extensions, see '-disp_exts'
                  - all extensions can be removed using ALL or -1 for INDEX
    
           e.g. to remove the extension #0:
           nifti_tool -rm_ext 0 -overwrite -infiles dset0.nii
    
           e.g. to remove ALL extensions:
           nifti_tool -rm_ext ALL -prefix dset1 -infiles dset0.nii
           nifti_tool -rm_ext -1  -prefix dset1 -infiles dset0.nii
    
           e.g. to remove the extensions #2, #3 and #5:
           nifti_tool -rm_ext 2 -rm_ext 3 -rm_ext 5 -overwrite \
                      -infiles dset0.nii
    
      ------------------------------
    
      options for showing differences:
    
        -diff_hdr         : display header field diffs between two datasets
    
           This option is used to find differences between two datasets.
           If any fields are different, the contents of those fields is
           displayed (unless the '-quiet' option is used).
    
           A list of fields can be specified by using multiple '-field'
           options.  If no '-field' option is given, all fields will be
           checked.
    
           Exactly two dataset names must be provided via '-infiles'.
    
           e.g. to display all nifti_1_header field differences:
           nifti_tool -diff_hdr -infiles dset0.nii dset1.nii
    
           e.g. to display selected nifti_1_header field differences:
           nifti_tool -diff_hdr -field dim -field intent_code  \
                      -infiles dset0.nii dset1.nii 
    
        -diff_nim         : display nifti_image field diffs between datasets
    
           This option works the same as '-diff_hdr', except that the fields
           in question are from the nifti_image structure.
    
      ------------------------------
    
      miscellaneous options:
    
        -debug LEVEL      : set the debugging level
    
           Level 0 will attempt to operate with no screen output, but errors.
           Level 1 is the default.
           Levels 2 and 3 give progressively more information.
    
           e.g. -debug 2
    
        -field FIELDNAME  : provide a field to work with
    
           This option is used to provide a field to display, modify or
           compare.  This option can be used along with one of the action
           options presented above.
    
           See '-disp_hdr', above, for complete examples.
    
           e.g. nifti_tool -field descrip
           e.g. nifti_tool -field descrip -field dim
    
        -infiles file0... : provide a list of files to work with
    
           This parameter is required for any of the actions, in order to
           provide a list of files to process.  If input filenames do not
           have an extension, the directory we be searched for any
           appropriate files (such as .nii or .hdr).
    
           Note: if the filename has the form MAKE_IM, then a new dataset
           will be created, without the need for file input.
    
           See '-mod_hdr', above, for complete examples.
    
           e.g. nifti_tool -infiles file0.nii
           e.g. nifti_tool -infiles file1.nii file2 file3.hdr
    
        -mod_field NAME 'VALUE_LIST' : provide new values for a field
    
           This parameter is required for any the modification actions.
           If the user wants to modify any fields of a dataset, this is
           where the fields and values are specified.
    
           NAME is a field name (in either the nifti_1_header structure or
           the nifti_image structure).  If the action option is '-mod_hdr',
           then NAME must be the name of a nifti_1_header field.  If the
           action is '-mod_nim', NAME must be from a nifti_image structure.
    
           VALUE_LIST must be one or more values, as many as are required
           for the field, contained in quotes if more than one is provided.
    
           Use 'nifti_tool -help_hdr' to get a list of nifti_1_header fields
           Use 'nifti_tool -help_nim' to get a list of nifti_image fields
    
           See '-mod_hdr', above, for complete examples.
    
           e.g. modifying nifti_1_header fields:
                -mod_field descrip 'toga, toga, toga'
                -mod_field qoffset_x 19.4 -mod_field qoffset_z -11
                -mod_field pixdim '1 0.9375 0.9375 1.2 1 1 1 1'
    
        -keep_hist         : add the command as COMMENT (to the 'history')
    
            When this option is used, the current command will be added
            as a NIFTI_ECODE_COMMENT type extension.  This provides the
            ability to keep a history of commands affecting a dataset.
    
           e.g. -keep_hist
    
        -overwrite        : any modifications will be made to input files
    
           This option is used so that all field modifications, including
           extension additions or deletions, will be made to the files that
           are input.
    
           In general, the user is recommended to use the '-prefix' option
           to create new files.  But if overwriting the contents of the
           input files is preferred, this is how to do it.
    
           See '-mod_hdr' or '-add_afni_ext', above, for complete examples.
    
           e.g. -overwrite
    
        -prefix           : specify an output file to write change into
    
           This option is used to specify an output file to write, after
           modifications have been made.  If modifications are being made,
           then either '-prefix' or '-overwrite' is required.
    
           If no extension is given, the output extension will be '.nii'.
    
           e.g. -prefix new_dset
           e.g. -prefix new_dset.nii
           e.g. -prefix new_dset.hdr
    
        -quiet            : report only errors or requested information
    
           This option is equivalent to '-debug 0'.
    
      ------------------------------
    
      basic help options:
    
        -help             : show this help
    
           e.g.  nifti_tool -help
    
        -help_hdr         : show nifti_1_header field info
    
           e.g.  nifti_tool -help_hdr
    
        -help_nim         : show nifti_image field info
    
           e.g.  nifti_tool -help_nim
    
        -help_ana         : show nifti_analyze75 field info
    
           e.g.  nifti_tool -help_ana
    
        -help_datatypes [TYPE] : display datatype table
    
           e.g.  nifti_tool -help_datatypes
           e.g.  nifti_tool -help_datatypes N
    
           This displays the contents of the nifti_type_list table.
           An additional 'D' or 'N' parameter will restrict the type
           name to 'DT_' or 'NIFTI_TYPE_' names, 'T' will test.
    
        -ver              : show the program version number
    
           e.g.  nifti_tool -ver
    
        -hist             : show the program modification history
    
           e.g.  nifti_tool -hist
    
        -nifti_ver        : show the nifti library version number
    
           e.g.  nifti_tool -nifti_ver
    
        -nifti_hist       : show the nifti library modification history
    
           e.g.  nifti_tool -nifti_hist
    
        -with_zlib        : print whether library was compiled with zlib
    
           e.g.  nifti_tool -with_zlib
    
      ------------------------------
    
      R. Reynolds
      compiled: Nov  9 2017
      version 1.24 (September 26, 2012)
