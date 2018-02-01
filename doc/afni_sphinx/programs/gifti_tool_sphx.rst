**********
gifti_tool
**********

.. _gifti_tool:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ------------------------------------------------------------
    gifti_tool  - create, display, modify or compare GIFTI datasets
    
      general examples:
    
        1. read in a GIFTI dataset (set verbose level?  show GIFTI dataset?)
    
             gifti_tool -infile dset.gii
             gifti_tool -infile dset.gii -verb 3
             gifti_tool -infile dset.gii -show_gifti
    
        2. copy a GIFTI dataset
    
          a. create a simple copy, and check for differences
    
             gifti_tool -infile dset.gii -write_gifti copy.gii
             diff dset.gii copy.gii
    
          b. copy only 3 DataArray indices: 4, 0, 5
    
             gifti_tool -infile time_series.gii -write_gifti ts3.gii  \
                        -read_DAs 4 0 5
                   OR
    
             gifti_tool -infile time_series.gii'[4,0,5]'  \
                        -write_gifti ts3.gii
    
        3. write datasets in other formats
    
          a. FreeSurfer-style .asc surface dataset
    
             gifti_tool -infile pial.gii -write_asc pial.asc
    
          b. .1D time series surface dataset
    
             gifti_tool -infile time_series.gii -write_1D ts.1D
    
        4. create a new gifti dataset from nothing, where
    
          a. - the dataset has 3 DataArray elements
             - the data will be of type 'short' (NIFTI_TYPE_INT16)
             - the intent codes will reflect a t-test
             - the data will be 2-dimensional (per DataArray), 5 by 2 shorts
             - memory will be allocated for the data (a modification option)
             - the result will be written to created.gii
    
             gifti_tool -new_dset                                \
                        -new_numDA 3 -new_dtype NIFTI_TYPE_INT16 \
                        -new_intent NIFTI_INTENT_TTEST           \
                        -new_ndim 2 -new_dims 5 2 0 0 0 0        \
                        -mod_add_data -write_gifti created.gii
    
          b. - the dataset has 12 DataArray elements (40 floats each)
             - the data is partitioned over 2 files (so 6*40 floats in each)
    
               ** Note: since dataset creation does not add data (without
                        -mod_add_data), this operation will not create or
                        try to overwrite the external datafiles.
    
             gifti_tool -new_dset -new_numDA 12                   \
                        -new_ndim 1 -new_dims 40 0 0 0 0 0        \
                        -set_extern_filelist ext1.bin ext2.bin    \
                        -write_gifti points_to_extern.gii
    
        5. modify a gifti dataset
    
          a. apply various modifications at the GIFTI level and to all DAs
    
             - set the Version attribute at the GIFTI level
             - set 'Date' as GIFTI MetaData, with value of today's date
             - set 'Description' as GIFTI MetaData, with some value
             - set all DA Intent attributes to be an F-test
             - set 'Name' as an attribute of all DAs, with some value
             - read created.gii, and write to first_mod.gii
    
             gifti_tool -mod_gim_atr Version 1.0                       \
                        -mod_gim_meta Date "`date`"                    \
                        -mod_gim_meta Description 'modified surface'   \
                        -mod_DA_atr Intent NIFTI_INTENT_FTEST          \
                        -mod_DA_meta Name 'same name for all DAs'      \
                        -infile created.gii -write_gifti first_mod.gii
    
          b. modify the 'Name' attribute is DA index #42 only
    
             gifti_tool -mod_DA_meta Name 'data from pickle #42'       \
                        -mod_DAs 42                                    \
                        -infile stats.gii -write_gifti mod_stats.gii
    
          c. set the data to point to a single external data file, without
             overwriting the external file on write (so use -no_data), 
             and where the DataArrays will point to sequential partitions
             of the file
    
             gifti_tool -infiles created.gii -no_data          \
                        -set_extern_filelist ex_data.bin       \
                        -write_gifti extern.gii
    
          d. convert a POINTSET/TRIANGLE Base64 format dataset to one where
             to one where the data is external (raw binary):
    
               gifti_tool -infiles inflated.gii                     \
                          -set_extern_filelist points.data tri.data \
                          -write_gifti inflated.external.gii
    
          e. convert a 5 run time series dataset from internal Base64 format
             to one where the data is external (raw binary):
    
             as one external file:
    
               gifti_tool -infiles epi.5runs.gii               \
                          -set_extern_filelist data.5runs.bin  \
                          -write_gifti epi.ext.5runs.gii
    
             as 5 external files (1 per run):
    
               gifti_tool -infiles epi.5runs.gii                      \
                     -set_extern_filelist data.5runs.r{1,2,3,4,5}.bin \
                     -write_gifti epi.ext.5runs.gii
    
          f. convert the previous external dataset back to internal form
             (i.e. it should be the same as epi.5runs.gii)
    
               gifti_tool -infiles epi.ext.5runs.gii      \
                          -encoding BASE64                \
                          -write_gifti epi.int.5runs.gii
    
        6. compare 2 gifti datasets
    
          a. compare GIFTI structures, compare data, and report all diffs
    
             gifti_tool -compare_gifti -compare_data -compare_verb 3 \
                        -infiles created.gii first_mod.gii
    
          b. report approximate comparison: focusing on data, but allowing
             for small, fractional differences varying per datatype
    
             gifti_tool -approx_gifti -compare_verb 3 \
                        -infiles created.gii first_mod.gii
    
        7. copy MetaData from one dataset to another
           (any old Value will be replaced if the Name already exists)
    
             - copy every (ALL) MetaData element at the GIFTI level
             - copy MetaData named 'Label' per DataArray element
             - only apply DataArray copies to indices 0, 3 and 6
             - first input file is the source, second is the destination
             - write the modified 'destination.gii' dataset to meta_copy.gii
    
             gifti_tool -copy_gifti_meta ALL                   \
                        -copy_DA_meta Label                    \
                        -DA_index_list 0 3 6                   \
                        -infiles source.gii destination.gii    \
                        -write_gifti meta_copy.gii
    
    ----------------------------------------------------------------------
    
      (all warranties are void in Montana, and after 4 pm on Tuesdays)
    
    ----------------------------------------------------------------------
      informational options:
    
         -help             : display this help
         -hist             : display the modification history of gifti_tool
         -ver              : display the gifti_tool version
         -gifti_hist       : display thd modification history of gifticlib
         -gifti_ver        : display gifticlib version
         -gifti_dtd_url    : display the gifti DTD URL
         -gifti_zlib       : display whether the zlib is linked in library
    
      ----------------------------------------
      general/input options
    
         -b64_check   TYPE : set method for checking base64 errors
    
               e.g. -b64_check COUNT
    
               This option sets the preference for how to deal with errors
               in Base64 encoded data (whether compressed or not).  The
               default is SKIPnCOUNT, which skips any illegal characters,
               and reports a count of the number found.
    
                   TYPE = NONE       : no checks - assume all is well
                   TYPE = DETECT     : report whether errors were found
                   TYPE = COUNT      : count the number of bad chars
                   TYPE = SKIP       : ignore any bad characters
                   TYPE = SKIPnCOUNT : ignore but count bad characters
    
               This default adds perhaps 10% to the reading time.
    
         -buf_size    SIZE : set the buffer size (given to expat library)
    
               e.g. -buf_size 1024
    
         -DA_index_list I0 I1 ... : specify a list of DataArray indices
    
               e.g. -DA_index_list 0
               e.g. -DA_index_list 0 17 19
    
               This option is used to specify a list of DataArray indices
               for use via some other option (such as -copy_DA_meta).
    
               Each DataArray element corresponding to one of the given
               indices will have the appropriate action applied, such as
               copying a given MetaData element from the source dataset
               to the destination dataset.
    
               Note that this differs from -read_DAs, which specifies which
               DataArray elements to even read in.  Both options could be
               used in the same command, such as if one wanted to copy the
               'Name' MetaData from index 17 of a source dataset into the
               MetaData of the first DataArray in a dataset with only two
               DataArray elements.
    
               e.g. gifti_tool -infiles source.gii dest.gii        \
                               -write_gifti new_dest.gii           \
                               -copy_DA_meta Name                  \
                               -read_DAs 17 17                     \
                               -DA_index_list 0
    
               Note that DA_index_list applies to the indices _after_ the
               datasets are read in.
    
         -gifti_test       : test whether each gifti dataset is valid
    
               This performs a consistency check on each input GIFTI
               dataset.  Lists and dimensions must be consistent.
    
         -infile     INPUT : specify one or more GIFTI datasets as input
    
               e.g. -input pial.gii
               e.g. -input run1.gii run2.gii
               e.g. -input MAKE_IM                 (create a new image)
               e.g. -input run1.gii'[3,4,5]'       (read DAs 3,4,5    )
               e.g. -input run1.gii'[0..16(2)]'    (read evens from 0 to 16)
               e.g. -input run1.gii'[4..$]'        (read all but 0..3)
    
               There are 2 special ways to specify input.  One is via the
               name 'MAKE_IM'.  That 'input' filename tell gifti_tool to
               create a new dataset, applying any '-new_*' options to it.
    
                   (refer to options: -new_*)
    
               The other special way is to specify which DataArray elements
               should be read in, using AFNI-style syntax within '[]'.  The
               quotes prevent the shell from interpreting the brackets.
    
               DataArray indices are zero-based.
    
               The list of DAs can be comma-delimited, and can use '..' or
               '-' to specify a range, and a value in parentheses to be used
               as a step.  The '$' character means the last index (numDA-1).
    
         -no_data          : do not read in data
    
               This option means not to read in the Data element in any
               DataArray, akin to reading only the header.
    
         -no_updates       : do not allow the library to modify metadata
    
               By default, the library may update some metadata fields, such
               as 'gifticlib-version'.  The -no_updates option will prevent
               that operation.
    
         -read_DAs s0 ...  : read DataArray list indices s0,... from input
    
               e.g. -read_DAs 0 4 3 3 8
               e.g. -input run1.gii -read_DAs 0 2 4 6 8
               e.g. -input run1.gii'[0..8(2)]'              (same effect)
    
               Specify a list of DataArray indices to read.  This is a
               simplified form of using brackets '[]' with -input names.
    
         -show_gifti       : show final gifti image
    
               Display all of the dataset information on the screen (sans
               data).  This includes meta data and all DataArray elements.
    
         -verb        VERB : set verbose level   (default: 1)
    
               e.g. -verb 2
    
               Print extra information to the screen.  The VERB level can
               be from 0 to 8, currently.
    
               Level 0 is considered 'quiet' mode, and should only report
               serious errors.  Level 1 is the default.
    
      ----------------------------------------
      output options
    
         -encoding    TYPE : set the data encoding for any output file
    
               e.g. -encoding BASE64GZIP
    
                   TYPE = ASCII      : ASCII encoding
                   TYPE = BASE64     : base64 binary
                   TYPE = BASE64GZIP : base64 compressed binary
    
               This operation can also be performed via -mod_DA_atr:
               e.g. -mod_DA_atr Encoding BASE64GZIP
    
         -set_extern_filelist F1 F2 ... : store data in external files
    
               e.g. -set_extern_filelist run.1.data run.2.data run.3.data
               e.g. -set_extern_filelist runs.all.data
               e.g. -set_extern_filelist points.data triangles.data
    
               Data is normally stored within the XML file as numerical
               text or Base64 encoded raw or compressed data.
    
               With use of this option, users can set to have data stored in
               external binary files (neither encoded nor compressed) upon a
               write operation.
    
               External file storage is subject to a couple of restrictions:
    
                 - GIFTI requires that they are in the same directory
    
                 - the library allows multiple DataArrays per file, but each
                   DataArray within the same file must have the same size
                   (this is a gifticlib limit, not a GIFTI limit)
    
                     OK : equal data in 1 file
                     OK : equal data in k files, numDA is multiple of k
                     BAD: equal data in k files, numDA is NOT multiple of k
                     OK : points/triangles in 2 files
                     BAD: points/triangles in 1 file (sizes differ)
    
               The most basic use of this option is to convert data from
               internal to external.  See examples 5d and 5e.
    
               Note that one can also create a GIFTI dataset out of nothing
               and use this option to point to existing external data files.
               This would help conversion from other dataset formats.  See
               example 5c.
    
               Note that one can convert from an external data format to
               internal just by modifying the -encoding.  See example 5f.
    
         -write_1D    DSET : write out data to AFNI style 1D file
    
               e.g. -write_1D stats.1D
    
               Currently, all DAs need to be of the same datatype.  This
               restriction could be lifted if there is interest.
    
         -write_asc   DSET : write out geometry to FreeSurfer style ASC file
    
               e.g. -write_asc pial.asc
    
               To write a surface file in FreeSurfer asc format, it must
               contain DataArray elements of intent NIFTI_INTENT_POINTSET
               and NIFTI_INTENT_TRIANGLE.  The POINTSET data is written as
               node coordinates and the TRIANGLE data as triangles (node
               index triplets).
    
         -write_gifti DSET : write out dataset as gifti image
    
               e.g. -write_gifti new.pial.gii
    
         -zlevel     LEVEL : set compression level (-1 or 0..9)
    
               This option sets the compression level used by zlib.  Some
               LEVEL values are noteworthy:
    
                  -1   : specify to use the default of zlib (currently 6)
                   0   : no compression (but still needs a few extra bytes)
                   1   : fastest but weakest compression
                   6   : default (good speed/compression trade-off)
                   9   : slowest but strongest compression
    
      ----------------------------------------
      modification options
    
         These modification options will affect every DataArray element
         specified by the -mod_DAs option.  If the option is not used,
         then ALL DataArray elements will be affected.
    
         -mod_add_data     : add data to empty DataArray elements
    
               Allocate data in every DataArray element.  Datasets can be
               created without any stored data.  This will allocate data
               and fill it with zeros of the given type.
    
         -mod_DA_atr  NAME VALUE : set the NAME=VALUE attribute pair
    
               e.g. -mod_DA_atr Intent NIFTI_INTENT_ZSCORE
    
               This option will set the DataArray attribute corresponding
               to NAME to the value, VALUE.  Attribute name=value pairs are
               specified in the gifti DTD (see -gifti_dtd_url).
    
               One NAME=VALUE pair can be specified per -mod_DA_atr
               option.  Multiple -mod_DA_atr options can be used.
    
         -mod_DA_meta NAME VALUE : set the NAME=VALUE pair in DA's MetaData
    
               e.g. -mod_DA_meta Description 'the best dataset, ever'
    
               Add a MetaData entry to each DataArray element for this
               NAME and VALUE.  If 'NAME' already exists, the old value
               is replaced by VALUE.
    
         -mod_DAs i0 i1 ...      : specify the set of DataArrays to modify
    
               e.g. -mod_DAs 0 4 5
    
               Specify the list of DataArray elements to modify.  All the
               -mod_* options apply to this list of DataArray indices.  If
               no -mod_DAs option is used, the operations apply to ALL
               DataArray elements.
    
               Note that the indices are zero-based, 0 .. numDA-1.
    
         -mod_gim_atr  NAME VALUE : set the GIFTI NAME=VALUE attribute pair
    
               e.g. -mod_gim_atr Version 3.141592
    
               Set the GIFTI element attribute corresponding to NAME to the
               value, VALUE.
    
               Given that numDA is computed and version will rarely change,
               this option will probably not feel much love.
    
         -mod_gim_meta NAME VALUE : add this pair to the GIFTI MetaData
    
               e.g. -mod_gim_meta date "`date`"
    
               Add a MetaData entry to each DataArray element for this
               NAME and VALUE pair.  If NAME exists, VALUE will replace
               the old value.
    
         -mod_to_float            : change all DataArray data to float
    
               Convert all DataArray elements of all datasets to datatype
               NIFTI_TYPE_FLOAT32 (4-byte floats).  If the data does not
               actually exist, only the attribute will be set.  Otherwise
               all of the data will be converted.  There are some types
               for which this operation may not be appropriate.
    
      ----------------------------------------
    
      creation (new dataset) options
    
         -new_dset         : create a new GIFTI dataset
         -new_numDA  NUMDA : new dataset will have NUMDA DataArray elements
                             e.g. -new_numDA 3
         -new_intent INTENT: DA elements will have intent INTENT
                             e.g. -new_intent NIFTI_INTENT_FTEST
         -new_dtype   TYPE : set datatype to TYPE
                             e.g. -new_dtype NIFTI_TYPE_FLOAT32
         -new_ndim NUMDIMS : set Dimensionality to NUMDIMS (see -new_dims)
         -new_dims D0...D5 : set dims[] to these 6 values
                             e.g. -new_ndim 2 -new_dims 7 2 0 0 0 0
         -new_data         : allocate space for data in created dataset
    
      ----------------------------------------
      comparison options
    
         -approx_gifti            : approximate comparison of GIFTI dsets
    
               This compares all data elements of the two GIFTI structures.
               The attributes, MetaData, etc. are ignored if they do not
               pertain directly to the data.
    
               The comparisons allow for small, fractional differences,
               which depend on the datatype.
    
         -compare_gifti           : specifies to compare two GIFTI datasets
    
               This compares all elements of the two GIFTI structures.
               The attributes, LabelTabels, MetaData are compared, and then
               each of the included DataArray elements.  All sub-structures
               of the DataArrays are compared, except for the actual 'data',
               which requires the '-compare_data' flag.
    
               There must be exactly 2 input datasets to use this option.
               See example #7 for sample usage.
    
         -compare_data            : flag to request comparison of the data
    
               Data comparison is done per DataArray element.
    
               Comparing data is a separate operation from comparing GIFTI.
               Neither implies the other.
    
         -compare_verb LEVEL      : set the verbose level of comparisons
    
               Data comparison is done per DataArray element.  Setting the
               verb level will have the following effect:
    
               0 : quiet, only return whether there was a difference
               1 : show whether there was a difference
               2 : show whether there was a difference per DataArray
               3 : show all differences
    
      ----------------------------------------
      MetaData copy options
    
         -copy_gifti_meta MD_NAME      : copy MetaData with name MD_NAME
    
               e.g. -copy_gifti_meta AFNI_History
    
               Copy the MetaData with the given name from the first input
               dataset to the second (last).  This applies to MetaData at
               the GIFTI level (not in the DataArray elements).
    
         -copy_DA_meta MD_NAME         : copy MetaData with name MD_NAME
    
               e.g. -copy_DA_meta intent_p1
    
               Copy the MetaData with the given name from the first input
               dataset to the second (last).  This applies to MetaData at
               DataArray level.
    
               This will apply to all DataArray elements, unless the
               -DA_index_list option is used to specify a zero-based
               index list.
    
               see also -DA_index_list
    
    ------------------------------------------------------------
    see the GIfTI community web site at:
    
               http://www.nitrc.org/projects/gifti
    
    R Reynolds, National Institutes of Health
