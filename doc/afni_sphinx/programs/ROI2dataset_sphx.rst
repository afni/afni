.. contents:: 
    :depth: 4 

***********
ROI2dataset
***********

.. code-block:: none

    
    Usage: 
       ROI2dataset <-prefix dsetname> [...] <-input ROI1 ROI2 ...>
                   [<-of ni_bi|ni_as|1D>] 
                   [<-dom_par_id idcode>] 
        This program transforms a series of ROI files
        to a node dataset. This data set will contain
        the node indices in the first column and their
        ROI values in the second column.
        Duplicate node entries (nodes that are part of
        multiple ROIs) will get ignored. You will be
        notified when this occurs. 
    
    Mandatory parameters:
        -prefix     dsetname: Prefix of output dataset.
                              Program will not overwrite existing
                              datasets.
                              See also -label_dset alternate below.
        -keep_separate: Output one column (sub-brick) for each ROI value
    
     and/or
        -nodelist        NL: Prefix for a set of .1D files
        -nodelist.nodups NL: that contain a list of node indices
                             in the order in which they appear in
                             an ROI. This way you can make use of the
                             directionality of an ROI line instead of just 
                             treating it as a set of nodes. 
                             For each integer label 'i' in the ROI files provided
                             with the -input option, you will get a file called
                             NL.i.1D listing the nodes in the order they were 
                             encountered in an ROI file and across ROI files.
                             If you want duplicate node entries removed, then
                             use -nodelist.nodups instead.
                       For example, say you traced an ROI that consisted of some 
                       arbitrary curved path and you want to get the nodes 
                       forming the path in the order traversed while drawing.
                       First save the path drawn, say to trace.niml.roi, 
                       then use the following command:
                             ROI2dataset -nodelist.nodups TRACE \
                                         -input trace.niml.roi
                       Note: You can use the output of -nodelist as input to 
                             ConvertDset's -node_select_1D option. 
                             This is not the case for -nodelist because
                             ConvertDset's -node_select_1D does not allow for 
                             duplicate node entries. 
        -nodelist_with_ROIval: Also add the ROIval as a second column in .1D
                               files output by -nodelist.
        -input ROI1 ROI2....: ROI files to turn into a 
                              data set. This parameter MUST
                              be the last one on command line.
    
    Optional parameters:
        All optional parameters must be specified before the -input parameters.
    
        -label_dset dsetname: Write a label dataset, instead of a simple dataset.
                              Labeled datasets are treated differently in SUMA.
                              This option also sets the output format to NIML.
                        Note: Using -keep_separate with this option is legal, but
                              makes little sense. You can't view more than one 
                              sub-brick in SUMA for Labeled datasets.
        -h | -help: This help message
        -of FORMAT: Output format of dataset. FORMAT is one of:
                    ni_bi: NIML binary
                    ni_as: NIML ascii (default)
                    1D   : 1D AFNI format.
        -dom_par_id id: Idcode of domain parent.
                        When specified, only ROIs have the same
                        domain parent are included in the output.
                        If id is not specified then the first
                        domain parent encountered in the ROI list
                        is adopted as dom_par_id.
                        1D roi files do not have domain parent 
                        information. They will be added to the 
                        output data under the chosen dom_par_id.
        -pad_to_node max_index: Output a full dset from node 0 
                                to node max_index (a total of 
                                max_index + 1 nodes). Nodes that
                                are not part of any ROI will get
                                a default label of 0 unless you
                                specify your own padding label.
        -pad_label padding_label: Use padding_label (an integer) to
                                label nodes that do not belong
                                to any ROI. Default is 0.
                             This padding value is also used in the multi-column
                                format of option -keep_separate.
    
    
    Compile Date:
       Nov  9 2017
    
